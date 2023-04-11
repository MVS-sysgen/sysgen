//ZP60014  JOB (SYSGEN),'J09 M28: ZP60014',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD CLIST CONTROL VARIABLE AND BUILT-IN FUNCTION EXTENSIONS.
//*
//STEP01  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60014)    REWORK(20201207)    /* ADD CLIST EXTENSIONS */.
++VER(Z038) FMID(EBB1102)
  PRE(UY16532,UY17021)
 /*
   PROBLEM DESCRIPTION:
     VARIOUS CLIST FACILITIES PRESENT IN TSO/E ARE NOT AVAILABLE.
       MANY USEFUL CLIST FACILITIES SUCH AS VARIOUS DATE AND TIME
       FORMATS, ADDITIONAL ENVIRONMENTAL SYMBOLS, AND THE ABILITY
       TO CAPTURE TERMINAL OUTPUT ARE NOT SUPPLIED AS PART OF TSO.

       THIS USERMOD CHANGES SEVERAL TSO MODULES.

       THE EXEC COMMAND HAS BEEN CHANGED TO DEFINE AND RESOLVE AN
       ADDITIONAL 21 CONTROL VARIABLES AND BUILT-IN FUNCTIONS AS
       WELL AS THE ORIGINAL 19.

       PUTLINE HAS BEEN ALTERED TO SUPPORT THE CAPTURE OF LINE-MODE
       TERMINAL OUTPUT INTO CLIST VARIABLES.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

     DOC:
       TSO PROVIDES THE FOLLOWING CLIST BUILT-IN FUNCTIONS:

         DATATYPE   - DETERMINE DATA TYPE: 'CHAR' OR 'NUM'
         EVAL       - FORCE ARITHMETIC EVALUATION
         LENGTH     - DETERMINE LENGTH OF EXPRESSION IN BYTES
         STR        - DEFINE CHARACTER STRING
         SUBSTR     - DEFINE SUBSTRING

       THIS SYSMOD ADDS THE FOLLOWING CLIST BUILT-IN FUNCTIONS:

         NRSTR      - SCAN DATA ONCE AND RETAIN DOUBLE AMPERSANDS
           EXAMINE THE FOLLOWING SAMPLE CLIST AND ITS OUTPUT TO
           DETERMINE THE APPROPRIATE USES OF NRSTR:
               PROC 0
               SET A = &&B
               SET B = FRED
               SET C = &NRSTR(&A)
               WRITE C IS &C
               WRITE C IS &NRSTR(&C)
               SET DDSTMT = &NRSTR(//SYSUT1 DD DSN=&&UT1,SPACE=(TRK,3),)
               WRITE &DDSTMT
               WRITE &NRSTR(&DDSTMT)

         SYSDSN     - DETERMINE DATA SET AVAILABILITY
           EG. SET ANS = &SYSDSN(&DSNAME)
             &ANS WILL EVALUATE TO ONE OF THE FOLLOWING:
               'OK'
               'UNAVAILABLE DATASET'
               'DATASET NOT FOUND'
               'MEMBER NOT FOUND'
               'MEMBER SPECIFIED, BUT DATASET IS NOT PARTITIONED'
               'VOLUME NOT ON SYSTEM'
               'MISSING DATASET NAME'
               'ERROR PROCESSING REQUESTED DATASET'
               'INVALID DATASET NAME, INPUTDSNAME'

       TSO PROVIDES THE FOLLOWING CLIST CONTROL VARIABLES:

         LASTCC     - LATEST TSO COMMAND RETURN CODE
         MAXCC      - HIGHEST TSO COMMAND RETURN CODE

         SYSDLM     - TERMIN DELIMITER
         SYSDVAL    - DEFAULT INPUT VALUE

         SYSNEST    - DETERMINE CLIST NESTING: 'NO' OR 'YES'
         SYSSCAN    - SYMBOLIC SUBSTITUTION SCAN LIMIT
         SYSICMD    - IMPLICIT COMMAND NAME
         SYSPCMD    - MOST RECENT TSO PRIMARY COMMAND
         SYSSCMD    - MOST RECENT TSO SUBCOMMAND

         SYSUID     - TSO USERID
         SYSPREF    - CURRENT DATA SET NAME PREFIX
         SYSPROC    - NAME OF THE TSO LOGON PROCEDURE

         SYSDATE    - CURRENT DATE:     'MM/DD/YY'
         SYSTIME    - CURRENT TIME:     'HH:MM:SS'

       THIS SYSMOD ADDS THE FOLLOWING CLIST CONTROL VARIABLES:

         SYSSTIME   - SHORTENED TIME:   'HH:MM'
         SYS4DATE   - CURRENT DATE:     'MM/DD/YYYY'
         SYSSDATE   - SORTABLE DATE:    'YY/MM/DD'
         SYS4SDATE  - SORTABLE DATE:    'YYYY/MM/DD'
         SYSJDATE   - JULIAN DATE:      'YY.DDD'
         SYS4JDATE  - JULIAN DATE:      'YYYY.DDD'
         SYS4IDATE  - ISO-FORMAT DATE:  'YYYY-MM-DD'

         SYSENV     - CLIST ENVIRONMENT: 'FORE' OR 'BACK'
         SYSISPF    - ISPF ENVIRONMENT: 'ACTIVE' OR 'NOT ACTIVE'
         SYSSMFID   - SYSTEM SMF IDENTIFIER
         SYSOPSYS   - OPERATING SYSTEM NAME: 'OS/VS2 3.8 EBB1102'
         SYSJES     - NAME OF SUBSYSTEM PROVIDING JOB ID
         SYSTERMID  - NAME OF TSO TERMINAL WHERE CLIST STARTED

         SYSLTERM   - CURRENT LINE COUNT OF TSO TERMINAL
         SYSWTERM   - CURRENT LINE WIDTH OF TSO TERMINAL

         SYSCPU     - CURRENT TCB TIME ACCUMULATED BY TSO SESSION
         SYSSRV     - CURRENT SERVICE ACCUMULATED BY TSO SESSION

         SYSOUTLINE - NUMBER OF DISPLAY LINES HELD IN VARIABLES
         SYSOUTTRAP - MAXIMUM NUMBER OF DISPLAY LINES TO CAPTURE

           CAPTURED LINES ARE HELD IN VARIABLES CALLED
           SYSOUTLINE1, SYSOUTLINE2, SYSOUTLINE3, ETC.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 14.

     REWORK HISTORY:
       2005-09-18: SKIP OUTTRAP CHECK FOR MESSAGES FROM EXEC (S0C4).
       2005-11-11: SKIP OUTTRAP CHECK FOR MESSAGES FROM COMMAND
                   SCAN/PARSE AND PUTLINE/GETLINE/PUTGET SERVICES.
       2006-07-18: REMOVE MSGID TESTING OF PREVIOUS REWORKS, AND
                   SKIP OUTTRAP CHECK IF CLIST ACTIVE FLAG NOT SET.
       2009-06-13: CORRECT RESUME SCAN DATA ADDRESS AFTER SYSDSN.
       2009-08-23: ADD NRSTR "NO RESCAN STRING".
       2020-12-07: ADD SYSISPF.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKJCT431
       IKJCT433
       IKJEFT56
 */.
++MOD(IKJCT431) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//STEP02  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IKJCT431 EXEC COMMAND SYMBOLIC PARAMETER DEFINITION'
*
*   MODIFIED BY GREG PRICE SEPTEMBER 2005 FOR USERMOD ZP60014
*
*   2009-08-23 - ADD &NRSTR "NO RESCAN STRING"
*   2020-12-07 - ADD &SYSISPF
*
IKJCT431 CSECT ,                                                   0001
         USING PSA,0
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                             0001
         DC    C'IKJCT431  87.344'                                 0001
         DC    C' ZP60014 20201207'
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R11,0                                               0001
@PSTART  LA    R12,4095(,R11)                                      0001
         USING @PSTART,R11                                         0001
         USING @PSTART+4095,R12                                    0001
         L     R0,@SIZDATD                                         0001
         GETMAIN  R,LV=(0)
         LR    R10,R1                                              0001
         USING @DATD,R10                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R10,8(,R13)                                         0001
         LR    R13,R10                                             0001
         MVC   @PC00001(4),0(R1)                                   0001
*   ECDAPTR=R1;                     /* SAVE ADDRESS OF COMMON DATA   */
         LR    ECDAPTR7,R1                                         0093
*   RFY                                                            0094
*     R1 UNRSTD;                                                   0094
*   LINELNG=RECLNG-CON4;            /* LINE LENGTH FOR FIRST RECORD  */
         L     R6,ECDAIREC(,ECDAPTR7)                              0095
         LH    R6,RECLNG(,R6)                                      0095
         SL    R6,FW04                                             0095
         ST    R6,LINELNG                                          0095
*   ECB=CON0;                       /* INIT AN ECB FOR SERVICE RTNS  */
         SLR   R6,R6                                               0096
         ST    R6,ECB                                              0096
*   FLGCT431='';                    /* INITIALIZE FLAG AREA          */
         MVI   FLGCT431,X'00'                                      0097
*   ERRPTR=ADDR(ERRPARMS);          /* SERVICE RTN PARMLIST AREA     */
*                                                                  0098
         LA    R6,ERRPARMS                                         0098
         ST    R6,ERRPTR                                           0098
*   /*****************************************************************/
*   /*                                                               */
*   /* ISSUE GETMAIN FOR: LSD, EXECDATA, SNTAB, SVTAB AND THE FIRST  */
*   /* COMMAND PROCEDURE BLOCK                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0099
*   RFY                                                            0099
*     R15 RSTD;                     /* ISSUE GETMAIN FOR STANDARD    */
*   DO;                             /* GETMAIN (EC) LV(GETSIZE)    0100
*                                      A(ECDAGADD) SP(78) RTCD(R15)  */
*     RESPECIFY                                                    0101
*      (R1,                                                        0101
*       R15) RESTRICTED;                                           0101
*     I004=I004&&I004;              /* INIT LIST                     */
         XC    I004(10),I004                                       0102
*     I00404='20'X;                 /* TYPE                          */
         MVI   I00404,X'20'                                        0103
*     I00401=GETSIZE;               /* LENGTH                        */
         MVC   I00401(4),FW8192     WAS 4096             ZP60014   0104
*     I00403=ADDR(ECDAGADD);        /* ADDR OF ADDR LIST             */
         LA    R14,ECDAGADD(,ECDAPTR7)                             0105
         STCM  R14,7,I00403                                        0105
*     I00405=78;                    /* SUBPOOL VALUE                 */
         MVI   I00405,X'4E'                                        0106
*     R1=ADDR(I004);                /* REG1 POINTS TO LIST           */
         LA    R1,I004                                             0107
*     SVC(4);                       /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0108
*     R15=R15;                      /* SET RETURN CODE               */
*     RESPECIFY                                                    0110
*      (R1,                                                        0110
*       R15) UNRESTRICTED;                                         0110
*   END;                            /* GETMAIN (EC) LV(GETSIZE)    0111
*                                      A(ECDAGADD) SP(78) RTCD(R15)
*                                      EXEC                          */
*                                                                  0112
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK GETMAIN RETURN CODE                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0112
*   IF R15^=CON0 THEN               /* CONTROL BLOCKS                */
         LTR   R15,R15                                             0112
         BZ    @RF00112                                            0112
*     DO;                           /* IF STORAGE COULD NOT BE       */
*       RFY                                                        0114
*         R15 UNRSTD;               /* OBTAINED THEN NOTIFY USER AND */
*       EXMSGID=M511;               /* RETURN RC=16                  */
         MVC   EXMSGID(4),$MSGM511                                 0115
*       CALL MSGRTN;                /* ISSUE MESSAGE                 */
         BAL   R14,MSGRTN                                          0116
*       NOTEXEC=YES;                /* COMMAND PROCEDURE NOT       0117
*                                      EXECUTABLE                    */
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0117
*       CT431RET=CON16;                                            0118
         MVC   CT431RET(4),FW16                                    0118
*     END;                          /* CONTROL RETURNS TO EXIT POINT */
*                                                                  0119
*   /*****************************************************************/
*   /*                                                               */
*   /* IF ERRORS HAVE OCCURRED THEN CONTROL RETURNS TO IKJCT431 EXIT */
*   /* POINT                                                         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0120
*                                                                  0120
*   /*****************************************************************/
*   /*                                                               */
*   /* INITIALIZE THE EXEC CONTROL BLOCKS - FIRST THE LSD            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0120
*   ELSE                            /* GETMAIN SUCCESSFUL - CONTINUE */
*     DO;                                                          0120
         B     @RC00112                                            0120
@RF00112 DS    0H                                                  0121
         L     R2,PSAAOLD
         L     R6,ASCBCSCB-ASCB(,R2)
         ST    R6,TIOTPTR
*       SP78BLK=YES;                /* INDICATE CORE HAS BEEN GOTTEN */
*       SP78CORE=YES;                                              0122
         OI    ECDAFLAG(ECDAPTR7),SP78CORE+SP78BLK                 0122
*       ECDALSD=ECDAGADD;           /* LSD IS FIRST BLOCK IN INITIAL */
         L     R6,ECDAGADD(,ECDAPTR7)                              0123
         ST    R6,ECDALSD(,ECDAPTR7)                               0123
*       LSDRCLEN=CON0;              /* GETMAIN AREA - RECLEN IS      */
         SLR   R8,R8                                               0124
         L     R9,LSDPTR(,ECDAPTR7)                                0124
         STH   R8,LSDRCLEN-LSD(,R9)                                0124
*       LSDADATA=ECDAGADD+HALF;     /* VARIABLE                      */
         L     R14,FW4096           WAS LA R14,2048      ZP60014   0125
         LR    R15,R6                                              0125
         ALR   R15,R14                                             0125
         ST    R15,LSDADATA-LSD(,R9)                               0125
*       LSDANEXT=LSDADATA+CON12;    /* UPDATE PAST BLOCK HEADER      */
         LA    R0,12                                               0126
         ALR   R15,R0                                              0126
         ST    R15,LSDANEXT-LSD(,R9)                               0126
*       LSDBLKID=CON1;              /* INIT TO ID OF FIRST BLOCK     */
         MVI   LSDBLKID-LSD(R9),X'01'                              0127
*       LSDADAID=CON1;                                             0128
         MVI   LSDADAID-LSD(R9),X'01'                              0128
*       LSDEXEC=ECDALSD+((LENGTH(LSD)+CON7)/CON8*CON8);/* PLACE    0129
*                                      EXECDATA ON DOUBLE WORD     0129
*                                      BOUNDRY                       */
*                                                                  0129
         AL    R6,FW16                                             0129
         ST    R6,LSDEXEC-LSD(,R9)                                 0129
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE THE COMMAND PROCEDURE BLOCK HEADER INFORMATION */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0130
*       ECDACBLK=LSDADATA;          /* INITIALIZE COMMAND PROCEDURE  */
         L     R1,LSDADATA-LSD(,R9)                                0130
         ST    R1,ECDACBLK(,ECDAPTR7)                              0130
*       ECDACNXT=LSDANEXT;                                         0131
         L     R9,LSDANEXT-LSD(,R9)                                0131
         ST    R9,ECDACNXT(,ECDAPTR7)                              0131
*       ECDACPRE=ECDACNXT;          /* SAME AS NEXT PTR INITIALLY    */
         ST    R9,ECDACPRE(,ECDAPTR7)                              0132
*       COMPRNXT=CON0;              /* INDICATE NO MORE BLOCKS       */
         STCM  R8,7,COMPRNXT(R1)                                   0133
*       COMPRID=CON1;               /* THIS IS FIRST BLOCK           */
         MVI   COMPRID(R1),X'01'                                   0134
*       COMPRLNG=HALF;              /* ORIGIONAL SIZE IS HALF OF PAGE*/
         ST    R14,COMPRLNG(,R1)                                   0135
*       COMPRUSE=CON12;                                            0136
         ST    R0,COMPRUSE(,R1)                                    0136
*                                                                  0137
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE THE EXEC DATA CONTROL BLOCK                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0137
*       EXECDATA=EXECDATA&&EXECDATA;/* CLEAR CORE                    */
         XC    EXECDATA(64,R6),EXECDATA(R6)                        0137
*       IF NESTED=YES THEN          /* IF THIS IS A NESTED EXEC THEN */
         TM    ECDAFLAG(ECDAPTR7),NESTED                           0138
         BNO   @RF00138                                            0138
*         GEXECDAT=ECDAGDAT->GEXECDAT;/* PICK UP ADDRESS OF NEXT   0139
*                                      LOWER EXECDATA OTHERWISE      */
         L     R1,ECDAGDAT(,ECDAPTR7)                              0139
         L     R1,GEXECDAT(,R1)                                    0139
         ST    R1,GEXECDAT(,R6)                                    0139
*       ELSE                        /* SAVE THE ADDRESS OF FIRST     */
*         GEXECDAT=ADDR(EXECDATA);  /* LEVEL EXECDATA                */
         B     @RC00138                                            0140
@RF00138 L     R6,LSDPTR(,ECDAPTR7)                                0140
         L     R6,LSDEXEC-LSD(,R6)                                 0140
         ST    R6,GEXECDAT(,R6)                                    0140
*       SNTABFST=LSDEXEC+(LENGTH(EXECDATA)+CON7)/CON8*CON8;/* SPLIT  */
@RC00138 L     R6,LSDPTR(,ECDAPTR7)                                0141
         L     R6,LSDEXEC-LSD(,R6)                                 0141
         LA    R14,64                                              0141
         ALR   R14,R6                                              0141
         ST    R14,SNTABFST(,R6)                                   0141
*       SNTABPTR=SNTABFST;          /* REMAINING CORE BETWEEN SNTAB
*                                      AND SVTAB                     */
         ST    R14,SNTABPTR                                        0142
*       SVTABFST=SNTABPTR+((((CPBLKPTR-SNTABBLK)/CON2)+CON7)/CON8*CON8)
*           ;                                                      0143
         SLR   R15,R15                                             0143
         ICM   R15,7,SNTABBLK                                      0143
         SLR   R0,R0                                               0143
         ICM   R0,7,CPBLKPTR(ECDAPTR7)                             0143
         LR    R2,R0                                               0143
         SLR   R2,R15                                              0143
         SRDA  R2,32                                               0143
         D     R2,FW02                                             0143
         LR    R4,R3                                               0143
         AL    R4,FW07                                             0143
         SRDA  R4,32                                               0143
         D     R4,FW08                                             0143
         SLA   R5,3                                                0143
         ALR   R5,R14                                              0143
         ST    R5,SVTABFST(,R6)                                    0143
*       SVTABPTR=SVTABFST;                                         0144
*                                                                  0144
         ST    R5,SVTABPTR                                         0144
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE FIRST SNTAB                                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0145
*       SNTABNXT=CON0;              /* ONLY ONE BLOCK                */
         SLR   R6,R6                                               0145
         ST    R6,SNTABNXT(,R14)                                   0145
*       SNTABLNG=SVTABBLK-SNTABBLK; /* TOTAL LENGTH                  */
         SLR   R1,R1                                               0146
         ICM   R1,7,SVTABBLK                                       0146
         LCR   R15,R15                                             0146
         ALR   R15,R1                                              0146
         ST    R15,SNTABLNG(,R14)                                  0146
*       SNTABUSE=LENGTH(SNTAB);     /* 12 BYTES OCCUPIED BY HEADER   */
         MVC   SNTABUSE(4,R14),FW12                                0147
*       SNTELPTR=ADDR(SNTELFST);    /* FIRST AVAILABLE SLOT          */
*                                                                  0148
         LA    SNTELPTR,SNTELFST(,R14)             R8=SNTELPTR     0148
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE FIRST SVTAB                                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0149
*       SVTABNXT=CON0;              /* ONLY ONE BLOCK                */
         ST    R6,SVTABNXT(,R5)                                    0149
*       SVTABLNG=CPBLKPTR-SVTABBLK; /* TOTAL LENGTH                  */
         SLR   R0,R1                                               0150
         ST    R0,SVTABLNG(,R5)                                    0150
*       SVTABUSE=LENGTH(SVTAB);     /* 16 BYTES OCCUPIED BY HEADER   */
         MVC   SVTABUSE(4,R5),FW16                                 0151
*       SVTABFRE=CON0;              /* NO UNUSED BYTES               */
         ST    R6,SVTABFRE(,R5)                                    0152
*       SVTELPTR=ADDR(SVTELFST);    /* ALL CONTROL BLOCKS ARE BUILT  */
         LA    SVTELPTR,SVTELFST(,R5)                              0153
*       SP78BLK=NO;                                                0154
         NI    ECDAFLAG(ECDAPTR7),255-SP78BLK                      0154
*                                                                  0155
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE THE CONTROL VARIABLES INITIALIZE A NULL VALUE  */
*       /* ELEMENT AND ALL CONTROL VARIABLES WITH A NULL VALUE LIST  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0155
*       SVTELPTR->SVTLNG=CON0;      /* NULL ELEMENT LENGTH           */
         STH   R6,SVTLNG(,SVTELPTR)                                0155
*       SVTELPTR->SVTORIG=CON0;     /* NULL ELEMENT FREE LENGTH      */
         STH   R6,SVTORIG(,SVTELPTR)                               0156
*       CALL SVTELUPT;                                             0157
         BAL   R14,SVTELUPT                                        0157
*       RFY                                                        0158
*        (R2,                                                      0158
*         R3,                                                      0158
*         R4,                                                      0158
*         R5) RSTD;                                                0158
*       R2=ADDR(CVARMAP);           /* ADDRESS OF CVAR AREA          */
         L     R2,SNTABPTR                                         0159
         LA    R2,CVARMAP(,R2)                                     0159
*       R3=LENGTH(CVARMAP);         /* LENGTH TO INITIALIZE          */
         LA    R3,272                                              0160
*       R4=R2;                      /* NO DATA TO MOVE               */
         LR    R4,R2                                               0161
*       R5=CON0;                    /* PADS CVARMAP WITH ZEROS       */
         SLR   R5,R5                                               0162
*       MVCL(R2,R4);                /* ZERO AREA                     */
         MVCL  R2,R4                                               0163
*       RFY                                                        0164
*        (R2,                                                      0164
*         R3,                                                      0164
*         R4,                                                      0164
*         R5) UNRSTD;                                              0164
*       DO ICTR=1 TO DIM(NULLVAR);  /* INITIALIZE NULL VALUE       0165
*                                      VARIABLES                     */
         LA    R6ICTR,1                                            0165
@DL00165 DS    0H                                                  0166
*         SNTDATA(1:NULLEN(ICTR))=NULLADR(ICTR)->NULLVDAT(1:NULLEN(ICTR
*             ));                                                  0166
         LR    R14,R6ICTR                                          0166
         SLA   R14,2                                               0166
         SLR   R15,R15                                             0166
         IC    R15,NULLEN-4(R14)                                   0166
         LR    R0,R15                                              0166
         BCTR  R0,0                                                0166
         LA    R1,NULLADR-4(R14)                                   0166
         SLR   R14,R14                                             0166
         ICM   R14,7,0(R1)                                         0166
         LR    R1,R0                                               0166
         EX    R1,@SM01540                                         0166
*         SNTLNG=NULLEN(ICTR);                                     0167
         STH   R15,SNTLNG(,SNTELPTR)                               0167
*         SNTVLPTR=ADDR(SVTELFST);                                 0168
         L     R1,SVTABPTR                                         0168
         LA    R2,SVTELFST(,R1)                                    0168
         ST    R2,SNTVLPTR(,SNTELPTR)                              0168
         C     R6ICTR,FW#EVAL       NEW IMMEDIATE EVALUATION?   ZP60014
         BH    @SYMEVAL             YES, GO SET THE FLAG        ZP60014
*         IF ICTR<CON11 THEN        /* FIRST 10 VARIABLES REQUIRE    */
         C     R6ICTR,FW11                                         0169
         BNL   @RF00169                                            0169
@SYMEVAL EQU   *                                                ZP60014
*           SNTEVAL=YES;            /* IMMEDIATE EVALUATION          */
         OI    SNTEVAL(SNTELPTR),B'00000010'                       0170
*         IF ICTR<=CON15 THEN       /* FIRST 15 VARIABLES CAN NOT    */
@RF00169 EQU   *                                                   0171
         C     R6ICTR,FW#GRP1       NO USER UPDATE 2ND GROUP?   ZP60014
         BH    @SYMNOUU             YES, SET THE FLAG           ZP60014
         C     R6ICTR,FW15                                         0171
         BH    @RF00171                                            0171
@SYMNOUU EQU   *                                                ZP60014
*           SNTNAUTH=YES;           /* BE SET BY USER                */
         OI    SNTNAUTH(SNTELPTR),B'00000100'                      0172
*         SNTABUSE=SNTABUSE+SNTLNG+LENGTH(SNTELEM);                0173
@RF00171 L     R3,SNTABPTR                                         0173
         LA    R2,8                                                0173
         LH    R4,SNTLNG(,SNTELPTR)                                0173
         L     R5,SNTABUSE(,R3)                                    0173
         ALR   R5,R4                                               0173
         ALR   R5,R2                                               0173
         ST    R5,SNTABUSE(,R3)                                    0173
*         SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);                0174
         ALR   R4,SNTELPTR                                         0174
         ALR   R4,R2                                               0174
         LR    SNTELPTR,R4                         R8=SNTELPTR     0174
*       END;                                                       0175
         AL    R6ICTR,FW01                                         0175
         C     R6ICTR,FW#SYM        ORIGINALLY 19         ZP60014  0175
         BNH   @DL00165                                            0175
*       RFY                                                        0176
*         SVTELEM BASED(SVTELPTR);                                 0176
*                                                                  0177
*       /*************************************************************/
*       /*                                                           */
*       /* INITIALIZE THOSE CONTROL VARIABLES REQUIRING VALUES       */
*       /* -SYSUID                                                   */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0177
*       SVTDATA(1:PSCBUSRL)=PSCBUSER(1:PSCBUSRL);                  0177
         L     R6,CPPLPTR(,ECDAPTR7)                               0177
         L     R6,CPPLPSCB-CPPL(,R6)                               0177
         SLR   R14,R14                                             0177
         IC    R14,PSCBUSRL-PSCB(,R6)                              0177
         LR    R15,R14                                             0177
         BCTR  R15,0                                               0177
         EX    R15,@SM01544                                        0177
*       SVTLNG=PSCBUSRL;            /* USERID LENGTH                 */
         STH   R14,SVTLNG(,SVTELPTR)                               0178
*       SVTORIG=SVTLNG;             /* ORIGIONAL LENGTH SAME         */
         STH   R14,SVTORIG(,SVTELPTR)                              0179
*       CUIDVAL=SVTELPTR;           /* SNTAB VALUE POINTER           */
         L     R6,SNTABPTR                                         0180
         STCM  SVTELPTR,15,CUIDVAL(R6)                             0180
*       CALL SVTELUPT;                                             0181
*                                                                  0181
         BAL   R14,SVTELUPT                                        0181
*                                                                  0182
*       /*************************************************************/
*       /*                                                           */
*       /* -SYSPROC                                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0182
*       DO ICTR=8 TO 1 BY-1 WHILE(CSCBPROC(ICTR)=BLANK);/* DETERMINE */
         LA    R6ICTR,8                                            0192
@DL00192 L     R1,TIOTPTR                                          0192
         ALR   R1,R6ICTR                                           0192
         CLI   CSCBPROC-1(R1),C' '                                 0192
         BNE   @DC00192                                            0192
*       END;                        /* LENGTH OF THE PROCEDURE NAME  */
         BCTR  R6ICTR,0                                            0193
         LTR   R6ICTR,R6ICTR                                       0193
         BP    @DL00192                                            0193
@DC00192 DS    0H                                                  0194
*       IF ICTR>CON0 THEN           /* MOVE PROCNAME IN IF PRESENT   */
         LTR   R6ICTR,R6ICTR                                       0194
         BNP   @RF00194                                            0194
*         DO;                                                      0195
*           SVTDATA(1:ICTR)=CSCBPROC(1:ICTR);                      0196
         LR    R2,R6ICTR                                           0196
         BCTR  R2,0                                                0196
         L     R1,TIOTPTR                                          0196
         EX    R2,@SM01546                                         0196
*           SVTLNG=ICTR;                                           0197
         STH   R6ICTR,SVTLNG(,SVTELPTR)                            0197
*           SVTORIG=ICTR;                                          0198
         STH   R6ICTR,SVTORIG(,SVTELPTR)                           0198
*           CPROCVAL=SVTELPTR;                                     0199
         L     R6,SNTABPTR                                         0199
         STCM  SVTELPTR,15,CPROCVAL(R6)                            0199
*           CALL SVTELUPT;                                         0200
         BAL   R14,SVTELUPT                                        0200
*         END;                                                     0201
*       ELSE                        /* IF NO PROCNAME THEN SET TO    */
*         CPROCVAL=ADDR(SVTELFST);  /* NULL ELEMENT                  */
         B     @RC00194                                            0202
@RF00194 L     R6,SVTABPTR                                         0202
         LA    R6,SVTELFST(,R6)                                    0202
         L     R1,SNTABPTR                                         0202
         STCM  R6,15,CPROCVAL(R1)                                  0202
*                                                                  0203
*       /*************************************************************/
*       /*                                                           */
*       /* - LASTCC                                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0203
*       SVTDATA(1)=CONC0;           /* LASTCC INITIALIZED TO ZERO    */
@RC00194 MVI   SVTDATA(SVTELPTR),C'0'                              0203
*       SVTLNG=LENGTH(CONC0);                                      0204
         MVC   SVTLNG(2,SVTELPTR),HW01                             0204
*       SVTORIG=CON4;               /* RESERVE FOUR BYTES            */
         MVC   SVTORIG(2,SVTELPTR),HW04                            0205
*       CLCCVAL=SVTELPTR;                                          0206
         L     R6,SNTABPTR                                         0206
         STCM  SVTELPTR,15,CLCCVAL(R6)                             0206
*       SVTABFRE=SVTABFRE+CON3;     /* UP FREE SPACE                 */
         L     R6,SVTABPTR                                         0207
         LA    R14,3                                               0207
         AL    R14,SVTABFRE(,R6)                                   0207
         ST    R14,SVTABFRE(,R6)                                   0207
*       CALL SVTELUPT;                                             0208
*                                                                  0208
         BAL   R14,SVTELUPT                                        0208
*                                                                  0209
*       /*************************************************************/
*       /*                                                           */
*       /* -MAXCC                                                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0209
*       SVTDATA(1)=CONC0;                                          0209
         MVI   SVTDATA(SVTELPTR),C'0'                              0209
*       SVTLNG=LENGTH(CONC0);                                      0210
         MVC   SVTLNG(2,SVTELPTR),HW01                             0210
*       SVTORIG=CON4;                                              0211
         MVC   SVTORIG(2,SVTELPTR),HW04                            0211
*       CMCCVAL=SVTELPTR;                                          0212
         L     R6,SNTABPTR                                         0212
         ST    SVTELPTR,CMCCVAL(,R6)                               0212
*       SVTABFRE=SVTABFRE+CON3;                                    0213
         L     R6,SVTABPTR                                         0213
         LA    R14,3                                               0213
         AL    R14,SVTABFRE(,R6)                                   0213
         ST    R14,SVTABFRE(,R6)                                   0213
*       CALL SVTELUPT;                                             0214
*                                                                  0214
         BAL   R14,SVTELUPT                                        0214
*                                                                  0215
*       /*************************************************************/
*       /*                                                           */
*       /* -SYSSCAN                                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0215
*       SVTDATA(1:2)=CONC16;        /* DEFAULT IS 16                 */
         MVC   SVTDATA(2,SVTELPTR),$CHAR16                         0215
*       SVTLNG=LENGTH(CONC16);                                     0216
         LA    R6,2                                                0216
         STH   R6,SVTLNG(,SVTELPTR)                                0216
*       SVTORIG=CON4;               /* FOUR BYTES ALLOCATED TO THIS  */
         MVC   SVTORIG(2,SVTELPTR),HW04                            0217
*       CSCANVAL=SVTELPTR;          /* VARIABLE                      */
         L     R1,SNTABPTR                                         0218
         STCM  SVTELPTR,15,CSCANVAL(R1)                            0218
*       SVTABFRE=SVTABFRE+CON2;                                    0219
         L     R2,SVTABPTR                                         0219
         AL    R6,SVTABFRE(,R2)                                    0219
         ST    R6,SVTABFRE(,R2)                                    0219
*       CALL SVTELUPT;                                             0220
         BAL   R14,SVTELUPT                                        0220
*                                                                  0221
*       /*************************************************************/
*       /*                                                           */
*       /* -SYSDLM                                                   */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0221
*       SVTDATA(1)=CONC0;           /* DEFAULT VALUE IS 0            */
         MVI   SVTDATA(SVTELPTR),C'0'                              0221
*       SVTLNG=LENGTH(CONC0);                                      0222
         LA    R6,1                                                0222
         STH   R6,SVTLNG(,SVTELPTR)                                0222
*       SVTORIG=CON2;                                              0223
         MVC   SVTORIG(2,SVTELPTR),HW02                            0223
*       CDLMVAL=SVTELPTR;                                          0224
         L     R1,SNTABPTR                                         0224
         STCM  SVTELPTR,15,CDLMVAL(R1)                             0224
*       SVTABFRE=SVTABFRE+CON1;                                    0225
         L     R2,SVTABPTR                                         0225
         AL    R6,SVTABFRE(,R2)                                    0225
         ST    R6,SVTABFRE(,R2)                                    0225
*       CALL SVTELUPT;                                             0226
*                                                                  0226
         BAL   R14,SVTELUPT                                        0226
*                                                                  0227
*       /*************************************************************/
*       /*                                                           */
*       /* -SYSNEST                                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0227
*       IF NESTED=YES THEN          /* IF WE ARE EXECUTING IN A      */
         TM    ECDAFLAG(ECDAPTR7),NESTED                           0227
         BNO   @RF00227                                            0227
*         DO;                       /* NESTED LEVEL THEN VALUE IS    */
*           SVTDATA(1:LENGTH(NYES))=NYES;/* YES                      */
         MVC   SVTDATA(3,SVTELPTR),$YES                            0229
*           SVTORIG=LENGTH(NYES);                                  0230
         MVC   SVTORIG(2,SVTELPTR),HW03                            0230
*         END;                                                     0231
*       ELSE                        /* ELSE IT IS NO                 */
*         DO;                                                      0232
         B     @RC00227                                            0232
@RF00227 DS    0H                                                  0233
*           SVTDATA(1:LENGTH(NNO))=NNO;                            0233
         MVC   SVTDATA(2,SVTELPTR),$NO                             0233
*           SVTORIG=LENGTH(NNO);                                   0234
         MVC   SVTORIG(2,SVTELPTR),HW02                            0234
*         END;                                                     0235
*       SVTLNG=SVTORIG;             /* LENGTH IS SAME AS ORIGIONAL 0236
*                                      LNG                           */
@RC00227 LH    R6,SVTORIG(,SVTELPTR)                               0236
         STH   R6,SVTLNG(,SVTELPTR)                                0236
*       CNESTVAL=SVTELPTR;          /* SAVE ADDRESS OF VALUE         */
         L     R6,SNTABPTR                                         0237
         ST    SVTELPTR,CNESTVAL(,R6)                              0237
*       CALL SVTELUPT;                                             0238
*                                                                  0238
         BAL   R14,SVTELUPT                                        0238
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSOUTLINE                                      ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         L     R14,CPPLPTR(,ECDAPTR7)
         L     R14,CPPLECT-CPPL(,R14)
         XC    44(4,R14),44(R14)           RESET ECTNUM
         MVI   SVTDATA(SVTELPTR),C'0'      SET VALUE FOR SYSOUTLINE
         LA    R0,1                        GET THE VALUE LENGTH
         STH   R0,SVTLNG(,SVTELPTR)        SET CURRENT VALUE LENGTH
         LA    R0,10                       GET THE VALUE LENGTH LIMIT
         STH   R0,SVTORIG(,SVTELPTR)       SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,COUTLINE(R6)    SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSOUTTRAP                                      ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         MVI   SVTDATA(SVTELPTR),C'0'      SET VALUE FOR SYSOUTLINE
         LA    R0,1                        GET THE VALUE LENGTH
         STH   R0,SVTLNG(,SVTELPTR)        SET CURRENT VALUE LENGTH
         LA    R0,10                       GET THE VALUE LENGTH LIMIT
         STH   R0,SVTORIG(,SVTELPTR)       SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,COUTTRAP(R6)    SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSENV                                          ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         L     R2,PSAAOLD                  POINT TO THE CURRENT ASCB
         ICM   R1,15,$FORE                 PREPARE FOR FOREGROUND
         ICM   R0,15,ASCBTSB-ASCB(R2)      GET ADDRESS OF TSB
         BNZ   GROUNDOK                    IT EXISTS SO IN FOREGROUND
         ICM   R1,15,$BACK                 NOT THERE SO IN BACKGROUND
GROUNDOK STCM  R1,15,SVTDATA(SVTELPTR)     SET VALUE FOR SYSENV
         LA    R0,4                        GET THE VALUE LENGTH
         STH   R0,SVTLNG(,SVTELPTR)        SET CURRENT VALUE LENGTH
         STH   R0,SVTORIG(,SVTELPTR)       SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,CENVVAL(R6)     SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSTERMID                                       ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         LA    R0,GTTRMPRM
         ST    R0,GTTRMLST
         LA    R0,GTTRMALT
         ST    R0,GTTRMLST+4
         LA    R0,GTTRMATR
         ST    R0,GTTRMLST+8
         LA    R0,GTTRMNAM
         ST    R0,GTTRMLST+12
         OI    GTTRMLST+12,X'80'           FLAG LAST PARAMETER
         LA    R1,GTTRMLST                 POINT TO PLIST
         LA    R0,17                       GET GTTERM MACRO ID
         SLL   R0,24                       PROMOTE TO FLAG BYTE
         SVC   94                          ISSUE GTTERM
         LTR   R15,R15                     SUCCESS?
         BNZ   TERMDONE                    NO, CANNOT SET TERMINAL ID
         LA    R1,GTTRMNAM                 YES, POINT TO TERMINAL NAME
         LA    R6ICTR,8                    GET MAXIMUM NAME LENGTH
TERMBLNK LA    R2,0(R6ICTR,R1)             POINT PAST TRAILING CHAR
         BCTR  R2,0                        POINT TO TRAILING CHAR
         CLI   0(R2),C' '                  TRAILING BLANK?
         BH    TERMOKAY                    NO, HAVE LENGTH
         BCT   R6ICTR,TERMBLNK             YES, DECREMENT LENGTH
         B     TERMDONE                    TERMINAL NAME NOT ACQUIRED
TERMOKAY LTR   R2,R6ICTR                   GET LENGTH OF TERMINAL NAME
         BNP   TERMDONE                    NAME NOT SUPPLIED
         BCTR  R2,0                        GET TERMINAL ID LENGTH CODE
         EX    R2,@SM01548                 MOVE IN TERMINAL NAME
         STH   R6ICTR,SVTLNG(,SVTELPTR)    SET CURRENT VALUE LENGTH
         STH   R6ICTR,SVTORIG(,SVTELPTR)   SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,CTERMVAL(R6)    SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
TERMDONE EQU   *
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSSMFID                                        ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         L     R2,CVTPTR                   ---> CVT
         L     R2,CVTSMCA-CVT(,R2)         ---> SMCA
         LA    R1,SMCASID-SMCABASE(,R2)    ---> SMCASID
         LA    R6ICTR,3
SMFBLANK LA    R2,0(R6ICTR,R1)             ---> END OF SMCASID
         CLI   0(R2),C' '                  TRAILING BLANK?
         BNE   SMFIDOK                     NO, HAVE LENGTH
         BCT   R6ICTR,SMFBLANK             YES, DECREMENT LENGTH
SMFIDOK  LR    R2,R6ICTR                   GET LENGTH CODE OF SID
         EX    R2,@SM01548                 MOVE IN SMF ID
         LA    R6ICTR,1(,R6ICTR)           RESTORE LENGTH
         STH   R6ICTR,SVTLNG(,SVTELPTR)    SET CURRENT VALUE LENGTH
         STH   R6ICTR,SVTORIG(,SVTELPTR)   SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,CSMFVAL(R6)     SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSOPSYS                                        ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         MVC   SVTDATA(L'$OSLEVEL,SVTELPTR),$OSLEVEL
         LA    R0,L'$OSLEVEL               GET THE VALUE LENGTH
         STH   R0,SVTLNG(,SVTELPTR)        SET CURRENT VALUE LENGTH
         STH   R0,SVTORIG(,SVTELPTR)       SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,COPERVAL(R6)    SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*
*       /*************************************************************/
*       /*                                                  ZP60014  */
*       /* -SYSJES                                          ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         L     R14,PSATOLD                 ---> TCB
         L     R14,TCBJSCB-TCB(,R14)       ---> JSCB
         L     R14,JSCBSSIB-IEZJSCB(,R14)  ---> SSIB
         LA    R1,SSIBSSNM-SSIB(,R14)      ---> JOB'S SUBSYSTEM NAME
         LA    R6ICTR,4                    GET MAXIMUM NAME LENGTH
JSNMBLNK LA    R2,0(R6ICTR,R1)             POINT PAST TRAILING CHAR
         BCTR  R2,0                        POINT TO TRAILING CHAR
         CLI   0(R2),C' '                  TRAILING BLANK?
         BH    JSNMOKAY                    NO, HAVE LENGTH
         BCT   R6ICTR,JSNMBLNK             YES, DECREMENT LENGTH
         B     JSNMDONE                    JES NAME NOT ACQUIRED
JSNMOKAY LTR   R2,R6ICTR                   GET LENGTH OF JES NAME
         BNP   JSNMDONE                    NAME NOT SUPPLIED
         BCTR  R2,0                        GET JES NAME LENGTH CODE
         EX    R2,@SM01548                 MOVE IN JES NAME
         STH   R6ICTR,SVTLNG(,SVTELPTR)    SET CURRENT VALUE LENGTH
         STH   R6ICTR,SVTORIG(,SVTELPTR)   SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,CJESVAL(R6)     SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
JSNMDONE EQU   *
*
*       /*************************************************************/
*       /*                                 2020-12-07       ZP60014  */
*       /* -SYSISPF                                         ZP60014  */
*       /*                                                  ZP60014  */
*       /*************************************************************/
*
         SR    R15,R15                     CLEAR FOR INSERT
         L     R2,PSATOLD                  POINT TO THE CURRENT TCB
         MVC   SVTDATA(6,SVTELPTR),$ACTIVE LOAD VALUE
         LA    R0,6                        GET THE VALUE LENGTH
ISPFTASK ICM   R2,15,TCBOTC-TCB(R2)        POINT TO PARENT TASK
         BZ    ISPFNOT                     MUST HAVE GOT TO RCT
         CL    R2,TCBJSTCB-TCB(R2)         JOB STEP TCB?
         BE    ISPFNOT                     YES, ISPF NOT ACTIVE
         L     R14,TCBRBP-TCB(,R2)         POINT TO CURRENT RB
         ICM   R15,7,RBCDE1-RBBASIC(R14)   POINT TO THE CDE
         BZ    ISPFTASK                    NONE SO GO UP A TASK
         CLC   ISPFLIT,CDNAME-CDENTRY(R15) ISPF?
         BNE   ISPFTASK                    NO
         B     ISPFRDY                     YES
ISPFNOT  MVC   SVTDATA(L'$NOTACTV,SVTELPTR),$NOTACTV
         LA    R0,L'$NOTACTV               GET THE VALUE LENGTH
ISPFRDY  STH   R0,SVTLNG(,SVTELPTR)        SET CURRENT VALUE LENGTH
         STH   R0,SVTORIG(,SVTELPTR)       SET ORIGINAL VALUE LENGTH
         L     R6,SNTABPTR                 POINT TO SYMBOL NAME TABLE
         STCM  SVTELPTR,15,CISPFVAL(R6)    SET POINTER TO VALUE
         BAL   R14,SVTELUPT                UPDATE SYMBOL VALUE TABLE
*                                                                  0239
*       /*************************************************************/
*       /*                                                           */
*       /* -SYSICMD                                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0239
*       IF IMPLICIT=YES THEN        /* INIT TO IMPLICIT INVOC NAME   */
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0239
         BNO   @RF00239                                            0239
*         DO;                                                      0240
*           SVTDATA(1:ECDAILNG)=PROCNAME(1:ECDAILNG);/* IMPLICIT NME */
         SLR   R6,R6                                               0241
         IC    R6,ECDAILNG(,ECDAPTR7)                              0241
         LR    R14,R6                                              0241
         BCTR  R14,0                                               0241
         L     R1,ECDAINME(,ECDAPTR7)                              0241
         EX    R14,@SM01548                                        0241
*           SVTORIG=ECDAILNG;       /* SAVE LENGTH                   */
         STH   R6,SVTORIG(,SVTELPTR)                               0242
*           SVTLNG=SVTORIG;                                        0243
         STH   R6,SVTLNG(,SVTELPTR)                                0243
*           CICMDVAL=SVTELPTR;      /* ADDR OF VALUE                 */
         L     R6,SNTABPTR                                         0244
         STCM  SVTELPTR,15,CICMDVAL(R6)                            0244
*           CALL SVTELUPT;          /* UPDATE TABLE INFO             */
         BAL   R14,SVTELUPT                                        0245
*         END;                                                     0246
*       ECDALNEL=ADDR(CLASTVAL);                          ZP60014  0247
@RF00239 L     R14,SNTABPTR                                        0247
         LA    R15,CLASTVAL(,R14)                         ZP60014  0247
         ST    R15,ECDALNEL(,ECDAPTR7)                             0247
*       USNTABST=SNTABPTR+SNTABUSE;                                0248
         LR    R15,R14                                             0248
         AL    R15,SNTABUSE(,R14)                                  0248
         ST    R15,USNTABST                                        0248
*       RFY                                                        0249
*         SVTELEM BASED(SNTVLPTR);                                 0249
*                                                                  0249
*       /*************************************************************/
*       /*                                                           */
*       /* DETERMINE IF FIRST STATEMENT IS A PROC STATEMENT          */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0250
*       ICTR=CON1;                                                 0250
         LA    R6ICTR,1                                            0250
*       CALL SKIPSEP;               /* SKIP LEADING SEPERATORS       */
         BAL   R14,SKIPSEP                                         0251
*       SCTR=ICTR;                  /* SAVE START OF PARM            */
         LR    R2SCTR,R6ICTR                                       0252
         C     R2SCTR,LINELNG
         BNH   DOFNDSEP
         MVC   CT431RET(4),FW08
         B     @RC00112
*       CALL FINDSEP;               /* LOCATE THE NEXT SEPERATOR     */
DOFNDSEP BAL   R14,FINDSEP                                         0253
*       IF(ICTR-SCTR)^=LENGTH(CPROC) RECORD(SCTR:ICTR-CON1)^=CPROC THEN
         LR    R9,R6ICTR                                           0254
         SLR   R9,R2SCTR                                           0254
         C     R9,FW04                                             0254
         BNE   @RT00254                                            0254
         L     R3,ECDAIREC(,ECDAPTR7)                              0254
         ALR   R3,R2SCTR                                           0254
         CLC   RECORD-1(4,R3),$PROC                                0254
         BE    @RF00254                                            0254
@RT00254 DS    0H                                                  0255
*         DO;                                                      0255
*           IF(ECDAFLAG=NO&(VLST=YES&VLSTLNG^=CON0)) (IMPLICIT=YES&
*               ECTNOPD=NO) THEN    /* IF THERE WERE PARMS IN        */
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0256
         BNZ   @GL00002                                            0256
         L     R9,ECDAEANS(,ECDAPTR7)                              0256
         TM    VLST(R9),B'10000000'                                0256
         BNO   @GL00002                                            0256
         LH    R9,VLSTLNG(,R9)                                     0256
         LTR   R9,R9                                               0256
         BNZ   @RT00256                                            0256
@GL00002 TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0256
         BNO   @RF00256                                            0256
         L     R9,CPPLPTR(,ECDAPTR7)                               0256
         L     R9,CPPLECT-CPPL(,R9)                                0256
         TM    ECTSWS-ECT(R9),ECTNOPD                              0256
         BNZ   @RF00256                                            0256
@RT00256 DS    0H                                                  0257
*             DO;                   /* VALUE LIST AND NO PROC STMT   */
*               EXMSGID=A529;       /* THEN TELL USER THE VALUE LIST
*                                      PARMS ARE IGNORED             */
         MVC   EXMSGID(4),$MSGA529                                 0258
*               IF IMPLICIT=NO THEN /* EXPLICIT - PICK UP ADDRESS OF */
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0259
         BNZ   @RF00259                                            0259
*                 DO;               /* VALUE LIST FROM PARSE PDL     */
*                   MVAR(1)=VLSTPTR;                               0261
         L     R9,ECDAEANS(,ECDAPTR7)                              0261
         L     R15,VLSTPTR(,R9)                                    0261
         ST    R15,MVAR                                            0261
*                   MVARLEN(1)=VLSTLNG;                            0262
         LH    R9,VLSTLNG(,R9)                                     0262
         STC   R9,MVARLEN                                          0262
*                 END;                                             0263
*               ELSE                /* IMPLICIT - PICK UP ADDRESS    */
*                 DO;               /* OF VALUE LIST ADDRESS USING   */
         B     @RC00259                                            0264
@RF00259 DS    0H                                                  0265
*                   MVAR(1)=ADDR(CBUFTEXT)+CBUFOFF;/* CMD BUFFER   0265
*                                      OFFSETS                       */
         L     R9,CPPLPTR(,ECDAPTR7)                               0265
         L     R9,CPPLCBUF-CPPL(,R9)                               0265
         LA    R14,CBUFTEXT(,R9)                                   0265
         LH    R15,CBUFOFF(,R9)                                    0265
         ALR   R14,R15                                             0265
         ST    R14,MVAR                                            0265
*                   MVARLEN(1)=CBUFLNG-CBUFOFF;                    0266
         LCR   R15,R15                                             0266
         AH    R15,CBUFLNG(,R9)                                    0266
         STC   R15,MVARLEN                                         0266
*                 END;                                             0267
*               CALL MSGRTN;                                       0268
@RC00259 BAL   R14,MSGRTN                                          0268
*             END;                                                 0269
*
*
*             CT431RET=CON0;
@RF00256 SLR   R9,R9
         ST    R9,CT431RET
         B     @RF00270                                            0272
*         END;                                                     0273
*                                                                  0273
*       /*************************************************************/
*       /*                                                           */
*       /* CONTROL RETURNS TO IKJCT431 EXIT POINT                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0274
*                                                                  0274
*       /*************************************************************/
*       /*                                                           */
*       /* SYNTAX CHECK THE POSITIONAL SPECIFICATION                 */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0274
*       ELSE                        /* CHECK POSIT SPECIFICATION     */
*         DO;                                                      0274
@RF00254 DS    0H                                                  0275
*           CT431RET=CON4;                                         0275
         MVC   CT431RET(4),FW04                                    0275
*                                   /* STMTS OTHER THAN PROC STMT ARE
*                                      RETURNED TO IKJCT430 SO THEY  */
*                                   /* WILL BE PROCESSED BY IKJCT432 */
@RF00270 CLC   CT431RET(4),FW04
         BNE   @RC00112
*           CALL SKIPSEP;           /* FIND START OF NUMBER          */
         BAL   R14,SKIPSEP                                         0276
*           SCTR=ICTR;                                             0277
         LR    R2SCTR,R6ICTR                                       0277
*           CALL FINDSEP;           /* FIND END OF NUMBER            */
         BAL   R14,FINDSEP                                         0278
*           IF ICTR-SCTR>CON0&ICTR-SCTR<CON9 THEN/* IF NUMBER EIGHT
*                                      OR LESS BYTES                 */
         LR    R3,R6ICTR                                           0279
         SLR   R3,R2SCTR                                           0279
         LTR   R3,R3                                               0279
         BNP   @RF00279                                            0279
         C     R3,FW09                                             0279
         BNL   @RF00279                                            0279
*             DO;                   /* LONG THEN IT MAY BE VALID-    */
*               PCTR=CON8;          /* OVER EIGHT BYTES IT IS INVALID*/
         LA    R5PCTR,8                                            0281
*               PACKLOC='';                                        0282
*                                                                  0282
         MVI   PACKLOC,C' '                                        0282
         MVC   PACKLOC+1(7),PACKLOC                                0282
*               /*****************************************************/
*               /*                                                   */
*               /* PREPARE TO PACK AND CONVERT POSITIONAL            */
*               /* SPECIFICATION                                     */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0283
*               DO JCTR=ICTR-CON1 TO SCTR BY-1 WHILE RECORD(JCTR)^<
*                     CONC0&RECORD(JCTR)^>CONC9;                   0283
         LR    R9,R6ICTR                                           0283
         BCTR  R9,0                                                0283
         ST    R9,JCTR                                             0283
         B     @DE00283                                            0283
@DL00283 L     R3,ECDAIREC(,ECDAPTR7)                              0283
         ALR   R9,R3                                               0283
         CLI   RECORD-1(R9),C'0'                                   0283
         BL    @DC00283                                            0283
         L     R4,JCTR                                             0283
         ALR   R4,R3                                               0283
         CLI   RECORD-1(R4),C'9'                                   0283
         BH    @DC00283                                            0283
*                 PACKLOC(PCTR)=RECORD(JCTR);/* POSITIONAL         0284
*                                      SPECIFICATION MUST            */
         LA    R9,PACKLOC-1(R5PCTR)                                0284
         L     R4,JCTR                                             0284
         ALR   R3,R4                                               0284
         MVC   0(1,R9),RECORD-1(R3)                                0284
*                 PCTR=PCTR-CON1;   /* BE NUMERIC                    */
         BCTR  R5PCTR,0                                            0285
*               END;                                               0286
         BCTR  R4,0                                                0286
         LR    R9,R4                                               0286
         ST    R9,JCTR                                             0286
@DE00283 CR    R9,R2SCTR                                           0286
         BNL   @DL00283                                            0286
@DC00283 DS    0H                                                  0287
*               IF JCTR^<SCTR THEN  /* IF ALL DIGITS WERE NOT MOVED  */
         C     R2SCTR,JCTR                                         0287
         BH    @RF00287                                            0287
*                 POSPCERR=YES;     /* IN THEN ONE OF THE DIGITS WAS
*                                      NOT NUMERIC - INDICATE      0288
*                                      POSITIONAL SPECIFICATION    0288
*                                      INVALID                       */
         OI    POSPCERR,B'01000000'                                0288
*               ELSE                /* CONVERT THE NUMBER TO BINARY  */
*                 DO;               /* AND SAVE                      */
         B     @RC00287                                            0289
@RF00287 DS    0H                                                  0290
*                   PACK(CVERTLOC,PACKLOC);                        0290
         PACK  CVERTLOC(8),PACKLOC(8)                              0290
*                   CVB(POSNUM,CVERTLOC);                          0291
         CVB   R9,CVERTLOC                                         0291
         ST    R9,POSNUM                                           0291
*                 END;                                             0292
*             END;                                                 0293
*           ELSE                                                   0294
*             POSPCERR=YES;                                        0294
         B     @RC00279                                            0294
@RF00279 OI    POSPCERR,B'01000000'                                0294
*           IF POSPCERR=YES THEN    /* IF POSITIONAL SPECIFICATION   */
@RC00279 TM    POSPCERR,B'01000000'                                0295
         BNO   @RF00295                                            0295
*             DO;                   /* INVALID NOTIFY USER AND       */
*               NOTEXEC=YES;        /* EXIT TO IKJCT430              */
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0297
*               MVAR(1)=ADDR(RECORD(SCTR));                        0298
         L     R3,ECDAIREC(,ECDAPTR7)                              0298
         LA    R3,RECORD-1(R2SCTR,R3)                              0298
         ST    R3,MVAR                                             0298
*               MVARLEN(1)=ICTR-SCTR;                              0299
         LR    R3,R6ICTR                                           0299
         SLR   R3,R2SCTR                                           0299
         STC   R3,MVARLEN                                          0299
*               EXMSGID=M506;                                      0300
         MVC   EXMSGID(4),$MSGM506                                 0300
*               CALL MSGRTN;                                       0301
         BAL   R14,MSGRTN                                          0301
*             END;                                                 0302
*                                                                  0303
*           /*********************************************************/
*           /*                                                       */
*           /* SYNTAX CHECK EACH POSITIONAL PARAMETER                */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0303
*           ELSE                    /* POSIT SPECIFICATION O.K.      */
*             DO;                                                  0303
         B     @RC00295                                            0303
@RF00295 DS    0H                                                  0304
*               POSCHAR=CON0;                                      0304
         SLR   R4POSCHR,R4POSCHR                                   0304
*               CALL SKIPSEP;       /* SKIP PRECEEDING SEPERATORS    */
         BAL   R14,SKIPSEP                                         0305
*               DO PCTR=1 TO POSNUM WHILE ICTR<=LINELNG&STABERR=NO;/*
*                                      FIND PARAMETER START          */
         LA    R5PCTR,1                                            0306
         B     @DE00306                                            0306
@DL00306 C     R6ICTR,LINELNG                                      0306
         BH    @DC00306                                            0306
         TM    STABERR,B'10000000'                                 0306
         BNZ   @DC00306                                            0306
*                 SCTR=ICTR;        /* SAVE START OF PARAMETER       */
         LR    R2SCTR,R6ICTR                                       0307
*                 POSERR=NO;                                       0308
         NI    POSERR,B'11011111'                                  0308
*                 IF(RECORD(SCTR)^<CONCA&/* FIRST CHARACTER MUST BE  */
*                     RECORD(SCTR)^>CONCI) (RECORD(SCTR)^<CONCJ&RECORD(
*                     SCTR)^>CONCR) (RECORD(SCTR)^<CONCS&RECORD(SCTR)^>
*                     CONCZ) THEN   /* ALPHA                         */
         L     R9,ECDAIREC(,ECDAPTR7)                              0309
         LR    R3,R9                                               0309
         ALR   R3,R2SCTR                                           0309
         CLI   RECORD-1(R3),C'A'                                   0309
         BL    @GL00010                                            0309
         ALR   R9,R2SCTR                                           0309
         CLI   RECORD-1(R9),C'I'                                   0309
         BNH   @RT00309                                            0309
@GL00010 L     R9,ECDAIREC(,ECDAPTR7)                              0309
         LR    R3,R9                                               0309
         ALR   R3,R2SCTR                                           0309
         CLI   RECORD-1(R3),C'J'                                   0309
         BL    @GL00009                                            0309
         ALR   R9,R2SCTR                                           0309
         CLI   RECORD-1(R9),C'R'                                   0309
         BNH   @RT00309                                            0309
@GL00009 L     R9,ECDAIREC(,ECDAPTR7)                              0309
         LR    R3,R9                                               0309
         ALR   R3,R2SCTR                                           0309
         CLI   RECORD-1(R3),C'S'                                   0309
         BL    @RF00309                                            0309
         ALR   R9,R2SCTR                                           0309
         CLI   RECORD-1(R9),C'Z'                                   0309
         BH    @RF00309                                            0309
@RT00309 DS    0H                                                  0310
*                   CALL VALIDATE;                                 0310
         BAL   R14,VALIDATE                                        0310
*                 ELSE              /* IF FIRST CHARACTER NOT ALPHA  */
*                   POSERR=YES;     /* THEN IT IS AN ERROR           */
         B     @RC00309                                            0311
@RF00309 OI    POSERR,B'00100000'                                  0311
*                 IF POSERR=NO&     /* IF THERE HAVE BEEN NO ERRORS  */
*                     VALIDERR=NO&  /* AND THE PARAMETER HAS A GOOD  */
*                     JCTR-SCTR<CON253 THEN/* LENGTH                 */
@RC00309 TM    POSERR,B'00100100'                                  0312
         BNZ   @RF00312                                            0312
         L     R9,JCTR                                             0312
         SLR   R9,R2SCTR                                           0312
         C     R9,FW253                                            0312
         BNL   @RF00312                                            0312
*                   DO;                                            0313
*                     PARMADR=ADDR(RECORD(SCTR));/* USE THE        0314
*                                      SNTAB/SVTAB                   */
         L     R3,ECDAIREC(,ECDAPTR7)                              0314
         LA    R3,RECORD-1(R2SCTR,R3)                              0314
         ST    R3,PARMADR                                          0314
*                     PARMLNG=JCTR-SCTR;/* UPDATE ROUTINE TO PLACE IN*/
         ST    R9,PARMLNG                                          0315
*                     POSCHAR=POSCHAR+PARMLNG;/* TABLES              */
         ALR   R4POSCHR,R9                                         0316
*                     PARMTYPE=POSIT;                              0317
         SLR   R9,R9                                               0317
         STH   R9,PARMTYPE                                         0317
*                     CALL SNTABUPT;                               0318
         BAL   R14,SNTABUPT                                        0318
*                   END;                                           0319
*                 ELSE                                             0320
*                   DO;                                            0320
         B     @RC00312                                            0320
@RF00312 DS    0H                                                  0321
*                     IF ICTR-SCTR^<CON253 THEN/* USE INVALID PARM 0321
*                                      LENGTH                        */
         LR    R3,R6ICTR                                           0321
         SLR   R3,R2SCTR                                           0321
         C     R3,FW253                                            0321
         BL    @RF00321                                            0321
*                       EXMSGID=A507;/* MESSAGE                      */
         MVC   EXMSGID(4),$MSGA507                                 0322
*                     ELSE          /* OTHERWISE EXPLAIN WHY PARM    */
*                       DO;         /* INVALID                       */
         B     @RC00321                                            0323
@RF00321 DS    0H                                                  0324
*                         EXMSGID=M507;                            0324
         MVC   EXMSGID(4),$MSGM507                                 0324
*                         ICTR=ICTR+CON1;                          0325
         AL    R6ICTR,FW01                                         0325
*                       END;                                       0326
*                     MVAR(1)=ADDR(CPOSIT);                        0327
@RC00321 LA    R9,@CC01304                                         0327
         ST    R9,MVAR                                             0327
*                     MVARLEN(1)=LENGTH(CPOSIT);                   0328
         MVI   MVARLEN,X'0A'                                       0328
*                     MVAR(2)=ADDR(RECORD(SCTR));                  0329
         L     R3,ECDAIREC(,ECDAPTR7)                              0329
         LA    R9,RECORD-1(R2SCTR,R3)                              0329
         ST    R9,MVAR+4                                           0329
*                     MVARLEN(2)=ICTR-SCTR;                        0330
         LR    R3,R6ICTR                                           0330
         SLR   R3,R2SCTR                                           0330
         STC   R3,MVARLEN+4                                        0330
*                     IF ICTR-SCTR>CON127 THEN/* TRUNCATE            */
         C     R3,FW127                                            0331
         BNH   @RF00331                                            0331
*                       MVARLEN(2)=CON127;/* LENGTHS GT 127          */
         MVI   MVARLEN+4,X'7F'                                     0332
*                     NOTEXEC=YES;  /* PROCEDURE HAS ERRORS          */
@RF00331 OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0333
*                     CALL MSGRTN;  /* ISSUE MESSAGE TO USER         */
         BAL   R14,MSGRTN                                          0334
*                     CALL SKIPSEP; /* FIND START OF NEXT PARM       */
         BAL   R14,SKIPSEP                                         0335
*                   END;                                           0336
*               END;                                               0337
@RC00312 AL    R5PCTR,FW01                                         0337
@DE00306 C     R5PCTR,POSNUM                                       0337
         BNH   @DL00306                                            0337
@DC00306 DS    0H                                                  0338
*               IF PCTR<=POSNUM&STABERR=NO THEN/* IF WE DID NOT    0338
*                                      PROCESS ENOUGH                */
         C     R5PCTR,POSNUM                                       0338
         BH    @RF00338                                            0338
         TM    STABERR,B'10000000'                                 0338
         BNZ   @RF00338                                            0338
*                 DO;               /* PARMS THEN NOTIFY USER        */
*                   NOTEXEC=YES;                                   0340
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0340
*                   EXMSGID=M509;                                  0341
         MVC   EXMSGID(4),$MSGM509                                 0341
*                   CALL MSGRTN;                                   0342
         BAL   R14,MSGRTN                                          0342
*                 END;                                             0343
*                                                                  0344
*               /*****************************************************/
*               /*                                                   */
*               /* SYNTAX CHECK KEYWORD SPECIFICATIONS               */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0344
*               KEYWORD=YES;        /* CURRENTLY PROCESSING KEYWORDS */
@RF00338 OI    KEYWORD,B'00001000'                                 0344
*               KEYWCHAR=CON0;                                     0345
         SLR   KEYWCHAR,KEYWCHAR                                   0345
*               KEYCHAR=CON0;                                      0346
         SLR   KEYCHAR,KEYCHAR                                     0346
*               KEYWNUM=CON0;                                      0347
         SLR   R9,R9                                               0347
         ST    R9,KEYWNUM                                          0347
*               KEYNUM=CON0;                                       0348
         ST    R9,KEYNUM                                           0348
*               DO WHILE ICTR<=LINELNG&STABERR=NO;                 0349
         B     @DE00349                                            0349
@DL00349 DS    0H                                                  0350
*                 SCTR=ICTR;                                       0350
         LR    R2SCTR,R6ICTR                                       0350
*                 KEYERR=NO;                                       0351
         NI    KEYERR,B'11101111'                                  0351
*                 IF(RECORD(SCTR)^<CONCA&/* FIRST CHARACTER MUST BE
*                                      ALPHA                         */
*                     RECORD(SCTR)^>CONCI) (RECORD(SCTR)^<CONCJ&RECORD(
*                     SCTR)^>CONCR) (RECORD(SCTR)^<CONCS&RECORD(SCTR)^>
*                     CONCZ) THEN                                  0352
         L     R9,ECDAIREC(,ECDAPTR7)                              0352
         LR    R1,R9                                               0352
         ALR   R1,R2SCTR                                           0352
         CLI   RECORD-1(R1),C'A'                                   0352
         BL    @GL00017                                            0352
         ALR   R9,R2SCTR                                           0352
         CLI   RECORD-1(R9),C'I'                                   0352
         BNH   @RT00352                                            0352
@GL00017 L     R9,ECDAIREC(,ECDAPTR7)                              0352
         LR    R1,R9                                               0352
         ALR   R1,R2SCTR                                           0352
         CLI   RECORD-1(R1),C'J'                                   0352
         BL    @GL00016                                            0352
         ALR   R9,R2SCTR                                           0352
         CLI   RECORD-1(R9),C'R'                                   0352
         BNH   @RT00352                                            0352
@GL00016 L     R9,ECDAIREC(,ECDAPTR7)                              0352
         LR    R1,R9                                               0352
         ALR   R1,R2SCTR                                           0352
         CLI   RECORD-1(R1),C'S'                                   0352
         BL    @RF00352                                            0352
         ALR   R9,R2SCTR                                           0352
         CLI   RECORD-1(R9),C'Z'                                   0352
         BH    @RF00352                                            0352
@RT00352 DS    0H                                                  0353
*                   CALL VALIDATE;                                 0353
         BAL   R14,VALIDATE                                        0353
*                 ELSE              /* OTHERWISE IT IS AN ERROR      */
*                   KEYERR=YES;                                    0354
         B     @RC00352                                            0354
@RF00352 OI    KEYERR,B'00010000'                                  0354
*                 IF(KEYERR=NO&VALIDERR=NO)&JCTR-SCTR<=CON31 THEN/*
*                                      IF THERE HAVE BEEN NO ERRORS  */
@RC00352 TM    KEYERR,B'00010100'                                  0355
         BNZ   @RF00355                                            0355
         L     R9,JCTR                                             0355
         SLR   R9,R2SCTR                                           0355
         C     R9,FW31                                             0355
         BH    @RF00355                                            0355
*                   DO;             /* THEN PREPARE FOR SNTAB ENTRY  */
*                     PARMADR=ADDR(RECORD(SCTR));                  0357
         L     R14,ECDAIREC(,ECDAPTR7)                             0357
         LA    R15,RECORD-1(R2SCTR,R14)                            0357
         ST    R15,PARMADR                                         0357
*                     PARMLNG=JCTR-SCTR;/* IF THE NEXT NON SEPERATOR
*                                      IS                            */
         ST    R9,PARMLNG                                          0358
*                     IF ICTR<=LINELNG&RECORD(ICTR)=LFPAREN THEN/* A
*                                      LEFT PAREN THEN IT A          */
         C     R6ICTR,LINELNG                                      0359
         BH    @RF00359                                            0359
         ALR   R14,R6ICTR                                          0359
         CLI   RECORD-1(R14),C'('                                  0359
         BNE   @RF00359                                            0359
*                       DO;         /* KEYWORD WITH VALUE            */
*                         PARMTYPE=KEYW;                           0361
         MVC   PARMTYPE(2),HW03                                    0361
*                         KEYWCHAR=KEYWCHAR+PARMLNG;               0362
         ALR   KEYWCHAR,R9                                         0362
*                         KEYWNUM=KEYWNUM+CON1;                    0363
         LA    R2,1                                                0363
         AL    R2,KEYWNUM                                          0363
         ST    R2,KEYWNUM                                          0363
*                         CALL VALUECHK;/* SYNTAX CHECK THE VALUE    */
         BAL   R14,VALUECHK                                        0364
*                         IF VALSTR^=CON0 THEN                     0365
         L     R9,VALSTR                                           0365
         LTR   R9,R9                                               0365
         BZ    @RF00365                                            0365
*                           DO;                                    0366
*                             VALLNG=VALEND-VALSTR+CON1;           0367
         L     R2,VALEND                                           0367
         SLR   R2,R9                                               0367
         AL    R2,FW01                                             0367
         ST    R2,VALLNG                                           0367
*                             VALADR=ADDR(RECORD(VALSTR));         0368
         L     R2,ECDAIREC(,ECDAPTR7)                              0368
         LA    R9,RECORD-1(R9,R2)                                  0368
         ST    R9,VALADR                                           0368
*                           END;                                   0369
*                         ELSE                                     0370
*                           VALLNG=CON0;                           0370
         B     @RC00365                                            0370
@RF00365 SLR   R2,R2                                               0370
         ST    R2,VALLNG                                           0370
*                       END;                                       0371
*                     ELSE          /* IF CHARACTER NOT A LEFT PAREN */
*                       DO;         /* THEN IT A KEYWORD WITH NO     */
         B     @RC00359                                            0372
@RF00359 DS    0H                                                  0373
*                         KEYCHAR=KEYCHAR+PARMLNG;/* VALUE           */
         AL    KEYCHAR,PARMLNG                                     0373
*                         PARMTYPE=KEYWO;                          0374
         LA    R9,1                                                0374
         STH   R9,PARMTYPE                                         0374
*                         KEYNUM=KEYNUM+CON1;                      0375
         AL    R9,KEYNUM                                           0375
         ST    R9,KEYNUM                                           0375
*                       END;                                       0376
*                     CALL SNTABUPT;/* CREATE ENTRY IN SNTAB/SVTAB   */
@RC00359 BAL   R14,SNTABUPT                                        0377
*                   END;                                           0378
*                 ELSE              /* IF ERRORS HAD BEEN DETECTED   */
*                   DO;                                            0379
         B     @RC00355                                            0379
@RF00355 DS    0H                                                  0380
*                     IF JCTR-SCTR>CON31 THEN/* THEN NOTIFY USER AND
*                                      CONTINUE                      */
         L     R9,JCTR                                             0380
         SLR   R9,R2SCTR                                           0380
         C     R9,FW31                                             0380
         BNH   @RF00380                                            0380
*                       EXMSGID=A507;/* SYNTAX CHECKING ANY OTHER    */
         MVC   EXMSGID(4),$MSGA507                                 0381
*                     ELSE          /* PARMS ON PROC STATEMENT       */
*                       DO;                                        0382
         B     @RC00380                                            0382
@RF00380 DS    0H                                                  0383
*                         EXMSGID=M507;                            0383
         MVC   EXMSGID(4),$MSGM507                                 0383
*                         ICTR=ICTR+CON1;                          0384
         AL    R6ICTR,FW01                                         0384
*                       END;                                       0385
*                     MVAR(1)=ADDR(CKEYWORD);                      0386
@RC00380 LA    R9,$KEYWORD                                         0386
         ST    R9,MVAR                                             0386
*                     MVARLEN(1)=LENGTH(CKEYWORD);                 0387
         MVI   MVARLEN,X'07'                                       0387
*                     MVAR(2)=ADDR(RECORD(SCTR));                  0388
         L     R9,ECDAIREC(,ECDAPTR7)                              0388
         LA    R9,RECORD-1(R2SCTR,R9)                              0388
         ST    R9,MVAR+4                                           0388
*                     MVARLEN(2)=ICTR-SCTR;                        0389
         LR    R9,R6ICTR                                           0389
         SLR   R9,R2SCTR                                           0389
         STC   R9,MVARLEN+4                                        0389
*                     NOTEXEC=YES;                                 0390
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0390
*                     CALL MSGRTN;                                 0391
         BAL   R14,MSGRTN                                          0391
*                   END;                                           0392
*                 CALL SKIPSEP;     /* UPDATE TO NEXT KEYWORD        */
@RC00355 BAL   R14,SKIPSEP                                         0393
*               END;                                               0394
@DE00349 C     R6ICTR,LINELNG                                      0394
         BH    @DC00349                                            0394
         TM    STABERR,B'10000000'                                 0394
         BZ    @DL00349                                            0394
@DC00349 DS    0H                                                  0395
*               IF NOTEXEC=YES (POSNUM+KEYNUM+KEYWNUM=CON0) THEN   0395
         TM    ECDAFLAG(ECDAPTR7),NOTEXEC                          0395
         BO    @RT00395                                            0395
         L     R2,POSNUM                                           0395
         AL    R2,KEYNUM                                           0395
         AL    R2,KEYWNUM                                          0395
         LTR   R2,R2                                               0395
         BNZ   @RF00395                                            0395
@RT00395 DS    0H                                                  0396
*                 DO;                                              0396
*                   IF NOTEXEC=NO THEN                             0397
         TM    ECDAFLAG(ECDAPTR7),NOTEXEC                          0397
         BNZ   @RF00397                                            0397
*                     DO;                                          0398
*                       IF(IMPLICIT=NO&(VLST=YES&VLSTLNG^=CON0)) ( 0399
*                           IMPLICIT=YES&ECTNOPD=NO) THEN          0399
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0399
         BNZ   @GL00025                                            0399
         L     R6,ECDAEANS(,ECDAPTR7)                              0399
         TM    VLST(R6),B'10000000'                                0399
         BNO   @GL00025                                            0399
         LH    R6,VLSTLNG(,R6)                                     0399
         LTR   R6,R6                                               0399
         BNZ   @RT00399                                            0399
@GL00025 TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0399
         BNO   @RF00399                                            0399
         L     R6,CPPLPTR(,ECDAPTR7)                               0399
         L     R6,CPPLECT-CPPL(,R6)                                0399
         TM    ECTSWS-ECT(R6),ECTNOPD                              0399
         BNZ   @RF00399                                            0399
@RT00399 DS    0H                                                  0400
*                         DO;                                      0400
*                           EXMSGID=B529;                          0401
         MVC   EXMSGID(4),$MSGB529                                 0401
*                           IF IMPLICIT=NO THEN/* EXPLICIT - PICK UP
*                                      ADDR FROM                     */
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0402
         BNZ   @RF00402                                            0402
*                             DO;   /* FROM PDL                      */
*                               MVAR(1)=VLSTPTR;                   0404
         L     R6,ECDAEANS(,ECDAPTR7)                              0404
         L     R9,VLSTPTR(,R6)                                     0404
         ST    R9,MVAR                                             0404
*                               MVARLEN(1)=VLSTLNG;                0405
         LH    R6,VLSTLNG(,R6)                                     0405
         STC   R6,MVARLEN                                          0405
*                             END;                                 0406
*                           ELSE    /* IMPLICIT - PICK UP ADDR USING */
*                             DO;                                  0407
         B     @RC00402                                            0407
@RF00402 DS    0H                                                  0408
*                               MVAR(1)=ADDR(CBUFTEXT)+CBUFOFF;    0408
         L     R6,CPPLPTR(,ECDAPTR7)                               0408
         L     R6,CPPLCBUF-CPPL(,R6)                               0408
         LA    R8,CBUFTEXT(,R6)                                    0408
         LH    R9,CBUFOFF(,R6)                                     0408
         ALR   R8,R9                                               0408
         ST    R8,MVAR                                             0408
*                               MVARLEN(1)=CBUFLNG-CBUFOFF;        0409
         LCR   R9,R9                                               0409
         AH    R9,CBUFLNG(,R6)                                     0409
         STC   R9,MVARLEN                                          0409
*                             END;                                 0410
*                           CALL MSGRTN;                           0411
@RC00402 BAL   R14,MSGRTN                                          0411
*                         END;                                     0412
*                     END;                                         0413
*                 END;                                             0414
*                                                                  0414
*               /*****************************************************/
*               /*                                                   */
*               /* IF ERRORS HAVE OCCURRED THEN CONTROL GOES TO EXIT */
*               /* AT THIS POINT                                     */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0415
*                                                                  0415
*               /*****************************************************/
*               /*                                                   */
*               /* GET CORE FOR THE VALUE LIST PCL                   */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0415
*               ELSE                /* PARAMETER SYNTAX O.K.         */
*                 DO;                                              0415
         B     @RC00395                                            0415
@RF00395 DS    0H                                                  0416
*                   POSAR=POSNUM*LENGTH(IDMOD)+POSCHAR;/* AMOUNT   0416
*                                      NEEDED FOR POSITIONAL       0416
*                                      PARAMETERS                    */
         L     R8POSAR,POSNUM                                      0416
         MH    R8POSAR,HW56                                        0416
         ALR   R8POSAR,R4POSCHR                                    0416
*                   KEYAR=KEYNUM*LENGTH(KWMOD)+KEYCHAR;/* AMOUNT   0417
*                                      NEEDED FOR KEYWORDS WITHOUT 0417
*                                      VALUES                        */
         L     R9,KEYNUM                                           0417
         MH    R9,HW11                                             0417
         ALR   R9,KEYCHAR                                          0417
         ST    R9,KEYAR                                            0417
*                   KEYWAR=KEYWNUM*(LENGTH(KWMOD)+CON2+LENGTH(SUBMOD)+
*                       LENGTH(VIDMAP))+KEYWCHAR*CON2;/* AMOUNT FOR
*                                      KEYWORDS WITH VALUES          */
         L     R2KEYWAR,KEYWNUM                                    0418
         MH    R2KEYWAR,HW69                                       0418
         LR    R4,KEYWCHAR                                         0418
         ALR   R4,R4                                               0418
         ALR   R2KEYWAR,R4                                         0418
*                   GETAMT=POSAR+KEYAR+KEYWAR+LENGTH(PCEHEAD)+LENGTH(
*                       PCEEND);                                   0419
         ALR   R9,R8POSAR                                          0419
         ALR   R9,R2KEYWAR                                         0419
         AL    R9,FW08                                             0419
         ST    R9,GETAMT                                           0419
*                   GETAMT=GETAMT-1;                                 */
         BCTR  R9,0
         ST    R9,GETAMT
*                   PCLAMT=GETAMT;  /* SAVE LENGTH OF PCL            */
*                                                                  0420
         ST    R9,PCLAMT                                           0420
*                   /*************************************************/
*                   /*                                               */
*                   /* IF THIS IS AN EXPLICIT EXEC WITH A VALUE LIST */
*                   /* THEN GET EXTRA SO WE CAN COPY OVER THE VALUE  */
*                   /* LIST                                          */
*                   /*                                               */
*                   /*************************************************/
*                                                                  0421
*                   IF IMPLICIT=NO&VLST=YES&VLSTLNG^=CON0 THEN/* IF
*                                      EXPLICIT THEN                 */
         TM    ECDAFLAG(ECDAPTR7),IMPLICIT                         0421
         BNZ   @RF00421                                            0421
         L     R6,ECDAEANS(,ECDAPTR7)                              0421
         TM    VLST(R6),B'10000000'                                0421
         BNO   @RF00421                                            0421
         LH    R4,VLSTLNG(,R6)                                     0421
         LTR   R4,R4                                               0421
         BZ    @RF00421                                            0421
*                     DO;           /* ADD EXTRA                     */
*                       GETAMT=GETAMT+VLSTLNG+CON4;                0423
         ALR   R9,R4                                               0423
         AL    R9,FW04                                             0423
         ST    R9,GETAMT                                           0423
*                       COPYVLST=YES;                              0424
         OI    COPYVLST,B'00000010'                                0424
*                     END;                                         0425
*                   RFY                                            0426
*                     R15 RSTD;                                    0426
@RF00421 DS    0H                                                  0427
*                   DO;             /* GETMAIN (EC) LV(GETAMT)     0427
*                                      A(GETADR) SP(1)               */
*                     RESPECIFY                                    0428
*                      (R1) RESTRICTED;                            0428
*                     I004=I004&&I004;/* INIT LIST                   */
         XC    I004(10),I004                                       0429
*                     I00404='20'X; /* TYPE                          */
         MVI   I00404,X'20'                                        0430
*                     I00401=GETAMT;/* LENGTH                        */
         L     R9,GETAMT                                           0431
         ST    R9,I00401                                           0431
*                     I00403=ADDR(GETADR);/* ADDR OF ADDR LIST       */
         LA    R14,GETADR                                          0432
         STCM  R14,7,I00403                                        0432
*                     I00405=1;     /* SUBPOOL VALUE                 */
         MVI   I00405,X'01'                                        0433
*                     R1=ADDR(I004);/* REG1 POINTS TO LIST           */
         LA    R1,I004                                             0434
*                     SVC(4);       /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0435
*                     RESPECIFY                                    0436
*                      (R1) UNRESTRICTED;                          0436
*                   END;            /* GETMAIN (EC) LV(GETAMT)     0437
*                                      A(GETADR) SP(1)               */
*                   IF R15^=CON0 THEN/* IF STORAGE COULD NOT BE      */
         LTR   R15,R15                                             0438
         BZ    @RF00438                                            0438
*                     DO;           /* OBTAINED THEN NOTIFY USER     */
*                       EXMSGID=M511;/* AND TERMINATE                */
         MVC   EXMSGID(4),$MSGM511                                 0440
*                       NOTEXEC=YES;                               0441
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0441
*                       CT431RET=CON16;                            0442
         MVC   CT431RET(4),FW16                                    0442
*                       CALL MSGRTN;                               0443
         BAL   R14,MSGRTN                                          0443
*                     END;                                         0444
*                   ELSE                                           0445
*                     DO;                                          0445
         B     @RC00438                                            0445
@RF00438 DS    0H                                                  0446
*                       PCLCUR=GETADR;/* INIT ADDRESS OF CURRENT PCL */
         L     R2PCLCUR,GETADR                                     0446
*                       PCLLNG=PCLAMT;/* LENGTH OF PCL               */
         L     R14,PCLAMT                                          0447
         STH   R14,PCLLNG(,R2PCLCUR)                               0447
*                       PCLBASE=PCLCUR;                            0448
         LR    PCLBASE,R2PCLCUR                                    0448
*                       PCLKEYOF=POSAR+LENGTH(PCEHEAD);/* OFFSET TO
*                                      KEYWORDS ADDRESS OF START OF
*                                      SUBFIELD AREA                 */
         LA    R6,6                                                0449
         LR    R14,R8POSAR                                         0449
         ALR   R14,R6                                              0449
         STH   R14,PCLKEYOF(,R2PCLCUR)                             0449
*                       SBFCUR=PCLCUR+LENGTH(PCEHEAD)+POSAR+KEYAR+ 0450
*                           KEYWNUM*(LENGTH(KWMOD)+CON2)+KEYWCHAR; 0450
         LR    R4SBFCUR,R2PCLCUR                                   0450
         ALR   R4SBFCUR,R6                                         0450
         ALR   R4SBFCUR,R8POSAR                                    0450
         AL    R4SBFCUR,KEYAR                                      0450
         L     R14,KEYWNUM                                         0450
         MH    R14,HW13                                            0450
         ALR   R4SBFCUR,R14                                        0450
         ALR   R4SBFCUR,KEYWCHAR                                   0450
*                       SBFBASE=SBFCUR;                            0451
         ST    R4SBFCUR,SBFBASE                                    0451
*                       PCLCUR=PCLCUR+LENGTH(PCEHEAD);/* UPDATE TO 0452
*                                      NEXT PCE                      */
         ALR   R2PCLCUR,R6                                         0452
*                       PDLCUR=CON8;/* DSECT OFFSET PAST PDL HEADER  */
         LA    R3PDLCUR,8                                          0453
*                                                                  0454
*                       /*********************************************/
*                       /*                                           */
*                       /* INITIALIZE PCES FOR THE POSITIONAL        */
*                       /* PARAMETERS                                */
*                       /*                                           */
*                       /*********************************************/
*                                                                  0454
*                       SNTELPTR=USNTABST;/* ELEMENT BASE TO FIRST 0454
*                                      USER                          */
         L     SNTELPTR,USNTABST                   R8=SNTELPTR     0454
*                       SNTABPTR=SNTABFST;/* PARM                    */
         L     R14,LSDPTR(,ECDAPTR7)                               0455
         L     R5,LSDEXEC-LSD(,R14)                                0455
         L     R14,SNTABFST(,R5)                                   0455
         ST    R14,SNTABPTR                                        0455
*                       DO PCTR=1 TO POSNUM;/* MOVE IN A COPY OF THE
*                                      MODEL                         */
         LA    R5PCTR,1                                            0456
         B     @DE00456                                            0456
@DL00456 DS    0H                                                  0457
*                         PCELMT(1:LENGTH(IDMOD))=IDMOD;           0457
         MVC   PCELMT(56,R2PCLCUR),IDMOD                           0457
*                         IDPRNME(1:SNTLNG)=SNTDATA(1:SNTLNG);/* COPY
*                                      POSIT PARM NAME               */
         LH    R6,SNTLNG(,SNTELPTR)                                0458
         LR    R14,R6                                              0458
         BCTR  R14,0                                               0458
         EX    R14,@SM01557                                        0458
*                         IDLNG=IDLNG+SNTLNG;/* ADJUST TOTAL LENGTH
*                                      TO INCLUDE NAME               */
         ICM   R14,12,IDLNG(R2PCLCUR)                              0459
         SRA   R14,16                                              0459
         ALR   R14,R6                                              0459
         STCM  R14,3,IDLNG(R2PCLCUR)                               0459
*                         IF IDPRMLNG+SNTLNG>CON223 THEN/* IF PROMPT
*                                      INFO TOO LARGE                */
         SLR   R14,R14                                             0460
         IC    R14,IDPRMLNG(,R2PCLCUR)                             0460
         ALR   R6,R14                                              0460
         C     R6,FW223                                            0460
         BNH   @RF00460                                            0460
*                           IDPRMLNG=CON223;/* FOR PARSE PROMPT    0461
*                                      MESSAGE THEN                  */
         MVI   IDPRMLNG(R2PCLCUR),X'DF'                            0461
*                         ELSE                                     0462
*                           IDPRMLNG=IDPRMLNG+SNTLNG;              0462
         B     @RC00460                                            0462
@RF00460 SLR   R6,R6                                               0462
         IC    R6,IDPRMLNG(,R2PCLCUR)                              0462
         AH    R6,SNTLNG(,SNTELPTR)                                0462
         STC   R6,IDPRMLNG(,R2PCLCUR)                              0462
*                         IDPDLOFF=PDLCUR;/* OFFSET TO PCE           */
@RC00460 STCM  R3PDLCUR,3,IDPDLOFF(R2PCLCUR)                       0463
*                         PCLCUR=PCLCUR+IDLNG;/* UPDATE TO START OF
*                                      NEXT PCE                      */
         ICM   R14,12,IDLNG(R2PCLCUR)                              0464
         SRA   R14,16                                              0464
         ALR   R2PCLCUR,R14                                        0464
*                         PDLCUR=PDLCUR+LENGTH(IDENTPDE);/* UPDATE TO
*                                      NEXT PDE                      */
         AL    R3PDLCUR,FW08                                       0465
*                         IF SNTLAST=YES THEN/* IF THIS IS LAST SNTAB
*                                      ELEMENT                       */
         TM    SNTLAST(SNTELPTR),B'00000001'                       0466
         BNO   @RF00466                                            0466
*                           DO;     /* UPDATE TO NEXT SNTAB TABLE    */
*                             SNTABPTR=SNTABNXT;                   0468
         L     R6,SNTABPTR                                         0468
         L     R14,SNTABNXT(,R6)                                   0468
         ST    R14,SNTABPTR                                        0468
*                             SNTELPTR=SNTABPTR+LENGTH(SNTAB);     0469
         AL    R14,FW12                                            0469
         LR    SNTELPTR,R14                        R8=SNTELPTR     0469
*                           END;                                   0470
*                         ELSE      /* OTHERWISE STEP UP TO NEXT     */
*                           SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);
         B     @RC00466                                            0471
@RF00466 LR    R6,SNTELPTR                                         0471
         AH    R6,SNTLNG(,SNTELPTR)                                0471
         AL    R6,FW08                                             0471
         LR    SNTELPTR,R6                         R8=SNTELPTR     0471
*                                   /* ELEMENT                       */
*                       END;                                       0472
@RC00466 AL    R5PCTR,FW01                                         0472
@DE00456 C     R5PCTR,POSNUM                                       0472
         BNH   @DL00456                                            0472
*                                                                  0473
*                       /*********************************************/
*                       /*                                           */
*                       /* BUILD PCES FOR EACH KEYWORD ENTRY         */
*                       /*                                           */
*                       /*********************************************/
*                                                                  0473
*                       DO PCTR=1 TO(KEYWNUM+KEYNUM);/* MOVE IN MODEL
*                                      PCL                           */
         LA    R5PCTR,1                                            0473
         B     @DE00473                                            0473
@DL00473 DS    0H                                                  0474
*                         PCELMT(1:LENGTH(KWMOD))=KWMOD;           0474
         MVC   PCELMT(11,R2PCLCUR),KWMOD                           0474
*                         KWPDLOFF=PDLCUR;/* SAVE OFFSET TO PDL      */
         STCM  R3PDLCUR,3,KWPDLOFF(R2PCLCUR)                       0475
*                         NADAT(1:SNTLNG)=SNTDATA(1:SNTLNG);/* COPY
*                                      NAME AND                      */
         LH    R6,SNTLNG(,SNTELPTR)                                0476
         LR    R14,R6                                              0476
         BCTR  R14,0                                               0476
         EX    R14,@SM01559                                        0476
*                         NALNG=SNTLNG+LENGTH(NAMPCE);/* LENGTH FROM
*                                      SNTAB                         */
         LA    R0,5                                                0477
         ALR   R0,R6                                               0477
         STCM  R0,3,NALNG(R2PCLCUR)                                0477
*                         NADATLNG=SNTLNG-CON1;                    0478
         STC   R14,NADATLNG(,R2PCLCUR)                             0478
*                         PDLCUR=PDLCUR+LENGTH(KEYPDE);/* UPDATE TO
*                                      NEXT PDL                      */
         LA    R1,2                                                0479
         ALR   R3PDLCUR,R1                                         0479
*                         IF SNTKEYW=YES THEN/* IF THIS A KEYWORD  0480
*                                      WITH                          */
         TM    SNTKEYW(SNTELPTR),B'00100000'                       0480
         BNO   @RF00480                                            0480
*                           DO;     /* WITH VALUE THEN BUILD SUBFIELD*/
*                             NALNG=NALNG+CON2;/* ADD TWO BYTES FOR
*                                      SBFD OFFSET                   */
         ALR   R0,R1                                               0482
         STCM  R0,3,NALNG(R2PCLCUR)                                0482
*                             KWSUBFLD=YES;/* SET SUBFLD INDICATOR   */
         OI    KWSUBFLD(R2PCLCUR),B'00000100'                      0483
*                             PCLCUR=PCLCUR+KEYLNG+NALNG;/* UPDATE TO
*                                      NEXT PCE                      */
         LR    R6,R2PCLCUR                                         0484
         ICM   R0,12,KEYLNG(R2PCLCUR)                              0484
         SRA   R0,16                                               0484
         ALR   R6,R0                                               0484
         ICM   R0,12,NALNG(R2PCLCUR)                               0484
         SRA   R0,16                                               0484
         ALR   R6,R0                                               0484
         LR    R2PCLCUR,R6                                         0484
*                             SBFOFF=SBFCUR-PCLBASE+CON1;/* SAVE   0485
*                                      OFFSET TO SUBFIELD            */
         LCR   R1,R1                                               0485
         ALR   R1,R2PCLCUR                                         0485
         LR    R6,R4SBFCUR                                         0485
         SLR   R6,PCLBASE                                          0485
         LA    R0,1                                                0485
         ALR   R0,R6                                               0485
         STH   R0,SBFOFF(,R1)                                      0485
*                             SBFELMT(1:LENGTH(SUBMOD))=SUBMOD;/*  0486
*                                      COPY MODEL SUBFLD             */
         MVC   SBFELMT(3,R4SBFCUR),SUBMOD                          0486
*                             SUBNXSUB=SBFCUR-PCLBASE+LENGTH(SUBFLD)+
*                                 LENGTH(VIDMAP)+SNTLNG;/* FIND    0487
*                                      OFFSET TO NEXT SUBFIELD       */
         AL    R6,FW56                                             0487
         LH    R1,SNTLNG(,SNTELPTR)                                0487
         ALR   R6,R1                                               0487
         STCM  R6,3,SUBNXSUB(R4SBFCUR)                             0487
*                             SBFCUR=SBFCUR+LENGTH(SUBFLD);/* UPDATE
*                                      TO NEXT JBF PCE               */
         LA    R6,3                                                0488
         ALR   R4SBFCUR,R6                                         0488
*                             SBFELMT(1:LENGTH(VIDMAP))=IDMOD;/* COPY
*                                      IDENT MODEL                   */
         MVC   SBFELMT(53,R4SBFCUR),IDMOD                          0489
*                             SBFCUR->IDMODTYP=KEYTYPE;/* MODIFY TYPE
*                                      TO KEYWORD                    */
         MVC   IDMODTYP(21,R4SBFCUR),@CC01285                      0490
*                             SBFCUR->VIDPRINF=KEYTYPE;            0491
         MVC   VIDPRINF(18,R4SBFCUR),@CC01285                      0491
*                             SBFCUR->VIDNAME(1:SNTLNG)=SNTDATA(1: 0492
*                                 SNTLNG);/* KEY-NAME                */
         EX    R14,@SM01561                                        0492
*                             SBFCUR->IDPRMLNG=SBFCUR->IDPRMLNG+SNTLNG-
*                                 CON3;                            0493
         SLR   R14,R14                                             0493
         IC    R14,IDPRMLNG(,R4SBFCUR)                             0493
         ALR   R14,R1                                              0493
         SLR   R14,R6                                              0493
         STC   R14,IDPRMLNG(,R4SBFCUR)                             0493
*                             SBFCUR->IDLNG=SBFCUR->IDLNG+SNTLNG-CON3;
         ICM   R14,12,IDLNG(R4SBFCUR)                              0494
         SRA   R14,16                                              0494
         ALR   R1,R14                                              0494
         SLR   R1,R6                                               0494
         STCM  R1,3,IDLNG(R4SBFCUR)                                0494
*                                   /* SAVE PCE LENGTH               */
*                             SBFCUR->IDCHAR=YES;/* INDICATE IDENT IS
*                                      CHARACTER                     */
         OI    IDCHAR(R4SBFCUR),B'00001000'                        0495
*                             SBFCUR->IDPDLOFF=PDLCUR;/* PDL OFFSET
*                                      FOR THIS PCE                  */
         STCM  R3PDLCUR,3,IDPDLOFF(R4SBFCUR)                       0496
*                             SBFCUR=SBFCUR+SBFCUR->IDLNG;/* UPDATE
*                                      TO NEXT SBF PCE               */
         ALR   R4SBFCUR,R1                                         0497
*                             PDLCUR=PDLCUR+LENGTH(IDENTPDE);/* NEXT
*                                      AVAILABLE PDE                 */
         AL    R3PDLCUR,FW08                                       0498
*                           END;                                   0499
*                         ELSE      /* FOR KEYWORDS WITHOUT VALUES   */
*                           PCLCUR=PCLCUR+KEYLNG+NALNG;/* NO SUBFIELD
*                                      NECESSARY                     */
         B     @RC00480                                            0500
@RF00480 LR    R6,R2PCLCUR                                         0500
         ICM   R14,12,KEYLNG(R2PCLCUR)                             0500
         SRA   R14,16                                              0500
         ALR   R6,R14                                              0500
         ICM   R14,12,NALNG(R2PCLCUR)                              0500
         SRA   R14,16                                              0500
         ALR   R6,R14                                              0500
         LR    R2PCLCUR,R6                                         0500
*                         IF SNTLAST=YES THEN/* IF LAST ELEMENT THEN
*                                      UPDATE                        */
@RC00480 TM    SNTLAST(SNTELPTR),B'00000001'                       0501
         BNO   @RF00501                                            0501
*                           DO;     /* TO NEXT SNTAB                 */
*                             SNTABPTR=SNTABNXT;                   0503
         L     R6,SNTABPTR                                         0503
         L     R14,SNTABNXT(,R6)                                   0503
         ST    R14,SNTABPTR                                        0503
*                             SNTELPTR=SNTABPTR+LENGTH(SNTAB);     0504
         AL    R14,FW12                                            0504
         LR    SNTELPTR,R14                        R8=SNTELPTR     0504
*                           END;                                   0505
*                         ELSE      /* OTHERWISE-STEP TO NEXT ELEMT  */
*                           SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);
         B     @RC00501                                            0506
@RF00501 LR    R6,SNTELPTR                                         0506
         AH    R6,SNTLNG(,SNTELPTR)                                0506
         AL    R6,FW08                                             0506
         LR    SNTELPTR,R6                         R8=SNTELPTR     0506
*                       END;                                       0507
@RC00501 AL    R5PCTR,FW01                                         0507
@DE00473 L     R14,KEYWNUM                                         0507
         AL    R14,KEYNUM                                          0507
         CR    R5PCTR,R14                                          0507
         BNH   @DL00473                                            0507
*                       SBFELMT(1)=ENDMOD;/* INITIALIZE THE END PCE  */
         MVC   SBFELMT(1,R4SBFCUR),ENDMOD                          0508
*                       PCLBASE->PDLLNG=PDLCUR;/* UPDATE FINAL PDL 0509
*                                      LENGTH                        */
         STH   R3PDLCUR,PDLLNG(,PCLBASE)                           0509
*                                                                  0510
*                       /*********************************************/
*                       /*                                           */
*                       /* PARSE THE VALUE LIST                      */
*                       /*                                           */
*                       /*********************************************/
*                                                                  0510
*                       PPLPTR=ADDR(SERVBLK);/* SET UP PARSE PARMS   */
         LA    R8PPLPTR,SERVBLK                                    0510
*                       PPLUPT=CPPLUPT;                            0511
         L     R14,CPPLPTR(,ECDAPTR7)                              0511
         L     R6,CPPLUPT-CPPL(,R14)                               0511
         ST    R6,PPLUPT-PPL(,R8PPLPTR)                            0511
*                       PPLPCL=PCLBASE;                            0512
         ST    PCLBASE,PPLPCL-PPL(,R8PPLPTR)                       0512
*                       PPLECT=CPPLECT;                            0513
         L     R14,CPPLECT-CPPL(,R14)                              0513
         ST    R14,PPLECT-PPL(,R8PPLPTR)                           0513
*                       PPLECB=ADDR(ECB);                          0514
         LA    R6,ECB                                              0514
         ST    R6,PPLECB-PPL(,R8PPLPTR)                            0514
*                       PPLANS=ADDR(VLSTANS);                      0515
         LA    R14,VLSTANS                                         0515
         ST    R14,PPLANS-PPL(,R8PPLPTR)                           0515
*                       IF COPYVLST=YES THEN/* COPY OVER THE VALUE   */
         TM    COPYVLST,B'00000010'                                0516
         BNO   @RF00516                                            0516
*                         DO;       /* LIST IF NECESSARY             */
*                           RFY                                    0518
*                            (R2,                                  0518
*                             R3,                                  0518
*                             R4,                                  0518
*                             R5) RSTD;                            0518
*                           R4=VLSTPTR;/* ADDRESS OF VALUE LIST      */
         L     R6,ECDAEANS(,ECDAPTR7)                              0519
         L     R4,VLSTPTR(,R6)                                     0519
*                           R3=VLSTLNG;/* LENGTH OF VALUE LIST       */
         LH    R6,VLSTLNG(,R6)                                     0520
         LR    R3,R6                                               0520
*                           R5=R3;  /* NEW LENGTH IS SAME            */
         LR    R5,R3                                               0521
*                           R2=PCLBASE+PCLAMT;/* ADDR OF NEW VLIST   */
         LR    R2,PCLBASE                                          0522
         AL    R2,PCLAMT                                           0522
*                           R2->NVLSTLNG=VLSTLNG+CON4;/* INIT      0523
*                                      STANDARD                      */
         LA    R14,4                                               0523
         ALR   R6,R14                                              0523
         STH   R6,NVLSTLNG(,R2)                                    0523
*                           R2->NVLSTOFF=CON0;/* LENGTH AND OFFSET   */
         SLR   R6,R6                                               0524
         STH   R6,NVLSTOFF(,R2)                                    0524
*                           PPLCBUF=R2;/* ADDRESS OF VALUE LIST      */
         ST    R2,PPLCBUF-PPL(,R8PPLPTR)                           0525
*                           R2=R2+CON4;/* UPDATE TO DATA AREA        */
         ALR   R2,R14                                              0526
*                           MVCL(R2,R4);/* MOVE DATA                 */
         MVCL  R2,R4                                               0527
*                           RFY                                    0528
*                            (R2,                                  0528
*                             R3,                                  0528
*                             R4,                                  0528
*                             R5) UNRSTD;                          0528
*                         END;                                     0529
*                       ELSE        /* SET ADDRESS OF THE VALUE LIST */
*                         PPLCBUF=CPPLCBUF;                        0530
         B     @RC00516                                            0530
@RF00516 L     R14,CPPLPTR(,ECDAPTR7)                              0530
         L     R14,CPPLCBUF-CPPL(,R14)                             0530
         ST    R14,PPLCBUF-PPL(,R8PPLPTR)                          0530
*                       RFY                                        0531
*                        (R1,                                      0531
*                         R15) RSTD;                               0531
@RC00516 DS    0H                                                  0532
*                       R1=PPLPTR;                                 0532
         LR    R1,R8PPLPTR                                         0532
*                       DO;         /* CALLTSSR EP(IKJPARS)          */
*                         RESPECIFY                                0534
*                           R1 RSTD;                               0534
*                         IF CVTPARS=0 THEN                        0535
         L     R6,CVTPTR                                           0535
         L     R8,CVTPARS-CVT(,R6)                                 0535
         LTR   R8,R8                                               0535
         BNZ   @RF00535                                            0535
*                           GEN(LINK EP=IKJPARS);                  0536
         LINK EP=IKJPARS
*                         ELSE                                     0537
*                           CALL CVTPARS;                          0537
         B     @RC00535                                            0537
@RF00535 L     R6,CVTPTR                                           0537
         L     R15,CVTPARS-CVT(,R6)                                0537
         BALR  R14,R15                                             0537
*                         RESPECIFY                                0538
*                           R1 UNRSTD;                             0538
@RC00535 DS    0H                                                  0539
*                       END;                                       0539
*                       RFY                                        0540
*                         R1 UNRSTD;                               0540
*                                                                  0541
*                       /*********************************************/
*                       /*                                           */
*                       /* IF THE PARSE RETURN CODE IS NON ZERO THEN */
*                       /* CALL IKJEFF19 TO ANALIZE THE PARSE ERROR  */
*                       /* CODE AND ISSUE A MESSAGE                  */
*                       /*                                           */
*                       /*********************************************/
*                                                                  0541
*                       IF R15^=CON0 THEN/* IF PARSE ERROR THEN USE  */
         SLR   R6,R6                                               0541
         CR    R15,R6                                              0541
         BE    @RF00541                                            0541
*                         DO;       /* IKJEFF19 TO DIAGNOSE          */
*                           GFPARMS=GFPARMS&&GFPARMS;              0543
         L     R8,ERRPTR                                           0543
         XC    GFPARMS(44,R8),GFPARMS(R8)                          0543
*                           GFRCODE=R15;                           0544
         ST    R15,GFRCODE(,R8)                                    0544
*                           RFY                                    0545
*                             R15 UNRSTD;                          0545
*                           GFCPPLP=CPPLPTR;                       0546
         L     R15,CPPLPTR(,ECDAPTR7)                              0546
         ST    R15,GFCPPLP(,R8)                                    0546
*                           GFCALLID=GFPARSE;                      0547
         MVC   GFCALLID(2,R8),HW21                                 0547
*                           GF02PTR=CON0;                          0548
         ST    R6,GF02PTR(,R8)                                     0548
*                           RFY                                    0549
*                             R1 RSTD;                             0549
*                           R1=ADDR(ERRPTR);                       0550
         LA    R1,ERRPTR                                           0550
*                           DO;     /* LINK EP(IKJEFF19)             */
*                             I00601='00'X;/* EP/DE FLAG             */
         MVI   I00601,X'00'                                        0552
*                             I00602=ADDR(IKJEFF19);/* ADDR OF EP/DE */
         LA    R8,IKJEFF19                                         0553
         STCM  R8,7,I00602                                         0553
*                             I00604=0;/* DCB PTR                    */
         STCM  R6,7,I00604                                         0554
*                             I00603='00'X;/* ERRET FLAG             */
         MVI   I00603,X'00'                                        0555
*                             RESPECIFY                            0556
*                              (R15) RESTRICTED;                   0556
*                             R15=ADDR(I006);/* ADDR OF LINK LIST */
         LA    R15,I006                                            0557
*                             SVC(6);/* ISSUE LINK SVC               */
         SVC   6                                                   0558
*                             RESPECIFY                            0559
*                              (R15) UNRESTRICTED;                 0559
*                           END;    /* LINK EP(IKJEFF19)             */
*                           RFY                                    0561
*                             R1 UNRSTD;                           0561
*                           NOTEXEC=YES;                           0562
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0562
*                         END;                                     0563
*                       ELSE        /* PARSE SUCCESSFUL - CONTINUE   */
*                         DO;                                      0564
         B     @RC00541                                            0564
@RF00541 DS    0H                                                  0565
*                           SNTABPTR=SNTABFST;/* RESET BASES TO START
*                                      OF PROC                       */
         L     R5,LSDPTR(,ECDAPTR7)                                0565
         L     R6,LSDEXEC-LSD(,R5)                                 0565
         L     R6,SNTABFST(,R6)                                    0565
         ST    R6,SNTABPTR                                         0565
*                           SNTELPTR=USNTABST;/* PARMS LOOP CONTROL  */
         L     SNTELPTR,USNTABST                   R8=SNTELPTR     0566
*                           PDLCUR=VLSTANS+LENGTH(PDLHEAD);/* RESET
*                                      BASES- PDLSTART               */
         LA    R3PDLCUR,8                                          0567
         AL    R3PDLCUR,VLSTANS                                    0567
*                           PCLCUR=PCLBASE+LENGTH(PCEHEAD);/* -    0568
*                                      PCLSTART                      */
         LA    R2PCLCUR,6                                          0568
         ALR   R2PCLCUR,PCLBASE                                    0568
*                           SBFCUR=SBFBASE+LENGTH(SUBFLD);/* -     0569
*                                      SUBSTART                      */
*                                                                  0569
         LA    R4SBFCUR,3                                          0569
         AL    R4SBFCUR,SBFBASE                                    0569
*                           /*****************************************/
*                           /*                                       */
*                           /* PLACE POSITIONAL PARAMETER VALUES IN  */
*                           /* SVTAB                                 */
*                           /*                                       */
*                           /*****************************************/
*                                                                  0570
*                           DO PCTR=1 TO POSNUM WHILE STABERR=NO;  0570
         LA    R5PCTR,1                                            0570
         B     @DE00570                                            0570
@DL00570 TM    STABERR,B'10000000'                                 0570
         BNZ   @DC00570                                            0570
*                             VALADR=IDPDEPTR;/* VALUE ADDRESS       */
         MVC   VALADR(4),IDPDEPTR(R3PDLCUR)                        0571
*                             VALLNG=IDPDELNG;/* VALUE LENGTH        */
         ICM   R6,12,IDPDELNG(R3PDLCUR)                            0572
         SRA   R6,16                                               0572
         ST    R6,VALLNG                                           0572
*                             RFY                                  0573
*                               R15 RSTD;                          0573
*                             CALL SVTABUPT;/* CREATE VALUE ENTRY    */
         BAL   R14,SVTABUPT                                        0574
*                             RFY                                  0575
*                               R15 UNRSTD;                        0575
*                             IF SNTLAST=YES THEN/* IF THIS IS LAST
*                                      ELEMT IN THIS                 */
         TM    SNTLAST(SNTELPTR),B'00000001'                       0576
         BNO   @RF00576                                            0576
*                               DO; /* TABLE THEN UPDATE TO NEXT     */
*                                 SNTABPTR=SNTABNXT;               0578
         L     R2,SNTABPTR                                         0578
         L     R4,SNTABNXT(,R2)                                    0578
         ST    R4,SNTABPTR                                         0578
*                                 SNTELPTR=SNTABPTR+LENGTH(SNTAB); 0579
         AL    R4,FW12                                             0579
         LR    SNTELPTR,R4                         R8=SNTELPTR     0579
*                               END;                               0580
*                             ELSE  /* OTHERWISE UPDATE TO NEXT    0581
*                                      ELEMENT IN THE CURRENT SNTAB  */
*                               SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM
*                                   );                             0581
         B     @RC00576                                            0581
@RF00576 LR    R6,SNTELPTR                                         0581
         AH    R6,SNTLNG(,SNTELPTR)                                0581
         AL    R6,FW08                                             0581
         LR    SNTELPTR,R6                         R8=SNTELPTR     0581
*                             PDLCUR=PDLCUR+LENGTH(IDENTPDE);      0582
@RC00576 AL    R3PDLCUR,FW08                                       0582
*                           END;                                   0583
         AL    R5PCTR,FW01                                         0583
@DE00570 C     R5PCTR,POSNUM                                       0583
         BNH   @DL00570                                            0583
@DC00570 DS    0H                                                  0584
*                                                                  0584
*                           /*****************************************/
*                           /*                                       */
*                           /* PLACE KEYWORD VALUES IN SVTAB         */
*                           /*                                       */
*                           /*****************************************/
*                                                                  0584
*                           DO PCTR=1 TO(KEYNUM+KEYWNUM) WHILE STABERR=
*                                 NO;                              0584
         LA    R5PCTR,1                                            0584
         B     @DE00584                                            0584
@DL00584 TM    STABERR,B'10000000'                                 0584
         BNZ   @DC00584                                            0584
*                             IF SNTKEY=YES THEN/* IF THIS IS A    0585
*                                      KEYWORD WITHOUT               */
         TM    SNTKEY(SNTELPTR),B'01000000'                        0585
         BNO   @RF00585                                            0585
*                               DO; /* VALUE THEN CHECK PDL TO SEE IF
*                                      FOUND OR NOT                  */
*                                 IF KEYPDEP=CON1 THEN/* IF FOUND IN
*                                      VALUE LIST THEN               */
         CLC   KEYPDEP(2,R3PDLCUR),HW01                            0587
         BNE   @RF00587                                            0587
*                                   DO;/* CONVERT IT TO A KEYWORD  0588
*                                      WITH VALUE WITH ITS VALUE THE */
*                                    VALADR=ADDR(SNTDATA);/* KEYWORD
*                                      NAME                          */
         LA    R6,SNTDATA(,SNTELPTR)                               0589
         ST    R6,VALADR                                           0589
*                                    VALLNG=SNTLNG;                0590
         LH    R2,SNTLNG(,SNTELPTR)                                0590
         ST    R2,VALLNG                                           0590
*                                   END;                           0591
*                                 ELSE/* IF NOT FOUND-STILL CONVERT  */
*                                   DO;/* HOWEVER IT HAS A NULL VLAUE*/
         B     @RC00587                                            0592
@RF00587 DS    0H                                                  0593
*                                    VALADR=ADDR(SNTDATA);         0593
         LA    R4,SNTDATA(,SNTELPTR)                               0593
         ST    R4,VALADR                                           0593
*                                    VALLNG=CON0;                  0594
         SLR   R6,R6                                               0594
         ST    R6,VALLNG                                           0594
*                                   END;                           0595
*                               END;                               0596
*                             ELSE  /* OTHERWISE UPDATE THE SVTAB  0597
*                                      WITH THE NEW VALUE IF IT WAS  */
*                               DO; /* SPECIFIED                     */
         B     @RC00585                                            0597
@RF00585 DS    0H                                                  0598
*                                 IF KEYPDEP=CON1 THEN/* IF KEYWORD
*                                      FOUND THEN                    */
         CLC   KEYPDEP(2,R3PDLCUR),HW01                            0598
         BNE   @RF00598                                            0598
*                                   DO;/* REPLACE OLD VALUE          */
*                                    VALADR=VIPDEPTR;              0600
         LA    R2,2                                                0600
         ALR   R2,R3PDLCUR                                         0600
         MVC   VALADR(4),VIPDEPTR(R2)                              0600
*                                    VALLNG=VIPDELNG;              0601
         ICM   R4,12,VIPDELNG(R2)                                  0601
         SRA   R4,16                                               0601
         ST    R4,VALLNG                                           0601
*                                   END;                           0602
*                               END;                               0603
@RF00598 DS    0H                                                  0604
*                             IF SNTKEY=YES /* IF THE KEYWORD VALUE
*                                      HAS                           */
*                                 KEYPDEP=CON1 THEN/* BEEN CHANGED 0604
*                                      THEN GO UPDATE                */
@RC00585 TM    SNTKEY(SNTELPTR),B'01000000'                        0604
         BO    @RT00604                                            0604
         CLC   KEYPDEP(2,R3PDLCUR),HW01                            0604
         BNE   @RF00604                                            0604
@RT00604 DS    0H                                                  0605
*                               DO; /* THE SVTAB                     */
*                                 RFY                              0606
*                                   R15 RSTD;                      0606
*                                 CALL SVTABUPT;/* UPDATE SVTAB      */
         BAL   R14,SVTABUPT                                        0607
*                                 RFY                              0608
*                                   R15 UNRSTD;                    0608
*                               END;                               0609
*                             IF SNTKEY=YES THEN/* UPDATE PDLCUR FOR
*                                      KEYWORDS                      */
@RF00604 TM    SNTKEY(SNTELPTR),B'01000000'                        0610
         BNO   @RF00610                                            0610
*                               PDLCUR=PDLCUR+LENGTH(KEYPDE);      0611
         AL    R3PDLCUR,FW02                                       0611
*                             ELSE  /* UPEATE PDLCUR FOR KEYWORDS  0612
*                                      WITH VALUE                    */
*                               PDLCUR=PDLCUR+LENGTH(KEYPDE)+LENGTH(
*                                   IDENTPDE);                     0612
         B     @RC00610                                            0612
@RF00610 AL    R3PDLCUR,FW10                                       0612
*                             IF SNTLAST=YES THEN/* IF THIS IS THE 0613
*                                      LAST ELMT IN                  */
@RC00610 TM    SNTLAST(SNTELPTR),B'00000001'                       0613
         BNO   @RF00613                                            0613
*                               DO; /* SNTAB THEN UPDATE TO NEXT     */
*                                 SNTABPTR=SNTABNXT;               0615
         L     R6,SNTABPTR                                         0615
         L     R2,SNTABNXT(,R6)                                    0615
         ST    R2,SNTABPTR                                         0615
*                                 SNTELPTR=SNTABPTR+LENGTH(SNTAB); 0616
         AL    R2,FW12                                             0616
         LR    SNTELPTR,R2                         R8=SNTELPTR     0616
*                               END;                               0617
*                             ELSE  /* OTHERWISE UPDATE TO NEXT ELMT
*                                      IN THE CURRENT SNTAB          */
*                               SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM
*                                   );                             0618
         B     @RC00613                                            0618
@RF00613 LR    R4,SNTELPTR                                         0618
         AH    R4,SNTLNG(,SNTELPTR)                                0618
         AL    R4,FW08                                             0618
         LR    SNTELPTR,R4                         R8=SNTELPTR     0618
*                           END;                                   0619
@RC00613 AL    R5PCTR,FW01                                         0619
@DE00584 L     R6,KEYNUM                                           0619
         AL    R6,KEYWNUM                                          0619
         CR    R5PCTR,R6                                           0619
         BNH   @DL00584                                            0619
@DC00584 DS    0H                                                  0620
*                         END;                                     0620
*                       GEN(IKJRLSA VLSTANS);/* FREE VALUE LIST PDL
*                                      CORE                          */
@RC00541 DS    0H                                                  0621
         IKJRLSA VLSTANS
*                       FREEADR=PCLBASE;/* FREE THE VALUE LIST PCL   */
         ST    PCLBASE,FREEADR                                     0622
*                       FREEAMT=PCLBASE->PCLLNG;/* FREE LENGTH       */
         LH    R8,PCLLNG(,PCLBASE)                                 0623
         ST    R8,FREEAMT                                          0623
*                       IF COPYVLST=YES THEN/* IF VALUE LIST WAS     */
         TM    COPYVLST,B'00000010'                                0624
         BNO   @RF00624                                            0624
*                         FREEAMT=FREEAMT+VLSTLNG+CON4;/* COPIED THEN
*                                      UP THE AMT TO BE FREED        */
         L     R6,ECDAEANS(,ECDAPTR7)                              0625
         AH    R8,VLSTLNG(,R6)                                     0625
         AL    R8,FW04                                             0625
         ST    R8,FREEAMT                                          0625
*                       DO;         /* FREEMAIN (E) LV(FREEAMT)    0626
*                                      A(FREEADR) SP(CON1)           */
@RF00624 DS    0H                                                  0627
*                         RESPECIFY                                0627
*                          (R1) RESTRICTED;                        0627
*                         I005=I005&&I005;/* INIT LIST               */
         XC    I005(10),I005                                       0628
*                         I00505='00'X;/* SET TYPE                   */
         MVI   I00505,X'00'                                        0629
*                         I00502=FREEAMT;/* LENGTH                   */
         MVC   I00502(3),FREEAMT+1                                 0630
*                         I00504=ADDR(FREEADR);/* ADDR OF ADDR LIST  */
         LA    R0,FREEADR                                          0631
         STCM  R0,7,I00504                                         0631
*                         I00506=CON1;/* SUBPOOL VALUE               */
         MVI   I00506,X'01'                                        0632
*                         R1=ADDR(I005);/* REG1 POINTS TO LIST       */
         LA    R1,I005                                             0633
*                         SVC(5);   /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0634
*                         RESPECIFY                                0635
*                          (R1) UNRESTRICTED;                      0635
*                       END;        /* FREEMAIN (E) LV(FREEAMT)    0636
*                                      A(FREEADR) SP(CON1)           */
*                     END;                                         0637
*                 END;                                             0638
@RC00438 DS    0H                                                  0639
*             END;                                                 0639
@RC00395 DS    0H                                                  0640
*         END;                                                     0640
@RC00295 DS    0H                                                  0641
*     END;                                                         0641
*                                                                  0641
@RC00254 DS    0H                                                  0642
*   /*****************************************************************/
*   /*                                                               */
*   /* IKJCT431 EXIT POINT                                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0642
*   RETURN CODE(CT431RET);                                         0642
@RC00112 L     R2,CT431RET                                         0642
         L     R13,4(,R13)                                         0642
         L     R0,@SIZDATD                                         0642
         LR    R1,R10                                              0642
         FREEMAIN R,LV=(0),A=(1)
         LR    R15,R2                                              0642
         L     R14,12(,R13)                                        0642
         LM    R0,R12,20(R13)                                      0642
         BR    R14                                                 0642
*                                                                  0643
*   /*****************************************************************/
*   /*                                                               */
*   /* SVTELUPT - UPDATES THE SVTABUSE WITH THE LENGTH OF THE VALUE  */
*   /* JUST ADDED AND UPDATES SNTELPTR TO THE NEXT AVAILABLE SLOT    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0643
*SVTELUPT:                                                         0643
*   PROC;                                                          0643
SVTELUPT STM   R14,R8,12(R13)                                      0643
         STM   R10,R12,60(R13)                                     0643
*   RFY                                                            0644
*     SVTELEM BASED(SVTELPTR);                                     0644
*   SVTABUSE=SVTABUSE+LENGTH(SVTELEM)+SVTORIG;                     0645
         L     R8,SVTABPTR                                         0645
         LA    R14,4                                               0645
         L     R15,SVTABUSE(,R8)                                   0645
         ALR   R15,R14                                             0645
         LH    R0,SVTORIG(,SVTELPTR)                               0645
         ALR   R15,R0                                              0645
         ST    R15,SVTABUSE(,R8)                                   0645
*   SVTELPTR=SVTELPTR+LENGTH(SVTELEM)+SVTORIG;                     0646
         ALR   R14,SVTELPTR                                        0646
         ALR   R14,R0                                              0646
         LR    SVTELPTR,R14                                        0646
*   RFY                                                            0647
*     SVTELEM BASED(SNTVLPTR);                                     0647
*   END SVTELUPT;                                                  0648
@EL00002 DS    0H                                                  0648
@EF00002 DS    0H                                                  0648
@ER00002 LM    R14,R8,12(R13)                                      0648
         LM    R10,R12,60(R13)                                     0648
         BR    R14                                                 0648
*                                                                  0649
*   /*****************************************************************/
*   /*                                                               */
*   /* SKIPSEP- SKIP BLANKS, COMMAS, TABS AND COMMENTS IN THE INPUT  */
*   /* RECORD LOCATED BY ECDAIREC                                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0649
*SKIPSEP:                                                          0649
*   PROC;                                                          0649
SKIPSEP  STM   R14,R5,12(R13)                                      0649
         STM   R7,R8,48(R13)                                       0649
         STM   R10,R12,60(R13)                                     0649
*   CYCLE=CON1;                                                    0650
         LA    R9CYCLE,1                                           0650
*   DO WHILE CYCLE=CON1;                                           0651
         B     @DE00651                                            0651
@DL00651 DS    0H                                                  0652
*     DO ICTR=ICTR TO LINELNG WHILE RECORD(ICTR)=BLANK RECORD(ICTR)=
*           COMMA RECORD(ICTR)=TAB;                                0652
         B     @DE00652                                            0652
@DL00652 L     R8,ECDAIREC(,ECDAPTR7)                              0652
         LR    R1,R8                                               0652
         ALR   R1,R6ICTR                                           0652
         CLI   RECORD-1(R1),C' '                                   0652
         BE    @DB00652                                            0652
         LR    R1,R8                                               0652
         ALR   R1,R6ICTR                                           0652
         CLI   RECORD-1(R1),C','                                   0652
         BE    @DB00652                                            0652
         ALR   R8,R6ICTR                                           0652
         CLI   RECORD-1(R8),X'05'                                  0652
         BNE   @DC00652                                            0652
@DB00652 DS    0H                                                  0653
*     END;                                                         0653
         AL    R6ICTR,FW01                                         0653
@DE00652 C     R6ICTR,LINELNG                                      0653
         BNH   @DL00652                                            0653
@DC00652 DS    0H                                                  0654
*     IF ICTR<LINELNG&RECORD(ICTR:ICTR+CON1)=SLASHAST THEN         0654
         C     R6ICTR,LINELNG                                      0654
         BNL   @RF00654                                            0654
         L     R8,ECDAIREC(,ECDAPTR7)                              0654
         ALR   R8,R6ICTR                                           0654
         CLC   RECORD-1(2,R8),SLSHASTR                             0654
         BNE   @RF00654                                            0654
*       DO;                                                        0655
*         DO ICTR=(ICTR+CON2) TO(LINELNG-CON1) WHILE RECORD(ICTR:ICTR+
*               CON1)^=ASTSLASH;                                   0656
         LA    R8,2                                                0656
         ALR   R8,R6ICTR                                           0656
         LR    R6ICTR,R8                                           0656
         B     @DE00656                                            0656
@DL00656 L     R8,ECDAIREC(,ECDAPTR7)                              0656
         ALR   R8,R6ICTR                                           0656
         CLC   RECORD-1(2,R8),ASTRSLSH                             0656
         BE    @DC00656                                            0656
*         END;                                                     0657
         AL    R6ICTR,FW01                                         0657
@DE00656 L     R8,LINELNG                                          0657
         BCTR  R8,0                                                0657
         CR    R6ICTR,R8                                           0657
         BNH   @DL00656                                            0657
@DC00656 DS    0H                                                  0658
*         IF ICTR=LINELNG THEN                                     0658
         C     R6ICTR,LINELNG                                      0658
         BNE   @RF00658                                            0658
*           DO;                                                    0659
*             CYCLE=CON0;                                          0660
         SLR   R9CYCLE,R9CYCLE                                     0660
*             ICTR=ICTR+CON1;                                      0661
         AL    R6ICTR,FW01                                         0661
*           END;                                                   0662
*         ELSE                                                     0663
*           ICTR=ICTR+CON2;                                        0663
         B     @RC00658                                            0663
@RF00658 AL    R6ICTR,FW02                                         0663
*       END;                                                       0664
*     ELSE                                                         0665
*       CYCLE=CON0;                                                0665
         B     @RC00654                                            0665
@RF00654 SLR   R9CYCLE,R9CYCLE                                     0665
*   END;                                                           0666
@RC00654 DS    0H                                                  0666
@DE00651 C     R9CYCLE,FW01                                        0666
         BE    @DL00651                                            0666
*   END;                            /* PROCEDURE END                 */
@EL00003 DS    0H                                                  0667
@EF00003 DS    0H                                                  0667
@ER00003 LM    R14,R5,12(R13)                                      0667
         LM    R7,R8,48(R13)                                       0667
         LM    R10,R12,60(R13)                                     0667
         BR    R14                                                 0667
*                                                                  0668
*   /*****************************************************************/
*   /*                                                               */
*   /* FINDSEP - FINDS NEXT SEPERATOR IN RECORD LOCATED BY TCDAIBUF  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0668
*FINDSEP:                                                          0668
*   PROC;                                                          0668
FINDSEP  STM   R14,R5,12(R13)                                      0668
         STM   R7,R12,48(R13)                                      0668
*   CYCLE=CON1;                                                    0669
         LA    R9CYCLE,1                                           0669
*   DO WHILE CYCLE=CON1;                                           0670
         B     @DE00670                                            0670
@DL00670 DS    0H                                                  0671
*     DO ICTR=ICTR TO LINELNG WHILE RECORD(ICTR)^=BLANK&RECORD(ICTR)^=
*           COMMA&RECORD(ICTR)^=TAB&RECORD(ICTR)^=SLASH;           0671
         B     @DE00671                                            0671
@DL00671 L     R8,ECDAIREC(,ECDAPTR7)                              0671
         LR    R1,R8                                               0671
         ALR   R1,R6ICTR                                           0671
         CLI   RECORD-1(R1),C' '                                   0671
         BE    @DC00671                                            0671
         LR    R1,R8                                               0671
         ALR   R1,R6ICTR                                           0671
         CLI   RECORD-1(R1),C','                                   0671
         BE    @DC00671                                            0671
         LR    R1,R8                                               0671
         ALR   R1,R6ICTR                                           0671
         CLI   RECORD-1(R1),X'05'                                  0671
         BE    @DC00671                                            0671
         ALR   R8,R6ICTR                                           0671
         CLI   RECORD-1(R8),C'/'                                   0671
         BE    @DC00671                                            0671
*     END;                                                         0672
*                                                                  0672
         AL    R6ICTR,FW01                                         0672
@DE00671 C     R6ICTR,LINELNG                                      0672
         BNH   @DL00671                                            0672
@DC00671 DS    0H                                                  0673
*     /***************************************************************/
*     /*                                                             */
*     /* IF NOT OPENING COMMENT THEN CONTINUE SEARCH                 */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0673
*     IF RECORD(ICTR)=SLASH&(ICTR=LINELNG ICTR<LINELNG&RECORD(ICTR:ICTR
*         +CON1)^=SLASHAST) THEN                                   0673
         L     R8,ECDAIREC(,ECDAPTR7)                              0673
         LR    R1,R8                                               0673
         ALR   R1,R6ICTR                                           0673
         CLI   RECORD-1(R1),C'/'                                   0673
         BNE   @RF00673                                            0673
         L     R2,LINELNG                                          0673
         CR    R6ICTR,R2                                           0673
         BE    @RT00673                                            0673
         CR    R6ICTR,R2                                           0673
         BNL   @RF00673                                            0673
         ALR   R8,R6ICTR                                           0673
         CLC   RECORD-1(2,R8),SLSHASTR                             0673
         BE    @RF00673                                            0673
@RT00673 DS    0H                                                  0674
*       ICTR=ICTR+CON1;                                            0674
         AL    R6ICTR,FW01                                         0674
*     ELSE                                                         0675
*       CYCLE=CON0;                                                0675
         B     @RC00673                                            0675
@RF00673 SLR   R9CYCLE,R9CYCLE                                     0675
*   END;                                                           0676
@RC00673 DS    0H                                                  0676
@DE00670 C     R9CYCLE,FW01                                        0676
         BE    @DL00670                                            0676
*   END;                            /* END OF FINDSEP PROCEDURE      */
@EL00004 DS    0H                                                  0677
@EF00004 DS    0H                                                  0677
@ER00004 LM    R14,R5,12(R13)                                      0677
         LM    R7,R12,48(R13)                                      0677
         BR    R14                                                 0677
*                                                                  0678
*   /*****************************************************************/
*   /*                                                               */
*   /* VALIDATE ROUTINE - THIS SUBROUTINE VERIFIES THAT REMAINING    */
*   /* CHARACTERS OF A SYMBOLIC PARAMETER UP TO FIRST SEPERATOR (OR  */
*   /* LEFT PAREN FOR KEYWORDS) ARE VALID ALPHANUMERIC               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0678
*VALIDATE:                                                         0678
*   PROC;                                                          0678
VALIDATE STM   R14,R5,@SA00005                                     0678
         STM   R7,R12,@SA00005+32                                  0678
*   VALIDERR=NO;                                                   0679
         NI    VALIDERR,B'11111011'                                0679
*   CYCLE=CON1;                                                    0680
         LA    R9CYCLE,1                                           0680
*   DO ICTR=ICTR TO LINELNG WHILE CYCLE=CON1&VALIDERR=NO;          0681
         B     @DE00681                                            0681
@DL00681 C     R9CYCLE,FW01                                        0681
         BNE   @DC00681                                            0681
         TM    VALIDERR,B'00000100'                                0681
         BNZ   @DC00681                                            0681
*     IF RECORD(ICTR)<CONCA         /* IF THE CHARACTER IS NOT A THRU*/
*         (RECORD(ICTR)>CONCI&      /* Z OR O THRU 9 THEN MAKE SURE  */
*         RECORD(ICTR)<CONCJ)       /* IT WAS A SEPERATOR BY CALLING */
*         (RECORD(ICTR)>CONCR&RECORD(ICTR)<CONCS) (RECORD(ICTR)>CONCZ&
*         RECORD(ICTR)<CONC0) RECORD(ICTR)>CONC9 THEN/* SKIPSEP - IF
*                                      OFFSETS DIFFER                */
         L     R8,ECDAIREC(,ECDAPTR7)                              0682
         LR    R1,R8                                               0682
         ALR   R1,R6ICTR                                           0682
         CLI   RECORD-1(R1),C'A'                                   0682
         BL    @RT00682                                            0682
         LR    R1,R8                                               0682
         ALR   R1,R6ICTR                                           0682
         CLI   RECORD-1(R1),C'I'                                   0682
         BNH   @GL00044                                            0682
         ALR   R8,R6ICTR                                           0682
         CLI   RECORD-1(R8),C'J'                                   0682
         BL    @RT00682                                            0682
@GL00044 L     R8,ECDAIREC(,ECDAPTR7)                              0682
         LR    R1,R8                                               0682
         ALR   R1,R6ICTR                                           0682
         CLI   RECORD-1(R1),C'R'                                   0682
         BNH   @GL00043                                            0682
         ALR   R8,R6ICTR                                           0682
         CLI   RECORD-1(R8),C'S'                                   0682
         BL    @RT00682                                            0682
@GL00043 L     R8,ECDAIREC(,ECDAPTR7)                              0682
         LR    R1,R8                                               0682
         ALR   R1,R6ICTR                                           0682
         CLI   RECORD-1(R1),C'Z'                                   0682
         BNH   @GL00042                                            0682
         ALR   R8,R6ICTR                                           0682
         CLI   RECORD-1(R8),C'0'                                   0682
         BL    @RT00682                                            0682
@GL00042 L     R8,ECDAIREC(,ECDAPTR7)                              0682
         ALR   R8,R6ICTR                                           0682
         CLI   RECORD-1(R8),C'9'                                   0682
         BNH   @RF00682                                            0682
@RT00682 DS    0H                                                  0683
*       DO;                         /* UPON RETURN THEN IT IS AN     */
*         JCTR=ICTR;                /* ERROR SYNTAX UNLESS THIS IS   */
         ST    R6ICTR,JCTR                                         0684
*         CALL SKIPSEP;             /* A KEYWORD WITH VALUE          */
         BAL   R14,SKIPSEP                                         0685
*         IF ICTR=JCTR THEN                                        0686
         C     R6ICTR,JCTR                                         0686
         BNE   @RF00686                                            0686
*           IF KEYWORD=YES&RECORD(ICTR)=LFPAREN THEN               0687
         TM    KEYWORD,B'00001000'                                 0687
         BNO   @RF00687                                            0687
         L     R8,ECDAIREC(,ECDAPTR7)                              0687
         ALR   R8,R6ICTR                                           0687
         CLI   RECORD-1(R8),C'('                                   0687
         BNE   @RF00687                                            0687
*             CYCLE=CON0;                                          0688
         SLR   R9CYCLE,R9CYCLE                                     0688
*           ELSE                                                   0689
*             VALIDERR=YES;                                        0689
         B     @RC00687                                            0689
@RF00687 OI    VALIDERR,B'00000100'                                0689
*         ELSE                                                     0690
*           CYCLE=CON0;                                            0690
         B     @RC00686                                            0690
@RF00686 SLR   R9CYCLE,R9CYCLE                                     0690
*       END;                                                       0691
@RC00686 DS    0H                                                  0692
*   END;                                                           0692
@RF00682 AL    R6ICTR,FW01                                         0692
@DE00681 C     R6ICTR,LINELNG                                      0692
         BNH   @DL00681                                            0692
@DC00681 DS    0H                                                  0693
*   IF CYCLE=CON1 THEN                                             0693
         C     R9CYCLE,FW01                                        0693
         BNE   @RF00693                                            0693
*     JCTR=ICTR;                                                   0694
         ST    R6ICTR,JCTR                                         0694
*   ELSE                                                           0695
*     ICTR=ICTR-CON1;                                              0695
         B     @RC00693                                            0695
@RF00693 BCTR  R6ICTR,0                                            0695
*   END;                            /* END OF VALIDATE PROCEDURE     */
@EL00005 DS    0H                                                  0696
@EF00005 DS    0H                                                  0696
@ER00005 LM    R14,R5,@SA00005                                     0696
         LM    R7,R12,@SA00005+32                                  0696
         BR    R14                                                 0696
*                                                                  0697
*   /*****************************************************************/
*   /*                                                               */
*   /* SNTABUPT = THE SNTABUPT ROUTINE UPDATES THE SNTAB(AND SVTAB)  */
*   /* BASED ON AN INPUT OPERATION CODE AND PARM TYPE PARMOP=CREATE-0*/
*   /* CREATE AN ENTRY PARMOP=LOCATE-1 LOCATE AN ENTRY (OR CREATE AN */
*   /* ENTRY IF ENTRY NOT FOUND AND THIS IS NOT A LABLE REQUEST      */
*   /* PARMTYPE=POSIT-1 POSITIONAL PARAMETER =KEY -2 KEYWORD WITHOUT */
*   /* VALUE =KEYWV-3 KEYWORD WITH VALUE                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0697
*SNTABUPT:                                                         0697
*   PROC;                                                          0697
SNTABUPT STM   R14,R7,@SA00006                                     0697
         STM   R9,R12,@SA00006+40                                  0697
*   SNTABPTR=SNTABFST;                                             0698
         L     R3,LSDPTR(,ECDAPTR7)                                0698
         L     R6,LSDEXEC-LSD(,R3)                                 0698
         L     R3,SNTABFST(,R6)                                    0698
         ST    R3,SNTABPTR                                         0698
*   UPTPTR1=SNTABFST+LENGTH(SNTAB); /* ADDRESS OF FIRST ELEMENT      */
*                                                                  0699
         AL    R3,FW12                                             0699
         LR    UPTPTR1,R3                                          0699
*   /*****************************************************************/
*   /*                                                               */
*   /* DETERMINE IF NAME ALREADY DEFINED IN SNTAB                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0700
*   CYCLE=CON1;                                                    0700
         LA    R9CYCLE,1                                           0700
*   DO WHILE UPTPTR1^=SNTELPTR&CYCLE=CON1;                         0701
         B     @DE00701                                            0701
@DL00701 DS    0H                                                  0702
*     IF UPTPTR1->SNTLNG=PARMLNG&UPTPTR1->SNTDATA(1:UPTPTR1->SNTLNG)=
*         PARMDAT(1:UPTPTR1->SNTLNG) THEN                          0702
         LH    R3,SNTLNG(,UPTPTR1)                                 0702
         C     R3,PARMLNG                                          0702
         BNE   @RF00702                                            0702
         BCTR  R3,0                                                0702
         L     R6,PARMADR                                          0702
         EX    R3,@SC01563                                         0702
         BNE   @RF00702                                            0702
*       CYCLE=CON0;                                                0703
         SLR   R9CYCLE,R9CYCLE                                     0703
*     ELSE                                                         0704
*       IF UPTPTR1->SNTLAST=YES THEN                               0704
         B     @RC00702                                            0704
@RF00702 TM    SNTLAST(UPTPTR1),B'00000001'                        0704
         BNO   @RF00704                                            0704
*         DO;                                                      0705
*           SNTABPTR=SNTABNXT;                                     0706
         L     R3,SNTABPTR                                         0706
         L     R6,SNTABNXT(,R3)                                    0706
         ST    R6,SNTABPTR                                         0706
*           UPTPTR1=SNTABPTR+LENGTH(SNTAB);                        0707
         AL    R6,FW12                                             0707
         LR    UPTPTR1,R6                                          0707
*         END;                                                     0708
*       ELSE                                                       0709
*         UPTPTR1=UPTPTR1+UPTPTR1->SNTLNG+LENGTH(SNTELEM);         0709
         B     @RC00704                                            0709
@RF00704 LR    R3,UPTPTR1                                          0709
         AH    R3,SNTLNG(,UPTPTR1)                                 0709
         AL    R3,FW08                                             0709
         LR    UPTPTR1,R3                                          0709
*   END;                                                           0710
@RC00704 DS    0H                                                  0710
@RC00702 DS    0H                                                  0710
@DE00701 CR    UPTPTR1,SNTELPTR                                    0710
         BE    @DC00701                                            0710
         C     R9CYCLE,FW01                                        0710
         BE    @DL00701                                            0710
@DC00701 DS    0H                                                  0711
*   IF CYCLE=CON0 THEN              /* CYCLE SET TO ZERO IF PARAMETER*/
         LTR   R9CYCLE,R9CYCLE                                     0711
         BNZ   @RF00711                                            0711
*     DO;                           /* IS MULTIPLY DEFINED           */
*       EXMSGID=M530;                                              0713
         MVC   EXMSGID(4),$MSGM530                                 0713
*       MVAR(1)=ADDR(MSGINS);                                      0714
         LA    R6,$STMT1                                           0714
         ST    R6,MVAR                                             0714
*       MVARLEN(1)=LENGTH(MSGINS);                                 0715
         MVI   MVARLEN,X'06'                                       0715
*       MVAR(2)=PARMADR;                                           0716
         L     R6,PARMADR                                          0716
         ST    R6,MVAR+4                                           0716
*       MVARLEN(2)=PARMLNG;                                        0717
         L     R6,PARMLNG                                          0717
         STC   R6,MVARLEN+4                                        0717
*       NOTEXEC=YES;                                               0718
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0718
*       CALL MSGRTN;                                               0719
         BAL   R14,MSGRTN                                          0719
*     END;                                                         0720
*   ELSE                            /* PARAMETER WAS NOT ALREADY     */
*     DO;                           /* DEFINED SO WE MUST CREATE AN
*                                      ENTRY                         */
         B     @RC00711                                            0721
@RF00711 DS    0H                                                  0722
*       IF NOTEXEC=NO&(SNTABLNG-SNTABUSE<PARMLNG+LENGTH(SNTELEM)) THEN
         TM    ECDAFLAG(ECDAPTR7),NOTEXEC                          0722
         BNZ   @RF00722                                            0722
         L     R9,SNTABPTR                                         0722
         L     R6,SNTABLNG(,R9)                                    0722
         L     R14,SNTABUSE(,R9)                                   0722
         LR    R15,R6                                              0722
         SLR   R15,R14                                             0722
         LA    R0,8                                                0722
         L     R1,PARMLNG                                          0722
         ALR   R1,R0                                               0722
         CR    R15,R1                                              0722
         BNL   @RF00722                                            0722
*         DO;                       /* IF REMAINING SNTAB NOT LARGE
*                                      ENOUGH THEN GET A NEW BLOCK   */
*           ECDALNEL->SNTLAST=YES;  /* INDICATE LAST ELEMENT IN    0724
*                                      CURRENT SNTAB                 */
*                                                                  0724
         L     R1,ECDALNEL(,ECDAPTR7)                              0724
         OI    SNTLAST(R1),B'00000001'                             0724
*           /*********************************************************/
*           /*                                                       */
*           /* FREE UNUSED AMOUNT                                    */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0725
*           FREEADR=(SNTABPTR+SNTABUSE+CON7)/CON8*CON8;            0725
         LR    R2,R9                                               0725
         ALR   R2,R14                                              0725
         AL    R2,FW07                                             0725
         SRDA  R2,32                                               0725
         DR    R2,R0                                               0725
         SLA   R3,3                                                0725
         ST    R3,FREEADR                                          0725
*           FREEAMT=SNTABPTR+SNTABLNG-FREEADR;                     0726
         ALR   R6,R9                                               0726
         SLR   R6,R3                                               0726
         ST    R6,FREEAMT                                          0726
*           SNTABLNG=SNTABUSE;                                     0727
         ST    R14,SNTABLNG(,R9)                                   0727
         C     R6,FW09
         BNL   LBL01200
         ST    R0,FREEAMT
         B     LBL01222
*           DO;                     /* FREEMAIN (E) LV(FREEAMT)    0728
*                                      A(FREEADR) SP(78)             */
*             RESPECIFY                                            0729
*              (R1) RESTRICTED;                                    0729
*             I005=I005&&I005;      /* INIT LIST                     */
LBL01200 XC    I005(10),I005                                       0730
*             I00505='00'X;         /* SET TYPE                      */
         MVI   I00505,X'00'                                        0731
*             I00502=FREEAMT;       /* LENGTH                        */
         MVC   I00502(3),FREEAMT+1                                 0732
*             I00504=ADDR(FREEADR); /* ADDR OF ADDR LIST             */
         LA    R6,FREEADR                                          0733
         STCM  R6,7,I00504                                         0733
*             I00506=78;            /* SUBPOOL VALUE                 */
         MVI   I00506,X'4E'                                        0734
*             R1=ADDR(I005);        /* REG1 POINTS TO LIST           */
         LA    R1,I005                                             0735
*             SVC(5);               /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0736
*             RESPECIFY                                            0737
*              (R1) UNRESTRICTED;                                  0737
*           END;                    /* FREEMAIN (E) LV(FREEAMT)    0738
*                                      A(FREEADR) SP(78)             */
*           GETAMT=(SNTABLNG+FREEAMT+PARMLNG+CON7)/CON8*CON8;      0739
LBL01222 L     R6,SNTABPTR                                         0739
         L     R4,SNTABLNG(,R6)                                    0739
         AL    R4,FREEAMT                                          0739
         AL    R4,PARMLNG                                          0739
         AL    R4,FW07                                             0739
         SRDA  R4,32                                               0739
         D     R4,FW08                                             0739
         SLA   R5,3                                                0739
         ST    R5,GETAMT                                           0739
*           RFY                                                    0740
*             R15 RSTD;                                            0740
*           DO;                     /* GETMAIN (EC) LV(GETAMT)     0741
*                                      A(GETADR) SP(78) RTCD(R15)    */
*             RESPECIFY                                            0742
*              (R1,                                                0742
*               R15) RESTRICTED;                                   0742
*             I004=I004&&I004;      /* INIT LIST                     */
         XC    I004(10),I004                                       0743
*             I00404='20'X;         /* TYPE                          */
         MVI   I00404,X'20'                                        0744
*             I00401=GETAMT;        /* LENGTH                        */
         ST    R5,I00401                                           0745
*             I00403=ADDR(GETADR);  /* ADDR OF ADDR LIST             */
         LA    R9,GETADR                                           0746
         STCM  R9,7,I00403                                         0746
*             I00405=78;            /* SUBPOOL VALUE                 */
         MVI   I00405,X'4E'                                        0747
*             R1=ADDR(I004);        /* REG1 POINTS TO LIST           */
         LA    R1,I004                                             0748
*             SVC(4);               /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0749
*             R15=R15;              /* SET RETURN CODE               */
*             RESPECIFY                                            0751
*              (R1,                                                0751
*               R15) UNRESTRICTED;                                 0751
*           END;                    /* GETMAIN (EC) LV(GETAMT)     0752
*                                      A(GETADR) SP(78) RTCD(R15)    */
*           IF R15^=CON0 THEN       /* RETURN CODE NON ZERO NOTIFY   */
         LTR   R15,R15                                             0753
         BZ    @RF00753                                            0753
*             DO;                   /* TYE USER -NOT ENOUGH STORAGE  */
*               RFY                                                0755
*                 R15 UNRSTD;                                      0755
*               EXMSGID=M511;                                      0756
         MVC   EXMSGID(4),$MSGM511                                 0756
*               NOTEXEC=YES;                                       0757
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0757
*               CALL MSGRTN;                                       0758
         BAL   R14,MSGRTN                                          0758
*               CT431RET=CON16;                                    0759
         MVC   CT431RET(4),FW16                                    0759
*               STABERR=YES;        /* SET ERROR FLAG TO             */
         OI    STABERR,B'10000000'                                 0760
*             END;                  /* AVOID ANY FURHTER PARMS       */
*           ELSE                    /* GETMAIN SUCCESSFUL -          */
*             DO;                   /* INITIALIZE HEADER OF NEW      */
         B     @RC00753                                            0762
@RF00753 DS    0H                                                  0763
*               GETADR->SNTABNXT=CON0;/* BLOCK                       */
         L     R2,GETADR                                           0763
         L     R3,GETAMT                                           0763
         XR    R4,R4                                               0763
         XR    R5,R5                                               0763
         MVCL  R2,R4                                               0763
         L     R6,GETADR
         SLR   R9,R9
         ST    R9,SNTABNXT(,R6)
*               GETADR->SNTABLNG=GETAMT;                           0764
         L     R9,GETAMT                                           0764
         ST    R9,SNTABLNG(,R6)                                    0764
*               GETADR->SNTABUSE=LENGTH(SNTAB);                    0765
         LA    R9,12                                               0765
         ST    R9,SNTABUSE(,R6)                                    0765
*               SNTABNXT=GETADR;                                   0766
         L     R1,SNTABPTR                                         0766
         ST    R6,SNTABNXT(,R1)                                    0766
*               SNTABPTR=SNTABNXT;                                 0767
         ST    R6,SNTABPTR                                         0767
*               SNTELPTR=SNTABPTR+LENGTH(SNTAB);                   0768
         ALR   R6,R9                                               0768
         LR    SNTELPTR,R6                         R8=SNTELPTR     0768
*             END;                                                 0769
*         END;                                                     0770
@RC00753 DS    0H                                                  0771
*       IF NOTEXEC=NO THEN          /* MOVE IN NAME OF SYMBOLIC PARM */
@RF00722 TM    ECDAFLAG(ECDAPTR7),NOTEXEC                          0771
         BNZ   @RF00771                                            0771
*         DO;                                                      0772
*           SNTDATA(1:PARMLNG)=PARMDAT(1:PARMLNG);                 0773
         L     R6,PARMLNG                                          0773
         LR    R9,R6                                               0773
         BCTR  R9,0                                                0773
         L     R1,PARMADR                                          0773
         EX    R9,@SM01567                                         0773
*           SNTLNG=PARMLNG;                                        0774
         STH   R6,SNTLNG(,SNTELPTR)                                0774
*           SNTFLAGS=SNTFLAGS&&SNTFLAGS;                           0775
         XC    SNTFLAGS(2,SNTELPTR),SNTFLAGS(SNTELPTR)             0775
*           SNTVLPTR=ADDR(SVTELFST);                               0776
         L     R9,SVTABPTR                                         0776
         LA    R9,SVTELFST(,R9)                                    0776
         ST    R9,SNTVLPTR(,SNTELPTR)                              0776
*           SNTABUSE=SNTABUSE+SNTLNG+LENGTH(SNTELEM);              0777
         L     R9,SNTABPTR                                         0777
         AL    R6,SNTABUSE(,R9)                                    0777
         AL    R6,FW08                                             0777
         ST    R6,SNTABUSE(,R9)                                    0777
*           IF PARMTYPE=POSIT THEN  /* TURN ON APPROPRIATE SNTFLAGS  */
         LH    R6,PARMTYPE                                         0778
         LTR   R6,R6                                               0778
         BNZ   @RF00778                                            0778
*             SNTPOSIT=YES;                                        0779
         OI    SNTPOSIT(SNTELPTR),B'10000000'                      0779
*           ELSE                                                   0780
*             IF PARMTYPE=KEYWO THEN/* IS IT A KEYWORD WITHOUT VALUE */
         B     @RC00778                                            0780
@RF00778 CLC   PARMTYPE(2),HW01                                    0780
         BNE   @RF00780                                            0780
*               SNTKEY=YES;                                        0781
         OI    SNTKEY(SNTELPTR),B'01000000'                        0781
*             ELSE                  /* IF IT IS A KEYWORD WITH VALUE */
*               DO;                 /* THEN ASSIGN A VTABELEMENT     */
         B     @RC00780                                            0782
@RF00780 DS    0H                                                  0783
*                 SNTKEYW=YES;                                     0783
         OI    SNTKEYW(SNTELPTR),B'00100000'                       0783
*                 CALL SVTABUPT;                                   0784
         BAL   R14,SVTABUPT                                        0784
*               END;                                               0785
*           ECDALNEL=SNTELPTR;                                     0786
@RC00780 DS    0H                                                  0786
@RC00778 ST    SNTELPTR,ECDALNEL(,ECDAPTR7)        R8=SNTELPTR     0786
*           SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);              0787
         LR    R9,SNTELPTR                                         0787
         AH    R9,SNTLNG(,SNTELPTR)                                0787
         AL    R9,FW08                                             0787
         LR    SNTELPTR,R9                         R8=SNTELPTR     0787
*         END;                                                     0788
*     END;                                                         0789
*   END;                            /* END OF SNTAB UPDATE PROCEDURE */
@EL00006 DS    0H                                                  0790
@EF00006 DS    0H                                                  0790
@ER00006 LM    R14,R7,@SA00006                                     0790
         LM    R9,R12,@SA00006+40                                  0790
         BR    R14                                                 0790
*                                                                  0791
*   /*****************************************************************/
*   /*                                                               */
*   /* SVTABUPT - SVTABUPT ROUTINE WILL CREATE OR ASSIGN A NEW VALUE */
*   /* ELEMENT FOR THE VALUE SPECIFIED IN THE SNTAB/SVTAB PARM BLOCK */
*   /* UPON ENTRY THE SNTELPTR IS POINTING TO THE SNTAB ELEMENT      */
*   /* REQUIRING A VALUE ELEMENT                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0791
*SVTABUPT:                                                         0791
*   PROC;                                                          0791
SVTABUPT STM   R14,R7,@SA00007                                     0791
         STM   R9,R12,@SA00007+40                                  0791
*   TOBEADD=SNTELPTR;                                              0792
         LR    TOBEADD,SNTELPTR                                    0792
*   SVTABPTR=SVTABFST;                                             0793
*                                                                  0793
         L     R6,LSDPTR(,ECDAPTR7)                                0793
         L     R6,LSDEXEC-LSD(,R6)                                 0793
         L     R6,SVTABFST(,R6)                                    0793
         ST    R6,SVTABPTR                                         0793
*   /*****************************************************************/
*   /*                                                               */
*   /* FIRST DETERMINE IF THE NEW VALUE WILL FIT IN THE OLD SLOT     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0794
*   IF VALLNG>SVTORIG THEN          /* IF A NEW ELEMENT MUST BE      */
         L     R14,SNTVLPTR(,SNTELPTR)                             0794
         LH    R15,SVTORIG(,R14)                                   0794
         C     R15,VALLNG                                          0794
         BNL   @RF00794                                            0794
*     DO;                           /* ASSIGNED THEN ADD THE CURRENT */
*       IF SNTVLPTR^=ADDR(SVTELFST) THEN/* ELEMENT LENGTH TO THE     */
         LA    R0,SVTELFST(,R6)                                    0796
         CR    R14,R0                                              0796
         BE    @RF00796                                            0796
*         SVTABFRE=SVTABFRE+SVTORIG+LENGTH(SVTELEM);/* FREE SPACE    */
*                                                                  0797
         AL    R15,SVTABFRE(,R6)                                   0797
         AL    R15,FW04                                            0797
         ST    R15,SVTABFRE(,R6)                                   0797
*       /*************************************************************/
*       /*                                                           */
*       /* IF THE NEW VALUE WILL NOT FIT IN THE EXISTING SVTAB       */
*       /* ELEMENT THEN A NEW ELEMENT MUST BE ASSIGNED FROM THE      */
*       /* REMAINING SPACE IN THE SVTAB.                             */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0798
*       IF(VALLNG+LENGTH(SVTELEM))>(SVTABLNG-SVTABUSE) THEN/* IF   0798
*                                      VALUE                         */
@RF00796 L     R6,VALLNG                                           0798
         LA    R14,4                                               0798
         ALR   R14,R6                                              0798
         L     R15,SVTABPTR                                        0798
         L     R0,SVTABLNG(,R15)                                   0798
         L     R1,SVTABUSE(,R15)                                   0798
         LR    R2,R0                                               0798
         SLR   R2,R1                                               0798
         CR    R14,R2                                              0798
         BNH   @RF00798                                            0798
*         DO;                       /* WILL NOT FIX IN EXISTING SVTAB
*                                      THEN WE WILL HAVE TO GET A NEW
*                                      SVTAB                         */
*           GETAMT=SVTABUSE-SVTABFRE;                              0800
         SL    R1,SVTABFRE(,R15)                                   0800
         ST    R1,GETAMT                                           0800
*           GETAMT=MAX(GETAMT,SVTABLNG);/* SELECT THE MAXIMUM OF THE
*                                      AMOUNT CALCULATED ABOVE AND 0801
*                                      THE LENGTH OF THE CURRENT   0801
*                                      SVTAB TABLE                   */
         CR    R1,R0                                               0801
         BNL   *+6
         LR    R1,R0                                               0801
         ST    R1,GETAMT                                           0801
*           GETAMT=GETAMT+MAX(GETAMT/2,VALLNG+GAS);                0802
         LR    R14,R1                                              0802
         SRDA  R14,32                                              0802
         D     R14,FW02                                            0802
         AL    R6,FW300                                            0802
         CR    R15,R6                                              0802
         BNL   *+6
         LR    R15,R6                                              0802
         ALR   R1,R15                                              0802
         ST    R1,GETAMT                                           0802
*           RFY                                                    0803
*             R15 RSTD;                                            0803
*           DO;                     /* GETMAIN (EC) LV(GETAMT)     0804
*                                      A(GETADR) SP(78) RTCD(R15)    */
*             RESPECIFY                                            0805
*              (R1,                                                0805
*               R15) RESTRICTED;                                   0805
*             I004=I004&&I004;      /* INIT LIST                     */
         XC    I004(10),I004                                       0806
*             I00404='20'X;         /* TYPE                          */
         MVI   I00404,X'20'                                        0807
*             I00401=GETAMT;        /* LENGTH                        */
         L     R6,GETAMT                                           0808
         ST    R6,I00401                                           0808
*             I00403=ADDR(GETADR);  /* ADDR OF ADDR LIST             */
         LA    R14,GETADR                                          0809
         STCM  R14,7,I00403                                        0809
*             I00405=78;            /* SUBPOOL VALUE                 */
         MVI   I00405,X'4E'                                        0810
*             R1=ADDR(I004);        /* REG1 POINTS TO LIST           */
         LA    R1,I004                                             0811
*             SVC(4);               /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0812
*             R15=R15;              /* SET RETURN CODE               */
*             RESPECIFY                                            0814
*              (R1,                                                0814
*               R15) UNRESTRICTED;                                 0814
*           END;                    /* GETMAIN (EC) LV(GETAMT)     0815
*                                      A(GETADR) SP(78) RTCD(R15)    */
*           IF R15^=CON0 THEN                                      0816
         LTR   R15,R15                                             0816
         BZ    @RF00816                                            0816
*             DO;                                                  0817
*               RFY                                                0818
*                 R15 UNRSTD;                                      0818
*               EXMSGID=M511;                                      0819
         MVC   EXMSGID(4),$MSGM511                                 0819
*               CT431RET=CON16;                                    0820
         MVC   CT431RET(4),FW16                                    0820
*               NOTEXEC=YES;                                       0821
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0821
*               CALL MSGRTN;                                       0822
         BAL   R14,MSGRTN                                          0822
*               STABERR=YES;                                       0823
         OI    STABERR,B'10000000'                                 0823
*               RETURN;                                            0824
@EL00007 DS    0H                                                  0824
@EF00007 DS    0H                                                  0824
@ER00007 LM    R14,R7,@SA00007                                     0824
         LM    R9,R12,@SA00007+40                                  0824
         BR    R14                                                 0824
*             END;                                                 0825
*           ELSE                    /* INITIAL SVTAB HEADER FIELDS   */
*             DO;                   /* IN THE NEW BLOCK              */
@RF00816 DS    0H                                                  0827
         L     R2,GETADR
         L     R3,GETAMT
         XR    R4,R4
         XR    R5,R5
         MVCL  R2,R4
*               GETADR->SVTABNXT=CON0;                             0827
         SLR   R4,R4                                               0827
         L     R5,GETADR                                           0827
         ST    R4,SVTABNXT(,R5)                                    0827
*               GETADR->SVTABLNG=GETAMT;                           0828
         L     R15,GETAMT                                          0828
         ST    R15,SVTABLNG(,R5)                                   0828
*               GETADR->SVTABUSE=LENGTH(SVTAB);                    0829
         LA    R0,16                                               0829
         ST    R0,SVTABUSE(,R5)                                    0829
*               GETADR->SVTABFRE=CON0;                             0830
         ST    R4,SVTABFRE(,R5)                                    0830
*               SVTABNXT=GETADR;                                   0831
         L     R1,SVTABPTR                                         0831
         ST    R5,SVTABNXT(,R1)                                    0831
*               NEWVELEM=SVTABNXT+LENGTH(SVTAB);/* FITST ASSIGNABLE
*                                      ELEMENT IN NEW SVTAB          */
         ALR   R0,R5                                               0832
         LR    NEWVELEM,R0                                         0832
*               NEWVELEM->SVTLNG=CON0;/* CREATE A NULL ELEMENT AS    */
         STH   R4,SVTLNG(,NEWVELEM)                                0833
*               NEWVELEM->SVTORIG=CON0;/* FIRST ELEMENT IN NEW SVTAB */
         STH   R4,SVTORIG(,NEWVELEM)                               0834
*               NEWVELEM=NEWVELEM+LENGTH(SVTELEM);                 0835
         LA    R4,4                                                0835
         ALR   NEWVELEM,R4                                         0835
*               SVTABNXT->SVTABUSE=SVTABNXT->SVTABUSE+LENGTH(SVTELEM);
         AL    R4,SVTABUSE(,R5)                                    0836
         ST    R4,SVTABUSE(,R5)                                    0836
*               SAVSNTAB=SNTABPTR;  /* SAVE ADDRESS OF CURRENT SNTAB
*                                      ESTABLISH BASE TO FIRST SNTAB */
         L     R14,SNTABPTR                                        0837
         ST    R14,SAVSNTAB                                        0837
*               SNTABPTR=SNTABFST;  /* AND FIRST SNTAB ELEMENT       */
         L     R1,LSDPTR(,ECDAPTR7)                                0838
         L     R1,LSDEXEC-LSD(,R1)                                 0838
         L     R2,SNTABFST(,R1)                                    0838
         ST    R2,SNTABPTR                                         0838
*               SNTELPTR=SNTABPTR+LENGTH(SNTAB);                   0839
         AL    R2,FW12                                             0839
         LR    SNTELPTR,R2                         R8=SNTELPTR     0839
*               RFY                                                0840
*                (R2) RSTD;                                        0840
*               R2=NEWVELEM;        /* REGISTER TWO IS START OF FREE
*                                      SPACE IN NEW SVTAB, UPDATED 0841
*                                      AUTO BY MVCL                  */
*                                                                  0841
         LR    R2,NEWVELEM                                         0841
*               /*****************************************************/
*               /*                                                   */
*               /* COPY VALUE ELEMENTS TO NEW TABLE                  */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0842
*               LASTELMT=ECDALNEL+ECDALNEL->SNTLNG+LENGTH(SNTELEM);
         L     R3,ECDALNEL(,ECDAPTR7)                              0842
         LH    LASTELMT,SNTLNG(,R3)                                0842
         ALR   LASTELMT,R3                                         0842
         AL    LASTELMT,FW08                                       0842
*               DO WHILE SNTELPTR^=LASTELMT;                       0843
         B     @DE00843                                            0843
@DL00843 DS    0H                                                  0844
*                 IF SNTELPTR^=TOBEADD THEN/* IF WE ARE AT THE     0844
*                                      ELEMENT                       */
         CR    SNTELPTR,TOBEADD                    R8=SNTELPTR     0844
         BE    @RF00844                                            0844
*                   DO;             /* THAT IS CHANGING THEN BYPASS
*                                      THE VALUE MOVE                */
*                     IF SNTVLPTR^=ADDR(SVTELFST) THEN/* IF THIS IS
*                                      NOT                           */
         L     R3,SNTVLPTR(,SNTELPTR)                              0846
         L     R1,SVTABPTR                                         0846
         LA    R4,SVTELFST(,R1)                                    0846
         CR    R3,R4                                               0846
         BE    @RF00846                                            0846
*                       DO;         /* A NULL VALUE THEN COPY VALUE  */
*                         RFY                                      0848
*                          (R3,                                    0848
*                           R4,                                    0848
*                           R5) RSTD;                              0848
*                         R4=SNTVLPTR;                             0849
         L     R14,SNTVLPTR(,SNTELPTR)                             0849
         LR    R4,R14                                              0849
*                         R3=SVTORIG+LENGTH(SVTELEM);              0850
         LA    R3,4                                                0850
         AH    R3,SVTORIG(,R14)                                    0850
*                         R5=R3;                                   0851
         LR    R5,R3                                               0851
*                         VLPTR=R2;                                0852
         ST    R2,VLPTR                                            0852
*                         MVCL(R2,R4);                             0853
         MVCL  R2,R4                                               0853
*                         SVTABNXT->SVTABUSE=SVTABNXT->SVTABUSE+SVTORIG
*                             +LENGTH(SVTELEM);                    0854
         L     R1,SVTABPTR                                         0854
         L     R14,SVTABNXT(,R1)                                   0854
         L     R15,SNTVLPTR(,SNTELPTR)                             0854
         LH    R0,SVTORIG(,R15)                                    0854
         L     R1,SVTABUSE(,R14)                                   0854
         ALR   R1,R0                                               0854
         AL    R1,FW04                                             0854
         ST    R1,SVTABUSE(,R14)                                   0854
*                         SVTABNXT->SVTABFRE=SVTABNXT->SVTABFRE+(  0855
*                             SVTORIG-SVTLNG);                     0855
         SH    R0,SVTLNG(,R15)                                     0855
         AL    R0,SVTABFRE(,R14)                                   0855
         ST    R0,SVTABFRE(,R14)                                   0855
*                         SNTVLPTR=VLPTR;                          0856
         L     R15,VLPTR                                           0856
         ST    R15,SNTVLPTR(,SNTELPTR)                             0856
*                         RFY                                      0857
*                          (R3,                                    0857
*                           R4,                                    0857
*                           R5) UNRSTD;                            0857
*                       END;                                       0858
*                     ELSE          /* OTHERWISE IF NULL ASSIGN VALUE*/
*                       SNTVLPTR=SVTABNXT+LENGTH(SVTAB);/* TO FIRST
*                                      ENTRY IN NEW SVTAB            */
         B     @RC00846                                            0859
@RF00846 LA    R3,16                                               0859
         L     R1,SVTABPTR                                         0859
         AL    R3,SVTABNXT(,R1)                                    0859
         ST    R3,SNTVLPTR(,SNTELPTR)                              0859
*                   END;                                           0860
@RC00846 DS    0H                                                  0861
*                 IF SNTLAST=YES THEN/* IF AT LAST ELEMENT IN SNTAB  */
@RF00844 TM    SNTLAST(SNTELPTR),B'00000001'                       0861
         BNO   @RF00861                                            0861
*                   DO;             /* THEN UPDATE TO NEXT SNTAB     */
*                     SNTABPTR=SNTABNXT;/* AND ITS FIRST ELEMENT     */
         L     R3,SNTABPTR                                         0863
         L     R3,SNTABNXT(,R3)                                    0863
         ST    R3,SNTABPTR                                         0863
*                     SNTELPTR=SNTABPTR+LENGTH(SNTAB);             0864
         AL    R3,FW12                                             0864
         LR    SNTELPTR,R3                         R8=SNTELPTR     0864
*                   END;                                           0865
*                 ELSE              /* OTHERWISE UPDATE TO NEXT      */
*                   SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);/*    0866
*                                      ELEMENT IN THE CURRENT SNTAB
*                                      TABLE                         */
         B     @RC00861                                            0866
@RF00861 LR    R3,SNTELPTR                                         0866
         AH    R3,SNTLNG(,SNTELPTR)                                0866
         AL    R3,FW08                                             0866
         LR    SNTELPTR,R3                         R8=SNTELPTR     0866
*               END;                                               0867
@RC00861 DS    0H                                                  0867
@DE00843 CR    SNTELPTR,LASTELMT                   R8=SNTELPTR     0867
         BE    LBL0154E
         L     R3,SNTVLPTR(,SNTELPTR)
         LTR   R3,R3
         BNZ   @DL00843
*               RFY                                                0868
*                 R2 UNRSTD;                                       0868
*               FREEADR=SVTABPTR;   /* FREE THE OLD SVTAB WHEN ALL   */
LBL0154E L     R6,SVTABPTR                                         0869
         ST    R6,FREEADR                                          0869
*               FREEAMT=SVTABLNG;   /* VALUES HAVE BEEN COPIED       */
         L     R8,SVTABLNG(,R6)                                    0870
         ST    R8,FREEAMT                                          0870
*               SVTABFST=SVTABNXT;                                 0871
         L     R1,LSDPTR(,ECDAPTR7)                                0871
         L     R1,LSDEXEC-LSD(,R1)                                 0871
         L     R6,SVTABNXT(,R6)                                    0871
         ST    R6,SVTABFST(,R1)                                    0871
*               SVTABPTR=SVTABFST;                                 0872
         ST    R6,SVTABPTR                                         0872
*               DO;                 /* FREEMAIN (E) LV(FREEAMT)    0873
*                                      A(FREEADR) SP(78)             */
*                 RESPECIFY                                        0874
*                  (R1) RESTRICTED;                                0874
*                 I005=I005&&I005;  /* INIT LIST                     */
         XC    I005(10),I005                                       0875
*                 I00505='00'X;     /* SET TYPE                      */
         MVI   I00505,X'00'                                        0876
*                 I00502=FREEAMT;   /* LENGTH                        */
         STCM  R8,7,I00502                                         0877
*                 I00504=ADDR(FREEADR);/* ADDR OF ADDR LIST          */
         LA    R8,FREEADR                                          0878
         STCM  R8,7,I00504                                         0878
*                 I00506=78;        /* SUBPOOL VALUE                 */
         MVI   I00506,X'4E'                                        0879
*                 R1=ADDR(I005);/* REG1 POINTS TO LIST               */
         LA    R1,I005                                             0880
*                 SVC(5);           /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0881
*                 RESPECIFY                                        0882
*                  (R1) UNRESTRICTED;                              0882
*               END;                /* FREEMAIN (E) LV(FREEAMT)    0883
*                                      A(FREEADR) SP(78)             */
*             END;                                                 0884
*           SNTABPTR=SAVSNTAB;      /* RESTORE TO CURRENT SNTAB      */
         L     R6,SAVSNTAB                                         0885
         ST    R6,SNTABPTR                                         0885
*           SNTELPTR=TOBEADD;       /* RESTORE SNTAB ELEMENT BASE    */
         LR    SNTELPTR,TOBEADD                    R8=SNTELPTR     0886
*         END;                                                     0887
*       SVTELPTR=SVTABFST+SVTABUSE; /* PLACE NEW ELEMENT AT END OF   */
@RF00798 L     R6,LSDPTR(,ECDAPTR7)                                0888
         L     R6,LSDEXEC-LSD(,R6)                                 0888
         L     R14,SVTABPTR                                        0888
         L     R15,SVTABUSE(,R14)                                  0888
         L     SVTELPTR,SVTABFST(,R6)                              0888
         ALR   SVTELPTR,R15                                        0888
*       SNTVLPTR=SVTELPTR;          /* THE OLD (OR NEW) TABLE        */
         ST    SVTELPTR,SNTVLPTR(,SNTELPTR)                        0889
*       SVTORIG=VALLNG;             /* NEW ELEMENTS ARE EXACT IN     */
         L     R6,SNTVLPTR(,SNTELPTR)                              0890
         L     R9,VALLNG                                           0890
         STH   R9,SVTORIG(,R6)                                     0890
*       SVTLNG=SVTORIG;             /* SIZE                          */
         STH   R9,SVTLNG(,R6)                                      0891
*       SVTABUSE=SVTABUSE+SVTORIG+LENGTH(SVTELEM);                 0892
         ALR   R15,R9                                              0892
         AL    R15,FW04                                            0892
         ST    R15,SVTABUSE(,R14)                                  0892
*     END;                                                         0893
*                                                                  0893
*   /*****************************************************************/
*   /*                                                               */
*   /* PLACE THE NEW VALUE INTO THE SVTAB ELEMENT                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0894
*   SVTABFRE=SVTABFRE-(VALLNG-SVTLNG);/* ADJUST THE FREE AREA TO TAKE
*                                      INTO ACCOUNT ANY DIFFERENCE IN
*                                      THE ORIGIONAL LENGTH          */
@RF00794 L     R6,SVTABPTR                                         0894
         L     R9,VALLNG                                           0894
         L     R14,SNTVLPTR(,SNTELPTR)                             0894
         LR    R15,R9                                              0894
         SH    R15,SVTLNG(,R14)                                    0894
         LCR   R15,R15                                             0894
         AL    R15,SVTABFRE(,R6)                                   0894
         ST    R15,SVTABFRE(,R6)                                   0894
*   SVTLNG=VALLNG;                  /* SET NEW LENGTH                */
         STH   R9,SVTLNG(,R14)                                     0895
*   RFY                                                            0896
*    (R2,                                                          0896
*     R3,                                                          0896
*     R4,                                                          0896
*     R5) RSTD;                                                    0896
*   R2=ADDR(SVTDATA);               /* MOVE TO ADDRESS               */
         LA    R2,SVTDATA(,R14)                                    0897
*   R4=VALADR;                      /* MOVE FROM ADDRESS             */
         L     R4,VALADR                                           0898
*   R3=VALLNG;                      /* LENGTH OF MOVE                */
         LR    R3,R9                                               0899
*   R5=R3;                                                         0900
         LR    R5,R3                                               0900
*   MVCL(R2,R4);                    /* MOVE DATA                     */
         MVCL  R2,R4                                               0901
*   RFY                                                            0902
*    (R2,                                                          0902
*     R3,                                                          0902
*     R4,                                                          0902
*     R5) UNRSTD;                                                  0902
*   END SVTABUPT;                                                  0903
         B     @EL00007                                            0903
*                                                                  0904
*   /*****************************************************************/
*   /*                                                               */
*   /* VALUECHK - LOCATES THE START AND ENDING POSITIONS FOR THE     */
*   /* VALUE OF A KEYWORD AND VERIFIES THAT IT IS EITHER A QUOTED    */
*   /* STRING OR A CHARACTER STRING                                  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0904
*VALUECHK:                                                         0904
*   PROC;                                                          0904
VALUECHK STM   R14,R5,@SA00008                                     0904
         STM   R7,R12,@SA00008+32                                  0904
*   LFPARCTR=CON1;                                                 0905
         LA    R8,1                                                0905
         LR    LFPARCTR,R8                                         0905
*   VALSTR=CON0;                                                   0906
         SLR   R3,R3                                               0906
         ST    R3,VALSTR                                           0906
*   VALEND=CON0;                                                   0907
         ST    R3,VALEND                                           0907
*   ICTR=ICTR+CON1;                 /* INCREMENT PAST LEFT PAREN     */
         ALR   R6ICTR,R8                                           0908
*   CALL SKIPSEP;                                                  0909
         BAL   R14,SKIPSEP                                         0909
*   IF ICTR^>LINELNG THEN                                          0910
         C     R6ICTR,LINELNG                                      0910
         BH    @RF00910                                            0910
*     DO;                                                          0911
*       IF RECORD(ICTR)=QUOTE THEN  /* QUOTED STRING PROCESSING      */
         L     R3,ECDAIREC(,ECDAPTR7)                              0912
         ALR   R3,R6ICTR                                           0912
         CLI   RECORD-1(R3),C''''                                  0912
         BNE   @RF00912                                            0912
*         DO;                                                      0913
*           ICTR=ICTR+CON1;         /* INCREMENT PAST FIRST QUOTE    */
         LA    R8,1                                                0914
         ALR   R6ICTR,R8                                           0914
*           VALSTR=ICTR;            /* SAVE START OF VALUE           */
         ST    R6ICTR,VALSTR                                       0915
*           CYCLE=CON1;             /* LOOP CONTROL SEARCH FOR VALUE
*                                      END                           */
         LR    R9CYCLE,R8                                          0916
*           DO ICTR=ICTR TO LINELNG WHILE CYCLE=CON1;              0917
         B     @DE00917                                            0917
@DL00917 LA    R8,1                                                0917
         CR    R9CYCLE,R8                                          0917
         BNE   @DC00917                                            0917
*             IF RECORD(ICTR)=QUOTE THEN/* CHECK FOR ENDING QUOTE    */
         L     R14,ECDAIREC(,ECDAPTR7)                             0918
         LR    R15,R14                                             0918
         ALR   R15,R6ICTR                                          0918
         CLI   RECORD-1(R15),C''''                                 0918
         BNE   @RF00918                                            0918
*               DO;                 /* IF THIS IS A DOUBLE QUOTE THEN
*                                      REMOVE ONE FROM VALUE AND   0919
*                                      CONTINUE SEARCH FOR VALUE END */
*                 IF ICTR<LINELNG&RECORD(ICTR+CON1)=QUOTE THEN     0920
         L     R15,LINELNG                                         0920
         CR    R6ICTR,R15                                          0920
         BNL   @RF00920                                            0920
         LR    R1,R14                                              0920
         ALR   R1,R6ICTR                                           0920
         CLI   RECORD(R1),C''''                                    0920
         BNE   @RF00920                                            0920
*                   DO;                                            0921
*                     RFY                                          0922
*                      (R2,                                        0922
*                       R3,                                        0922
*                       R4,                                        0922
*                       R5) RSTD;                                  0922
*                     R2=ADDR(RECORD(ICTR));                       0923
         LA    R2,RECORD-1(R6ICTR,R14)                             0923
*                     R4=ADDR(RECORD(ICTR+CON1));                  0924
         LA    R4,RECORD(R6ICTR,R14)                               0924
*                     R3=LINELNG-ICTR+CON1;                        0925
         SLR   R15,R6ICTR                                          0925
         ALR   R15,R8                                              0925
         LR    R3,R15                                              0925
*                     R5=R3;                                       0926
         LR    R5,R3                                               0926
*                     MVCL(R2,R4);                                 0927
         MVCL  R2,R4                                               0927
*                     RFY                                          0928
*                      (R2,                                        0928
*                       R3,                                        0928
*                       R4,                                        0928
*                       R5) UNRSTD;                                0928
*                     LINELNG=LINELNG-CON1;                        0929
         L     R8,LINELNG                                          0929
         BCTR  R8,0                                                0929
         ST    R8,LINELNG                                          0929
*                   END;                                           0930
*                 ELSE              /* IF NOT DOUBLE QUOTES THEN THIS*/
*                   DO;             /* IS THE VALUE END              */
         B     @RC00920                                            0931
@RF00920 DS    0H                                                  0932
*                     VALEND=ICTR-CON1;                            0932
         LR    R8,R6ICTR                                           0932
         BCTR  R8,0                                                0932
         ST    R8,VALEND                                           0932
*                     CYCLE=CON0;                                  0933
         SLR   R9CYCLE,R9CYCLE                                     0933
*                     ICTR=ICTR+CON1;                              0934
         AL    R6ICTR,FW01                                         0934
*                     CALL SKIPSEP; /* FIND END OF VALUE (END PAREN) */
         BAL   R14,SKIPSEP                                         0935
*                     IF ICTR<=LINELNG THEN/* IF CLOSING PAREN NOT   */
         C     R6ICTR,LINELNG                                      0936
         BH    @RF00936                                            0936
*                       DO;                                        0937
*                         IF RECORD(ICTR)^=RTPAREN THEN/* FOUND THEN
*                                      IT                            */
         L     R8,ECDAIREC(,ECDAPTR7)                              0938
         LR    R1,R8                                               0938
         ALR   R1,R6ICTR                                           0938
         CLI   RECORD-1(R1),C')'                                   0938
         BE    @RF00938                                            0938
*                           DO;     /* IS AN ERROR                   */
*                             EXMSGID=M508;                        0940
         MVC   EXMSGID(4),$MSGM508                                 0940
*                             MVAR(1)=PARMADR;                     0941
         L     R2,PARMADR                                          0941
         ST    R2,MVAR                                             0941
*                             MVARLEN(1)=ADDR(RECORD(ICTR))-PARMADR+
*                                 CON1;                            0942
         LA    R8,RECORD-1(R6ICTR,R8)                              0942
         SLR   R8,R2                                               0942
         AL    R8,FW01                                             0942
         STC   R8,MVARLEN                                          0942
*                             NOTEXEC=YES;                         0943
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0943
*                             CALL MSGRTN;                         0944
         BAL   R14,MSGRTN                                          0944
*                           END;                                   0945
*                       END;                                       0946
@RF00938 DS    0H                                                  0947
*                   END;                                           0947
@RF00936 DS    0H                                                  0948
*               END;                                               0948
@RC00920 DS    0H                                                  0949
*           END;                    /* IF WE DID NOT FIND END OF THE */
@RF00918 AL    R6ICTR,FW01                                         0949
@DE00917 C     R6ICTR,LINELNG                                      0949
         BNH   @DL00917                                            0949
@DC00917 DS    0H                                                  0950
*           IF CYCLE=CON1 THEN      /* VALUE THEN ASSUME END AT      */
         C     R9CYCLE,FW01                                        0950
         BNE   @RF00950                                            0950
*             DO;                   /* RECORD END                    */
*               EXMSGID=M527;                                      0952
         MVC   EXMSGID(4),$MSGM527                                 0952
*               MVAR(1)=PARMADR;                                   0953
         L     R9,PARMADR                                          0953
         ST    R9,MVAR                                             0953
*               MVARLEN(1)=ADDR(RECORD(ICTR))-PARMADR;             0954
         L     R1,ECDAIREC(,ECDAPTR7)                              0954
         LA    R8,RECORD-1(R6ICTR,R1)                              0954
         SLR   R8,R9                                               0954
         STC   R8,MVARLEN                                          0954
*               CALL MSGRTN;                                       0955
         BAL   R14,MSGRTN                                          0955
*               VALEND=ICTR-CON1;                                  0956
         LR    R9,R6ICTR                                           0956
         BCTR  R9,0                                                0956
         ST    R9,VALEND                                           0956
*             END;                  /* IF THE VALUE WAS A NULL QUOTED*/
*           IF VALEND<VALSTR THEN   /* STRING THEN MAKE IT LOOK LIKE */
@RF00950 L     R14,VALEND                                          0958
         C     R14,VALSTR                                          0958
         BNL   @RF00958                                            0958
*             DO;                   /* WE FOUND NOTHING              */
*               VALEND=CON0;                                       0960
         SLR   R15,R15                                             0960
         ST    R15,VALEND                                          0960
*               VALSTR=CON0;                                       0961
         ST    R15,VALSTR                                          0961
*             END;                                                 0962
*         END;                                                     0963
*                                                                  0963
*       /*************************************************************/
*       /*                                                           */
*       /* CHARACTER STRING VALUE PROCESSING                         */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0964
*       ELSE                                                       0964
*         DO;                                                      0964
         B     @RC00912                                            0964
@RF00912 DS    0H                                                  0965
*           VALSTR=ICTR;            /* SAVE VALUE START              */
         ST    R6ICTR,VALSTR                                       0965
*           CYCLE=CON1;                                            0966
         LA    R9CYCLE,1                                           0966
*           DO ICTR=ICTR TO LINELNG WHILE CYCLE=CON1;              0967
         B     @DE00967                                            0967
@DL00967 LA    R3,1                                                0967
         CR    R9CYCLE,R3                                          0967
         BNE   @DC00967                                            0967
*             IF RECORD(ICTR)=LFPAREN THEN/* IF LEFT PAREN FOUND UP  */
         L     R8,ECDAIREC(,ECDAPTR7)                              0968
         ALR   R8,R6ICTR                                           0968
         CLI   RECORD-1(R8),C'('                                   0968
         BNE   @RF00968                                            0968
*               LFPARCTR=LFPARCTR+CON1;/* COUNT                      */
         ALR   LFPARCTR,R3                                         0969
*             ELSE                                                 0970
*               IF RECORD(ICTR)=RTPAREN THEN/* IF THIS IS A RIGHT    */
         B     @RC00968                                            0970
@RF00968 L     R3,ECDAIREC(,ECDAPTR7)                              0970
         ALR   R3,R6ICTR                                           0970
         CLI   RECORD-1(R3),C')'                                   0970
         BNE   @RF00970                                            0970
*                 DO;               /* PAREN THEN DETERMINE IF WE ARE
*                                      AT END OF THE VALUE           */
*                   LFPARCTR=LFPARCTR-CON1;                        0972
         BCTR  LFPARCTR,0                                          0972
*                   IF LFPARCTR=CON0 THEN                          0973
         LTR   LFPARCTR,LFPARCTR                                   0973
         BNZ   @RF00973                                            0973
*                     DO;                                          0974
*                       VALEND=ICTR-CON1;                          0975
         LR    R8,R6ICTR                                           0975
         BCTR  R8,0                                                0975
         ST    R8,VALEND                                           0975
*                       CYCLE=CON0;                                0976
         SLR   R9CYCLE,R9CYCLE                                     0976
*                     END;                                         0977
*                 END;                                             0978
*               ELSE                /* IF THIS IS A DELIMITER THEN   */
*                 DO;               /* IT IS END OF THE VALUE        */
         B     @RC00970                                            0979
@RF00970 DS    0H                                                  0980
*                   IF RECORD(ICTR)=BLANK RECORD(ICTR)=COMMA RECORD(
*                       ICTR)=TAB (RECORD(ICTR)=SLASH&(ICTR<LINELNG&
*                       RECORD(ICTR:ICTR+CON1)=SLASHAST)) THEN     0980
         L     R3,ECDAIREC(,ECDAPTR7)                              0980
         LR    R8,R3                                               0980
         ALR   R8,R6ICTR                                           0980
         CLI   RECORD-1(R8),C' '                                   0980
         BE    @RT00980                                            0980
         LR    R8,R3                                               0980
         ALR   R8,R6ICTR                                           0980
         CLI   RECORD-1(R8),C','                                   0980
         BE    @RT00980                                            0980
         LR    R8,R3                                               0980
         ALR   R8,R6ICTR                                           0980
         CLI   RECORD-1(R8),X'05'                                  0980
         BE    @RT00980                                            0980
         LR    R8,R3                                               0980
         ALR   R8,R6ICTR                                           0980
         CLI   RECORD-1(R8),C'/'                                   0980
         BNE   @RF00980                                            0980
         C     R6ICTR,LINELNG                                      0980
         BNL   @RF00980                                            0980
         ALR   R3,R6ICTR                                           0980
         CLC   RECORD-1(2,R3),SLSHASTR                             0980
         BNE   @RF00980                                            0980
@RT00980 DS    0H                                                  0981
*                     DO;                                          0981
*                       CYCLE=CON0;                                0982
         SLR   R9CYCLE,R9CYCLE                                     0982
*                       VALEND=ICTR-CON1;                          0983
         LR    R3,R6ICTR                                           0983
         BCTR  R3,0                                                0983
         ST    R3,VALEND                                           0983
*                       CALL SKIPSEP;                              0984
         BAL   R14,SKIPSEP                                         0984
*                       IF ICTR<=LINELNG THEN                      0985
         C     R6ICTR,LINELNG                                      0985
         BH    @RF00985                                            0985
*                         DO;       /* IF NO ENDING PAREN COULD BE   */
*                           IF RECORD(ICTR)^=RTPAREN THEN/* THEN   0987
*                                      NOTIFY                        */
         L     R8,ECDAIREC(,ECDAPTR7)                              0987
         LR    R3,R8                                               0987
         ALR   R3,R6ICTR                                           0987
         CLI   RECORD-1(R3),C')'                                   0987
         BE    @RF00987                                            0987
*                             DO;   /* USER AND RETURN TO SYNTAX ANY */
*                               EXMSGID=M508;/* REMAINING PARMS      */
         MVC   EXMSGID(4),$MSGM508                                 0989
*                               MVAR(1)=PARMADR;                   0990
         L     R3,PARMADR                                          0990
         ST    R3,MVAR                                             0990
*                               MVARLEN(1)=ADDR(RECORD(ICTR))-PARMADR+
*                                   CON1;                          0991
         LA    R8,RECORD-1(R6ICTR,R8)                              0991
         SLR   R8,R3                                               0991
         AL    R8,FW01                                             0991
         STC   R8,MVARLEN                                          0991
*                               NOTEXEC=YES;                       0992
         OI    ECDAFLAG(ECDAPTR7),NOTEXEC                          0992
*                               CALL MSGRTN;                       0993
         BAL   R14,MSGRTN                                          0993
*                             END;                                 0994
*                         END;                                     0995
@RF00987 DS    0H                                                  0996
*                     END;                                         0996
@RF00985 DS    0H                                                  0997
*                 END;                                             0997
@RF00980 DS    0H                                                  0998
*           END;                    /* IF WE DID NOT FIND END OF     */
@RC00970 DS    0H                                                  0998
@RC00968 AL    R6ICTR,FW01                                         0998
@DE00967 C     R6ICTR,LINELNG                                      0998
         BNH   @DL00967                                            0998
@DC00967 DS    0H                                                  0999
*           IF CYCLE=CON1 THEN      /* VALUE THEN ASSUME END AT END  */
         C     R9CYCLE,FW01                                        0999
         BNE   @RF00999                                            0999
*             VALEND=ICTR-CON1;     /* OF RECORD                     */
         LR    R9,R6ICTR                                           1000
         BCTR  R9,0                                                1000
         ST    R9,VALEND                                           1000
*         END;                                                     1001
*     END;                                                         1002
*   END;                                                           1003
*                                                                  1003
@EL00008 DS    0H                                                  1003
@EF00008 DS    0H                                                  1003
@ER00008 LM    R14,R5,@SA00008                                     1003
         LM    R7,R12,@SA00008+32                                  1003
         BR    R14                                                 1003
*   /*****************************************************************/
*   /*                                                               */
*   /* END OF VALUE CHECK PROCEDURE                                  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1004
*                                                                  1004
*   /*****************************************************************/
*   /*                                                               */
*   /* MSGRTN - THE MESSAGE ROUTINE BUILDS THE IKJEFF02 PARAMETER    */
*   /* LIST AND THEN INVOKES IKJEFF02 TO ISSUE A MESSAGE             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1004
*MSGRTN:                                                           1004
*   PROC OPTIONS(SAVEAREA);                                        1004
MSGRTN   STM   R14,R12,12(R13)                                     1004
         ST    R13,@SA00009+4                                      1004
         LA    R14,@SA00009                                        1004
         ST    R14,8(,R13)                                         1004
         LR    R13,R14                                             1004
*   MSGTABLE=MSGTABLE&&MSGTABLE;    /* INITIALIZE IKJEFF02 PARMLIST  */
         L     R6,ERRPTR                                           1005
         XC    MSGTABLE(52,R6),MSGTABLE(R6)                        1005
*   LISTPTR=ADDR(MSGCSECT);         /* START OF MESSAGE PARMS        */
         LA    R8,MSGCSECT(,R6)                                    1006
         ST    R8,LISTPTR(,R6)                                     1006
*   TMCTPTR=CPPLPTR;                /* ADDRESS OF THE CPPL           */
         L     R14,CPPLPTR(,ECDAPTR7)                              1007
         ST    R14,TMCTPTR(,R6)                                    1007
*   ECBPTR=ADDR(ECB);               /* DUMMY ECB ADDRESS             */
         LA    R15,ECB                                             1008
         ST    R15,ECBPTR(,R6)                                     1008
*   MSGCSECT=ADDR(IKJCT435);        /* IKJCT435 IS MESSAGE CSECT     */
         L     R0,IKJCT435                                         1009
         ST    R0,MSGCSECT(,R6)                                    1009
*   MTPUTLSW=YES;                   /* PUTLINE MESSAGE REUESTED      */
*   MTHEXSW=YES;                    /* PRINT RETURN CODES IN HEX     */
         OI    MTPUTLSW(R6),B'01010000'                            1011
*   MSGID=EXMSGID;                  /* ID OF THE MESSAGE TO BE ISSUED*/
         MVC   MSGID(4,R6),EXMSGID                                 1012
*   DO MCTR=1 TO DIM(MVAR);         /* INITIALIZE INSERTION VARIABLES*/
         LA    R2MCTR,1                                            1013
@DL01013 DS    0H                                                  1014
*     MTINSRT(MCTR)=MVAR(MCTR);     /* MVAR HAS BEEN PREVIOUSLY SET  */
         LR    R3,R2MCTR                                           1014
         SLA   R3,2                                                1014
         L     R6,MVAR-4(R3)                                       1014
         L     R1,ERRPTR                                           1014
         ST    R6,MTINSRT-4(R3,R1)                                 1014
*   END;                                                           1015
         AL    R2MCTR,FW01                                         1015
         C     R2MCTR,FW02                                         1015
         BNH   @DL01013                                            1015
*   RFY                                                            1016
*     R1 RSTD;                                                     1016
*   R1=ERRPTR;                      /* ADDRESS OF MSGTABLE           */
         L     R1,ERRPTR                                           1017
*   RFY                                                            1018
*     R1 UNRSTD;                                                   1018
*   DO;                             /* LINK EP(IKJEFF02)             */
*     I00601='00'X;                 /* EP/DE FLAG                    */
         MVI   I00601,X'00'                                        1020
*     I00602=ADDR(IKJEFF02);        /* ADDR OF EP/DE                 */
         LA    R6,IKJEFF02                                         1021
         STCM  R6,7,I00602                                         1021
*     I00604=0;                     /* DCB PTR                       */
         SLR   R6,R6                                               1022
         STCM  R6,7,I00604                                         1022
*     I00603='00'X;                 /* ERRET FLAG                    */
         MVI   I00603,X'00'                                        1023
*     RESPECIFY                                                    1024
*      (R15) RESTRICTED;                                           1024
*     R15=ADDR(I006);               /* ADDR OF LINK LIST             */
         LA    R15,I006                                            1025
*     SVC(6);                       /* ISSUE LINK SVC                */
         SVC   6                                                   1026
*     RESPECIFY                                                    1027
*      (R15) UNRESTRICTED;                                         1027
*   END;                            /* LINK EP(IKJEFF02) USE IKJEFF02
*                                      TO ISSUE MESSAGE              */
*   END MSGRTN;                     /* END OF MSGRTN                 */
@EL00009 L     R13,4(,R13)                                         1029
@EF00009 DS    0H                                                  1029
@ER00009 LM    R14,R12,12(R13)                                     1029
         BR    R14                                                 1029
@EL00001 L     R13,4(,R13)                                         1039
@EF00001 L     R0,@SIZDATD                                         1039
         LR    R1,R10                                              1039
         FREEMAIN R,LV=(0),A=(1)
@ER00001 LM    R14,R12,12(R13)                                     1039
         BR    R14                                                 1039
@DATA    DS    0H
HW03     DC    H'3'
HW13     DC    H'13'
HW21     DC    H'21'
HW69     DC    H'69'
@SM01540 MVC   SNTDATA(0,SNTELPTR),NULLVDAT(R14)
@SM01544 MVC   SVTDATA(0,SVTELPTR),PSCBUSER-PSCB(R6)
@SM01546 MVC   SVTDATA(0,SVTELPTR),CSCBPROC(R1)
@SM01548 MVC   SVTDATA(0,SVTELPTR),PROCNAME(R1)
@SM01557 MVC   IDPRNME(0,R2PCLCUR),SNTDATA(SNTELPTR)
@SM01559 MVC   NADAT(0,R2PCLCUR),SNTDATA(SNTELPTR)
@SM01561 MVC   VIDNAME(0,R4SBFCUR),SNTDATA(SNTELPTR)
@SC01563 CLC   SNTDATA(0,UPTPTR1),PARMDAT(R6)
@SM01567 MVC   SNTDATA(0,SNTELPTR),PARMDAT(R1)
*   DCL                                                            1030
*     FEPATCH FIXED(31) STATIC;                                    1030
*   GEN DATA DEFS(FEPATCH);                                        1031
*   DECLARE                         /* GENERAL PURPOSE REGISTERS     */
*     R15 FIXED(31) REG(15),                                       1032
*     R0 PTR(31) REG(0),                                           1032
*     R1 PTR(31) REG(1),                                           1032
*     R2 PTR(31) REG(2),                                           1032
*     R3 PTR(31) REG(3),                                           1032
*     R4 PTR(31) REG(4),                                           1032
*     R5 PTR(31) REG(5),                                           1032
*     R6 PTR(31) REG(6),                                           1032
*     R7 PTR(31) REG(7),                                           1032
*     R8 PTR(31) REG(8),                                           1032
*     R9 PTR(31) REG(9),                                           1032
*     R14 PTR(31) REG(14),                                         1032
*     R15 PTR(31) REG(15);                                         1032
*   DCL                                                            1033
*     1 I004 DEF(ILIST) BDY(WORD),  /* LIST FOR GETMAIN              */
*      2 I00401 PTR(31),            /* LENGTH                        */
*      2 I00402 CHAR(1),            /* HIARCHY                       */
*      2 I00403 PTR(24),            /* ADDR OF ADDR LIST             */
*      2 I00404 CHAR(1),            /* TYPE OF REQUEST               */
*      2 I00405 PTR(8);             /* SUBPOOL VALUE                 */
*   DCL                                                            1034
*     1 I005 DEF(ILIST) BDY(WORD),  /* LIST FOR FREEMAIN             */
*       3 I00501 PTR(8),            /* RESERVED                      */
*       3 I00502 PTR(24),           /* LENGTH                        */
*       3 I00503 PTR(8),            /* RESERVED                      */
*       3 I00504 PTR(24),           /* ADDR OF ADDR LIST             */
*       3 I00505 BIT(8),            /* TYPE OF FREEMAIN              */
*       3 I00506 PTR(8);            /* SUBPOOL VALUE                 */
*   DCL                                                            1035
*     1 I006 DEF(ILIST),            /* DEFINE LIST                   */
*      2 I00601 CHAR(1),            /* EP/DE FLAG                    */
*      2 I00602 PTR(24),            /* EP/DE PTR                     */
*      2 I00603 CHAR(1),            /* ERRET FLAG                    */
*      2 I00604 PTR(24),            /* DCB PTR                       */
*      2 I00605 PTR(31);            /* ERRET PTR                     */
*   DCL                                                            1036
*     1 I040 DEF(ILIST) BDY(WORD),  /* LIST FOR EXTRACT              */
*      2 I04001 PTR(31),            /* ADDRESS OF USERS ANSWER AREA  */
*      2 I04002 PTR(31),            /* ADDRESS OF TCB                */
*      2 I04003 BIT(16),            /* FLAGS INDICATING TCB FIELDS TO
*                                      EXTRACT                       */
*      2 I04004 CHAR(2);            /* RESERVED SPACE                */
*   DECLARE                                                        1037
*     ILIST CHAR(12) BDY(DWORD);    /* STANDARD LIST                 */
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       1038
*     I031F FIXED(31) BASED,                                       1038
*     I031P PTR(31) BASED,                                         1038
*     I015F FIXED(15) BASED,                                       1038
*     I015P PTR(15) BASED,                                         1038
*     I008P PTR(8) BASED,                                          1038
*     I001C CHAR(1) BASED;                                         1038
*   END                                                            1039
*                                                                  1039
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IKJCPPL )                                       */
         IKJCPPL
*/*%INCLUDE SYSLIB  (IKJEXEC )                                       */
*/*%INCLUDE SYSLIB  (IKJLSD  )                                       */
         IKJLSD
*/*%INCLUDE SYSLIB  (IKJEFFMT)                                       */
*/*%INCLUDE SYSLIB  (IKJEFFGF)                                       */
*/*%INCLUDE SYSLIB  (IKJECT  )                                       */
         IKJECT
*/*%INCLUDE SYSLIB  (IKJPSCB )                                       */
         IKJPSCB
*/*%INCLUDE SYSLIB  (IKJPPL  )                                       */
         IKJPPL
*/*%INCLUDE SYSLIB  (CVT     )                                       */
         CVT   DSECT=YES
         IHAPSA
         IHAASCB
         IEESMCA ,                                              ZP60014
         IKJTCB ,                                               ZP60014
         IKJRB  ,                               2020-12-07      ZP60014
         IHACDE ,                               2020-12-07      ZP60014
         IEZJSCB ,                                              ZP60014
         IEFJSSIB ,                                             ZP60014
*                                                                  1039
*       ;                                                          1039
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    1F
@SA00009 DS    18F
@SA00007 DS    14F
@SA00006 DS    14F
@SA00005 DS    14F
@SA00008 DS    14F
@TF00001 DS    F
IKJCT431 CSECT
         DS    0F
FW01     DC    F'1'
HW01     EQU   FW01+2
FW02     DC    F'2'
HW02     EQU   FW02+2
FW04     DC    F'4'
HW04     EQU   FW04+2
FW07     DC    F'7'
FW08     DC    F'8'
FW09     DC    F'9'
FW10     DC    F'10'
FW11     DC    F'11'
HW11     EQU   FW11+2
FW12     DC    F'12'
FW15     DC    F'15'
FW16     DC    F'16'
FW31     DC    F'31'
FW56     DC    F'56'
HW56     EQU   FW56+2
FW127    DC    F'127'
FW223    DC    F'223'
FW253    DC    F'253'
FW300    DC    F'300'
FW4096   DC    F'4096'
FW8192   DC    F'8192'                                          ZP60014
FW#GRP1  DC    A(GROUPONE)                                      ZP60014
FW#SYM   DC    A(SYMBOLCT)                                      ZP60014
FW#EVAL  DC    A(NEWIMMED)                                      ZP60014
@DATD    DSECT
         DS    0D
SNTABPTR DS    A
SVTABPTR DS    A
ERRPTR   DS    A
         DS    A
TIOTPTR  DS    A
         DS    A
POSNUM   DS    F
KEYNUM   DS    F
KEYWNUM  DS    F
SBFBASE  DS    A
PCLAMT   DS    F
ECB      DS    F
VLSTANS  DS    A
JCTR     DS    F
SAVSNTAB DS    A
CT431RET DS    F
VLPTR    DS    A
VALEND   DS    F
KEYAR    DS    F
USNTABST DS    A
LINELNG  DS    F
VALSTR   DS    F
FREEADR  DS    AL4
         ORG   FREEADR
GETADR   DS    AL4
         ORG   FREEADR+4
FREEAMT  DS    FL4
         ORG   FREEAMT
GETAMT   DS    FL4
         ORG   FREEAMT+4
ERRPARMS DS    CL52
SERVBLK  DS    CL28
EXMSGID  DS    CL4
PACKLOC  DS    CL8
         DS    CL4
CVERTLOC DS    CL8
FLGCT431 DS    BL1
         ORG   FLGCT431
STABERR  DS    BL1
POSPCERR EQU   FLGCT431+0
POSERR   EQU   FLGCT431+0
KEYERR   EQU   FLGCT431+0
KEYWORD  EQU   FLGCT431+0
VALIDERR EQU   FLGCT431+0
COPYVLST EQU   FLGCT431+0
         ORG   FLGCT431+1
         DS    CL3
NTUPTPRM DS    CL20
         ORG   NTUPTPRM
PARMADR  DS    AL4
PARMLNG  DS    FL4
PARMTYPE DS    FL2
PARMOP   DS    FL2
VALADR   DS    AL4
VALLNG   DS    FL4
         ORG   NTUPTPRM+20
ILIST    DS    CL12
MVAR     DS    AL4
         ORG   MVAR+0
MVARLEN  DS    AL1
         ORG   MVARLEN+0
MVARHEX  DS    BL1
         ORG   MVAR+8
IKJCT431 CSECT
         DS    0F
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
IKJCT435 DC    V(IKJCT435)
         DS    0D
@CC01285 DC    C'VALUE FOR KEYWORD    '
@CC01304 DC    C'POSITIONAL'
$OSLEVEL DC    C'OS/VS2 3.8 EBB1102'                            ZP60014
$NOTACTV DC    C'NOT ACTIVE'                         2020-12-07 ZP60014
$ACTIVE  EQU   *-6,6                                 2020-12-07 ZP60014
$OUTLINE DC    C'SYSOUTLINE'                                    ZP60014
$OUTTRAP DC    C'SYSOUTTRAP'                                    ZP60014
$SYSTERM DC    C'SYSTERMID'                                     ZP60014
$SYS4SDT DC    C'SYS4SDATE'            SORTABLE DATE (4D-YR)    ZP60014
$SYS4JDT DC    C'SYS4JDATE'            JULIAN DATE (4D-YR)      ZP60014
$SYS4IDT DC    C'SYS4IDATE'            ISO-FORMAT DATE (4D-YR)  ZP60014
$SYSISPF DC    C'SYSISPF '                           2020-12-07 ZP60014
ISPFLIT  EQU   *-5,5                                 2020-12-07 ZP60014
IKJEFF02 DC    C'IKJEFF02'
IKJEFF19 DC    C'IKJEFF19'
$DATATYP DC    C'DATATYPE'
$SYSSMFI DC    C'SYSSMFID'                                      ZP60014
$SYSOPER DC    C'SYSOPSYS'                                      ZP60014
$SYSSTIM DC    C'SYSSTIME'             SHORTENED TIME           ZP60014
$SYSSDAT DC    C'SYSSDATE'             SORTABLE DATE            ZP60014
$SYSJDAT DC    C'SYSJDATE'             JULIAN DATE              ZP60014
$SYS4DAT DC    C'SYS4DATE'             (4D-YR)                  ZP60014
$SYSLTRM DC    C'SYSLTERM'                                      ZP60014
$SYSWTRM DC    C'SYSWTERM'                                      ZP60014
$SYSTIME DC    C'SYSTIME'
$SYSPROC DC    C'SYSPROC'
$KEYWORD DC    C'KEYWORD'
$SYSSCAN DC    C'SYSSCAN'
$SYSDATE DC    C'SYSDATE'
$SYSNEST DC    C'SYSNEST'
$SYSPREF DC    C'SYSPREF'
$SYSDVAL DC    C'SYSDVAL'
$SYSPCMD DC    C'SYSPCMD'
$SYSSCMD DC    C'SYSSCMD'
$SYSICMD DC    C'SYSICMD'
$STMT1   DC    C'STMT 1'
$LASTCC  DC    C'LASTCC'
$SYSUID  DC    C'SYSUID'
$SUBSTR  DC    C'SUBSTR'
$SYSDLM  DC    C'SYSDLM'
$LENGTH  DC    C'LENGTH'
$SYSENV  DC    C'SYSENV'                                        ZP60014
$SYSJES  DC    C'SYSJES'                                        ZP60014
$SYSCPU  DC    C'SYSCPU'                                        ZP60014
$SYSSRV  DC    C'SYSSRV'                                        ZP60014
$SYSDSN  DC    C'SYSDSN'                                        ZP60014
$MAXCC   DC    C'MAXCC'
$BACK    DC    C'BACK'                                          ZP60014
$FORE    DC    C'FORE'                                          ZP60014
$MSGM511 DC    C'M511'
$MSGM507 DC    C'M507'
$MSGM530 DC    C'M530'
$MSGM527 DC    C'M527'
$MSGA507 DC    C'A507'
$MSGA529 DC    C'A529'
$MSGB529 DC    C'B529'
$EVAL    DC    C'EVAL'
$PROC    DC    C'PROC'
$MSGM506 DC    C'M506'
$MSGM508 DC    C'M508'
$MSGM509 DC    C'M509'
$NRSTR   DC    C'NR'                   KEEP WITH STR 2009-08-23 ZP60014
$STR     DC    C'STR'
$YES     DC    C'YES'
$NO      DC    C'NO'
$CHAR16  DC    C'16'
SLSHASTR DC    C'/*'
ASTRSLSH DC    C'*/'
IDMOD    DS    CL56
         ORG   IDMOD
@NM00045 DC    X'9000'
@NM00046 DC    FL2'56'
@NM00047 DC    FL4'0'
@NM00048 DC    AL1(0)
@NM00049 DC    FL2'25'
@NM00050 DC    X'0012'
@NM00051 DC    CL21'POSITIONAL PARAMETER '
@NM00052 DC    AL1(20)
@NM00053 DC    CL21'POSITIONAL PARAMETER '
         ORG   IDMOD+56
KWMOD    DS    CL11
         ORG   KWMOD
@NM00055 DC    X'4000'
@NM00056 DC    FL2'6'
@NM00057 DS    FL2
@NM00058 DC    X'6000'
@NM00059 DS    CL3
         ORG   KWMOD+11
SUBMOD   DC    X'000000'
ENDMOD   DC    X'00'
NULLVARX DS    0A
NULLENX  DC    AL1(7)
NULLADRX DC    AL3($SYSTIME)           HH:MM:SS
         DC    AL1(7)
         DC    AL3($SYSPCMD)
         DC    AL1(7)
         DC    AL3($SYSSCMD)
         DC    AL1(7)
         DC    AL3($SYSDATE)           MM/DD/YY
         DC    AL1(7)
         DC    AL3($SYSPREF)
         DC    AL1(6)
         DC    AL3($LENGTH)
         DC    AL1(8)
         DC    AL3($DATATYP)
         DC    AL1(3)
         DC    AL3($STR)
         DC    AL1(6)
         DC    AL3($SUBSTR)
         DC    AL1(4)
         DC    AL3($EVAL)
         DC    AL1(7)
         DC    AL3($SYSICMD)
         DC    AL1(6)
         DC    AL3($SYSDLM)
         DC    AL1(6)
         DC    AL3($SYSUID)
         DC    AL1(7)
         DC    AL3($SYSPROC)
         DC    AL1(7)
         DC    AL3($SYSNEST)
         DC    AL1(7)
         DC    AL3($SYSDVAL)
         DC    AL1(6)
         DC    AL3($LASTCC)
         DC    AL1(5)
         DC    AL3($MAXCC)
         DC    AL1(7)
         DC    AL3($SYSSCAN)
         DC    AL1(10)                                          ZP60014
         DC    AL3($OUTLINE)                                    ZP60014
         DC    AL1(10)                                          ZP60014
         DC    AL3($OUTTRAP)                                    ZP60014
GROUPONE EQU   (*-NULLVARX)/4          NEW NOT-AUTH VARS FOLLOW ZP60014
         DC    AL1(6)                                           ZP60014
         DC    AL3($SYSENV)                                     ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSSMFI)                                    ZP60014
         DC    AL1(9)                                           ZP60014
         DC    AL3($SYSTERM)                                    ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSOPER)                                    ZP60014
         DC    AL1(6)                                           ZP60014
         DC    AL3($SYSJES)                                     ZP60014
         DC    AL1(7)                                2009-12-07 ZP60014
         DC    AL3($SYSISPF)                         2009-12-07 ZP60014
NEWIMMED EQU   (*-NULLVARX)/4          NEW IMMED-EVALS FOLLOW   ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSSTIM)           HH:MM                    ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSSDAT)           YY/MM/DD                 ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSJDAT)           YY.DDD                   ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYS4DAT)           MM/DD/YYYY               ZP60014
         DC    AL1(9)                                           ZP60014
         DC    AL3($SYS4SDT)           YYYY/MM/DD               ZP60014
         DC    AL1(9)                                           ZP60014
         DC    AL3($SYS4JDT)           YYYY.DDD                 ZP60014
         DC    AL1(9)                                           ZP60014
         DC    AL3($SYS4IDT)           YYYY-MM-DD               ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSLTRM)                                    ZP60014
         DC    AL1(8)                                           ZP60014
         DC    AL3($SYSWTRM)                                    ZP60014
         DC    AL1(6)                                           ZP60014
         DC    AL3($SYSCPU)                                     ZP60014
         DC    AL1(6)                                           ZP60014
         DC    AL3($SYSSRV)                                     ZP60014
         DC    AL1(6)                                           ZP60014
         DC    AL3($SYSDSN)                                     ZP60014
         DC    AL1(5)                                2009-08-23 ZP60014
         DC    AL3($NRSTR)                           2009-08-23 ZP60014
SYMBOLCT EQU   (*-NULLVARX)/4          SYMBOL ENTRY COUNT       ZP60014
IKJCT431 CSECT
         DS    0H
FEPATCH  DC    ((@DATA-@PSTART)/20)X'00'
         DC    0D'0'                   ALIGN END OF CSECT       ZP60014
@DATD    DSECT
GTTRMLST DS    4A                      GTTERM PLIST             ZP60014
GTTRMPRM DS    H                       PRIMARY SIZE             ZP60014
GTTRMALT DS    H                       ALTERNATE SIZE           ZP60014
GTTRMATR DS    F                       TERMINAL ATTRIBUTES      ZP60014
GTTRMNAM DS    CL16                    TERMINAL NAME (+NETID?)  ZP60014
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD DS    0D
IKJCT431 CSECT
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
UPTPTR1  EQU   R2
TOBEADD  EQU   R9
SVTELPTR EQU   R9
R2KEYWAR EQU   R2
R8POSAR  EQU   R8
NEWVELEM EQU   R6
LASTELMT EQU   R6
LFPARCTR EQU   R2
R2SCTR   EQU   R2
R5PCTR   EQU   R5
R2MCTR   EQU   R2
R6ICTR   EQU   R6
R9CYCLE  EQU   R9
KEYWCHAR EQU   R5
KEYCHAR  EQU   R3
PCLBASE  EQU   R9
R4POSCHR EQU   R4
R3PDLCUR EQU   R3
R4SBFCUR EQU   R4
R2PCLCUR EQU   R2
R8PPLPTR EQU   R8
SNTELPTR EQU   R8
ECDAPTR7 EQU   R7
ECDA     EQU   0
ECDACPPL EQU   ECDA
ECDAGADD EQU   ECDA+4
ECDAEANS EQU   ECDA+8
ECDALSD  EQU   ECDA+12
ECDAIREC EQU   ECDA+16
ECDACBLK EQU   ECDA+20
ECDACPRE EQU   ECDA+24
ECDACNXT EQU   ECDA+28
ECDALNEL EQU   ECDA+40
ECDAFLAG EQU   ECDA+56
IMPLICIT EQU   X'80'
NOTEXEC  EQU   X'40'
SP78CORE EQU   X'20'
SP78BLK  EQU   X'04'
NESTED   EQU   X'02'
ECDAGDAT EQU   ECDA+64
ECDAINME EQU   ECDA+72
ECDAILNG EQU   ECDAINME
EXECDATA EQU   0
SNTABFST EQU   EXECDATA
SVTABFST EQU   EXECDATA+4
GEXECDAT EQU   EXECDATA+8
@NM00004 EQU   EXECDATA+16
EXDATFLG EQU   EXECDATA+32
COMPROC  EQU   0
COMPRPTR EQU   COMPROC
COMPRID  EQU   COMPRPTR
COMPRNXT EQU   COMPRPTR+1
COMPRLNG EQU   COMPROC+4
COMPRUSE EQU   COMPROC+8
SNTAB    EQU   0
SNTABNXT EQU   SNTAB
SNTABLNG EQU   SNTAB+4
SNTABUSE EQU   SNTAB+8
SNTELFST EQU   SNTAB+12
SNTELEM  EQU   0
SNTVLPTR EQU   SNTELEM
SNTFLAGS EQU   SNTELEM+4
SNTPOSIT EQU   SNTFLAGS                B'10000000'
SNTKEY   EQU   SNTFLAGS                B'01000000'
SNTKEYW  EQU   SNTFLAGS                B'00100000'
SNTLABEL EQU   SNTFLAGS                B'00010000'  UNREFD
SNTNOSCN EQU   SNTFLAGS                B'00001000'  UNREFD
SNTNAUTH EQU   SNTFLAGS                B'00000100'
SNTEVAL  EQU   SNTFLAGS                B'00000010'
SNTLAST  EQU   SNTFLAGS                B'00000001'
SNTGLOB  EQU   SNTFLAGS+1              B'10000000'  UNREFD
SNTLNG   EQU   SNTELEM+6
SNTDATA  EQU   SNTELEM+8
SVTAB    EQU   0
SVTABNXT EQU   SVTAB
SVTABLNG EQU   SVTAB+4
SVTABUSE EQU   SVTAB+8
SVTABFRE EQU   SVTAB+12
SVTELFST EQU   SVTAB+16
SVTELEM  EQU   0
SVTLNG   EQU   SVTELEM
SVTORIG  EQU   SVTELEM+2
SVTDATA  EQU   SVTELEM+4
MSGTABLE EQU   0
LISTPTR  EQU   MSGTABLE
TMCTPTR  EQU   MSGTABLE+4
ECBPTR   EQU   MSGTABLE+8
@NM00008 EQU   MSGTABLE+12
MSGCSECT EQU   MSGTABLE+16
SW       EQU   MSGTABLE+20
MTPUTLSW EQU   SW
MTHEXSW  EQU   SW
RETMSG   EQU   MSGTABLE+21
SW2      EQU   MSGTABLE+24
MSGID    EQU   MSGTABLE+40
MTINSRTS EQU   MSGTABLE+44
MTINSRT  EQU   MTINSRTS
MTLEN    EQU   MTINSRT
RET      EQU   0
GFPARMS  EQU   0
GFRCODE  EQU   GFPARMS+4
GF02PTR  EQU   GFPARMS+8
GFCALLID EQU   GFPARMS+12
GFBITS   EQU   GFPARMS+14
GFCPPLP  EQU   GFPARMS+16
IOSRL    EQU   0
IOSTELM  EQU   IOSRL
INSTACK  EQU   0
INSCODE  EQU   INSTACK
NULLVDAT EQU   0
CSCB     EQU   0
CSCBPROC EQU   CSCB+16
PROCNAME EQU   0
IREC     EQU   0
RECLNG   EQU   IREC
RECORD   EQU   IREC+4
PARMDAT  EQU   0
PCELMT   EQU   0
SBFELMT  EQU   0
IDENTPDE EQU   0
IDPDEPTR EQU   IDENTPDE
IDPDELNG EQU   IDENTPDE+4
IDPDEFLG EQU   IDENTPDE+6
KEYPDE   EQU   0
KEYPDEP  EQU   KEYPDE
PDLHEAD  EQU   0
VIDENPDE EQU   0
VIPDEPTR EQU   VIDENPDE
VIPDELNG EQU   VIDENPDE+4
VIPDEFLG EQU   VIDENPDE+6
IDENTMAP EQU   0
IDLNG    EQU   IDENTMAP+2
IDPDLOFF EQU   IDENTMAP+4
IDCHAR   EQU   IDENTMAP+6
IDMODTYP EQU   IDENTMAP+13
IDPRMLNG EQU   IDENTMAP+34
IDPRNME  EQU   IDENTMAP+56
KEYWDMAP EQU   0
KEYLNG   EQU   KEYWDMAP+2
KWPDLOFF EQU   KEYWDMAP+4
KWSUBFLD EQU   KEYWDMAP+6
SBFOFF   EQU   0
SUBFLD   EQU   0
SUBNXSUB EQU   SUBFLD+1
PCEHEAD  EQU   0
PCLLNG   EQU   PCEHEAD
PDLLNG   EQU   PCEHEAD+2
PCLKEYOF EQU   PCEHEAD+4
PCEEND   EQU   0
EXPAROUT EQU   0
DSNFLG   EQU   EXPAROUT+14
MEMFLG   EQU   EXPAROUT+22
PSWDFLG  EQU   EXPAROUT+30
VLSTPTR  EQU   EXPAROUT+32
VLSTLNG  EQU   EXPAROUT+36
VLSTFLG  EQU   EXPAROUT+38
VLST     EQU   VLSTFLG
CMDBUFF  EQU   0
CBUFLNG  EQU   CMDBUFF
CBUFOFF  EQU   CMDBUFF+2
CBUFTEXT EQU   CMDBUFF+4
NEWVLST  EQU   0
NVLSTLNG EQU   NEWVLST
NVLSTOFF EQU   NEWVLST+2
VIDMAP   EQU   0
VIDPRINF EQU   VIDMAP+35
VIDNAME  EQU   VIDMAP+53
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
MSGECB   EQU   0
MSGTMCT  EQU   0
VLSTBUF  EQU   0
APARM    EQU   0
CPPLPTR  EQU   ECDACPPL
CVARMAP  EQU   SNTELFST
CICMDVAL EQU   CVARMAP+142
CDLMVAL  EQU   CVARMAP+157
CUIDVAL  EQU   CVARMAP+171
CPROCVAL EQU   CVARMAP+185
CNESTVAL EQU   CVARMAP+200
CLCCVAL  EQU   CVARMAP+230
CMCCVAL  EQU   CVARMAP+244
CSCANVAL EQU   CVARMAP+257
COUTLINE EQU   CSCANVAL+15                                      ZP60014
COUTTRAP EQU   COUTLINE+18                                      ZP60014
CENVVAL  EQU   COUTTRAP+18                                      ZP60014
CSMFVAL  EQU   CENVVAL+14                                       ZP60014
CTERMVAL EQU   CSMFVAL+16                                       ZP60014
COPERVAL EQU   CTERMVAL+17                                      ZP60014
CJESVAL  EQU   COPERVAL+16                                      ZP60014
CISPFVAL EQU   CJESVAL+14                          A 2020-12-07 ZP60014
CSTIMVAL EQU   CISPFVAL+15                         C 2020-12-07 ZP60014
CSDATVAL EQU   CSTIMVAL+16                                      ZP60014
CJDATVAL EQU   CSDATVAL+16                                      ZP60014
C4DATVAL EQU   CJDATVAL+16                                      ZP60014
C4SDTVAL EQU   C4DATVAL+16                                      ZP60014
C4JDTVAL EQU   C4SDTVAL+17                                      ZP60014
C4IDTVAL EQU   C4JDTVAL+17                                      ZP60014
CLTRMVAL EQU   C4IDTVAL+17                                      ZP60014
CWTRMVAL EQU   CLTRMVAL+16                                      ZP60014
CCPUVAL  EQU   CWTRMVAL+16                                      ZP60014
CSRVVAL  EQU   CCPUVAL+14                                       ZP60014
CDSNVAL  EQU   CSRVVAL+14                                       ZP60014
CNRSVAL  EQU   CDSNVAL+14                            2009-08-23 ZP60014
CLASTVAL EQU   CNRSVAL         LAST NAME ADDED HERE  2009-08-23 ZP60014
LSDPTR   EQU   ECDALSD
LSDBLKID EQU   LSDANEXT
LSDADAID EQU   LSDADATA
NAMPCE   EQU   KEYWDMAP+6
NALNG    EQU   NAMPCE+2
NADATLNG EQU   NAMPCE+4
NADAT    EQU   NAMPCE+5
CPBLKPTR EQU   ECDACBLK+1
SNTABBLK EQU   SNTABPTR+1
SVTABBLK EQU   SVTABPTR+1
NULLVAR  EQU   NULLVARX
NULLEN   EQU   NULLVAR
NULLADR  EQU   NULLVAR+1
I004     EQU   ILIST
I00401   EQU   I004
I00403   EQU   I004+5
I00404   EQU   I004+8
I00405   EQU   I004+9
I040     EQU   ILIST
I04001   EQU   I040
I04002   EQU   I040+4
I04003   EQU   I040+8
I006     EQU   ILIST
I00601   EQU   I006
I00602   EQU   I006+1
I00603   EQU   I006+4
I00604   EQU   I006+5
I005     EQU   ILIST
I00502   EQU   I005+1
I00504   EQU   I005+5
I00505   EQU   I005+8
I00506   EQU   I005+9
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
I00503   EQU   I005+4
I00501   EQU   I005
I00605   EQU   I006+8
I04004   EQU   I040+10
I00402   EQU   I004+4
NAMFLG   EQU   NAMPCE
SYSSCAN  EQU   CVARMAP+265
CSCANLNG EQU   CVARMAP+263
CSCANFLG EQU   CVARMAP+261
MAXCC    EQU   CVARMAP+252
CMCCLNG  EQU   CVARMAP+250
CMCCFLG  EQU   CVARMAP+248
LASTCC   EQU   CVARMAP+238
CLCCLNG  EQU   CVARMAP+236
CLCCFLG  EQU   CVARMAP+234
SYSDVAL  EQU   CVARMAP+223
CDVALLNG EQU   CVARMAP+221
CDVALFLG EQU   CVARMAP+219
CDVALVAL EQU   CVARMAP+215
SYSNEST  EQU   CVARMAP+208
CNESTLNG EQU   CVARMAP+206
CNESTFLG EQU   CVARMAP+204
SYSPROC  EQU   CVARMAP+193
CPROCLNG EQU   CVARMAP+191
CPROCFLG EQU   CVARMAP+189
SYSUID   EQU   CVARMAP+179
CUIDLNG  EQU   CVARMAP+177
CUIDFLG  EQU   CVARMAP+175
SYSDLM   EQU   CVARMAP+165
CDLMLNG  EQU   CVARMAP+163
CDLMFLG  EQU   CVARMAP+161
SYSICMD  EQU   CVARMAP+150
CICMDLNG EQU   CVARMAP+148
CICMDFLG EQU   CVARMAP+146
EVAL     EQU   CVARMAP+138
CEVALLNG EQU   CVARMAP+136
CEVALFLG EQU   CVARMAP+134
CEVALVAL EQU   CVARMAP+130
SUBSTR   EQU   CVARMAP+124
CSSTRLNG EQU   CVARMAP+122
CSSTRFLG EQU   CVARMAP+120
CSSTRVAL EQU   CVARMAP+116
STRING   EQU   CVARMAP+113
CSTRLNG  EQU   CVARMAP+111
CSTRFLG  EQU   CVARMAP+109
CSTRVAL  EQU   CVARMAP+105
DATATYPE EQU   CVARMAP+97
CTYPELNG EQU   CVARMAP+95
CTYPEFLG EQU   CVARMAP+93
CTYPEVAL EQU   CVARMAP+89
CLENGTH  EQU   CVARMAP+83
CLENLNG  EQU   CVARMAP+81
CLENFLG  EQU   CVARMAP+79
CLENVAL  EQU   CVARMAP+75
SYSPREF  EQU   CVARMAP+68
CPREFLNG EQU   CVARMAP+66
CPREFLG  EQU   CVARMAP+64
CPREFVAL EQU   CVARMAP+60
SYSDATE  EQU   CVARMAP+53
CDATELNG EQU   CVARMAP+51
CDATEFLG EQU   CVARMAP+49
CDATEVAL EQU   CVARMAP+45
SYSSCMD  EQU   CVARMAP+38
CSCMDLNG EQU   CVARMAP+36
CSCMDFLG EQU   CVARMAP+34
CSCMDVAL EQU   CVARMAP+30
SYSPCMD  EQU   CVARMAP+23
CPCMDLNG EQU   CVARMAP+21
CPCMDFLG EQU   CVARMAP+19
CPCMDVAL EQU   CVARMAP+15
SYSTIME  EQU   CVARMAP+8
CTIMELNG EQU   CVARMAP+6
CTIMEFLG EQU   CVARMAP+4
CTIMEVAL EQU   CVARMAP
VIDPRLNG EQU   VIDMAP+34
@NM00054 EQU   VIDMAP
NVLSTDAT EQU   NEWVLST+4
PRMPPDE  EQU   EXPAROUT+42
LISTPDE  EQU   EXPAROUT+40
@NM00044 EQU   EXPAROUT+39
@NM00043 EQU   EXPAROUT+31
PSWD     EQU   PSWDFLG
PSWDLNG  EQU   EXPAROUT+28
PSWDPTR  EQU   EXPAROUT+24
@NM00042 EQU   EXPAROUT+23
MEM      EQU   MEMFLG
MEMLNG   EQU   EXPAROUT+20
MEMPTR   EQU   EXPAROUT+16
@NM00041 EQU   EXPAROUT+15
DSNQTE   EQU   DSNFLG
DSN      EQU   DSNFLG
DSNLNG   EQU   EXPAROUT+12
DSNPTR   EQU   EXPAROUT+8
EPDLLNG  EQU   EXPAROUT+6
EPDLSPNO EQU   EXPAROUT+4
EPDLNXT  EQU   EXPAROUT
@NM00040 EQU   SUBFLD
KWNAMDAT EQU   KEYWDMAP+11
KWNAMLNG EQU   KEYWDMAP+10
KWLNG    EQU   KEYWDMAP+8
@NM00039 EQU   KEYWDMAP+6
@NM00038 EQU   KEYWDMAP+6
@NM00037 EQU   KEYWDMAP
IDPRINFO EQU   IDENTMAP+35
@NM00036 EQU   IDENTMAP+7
@NM00035 EQU   IDENTMAP+6
@NM00034 EQU   IDENTMAP+6
@NM00033 EQU   IDENTMAP
IDPROMPT EQU   IDENTMAP
@NM00032 EQU   IDENTMAP
VIPDEP   EQU   VIPDEFLG
PDLLEN   EQU   PDLHEAD+6
PDLSPNO  EQU   PDLHEAD+4
PDLNXT   EQU   PDLHEAD
IDPDEP   EQU   IDPDEFLG
RECOFF   EQU   IREC+2
TIOTSTP  EQU   TIOT+16
TIOTNJOB EQU   TIOT
INSADLSD EQU   INSTACK+1
INSLIST  EQU   INSCODE
INSPROC  EQU   INSCODE
INSPROM  EQU   INSCODE
INSEXEC  EQU   INSCODE
INSOTDD  EQU   INSCODE
INSINDD  EQU   INSCODE
INSSTOR  EQU   INSCODE
INSDATA  EQU   INSCODE
@NM00031 EQU   IOSRL+12
IOSNELM  EQU   IOSRL+10
IOSTLEN  EQU   IOSRL+8
IOSBELM  EQU   IOSRL+4
@NM00016 EQU   GFPARMS+40
@NM00015 EQU   GFPARMS+36
GFPGMNP  EQU   GFPARMS+32
GFDSNP   EQU   GFPARMS+28
GFPGMNL  EQU   GFPARMS+26
GFDSNLEN EQU   GFPARMS+24
GFECBP   EQU   GFPARMS+20
@NM00014 EQU   GFPARMS+15
@NM00013 EQU   GFBITS
GFWTPSW  EQU   GFBITS
GFSUBSYS EQU   GFBITS
GFKEYN08 EQU   GFBITS
GFCBPTR  EQU   GFPARMS
RETCHAR  EQU   RET+2
RETSIZE  EQU   RET
MTADDR   EQU   MTINSRT+1
MTHIGHL  EQU   MTLEN
@NM00012 EQU   MSGTABLE+36
@NM00011 EQU   MSGTABLE+32
MTOLDPTR EQU   MSGTABLE+28
@NM00010 EQU   MSGTABLE+25
@NM00009 EQU   SW2
MTPGMSW  EQU   SW2
MTNPLMSW EQU   SW2
MTNOXQSW EQU   SW2
MTDOMSW  EQU   SW2
MT2OLDSW EQU   SW2
MTNHEXSW EQU   SW
MTWTPSW  EQU   SW
MTJOBISW EQU   SW
MTKEY1SW EQU   SW
MTWTOSW  EQU   SW
MTNOIDSW EQU   SW
MTHIGH   EQU   @NM00008
MTCPPL   EQU   TMCTPTR
@NM00007 EQU   SNTFLAGS+1
SNTGLOB  EQU   SNTFLAGS+1
SNTNOSCN EQU   SNTFLAGS
SNTLABEL EQU   SNTFLAGS
SNTGVAL  EQU   SNTVLPTR
COMRCDS  EQU   COMPROC+12
@NM00006 EQU   EXECDATA+60
FILEDCBS EQU   EXECDATA+56
RETPTR2  EQU   EXECDATA+52
ATACTEND EQU   EXECDATA+48
ATACTSTR EQU   EXECDATA+44
EXDLMPTR EQU   EXECDATA+40
GEXECCNT EQU   EXECDATA+36
NOLASTCC EQU   EXDATFLG+1
ATINCNTL EQU   EXDATFLG+1
ATTNCMD  EQU   EXDATFLG
NOMSG    EQU   EXDATFLG
CMAIN    EQU   EXDATFLG
ERINCNTL EQU   EXDATFLG
SYMLST   EQU   EXDATFLG
NOFLUSH  EQU   EXDATFLG
ERRCMD   EQU   EXDATFLG
CONLST   EQU   EXDATFLG
RETPTR   EQU   EXECDATA+28
ERACTEND EQU   EXECDATA+24
ERACTSTR EQU   EXECDATA+20
EXINSAVE EQU   @NM00004
LASTTSO  EQU   EXECDATA+12
ECDACPGM EQU   ECDA+68
ECDAGETL EQU   ECDA+60
@NM00003 EQU   ECDA+58
@NM00002 EQU   ECDAFLAG
@NM00001 EQU   ECDAFLAG
ECDAAEND EQU   ECDA+52
ECDAELNG EQU   ECDA+50
ECDANDO  EQU   ECDA+48
ECDAGCNT EQU   ECDA+44
ECDALCTR EQU   ECDA+36
ECDASPTR EQU   ECDA+32
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RC00270 EQU   @RC00254
@RC00287 EQU   @RC00279
@RC00365 EQU   @RC00359
@RF00397 EQU   @RC00395
@RF00399 EQU   @RC00395
@RC00587 EQU   @RC00585
@RC00658 EQU   @RC00654
@RC00687 EQU   @RC00686
@RC00693 EQU   @EL00005
@RC00711 EQU   @EL00006
@RF00771 EQU   @EL00006
@RF00973 EQU   @RC00970
@RF00910 EQU   @EL00008
@RC00912 EQU   @EL00008
@RF00999 EQU   @EL00008
@RF00958 EQU   @RC00912
@ENDDATA EQU   *
         END   IKJCT431,(C'PLS-III',0300,87344)
/*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJCT431('ZP60014')
++MOD(IKJCT433) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//STEP04  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IKJCT433, EXEC CONTROL MODULE - PHASE II PART I'
*
*   MODIFIED BY GREG PRICE SEPTEMBER 2005 FOR USERMOD ZP60014
*
*   2009-08-23 - ADD &NRSTR "NO RESCAN STRING"
*
IKJCT433 CSECT ,                                                   0001
         USING PSA,0                                            ZP60014
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL2(@EP00001-@MAINENT)
         DC    AL1(33)                                             0001
         DC    C'IKJCT433  88.013'                                 0001
         DC    C' ZP60014 20090823'
         DROP  R15
IKJUPDT  DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL2(@EP0UPDT-IKJUPDT)                            ZP60014
         ENTRY IKJUPDT
         DROP  R15
IKJINIT  DS    0H                                               ZP60014
         USING *,R15                                            ZP60014
         B     @PROLOG                                          ZP60014
         DC    AL2(@EP0INIT-IKJINIT)                            ZP60014
         ENTRY IKJINIT                                          ZP60014
         DROP  R15                                              ZP60014
IKJGET#  DS    0H                                               ZP60014
         USING *,R15                                            ZP60014
         B     @PROLOG                                          ZP60014
         DC    AL2(@EP0GET#-IKJGET#)                            ZP60014
         ENTRY IKJGET#                                          ZP60014
         DROP  R15                                              ZP60014
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  LA    R11,4095(,R12)                                      0001
         LA    R10,4095(,R11)                                      0001
         USING @PSTART,R12                                         0001
         USING @PSTART+4095,R11                                    0001
         USING @PSTART+8190,R10                                    0001
         L     R0,@SIZDATD                                         0001
         IKJEXEC  R,LV=(0)
         LR    R9,R1                                               0001
         USING @DATD,R9                                            0001
         ST    R13,@SA00001+4                                      0001
         LM    R15,R1,16(R13)                                      0001
         ST    R9,8(,R13)                                          0001
         LR    R13,R9                                              0001
         MVI   LCLFLAGS,0           RESET LOCAL FLAGS           ZP60014
         AH    R15,4(,R15)                                         0001
         BR    R15                                                 0001
@EP00001 MVC   @PC00001(20),0(R1)                                  0001
*                                                                  0035
*   /*****************************************************************/
*   /*                                                               */
*   /* START OF EXECUTABLE CODE                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0035
*   CALL INTSETUP;                  /* INIT MAIN SWITCHES - VARIABLES*/
         BAL   R14,INTSETUP                                        0035
*   OLDLINE=GTPBLINE;               /* POINT TO OLD RECORD           */
         L     R1,@PC00001+12                                      0036
         L     R1,GTPBLINE(,R1)                                    0036
         ST    R1,OLDLINE(,COMONPTR)                               0036
*   SCANLIM=F0;                     /* CLEAR SCAN LIMIT              */
         SLR   R2,R2                                               0037
         ST    R2,SCANLIM                                          0037
*   ERCOM=F0;                       /* INIT ERROR COUNTER (COMMON)   */
         ST    R2,ERCOM(,COMONPTR)                                 0038
*   CONTINU=YES;                    /* ALLOW CONTINUING              */
         OI    CONTINU(COMONPTR),B'01000000'                       0039
*   STM(R9,R12,REGAREA);            /* SAVE PHASE2 PART 1 REGS       */
         STM   R9,R12,REGAREA(COMONPTR)                            0040
*   DO WHILE CONTINU=YES;           /* PROCESS UNTIL DONE            */
         B     @DE00041                                            0041
@DL00041 DS    0H                                                  0042
*     EXECDPTR=LSDEXEC;             /* BASE EXEC DATA ON PTR IN LSD  */
         L     R1,LSDPTR(,COMONPTR)                                0042
         L     R2,LSDEXEC-LSD(,R1)                                 0042
         ST    R2,EXECDPTR(,COMONPTR)                              0042
*     ERROR=F0;                     /* INIT ERROR COUNTER TO ZERO    */
         SLR   R3,R3                                               0043
         ST    R3,ERROR                                            0043
*     SWS=''B;                      /* CLEAR INIT SWITCHES           */
         XC    SWS(5,COMONPTR),SWS(COMONPTR)                       0044
*     NEWBASE=MIN(BUFBASE,NEWBASE); /* POINTER TO OLD BUFFER IF ONE  */
         L     R3,BUFBASE(,COMONPTR)                               0045
         L     R4,NEWBASE(,COMONPTR)                               0045
         CR    R3,R4                                               0045
         BNH   *+6
         LR    R3,R4                                               0045
         ST    R3,NEWBASE(,COMONPTR)                               0045
*     BUFBASE=GTPBLINE;             /* SET BASE TO LINE PASSED       */
         L     R3,@PC00001+12                                      0046
         L     R1,GTPBLINE(,R3)                                    0046
         ST    R1,BUFBASE(,COMONPTR)                               0046
*     IF GTPBLINE>=ERACTSTR&GTPBLINE<ERACTEND&/* IF ERROR ACTIVE     */
*         ERINCNTL=YES THEN         /* AND WITHIN RANGE              */
         C     R1,ERACTSTR(,R2)                                    0047
         BL    @RF00047                                            0047
         C     R1,ERACTEND(,R2)                                    0047
         BNL   @RF00047                                            0047
         TM    ERINCNTL(R2),B'00001000'                            0047
         BNO   @RF00047                                            0047
*       ERRRANGE=YES;               /* SET ERROR RANGE INDICATOR     */
         OI    ERRRANGE(COMONPTR),B'00000100'                      0048
*     ELSE                          /* OTHERWISE                     */
*       IF ERINCNTL=YES THEN        /* IF DROPPING OUT OF ERROR      */
         B     @RC00047                                            0049
@RF00047 L     R2,EXECDPTR(,COMONPTR)                              0049
         TM    ERINCNTL(R2),B'00001000'                            0049
         BNO   @RF00049                                            0049
*         DO;                       /* SWAP RETURN POINTERS          */
*           RETPTR=RETPTR&&RETPTR2; /*                               */
         L     R3,RETPTR2(,R2)                                     0051
         L     R4,RETPTR(,R2)                                      0051
         XR    R4,R3                                               0051
         ST    R4,RETPTR(,R2)                                      0051
*           RETPTR2=RETPTR2&&RETPTR;/*                               */
         XR    R3,R4                                               0052
         ST    R3,RETPTR2(,R2)                                     0052
*           RETPTR=RETPTR&&RETPTR2; /*                               */
         XR    R4,R3                                               0053
         ST    R4,RETPTR(,R2)                                      0053
*           ERINCNTL=NO;            /* RESET SWITCH                  */
         NI    ERINCNTL(R2),B'11110111'                            0054
*         END;                      /*                               */
*     IF GTPBLINE>=ATACTSTR&GTPBLINE<ATACTEND&/* IF ERROR ACTIVE     */
*         ATINCNTL=YES THEN         /* AND WITHIN RANGE              */
@RF00049 DS    0H                                                  0056
@RC00047 L     R5,@PC00001+12                                      0056
         L     R1,GTPBLINE(,R5)                                    0056
         L     R2,EXECDPTR(,COMONPTR)                              0056
         C     R1,ATACTSTR(,R2)                                    0056
         BL    @RF00056                                            0056
         C     R1,ATACTEND(,R2)                                    0056
         BNL   @RF00056                                            0056
         TM    ATINCNTL(R2),B'10000000'                            0056
         BNO   @RF00056                                            0056
*       ATRANGE=YES;                /* SET ERROR RANGE INDICATOR     */
         OI    ATRANGE(COMONPTR),B'00000010'                       0057
*     ELSE                          /* OTHERWISE                     */
*       IF ATINCNTL=YES THEN        /* IF DROPPING OUT OF ERROR      */
         B     @RC00056                                            0058
@RF00056 L     R3,EXECDPTR(,COMONPTR)                              0058
         TM    ATINCNTL(R3),B'10000000'                            0058
         BNO   @RF00058                                            0058
*         DO;                       /* SWAP RETURN POINTERS          */
*           RETPTR=RETPTR&&RETPTR2; /*                               */
         L     R2,RETPTR2(,R3)                                     0060
         L     R4,RETPTR(,R3)                                      0060
         XR    R4,R2                                               0060
         ST    R4,RETPTR(,R3)                                      0060
*           RETPTR2=RETPTR2&&RETPTR;/*                               */
         XR    R2,R4                                               0061
         ST    R2,RETPTR2(,R3)                                     0061
*           RETPTR=RETPTR&&RETPTR2; /*                               */
         XR    R4,R2                                               0062
         ST    R4,RETPTR(,R3)                                      0062
*           ATINCNTL=NO;            /* RESET SWITCH                  */
         NI    ATINCNTL(R3),B'01111111'                            0063
*         END;                      /*                               */
*     IF ECTATTN=YES THEN           /* IF TMP ATTN ENTRY      @Z30969*/
@RF00058 DS    0H                                                  0065
@RC00056 L     R5,@PC00001+4                                       0065
         TM    ECTSWS-ECT(R5),ECTATTN                              0065
         BNO   @RF00065                                            0065
*       DO;                         /* PROCESS                       */
*         ECTATTN=NO;               /* RESET ENTRY                   */
         NI    ECTSWS-ECT(R5),255-ECTATTN                          0067
*         ATINCNTL=YES;             /* SET CNTL BIT                  */
         L     R1,EXECDPTR(,COMONPTR)                              0068
         OI    ATINCNTL(R1),B'10000000'                            0068
*         CALL RETUPDT(GTPBLINE);   /* SET RETURN POINTER            */
         L     R2,@PC00001+12                                      0069
         LA    R3,GTPBLINE(,R2)                                    0069
         ST    R3,@AL00001                                         0069
         LA    R1,@AL00001                                         0069
         BAL   R14,RETUPDT                                         0069
*         LSDANEXT=ATACTSTR;        /* BRANCH TO ACTION START        */
         L     R4,LSDPTR(,COMONPTR)                                0070
         L     R5,EXECDPTR(,COMONPTR)                              0070
         L     R5,ATACTSTR(,R5)                                    0070
         ST    R5,LSDANEXT-LSD(,R4)                                0070
*         ERROR=E4;                 /* GET ANOTHER COMMAND           */
         MVC   ERROR(4),FW4                                        0071
*         GTPBLINE=LSDANEXT;        /* SAVE LSDANEXT         @ZA08099*/
         L     R6,@PC00001+12                                      0072
         ST    R5,GTPBLINE(,R6)                                    0072
*         LSDANEXT=LSDANEXT+LSDANEXT->LL;/* UPDATE LSD POINTER     0073
*                                                            @ZA08099*/
         LH    R7,LL(,R5)                                          0073
         ALR   R7,R5                                               0073
         ST    R7,LSDANEXT-LSD(,R4)                                0073
*         GOTO FASTCYCL;            /* START MAIN LOOP AGAIN @ZA08099*/
         B     FASTCYCL                                            0074
*       END;                        /*                               */
*                                                                  0076
*     /***************************************************************/
*     /*                                                             */
*     /* PROCESS RETURN CODES                                        */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0076
*     ERRSAV=ECTRTCD;               /* SAVE ECT RETURN CODE          */
@RF00065 L     R14,@PC00001+4                                      0076
         SLR   R15,R15                                             0076
         ICM   R15,7,ECTRTCD-ECT(R14)                              0076
         ST    R15,ERRSAV                                          0076
*     IF ECTRCDF=ABNDFLG THEN       /* IF CODE IS ABEND              */
         CLI   ECTRCDF-ECT(R14),X'80'                              0077
         BNE   @RF00077                                            0077
*       RCABEND=YES;                /* USE ABEND CONVERSION          */
         OI    RCABEND(COMONPTR),B'10000000'                       0078
*     IF ERRSAV^=F0 THEN            /* IF TSO CMD ENDED IN ERROR     */
@RF00077 L     R0,ERRSAV                                           0079
         LTR   R0,R0                                               0079
         BZ    @RF00079                                            0079
*       DO;                         /*                               */
*         IF ERRCMD=NO THEN         /* AN NO ERROR IN EFFECT         */
         L     R1,EXECDPTR(,COMONPTR)                              0081
         TM    ERRCMD(R1),B'01000000'                              0081
         BNZ   @RF00081                                            0081
*           CALL RCUPDT;            /* UPDATE LASTCC AND MAXCC       */
         BAL   R14,RCUPDT                                          0082
*         ELSE                      /* OTHERWISE                     */
*           DO;                     /* CHECK IF ERROR ALREADY IN CNTL*/
         B     @RC00081                                            0083
@RF00081 DS    0H                                                  0084
*             IF ERRRANGE=YES THEN  /* IF IT IS THEN SET ERROR CODE  */
         TM    ERRRANGE(COMONPTR),B'00000100'                      0084
         BNO   @RF00084                                            0084
*               IF GTPBLINE^=ERACTSTR THEN/* NOT FIRST ERROR CMD     */
         L     R2,@PC00001+12                                      0085
         L     R1,EXECDPTR(,COMONPTR)                              0085
         CLC   GTPBLINE(4,R2),ERACTSTR(R1)                         0085
         BE    @RF00085                                            0085
*                 DO;               /* MUST HAVE BEEN A TSO COMMAND  */
*                   BACKUP=YES;     /* BACK UP AND LIST LAST TSO     */
         OI    BACKUP(COMONPTR),B'00000001'                        0087
*                   ERROR=E908;     /*                               */
         MVC   ERROR(4),FW908                                      0088
*                 END;              /*                               */
*               ELSE                                               0090
*                 ;                 /* OTHERWISE NOTHING             */
@RF00085 DS    0H                                                  0091
*             ELSE                  /* OTHERWISE GIVE ERRACTION CNTL */
*               DO;                 /*                               */
         B     @RC00084                                            0091
@RF00084 DS    0H                                                  0092
*                 CALL RCUPDT;      /* PROCESS MAXCC AND LASTCC      */
         BAL   R14,RCUPDT                                          0092
*                 IF ERACTSTR^=F0 THEN/* IF A ERROR ACTION SPECIFIED */
         L     R2,EXECDPTR(,COMONPTR)                              0093
         L     R3,ERACTSTR(,R2)                                    0093
         LTR   R3,R3                                               0093
         BZ    @RF00093                                            0093
*                   DO;             /* GO TO ERRACTION               */
*                     ERINCNTL=YES; /* SET ERROR IN CONTROL          */
         OI    ERINCNTL(R2),B'00001000'                            0095
*                     CALL RETUPDT(GTPBLINE);/* UPDATE RETURN POINT  */
         L     R4,@PC00001+12                                      0096
         LA    R5,GTPBLINE(,R4)                                    0096
         ST    R5,@AL00001                                         0096
         LA    R1,@AL00001                                         0096
         BAL   R14,RETUPDT                                         0096
*                     LSDANEXT=ERACTSTR;/* UPDATE LSD NEXT CMD       */
         L     R1,LSDPTR(,COMONPTR)                                0097
         L     R2,EXECDPTR(,COMONPTR)                              0097
         L     R0,ERACTSTR(,R2)                                    0097
         ST    R0,LSDANEXT-LSD(,R1)                                0097
*                     ERROR=E4;     /* SET ERROR CODE                */
         MVC   ERROR(4),FW4                                        0098
*                     GOTO FASTEXIT;/* GET NEXT COMMAND              */
         L     R1,@PC00001+4
         SLR   R0,R0
         STCM  R0,7,ECTRTCD-ECT(R1)
         MVI   ECTRCDF-ECT(R1),X'00'
         B     FASTEXIT                                            0099
*                   END;            /*                               */
*                 ELSE              /* IF NO ERROR ACTION, SET UP    */
*                   IF INSLIST=NO THEN/* TSO NOT ALREADY LISTED      */
***00093 L     R15,@PC00001+4                                      0101
***      L     R1,ECTIOWA-ECT(,R15)                                0101
***      L     R1,IOSTELM(,R1)                                     0101
***      TM    INSLIST(R1),B'00000001'                             0101
***      BNZ   @RF00101                                            0101
*                     BACKUP=YES;   /* BACKUP TO LIST TSO            */
@RF00093 OI    BACKUP(COMONPTR),B'00000001'                        0102
*               END;                /*                               */
@RF00101 DS    0H                                                  0104
*             IF BACKUP=YES THEN    /* BACK UP AND LIST ?            */
@RC00084 TM    BACKUP(COMONPTR),B'00000001'                        0104
         BNO   @RF00104                                            0104
*               DO;                 /* YES -                         */
*                 LSDANEXT=GTPBLINE;/* BACK UP LSD COMMAND POINTER   */
         L     R1,LSDPTR(,COMONPTR)                                0106
         L     R2,@PC00001+12                                      0106
         L     R0,GTPBLINE(,R2)                                    0106
         ST    R0,LSDANEXT-LSD(,R1)                                0106
*                 GTPBLINE=LASTTSO; /* GET TSO COMMAND               */
         L     R1,EXECDPTR(,COMONPTR)                              0107
         L     R3,LASTTSO(,R1)                                     0107
         ST    R3,GTPBLINE(,R2)                                    0107
*                 BUFBASE=LASTTSO;  /* POINT BUFFER TO LAST TSO CMD  */
         ST    R3,BUFBASE(,COMONPTR)                               0108
*                 LISTONLY=YES;     /* LIST COMMAND ONLY             */
         OI    LISTONLY(COMONPTR),B'00100000'                      0109
*               END;                /*                               */
*           END;                    /*                               */
@RF00104 DS    0H                                                  0112
*         ECTRTCD=F0;               /* ZERO ECT RETURN CODE          */
@RC00081 L     R4,@PC00001+4                                       0112
         SLR   R5,R5                                               0112
         STCM  R5,7,ECTRTCD-ECT(R4)                                0112
*         ECTRCDF=''B;              /* ZERO ABEND FLAG IF ON         */
         MVI   ECTRCDF-ECT(R4),X'00'                               0113
*       END;                        /*                               */
*     ELSE                          /* IF RETURN CODE WAS ZERO       */
*       IF ERRRANGE=NO THEN         /* AND NOT IN ERROR RANGE        */
         B     @RC00079                                            0115
@RF00079 TM    ERRRANGE(COMONPTR),B'00000100'                      0115
         BNZ   @RF00115                                            0115
*         CALL RCUPDT;              /* UPDATE RETURN CODES           */
         BAL   R14,RCUPDT                                          0116
*     RCABEND=NO;                   /* INSURE ABEND CLEAR            */
@RF00115 DS    0H                                                  0117
@RC00079 NI    RCABEND(COMONPTR),B'01111111'                       0117
*                                                                  0118
*     /***************************************************************/
*     /*                                                             */
*     /* SYMBOLIC SUBSTITUTION INITIALIZATION                        */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0118
*     OLDLINE=GTPBLINE;             /* BASE FOR OLD LINE WITH PARMS  */
         L     R6,@PC00001+12                                      0118
         L     R14,GTPBLINE(,R6)                                   0118
         ST    R14,OLDLINE(,COMONPTR)                              0118
*     V2=F0;                        /* INIT TO ZERO                  */
         SLR   R15,R15                                             0119
         ST    R15,V2                                              0119
*     MYO2=O2-OO+F1;                /* SET OFFSET TO START OF SCAN   */
         L     R0,BUFBASE(,COMONPTR)                               0120
         LR    R1,R0                                               0120
         LH    R1,OO(,R1)                                          0120
         LR    R2,R0                                               0120
         LH    R0,O2(,R2)                                          0120
         SLR   R0,R1                                               0120
         AL    R0,FW1                                              0120
         ST    R0,MYO2(,COMONPTR)                                  0120
*     IF CORE=NO THEN               /* IF NO CORE HAS BEEN GOTTEN    */
         TM    CORE(COMONPTR),B'10000000'                          0121
         BNZ   @RF00121                                            0121
*       CALL GETMORE(EVAL(-OO));    /* GETMAIN FOR NEW BUFFER        */
         LCR   R1,R1                                               0122
         ST    R1,@AFTEMPS+8                                       0122
         LA    R1,@AFTEMPS+8                                       0122
         ST    R1,@AL00001                                         0122
         LA    R1,@AL00001                                         0122
         BAL   R14,GETMORE                                         0122
*     ELSE                          /* IF OTHER THAN FIRST LOOP      */
*       IF LL>NEWLL THEN            /* IF NEW LINE BIGGER THAN GOTTEN*/
         B     @RC00121                                            0123
@RF00121 L     R1,BUFBASE(,COMONPTR)                               0123
         L     R2,NEWBASE(,COMONPTR)                               0123
         LH    R3,NEWLL(,R2)                                       0123
         CH    R3,LL(,R1)                                          0123
         BNL   @RF00123                                            0123
*         DO;                       /* CORE, GET MORE CORE           */
*           DO;                     /* FREEMAIN LV(NEWLL*2)        0125
*                                      A(NEWBASE) SP(1) MF(E,FRELST) */
*             RESPECIFY                                            0126
*              (R1) RESTRICTED;                                    0126
*             FRELST02=NEWLL*2;     /* LENGTH                        */
         ALR   R3,R3                                               0127
         STCM  R3,7,FRELST02                                       0127
*             FRELST04=ADDR(NEWBASE);/* ADDR OF ADDR LIST            */
         LA    R0,NEWBASE(,COMONPTR)                               0128
         STCM  R0,7,FRELST04                                       0128
*             FRELST06=1;           /* SUBPOOL VALUE                 */
         MVI   FRELST06,X'01'                                      0129
*             R1=ADDR(FRELST);      /* REG1 POINTS TO LIST           */
         LA    R1,FRELST                                           0130
*             SVC(5);               /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0131
*             RESPECIFY                                            0132
*              (R1) UNRESTRICTED;                                  0132
*           END;                    /* FREEMAIN LV(NEWLL*2)        0133
*                                      A(NEWBASE) SP(1) MF(E,FRELST) */
*           CORE=NO;                /* YES WE HAVE NO CORE TODAY     */
         NI    CORE(COMONPTR),B'01111111'                          0134
*           CALL GETMORE(EVAL(-OO));/* MINIMUM CORE REQIRED          */
         L     R1,BUFBASE(,COMONPTR)                               0135
         LH    R2,OO(,R1)                                          0135
         LCR   R2,R2                                               0135
         ST    R2,@AFTEMPS+8                                       0135
         LA    R3,@AFTEMPS+8                                       0135
         ST    R3,@AL00001                                         0135
         LA    R1,@AL00001                                         0135
         BAL   R14,GETMORE                                         0135
*         END;                      /*                               */
*       ELSE                        /* OTHERWISE                     */
*         FREEAMT=NEWLL-(LL-OO);    /* SET FREEAMT                   */
         B     @RC00123                                            0137
@RF00123 L     R1,NEWBASE(,COMONPTR)                               0137
         L     R2,BUFBASE(,COMONPTR)                               0137
         LH    R3,LL(,R2)                                          0137
         SH    R3,OO(,R2)                                          0137
         LCR   R3,R3                                               0137
         AH    R3,NEWLL(,R1)                                       0137
         ST    R3,FREEAMT(,COMONPTR)                               0137
*                                                                  0138
*     /***************************************************************/
*     /*                                                             */
*     /* COPY INPUT BUFFER TO GOTTEN BUFFER                          */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0138
*     RFY                                                          0138
*      (R2,                                                        0138
*       R3,                                                        0138
*       R4,                                                        0138
*       R5) RSTD;                   /* RESTRICT REGS FOR MVCL        */
@RC00123 DS    0H                                                  0138
@RC00121 DS    0H                                                  0139
*     R2=(ADDR(NEWC(1)));           /* POINTER TO 'TO' BUFFER        */
         L     R6,NEWBASE(,COMONPTR)                               0139
         LA    R2,NEWC(,R6)                                        0139
*     R4=ADDR(C(OO+1));             /* POINTER TO 'FROM' BUFFER      */
         L     R7,BUFBASE(,COMONPTR)                               0140
         LH    R14,OO(,R7)                                         0140
         LA    R4,C(R14,R7)                                        0140
*     R3=NEWLL-F4;                  /* LENGTH OF 'TO' BUFFER         */
         LA    R15,4                                               0141
         LH    R3,NEWLL(,R6)                                       0141
         SLR   R3,R15                                              0141
*     R5=(LL-(F4+OO)) BLANKS;       /* LENGTH OF 'FROM' BUFFER AND 0142
*                                      PADDING CHARACTER FOR MVCL    */
         ALR   R14,R15                                             0142
         LCR   R14,R14                                             0142
         AH    R14,LL(,R7)                                        0142
         O     R14,HIBLANK                                         0142
         LR    R5,R14                                              0142
*     MVCL(R2,R4);                  /* MOVE TO NEW BUFFER            */
         MVCL  R2,R4                                               0143
*     RFY                                                          0144
*      (R2,                                                        0144
*       R3,                                                        0144
*       R4,                                                        0144
*       R5) UNRSTD;                 /* RELEASE REGISTERS             */
*     BUFBASE=NEWBASE;              /* POINT TO GOTTEN BUFFER        */
         L     R4,NEWBASE(,COMONPTR)                               0145
         ST    R4,BUFBASE(,COMONPTR)                               0145
*     OO=F0;                        /* ZERO OFFSET FIELD             */
         SLR   R5,R5                                               0146
         STH   R5,OO(,R4)                                          0146
*     NEWBASE=NEWBASE+LL;           /* POINT TO NEW BUFFER           */
         LH    R6,LL(,R4)                                          0147
         ALR   R4,R6                                               0147
         ST    R4,NEWBASE(,COMONPTR)                               0147
*     IF ERROR=E16 THEN             /* IF A GETMAIN ERROR OCCURRED   */
         L     R7,ERROR                                            0148
         C     R7,FW16                                             0148
         BE    @RT00148                                            0148
*       GO TO FASTEXIT;             /* PROCESS ERROR MSG AND QUIT    */
*     CORE=YES;                     /* CORE HAS BEEN GOTTEN          */
         OI    CORE(COMONPTR),B'10000000'                          0150
*     NEWLL=LL;                     /* INIT LENGTH                   */
         STH   R6,NEWLL(,R4)                                       0151
*     NEWOO=F0;                     /* INIT OFFSET                   */
         STH   R5,NEWOO(,R4)                                       0152
*     IF ERROR=F0 THEN              /* IF OK SO FAR                  */
         CR    R7,R5                                               0153
         BNE   @RF00153                                            0153
*       DO;                         /* LOCATE SYSDLM                 */
*         CALL LOCATE(SYSSCAN,LENGTH(SYSSCAN),F0);/* LOCATE SYSSCAN  */
         LA    R1,@AL00155                                         0155
         BAL   R14,LOCATE                                          0155
*         IF ERROR=F0 THEN          /* IF LOCATE SUCCESSFUL          */
         L     R14,ERROR                                           0156
         LTR   R14,R14                                             0156
         BNZ   @RF00156                                            0156
*           DO;                     /* PROCESS VALUE                 */
*             ANSPTR=ADDR(SVTDATA); /* POINT TO SCAN DATA            */
         L     R1,SNTELPTR(,COMONPTR)                              0158
         L     R2,SNTVLPTR(,R1)                                    0158
         LA    R3,SVTDATA(,R2)                                     0158
         ST    R3,ANSPTR(,COMONPTR)                                0158
*             CLEN=SVTLNG;          /* LENGTH OF SCAN NUMBER         */
         LH    R4,SVTLNG(,R2)                                      0159
         STH   R4,CLEN(,COMONPTR)                                  0159
*             CALL CONVERT;         /* CONVERT TO USABLE FORM        */
         BAL   R14,CONVERT                                         0160
*             SCANLIM=V2;           /* SET SCAN LIMIT                */
         L     R6,V2                                               0161
         ST    R6,SCANLIM                                          0161
*             IF ERROR^=F0 THEN     /* CONVERT OK                    */
         L     R7,ERROR                                            0162
         LTR   R7,R7                                               0162
         BZ    @RF00162                                            0162
*               ERROR=E960;         /* SET ERROR CODE                */
         MVC   ERROR(4),FW960                                      0163
*           END;                    /*                               */
*         ELSE                      /* OTHERWISE                     */
*           ERROR=E999;             /* SET ERROR CODE                */
         B     @RC00156                                            0165
@RF00156 MVC   ERROR(4),FW999                                      0165
*       END;                                                       0166
@RC00156 DS    0H                                                  0167
*     IF SYMLST=YES THEN            /* SYMBOL LIST SPECIFIED ?       */
@RF00153 L     R1,EXECDPTR(,COMONPTR)                              0167
         TM    SYMLST(R1),B'00010000'                              0167
         BNO   @RF00167                                            0167
*       CALL CMDLST;                /* LIST COMMAND                  */
         BAL   R14,CMDLST                                          0168
*     IF OPCODE=IFCODE THEN         /* ON IF CMD, REMOVE 'THEN'      */
@RF00167 L     R1,OLDLINE(,COMONPTR)                               0169
         CLI   OPCODE(R1),2                                        0169
         BNE   @RF00169                                            0169
*       FREEAMT=FREEAMT+F4;         /* LOGICALLY REMOVE THEN         */
         LA    R2,4                                                0170
         AL    R2,FREEAMT(,COMONPTR)                               0170
         ST    R2,FREEAMT(,COMONPTR)                               0170
*                                                                  0171
*     /***************************************************************/
*     /*                                                             */
*     /* SYMBOLIC SUBSTITUTION MAINLINE                              */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0171
*     DO Z=1 TO SCANLIM WHILE EXITA=NO&ERROR=F0;/* SUB TO SCANLIM OR
*                                      DONE                          */
@RF00169 LA    R3,1                                                0171
         B     @DE00171                                            0171
@DL00171 TM    EXITA(COMONPTR),B'10000000'                         0171
         BNZ   @DC00171                                            0171
         L     R4,ERROR                                            0171
         LTR   R4,R4                                               0171
         BNZ   @DC00171                                            0171
*       EXITA=YES;                  /* CAUSE END UNLESS INSERT HAS 0172
*                                      BEEN CALLED AT LEAST ONCE     */
         OI    EXITA(COMONPTR),B'10000000'                         0172
*       DO I=MYO2 TO LL-(F4+FREEAMT) WHILE ERROR=F0;/* SCAN RECORD   */
         L     R5,MYO2(,COMONPTR)                                  0173
         B     @DE00173                                            0173
@DL00173 L     R6,ERROR                                            0173
         LTR   R6,R6                                               0173
         BNZ   @DC00173                                            0173
*         IF C(I)=AMPSAND THEN      /* IF AN AMPERSAND               */
         L     R7,I(,COMONPTR)                                     0174
         L     R6,BUFBASE(,COMONPTR)                               0174
         ALR   R6,R7                                               0174
         CLI   C-1(R6),C'&&'                                       0174
         BNE   @RF00174                                            0174
*           DO;                     /*                               */
*             I=I+F1;               /* SKIP AMPERSAND                */
         AL    R7,FW1                                              0176
         ST    R7,I(,COMONPTR)                                     0176
*             V=I;                  /* SAVE START OF VARIABLE        */
         ST    R7,V                                                0177
*             CALL SCANWORD;        /* SCAN FOR VALID CHARS          */
         BAL   R14,SCANWORD                                        0178
*             IF C(I)=PERIOD THEN   /* IF A CONCAT '.', SQUASH IT OUT*/
         L     R1,I(,COMONPTR)                                     0179
         L     R2,BUFBASE(,COMONPTR)                               0179
         ALR   R2,R1                                               0179
         CLI   C-1(R2),C'.'                                        0179
         BNE   @RF00179                                            0179
*               CALL SQUASH;        /* GET RID OF PERIOD             */
         BAL   R14,SQUASH                                          0180
*             IF LEN=F0 THEN        /* IF ONLY AN AMPERSAND          */
@RF00179 L     R3,LEN(,COMONPTR)                                   0181
         LTR   R3,R3                                               0181
         BNZ   @RF00181                                            0181
*               IF C(I)^=AMPSAND THEN/* IF NOT DOUBLE AMPERSANDS     */
         L     R1,I(,COMONPTR)                                     0182
         L     R2,BUFBASE(,COMONPTR)                               0182
         ALR   R2,R1                                               0182
         CLI   C-1(R2),C'&&'                                       0182
         BE    @RF00182                                            0182
*                 ERROR=E900;       /* SET ERROR CODE                */
         MVC   ERROR(4),FW900                                      0183
*               ELSE                                               0184
*                 ;                 /* OTHERWISE NOTHING             */
@RF00182 DS    0H                                                  0185
*             ELSE                  /* OTHERWISE SUBSTITUTE VALUE    */
*               CALL INSERT;        /* CALL INSERT ROUTINE           */
         B     @RC00181                                            0185
@RF00181 BAL   R14,INSERT                                          0185
*           END;                                                   0186
*         ELSE                      /* IF NOT AN & CHECK FOR COMMENT */
*           IF I<LL-(FREEAMT+F4)&C(I:I+F1)=OPENCMT THEN/*            */
         B     @RC00174                                            0187
@RF00174 L     R3,I(,COMONPTR)                                     0187
         L     R2,BUFBASE(,COMONPTR)                               0187
         LA    R4,4                                                0187
         AL    R4,FREEAMT(,COMONPTR)                               0187
         LCR   R4,R4                                               0187
         AH    R4,LL(,R2)                                          0187
         CR    R3,R4                                               0187
         BNL   @RF00187                                            0187
         ALR   R2,R3                                               0187
         CLC   C-1(2,R2),SLSHASTR                                  0187
         BNE   @RF00187                                            0187
*             DO I=I+F3 TO LL-(F4+FREEAMT) WHILE C(I-F1:I)^=CLOSCMT;
         AL    R3,FW3                                              0188
         B     @DE00188                                            0188
@DL00188 L     R1,I(,COMONPTR)                                     0188
         L     R2,BUFBASE(,COMONPTR)                               0188
         ALR   R2,R1                                               0188
         CLC   C-2(2,R2),ASTRSLSH                                  0188
         BE    @DC00188                                            0188
*                                   /*                               */
*             END;                  /* FIND END OF COMMENT           */
         LA    R3,1                                                0189
         AL    R3,I(,COMONPTR)                                     0189
@DE00188 ST    R3,I(,COMONPTR)                                     0189
         L     R1,BUFBASE(,COMONPTR)                               0189
         LA    R2,4                                                0189
         AL    R2,FREEAMT(,COMONPTR)                               0189
         LCR   R2,R2                                               0189
         AH    R2,LL(,R1)                                          0189
         CR    R3,R2                                               0189
         BNH   @DL00188                                            0189
@DC00188 DS    0H                                                  0190
*       END;                                                       0190
@RF00187 DS    0H                                                  0190
@RC00174 LA    R5,1                                                0190
         AL    R5,I(,COMONPTR)                                     0190
@DE00173 ST    R5,I(,COMONPTR)                                     0190
         L     R1,BUFBASE(,COMONPTR)                               0190
         LA    R4,4                                                0190
         AL    R4,FREEAMT(,COMONPTR)                               0190
         LCR   R4,R4                                               0190
         AH    R4,LL(,R1)                                          0190
         CR    R5,R4                                               0190
         BNH   @DL00173                                            0190
@DC00173 DS    0H                                                  0191
*     END;                                                         0191
         LA    R3,1                                                0191
         AL    R3,Z                                                0191
@DE00171 ST    R3,Z                                                0191
         C     R3,SCANLIM                                          0191
         BNH   @DL00171                                            0191
@DC00171 DS    0H                                                  0192
*                                                                  0192
*     /***************************************************************/
*     /*                                                             */
*     /* ACCESS METHOD SERVICES COMPATABILITY CHECK - LASTCC AND     */
*     /* MAXCC ARE ALLOWED WITHOUT & ON THE LEFT OF THE OPERATOR     */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0192
*     IF OPCODE=IFCODE OPCODE=DOCODE THEN/* IF OR DO COMMANDS        */
         L     R4,OLDLINE(,COMONPTR)                               0192
         CLI   OPCODE(R4),2                                        0192
         BE    @RT00192                                            0192
         CLI   OPCODE(R4),7                                        0192
         BNE   @RF00192                                            0192
@RT00192 DS    0H                                                  0193
*       DO;                         /* MAKE CHECK                    */
*         I=MYO2;                   /* START OF SCAN                 */
         L     R6,MYO2(,COMONPTR)                                  0194
         ST    R6,I(,COMONPTR)                                     0194
*         CALL FINDWORD;            /* FIND A WORD                   */
         BAL   R14,FINDWORD                                        0195
*         IF SEPS=NO THEN           /* IF NOT ALL SEPARATORS         */
         TM    SEPS(COMONPTR),B'10000000'                          0196
         BNZ   @RF00196                                            0196
*           IF(LEN=LENGTH(LASTCC)&  /* LASTCC LENGTH                 */
*               C(BEGIN:BEGIN+LENGTH(LASTCC)-F1)=LASTCC) /* LASTCC   */
*               (LEN=LENGTH(MAXCC)& /* MAXCC LENGTH                  */
*               C(BEGIN:BEGIN+LENGTH(MAXCC)-F1)=MAXCC) THEN/* MAXCC  */
         CLC   LEN(4,COMONPTR),FW6                                 0197
         BNE   @GL00008                                            0197
         L     R1,BEGIN(,COMONPTR)                                 0197
         L     R2,BUFBASE(,COMONPTR)                               0197
         ALR   R2,R1                                               0197
         CLC   C-1(6,R2),$LASTCC                                   0197
         BE    @RT00197                                            0197
@GL00008 CLC   LEN(4,COMONPTR),FW5                                 0197
         BNE   @RF00197                                            0197
         L     R1,BEGIN(,COMONPTR)                                 0197
         L     R2,BUFBASE(,COMONPTR)                               0197
         ALR   R2,R1                                               0197
         CLC   C-1(5,R2),$MAXCC                                    0197
         BNE   @RF00197                                            0197
@RT00197 DS    0H                                                  0198
*             DO;                   /* PROCESS                       */
*               V=BEGIN;            /* BACK UP ONE FOR INSERT        */
         L     R4,BEGIN(,COMONPTR)                                 0199
         ST    R4,V                                                0199
*               NOAMPER=YES;        /* INDICATE NO AMPERSAND         */
         OI    NOAMPER(COMONPTR),B'00000010'                       0200
*               CALL INSERT;        /* INSERT VALUE HERE             */
         BAL   R14,INSERT                                          0201
*             END;                                                 0202
*         IF OPCODE=IFCODE THEN     /* ON OF STMT                    */
@RF00197 DS    0H                                                  0203
@RF00196 L     R1,OLDLINE(,COMONPTR)                               0203
         CLI   OPCODE(R1),2                                        0203
         BNE   @RF00203                                            0203
*           DO;                     /* RE-INSERT 'THEN'              */
*             V=LL-(F3+FREEAMT);    /* SET START OF AREA             */
         L     R1,BUFBASE(,COMONPTR)                               0205
         LA    R0,3                                                0205
         AL    R0,FREEAMT(,COMONPTR)                               0205
         LCR   R0,R0                                               0205
         AH    R0,LL(,R1)                                          0205
         ST    R0,V                                                0205
*             I=V;                  /* SET END AREA                  */
         ST    R0,I(,COMONPTR)                                     0206
*             LEN=F0;               /* SET ZERO LENGTH TO REMOVE     */
         SLR   R1,R1                                               0207
         ST    R1,LEN(,COMONPTR)                                   0207
*             NOAMPER=YES;          /* NO AMPERSAND ON VARIABLE      */
*             INSTHEN=YES;          /* INSERT THEN TO INSERT         */
         OI    NOAMPER(COMONPTR),B'00000011'                       0209
*             ERRSAV=ERROR;         /* SAVE ERROR CODE               */
         L     R0,ERROR                                            0210
         ST    R0,ERRSAV                                           0210
*             ERROR=F0;             /* CLEAR ERROR IF ANY            */
         ST    R1,ERROR                                            0211
*             CALL INSERT;          /* INSERT THEN                   */
         BAL   R14,INSERT                                          0212
*             ERROR=ERRSAV;         /* RESTORE ERROR CONDITION       */
         L     R2,ERRSAV                                           0213
         ST    R2,ERROR                                            0213
*           END;                    /*                               */
*       END;                        /*                               */
@RF00203 DS    0H                                                  0216
*                                                                  0216
*     /***************************************************************/
*     /*                                                             */
*     /* CONTROL COMMAND LIST                                        */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0216
*     EXECDPTR=LSDEXEC;             /* RESET EXEC DATA POINTER       */
@RF00192 L     R1,LSDPTR(,COMONPTR)                                0216
         L     R1,LSDEXEC-LSD(,R1)                                 0216
         ST    R1,EXECDPTR(,COMONPTR)                              0216
*     IF ERROR=F0 THEN              /* IF OK SO FAR                  */
         L     R2,ERROR                                            0217
         LTR   R2,R2                                               0217
         BNZ   @RF00217                                            0217
*       DO I=1 TO LL-(F4+FREEAMT);  /* DO UNTIL END OF BUFFER        */
         LA    R3,1                                                0218
         B     @DE00218                                            0218
@DL00218 DS    0H                                                  0219
*         IF I<LL-(F4+FREEAMT)&     /* NOT BEFORE END OF BUFFER      */
*             (C(I:I+F1)=DAMPSAND   /* DOUBLE AMPERSAND              */
*             C(I:I+F1)=FDMPSAND) THEN/* FOLDED AMPERSANDS           */
         L     R4,I(,COMONPTR)                                     0219
         L     R5,BUFBASE(,COMONPTR)                               0219
         LA    R6,4                                                0219
         AL    R6,FREEAMT(,COMONPTR)                               0219
         LCR   R6,R6                                               0219
         AH    R6,LL(,R5)                                          0219
         CR    R4,R6                                               0219
         BNL   @RF00219                                            0219
         ALR   R4,R5                                               0219
         CLC   C-1(2,R4),$DBLAMPR                                  0219
         BE    @RT00219                                            0219
         L     R4,I(,COMONPTR)                                     0219
         ALR   R5,R4                                               0219
         CLC   C-1(2,R5),$HEX1010                                  0219
         BNE   @RF00219                                            0219
@RT00219 DS    0H                                                  0220
         TM    LCLFLAGS,$#NRSTR     PROCESSING &NRSTR?          ZP60014
         BO    @RF00219             YES, SUPPRESS SQUASH        ZP60014
*           CALL SQUASH;            /* SQUASH OUT THE EXTRA AMPERSAND*/
         BAL   R14,SQUASH                                          0220
*         IF OPCODE^=IFCODE&OPCODE^=DOCODE&/* NOT IF AND NOT DO      */
*             OPCODE^=SETCODE THEN  /* AND NOT SET                   */
@RF00219 L     R5,OLDLINE(,COMONPTR)                               0221
         CLI   OPCODE(R5),2                                        0221
         BE    @RF00221                                            0221
         CLI   OPCODE(R5),7                                        0221
         BE    @RF00221                                            0221
         CLI   OPCODE(R5),13                                       0221
         BE    @RF00221                                            0221
*           DO;                     /* FOLD TO UPPER CASE            */
*             FOLD=NO;              /* RESET FOLD INDICATOR          */
         NI    FOLD(COMONPTR),B'11101111'                          0223
*             TR(C(I),TRTABUP);     /* FOLD TO NORMAL CHARACTERS     */
         L     R1,I(,COMONPTR)                                     0224
         L     R2,BUFBASE(,COMONPTR)                               0224
         ALR   R2,R1                                               0224
         TR    C-1(1,R2),TRTABUP                                   0224
*           END;                    /*                               */
*       END;                                                       0226
@RF00221 LA    R3,1                                                0226
         AL    R3,I(,COMONPTR)                                     0226
@DE00218 ST    R3,I(,COMONPTR)                                     0226
         L     R1,BUFBASE(,COMONPTR)                               0226
         LA    R2,4                                                0226
         AL    R2,FREEAMT(,COMONPTR)                               0226
         LCR   R2,R2                                               0226
         AH    R2,LL(,R1)                                          0226
         CR    R3,R2                                               0226
         BNH   @DL00218                                            0226
         NI    LCLFLAGS,255-$#NRSTR RESET FUNCTION ACTIVE FLAG  ZP60014
*     IF LISTONLY=YES               /* BACKUP LIST OF TSO COMMAND    */
*         (ERROR>E16&(ERACTSTR=F0 ERRRANGE=YES)) /* A SEVERE ERROR   */
*         (CONLST=YES&(OPCODE^=F0 ERROR>E16)) THEN/* CONLIST         */
@RF00217 TM    LISTONLY(COMONPTR),B'00100000'                      0227
         BO    @RT00227                                            0227
         L     R3,ERROR                                            0227
         C     R3,FW16                                             0227
         BNH   @GL00015                                            0227
         L     R1,EXECDPTR(,COMONPTR)                              0227
         L     R2,ERACTSTR(,R1)                                    0227
         LTR   R2,R2                                               0227
         BZ    @RT00227                                            0227
         TM    ERRRANGE(COMONPTR),B'00000100'                      0227
         BO    @RT00227                                            0227
@GL00015 L     R1,EXECDPTR(,COMONPTR)                              0227
         TM    CONLST(R1),B'10000000'                              0227
         BNO   @RF00227                                            0227
         L     R1,OLDLINE(,COMONPTR)                               0227
         CLI   OPCODE(R1),0                                        0227
         BNE   @RT00227                                            0227
         L     R2,ERROR                                            0227
         C     R2,FW16                                             0227
         BNH   @RF00227                                            0227
@RT00227 DS    0H                                                  0228
*       DO;                                                        0228
*         LISTED=YES;               /* COMMAND ALREADY LISTED        */
         OI    LISTED(COMONPTR),B'00001000'                        0229
*         CALL CMDLST;              /* LIST COMMAND                  */
         BAL   R14,CMDLST                                          0230
*         IF LISTONLY=YES&ERROR=F0 THEN/* IF LISTED OK               */
         TM    LISTONLY(COMONPTR),B'00100000'                      0231
         BNO   @RF00231                                            0231
         L     R3,ERROR                                            0231
         LTR   R3,R3                                               0231
         BNZ   @RF00231                                            0231
*           ERROR=E4;               /* GET ANOTHER LINE              */
         MVC   ERROR(4),FW4                                        0232
*       END;                                                       0233
@RF00231 DS    0H                                                  0234
*                                                                  0234
*     /***************************************************************/
*     /*                                                             */
*     /* FUNCTION CALL AND NEXT COMMAND UPDATE                       */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0234
*     IF ERROR=F0 THEN              /* IF OK UP TO NOW               */
@RF00227 L     R4,ERROR                                            0234
         LTR   R4,R4                                               0234
         BNZ   @RF00234                                            0234
*       DO;                         /* CALL FUNCTION                 */
*         RFY                                                      0236
*          (R15) RSTD;              /* RESTRICT REG 15               */
*         CALL IKJCT436(UPTX,ECTX,ECB,GTPB,PARM433);/* CALL FUNCTION */
         L     R14,@PC00001                                        0237
         ST    R14,@AL00001                                        0237
         L     R14,@PC00001+4                                      0237
         ST    R14,@AL00001+4                                      0237
         L     R14,@PC00001+8                                      0237
         ST    R14,@AL00001+8                                      0237
         L     R14,@PC00001+12                                     0237
         ST    R14,@AL00001+12                                     0237
         L     R14,@PC00001+16                                     0237
         ST    R14,@AL00001+16                                     0237
         L     R15,IKJCT436                                        0237
         LA    R1,@AL00001                                         0237
         BALR  R14,R15                                             0237
*         ERROR=R15;                /* SET ERROR CODE                */
         ST    R15,ERROR                                           0238
*         RFY                                                      0239
*          (R15) UNRSTD;            /* RELEASE REG 15                */
*       END;                        /*                               */
*     ERCOM=F0;                     /* CLEAR COMMON ERROR FOR MSGOUT */
@RF00234 SLR   R0,R0                                               0241
         ST    R0,ERCOM(,COMONPTR)                                 0241
*     IF PARMDEFR=YES THEN          /* IF OK TO CHANGE STATUS        */
         L     R1,@PC00001+16                                      0242
         TM    PARMDEFR(R1),B'10000000'                            0242
         BNO   @RF00242                                            0242
*       GEN REFS(DEFERNO)(STAX MF=(E,DEFERNO));/* ALLOW ATTNS        */
         STAX MF=(E,DEFERNO)
*     IF ECBCBIT=YES THEN           /* IF ATTN POSTED                */
@RF00242 L     R2,@PC00001+8                                       0244
         TM    ECBCBIT(R2),B'01000000'                             0244
         BNO   @RF00244                                            0244
*       ERROR=E8;                   /* SET ATTN CODE                 */
         MVC   ERROR(4),FW8                                        0245
*     IF PARMDEFR=YES THEN          /* IF OK TO CHANGE STATUS        */
@RF00244 L     R3,@PC00001+16                                      0246
         TM    PARMDEFR(R3),B'10000000'                            0246
         BNO   @RF00246                                            0246
*       GEN REFS(DEFERYES)(STAX MF=(E,DEFERYES));/* NO ATTNS AGAIN   */
         STAX MF=(E,DEFERYES)
*     IF ERROR^=E4 THEN             /* IF NOT A CONTINU RETURN CODE  */
@RF00246 CLC   ERROR(4),FW4                                        0248
         BNE   LBL007EC
         TM    CONTINU(COMONPTR),B'00001110'
         BZ    @RF00248                                            0248
*       CONTINU=NO;                 /* DON''T CONTINUE               */
LBL007EC NI    CONTINU(COMONPTR),B'10111111'                       0249
*     IF CONTINU=YES THEN           /* IF OK TO CONTINUE             */
@RF00248 TM    CONTINU(COMONPTR),B'01000000'                       0250
         BNO   @RF00250                                            0250
*       DO;                         /* CHECK FOR END OF CLIST        */
*         ECDACBLK=LSDADATA;        /* POINT TO FIRST BLOCK          */
         L     R1,LSDPTR(,COMONPTR)                                0252
         L     R1,LSDADATA-LSD(,R1)                                0252
         ST    R1,ECDACBLK                                         0252
*         DO WHILE ECDACBK2^=F0&CONTINU=YES;/* FIND NEXT BLOCK       */
         B     @DE00253                                            0253
@DL00253 DS    0H                                                  0254
*           IF CURID=COMPRID&       /* IF IDS MATCH                  */
*               CURADDR>=ECDACBK2+COMPRLNG THEN/* AND AT END         */
         L     R2,LSDPTR(,COMONPTR)                                0254
         L     R3,ECDACBLK                                         0254
         CLC   CURID(1,R2),COMPRID(R3)                             0254
         BNE   @RF00254                                            0254
         SLR   R4,R4                                               0254
         ICM   R4,7,ECDACBK2                                       0254
         AL    R4,COMPRLNG(,R3)                                    0254
         SLR   R5,R5                                               0254
         ICM   R5,7,CURADDR(R2)                                    0254
         CR    R4,R5                                               0254
         BH    @RF00254                                            0254
*             IF COMPRNXT=F0 THEN   /* IF NO MORE BLOCKS     @ZA08099*/
         SLR   R2,R2                                               0255
         ICM   R2,7,COMPRNXT(R3)                                   0255
         LTR   R2,R2                                               0255
         BNZ   @RF00255                                            0255
*               DO;                 /*                       @ZA08099*/
*                 EOCLIST=YES;      /* END OF THIS CLIST     @ZA08099*/
         L     R3,@PC00001+16                                      0257
         OI    EOCLIST(R3),B'01000000'                             0257
*                 CONTINU=NO;       /* DISSALLOW CONTINUE            */
         NI    CONTINU(COMONPTR),B'10111111'                       0258
*               END;                /*                       @ZA08099*/
*             ELSE                  /* OTHERWISE                     */
*               DO;                 /* GO TO NEXT BLOCK              */
         B     @RC00255                                            0260
@RF00255 DS    0H                                                  0261
*                 CURADDR=COMPRNXT+LENGTH(COMPROC);/* UPDATE LSD     */
         L     R4,LSDPTR(,COMONPTR)                                0261
         L     R5,ECDACBLK                                         0261
         LA    R6,12                                               0261
         SLR   R7,R7                                               0261
         ICM   R7,7,COMPRNXT(R5)                                   0261
         ALR   R6,R7                                               0261
         STCM  R6,7,CURADDR(R4)                                    0261
*                 CURID=COMPRID+1;  /* INCREMENT BLOCK ID            */
         LA    R14,1                                               0262
         SLR   R15,R15                                             0262
         IC    R15,COMPRID(,R5)                                    0262
         ALR   R14,R15                                             0262
         STC   R14,CURID(,R4)                                      0262
*                 ECDACBLK=F0;      /* END SCAN                      */
         SLR   R0,R0                                               0263
         ST    R0,ECDACBLK                                         0263
*               END;                /*                               */
*           ELSE                    /* OTHERWISE                     */
*             ECDACBLK=COMPRPTR;    /* CHECK NEXT BLOCK              */
         B     @RC00254                                            0265
@RF00254 L     R1,ECDACBLK                                         0265
         L     R2,COMPRPTR(,R1)                                    0265
         ST    R2,ECDACBLK                                         0265
*         END;                      /*                               */
@RC00254 DS    0H                                                  0266
@DE00253 SLR   R3,R3                                               0266
         ICM   R3,7,ECDACBK2                                       0266
         LTR   R3,R3                                               0266
         BZ    @DC00253                                            0266
         TM    CONTINU(COMONPTR),B'01000000'                       0266
         BO    @DL00253                                            0266
@DC00253 DS    0H                                                  0267
*         IF CONTINU=YES THEN       /* IF CONTINUING                 */
         TM    CONTINU(COMONPTR),B'01000000'                       0267
         BNO   @RF00267                                            0267
*           DO;                     /* UPDATE POINTERS               */
*             GTPBLINE=LSDANEXT;    /* SAVE LSDANEXT                 */
         L     R4,@PC00001+12                                      0269
         L     R5,LSDPTR(,COMONPTR)                                0269
         L     R6,LSDANEXT-LSD(,R5)                                0269
         ST    R6,GTPBLINE(,R4)                                    0269
*             LSDANEXT=LSDANEXT+LSDANEXT->LL;/* UPDATE LSD POINTER   */
         LH    R7,LL(,R6)                                          0270
         ALR   R7,R6                                               0270
         ST    R7,LSDANEXT-LSD(,R5)                                0270
*           END;                    /*                               */
*       END;                        /*                               */
@RF00267 DS    0H                                                  0273
*FASTCYCL:                                                         0273
*   END;                            /*                       @ZA08099*/
@RF00250 DS    0H                                                  0273
FASTCYCL DS    0H                                                  0273
@DE00041 TM    CONTINU(COMONPTR),B'01000000'                       0273
         BO    @DL00041                                            0273
*                                                                  0274
*   /*****************************************************************/
*   /*                                                               */
*   /* RETURN CODE UPDATE IF ERROR AND STACK FLUSH IF NEEDED         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0274
*FASTEXIT:                                                         0274
*   IF ERROR>E8 THEN                /* IF AN ERROR OCCURRED          */
FASTEXIT L     R14,ERROR                                           0274
         C     R14,FW8                                             0274
         BNH   @RF00274                                            0274
*     DO;                           /* PROCESS ERROR CONDITION       */
*       IF ERROR=E16 THEN           /* IF A GETMAIN FAIL             */
         C     R14,FW16                                            0276
         BE    LBL008F2
*       IF ERROR=E999
         C     R14,FW999                                           0276
         BE    LBL008F2
         L     R1,EXECDPTR(,COMONPTR)
         CLC   STAECNT(2,R1),HW1
         BNH   @RF00276                                            0276
*         QUIT=YES;                 /* FLUSH STACK                   */
LBL008F2 OI    QUIT(COMONPTR),B'00100000'                          0277
*       ELSE                        /* OTHERWISE                     */
*         QUIT=NO;                  /* DONT FLUSH -- YET             */
         B     @RC00276                                            0278
@RF00276 NI    QUIT(COMONPTR),B'11011111'                          0278
*       ERRSAV=ERROR;               /* SAVE ERROR CODE               */
@RC00276 L     R3,ERROR                                            0279
         ST    R3,ERRSAV                                           0279
*       ERROR=F0;                   /* SET ERROR TO ZERO TO          */
         SLR   R4,R4                                               0280
         ST    R4,ERROR                                            0280
*       CALL RCUPDT;                /* SET RETURN CODE INDICATORS    */
         BAL   R14,RCUPDT                                          0281
*       IF ERRCMD=YES THEN          /* ID ERROR COMMAND IN EFFECT    */
         L     R5,EXECDPTR(,COMONPTR)                              0282
         TM    ERRCMD(R5),B'01000000'                              0282
         BNO   @RF00282                                            0282
         CLC   STAECNT(2,R5),HW1
         BH    @RF00282
*         IF ERRRANGE=YES THEN      /* IF IN AN ERROR RANGE          */
         TM    ERRRANGE(COMONPTR),B'00000100'                      0283
         BNO   @RF00283                                            0283
*           DO;                     /*                               */
*             IF ERRSAV^=E908 THEN  /* NOT ALREADY A RECURSE MSG     */
         CLC   ERRSAV(4),FW908                                     0285
         BE    @RF00285                                            0285
*               RECURSE=YES;        /* SET RECURSIVE ERROR BIT       */
         OI    RECURSE(COMONPTR),B'00100000'                       0286
*             QUIT=YES;             /* FLUSH STACK                   */
@RF00285 OI    QUIT(COMONPTR),B'00100000'                          0287
*           END;                    /*                               */
*         ELSE                      /* IF NOT IN ERRANGE             */
*           IF ERACTSTR^=F0 THEN    /* IF NO ERACTSTR                */
         B     @RC00283                                            0289
@RF00283 L     R6,EXECDPTR(,COMONPTR)                              0289
         L     R7,ERACTSTR(,R6)                                    0289
         LTR   R7,R7                                               0289
         BZ    @RF00289                                            0289
*             DO;                   /*                               */
*               ERINCNTL=YES;       /* SET ERROR IN CONTROL          */
         OI    ERINCNTL(R6),B'00001000'                            0291
*               CALL RETUPDT(LSDANEXT);/* UPDATE RETURN POINT        */
         L     R1,LSDPTR(,COMONPTR)                                0292
         LA    R2,LSDANEXT-LSD(,R1)                                0292
         ST    R2,@AL00001                                         0292
         LA    R1,@AL00001                                         0292
         BAL   R14,RETUPDT                                         0292
*               LSDANEXT=ERACTSTR;  /* SET GOTO ADDR                 */
         L     R1,LSDPTR(,COMONPTR)                                0293
         L     R2,EXECDPTR(,COMONPTR)                              0293
         L     R0,ERACTSTR(,R2)                                    0293
         ST    R0,LSDANEXT-LSD(,R1)                                0293
*             END;                  /*                               */
*           ELSE                    /* OTHERWISE                     */
*             IF LISTED=NO THEN     /* IF COMMAND NOT LISTED         */
         B     @RC00289                                            0295
@RF00289 TM    LISTED(COMONPTR),B'00001000'                        0295
         BNZ   @RF00295                                            0295
*               DO;                 /* LIST COMMAND IN ERROR         */
*                 LISTED=YES;       /* SAY WE HAVE LISTED COMMAND    */
         OI    LISTED(COMONPTR),B'00001000'                        0297
*                 CALL CMDLST;      /* LIST COMMAND                  */
         BAL   R14,CMDLST                                          0298
*               END;                /*                               */
*             ELSE                                                 0300
*               ;                   /* NOTHING                       */
@RF00295 DS    0H                                                  0301
*       ELSE                        /* IF NO ERRCMD                  */
*         QUIT=YES;                 /* FLUSH STACK                   */
         B     @RC00282                                            0301
@RF00282 OI    QUIT(COMONPTR),B'00100000'                          0301
*       IF QUIT=YES LISTED=YES THEN /* FLUSH STACK ?                 */
@RC00282 TM    QUIT(COMONPTR),B'00100000'                          0302
         BO    @RT00302                                            0302
         TM    LISTED(COMONPTR),B'00001000'                        0302
         BNO   @RF00302                                            0302
@RT00302 DS    0H                                                  0303
*         DO;                       /* IF SO PRINT ERROR MSG         */
*           IF LISTED=NO THEN       /* IF NOT ALREADY LISTED         */
         TM    LISTED(COMONPTR),B'00001000'                        0304
         BNZ   @RF00304                                            0304
*             CALL CMDLST;          /* LIST COMMAND                  */
         BAL   R14,CMDLST                                          0305
*           IF QUIT=YES THEN        /* GOING TO FLUSH ?              */
@RF00304 TM    QUIT(COMONPTR),B'00100000'                          0306
         BNO   @RF00306                                            0306
*             NOMSG=NO;             /* ALLOW MESSAGE TO GO OUT       */
         L     R1,EXECDPTR(,COMONPTR)                              0307
         NI    NOMSG(R1),B'11111101'                               0307
*           CALL MSGOUT;            /* PUT OUT SELECTED MESSAGE      */
@RF00306 BAL   R14,MSGOUT                                          0308
*           IF RECURSE=YES THEN     /* A RECURSIVE ERROR ?           */
         TM    RECURSE(COMONPTR),B'00100000'                       0309
         BNO   @RF00309                                            0309
*             DO;                   /* BUILD CORRECT MESSAGE         */
*               ERRSAV=E908;        /* SET RECURSIVE ERROR           */
         MVC   ERRSAV(4),FW908                                     0311
*               CALL MSGOUT;        /* PUT OUT RECURSIVE MESSAGE     */
         BAL   R14,MSGOUT                                          0312
*             END;                  /*                               */
*           IF QUIT=YES THEN        /* FLUSH THE STACK               */
@RF00309 TM    QUIT(COMONPTR),B'00100000'                          0314
         BNO   @RF00314                                            0314
*             DO;                   /* FLUSH IT                      */
*               ECTRTCD=ERRSAV;     /* SET ECT RETURN CODE FIELD     */
         L     R2,@PC00001+4                                       0316
         MVC   ECTRTCD-ECT(3,R2),ERRSAV+1                          0316
*               ECTRCDF=''B;        /* CLEAR ABEND FLAG              */
         MVI   ECTRCDF-ECT(R2),X'00'                               0317
*               NOFLUSH=NO;         /* TURN OFF NOFLUSH OPTION       */
*               CMAIN=NO;           /* TURN OFF MAIN OPTION          */
         L     R1,EXECDPTR(,COMONPTR)                              0319
         NI    NOFLUSH(R1),B'11011011'                             0319
*               IOPL(4)=ADDR(STFLUSH);/* PUTLINE PARM BLOCK          */
         LA    R2,STFLUSH                                          0320
         ST    R2,IOPL+12(,COMONPTR)                               0320
*               RFY                                                0321
*                (R1) RSTD;         /* RESTRICT REG ONE              */
*               R1=ADDR(IOPL);      /* LOAD PARM ADDRESS             */
         LR    R1,COMONPTR                                         0322
*               CALL IKJEFT30;      /* FLUSH STACK                   */
         L     R15,IKJEFT30                                        0323
         BALR  R14,R15                                             0323
*               RFY                                                0324
*                (R1) UNRSTD;       /* RELEASE REG ONE               */
*             END;                  /*                               */
*         END;                      /*                               */
@RF00314 DS    0H                                                  0327
*       ERROR=E4;                   /* SET OK TO GET ANOTHER COMMAND */
@RF00302 MVC   ERROR(4),FW4                                        0327
*     END;                          /*                               */
*                                                                  0329
*   /*****************************************************************/
*   /*                                                               */
*   /* FREE BUFFERS AS REQUIRED                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0329
*   IF CORE=YES THEN                /* NO GETMAIN ERROR YET          */
@RF00274 TM    CORE(COMONPTR),B'10000000'                          0329
         BNO   @RF00329                                            0329
*     DO;                           /* FREE PARTS OF OR ALL BUFFERS  */
*       IF ERROR=F0 THEN            /* IF ALL OK                     */
         L     R0,ERROR                                            0331
         LTR   R0,R0                                               0331
         BNZ   @RF00331                                            0331
*         DO;                       /* FREEMAIN EXCESS PART OF BUFFER*/
*           FREEADDR(1)=(BUFBASE+LL-FREEAMT+7)/8*8;/* CALC FREE ADDR */
         L     R1,BUFBASE(,COMONPTR)                               0333
         LH    R2,LL(,R1)                                          0333
         LR    R4,R1                                               0333
         ALR   R4,R2                                               0333
         L     R3,FREEAMT(,COMONPTR)                               0333
         SLR   R4,R3                                               0333
         AL    R4,FW7                                              0333
         SRDA  R4,32                                               0333
         D     R4,FW8                                              0333
         SLA   R5,3                                                0333
         ST    R5,FREEADDR                                         0333
*           FREELEN(1)=LL-(FREEADDR(1)-BUFBASE);/* CALC FREE LENGTH  */
         SLR   R5,R1                                               0334
         LCR   R5,R5                                               0334
         ALR   R5,R2                                               0334
         ST    R5,FREELEN                                          0334
*           LL=LL-FREEAMT;          /* ADJUST BUFFER LL              */
         SLR   R2,R3                                               0335
         STH   R2,LL(,R1)                                          0335
*           GTPBLINE=BUFBASE;       /* PUT LINE ADDR IN CALLERS AREA */
         L     R4,@PC00001+12                                      0336
         ST    R1,GTPBLINE(,R4)                                    0336
*         END;                      /*                               */
*       ELSE                        /* OTHERWISE FREE OBTAINED BUFFER*/
*         DO;                       /*                               */
         B     @RC00331                                            0338
@RF00331 DS    0H                                                  0339
*           FREEADDR(1)=BUFBASE;    /* BUFFER ADDRESS                */
         L     R5,BUFBASE(,COMONPTR)                               0339
         ST    R5,FREEADDR                                         0339
*           FREELEN(1)=LL;          /* BUFFER LENGTH                 */
         LH    R6,LL(,R5)                                          0340
         ST    R6,FREELEN                                          0340
*         END;                      /*                               */
*       FREEADDR(2)=NEWBASE;        /* ALTERNATE BUFFER ALWAYS FREED */
@RC00331 L     R7,NEWBASE(,COMONPTR)                               0342
         ST    R7,FREEADDR+4                                       0342
*       FREELEN(2)=NEWLL FREELAST;  /* LENGTH OF ALTERNATE BUFFER    */
         LH    R14,NEWLL(,R7)                                      0343
         O     R14,HIGHBIT                                         0343
         ST    R14,FREELEN+4                                       0343
*       DO;                         /* FREEMAIN (L) LA(FREELEN(1)) 0344
*                                      A(FREEADDR(1)) SP(F1)       0344
*                                      MF(E,FRELST)                  */
*         RESPECIFY                                                0345
*          (R1) RESTRICTED;                                        0345
*         FRELST05='80'X;           /* SET TYPE                      */
         MVI   FRELST05,X'80'                                      0346
*         FRELST02=ADDR(FREELEN(1));/* ADDR OF LENGTH LIST           */
         LA    R0,FREELEN                                          0347
         STCM  R0,7,FRELST02                                       0347
*         FRELST04=ADDR(FREEADDR(1));/* ADDR OF ADDR LIST            */
         LA    R0,FREEADDR                                         0348
         STCM  R0,7,FRELST04                                       0348
*         FRELST06=F1;              /* SUBPOOL VALUE                 */
         MVI   FRELST06,X'01'                                      0349
*         R1=ADDR(FRELST);          /* REG1 POINTS TO LIST           */
         LA    R1,FRELST                                           0350
*         SVC(5);                   /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0351
*         RESPECIFY                                                0352
*          (R1) UNRESTRICTED;                                      0352
*       END;                        /* FREEMAIN (L) LA(FREELEN(1)) 0353
*                                      A(FREEADDR(1)) SP(F1)       0353
*                                      MF(E,FRELST)                  */
*     END;                                                         0354
*   RETURN CODE(ERROR);             /* RETURN TO GETLINE             */
@RF00329 L     R2,ERROR                                            0355
         L     R13,4(,R13)                                         0355
         L     R0,@SIZDATD                                         0355
         LR    R1,R9                                               0355
         IKJEXEC  R,LV=(0),A=(1)
         LR    R15,R2                                              0355
         L     R14,12(,R13)                                        0355
         LM    R0,R12,20(R13)                                      0355
         BR    R14                                                 0355
*                                                               ZP60014
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* RETURN NUMERIC VALUE                                 ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*                                                               ZP60014
@EP0GET# LA    R3,FW12              SET NUMERIC FUNCTION CODE   ZP60014
         B     @EP00356             BRANCH TO COMMON LOGIC      ZP60014
*                                                               ZP60014
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* UPDATE WITH IMPLICIT CREATE ENTRY                    ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*                                                               ZP60014
@EP0INIT LA    R3,FW8               SET LOCATE FUNCTION CODE    ZP60014
         B     @EP00356             BRANCH TO COMMON LOGIC      ZP60014
*                                                                  0356
*   /*****************************************************************/
*   /*                                                               */
*   /* UPDATE ONLY ENTRY                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0356
*IKJUPDT:                                                          0356
*   ENTRY(UPTX,ECTX,ECB,UPLIST);    /*                               */
@EP0UPDT LA    R3,FW0               SET LOCATE FUNCTION CODE    ZP60014
@EP00356 MVC   @PC00001(12),0(R1)                                  0356
         MVC   @PC00001+20(4),12(R1)                               0356
*   SWS=''B;                        /* CLEAR MOST SWITCHES           */
         XC    SWS(5,COMONPTR),SWS(COMONPTR)                       0357
*   CALL INTSETUP;                  /* INIT VARIABLES                */
         BAL   R14,INTSETUP                                        0358
*   SYSUPDTE=YES;                   /* INDICATE SYSTEM UPDATE        */
         OI    SYSUPDTE(COMONPTR),B'00000100'                      0359
*   CALL LOCATE(LCVAL,LCVALLEN,F0); /* LOCATE VARIABLE               */
         L     R2,@PC00001+20       (PRESERVE R3 - ZP60014)        0360
         L     R1,LCVALPTR(,R2)     (PRESERVE R3 - ZP60014)        0360
         ST    R1,@AL00001                                         0360
         LA    R2,LCVALLEN(,R2)     (PRESERVE R3 - ZP60014)        0360
         ST    R2,@AL00001+4                                       0360
         ST    R3,@AL00001+8        SET LOCATE FUNCTION CODE       0360
         LA    R1,@AL00001                                         0360
         BAL   R14,LOCATE                                          0360
*   IF ERROR=F0 THEN                /* IF FOUND                      */
         L     R4,ERROR                                            0361
         LTR   R4,R4                                               0361
         BNZ   @RF00361                                            0361
         L     R3,@AL00001+8        POINT TO FUNCTION CODE      ZP60014
         CLC   FUNC(4,R3),FW12      GETTING NUMERIC VALUE?      ZP60014
         BNE   NOT#VALU             NO                          ZP60014
         L     R3,SNTELPTR(,COMONPTR)                           ZP60014
         L     R1,SNTVLPTR(,R3)     POINT TO VALUE ELEMENT      ZP60014
         LH    R0,SVTLNG(,R1)       GET VALUE LENGTH            ZP60014
         LA    R1,SVTDATA(,R1)      GET VALUE ADDRESS           ZP60014
         STH   R0,CLEN(,COMONPTR)   SET VALUE LENGTH            ZP60014
         ST    R1,ANSPTR(,COMONPTR) SET VALUE ADDRESS           ZP60014
         BAL   R14,CONVERT          CONVERT TO BINARY           ZP60014
         L     R15,ERROR            GET ERROR CODE              ZP60014
         XC    ERROR,ERROR          RESET ERROR CODE            ZP60014
         LTR   R15,R15              VALID NUMERIC?              ZP60014
         BNZ   @RF00361             NO, RETURN ZERO             ZP60014
         MVC   ERROR,V2             YES, SET NUMERIC VALUE      ZP60014
         B     @RF00361             RETURN BINARY VALUE         ZP60014
NOT#VALU DS    0H                                               ZP60014
*     CALL UPDATE(UPVAL,UPVALLEN);  /* UPDATE THE VARIABLE           */
         L     R5,@PC00001+20                                      0362
         L     R1,UPVALPTR(,R5)                                    0362
         ST    R1,@AL00001                                         0362
         LA    R2,UPVALLEN(,R5)                                    0362
         ST    R2,@AL00001+4                                       0362
         LA    R1,@AL00001                                         0362
         BAL   R14,UPDATE                                          0362
*   RETURN CODE(ERROR);             /* RETURN TO GETLINE             */
@RF00361 L     R3,ERROR                                            0363
         L     R13,4(,R13)                                         0363
         L     R0,@SIZDATD                                         0363
         LR    R1,R9                                               0363
         IKJEXEC  R,LV=(0),A=(1)
         LR    R15,R3                                              0363
         L     R14,12(,R13)                                        0363
         LM    R0,R12,20(R13)                                      0363
         BR    R14                                                 0363
*                                                                  0364
*   /*****************************************************************/
*   /*                                                               */
*   /* INITIALIZATION ROUTINE                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0364
*INTSETUP:                                                         0364
*   PROC;                                                          0364
*EL00001 L     R13,4(,R13)                                         0364
*EF00001 L     R0,@SIZDATD                                         0364
*        LR    R1,R9                                               0364
*        IKJEXEC  R,LV=(0),A=(1)
*ER00001 LM    R14,R12,12(R13)                                     0364
*        BR    R14                                                 0364
INTSETUP STM   R14,R12,12(R13)                                     0364
*   STFLUSH(1:MACLEN)=STACKL(1:MACLEN);/* COPY LIST MACROS           */
**       L     R4,MACLEN                                  ZP60014  0365
**       BCTR  R4,0                                       ZP60014  0365
**       EX    R4,@SM01094                                ZP60014  0365
         MVC   STFLUSH(MACLEN),STACKL                     ZP60014  0365
*   IOPL(1)=ADDR(UPT);              /* ADDR OF UPT                   */
         L     R5,@PC00001                                         0366
         ST    R5,IOPL(,COMONPTR)                                  0366
*   IOPL(2)=ADDR(ECT);              /* ADDR OF ECT                   */
         L     R6,@PC00001+4                                       0367
         ST    R6,IOPL+4(,COMONPTR)                                0367
*   IOPL(3)=ADDR(ECB);              /* ADDR OF ECB                   */
         L     R7,@PC00001+8                                       0368
         ST    R7,IOPL+8(,COMONPTR)                                0368
*   ERROR=F0;                       /* ZERO ERROR INDICATOR          */
         SLR   R7,R7                                               0369
         ST    R7,ERROR                                            0369
*   ERCOM=F0;                       /* ZERO ERROR INDICATOR (COMMON) */
         ST    R7,ERCOM(,COMONPTR)                                 0370
*   FREEAMT=F0;                     /* ZERO AMOUNT THAT IS FREE      */
         ST    R7,FREEAMT(,COMONPTR)                               0371
*   FREEADDR(1)=F0;                 /* INIT FREEMAIN ADDR TO 0       */
         ST    R7,FREEADDR                                         0372
*   FREELEN(1)=F0;                  /* INIT FREEMAIN LENGTH TO 0     */
         ST    R7,FREELEN                                          0373
*   SW2=''B;                        /* TURN OFF LOOP SWITCHES        */
         MVI   SW2(COMONPTR),X'00'                                 0374
*   FRELST=''B;                     /* CLEAR FREEMAIN LIST AREA      */
         XC    FRELST(10),FRELST                                   0375
*   GETLST=''B;                     /* CLEAR GETMAIN LIST AREA       */
         XC    GETLST(10),GETLST                                   0376
*   LSDPTR=INSADLSD;                /* POINT TO LSD                  */
         L     R1,ECTIOWA-ECT(,R6)                                 0377
         L     R1,IOSTELM(,R1)                                     0377
         SLR   R0,R0                                               0377
         ICM   R0,7,INSADLSD(R1)                                   0377
         ST    R0,LSDPTR(,COMONPTR)                                0377
*   DO;                             /* GETMAIN (EC) MF(M,GETLST)     */
*     GETLST04='20'X;               /* TYPE                          */
         MVI   GETLST04,X'20'                                      0379
*   END;                            /* GETMAIN (EC) MF(M,GETLST) SET
*                                      TYPE OF GETMAIN               */
*   DO;                             /* FREEMAIN (E) MF(M,FRELST)     */
*     FRELST05='00'X;               /* SET TYPE                      */
         MVI   FRELST05,X'00'                                      0382
*   END;                            /* FREEMAIN (E) MF(M,FRELST) SET
*                                      TYPE OF FREEMAIN              */
*   END INTSETUP;                                                  0384
@EL00002 DS    0H                                                  0384
@EF00002 DS    0H                                                  0384
@ER00002 LM    R14,R12,12(R13)                                     0384
         BR    R14                                                 0384
*                                                                  0385
*   /*****************************************************************/
*   /*                                                               */
*   /* MESSAGE OUTPUT ROUTINE                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0385
*MSGOUT:                                                           0385
*   PROC OPTIONS(SAVEAREA);                                        0385
MSGOUT   STM   R14,R12,12(R13)                                     0385
         ST    R13,@SA00003+4                                      0385
         LA    R14,@SA00003                                        0385
         ST    R14,8(,R13)                                         0385
         LR    R13,R14                                             0385
*   MATCH=NO;                       /* RESET MATCH SWITCH            */
         NI    MATCH(COMONPTR),B'11101111'                         0386
*   IF RCABEND=YES THEN             /* FOR AN ABEND ERROR            */
         TM    RCABEND(COMONPTR),B'10000000'                       0387
         BNO   @RF00387                                            0387
*     I=MSGDIM;                     /* GET INTERNAL ERROR MSG        */
         L     R1,@MSGTABL                                         0388
         LH    R2,MSGDIM-MSGTABL(,R1)                              0388
         ST    R2,I(,COMONPTR)                                     0388
*   ELSE                            /* OTHERWISE START FROM BEGINNING*/
*     I=F1;                         /*                               */
         B     @RC00387                                            0389
@RF00387 MVC   I(4,COMONPTR),FW1                                   0389
*   DO I=I TO MSGDIM WHILE MATCH=NO;/* NOT FOUND?-TRY HERE           */
@RC00387 L     R3,I(,COMONPTR)                                     0390
         B     @DE00390                                            0390
@DL00390 TM    MATCH(COMONPTR),B'00010000'                         0390
         BNZ   @DC00390                                            0390
*     IF(ERRSAV>=IDA(I)&ERRSAV<=IDC(I)) I=MSGDIM THEN/* A MATCH      */
         L     R4,ERRSAV                                           0391
         L     R5,I(,COMONPTR)                                     0391
         MH    R5,HW12                                             0391
         L     R6,@MSGTABL                                         0391
         LA    R1,0(R5,R6)                                         0391
         AL    R1,FWNEG10                                          0391
         CH    R4,IDA-MSGTABL-2(,R1)                               0391
         BL    @GL00025                                            0391
         AL    R6,FWNEG8                                           0391
         CH    R4,IDC-MSGTABL-4(R5,R6)                             0391
         BNH   @RT00391                                            0391
@GL00025 L     R2,@MSGTABL                                         0391
         L     R3,I(,COMONPTR)                                     0391
         CH    R3,MSGDIM-MSGTABL(,R2)                              0391
         BNE   @RF00391                                            0391
@RT00391 DS    0H                                                  0392
*       DO;                         /* FILL IN OLD                   */
*         MATCH=YES;                /* TERMINATE SCAN FOR MESSAGE    */
         OI    MATCH(COMONPTR),B'00010000'                         0393
*         N=SEGNUM(I);              /* GET CURRENT NUMBER OF SEGMENTS*/
         L     R4,I(,COMONPTR)                                     0394
         MH    R4,HW12                                             0394
         L     R5,@MSGTABL                                         0394
         LA    R1,0(R4,R5)                                         0394
         BCTR  R1,0                                                0394
         BCTR  R1,0                                                0394
         SLR   R0,R0                                               0394
         IC    R0,SEGNUM-MSGTABL-10(,R1)                           0394
         ST    R0,N                                                0394
*         OLD(3)=MSGSEG1(I);        /* FIRST SEGMENT                 */
         ST    R4,@AFTEMPS
         ALR   R4,R5
         AL    R4,FWNEG5                                           0395
         SLR   R1,R1                                               0395
         ICM   R1,7,MSGSEG1-MSGTABL-7(R4)                          0395
         ST    R1,OLD+8                                            0395
*         OLD(4)=MSGSEG2(I);        /* SECOND SEGMENT IF ONE         */
         AL    R5,@AFTEMPS
         BCTR  R5,0                                                0396
         SLR   R4,R4                                               0396
         ICM   R4,7,MSGSEG2-MSGTABL-11(R5)                         0396
         ST    R4,OLD+12                                           0396
*         IF RECURSE=YES&ERRSAV=E908 THEN/* IF ALSO TYPE ERROR       */
         TM    RECURSE(COMONPTR),B'00100000'                       0397
         BNO   @RF00397                                            0397
         CLC   ERRSAV(4),FW908                                     0397
         BNE   @RF00397                                            0397
*           DO;                     /* FILL IN OLD                   */
*             OLD(2)=N+F1;          /* SET COUNT                     */
         AL    R0,FW1                                              0399
         ST    R0,OLD+4                                            0399
*             OLD(5)=OLD(4);        /* GET ROOM FOR ALSO             */
         L     R6,OLD+12                                           0400
         ST    R6,OLD+16                                           0400
*             OLD(4)=ADDR(ALSO);    /* GET ADDR OF ALSO              */
         L     R7,@ALSO                                            0401
         ST    R7,OLD+12                                           0401
*           END;                    /*                               */
*         ELSE                      /* OTHERWISE                     */
*           OLD(2)=N;               /* NUMBER OF SEGMENTS            */
         B     @RC00397                                            0403
@RF00397 L     R15,N                                               0403
         ST    R15,OLD+4                                           0403
*         IF SECLEVEL(I)=YES THEN   /* IF PROCESSING REQ SECOND LVL  */
@RC00397 L     R0,I(,COMONPTR)                                     0404
         MH    R0,HW12                                             0404
         L     R1,@MSGTABL                                         0404
         ALR   R1,R0                                               0404
         AL    R1,FWNEG6                                           0404
         TM    SECLEVEL-MSGTABL-6(R1),B'10000000'                  0404
         BNO   @RF00404                                            0404
*           DO;                     /* BUILD SECOND LEVEL MSG        */
*             OLD(1)=ADDR(OLD2);    /* POINT TO SECOND LEVEL         */
         LA    R2,OLD2                                             0406
         ST    R2,OLD                                              0406
*             OLD2(1)=F0;           /* NO THIRD LEVEL MESSAGE        */
         SLR   R3,R3                                               0407
         ST    R3,OLD2                                             0407
*             OLD2(2)=F1;           /* ONE SEGMENT                   */
         MVC   OLD2+4(4),FW1                                       0408
*             OLD2(3)=ADDR(MYSEG);  /* MESSAGE                       */
         LA    R2,MYSEG                                            0409
         ST    R2,OLD2+8                                           0409
*             MYSEG=MSTRLV2(1:LENGTH(MYSEG));/* COPY MSG TO GOTTEN   */
         L     R2,@MSTRLV2                                         0410
         MVC   MYSEG(35),MSTRLV2-MSTRLV2(R2)                       0410
         L     R2,EXECDPTR(,COMONPTR)
         CLC   STAECNT(2,R2),HW1
         BNH   LBL00D28
         STH   R3,STAECNT(,R2)
         MVC   OLD2+4(4),FW2
         LA    R3,1484(,R9)
         ST    R3,1556(,R9)
         L     R4,@MSTRLV3
         MVC   1484(32,R9),MSTRLV3-MSTRLV3(R4)
*             CALL LOCATE(LASTCC,LENGTH(LASTCC),F0);/* LOCATE LASTCC */
LBL00D28 LA    R1,@AL00411                                         0411
         BAL   R14,LOCATE                                          0411
*             IF ERROR=F0&SVTLNG>F0 THEN/* IF LOCATE OK              */
         SLR   R5,R5                                               0412
         C     R5,ERROR                                            0412
         BNE   @RF00412                                            0412
         L     R1,SNTELPTR(,COMONPTR)                              0412
         L     R4,SNTVLPTR(,R1)                                    0412
         LH    R6,SVTLNG(,R4)                                      0412
         CR    R6,R5                                               0412
         BNH   @RF00412                                            0412
*               DO;                 /* FILL IN CODE                  */
*                 N=MIN(SVTLNG,F5); /* SAVE LENGTH (MAX OF 5)        */
         LA    R7,5                                                0414
         CR    R6,R7                                               0414
         BNH   *+6
         LR    R6,R7                                               0414
         ST    R6,N                                                0414
*                 IF SVTDATA(1)=CCS SVTDATA(1)=CCU THEN/* ABEND CD   */
         CLI   SVTDATA(R4),C'S'                                    0415
         BE    @RT00415                                            0415
         CLI   SVTDATA(R4),C'U'                                    0415
         BNE   @RF00415                                            0415
@RT00415 DS    0H                                                  0416
*                   DO;             /* ABEND CODE ?                  */
*                     N=MIN(SVTLNG,F5);/* SAVE LENGTH (MAX OF 5)     */
         L     R1,SNTELPTR(,COMONPTR)                              0417
         L     R2,SNTVLPTR(,R1)                                    0417
         LH    R3,SVTLNG(,R2)                                      0417
         LA    R4,5                                                0417
         CR    R3,R4                                               0417
         BNH   *+6
         LR    R3,R4                                               0417
         ST    R3,N                                                0417
*                     OUTCD=SVTDATA(1:N);/* OK ALL CHARS             */
         MVI   OUTCD+1,C' '                                        0418
         MVC   OUTCD+2(3),OUTCD+1                                  0418
         BCTR  R3,0                                                0418
         EX    R3,@SM01100                                         0418
*                   END;            /*                               */
*                 ELSE              /* OTHERWISE                     */
*                   DO;             /* DONT OVERLAY THE 'E'          */
         B     @RC00415                                            0420
@RF00415 DS    0H                                                  0421
*                     N=MIN(SVTLNG,F3);/* MAX NUMBER LENGTH          */
         L     R1,SNTELPTR(,COMONPTR)                              0421
         L     R2,SNTVLPTR(,R1)                                    0421
         LH    R3,SVTLNG(,R2)                                      0421
         LA    R4,3                                                0421
         CR    R3,R4                                               0421
         BNH   *+6
         LR    R3,R4                                               0421
         ST    R3,N                                                0421
*                     OUTCD(5-N:4)=SVTDATA(1:N);/* ON NUMBERS        */
         LCR   R3,R3                                               0422
         AL    R3,FW5                                              0422
         LA    R1,OUTCD-1(R3)                                      0422
         LCR   R3,R3                                               0422
         AL    R3,FW4                                              0422
         EX    R3,@SM01102                                         0422
*                   END;            /*                               */
*               END;                /*                               */
*           END;                    /*                               */
*         ELSE                      /* OTHERWISE                     */
*           OLD(1)=F0;              /* NO SECOUND LEVEL MESSAGE      */
         B     @RC00404                                            0426
@RF00404 SLR   R2,R2                                               0426
         ST    R2,OLD                                              0426
*       END;                        /*                               */
@RC00404 DS    0H                                                  0428
*   END;                            /*                               */
@RF00391 LA    R3,1                                                0428
         AL    R3,I(,COMONPTR)                                     0428
         ST    R3,I(,COMONPTR)                                     0428
@DE00390 L     R2,@MSGTABL                                         0428
         CH    R3,MSGDIM-MSGTABL(,R2)                              0428
         BNH   @DL00390                                            0428
@DC00390 DS    0H                                                  0429
*   IOPL(4)=ADDR(PUTMLVL);          /* PUTLINE LIST FORM             */
         LA    R3,PUTMLVL                                          0429
         ST    R3,IOPL+12(,COMONPTR)                               0429
*   RFY                                                            0430
*    (R1,                                                          0430
*     R2,                                                          0430
*     R15) RSTD;                    /* RESTRICT ENTRY REG            */
*   R1=ADDR(IOPL);                  /* GET ADDR OF IOPL              */
         LR    R1,COMONPTR                                         0431
*   R2=ADDR(OLD);                   /* GET ADDR OF OLD               */
         LA    R2,OLD                                              0432
*   R15=ADDR(IKJEFT40);             /* ENTRY TO PUTLINE              */
         L     R15,IKJEFT40                                        0433
*   GEN REFS(R1,R2,R15)                                            0434
*       (PUTLINE OUTPUT=((2),MULTLVL),MF=(E,(1)),ENTRY=(15));      0434
         PUTLINE OUTPUT=((2),MULTLVL),MF=(E,(1)),ENTRY=(15)
*   RFY                                                            0435
*    (R1,                                                          0435
*     R2,                                                          0435
*     R15) UNRSTD;                  /* RELEASE REG 15                */
*   END MSGOUT;                                                    0436
@EL00003 L     R13,4(,R13)                                         0436
@EF00003 DS    0H                                                  0436
@ER00003 LM    R14,R12,12(R13)                                     0436
         BR    R14                                                 0436
*                                                                  0437
*   /*****************************************************************/
*   /*                                                               */
*   /* INSERT MAINLINE                                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0437
*INSERT:                                                           0437
*   PROC;                                                          0437
INSERT   STM   R14,R12,@SA00004                                    0437
*   IF INSTHEN=NO THEN              /* AS LONG AS NOT INSERT THEN    */
         TM    INSTHEN(COMONPTR),B'00000001'                       0438
         BNZ   @RF00438                                            0438
*     CALL LOCATE(C(V),LEN,F0);     /* LOCATE SYMBOLIC VARIABLE      */
         L     R1,BUFBASE(,COMONPTR)                               0439
         L     R2,V                                                0439
         LA    R3,C-1(R2,R1)                                       0439
         ST    R3,@AL00001                                         0439
         LA    R4,LEN(,COMONPTR)                                   0439
         ST    R4,@AL00001+4                                       0439
         LA    R5,FW0                                              0439
         ST    R5,@AL00001+8                                       0439
         LA    R1,@AL00001                                         0439
         BAL   R14,LOCATE                                          0439
*   IF ERROR=F0 THEN                /* IF VARIABLE FOUND             */
@RF00438 L     R6,ERROR                                            0440
         LTR   R6,R6                                               0440
         BNZ   @RF00440                                            0440
*     DO;                           /*                               */
*       IF NOAMPER=NO THEN          /* IF AMPERSAND PRESENT          */
         TM    NOAMPER(COMONPTR),B'00000010'                       0442
         BNZ   @RF00442                                            0442
*         DO;                       /* RESET V TO AMPERSAND AND      */
*           V=V-F1;                 /* ADD THE LENGTH OF AN          */
         L     R7,V                                                0444
         BCTR  R7,0                                                0444
         ST    R7,V                                                0444
*           LEN=LEN+F1;             /* AMPERSAND TO LEN              */
         LA    R14,1                                               0445
         AL    R14,LEN(,COMONPTR)                                  0445
         ST    R14,LEN(,COMONPTR)                                  0445
*         END;                      /*                               */
*       IF INSTHEN=YES THEN         /* IF IN INSERT THEN MODE        */
@RF00442 TM    INSTHEN(COMONPTR),B'00000001'                       0447
         BNO   @RF00447                                            0447
*         DO;                       /* POINT TO THEN                 */
*           INSRTDA=ADDR(CTHEN);    /* ADDR OF THEN                  */
         LA    R15,$THEN                                           0449
         ST    R15,INSRTDA                                         0449
*           INSRTLN=LENGTH(CTHEN);  /* LENGTH OF THEN                */
         MVC   INSRTLN(4),FW4                                      0450
*         END;                      /*                               */
*       ELSE                        /* OTHERWISE                     */
*         DO;                       /* GET VARIABLES INSERT DATA     */
         B     @RC00447                                            0452
@RF00447 DS    0H                                                  0453
*           INSRTLN=SVTLNG;         /* GET INSERT LENGTH             */
         L     R0,SNTELPTR(,COMONPTR)                              0453
         LR    R1,R0
         L     R1,SNTVLPTR(,R1)                                    0453
         LH    R2,SVTLNG(,R1)                                      0453
         ST    R2,INSRTLN                                          0453
*           INSRTDA=ADDR(SVTDATA);  /* GET POINTER TO INSERT DATA    */
         LA    R1,SVTDATA(,R1)                                     0454
         ST    R1,INSRTDA                                          0454
*           IF SNTEVAL=YES THEN     /* IF EVALUATION REQUIRED        */
         LR    R1,R0
         TM    SNTEVAL(R1),B'00000010'                             0455
         BNO   @RF00455                                            0455
*             CALL IMVALRTN;        /* CALL ROUTINE TO PROCESS EVAL  */
         BAL   R14,IMVALRTN                                        0456
*         END;                                                     0457
@RF00455 DS    0H                                                  0458
*       EXITA=NO;                   /* SAY WE HAVE INSERTED DATA     */
@RC00447 NI    EXITA(COMONPTR),B'01111111'                         0458
*       IF ERROR=F0&WAIT=NO THEN    /* IF NO ERROR FROM BUILT IN FUNC*/
         L     R2,ERROR                                            0459
         LTR   R2,R2                                               0459
         BNZ   @RF00459                                            0459
         TM    WAIT(COMONPTR),B'00010000'                          0459
         BNZ   @RF00459                                            0459
*         DO;                       /* PERFORM INSERT                */
*           IF INSRTLN-LEN>FREEAMT THEN/* IF INSERT DATA IS GREATER  */
         L     R3,INSRTLN                                          0461
         SL    R3,LEN(,COMONPTR)                                   0461
         L     R2,FREEAMT(,COMONPTR)                               0461
         CR    R3,R2                                               0461
         BNH   @RF00461                                            0461
*             CALL GETMORE(INSRTLN-LEN-FREEAMT);/* GET BIGGER BUFFER */
         SLR   R3,R2                                               0462
         ST    R3,@AFTEMPS+4                                       0462
         LA    R3,@AFTEMPS+4                                       0462
         ST    R3,@AL00001                                         0462
         LA    R1,@AL00001                                         0462
         BAL   R14,GETMORE                                         0462
*           ELSE                    /* OTHERWISE UPDATE FREE AMOUNT  */
*             FREEAMT=FREEAMT-(INSRTLN-LEN);/* UPDATE FREEAMT        */
         B     @RC00461                                            0463
@RF00461 L     R4,INSRTLN                                          0463
         SL    R4,LEN(,COMONPTR)                                   0463
         LCR   R4,R4                                               0463
         AL    R4,FREEAMT(,COMONPTR)                               0463
         ST    R4,FREEAMT(,COMONPTR)                               0463
*           IF ERROR=F0 THEN        /* NO ERROR FROM GETMORE         */
@RC00461 L     R5,ERROR                                            0464
         LTR   R5,R5                                               0464
         BNZ   @RF00464                                            0464
*             DO;                   /*                               */
*               RFY                                                0466
*                (R2,                                              0466
*                 R3,                                              0466
*                 R4,                                              0466
*                 R5) RSTD;         /* RESTRICT REGISTERS            */
*               R2=ADDR(NEWC);      /* POINT TO NEW BUFFER           */
         L     R2,NEWBASE(,COMONPTR)                               0467
         LA    R2,NEWC(,R2)                                        0467
*               R3=V-F1;            /* LENGTH OF FIRST PART          */
         L     R3,V                                                0468
         BCTR  R3,0                                                0468
*               R4=ADDR(C);         /* POINT TO OLD BUFFER           */
         L     R4,BUFBASE(,COMONPTR)                               0469
         LA    R4,C(,R4)                                           0469
*               R5=R3;              /* SAME LENGTH                   */
         LR    R5,R3                                               0470
*               MVCL(R2,R4);        /* MOVE FIRST PART OF BUFFER     */
         MVCL  R2,R4                                               0471
*               KPTR=R2;            /* SAVE LOC IN BUFFER WHERE DATA
*                                      IS GOING                      */
         ST    R2,KPTR                                             0472
*               R3=INSRTLN;         /* LENGTH OF SYMBOLIC VARIABLE   */
         L     R3,INSRTLN                                          0473
*               R4=INSRTDA;         /* POINT TO SUBSTITUTE DATA      */
         L     R4,INSRTDA                                          0474
*               R5=R3;              /* EQUAL LENGTH                  */
         LR    R5,R3                                               0475
*               MVCL(R2,R4);        /* MOVE IN DATA (R2 UPDATED BY 0476
*                                      MVCL)                         */
         MVCL  R2,R4                                               0476
*               R3=NEWLL-(INSRTLN+V+F3);/* TO LENGTH - ALREADY MOVED */
         L     R6,NEWBASE(,COMONPTR)                               0477
         LA    R7,3                                                0477
         L     R3,INSRTLN                                          0477
         AL    R3,V                                                0477
         ALR   R3,R7                                               0477
         LCR   R3,R3                                               0477
         AH    R3,NEWLL(,R6)                                       0477
*               R4=ADDR(C(I));      /* POINT TO RESUME BUFFER        */
         L     R14,I(,COMONPTR)                                    0478
         L     R15,BUFBASE(,COMONPTR)                              0478
         LA    R4,C-1(R14,R15)                                     0478
*               R5=(LL-(I+F3)) BLANKS;/* THE REMAINING SEGMENT AND 0479
*                                      PAD                           */
         ALR   R14,R7                                              0479
         LCR   R14,R14                                             0479
         AH    R14,LL(,R15)                                        0479
         O     R14,HIBLANK                                         0479
         LR    R5,R14                                              0479
*               I=R2-(ADDR(NEWC));  /* POINT I TO RESUME SEGMENT     */
         LA    R6,NEWC(,R6)                                        0480
         LCR   R6,R6                                               0480
         ALR   R6,R2                                               0480
         ST    R6,I(,COMONPTR)                                     0480
*               MVCL(R2,R4);        /* MOVE RESUME PART OF BUFFER    */
         MVCL  R2,R4                                               0481
*               RFY                                                0482
*                (R2,                                              0482
*                 R3,                                              0482
*                 R4,                                              0482
*                 R5) UNRSTD;       /* RELEASE REGISTERS             */
*               IF SNTNOSCN=YES THEN/* IF NO RESCAN ALLOWED          */
         L     R1,SNTELPTR(,COMONPTR)                              0483
         TM    SNTNOSCN(R1),B'00001000'                            0483
         BNO   @RF00483                                            0483
*                 DO K=INSRTLN TO F1 BY-F1;                        0484
         L     R2,INSRTLN                                          0484
         ST    R2,K                                                0484
         B     @DE00484                                            0484
@DL00484 DS    0H                                                  0485
*                   FOLD=YES;       /* FOLD TO SPECIAL CHARS         */
         OI    FOLD(COMONPTR),B'00010000'                          0485
*                   IF KPTR->INSRTCHR(K)=AMPSAND THEN/* NO RESCAN    */
         L     R3,KPTR                                             0486
         ALR   R2,R3                                               0486
         BCTR  R2,0                                                0486
         CLI   INSRTCHR(R2),C'&&'                                  0486
         BNE   @RF00486                                            0486
*                     KPTR->INSRTCHR(K)=NOSCNAMP;/* REQUIRES SPECIAL
*                                      FOLDED NO-RESCAN AMPERSAND    */
         L     R2,K                                                0487
         ALR   R3,R2                                               0487
         BCTR  R3,0                                                0487
         MVI   INSRTCHR(R3),X'03'                                  0487
*                   ELSE                                           0488
*                     TR(KPTR->INSRTCHR(K),TRTABDN);               0488
         B     @RC00486                                            0488
@RF00486 L     R1,KPTR                                             0488
         AL    R1,K                                                0488
         BCTR  R1,0                                                0488
         TR    INSRTCHR(1,R1),TRTABDN                              0488
*                 END;                                             0489
*                                                                  0489
@RC00486 SLR   R2,R2                                               0489
         BCTR  R2,0                                                0489
         AL    R2,K                                                0489
         ST    R2,K                                                0489
@DE00484 LTR   R2,R2                                               0489
         BP    @DL00484                                            0489
*               /*****************************************************/
*               /*                                                   */
*               /* FREE OLD BUFFER IF A NEW ONE WAS GOTTEN           */
*               /*                                                   */
*               /*****************************************************/
*                                                                  0490
*               IF FREESW=YES THEN  /* IF FREEMAIN NECESSARY         */
@RF00483 TM    FREESW(COMONPTR),B'00100000'                        0490
         BNO   @RF00490                                            0490
*                 DO;               /*                               */
*                   DO;             /* FREEMAIN LV(FREELEN(1))     0492
*                                      A(FREEADDR) SP(1) MF(E,FRELST)*/
*                     RESPECIFY                                    0493
*                      (R1) RESTRICTED;                            0493
*                     FRELST02=FREELEN(1);/* LENGTH                  */
         MVC   FRELST02(3),FREELEN+1                               0494
*                     FRELST04=ADDR(FREEADDR);/* ADDR OF ADDR LIST   */
         LA    R0,FREEADDR                                         0495
         STCM  R0,7,FRELST04                                       0495
*                     FRELST06=1;   /* SUBPOOL VALUE                 */
         MVI   FRELST06,X'01'                                      0496
*                     R1=ADDR(FRELST);/* REG1 POINTS TO LIST         */
         LA    R1,FRELST                                           0497
*                     SVC(5);       /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0498
*                     RESPECIFY                                    0499
*                      (R1) UNRESTRICTED;                          0499
*                   END;            /* FREEMAIN LV(FREELEN(1))     0500
*                                      A(FREEADDR) SP(1) MF(E,FRELST)*/
*                   BUFBASE=NEWBASE;/* RESET POINTER TO BUFFER       */
         L     R2,NEWBASE(,COMONPTR)                               0501
         ST    R2,BUFBASE(,COMONPTR)                               0501
*                   NEWBASE=NEWBASE+LL;/* POINT TO NEW FREE BUFFER   */
         LH    R3,LL(,R2)                                          0502
         ALR   R2,R3                                               0502
         ST    R2,NEWBASE(,COMONPTR)                               0502
*                   NEWLL=LL;       /* INIT LENGTH                   */
         STH   R3,NEWLL(,R2)                                       0503
*                   NEWOO=F0;       /* INIT OFFSET                   */
         SLR   R3,R3                                               0504
         STH   R3,NEWOO(,R2)                                       0504
*                   FREESW=NO;      /* RESET FREESWITCH              */
         NI    FREESW(COMONPTR),B'11011111'                        0505
*                 END;                                             0506
*               ELSE                /* OTHERWISE                     */
*                 DO;                                              0507
         B     @RC00490                                            0507
@RF00490 DS    0H                                                  0508
*                   BUFBASE=BUFBASE&&NEWBASE;/* SWAP BUFFER POINTERS */
         L     R4,NEWBASE(,COMONPTR)                               0508
         L     R5,BUFBASE(,COMONPTR)                               0508
         XR    R5,R4                                               0508
         ST    R5,BUFBASE(,COMONPTR)                               0508
*                   NEWBASE=NEWBASE&&BUFBASE;/*                      */
         XR    R4,R5                                               0509
         ST    R4,NEWBASE(,COMONPTR)                               0509
*                   BUFBASE=BUFBASE&&NEWBASE;/*                      */
         XR    R5,R4                                               0510
         ST    R5,BUFBASE(,COMONPTR)                               0510
*                 END;                                             0511
*             END;                                                 0512
*         END;                                                     0513
*       ELSE                        /* OTHERWISE                     */
*         WAIT=NO;                  /* TURN OFFF WAIT                */
         B     @RC00459                                            0514
@RF00459 NI    WAIT(COMONPTR),B'11101111'                          0514
*     END;                                                         0515
*   END INSERT;                                                    0516
@EL00004 DS    0H                                                  0516
@EF00004 DS    0H                                                  0516
@ER00004 LM    R14,R12,@SA00004                                    0516
         BR    R14                                                 0516
*                                                                  0517
*   /*****************************************************************/
*   /*                                                               */
*   /* GET BIGGER BUFFER                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0517
*GETMORE:                                                          0517
*   PROC(MINIMUM);                                                 0517
GETMORE  STM   R14,R12,12(R13)                                     0517
         MVC   @PC00005(4),0(R1)                                   0517
*   DCL                                                            0518
*     MINIMUM FIXED(31);                                           0518
*   HOWMUCH=F16*((GAS+MINIMUM+LL)/F8);/* CALCULATE HOWMUCH TO GET    */
         L     R6,@PC00005                                         0519
         LA    R14,200                                             0519
         AL    R14,MINIMUM(,R6)                                    0519
         L     R7,BUFBASE(,COMONPTR)                               0519
         LH    R6,LL(,R7)                                          0519
         ALR   R14,R6                                              0519
         SRDA  R14,32                                              0519
         D     R14,FW8                                             0519
         SLA   R15,4                                               0519
         ST    R15,HOWMUCH                                         0519
*   IF CORE=YES THEN                /* IF ALREADY CORE               */
         TM    CORE(COMONPTR),B'10000000'                          0520
         BNO   @RF00520                                            0520
*     DO;                           /* FIND ADDR OF OLD CORE         */
*       FREESW=YES;                 /* ALLOW FREEMAIN                */
         OI    FREESW(COMONPTR),B'00100000'                        0522
*       FREEADDR(1)=MIN(BUFBASE,NEWBASE);/* GET LOW ADDR FOR FREE    */
         L     R14,NEWBASE(,COMONPTR)                              0523
         CR    R7,R14                                              0523
         BNH   *+6
         LR    R7,R14                                              0523
         ST    R7,FREEADDR                                         0523
*       FREELEN(1)=LL*F2;           /* AMOUNT TO FREE                */
         ALR   R6,R6                                               0524
         ST    R6,FREELEN                                          0524
*     END;                          /*                               */
*   DO;                             /* GETMAIN LV(HOWMUCH) A(NEWBASE)
*                                      SP(1) MF(E,GETLST)          0526
*                                      RTCD(RETURNCD)                */
@RF00520 DS    0H                                                  0527
*     RESPECIFY                                                    0527
*      (R1,                                                        0527
*       R15) RESTRICTED;                                           0527
*     GETLST01=HOWMUCH;             /* LENGTH                        */
         L     R14,HOWMUCH                                         0528
         ST    R14,GETLST01                                        0528
*     GETLST03=ADDR(NEWBASE);       /* ADDR OF ADDR LIST             */
         LA    R0,NEWBASE(,COMONPTR)                               0529
         STCM  R0,7,GETLST03                                       0529
*     GETLST05=1;                   /* SUBPOOL VALUE                 */
         MVI   GETLST05,X'01'                                      0530
*     R1=ADDR(GETLST);              /* REG1 POINTS TO LIST           */
         LA    R1,GETLST                                           0531
*     SVC(4);                       /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0532
*     RETURNCD=R15;                 /* SET RETURN CODE               */
         ST    R15,RETURNCD                                        0533
*     RESPECIFY                                                    0534
*      (R1,                                                        0534
*       R15) UNRESTRICTED;                                         0534
*   END;                            /* GETMAIN LV(HOWMUCH) A(NEWBASE)
*                                      SP(1) MF(E,GETLST)          0535
*                                      RTCD(RETURNCD)                */
*   IF ERROR=F0&RETURNCD^=F0 THEN   /* GETMAIN FAIL                  */
         SLR   R2,R2                                               0536
         C     R2,ERROR                                            0536
         BNE   @RF00536                                            0536
         L     R3,RETURNCD                                         0536
         CR    R3,R2                                               0536
         BE    @RF00536                                            0536
*     DO;                           /* AND PROCESS ORIG BUFFER       */
*       IF CORE=NO THEN             /* POINT TO ORIG BUFFER          */
         TM    CORE(COMONPTR),B'10000000'                          0538
         BNZ   @RF00538                                            0538
*         NEWBASE=BUFBASE;          /* FOR SQUASH                    */
         L     R5,BUFBASE(,COMONPTR)                               0539
         ST    R5,NEWBASE(,COMONPTR)                               0539
*       ERROR=E16;                  /* SET ERROR CODE                */
@RF00538 MVC   ERROR(4),FW16                                       0540
*     END;                                                         0541
*   ELSE                                                           0542
*     DO;                           /*                               */
         B     @RC00536                                            0542
@RF00536 DS    0H                                                  0543
*       NEWLL=HOWMUCH/F2;           /* SET NEW LL                    */
         L     R6,NEWBASE(,COMONPTR)                               0543
         L     R0,HOWMUCH                                          0543
         SRDA  R0,32                                               0543
         D     R0,FW2                                              0543
         STH   R1,NEWLL(,R6)                                       0543
*       FREEAMT=NEWLL-(LL+MINIMUM); /* RESET FREEAMT                 */
         L     R7,BUFBASE(,COMONPTR)                               0544
         L     R14,@PC00005                                        0544
         LH    R15,LL(,R7)                                         0544
         AL    R15,MINIMUM(,R14)                                   0544
         SLR   R1,R15                                              0544
         ST    R1,FREEAMT(,COMONPTR)                               0544
*       NEWOO=F0;                   /* INIT OFFSET TO ZERO           */
         SLR   R7,R7                                               0545
         STH   R7,NEWOO(,R6)                                       0545
*     END;                          /*                               */
*   END GETMORE;                                                   0547
@EL00005 DS    0H                                                  0547
@EF00005 DS    0H                                                  0547
@ER00005 LM    R14,R12,12(R13)                                     0547
         BR    R14                                                 0547
*                                                                  0548
*   /*****************************************************************/
*   /*                                                               */
*   /* REMOVE ONE CHARACTER FROM BUFFER                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0548
*SQUASH:                                                           0548
*   PROC;                                                          0548
SQUASH   STM   R14,R12,12(R13)                                     0548
*   RFY                                                            0549
*    (R2,                                                          0549
*     R3,                                                          0549
*     R4,                                                          0549
*     R5) RSTD;                     /* RESTRICT REGISTERS            */
*   R2=ADDR(C(I));                  /* ADDRESS OF 'TO'               */
         L     R14,I(,COMONPTR)                                    0550
         L     R15,BUFBASE(,COMONPTR)                              0550
         LA    R2,C-1(R14,R15)                                     0550
*   R4=ADDR(C(I+F1));               /* ADDRESS OF 'FROM'             */
         LA    R4,C(R14,R15)                                       0551
*   R3=LL-(I+F3);                   /* LENGTH OF 'TO'                */
         AL    R14,FW3                                             0552
         LCR   R14,R14                                             0552
         AH    R14,LL(,R15)                                        0552
         LR    R3,R14                                              0552
*   R5=(R3-F1) BLANKS;              /* LENGTH OF 'FROM' AND PADDING
*                                      CHARACTER FOR LONGER 'TO' AREA*/
         LR    R5,R3                                               0553
         BCTR  R5,0                                                0553
         O     R5,HIBLANK                                          0553
*   MVCL(R2,R4);                    /* SQUASH BUFFER                 */
         MVCL  R2,R4                                               0554
*   RFY                                                            0555
*    (R2,                                                          0555
*     R3,                                                          0555
*     R4,                                                          0555
*     R5) UNRSTD;                   /* RELEASE REGISTERS             */
*   FREEAMT=FREEAMT+F1;             /* ADD REMOVED BYTE TO FREEAMT   */
         LA    R6,1                                                0556
         AL    R6,FREEAMT(,COMONPTR)                               0556
         ST    R6,FREEAMT(,COMONPTR)                               0556
*   END SQUASH;                                                    0557
@EL00006 DS    0H                                                  0557
@EF00006 DS    0H                                                  0557
@ER00006 LM    R14,R12,12(R13)                                     0557
         BR    R14                                                 0557
*                                                                  0558
*   /*****************************************************************/
*   /*                                                               */
*   /* RETURN POINTER UPDATE ROUTINE                                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0558
*RETUPDT:                                                          0558
*   PROC(NEXTCMD);                                                 0558
RETUPDT  STM   R14,R12,12(R13)                                     0558
         MVC   @PC00007(4),0(R1)                                   0558
*   DCL                                                            0559
*     NEXTCMD FIXED(31);            /* THE NEXT COMMAND AFTER THE ONE
*                                      INTERRUPTED FROM              */
*   RETPTR2=RETPTR;                 /* SWAP AND SAVE RETURN          */
         L     R7,EXECDPTR(,COMONPTR)                              0560
         L     R6,RETPTR(,R7)                                      0560
         ST    R6,RETPTR2(,R7)                                     0560
*   RETPTR=NEXTCMD;                 /* SAVE ANSWER                   */
         L     R6,@PC00007                                         0561
         L     R6,NEXTCMD(,R6)                                     0561
         ST    R6,RETPTR(,R7)                                      0561
*   END RETUPDT;                    /*                               */
@EL00007 DS    0H                                                  0562
@EF00007 DS    0H                                                  0562
@ER00007 LM    R14,R12,12(R13)                                     0562
         BR    R14                                                 0562
*                                                                  0563
*   /*****************************************************************/
*   /*                                                               */
*   /* SUBSTRING FUNCTION                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0563
*SUBSTRTN:                                                         0563
*   PROC OPTIONS(SAVEAREA);                                        0563
SUBSTRTN STM   R14,R12,12(R13)                                     0563
         ST    R13,@SA00008+4                                      0563
         LA    R14,@SA00008                                        0563
         ST    R14,8(,R13)                                         0563
         LR    R13,R14                                             0563
*   CALL WAITRTN;                   /* CHECK IF '&' VARIABLE IN RNGE */
         BAL   R14,WAITRTN                                         0564
*   IF WAIT=NO&ERROR=F0 THEN        /* IF OK TO PROCEED THEN         */
         TM    WAIT(COMONPTR),B'00010000'                          0565
         BNZ   @RF00565                                            0565
         L     R7,ERROR                                            0565
         LTR   R7,R7                                               0565
         BNZ   @RF00565                                            0565
*     DO;                           /* GET RANGE TO PROCESS          */
*       CSAVE=C(I-F1:I);            /* SAVE ORIGINAL BUFFER          */
         L     R14,I(,COMONPTR)                                    0567
         L     R15,BUFBASE(,COMONPTR)                              0567
         ALR   R14,R15                                             0567
         MVC   CSAVE(2),C-2(R14)                                   0567
*       C(I-F1:I)=XEQUAL;           /* MAKE RANGE LOOK LINE SET STMT */
         L     R14,I(,COMONPTR)                                    0568
         ALR   R14,R15                                             0568
         MVC   C-2(2,R14),$XEQUALS                                 0568
*       BUFPTR=ADDR(C(I-F1));       /* POINT TO DUMMY SET STATEMENT  */
         L     R14,I(,COMONPTR)                                    0569
         LA    R15,C-2(R14,R15)                                    0569
         ST    R15,BUFPTR(,COMONPTR)                               0569
*       DO CLEN=F3 TO M-I+F1 WHILE BUF(CLEN)^=COLON&BUF(CLEN)^=COMMA;
         LA    R0,3                                                0570
         B     @DE00570                                            0570
@DL00570 LH    R1,CLEN(,COMONPTR)                                  0570
         L     R2,BUFPTR(,COMONPTR)                                0570
         ALR   R1,R2                                               0570
         BCTR  R1,0                                                0570
         CLI   BUF(R1),C':'                                        0570
         BE    @DC00570                                            0570
         LH    R3,CLEN(,COMONPTR)                                  0570
         ALR   R2,R3                                               0570
         BCTR  R2,0                                                0570
         CLI   BUF(R2),C','                                        0570
         BE    @DC00570                                            0570
*                                   /*                               */
*       END;                        /* AND LENGTH OF DUMMY STMT      */
         LA    R0,1                                                0571
         AH    R0,CLEN(,COMONPTR)                                  0571
@DE00570 STH   R0,CLEN(,COMONPTR)                                  0571
         L     R1,M                                                0571
         SL    R1,I(,COMONPTR)                                     0571
         AL    R1,FW1                                              0571
         CR    R0,R1                                               0571
         BNH   @DL00570                                            0571
@DC00570 DS    0H                                                  0572
*       IF BUF(CLEN)=':' THEN       /* IF COLON FOUND, THE SET RANGE */
         LH    R1,CLEN(,COMONPTR)                                  0572
         L     R2,BUFPTR(,COMONPTR)                                0572
         ALR   R2,R1                                               0572
         BCTR  R2,0                                                0572
         CLI   BUF(R2),C':'                                        0572
         BNE   @RF00572                                            0572
*         RANGE=YES;                /* SWITCH                        */
         OI    RANGE(COMONPTR),B'00001000'                         0573
*       ELSE                        /* CHECK FOR STRING DELIM        */
*         IF BUF(CLEN)=COMMA THEN   /* IF IT IS                      */
         B     @RC00572                                            0574
@RF00572 LH    R3,CLEN(,COMONPTR)                                  0574
         L     R2,BUFPTR(,COMONPTR)                                0574
         ALR   R3,R2                                               0574
         BCTR  R3,0                                                0574
         CLI   BUF(R3),C','                                        0574
         BNE   @RF00574                                            0574
*           DO;                     /* INIT INSERT START AND LENGTH  */
*             INSRTDA=ADDR(BUF(CLEN+F1));/* BEGIN OF STRING          */
         LH    R3,CLEN(,COMONPTR)                                  0576
         LA    R4,BUF(R3,R2)                                       0576
         ST    R4,INSRTDA                                          0576
*             INSRTLN=ADDR(C(M))-INSRTDA;/* LENGTH OF STRING         */
         L     R1,BUFBASE(,COMONPTR)                               0577
         L     R5,M                                                0577
         LA    R5,C-1(R5,R1)                                       0577
         SLR   R5,R4                                               0577
         ST    R5,INSRTLN                                          0577
*             RANGE=NO;             /* RE-INIT RANGE SWITCH  @ZA24504*/
         NI    RANGE(COMONPTR),B'11110111'                         0578
*           END;                    /*                               */
*       IF ERROR=F0 THEN            /* IF OK UNTIL NOW               */
@RF00574 DS    0H                                                  0580
@RC00572 SLR   R6,R6                                               0580
         C     R6,ERROR                                            0580
         BNE   @RF00580                                            0580
*         DO;                       /* THEN CONTINUE                 */
*           CLEN=CLEN-F1;           /* DECREMENT CLEN FROM DO INCR   */
         LH    R7,CLEN(,COMONPTR)                                  0582
         BCTR  R7,0                                                0582
         STH   R7,CLEN(,COMONPTR)                                  0582
*           TYPE=F0;                /* ALLOW ONLY NUMERIC STMT       */
         ST    R6,TYPE(,COMONPTR)                                  0583
*           LEN1=CLEN;              /* SAVE LENGTH OF FIRST STMT     */
         ST    R7,LEN1                                             0584
*           RFY                                                    0585
*            (R15) RSTD;            /* RESTRICT R15 FOR RETURNCD     */
*           CALL IKJCT434(BUF,CLEN,TYPE,OUTA1);/* PROCESS EXPRESSOIN */
         L     R14,BUFPTR(,COMONPTR)                               0586
         ST    R14,@AL00001                                        0586
         LA    R14,CLEN(,COMONPTR)                                 0586
         ST    R14,@AL00001+4                                      0586
         LA    R14,TYPE(,COMONPTR)                                 0586
         ST    R14,@AL00001+8                                      0586
         LA    R14,OUTA1                                           0586
         ST    R14,@AL00001+12                                     0586
         L     R15,IKJCT434                                        0586
         LA    R1,@AL00001                                         0586
         BALR  R14,R15                                             0586
*           ERROR=R15;              /* SAVE ERROR CODE               */
         ST    R15,ERROR                                           0587
*           C(I-F1:I)=CSAVE;        /* RESTORE ORIGINAL BUFFER       */
         L     R14,I(,COMONPTR)                                    0588
         L     R1,BUFBASE(,COMONPTR)                               0588
         ALR   R1,R14                                              0588
         MVC   C-2(2,R1),CSAVE                                     0588
*           RFY                                                    0589
*            (R15) UNRSTD;          /* RELEASE R15                   */
*                                                                  0590
*           /*********************************************************/
*           /*                                                       */
*           /* PROCESS SUBSTRING                                     */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0590
*           IF ERROR=F0 THEN        /* IF OK                         */
         L     R2,ERROR                                            0590
         LTR   R2,R2                                               0590
         BNZ   @RF00590                                            0590
*             DO;                   /* CONVERT ANSWER TO USEABLE     */
*               CALL CONVERT;       /* FORM                          */
         BAL   R14,CONVERT                                         0592
*               V1=V2;              /* SET START OF RANGE            */
         L     R4,V2                                               0593
         ST    R4,V1                                               0593
*               IF ERROR=F0&RANGE=YES THEN/* IF RANGE, THEN FIND   0594
*                                      VALUE 2                       */
         L     R5,ERROR                                            0594
         LTR   R5,R5                                               0594
         BNZ   @RF00594                                            0594
         TM    RANGE(COMONPTR),B'00001000'                         0594
         BNO   @RF00594                                            0594
*                 DO;               /*                               */
*                   BUFPTR=ADDR(BUF(LEN1));/* POINT TO DUMMY STMT  0596
*                                      NUMBER 2                      */
         L     R1,BUFPTR(,COMONPTR)                                0596
         L     R2,LEN1                                             0596
         BCTR  R1,0                                                0596
         LA    R3,BUF(R2,R1)                                       0596
         ST    R3,BUFPTR(,COMONPTR)                                0596
*                   CSAVE=BUF(F1:F2);/* SAVE ORIGINAL BUFFER         */
         MVC   CSAVE(2),BUF(R3)                                    0597
*                   BUF(F1:F2)=XEQUAL;/* CREATE DUMMY STMT           */
         MVC   BUF(2,R3),$XEQUALS                                  0598
*                   DO CLEN=F3 TO M-(I+LEN1)+F2 WHILE BUF(CLEN)^=COMMA;
         LA    R4,3                                                0599
         B     @DE00599                                            0599
@DL00599 LH    R1,CLEN(,COMONPTR)                                  0599
         L     R2,BUFPTR(,COMONPTR)                                0599
         ALR   R2,R1                                               0599
         BCTR  R2,0                                                0599
         CLI   BUF(R2),C','                                        0599
         BE    @DC00599                                            0599
*                   END;                                           0600
         LA    R4,1                                                0600
         AH    R4,CLEN(,COMONPTR)                                  0600
@DE00599 STH   R4,CLEN(,COMONPTR)                                  0600
         L     R5,I(,COMONPTR)                                     0600
         AL    R5,LEN1                                             0600
         LCR   R5,R5                                               0600
         AL    R5,M                                                0600
         AL    R5,FW2                                              0600
         CR    R4,R5                                               0600
         BNH   @DL00599                                            0600
@DC00599 DS    0H                                                  0601
*                   INSRTDA=ADDR(BUF(CLEN+1));/* POINT TO INSERT DATA*/
         LH    R6,CLEN(,COMONPTR)                                  0601
         L     R7,BUFPTR(,COMONPTR)                                0601
         LA    R14,BUF(R6,R7)                                      0601
         ST    R14,INSRTDA                                         0601
*                   INSRTLN=ADDR(C(M))-INSRTDA;/* GET LENGTH OF DATA */
         L     R1,BUFBASE(,COMONPTR)                               0602
         L     R15,M                                               0602
         LA    R15,C-1(R15,R1)                                     0602
         SLR   R15,R14                                             0602
         ST    R15,INSRTLN                                         0602
*                   CLEN=CLEN-F1;   /* BACK PAST COMMA               */
         BCTR  R6,0                                                0603
         STH   R6,CLEN(,COMONPTR)                                  0603
*                   TYPE=F0;        /* ALLOW ONLY NUMERIC            */
         SLR   R6,R6                                               0604
         ST    R6,TYPE(,COMONPTR)                                  0604
*                   RFY                                            0605
*                    (R15) RSTD;    /* RESTRICT R15 FOR RETURN CODE  */
*                   CALL IKJCT434(BUF,CLEN,TYPE,OUTA1);/* PROCESS  0606
*                                      NUMBER 2                      */
         ST    R7,@AL00001                                         0606
         LA    R14,CLEN(,COMONPTR)                                 0606
         ST    R14,@AL00001+4                                      0606
         LA    R14,TYPE(,COMONPTR)                                 0606
         ST    R14,@AL00001+8                                      0606
         LA    R14,OUTA1                                           0606
         ST    R14,@AL00001+12                                     0606
         L     R15,IKJCT434                                        0606
         LA    R1,@AL00001                                         0606
         BALR  R14,R15                                             0606
*                   ERROR=R15;      /* SAVE ERROR CODE               */
         ST    R15,ERROR                                           0607
*                   BUF(F1:F2)=CSAVE;/* RESTORE OLD BUFFER           */
         L     R14,BUFPTR(,COMONPTR)                               0608
         MVC   BUF(2,R14),CSAVE                                    0608
*                   RFY                                            0609
*                    (R15) UNRSTD;  /* RELEASE R15                   */
*                   IF ERROR=F0 THEN/* IF OKAY                       */
         L     R15,ERROR                                           0610
         LTR   R15,R15                                             0610
         BNZ   @RF00610                                            0610
*                     DO;           /* CHECK FOR VALID RANGE         */
*                       CALL CONVERT;/* CONVERT NUMBER 2 TO USABLE   */
         BAL   R14,CONVERT                                         0612
*                       IF ERROR=F0&V1>V2 THEN/* FORM AND CHECK RANGE*/
         L     R0,ERROR                                            0613
         LTR   R0,R0                                               0613
         BNZ   @RF00613                                            0613
         L     R1,V1                                               0613
         C     R1,V2                                               0613
         BNH   @RF00613                                            0613
*                         ERROR=E912;/* SET ERROR CODE               */
         MVC   ERROR(4),FW912                                      0614
*                     END;          /*                               */
@RF00613 DS    0H                                                  0616
*                 END;                                             0616
@RF00610 DS    0H                                                  0617
*               IF ERROR=F0 THEN    /* IF EVERYTHING STILL OK        */
@RF00594 SLR   R2,R2                                               0617
         C     R2,ERROR                                            0617
         BNE   @RF00617                                            0617
*                 IF V2>INSRTLN V1=F0 THEN/* IF OUTSIDE RANGE, SET 0618
*                                      ERROR                         */
         L     R3,V2                                               0618
         C     R3,INSRTLN                                          0618
         BH    @RT00618                                            0618
         C     R2,V1                                               0618
         BNE   @RF00618                                            0618
@RT00618 DS    0H                                                  0619
*                   ERROR=E932;     /* INDICATOR                     */
         MVC   ERROR(4),FW932                                      0619
*                 ELSE              /*                               */
*                   DO;             /* SET UP INSERT INFO            */
         B     @RC00618                                            0620
@RF00618 DS    0H                                                  0621
*                     INSRTLN=V2-V1+F1;/* LENGTH OF INSERT DATA      */
         LA    R4,1                                                0621
         L     R5,V1                                               0621
         L     R6,V2                                               0621
         SLR   R6,R5                                               0621
         ALR   R6,R4                                               0621
         ST    R6,INSRTLN                                          0621
*                     INSRTDA=INSRTDA+V1-F1;/* POINTER TO INSERT DATA*/
         AL    R5,INSRTDA                                          0622
         BCTR  R5,0                                                0622
         ST    R5,INSRTDA                                          0622
*                     DO I=1 TO INSRTLN;/* NUMBER OF CHARS           */
         B     @DE00623                                            0623
@DL00623 DS    0H                                                  0624
*                       TR(INSRTCHR(I),TRTABDN);/* FOLD TO SPCL CHRS */
         L     R1,I(,COMONPTR)                                     0624
         L     R2,INSRTDA                                          0624
         ALR   R2,R1                                               0624
         BCTR  R2,0                                                0624
         TR    INSRTCHR(1,R2),TRTABDN                              0624
*                     END;          /*                               */
         LA    R4,1                                                0625
         AL    R4,I(,COMONPTR)                                     0625
@DE00623 ST    R4,I(,COMONPTR)                                     0625
         C     R4,INSRTLN                                          0625
         BNH   @DL00623                                            0625
*                     I=M+F1;       /* POINT I TO RESUME DATA        */
         LA    R5,1                                                0626
         AL    R5,M                                                0626
         ST    R5,I(,COMONPTR)                                     0626
*                   END;            /*                               */
*             END;                  /*                               */
*         END;                      /*                               */
*     END;                          /*                               */
*   END SUBSTRTN;                   /*                               */
@EL00008 L     R13,4(,R13)                                         0631
@EF00008 DS    0H                                                  0631
@ER00008 LM    R14,R12,12(R13)                                     0631
         BR    R14                                                 0631
*                                                                  0632
*   /*****************************************************************/
*   /*                                                               */
*   /* WAIT ROUTINE                                                  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0632
*WAITRTN:                                                          0632
*   PROC;                                                          0632
WAITRTN  STM   R14,R12,12(R13)                                     0632
*   PCTR=F1;                        /* INIT PAREN COUNTER            */
         MVC   PCTR(4),FW1                                         0633
*   SENTINAL=NO;                    /* CLEAR SWITCH                  */
*   FOLD=YES;                       /* INDICATE POSSIBLE FOLD        */
         OI    FOLD(COMONPTR),B'00010000'                          0635
         NI    SENTINAL(COMONPTR),B'01111111'                      0635
*   M=I;                            /* START OF PSTRING SCAN         */
         L     R6,I(,COMONPTR)                                     0636
         ST    R6,M                                                0636
*   IF M>LL-(FREEAMT+F4) (C(M)^=LPAREN&/* PAST BFR OR NO '('         */
*       C(M)^=SENTNLL) THEN         /* OR LEFT SENTINAL              */
         L     R7,BUFBASE(,COMONPTR)                               0637
         LA    R14,4                                               0637
         AL    R14,FREEAMT(,COMONPTR)                              0637
         LCR   R14,R14                                             0637
         AH    R14,LL(,R7)                                         0637
         CR    R6,R14                                              0637
         BH    @RT00637                                            0637
         ALR   R6,R7                                               0637
         CLI   C-1(R6),C'('                                        0637
         BE    @RF00637                                            0637
         L     R6,M                                                0637
         ALR   R7,R6                                               0637
         CLI   C-1(R7),X'01'                                       0637
         BE    @RF00637                                            0637
@RT00637 DS    0H                                                  0638
*     ERROR=E936;                   /* SET ERROR CODE                */
         MVC   ERROR(4),FW936                                      0638
*   ELSE                            /* OTHERWISE                     */
*     DO;                           /* FIND END OF BIF               */
         B     @RC00637                                            0639
@RF00637 DS    0H                                                  0640
*       IF C(M)=SENTNLL THEN        /* IF SENTINAL PROCESSING        */
         L     R1,BUFBASE(,COMONPTR)                               0640
         AL    R1,M                                                0640
         CLI   C-1(R1),X'01'                                       0640
         BNE   @RF00640                                            0640
*         SENTINAL=YES;             /* SETIN SWITCH                  */
         OI    SENTINAL(COMONPTR),B'10000000'                      0641
*       ELSE                        /* OTHERWISE                     */
*         C(M)=SENTNLL;             /* FLAG AS SENTINAL PROCESS      */
         B     @RC00640                                            0642
@RF00640 L     R1,BUFBASE(,COMONPTR)                               0642
         AL    R1,M                                                0642
         MVI   C-1(R1),X'01'                                       0642
*       DO M=M+F1 TO LL-(F4+FREEAMT) WHILE PCTR>F0;/* FIND END OF BF */
@RC00640 LA    R2,1                                                0643
         AL    R2,M                                                0643
         B     @DE00643                                            0643
@DL00643 L     R3,PCTR                                             0643
         LTR   R3,R3                                               0643
         BNP   @DC00643                                            0643
*         IF M<LL-(F4+FREEAMT)&C(M:M+F1)=DAMPSAND THEN/* NOT D AMP   */
         L     R3,BUFBASE(,COMONPTR)                               0644
         LA    R4,4                                                0644
         AL    R4,FREEAMT(,COMONPTR)                               0644
         LCR   R4,R4                                               0644
         AH    R4,LL(,R3)                                          0644
         CR    R2,R4                                               0644
         BNL   @RF00644                                            0644
         ALR   R3,R2                                               0644
         CLC   C-1(2,R3),$DBLAMPR                                  0644
         BNE   @RF00644                                            0644
*           M=M+F1;                 /* SKIP OVER                     */
         AL    R2,FW1                                              0645
         ST    R2,M                                                0645
*         ELSE                      /* CHECK IF A SINGLE AMPERSAND   */
*           IF C(M)=AMPSAND THEN    /* IF IT IS THEN                 */
         B     @RC00644                                            0646
@RF00644 L     R1,BUFBASE(,COMONPTR)                               0646
         AL    R1,M                                                0646
         CLI   C-1(R1),C'&&'                                       0646
         BNE   @RF00646                                            0646
         TM    LCLFLAGS,$#NRSTR     PROCESSING &NRSTR?          ZP60014
         BNO   NORMWAIT             NO, SCAN AS NORMAL          ZP60014
         CLC   Z,FW1                ON FIRST SCAN?              ZP60014
         BNH   NORMWAIT             YES, SCAN AS NORMAL         ZP60014
         CLC   C(5,R1),$NRSTR       REMOVING &NRSTR?            ZP60014
         BNE   @RF00646             NO, SKIP SUBSTITUTION       ZP60014
NORMWAIT EQU   *                    DO NORMAL SUBSTITUTION      ZP60014
*             WAIT=YES;             /* SET WAIT SWITCH               */
         OI    WAIT(COMONPTR),B'00010000'                          0647
*           ELSE                    /* OTHERWISE                     */
*             IF C(M)=SENTNLR THEN  /* IF A SENTINAL                 */
         B     @RC00646                                            0648
@RF00646 L     R2,M                                                0648
         L     R1,BUFBASE(,COMONPTR)                               0648
         ALR   R1,R2                                               0648
         CLI   C-1(R1),X'02'                                       0648
         BNE   @RF00648                                            0648
*               DO;                 /* CHECK IF SENTINAL MODE        */
*                 PCTR=F0;          /* TERMINATE SCAN                */
         SLR   R3,R3                                               0650
         ST    R3,PCTR                                             0650
*                 IF SENTINAL=NO THEN/* IF IN SENTINAL MODE          */
         TM    SENTINAL(COMONPTR),B'10000000'                      0651
         BNZ   @RF00651                                            0651
*                   M=M-F1;         /* DONT COUNT SENTINAL           */
         BCTR  R2,0                                                0652
         ST    R2,M                                                0652
*               END;                /* IF A CLOSE PAREN              */
*             ELSE                  /* IF NOT SENTINAL               */
*               IF SENTINAL=NO THEN /* IF IN SENTINAL MODE           */
         B     @RC00648                                            0654
@RF00648 TM    SENTINAL(COMONPTR),B'10000000'                      0654
         BNZ   @RF00654                                            0654
*                 DO;               /* SKIP PAREN CHECK              */
*                   IF C(M)=RPAREN THEN/* IF A CLOSE PAREN           */
         L     R4,M                                                0656
         L     R5,BUFBASE(,COMONPTR)                               0656
         ALR   R4,R5                                               0656
         CLI   C-1(R4),C')'                                        0656
         BNE   @RF00656                                            0656
*                     DO;           /* DECREMENT COUNT               */
*                       PCTR=PCTR-F1;/*                              */
         L     R4,PCTR                                             0658
         BCTR  R4,0                                                0658
         ST    R4,PCTR                                             0658
*                       IF PCTR=F0 THEN/* IF PAREN COUNT IS ZERO     */
         LTR   R4,R4                                               0659
         BNZ   @RF00659                                            0659
*                         C(M)=SENTNLR;/* PAREN TO SENTINAL          */
         L     R4,M                                                0660
         ALR   R5,R4                                               0660
         MVI   C-1(R5),X'02'                                       0660
*                     END;          /*                               */
*                   ELSE            /* OTHERWISE                     */
*                     IF C(M)=LPAREN THEN/* IF AN OPEN PAREN         */
         B     @RC00656                                            0662
@RF00656 L     R1,BUFBASE(,COMONPTR)                               0662
         AL    R1,M                                                0662
         CLI   C-1(R1),C'('                                        0662
         BNE   @RF00662                                            0662
*                       PCTR=PCTR+F1;/* INCREMENT THE PAREN COUNT    */
         LA    R2,1                                                0663
         AL    R2,PCTR                                             0663
         ST    R2,PCTR                                             0663
*                 END;              /*                               */
@RF00662 DS    0H                                                  0664
@RC00656 DS    0H                                                  0665
*       END;                        /*                               */
@RF00654 DS    0H                                                  0665
@RC00648 DS    0H                                                  0665
@RC00646 DS    0H                                                  0665
@RC00644 LA    R2,1                                                0665
         AL    R2,M                                                0665
@DE00643 ST    R2,M                                                0665
         L     R3,BUFBASE(,COMONPTR)                               0665
         LA    R4,4                                                0665
         AL    R4,FREEAMT(,COMONPTR)                               0665
         LCR   R4,R4                                               0665
         AH    R4,LL(,R3)                                          0665
         CR    R2,R4                                               0665
         BNH   @DL00643                                            0665
@DC00643 DS    0H                                                  0666
*     END;                          /*                               */
*   LEN=M-V;                        /* ADJUST LEN FOR WHOLE BIF      */
@RC00637 L     R5,M                                                0667
         LR    R4,R5                                               0667
         SL    R4,V                                                0667
         ST    R4,LEN(,COMONPTR)                                   0667
*   IF PCTR=F0 THEN                 /* IF CLOSE PAREN FOUND          */
         L     R4,PCTR                                             0668
         LTR   R4,R4                                               0668
         BNZ   @RF00668                                            0668
*     M=M-F1;                       /* DECREMENT M                   */
         BCTR  R5,0                                                0669
         ST    R5,M                                                0669
*   END WAITRTN;                                                   0670
@EL00009 DS    0H                                                  0670
@EF00009 DS    0H                                                  0670
@ER00009 LM    R14,R12,12(R13)                                     0670
         BR    R14                                                 0670
*                                                                  0671
*   /*****************************************************************/
*   /*                                                               */
*   /* CONVERT ROUTINE                                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0671
*CONVERT:                                                          0671
*   PROC;                                                          0671
CONVERT  STM   R14,R12,12(R13)                                     0671
*   DCL                                                            0672
*     NDX FIXED(31);                /* INDEX TO ANSWER               */
*   AREA=ZEROS;                     /* PLACE ZEROS IN AREA           */
         MVC   AREA(16),$16ZEROS                                   0673
*   IF ANSWER(F1)=MINUS THEN        /* IF ANSWER IS NEGATIVE         */
         L     R1,ANSPTR(,COMONPTR)                                0674
         CLI   ANSWER(R1),C'-'                                     0674
         BNE   @RF00674                                            0674
*     ERCOM=E920;                   /* SET ERROR CODE                */
         MVC   ERCOM(4,COMONPTR),FW920                             0675
*   ELSE                            /* OTHERWISE OK                  */
*     DO;                           /* BUILD PACKAREA                */
         B     @RC00674                                            0676
@RF00674 DS    0H                                                  0677
*       IF CLEN<F1 CLEN>F16 THEN    /* IF ANS NOT NUMERIC OR NULL    */
         LH    R2,CLEN(,COMONPTR)                                  0677
         LTR   R2,R2                                               0677
         BNP   @RT00677                                            0677
         C     R2,FW16                                             0677
         BNH   @RF00677                                            0677
@RT00677 DS    0H                                                  0678
*         ERCOM=E924;               /* SET ERROR CODE                */
         MVC   ERCOM(4,COMONPTR),FW924                             0678
*       ELSE                        /* IF OK THEN MOVE INTO AREA     */
*         AREA(F17-CLEN:F16)=ANSWER(F1:CLEN);/* MOVE IN VARIABLE   0679
*                                      ANSWER                        */
         B     @RC00677                                            0679
@RF00677 LA    R3,17                                               0679
         SH    R3,CLEN(,COMONPTR)                                  0679
         LA    R2,AREA-1(R3)                                       0679
         LCR   R3,R3                                               0679
         AL    R3,FW16                                             0679
         L     R1,ANSPTR(,COMONPTR)                                0679
         EX    R3,@SM01104                                         0679
*       DO NDX=F16 TO F1 BY-F1 WHILE ERCOM=F0;/* CHECK FOR ALPHA   0680
*                                      ANSWER                        */
@RC00677 LA    R2,16                                               0680
         ST    R2,NDX                                              0680
@DL00680 L     R3,ERCOM(,COMONPTR)                                 0680
         LTR   R3,R3                                               0680
         BNZ   @DC00680                                            0680
*         IF AREA(NDX)<CC0 AREA(NDX)>CC9 THEN/*                      */
         LA    R3,AREA-1(R2)                                       0681
         CLI   0(R3),C'0'                                          0681
         BL    @RT00681                                            0681
         LA    R3,AREA-1(R2)                                       0681
         CLI   0(R3),C'9'                                          0681
         BNH   @RF00681                                            0681
@RT00681 DS    0H                                                  0682
*           ERCOM=E916;             /* SET ERROR CODE                */
         MVC   ERCOM(4,COMONPTR),FW916                             0682
*       END;                                                       0683
@RF00681 SLR   R2,R2                                               0683
         BCTR  R2,0                                                0683
         AL    R2,NDX                                              0683
         ST    R2,NDX                                              0683
         LTR   R2,R2                                               0683
         BP    @DL00680                                            0683
@DC00680 DS    0H                                                  0684
*       IF ERCOM=F0 THEN            /* IF NO ERROR SO FAR            */
         L     R3,ERCOM(,COMONPTR)                                 0684
         LTR   R3,R3                                               0684
         BNZ   @RF00684                                            0684
*         DO;                       /* PACK INTO USABLE FORM         */
*           PACK(WORKAREA,AREA);    /* ISSUE PACK                    */
         PACK  WORKAREA(8),AREA(16)                                0686
*           CVB(V2,WORKAREA);       /* ISSUE CONVERT TO BINARY       */
         CVB   R4,WORKAREA                                         0687
         ST    R4,V2                                               0687
*           CNVTANS=V2;             /* PLACE IN COMMON AREA          */
         ST    R4,CNVTANS(,COMONPTR)                               0688
*         END;                      /*                               */
*     END;                                                         0690
@RF00684 DS    0H                                                  0691
*   ERROR=ERCOM;                    /* SAVE ERROR FOR INT USE        */
@RC00674 L     R6,ERCOM(,COMONPTR)                                 0691
         ST    R6,ERROR                                            0691
*   END CONVERT;                                                   0692
@EL00010 DS    0H                                                  0692
@EF00010 DS    0H                                                  0692
@ER00010 LM    R14,R12,12(R13)                                     0692
         BR    R14                                                 0692
*                                                                  0693
*   /*****************************************************************/
*   /*                                                               */
*   /* IMMEDIATE VALUE NEEDED                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0693
*IMVALRTN:                                                         0693
*   PROC;                                                          0693
IMVALRTN STM   R14,R12,@SA00011                                    0693
*   INDEX=F0;                       /* INIT INDEX                    */
         SLR   R7,R7                                               0694
         ST    R7,INDEX                                            0694
*   DO N=1 TO DIM(IMMDATA) WHILE INDEX=F0;/* FIND BUILT IN FUNCTION  */
         LA    R14,1                                               0695
         ST    R14,N                                               0695
@DL00695 L     R15,INDEX                                           0695
         LTR   R15,R15                                             0695
         BNZ   @DC00695                                            0695
*     IF IMMLEN(N)=SNTLNG&          /* SAME LENGTH                   */
*         IMMDATA(N,1:SNTLNG)=C(V+F1:V+SNTLNG) THEN/* SAME TEXT      */
         LR    R15,R14                                             0696
         SLA   R15,4                                               0696
         L     R1,SNTELPTR(,COMONPTR)                              0696
         LH    R2,SNTLNG(,R1)                                      0696
         CH    R2,IMMLEN-16(R15)                                   0696
         BNE   @RF00696                                            0696
         L     R1,BUFBASE(,COMONPTR)                               0696
         BCTR  R2,0                                                0696
         LA    R3,IMMDATA-16(R15)                                  0696
         AL    R1,V                                                0696
         EX    R2,@SC01106                                         0696
         BNE   @RF00696                                            0696
*       INDEX=N;                    /* SET INDEX TO ROUTINE          */
         ST    R14,INDEX                                           0697
*   END;                                                           0698
@RF00696 LA    R14,1                                               0698
         AL    R14,N                                               0698
         ST    R14,N                                               0698
         C     R14,INDX#MAX                               ZP60014  0698
         BNH   @DL00695                                            0698
@DC00695 DS    0H                                                  0699
*   IF INDEX=F0 THEN                /* IF NO MATCH                   */
         L     R15,INDEX                                           0699
         LTR   R15,R15                                             0699
         BNZ   @RF00699                                            0699
*     ERROR=E999;                   /* SET ERROR CODE                */
         MVC   ERROR(4),FW999                                      0700
*   ELSE                            /* CHECK FOR &STR                */
*     CALL ROUTINE;                 /* PROCESS IMVAL                 */
         B     @RC00699                                            0701
@RF00699 L     R0,INDEX                                            0701
         SLA   R0,4                                                0701
         LR    R1,R0
         L     R15,VECTOR-16(R1)                                   0701
         BALR  R14,R15                                             0701
*   END;                                                           0702
@EL00011 DS    0H                                                  0702
@EF00011 DS    0H                                                  0702
@ER00011 LM    R14,R12,@SA00011                                    0702
         BR    R14                                                 0702
*                                                                  0703
*   /*****************************************************************/
*   /*                                                               */
*   /* TIME BUILT IN FUNCTION                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0703
*TIMERTN:                                                          0703
*   PROC;                                                          0703
TIMERTN  STM   R14,R12,@SA00012                                    0703
         LA    R2,IMMDATA-16(R1)    POINT TO FUNCTION NAME      ZP60014
*   RFY                                                            0704
*    (R0) RSTD;                     /* RESTRICT TIME REGISTER        */
*   GEN SETS(R0)(TIME DEC);         /* ISSUE TIME MACRO              */
         TIME  DEC
*   TEMPA=R0;                       /* SAVE TIME OF DAY              */
         ST    R0,TEMPA                                            0706
*   RFY                                                            0707
*    (R0) UNRSTD;                   /* RELEASE REGISTER              */
*   UNPK(TIMEOUT,TEMPA);            /* UNPACK TIME                   */
         UNPK  TIMEOUT(7),TEMPA(4)                                 0708
*   OUT1=HRS;                       /* HOURS                         */
         MVC   OUT1(2),HRS                                         0709
*   OUT2=MINUTES;                   /* MINUTES                       */
         MVC   OUT2(2),MINUTES                                     0710
*   OUT3=SEC;                       /* SECONDS                       */
         MVC   OUT3(2),SEC                                         0711
*   DELIM1=COLON;                   /* SEPARATE TIME ELEMENTS WITH   */
         MVI   DELIM1,C':'                                         0712
*   DELIM2=COLON;                   /* A COLON                       */
         MVI   DELIM2,C':'                                         0713
*   INSRTDA=ADDR(TIMEDATE);         /* POINT TO INSERT DATA          */
         LA    R0,TIMEDATE                                         0714
         ST    R0,INSRTDA                                          0714
*   INSRTLN=LENGTH(TIMEDATE);       /* SET LENGTH OF INSERT DATA     */
         MVC   INSRTLN(4),FW8                                      0715
         CLI   3(R2),C'S'           SYSSTIME?                   ZP60014
         BNE   @EL00012             NO, REALLY IS SYSTIME       ZP60014
         MVC   INSRTLN(4),FW5       YES, SET CORRECT LENGTH     ZP60014
*   END TIMERTN;                                                   0716
@EL00012 DS    0H                                                  0716
@EF00012 DS    0H                                                  0716
@ER00012 LM    R14,R12,@SA00012                                    0716
         BR    R14                                                 0716
*                                                                  0717
*   /*****************************************************************/
*   /*                                                               */
*   /* DATE BUILT IN FUNCTION                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0717
*DATERTN:                                                          0717
*   PROC;                                                          0717
DATERTN  STM   R14,R12,@SA00013                                    0717
         LA    R2,IMMDATA-16(R1)    POINT TO FUNCTION NAME      ZP60014
*   RFY                                                            0718
*    (R1) RSTD;                     /* RESTRICT DATE REG             */
*   GEN SETS(R1)(TIME DEC);         /* ISSUE TIME MACRO              */
         TIME  DEC
         LR    R14,R2               POINT TO FUNCTION NAME      ZP60014
*   TEMPA=R1;                       /* SAVE DATE                     */
         ST    R1,TEMPA                                            0720
         AP    TEMPA(4),YRS1900     CONVERT TO 4-DIGIT YEAR     ZP60014
         OI    TEMPA+3,X'0F'        RESTORE SIGN                ZP60014
         CLI   4(R14),C'J'          SYS4JDATE?                  ZP60014
         BE    ISJULIAN             YES                         ZP60014
         CLI   3(R14),C'J'          SYSJDATE?                   ZP60014
         BNE   NTJULIAN             NO                          ZP60014
ISJULIAN UNPK  TIMEDATE+1(7),TEMPA(4)                           ZP60014
         MVC   TIMEDATE(4),TIMEDATE+1                           ZP60014
         MVI   TIMEDATE+4,C'.'      GET YYYY.DDD                ZP60014
         LA    R1,TIMEDATE          POINT TO DATE DATA          ZP60014
         LA    R0,8                 GET DATE DATA LENGTH        ZP60014
         CLI   3(R14),C'4'          4-DIGIT YEAR REQUIRED?      ZP60014
         BE    HAVEDATE             YES                         ZP60014
         LA    R1,TIMEDATE+2        POINT TO DATE DATA          ZP60014
         LA    R0,6                 GET DATE DATA LENGTH        ZP60014
HAVEDATE ST    R1,INSRTDA           SET DATA ADDRESS            ZP60014
         ST    R0,INSRTLN           SET DATA LENGTH             ZP60014
         B     @EL00013                                         ZP60014
*                                                               ZP60014
NTJULIAN EQU   *                    NON-JULIAN DATE PROCESSING  ZP60014
*   RFY                                                            0721
*    (R1) UNRSTD;                   /* RELEASE DATE REGISTER         */
*   UNPK(DATEOUT,TEMPA);            /* UNPACK DATE                   */
         UNPK  DATEOUT(7),TEMPA(4)                        ZP60014  0722
*   PACK(WORKAREA,YR);              /* CONVERT YEAR TO USEABLE FORM  */
         PACK  WORKAREA(8),YR(4)                          ZP60014  0723
*   CVB(YEAR,WORKAREA);             /*                               */
         CVB   R1,WORKAREA                                         0724
         ST    R1,YEAR                                             0724
*   PACK(WORKAREA,DYS);             /* CONVERT DAYS TO USEABLE FORM  */
         PACK  WORKAREA(8),DYS(3)                                  0725
*   CVB(DAYS,WORKAREA);             /*                               */
         CVB   R0,WORKAREA                                         0726
         ST    R0,DAYS                                             0726
*   IF YEAR//F4=F0 THEN             /* IF A LEAP YEAR, SET LEAP      */
         LR    R2,R1                                               0727
         SRDA  R2,32                                               0727
         D     R2,FW4                                              0727
         LTR   R2,R2                                               0727
         BNZ   @RF00727                                            0727
*     LEAP=YES;                     /* YEAR SWITCH                   */
         OI    LEAP(COMONPTR),B'00000100'                          0728
*   ELSE                            /* IF NOT, TURN IT OFF           */
*     LEAP=NO;                      /*                               */
         B     @RC00727                                            0729
@RF00727 NI    LEAP(COMONPTR),B'11111011'                          0729
*   DO MONTH=1 TO DIM(DAYIN) WHILE DAYS>F0;/* SUBTRACT DAYS OF EACH
*                                      MONTH                         */
@RC00727 LA    R1,1                                                0730
         ST    R1,MONTH                                            0730
@DL00730 L     R2,DAYS                                             0730
         LTR   R2,R2                                               0730
         BNP   @DC00730                                            0730
*     DAYS=DAYS-DAYIN(MONTH);       /* UNTIL DAYS GOES NEGATIVE      */
         LR    R3,R1                                               0731
         ALR   R3,R3                                               0731
         SH    R2,DAYIN-2(R3)                                      0731
         ST    R2,DAYS                                             0731
*     IF MONTH=2&LEAP=YES THEN      /* IF LEAP YEAR, ADJUST FOR      */
         C     R1,FW2                                              0732
         BNE   @RF00732                                            0732
         TM    LEAP(COMONPTR),B'00000100'                          0732
         BNO   @RF00732                                            0732
*       DAYS=DAYS-F1;               /* EXTRA DAY IN FEBRUARY         */
         BCTR  R2,0                                                0733
         ST    R2,DAYS                                             0733
*   END;                            /*                               */
@RF00732 LA    R1,1                                                0734
         AL    R1,MONTH                                            0734
         ST    R1,MONTH                                            0734
         C     R1,FW12                                             0734
         BNH   @DL00730                                            0734
@DC00730 DS    0H                                                  0735
*   MONTH=MONTH-F1;                 /* ADJUST MONTH                  */
         L     R2,MONTH                                            0735
         BCTR  R2,0                                                0735
         ST    R2,MONTH                                            0735
*   DAYS=DAYS+DAYIN(MONTH);         /* ADD BACK DAYS                 */
         LR    R3,R2                                               0736
         ALR   R3,R3                                               0736
         LH    R3,DAYIN-2(R3)                                      0736
         AL    R3,DAYS                                             0736
         ST    R3,DAYS                                             0736
*   IF LEAP=YES&MONTH=FEBRUARY THEN /* IF LEAP YEAR, READJUST DAYS   */
         TM    LEAP(COMONPTR),B'00000100'                          0737
         BNO   @RF00737                                            0737
         C     R2,FW2                                              0737
         BNE   @RF00737                                            0737
*     DAYS=DAYS+F1;                 /*                               */
         AL    R3,FW1                                              0738
         ST    R3,DAYS                                             0738
*   CVD(MONTH,WORKAREA);            /* CONVERT MONTH AND DAY TO      */
@RF00737 L     R4,MONTH                                            0739
         CVD   R4,WORKAREA                                         0739
*   UNPK(OUT1,WORKAREA);            /* PRINTABLE FORM                */
         UNPK  OUT1(2),WORKAREA(8)                                 0740
*   OUT1(2)=OUT1(2) CC0;            /* INSURE PRINTABLE CHAR         */
         OI    OUT1+1,C'0'                                         0741
*   CVD(DAYS,WORKAREA);             /* DITTO FOR DAYS                */
         L     R5,DAYS                                             0742
         CVD   R5,WORKAREA                                         0742
*   UNPK(OUT2,WORKAREA);            /*                               */
         UNPK  OUT2(2),WORKAREA(8)                                 0743
*   OUT2(2)=OUT2(2) CC0;            /*                               */
         OI    OUT2+1,C'0'                                         0744
*   OUT3=YR;                        /* GET YEAR                      */
         MVC   OUT3(4),YR                                          0745
*   DELIM1=SLASH;                   /* SET DATE DELIMITERS TO SLASH  */
         MVI   DELIM1,C'/'                                         0746
*   DELIM2=SLASH;                   /*                               */
         MVI   DELIM2,C'/'                                         0747
*   INSRTDA=ADDR(TIMEDATE);         /* POINT TO INSERT DATA          */
         LA    R6,TIMEDATE                                         0748
         ST    R6,INSRTDA                                          0748
         MVC   INSRTLN(4),FW10      PREPARE FOR 4-DIGIT YEAR    ZP60014
         CLI   3(R14),C'4'          4-DIGIT YEAR REQUESTED?     ZP60014
         BNE   YEAR2DIG             NO                          ZP60014
         CLI   4(R14),C'D'          SYS4DATE?                   ZP60014
         BE    @EL00013             YES, ALL DONE               ZP60014
         MVC   TIMEDATE+5(5),TIMEDATE    MOVE MM/DD             ZP60014
         MVC   TIMEDATE(4),YR       SUPPLY 4-DIGIT YEAR         ZP60014
         MVI   TIMEDATE+4,C'/'      SUPPLY DELIMITER            ZP60014
         CLI   4(R14),C'S'          SYS4SDATE?                  ZP60014
         BE    @EL00013             YES, ALL DONE               ZP60014
         MVI   TIMEDATE+4,C'-'      NO, SUPPLY DELIMITERS       ZP60014
         MVI   TIMEDATE+7,C'-'          FOR SYS4IDATE           ZP60014
         B     @EL00013             ALL DONE                    ZP60014
YEAR2DIG EQU   *                                                ZP60014
         MVC   OUT3(2),OUT3+2       REMOVE CENTURY              ZP60014
*   INSRTLN=LENGTH(TIMEDATE);       /* SET INSERT DATA LENGTH        */
         MVC   INSRTLN(4),FW8                                      0749
         CLI   3(R14),C'D'          SYSDATE?                    ZP60014
         BE    @EL00013             YES, ALL DONE               ZP60014
         MVC   OUT3,OUT2            SHUFFLE DD RIGHT            ZP60014
         MVC   OUT2,OUT1            SHUFFLE MM RIGHT            ZP60014
         MVC   OUT1,YR+2            RELOAD YY - SYSSDATE DONE   ZP60014
*   END DATERTN;                                                   0750
@EL00013 DS    0H                                                  0750
@EF00013 DS    0H                                                  0750
@ER00013 LM    R14,R12,@SA00013                                    0750
         BR    R14                                                 0750
*                                                               ZP60014
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* TERMID BUILT IN FUNCTION                             ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*
TERMRTN  STM   R14,R12,12(R13)                                  ZP60014
         LA    R2,IMMDATA-16(R1)    POINT TO FUNCTION NAME           "
         GTSIZE
         CLI   3(R2),C'L'           SYSLTERM?                        "
         BE    HAVETERM             YES                              "
         LR    R0,R1                NO, MUST BE SYSWTERM             "
HAVETERM CVD   R0,WORKAREA          GET DIMENSION IN DECIMAL         "
         OI    WORKAREA+7,X'0F'     PREPARE FOR UNPACK               "
         UNPK  TIMEDATE(3),WORKAREA GET DIMENSION IN NUMERIC CHARS   "
         LA    R1,TIMEDATE          POINT TO DATA                    "
         LA    R2,3                 GET LENGTH OF DATA               "
         LA    R0,2                 GET MAXIMUM LEADING ZERO COUNT   "
TERMZERO CLI   0(R1),C'0'           LEADING ZERO?                    "
         BNE   TERMOKAY             NO, ALL READY                    "
         LA    R1,1(,R1)            YES, POINT PAST IT               "
         BCTR  R2,0                 DECREMENT LENGTH                 "
         BCT   R0,TERMZERO          CHECK FOR ANOTHER                "
TERMOKAY ST    R1,INSRTDA           POINT TO INSERT DATA             "
         ST    R2,INSRTLN           SET LENGTH OF INSERT DATA        "
         LM    R14,R12,12(R13)                                       "
         BR    R14                                              ZP60014
*                                                               ZP60014
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* CPU BUILT IN FUNCTION                                ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*
CPURTN   STM   R14,R12,12(R13)                                  ZP60014
         L     R2,PSAAOLD           POINT TO CURRENT ASCB            "
         LM    R0,R1,ASCBEJST-ASCB(R2)                               "
         SRDL  R0,12                GET TCB TIME IN MICROSECONDS     "
         D     R0,FW10K             CONVERT TO CENTISECONDS          "
         CVD   R1,WORKAREA          CONVERT TO DECIMAL               "
         OI    WORKAREA+7,X'0F'     PREPARE FOR UNPACK               "
         UNPK  AREA,WORKAREA        CONVERT TO NUMERIC CHARACTERS    "
         MVC   AREA(13),AREA+1      PROMOTE SECONDS                  "
         MVI   AREA+13,C'.'         INSERT DECIMAL POINT             "
         LA    R1,AREA              POINT TO START OF NUMBER         "
         LA    R2,12                GET MAX LEADING ZEROS TO CHOP    "
CPU0LOOP CLI   0(R1),C'0'           LEADING ZERO?                    "
         BNE   CPUTIMOK             NO, ALL READY                    "
         LA    R1,1(,R1)            YES, POINT PAST IT               "
         BCT   R2,CPU0LOOP          CHECK FOR ANOTHER                "
CPUTIMOK LA    R2,4(,R2)            GET DATA LENGTH                  "
         ST    R1,INSRTDA           POINT TO INSERT DATA             "
         ST    R2,INSRTLN           SET LENGTH OF INSERT DATA        "
         LM    R14,R12,12(R13)                                       "
         BR    R14                                              ZP60014
*                                                               ZP60014
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* SRV BUILT IN FUNCTION                                ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*
SRVRTN   STM   R14,R12,12(R13)                                  ZP60014
         LA    R1,AREA              POINT TO THREE WORDS             "
         SYSEVENT REQSERVC          GET ACCUMULATED SERVICE          "
         L     R1,AREA              LOAD JOB/SESSION SERVICE UNITS   "
         CVD   R1,WORKAREA          CONVERT TO DECIMAL               "
         MVC   AREA,ED15            LOAD EDIT MASK                   "
         LA    R1,AREA+15           PREPARE FOR ZERO (UNLIKELY)      "
         EDMK  AREA,WORKAREA        CONVERT TO NUMERIC CHARACTERS    "
         LA    R2,AREA+16           POINT PAST NUMBER                "
         SR    R2,R1                GET LENGTH OF NUMBER             "
         ST    R1,INSRTDA           POINT TO INSERT DATA             "
         ST    R2,INSRTLN           SET LENGTH OF INSERT DATA        "
         LM    R14,R12,12(R13)                                       "
         BR    R14                                              ZP60014
*                                                                  0751
*   /*****************************************************************/
*   /*                                                               */
*   /* LENGTH BUILT IN FUNCTION                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0751
*LENGRTN:                                                          0751
*   PROC;                                                          0751
LENGRTN  STM   R14,R12,@SA00014                                    0751
*   CALL SETUP;                     /* CHECK IF TO WAIT AND CALL     */
         BAL   R14,SETUP                                           0752
*   IF ERROR=F0&WAIT=NO THEN        /* EVALUATE P-STRING             */
         L     R7,ERROR                                            0753
         LTR   R7,R7                                               0753
         BNZ   @RF00753                                            0753
         TM    WAIT(COMONPTR),B'00010000'                          0753
         BNZ   @RF00753                                            0753
*     DO;                           /*                               */
*       CVD(CLEN,WORKAREA);         /* GET LENGTH IN PRINTABLE       */
         LH    R14,CLEN(,COMONPTR)                                 0755
         CVD   R14,WORKAREA                                        0755
*       UNPK(AREA,WORKAREA);        /* FORM FOR TABLE                */
         UNPK  AREA(16),WORKAREA(8)                                0756
*       AREA(16)=AREA(16) CC0;      /* INSURE NO SIGN                */
         OI    AREA+15,C'0'                                        0757
*       DO I=F16 TO F2 BY-F1 WHILE AREA(1)=CC0;/* LEFT JUSTIFY       */
         LA    R15,16                                              0758
         ST    R15,I(,COMONPTR)                                    0758
@DL00758 CLI   AREA,C'0'                                           0758
         BNE   @DC00758                                            0758
*         AREA(1:F15)=AREA(2:F16);  /* SHIFT BUFFER TO LEFT          */
         MVC   @TS00001(15),AREA+1                                 0759
         MVC   AREA(15),@TS00001                                   0759
*       END;                        /*                               */
         SLR   R15,R15                                             0760
         BCTR  R15,0                                               0760
         AL    R15,I(,COMONPTR)                                    0760
         ST    R15,I(,COMONPTR)                                    0760
         C     R15,FW2                                             0760
         BNL   @DL00758                                            0760
@DC00758 DS    0H                                                  0761
*       INSRTDA=ADDR(AREA);         /* POINT TO INSERT LENGTH        */
         LA    R0,AREA                                             0761
         ST    R0,INSRTDA                                          0761
*       INSRTLN=I;                  /* GET LENGTH OF LENGTH          */
         L     R2,I(,COMONPTR)                                     0762
         ST    R2,INSRTLN                                          0762
*       I=M+F1;                     /* POINT TO RESUME DATA          */
         LA    R3,1                                                0763
         AL    R3,M                                                0763
         ST    R3,I(,COMONPTR)                                     0763
*     END;                          /*                               */
*   END LENGRTN;                                                   0765
@EL00014 DS    0H                                                  0765
@EF00014 DS    0H                                                  0765
@ER00014 LM    R14,R12,@SA00014                                    0765
         BR    R14                                                 0765
*                                                                  0766
*   /*****************************************************************/
*   /*                                                               */
*   /* EVAL BUILT IN FUNCTION                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0766
*EVALRTN:                                                          0766
*   PROC;                                                          0766
EVALRTN  STM   R14,R12,@SA00015                                    0766
*   CALL SETUP;                     /* CHECK IF TO WAIT AND PSTR     */
         BAL   R14,SETUP                                           0767
*   IF ERROR=F0&WAIT=NO THEN        /* OK TO PROCESS                 */
         L     R4,ERROR                                            0768
         LTR   R4,R4                                               0768
         BNZ   @RF00768                                            0768
         TM    WAIT(COMONPTR),B'00010000'                          0768
         BNZ   @RF00768                                            0768
*     DO;                           /*                               */
*       INSRTDA=ADDR(ANSWER);       /* ANSWER OF EVALUATION          */
         L     R6,ANSPTR(,COMONPTR)                                0770
         ST    R6,INSRTDA                                          0770
*       INSRTLN=CLEN;               /* LENGTH OF ANSWER              */
         LH    R7,CLEN(,COMONPTR)                                  0771
         ST    R7,INSRTLN                                          0771
*       I=M+F1;                     /* POINT TO RESUME DATA          */
         LA    R14,1                                               0772
         AL    R14,M                                               0772
         ST    R14,I(,COMONPTR)                                    0772
*     END;                          /*                               */
*   END EVALRTN;                    /* END OF EVAL BIF               */
@EL00015 DS    0H                                                  0774
@EF00015 DS    0H                                                  0774
@ER00015 LM    R14,R12,@SA00015                                    0774
         BR    R14                                                 0774
*                                                                  0775
*   /*****************************************************************/
*   /*                                                               */
*   /* DATATYPE ROUTINE                                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0775
*DATYRTN:                                                          0775
*   PROC;                                                          0775
DATYRTN  STM   R14,R12,@SA00016                                    0775
*   ALPHA=NO;                       /* RESET ALPHA SWITCH            */
         NI    ALPHA(COMONPTR),B'11111101'                         0776
*   CALL SETUP;                     /* CHECK TO SEE IF TO WAIT AND   */
         BAL   R14,SETUP                                           0777
*   IF ERROR=F0&WAIT=NO THEN        /* EVALUATE STRING               */
         L     R15,ERROR                                           0778
         LTR   R15,R15                                             0778
         BNZ   @RF00778                                            0778
         TM    WAIT(COMONPTR),B'00010000'                          0778
         BNZ   @RF00778                                            0778
*     DO;                           /*                               */
*       DO I=CLEN TO F1 BY-F1 WHILE ALPHA=NO;/* CHECK IF ANY ALPHA   */
         LH    R0,CLEN(,COMONPTR)                                  0780
         ST    R0,I(,COMONPTR)                                     0780
         B     @DE00780                                            0780
@DL00780 TM    ALPHA(COMONPTR),B'00000010'                         0780
         BNZ   @DC00780                                            0780
*         IF ANSWER(I)<CC0 ANSWER(I)>CC9 THEN/* IF AN ALPHA CHAR     */
         L     R1,I(,COMONPTR)                                     0781
         L     R2,ANSPTR(,COMONPTR)                                0781
         ALR   R1,R2                                               0781
         BCTR  R1,0                                                0781
         CLI   ANSWER(R1),C'0'                                     0781
         BL    @RT00781                                            0781
         L     R3,I(,COMONPTR)                                     0781
         ALR   R2,R3                                               0781
         BCTR  R2,0                                                0781
         CLI   ANSWER(R2),C'9'                                     0781
         BNH   @RF00781                                            0781
@RT00781 DS    0H                                                  0782
*           IF I^=F1 ANSWER(I)^=MINUS THEN/* IF NOT NEGATIVE         */
LBL019A8 LA    R4,1
         CH    R4,CLEN(,COMONPTR)
         BE    @RT00782
         L     R5,I(,COMONPTR)                                     0782
         CR    R5,R4                                               0782
         BNE   @RT00782                                            0782
         L     R1,ANSPTR(,COMONPTR)                                0782
         ALR   R1,R5                                               0782
         BCTR  R1,0                                                0782
         CLI   ANSWER(R1),C'-'                                     0782
         BE    @RF00782                                            0782
@RT00782 DS    0H                                                  0783
*             ALPHA=YES;            /* SET ALPHA SWITCH              */
         OI    ALPHA(COMONPTR),B'00000010'                         0783
*       END;                        /*                               */
@RF00782 DS    0H                                                  0784
@RF00781 SLR   R0,R0                                               0784
         BCTR  R0,0                                                0784
         AL    R0,I(,COMONPTR)                                     0784
         ST    R0,I(,COMONPTR)                                     0784
@DE00780 LTR   R0,R0                                               0784
         BP    @DL00780                                            0784
@DC00780 DS    0H                                                  0785
*       IF ALPHA=YES CLEN=F0 THEN   /* IF DATATYPE IS ALPHA THEN     */
         TM    ALPHA(COMONPTR),B'00000010'                         0785
         BO    @RT00785                                            0785
         LH    R1,CLEN(,COMONPTR)                                  0785
         LTR   R1,R1                                               0785
         BNZ   @RF00785                                            0785
@RT00785 DS    0H                                                  0786
*         DO;                       /* POINT TO 'CHAR' AND ITS LENGTH*/
*           INSRTDA=ADDR(CHR);      /*                               */
         LA    R2,CHR                                              0787
         ST    R2,INSRTDA                                          0787
*           INSRTLN=LENGTH(CHR);    /*                               */
         MVC   INSRTLN(4),FW4                                      0788
*         END;                      /*                               */
*       ELSE                        /* IF NOT ALPHA, POINT INSERT    */
*         DO;                       /* TO 'NUM' AND ITS LENGTH       */
         B     @RC00785                                            0790
@RF00785 DS    0H                                                  0791
*           INSRTDA=ADDR(NUM);      /*                               */
         LA    R3,NUM                                              0791
         ST    R3,INSRTDA                                          0791
*           INSRTLN=LENGTH(NUM);    /*                               */
         MVC   INSRTLN(4),FW3                                      0792
*         END;                      /*                               */
*       I=M+F1;                     /* POINT TO RESUME DATA          */
@RC00785 LA    R4,1                                                0794
         AL    R4,M                                                0794
         ST    R4,I(,COMONPTR)                                     0794
*     END;                          /*                               */
*   END DATYRTN;                                                   0796
@EL00016 DS    0H                                                  0796
@EF00016 DS    0H                                                  0796
@ER00016 LM    R14,R12,@SA00016                                    0796
         BR    R14                                                 0796
*                                                                  0797
*   /*****************************************************************/
*   /*                                                               */
*   /* USERID PREFIX ROUTINE                                         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0797
*PREFRTN:                                                          0797
*   PROC;                                                          0797
PREFRTN  STM   R14,R12,12(R13)                                     0797
*   INSRTDA=ADDR(UPTPREFX);         /* POINT TO PREFIX               */
         L     R5,@PC00001                                         0798
         LA    R4,UPTPREFX-UPT(,R5)                                0798
         ST    R4,INSRTDA                                          0798
*   INSRTLN=UPTPREFL;               /* GET PREFIX LENGTH             */
         SLR   R4,R4                                               0799
         IC    R4,UPTPREFL-UPT(,R5)                                0799
         ST    R4,INSRTLN                                          0799
*   END PREFRTN;                                                   0800
@EL00017 DS    0H                                                  0800
@EF00017 DS    0H                                                  0800
@ER00017 LM    R14,R12,12(R13)                                     0800
         BR    R14                                                 0800
*                                                                  0801
*   /*****************************************************************/
*   /*                                                               */
*   /* SYSPCMD ROUTINE GET SYSTEM PRIMARY COMMAND NAME               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0801
*SYSPRTN:                                                          0801
*   PROC;                                                          0801
SYSPRTN  STM   R14,R12,12(R13)                                     0801
*   INSRTDA=ADDR(ECTPCMD);          /* POINT TO PRIMARY NAME         */
         L     R5,@PC00001+4                                       0802
         LA    R6,ECTPCMD-ECT(,R5)                                 0802
         ST    R6,INSRTDA                                          0802
*   DO INSRTLN=F8 TO F2 BY-F1 WHILE INSRTCHR(INSRTLN)=BLANK;       0803
         LA    R7,8                                                0803
         ST    R7,INSRTLN                                          0803
@DL00803 L     R1,INSRTDA                                          0803
         ALR   R1,R7                                               0803
         BCTR  R1,0                                                0803
         CLI   INSRTCHR(R1),C' '                                   0803
         BNE   @DC00803                                            0803
*   END;                            /* SCAN FOR LAST NON BLANK       */
         BCTR  R7,0                                                0804
         ST    R7,INSRTLN                                          0804
         C     R7,FW2                                              0804
         BNL   @DL00803                                            0804
@DC00803 DS    0H                                                  0805
*   END SYSPRTN;                                                   0805
@EL00018 DS    0H                                                  0805
@EF00018 DS    0H                                                  0805
@ER00018 LM    R14,R12,12(R13)                                     0805
         BR    R14                                                 0805
*                                                                  0806
*   /*****************************************************************/
*   /*                                                               */
*   /* SYSSCMD ROUTINE GET SYSTEM SECONDARY COMMAND NAME             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0806
*SYSSRTN:                                                          0806
*   PROC;                                                          0806
SYSSRTN  STM   R14,R12,12(R13)                                     0806
*   INSRTDA=ADDR(ECTSCMD);          /* POINT TO SECONDARY NAME       */
         L     R2,@PC00001+4                                       0807
         LA    R3,ECTSCMD-ECT(,R2)                                 0807
         ST    R3,INSRTDA                                          0807
*   DO INSRTLN=F8 TO F2 BY-F1 WHILE INSRTCHR(INSRTLN)=BLANK;       0808
         LA    R4,8                                                0808
         ST    R4,INSRTLN                                          0808
@DL00808 L     R1,INSRTDA                                          0808
         ALR   R1,R4                                               0808
         BCTR  R1,0                                                0808
         CLI   INSRTCHR(R1),C' '                                   0808
         BNE   @DC00808                                            0808
*   END;                            /* SCAN FOR LAST NON BLANK       */
         BCTR  R4,0                                                0809
         ST    R4,INSRTLN                                          0809
         C     R4,FW2                                              0809
         BNL   @DL00808                                            0809
@DC00808 DS    0H                                                  0810
*   END SYSSRTN;                                                   0810
@EL00019 DS    0H                                                  0810
@EF00019 DS    0H                                                  0810
@ER00019 LM    R14,R12,12(R13)                                     0810
         BR    R14                                                 0810
*                                                                  0811
*   /*****************************************************************/
*   /*                                                               */
*   /* EVALUATION SETUP                                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0811
*SETUP:                                                            0811
*   PROC OPTIONS(SAVEAREA);                                        0811
SETUP    STM   R14,R12,12(R13)                                     0811
         ST    R13,@SA00020+4                                      0811
         LA    R14,@SA00020                                        0811
         ST    R14,8(,R13)                                         0811
         LR    R13,R14                                             0811
*   CALL WAITRTN;                   /* CHECK FOR & IN P-STRING       */
         BAL   R14,WAITRTN                                         0812
*   IF WAIT=NO&ERROR=F0 THEN        /* IF OK TO PROCESS              */
         TM    WAIT(COMONPTR),B'00010000'                          0813
         BNZ   @RF00813                                            0813
         SLR   R2,R2                                               0813
         C     R2,ERROR                                            0813
         BNE   @RF00813                                            0813
*     DO;                           /*                               */
*       BUFPTR=ADDR(C(I-F1));       /* POINT TO DUMMY BUFFER         */
         L     R3,I(,COMONPTR)                                     0815
         L     R1,BUFBASE(,COMONPTR)                               0815
         LA    R4,C-2(R3,R1)                                       0815
         ST    R4,BUFPTR(,COMONPTR)                                0815
*       CSAVE=BUF(F1:F2);           /* SAVE ORIGINAL BUFFER          */
         MVC   CSAVE(2),BUF(R4)                                    0816
*       BUF(F1:F2)=XEQUAL;          /* MAKE DUMMY STATEMENT          */
         MVC   BUF(2,R4),$XEQUALS                                  0817
*       CLEN=M-I+F1;                /* SET LENGTH OF DUMMY STMT      */
         LCR   R3,R3                                               0818
         AL    R3,M                                                0818
         AL    R3,FW1                                              0818
         STH   R3,CLEN(,COMONPTR)                                  0818
*       TYPE=F0;                    /* NUMERIC ASSIGNMENT            */
         ST    R2,TYPE(,COMONPTR)                                  0819
*       RFY                                                        0820
*        (R15) RSTD;                /* RESTRICT RETURN CODE REG      */
*       CALL IKJCT434(BUF,CLEN,TYPE,OUTA1);/* PROCESS EXPRESSION     */
         ST    R4,@AL00001                                         0821
         LA    R14,CLEN(,COMONPTR)                                 0821
         ST    R14,@AL00001+4                                      0821
         LA    R14,TYPE(,COMONPTR)                                 0821
         ST    R14,@AL00001+8                                      0821
         LA    R14,OUTA1                                           0821
         ST    R14,@AL00001+12                                     0821
         L     R15,IKJCT434                                        0821
         LA    R1,@AL00001                                         0821
         BALR  R14,R15                                             0821
*       ERROR=R15;                  /* SAVE ERROR CODE               */
         ST    R15,ERROR                                           0822
*       BUF(F1:F2)=CSAVE;           /* RESTORE BUFFER                */
         L     R14,BUFPTR(,COMONPTR)                               0823
         MVC   BUF(2,R14),CSAVE                                    0823
*       RFY                                                        0824
*        (R15) UNRSTD;              /* RELEASE RETURN CODE REG       */
*     END;                                                         0825
*   END SETUP;                                                     0826
@EL00020 L     R13,4(,R13)                                         0826
@EF00020 DS    0H                                                  0826
@ER00020 LM    R14,R12,12(R13)                                     0826
         BR    R14                                                 0826
*                                                                  0827
*   /*****************************************************************/
*   /*                                                               */
*   /* SYMBOLIC VARIABLE LOCATE                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0827
*LOCATE:                                                           0827
*   PROC(NAME,NAML,FUNC);                                          0827
LOCATE   STM   R14,R12,@SA00021                                    0827
         MVC   @PC00021(12),0(R1)                                  0827
*   DCL                                                            0828
*     NAME CHAR(*),                                                0828
*     NAML FIXED(31),                                              0828
*     FUNC FIXED(31);                                              0828
*   EXECDPTR=LSDEXEC;               /* RESET EXEC DATA POINTER       */
         L     R1,LSDPTR(,COMONPTR)                                0829
         L     R2,LSDEXEC-LSD(,R1)                                 0829
         ST    R2,EXECDPTR(,COMONPTR)                              0829
*   SNTABPTR=SNTABFST;              /* POINT TO FIRST SNTAB          */
         L     R3,SNTABFST(,R2)                                    0830
         ST    R3,SNTABPTR                                         0830
*   SNTELPTR=ADDR(SNTELFST);        /* POINT TO FIRST NAME ENTRY     */
         LA    R4,SNTELFST(,R3)                                    0831
         ST    R4,SNTELPTR(,COMONPTR)                              0831
*   EXITB=NO;                       /* CLEAR EXITB                   */
         NI    EXITB(COMONPTR),B'10111111'                         0832
*   IF NAML>F252 THEN               /* IF NAME IS TOO LONG           */
         L     R5,@PC00021+4                                       0833
         L     R6,NAML(,R5)                                        0833
         C     R6,FW252                                            0833
         BNH   @RF00833                                            0833
*     ERCOM=E940;                   /* SET ERCOM CODE                */
         MVC   ERCOM(4,COMONPTR),FW940                             0834
*   DO WHILE EXITB=NO&ERCOM=F0;     /* SEARCH TABLE UNTIL FOUND OR 0835
*                                      UNTIL AN ERCOM OCCURS         */
@RF00833 B     @DE00835                                            0835
@DL00835 DS    0H                                                  0836
*     IF NAML=SNTLNG&NAME(1:NAML)=SNTDATA(1:SNTLNG) THEN/* IF FOUND  */
         L     R7,@PC00021+4                                       0836
         L     R6,SNTELPTR(,COMONPTR)                              0836
         LH    R14,SNTLNG(,R6)                                     0836
         C     R14,NAML(,R7)                                       0836
         BNE   @RF00836                                            0836
         L     R15,@PC00021                                        0836
         BCTR  R14,0                                               0836
         EX    R14,@SC01109                                        0836
         BNE   @RF00836                                            0836
*       EXITB=YES;                  /* SET FOUND SWITCH              */
         OI    EXITB(COMONPTR),B'01000000'                         0837
*     ELSE                          /* OTHERWISE UPDATE POINTER      */
*       IF SNTLAST=YES THEN         /* IF END OF TABLE               */
         B     @RC00836                                            0838
@RF00836 L     R1,SNTELPTR(,COMONPTR)                              0838
         TM    SNTLAST(R1),B'00000001'                             0838
         BNO   @RF00838                                            0838
*         IF SNTABNXT=F0 THEN       /* AND NO MORE TABLES            */
         L     R1,SNTABPTR                                         0839
         L     R2,SNTABNXT(,R1)                                    0839
         LTR   R2,R2                                               0839
         BNZ   @RF00839                                            0839
*           IF FUNC=F8 THEN         /* IF VARIABLE TO BE ADDED       */
         L     R3,@PC00021+8                                       0840
         CLC   FUNC(4,R3),FW8                                      0840
         BNE   @RF00840                                            0840
*             DO;                   /* ADD NAME TO TABLE             */
*               CALL NAMEADD;       /* ADD NAME TO TABLE             */
         BAL   R14,NAMEADD                                         0842
*               EXITB=YES;          /* CAUSE TERMINATION             */
         OI    EXITB(COMONPTR),B'01000000'                         0843
*             END;                  /*                               */
*           ELSE                    /* OTHERWISE                     */
*             ERCOM=E904;           /* SET NOT FOUND RETURN CODE     */
         B     @RC00840                                            0845
@RF00840 MVC   ERCOM(4,COMONPTR),FW904                             0845
*         ELSE                      /*                               */
*           DO;                     /* IF NOT THE END, UPDATE TO NEXT*/
         B     @RC00839                                            0846
@RF00839 DS    0H                                                  0847
*             SNTABPTR=SNTABNXT;    /* TABLE                         */
         L     R1,SNTABPTR                                         0847
         L     R2,SNTABNXT(,R1)                                    0847
         ST    R2,SNTABPTR                                         0847
*             SNTELPTR=ADDR(SNTELFST);/* POINT TO FIRST NAME ENTRY IN*/
         LA    R3,SNTELFST(,R2)                                    0848
         ST    R3,SNTELPTR(,COMONPTR)                              0848
*           END;                    /* NEXT TABLE                    */
*       ELSE                        /* NO MORE TABLES AND NOT FOUND  */
*         SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);/* UPDATE POINTER */
         B     @RC00838                                            0850
@RF00838 L     R4,SNTELPTR(,COMONPTR)                              0850
         LH    R5,SNTLNG(,R4)                                      0850
         ALR   R5,R4                                               0850
         AL    R5,FW8                                              0850
         ST    R5,SNTELPTR(,COMONPTR)                              0850
*   END;                                                           0851
@RC00838 DS    0H                                                  0851
@RC00836 DS    0H                                                  0851
@DE00835 TM    EXITB(COMONPTR),B'01000000'                         0851
         BNZ   @DC00835                                            0851
         L     R6,ERCOM(,COMONPTR)                                 0851
         LTR   R6,R6                                               0851
         BZ    @DL00835                                            0851
@DC00835 DS    0H                                                  0852
*   IF ERCOM=F0&((FUNC^=F4&SNTLABEL=YES) /* IF A LABEL WAS FOUND WHEN*/
*       (FUNC=F4&SNTLABEL=NO)) THEN /* IT SHOULDNT, THEN SET         */
         L     R7,ERCOM(,COMONPTR)                                 0852
         LTR   R7,R7                                               0852
         BNZ   @RF00852                                            0852
         L     R14,@PC00021+8                                      0852
         CLC   FUNC(4,R14),FW4                                     0852
         BE    @GL00055                                            0852
         L     R1,SNTELPTR(,COMONPTR)                              0852
         TM    SNTLABEL(R1),B'00010000'                            0852
         BO    @RT00852                                            0852
@GL00055 L     R2,@PC00021+8                                       0852
         CLC   FUNC(4,R2),FW4                                      0852
         BNE   @RF00852                                            0852
         L     R1,SNTELPTR(,COMONPTR)                              0852
         TM    SNTLABEL(R1),B'00010000'                            0852
         BNZ   @RF00852                                            0852
@RT00852 DS    0H                                                  0853
*     ERCOM=E944;                   /* ERCOM INDICATOR               */
         MVC   ERCOM(4,COMONPTR),FW944                             0853
*   IF ERCOM=F0&SNTGLOB=YES THEN    /* IF A GLOBAL VARIABLE          */
@RF00852 L     R2,ERCOM(,COMONPTR)                                 0854
         LTR   R2,R2                                               0854
         BNZ   @RF00854                                            0854
         L     R1,SNTELPTR(,COMONPTR)                              0854
         TM    SNTGLOB(R1),B'10000000'                             0854
         BNO   @RF00854                                            0854
*     CALL GLOCATE;                 /* DO A GLOBAL SEARCH            */
         BAL   R14,GLOCATE                                         0855
*   ERROR=ERCOM;                    /*                               */
@RF00854 L     R3,ERCOM(,COMONPTR)                                 0856
         ST    R3,ERROR                                            0856
*                                                                  0857
*   /*****************************************************************/
*   /*                                                               */
*   /* NAME TABLE UPDATE - ADD A NAME ENTRY                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0857
*NAMEADD:                                                          0857
*   PROC;                                                          0857
         B     @PB00022                                            0857
NAMEADD  STM   R14,R12,12(R13)                                     0857
*   IF NAML>F31 THEN                /* IF NAME TOO LONG              */
         L     R4,@PC00021+4                                       0858
         L     R5,NAML(,R4)                                        0858
         C     R5,FW31                                             0858
         BNH   @RF00858                                            0858
*     ERCOM=E940;                   /* SET ERCOM CODE                */
         MVC   ERCOM(4,COMONPTR),FW940                             0859
*   ELSE                            /* OTHERWISE                     */
*     IF NAML+LENGTH(SNTELEM)>SNTABLNG-SNTABUSE THEN/* IF NO ROOM  0860
*                                      LEFT                          */
         B     @RC00858                                            0860
@RF00858 L     R6,@PC00021+4                                       0860
         L     R1,NAML(,R6)                                        0860
         LA    R0,8                                                0860
         ALR   R0,R1                                               0860
         L     R2,SNTABPTR                                         0860
         L     R3,SNTABLNG(,R2)                                    0860
         LR    R4,R3                                               0860
         SL    R4,SNTABUSE(,R2)                                    0860
         CR    R0,R4                                               0860
         BNH   @RF00860                                            0860
*       DO;                         /* GET ANOTHER TABLE             */
*         NEWLNG=MAX(SNTABLNG,NAML+LENGTH(SNTELEM)+LENGTH(SNTAB)+GAS);
         AL    R1,FW220                                            0862
         CR    R3,R1                                               0862
         BNL   *+6
         LR    R3,R1                                               0862
         ST    R3,NEWLNG                                           0862
*         DO;                       /* GETMAIN LV(NEWLNG) A(SNTSAVE)
*                                      SP(78) MF(E,GETLST)         0863
*                                      RTCD(ERCOM)                   */
*           RESPECIFY                                              0864
*            (R1,                                                  0864
*             R15) RESTRICTED;                                     0864
*           GETLST01=NEWLNG;        /* LENGTH                        */
         ST    R3,GETLST01                                         0865
*           GETLST03=ADDR(SNTSAVE); /* ADDR OF ADDR LIST             */
         LA    R14,SNTSAVE                                         0866
         STCM  R14,7,GETLST03                                      0866
*           GETLST05=78;            /* SUBPOOL VALUE                 */
         MVI   GETLST05,X'4E'                                      0867
*           R1=ADDR(GETLST);        /* REG1 POINTS TO LIST           */
         LA    R1,GETLST                                           0868
*           SVC(4);                 /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0869
*           ERCOM=R15;              /* SET RETURN CODE               */
         ST    R15,ERCOM(,COMONPTR)                                0870
*           RESPECIFY                                              0871
*            (R1,                                                  0871
*             R15) UNRESTRICTED;                                   0871
*         END;                      /* GETMAIN LV(NEWLNG) A(SNTSAVE)
*                                      SP(78) MF(E,GETLST)         0872
*                                      RTCD(ERCOM)                   */
*         IF ERCOM^=F0 THEN         /* IF GETMAIN ERCOM              */
         L     R2,ERCOM(,COMONPTR)                                 0873
         LTR   R2,R2                                               0873
         BZ    @RF00873                                            0873
*           ERCOM=E16;              /* SET ERCOM CODE                */
         MVC   ERCOM(4,COMONPTR),FW16                              0874
*         ELSE                      /* OTHERWISE                     */
*           DO;                     /* CHAIN NEW TABLE               */
         B     @RC00873                                            0875
@RF00873 DS    0H                                                  0876
*             RFY                                                  0876
*               SNTAB BASED(SNTSAVE);/* RESET BASE FOR NEW TABLE     */
*             SNTABNXT=F0;          /* NO NEXT TABLE                 */
         L     R3,SNTSAVE                                          0877
         SLR   R2,R2                                               0877
         ST    R2,SNTABNXT(,R3)                                    0877
*             SNTABLNG=NEWLNG;      /* LENGTH OF THIS TABLE          */
         L     R2,NEWLNG                                           0878
         ST    R2,SNTABLNG(,R3)                                    0878
*             SNTABUSE=LENGTH(SNTAB);/* LENGTH OF HEADER             */
         MVC   SNTABUSE(4,R3),FW12                                 0879
*             RFY                                                  0880
*               SNTAB BASED(SNTABPTR);/* RESTORE BASE                */
*             SNTABNXT=SNTSAVE;     /* CHAIN NEW TABLE               */
         L     R2,SNTABPTR                                         0881
         ST    R3,SNTABNXT(,R2)                                    0881
*             SNTABPTR=SNTSAVE;     /* SET BASE TO NEW TABLE         */
         ST    R3,SNTABPTR                                         0882
*           END;                    /*                               */
*       END;                        /*                               */
*     ELSE                          /* NOT A NEW TABLE               */
*       SNTLAST=NO;                 /* CLEAR OLD LAST SWITCH         */
         B     @RC00860                                            0885
@RF00860 L     R1,SNTELPTR(,COMONPTR)                              0885
         NI    SNTLAST(R1),B'11111110'                             0885
*   IF ERCOM=F0 THEN                /* IF NO GETMAIN ERCOM           */
@RC00860 DS    0H                                                  0886
@RC00858 L     R2,ERCOM(,COMONPTR)                                 0886
         LTR   R2,R2                                               0886
         BNZ   @RF00886                                            0886
*     DO;                           /* ADD ENTRY                     */
*       SNTELPTR=SNTABPTR+SNTABUSE; /* POINT TO NEW SLOT             */
         L     R3,SNTABPTR                                         0888
         L     R2,SNTABUSE(,R3)                                    0888
         LR    R4,R3                                               0888
         ALR   R4,R2                                               0888
         ST    R4,SNTELPTR(,COMONPTR)                              0888
*       SNTFLAGS=''B;               /* CLEAR FLAGS                   */
         XC    SNTFLAGS(2,R4),SNTFLAGS(R4)                         0889
*       SNTKEYW=YES;                /* KEYWORD WITH VALUE            */
*       SNTLAST=YES;                /* NOW LAST ELEMENT              */
         OI    SNTKEYW(R4),B'00100001'                             0891
*       SNTVLPTR=SVTABFST+LENGTH(SVTAB);/* POINT TO NULL ELEMENT     */
         L     R1,EXECDPTR(,COMONPTR)                              0892
         LA    R5,16                                               0892
         AL    R5,SVTABFST(,R1)                                    0892
         ST    R5,SNTVLPTR(,R4)                                    0892
*       SNTLNG=NAML;                /* LENGTH OF NAME                */
         L     R5,@PC00021+4                                       0893
         L     R1,NAML(,R5)                                        0893
         STH   R1,SNTLNG(,R4)                                      0893
*       SNTDATA(1:NAML)=NAME(1:NAML);/* NAME                         */
         LR    R5,R1                                               0894
         BCTR  R5,0                                                0894
         L     R6,@PC00021                                         0894
         EX    R5,@SM01111                                         0894
*       SNTABUSE=SNTABUSE+NAML+LENGTH(SNTELEM);/* AMOUNT IN USE      */
         ALR   R2,R1                                               0895
         AL    R2,FW8                                              0895
         ST    R2,SNTABUSE(,R3)                                    0895
*     END;                          /*                               */
*   END NAMEADD;                                                   0897
@EL00022 DS    0H                                                  0897
@EF00022 DS    0H                                                  0897
@ER00022 LM    R14,R12,12(R13)                                     0897
         BR    R14                                                 0897
*   END LOCATE;                                                    0898
@EL00021 DS    0H                                                  0898
@EF00021 DS    0H                                                  0898
@ER00021 LM    R14,R12,@SA00021                                    0898
         BR    R14                                                 0898
*                                                                  0899
*   /*****************************************************************/
*   /*                                                               */
*   /* GLOBAL LOCATE FUNCTION                                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0899
*GLOCATE:                                                          0899
*   PROC;                                                          0899
GLOCATE  STM   R14,R12,12(R13)                                     0899
*   SNTSAVE=SNTELPTR;               /* SAVE NAME POINTER             */
         L     R14,SNTELPTR(,COMONPTR)                             0900
         ST    R14,SNTSAVE                                         0900
*   EXECDPTR=GEXECDAT;              /* POINT TO GLOBAL EXEC          */
         L     R1,EXECDPTR(,COMONPTR)                              0901
         L     R2,GEXECDAT(,R1)                                    0901
         ST    R2,EXECDPTR(,COMONPTR)                              0901
*   SNTABPTR=SNTABFST;              /* POINT TO FIRST NAME TABLE     */
         L     R3,SNTABFST(,R2)                                    0902
         ST    R3,SNTABPTR                                         0902
*   SNTELPTR=ADDR(SNTELFST);        /* BEGIN OF GLOBAL NAMES         */
         LA    R4,SNTELFST(,R3)                                    0903
         ST    R4,SNTELPTR(,COMONPTR)                              0903
*   EXITB=NO;                       /* TURN OFF EXIT SWITCH          */
         NI    EXITB(COMONPTR),B'10111111'                         0904
*   DO WHILE EXITB=NO&ERCOM=F0;     /* FIND GLOBAL VARIABLE          */
         B     @DE00905                                            0905
@DL00905 DS    0H                                                  0906
*     IF SNTLNG=F4&                 /* IF LENGTH IS FOUR             */
*         SNTSAVE->SNTGVAL=SNTDATA(1:F4) THEN/* MATCH                */
         L     R5,SNTELPTR(,COMONPTR)                              0906
         CLC   SNTLNG(2,R5),HW4                                    0906
         BNE   @RF00906                                            0906
         L     R1,SNTSAVE                                          0906
         CLC   SNTGVAL(4,R1),SNTDATA(R5)                           0906
         BNE   @RF00906                                            0906
*       EXITB=YES;                  /* EXIT LOCATE                   */
         OI    EXITB(COMONPTR),B'01000000'                         0907
*     ELSE                          /* OTHERWISE                     */
*       IF SNTLAST=YES THEN         /* LAST NAME ?                   */
         B     @RC00906                                            0908
@RF00906 L     R1,SNTELPTR(,COMONPTR)                              0908
         TM    SNTLAST(R1),B'00000001'                             0908
         BNO   @RF00908                                            0908
*         IF SNTABNXT=F0 THEN       /* NO MORE TABLES ?              */
         L     R1,SNTABPTR                                         0909
         L     R2,SNTABNXT(,R1)                                    0909
         LTR   R2,R2                                               0909
         BNZ   @RF00909                                            0909
*           ERCOM=E312;             /* SET ERCOM CODE                */
         MVC   ERCOM(4,COMONPTR),FW312                             0910
*         ELSE                      /* OTHERWISE                     */
*           DO;                     /* SCAN NEXT TABLE               */
         B     @RC00909                                            0911
@RF00909 DS    0H                                                  0912
*             SNTABPTR=SNTABNXT;    /* BASE NEXT TABLE               */
         L     R1,SNTABPTR                                         0912
         L     R2,SNTABNXT(,R1)                                    0912
         ST    R2,SNTABPTR                                         0912
*             SNTELPTR=ADDR(SNTELFST);/* POINT TO FIRST NAME         */
         LA    R3,SNTELFST(,R2)                                    0913
         ST    R3,SNTELPTR(,COMONPTR)                              0913
*           END;                    /*                               */
*       ELSE                        /* OTHERWISE                     */
*         SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);/* GET NEXT       */
         B     @RC00908                                            0915
@RF00908 L     R4,SNTELPTR(,COMONPTR)                              0915
         LH    R5,SNTLNG(,R4)                                      0915
         ALR   R5,R4                                               0915
         AL    R5,FW8                                              0915
         ST    R5,SNTELPTR(,COMONPTR)                              0915
*   END;                            /*                               */
@RC00908 DS    0H                                                  0916
@RC00906 DS    0H                                                  0916
@DE00905 TM    EXITB(COMONPTR),B'01000000'                         0916
         BNZ   @DC00905                                            0916
         L     R6,ERCOM(,COMONPTR)                                 0916
         LTR   R6,R6                                               0916
         BZ    @DL00905                                            0916
@DC00905 DS    0H                                                  0917
*   ERROR=ERCOM;                                                   0917
         L     R14,ERCOM(,COMONPTR)                                0917
         ST    R14,ERROR                                           0917
*   END GLOCATE;                    /*                               */
@EL00023 DS    0H                                                  0918
@EF00023 DS    0H                                                  0918
@ER00023 LM    R14,R12,12(R13)                                     0918
         BR    R14                                                 0918
*                                                                  0919
*   /*****************************************************************/
*   /*                                                               */
*   /* SYMBOLIC VARIABLE UPDATE                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0919
*UPDATE:                                                           0919
*   PROC(VALUE,VALLEN);             /* UPDATE A VTAB ELEMENT         */
UPDATE   STM   R14,R12,12(R13)                                     0919
         MVC   @PC00024(8),0(R1)                                   0919
*   DCL                                                            0920
*     VALUE CHAR(*),                                               0920
*     VALLEN FIXED(31);                                            0920
*   SVTABPTR=SVTABFST;              /* POINT TO CURRENT VTAB         */
         L     R1,EXECDPTR(,COMONPTR)                              0921
         L     R1,SVTABFST(,R1)                                    0921
         ST    R1,SVTABPTR                                         0921
*   IF SNTNAUTH=YES&SYSUPDTE=NO THEN/* IF AN UNAUTHORIZED VARIABLE   */
         L     R1,SNTELPTR(,COMONPTR)                              0922
         TM    SNTNAUTH(R1),B'00000100'                            0922
         BNO   @RF00922                                            0922
         TM    SYSUPDTE(COMONPTR),B'00000100'                      0922
         BNZ   @RF00922                                            0922
*     ERCOM=E300;                   /* SET ERCOM CODE                */
         MVC   ERCOM(4,COMONPTR),FW300                             0923
*   ELSE                            /* OTHERWISE                     */
*     DO;                           /* UPDATE VARIABLE               */
         B     @RC00922                                            0924
@RF00922 DS    0H                                                  0925
*       IF VALLEN>SVTORIG THEN      /* IF A NEW SLOT IS NEEDED       */
         L     R2,@PC00024+4                                       0925
         L     R1,VALLEN(,R2)                                      0925
         L     R2,SNTELPTR(,COMONPTR)                              0925
         L     R3,SNTVLPTR(,R2)                                    0925
         CH    R1,SVTORIG(,R3)                                     0925
         BNH   @RF00925                                            0925
*         DO;                                                      0926
*           IF VALLEN+LENGTH(SVTELEM)>SVTABLNG-SVTABUSE THEN/* IF NEW
*                                      TABLE                         */
         AL    R1,FW4                                              0927
         L     R3,SVTABPTR                                         0927
         L     R0,SVTABLNG(,R3)                                    0927
         SL    R0,SVTABUSE(,R3)                                    0927
         CR    R1,R0                                               0927
         BNH   @RF00927                                            0927
*             DO;                   /* IS NEEDED, GET ONE            */
*               SNTSAVE=SNTELPTR;   /* SAVE NAME POINTER             */
         ST    R2,SNTSAVE                                          0929
*               EXITC=NO;           /* TURN OFF SWITCH               */
LBL01EDE NI    EXITC(COMONPTR),B'11111110'                         0930
*               NEWLNG=MAX(SVTABLNG+VALLEN+GAS,/* OLD SIZE + GAS     */
*                   (SVTABUSE-SVTABFRE)*F3/F2);/* 1.5 TIMES USED     */
         L     R1,SVTABPTR
         L     R2,@PC00024+4
         L     R3,SVTABLNG(,R1)
         AL    R3,VALLEN(,R2)
         AL    R3,FW200                                            0931
         L     R0,SVTABUSE(,R1)                                    0931
         SL    R0,SVTABFRE(,R1)                                    0931
         ALR   R0,R0                                               0931
         CR    R3,R0                                               0931
         BNL   *+6
         LR    R3,R0                                               0931
         ST    R3,NEWLNG                                           0931
*               DO;                 /* GETMAIN LV(NEWLNG) A(SVTABPTR)
*                                      SP(78) MF(E,GETLST)         0932
*                                      RTCD(ERCOM)                   */
*                 RESPECIFY                                        0933
*                  (R1,                                            0933
*                   R15) RESTRICTED;                               0933
*                 GETLST01=NEWLNG;  /* LENGTH                        */
         ST    R3,GETLST01                                         0934
*                 GETLST03=ADDR(SVTABPTR);/* ADDR OF ADDR LIST       */
         LA    R14,SVTABPTR                                        0935
         STCM  R14,7,GETLST03                                      0935
*                 GETLST05=78;      /* SUBPOOL VALUE                 */
         MVI   GETLST05,X'4E'                                      0936
*                 R1=ADDR(GETLST);/* REG1 POINTS TO LIST             */
         LA    R1,GETLST                                           0937
*                 SVC(4);           /* STANDARD-FORM OF GETMAIN      */
         SVC   4                                                   0938
*                 ERCOM=R15;        /* SET RETURN CODE               */
         ST    R15,ERCOM(,COMONPTR)                                0939
*                 RESPECIFY                                        0940
*                  (R1,                                            0940
*                   R15) UNRESTRICTED;                             0940
*               END;                /* GETMAIN LV(NEWLNG) A(SVTABPTR)
*                                      SP(78) MF(E,GETLST)         0941
*                                      RTCD(ERCOM)                   */
*               IF ERCOM^=F0 THEN   /* IF GETMAIN FAILS              */
         L     R2,ERCOM(,COMONPTR)                                 0942
         LTR   R2,R2                                               0942
         BZ    @RF00942                                            0942
*                 DO;               /* TERMINATE UPDATE              */
*                   ERCOM=E16;      /* SET ERCOM CODE                */
         LA    R3,16                                               0944
         ST    R3,ERCOM(,COMONPTR)                                 0944
*                   ERROR=ERCOM;    /* SET INT ERROR                 */
         ST    R3,ERROR                                            0945
*                   RETURN;         /* RETURN                        */
@EL00024 DS    0H                                                  0946
@EF00024 DS    0H                                                  0946
@ER00024 LM    R14,R12,12(R13)                                     0946
         BR    R14                                                 0946
*                 END;              /*                               */
*               ELSE                /* OTHERWISE                     */
*                 DO;               /* SET UP AND COPY TABLE         */
@RF00942 DS    0H                                                  0949
*                   SVTABNXT=SVTABFST;/* POINT TO OLD TABLE          */
         L     R4,SVTABPTR                                         0949
         L     R5,EXECDPTR(,COMONPTR)                              0949
         L     R7,SVTABFST(,R5)                                    0949
         ST    R7,SVTABNXT(,R4)                                    0949
*                   SVTABFRE=F0;    /* NO FREE SPACE IN TABLE        */
         SLR   R14,R14                                             0950
         ST    R14,SVTABFRE(,R4)                                   0950
*                   SVTABLNG=NEWLNG;/* LENGTH OF THIS TABLE          */
         L     R15,NEWLNG                                          0951
         ST    R15,SVTABLNG(,R4)                                   0951
*                   SVTABUSE=LENGTH(SVTAB)+LENGTH(NULELEM);/* INIT 0952
*                                      USED                          */
         MVC   SVTABUSE(4,R4),FW20                                 0952
*                   SVTABFST=SVTABPTR;/* INIT EXEC DATA POINTER      */
         ST    R4,SVTABFST(,R5)                                    0953
*                   SNTABPTR=SNTABFST;/* POINT TO FIRST NAME TAB     */
         L     R5,SNTABFST(,R5)                                    0954
         ST    R5,SNTABPTR                                         0954
*                   SNTELPTR=ADDR(SNTELFST);/* GET ADDR OF FIRST NAME*/
         LA    R15,SNTELFST(,R5)                                   0955
         ST    R15,SNTELPTR(,COMONPTR)                             0955
*                   RFY                                            0956
*                    (R2,                                          0956
*                     R3,                                          0956
*                     R4,                                          0956
*                     R5) RSTD;     /* RESTRICT REGS                 */
*                   EXITC=NO;       /* RESET EXIT SWITCH             */
         NI    EXITC(COMONPTR),B'11111110'                         0957
*                   R2=ADDR(SVTELFST)+LENGTH(NULELEM);/* GET FIRST 0958
*                                      ALAIL SLOT                    */
         L     R2,SVTABPTR               ( WAS SVTELFST(,R4) )     0958
         LA    R15,NULELEM(,R2)
         AL    R15,FW4                                             0958
         LR    R2,R15
         L     R15,SVTABPTR
*                   NULELEM=NULVAL; /* INIT NULL ELEMENT             */
         ST    R14,NULELEM(,R15)                                   0959
*                   DO WHILE EXITC=NO;/* MOVE UNTIL END OF TABLE     */
         B     @DE00960                                            0960
@DL00960 DS    0H                                                  0961
*                     IF SNTELPTR^=SNTSAVE&SNTLABEL=NO&/* OK TO COPY */
*                         SNTGLOB=NO THEN/* ELEMENT ?                */
         L     R0,SNTELPTR(,COMONPTR)                              0961
         C     R0,SNTSAVE                                          0961
         BE    @RF00961                                            0961
         LR    R1,R0
         TM    SNTLABEL(R1),B'00010000'                            0961
         BNZ   @RF00961                                            0961
         LR    R1,R0
         TM    SNTGLOB(R1),B'10000000'                             0961
         BNZ   @RF00961                                            0961
*                       DO;         /* YES-MOVE INTO NEW TABLE       */
*                         IF SVTORIG=F0 THEN/* IF ORIGINALLY NULL    */
         LR    R1,R0
         L     R1,SNTVLPTR(,R1)                                    0963
         LH    R1,SVTORIG(,R1)                                     0963
         LTR   R1,R1                                               0963
         BNZ   @RF00963                                            0963
*                           SNTVLPTR=ADDR(NULELEM);/* POINT TO NULL
*                                      ELEM                          */
         L     R1,SVTABPTR                                         0964
         LA    R1,NULELEM(,R1)                                     0964
         LR    R6,R0
         ST    R1,SNTVLPTR(,R6)                                    0964
*                         ELSE      /* OTHERWISE ASSIGN A SLOT       */
*                           DO;     /* IN NEW TABLE                  */
         B     @RC00963                                            0965
@RF00963 DS    0H                                                  0966
*                             R3=SVTORIG+LENGTH(SVTELEM);/* LENGTH OF
*                                      MOVE                          */
         LA    R7,4                                                0966
         L     R6,SNTELPTR(,COMONPTR)                              0966
         L     R14,SNTVLPTR(,R6)                                   0966
         LH    R15,SVTORIG(,R14)                                   0966
         LR    R3,R15                                              0966
         ALR   R3,R7                                               0966
*                             R4=SNTVLPTR;/* POINT TO OLD DATA       */
         LR    R4,R14                                              0967
*                             R5=R3;/* GET FROM LENGTH               */
         LR    R5,R3                                               0968
*                             SVTABFRE=SVTABFRE+SVTORIG-SVTLNG;/*  0969
*                                      UPDATE FREE                   */
         L     R0,SVTABPTR                                         0969
         LR    R1,R0
         L     R1,SVTABFRE(,R1)                                    0969
         ALR   R1,R15                                              0969
         SH    R1,SVTLNG(,R14)                                     0969
         LR    R14,R0
         ST    R1,SVTABFRE(,R14)                                   0969
*                             SVTABUSE=SVTABUSE+SVTORIG+LENGTH(SVTELEM)
*                                 ;                                0970
         LR    R14,R0
         AL    R15,SVTABUSE(,R14)                                  0970
         ALR   R15,R7                                              0970
         LR    R14,R0
         ST    R15,SVTABUSE(,R14)                                  0970
         LR    R1,R0
         C     R15,SVTABLNG(,R1)
         BH    LBL01EDE
*                             SNTVLPTR=R2;/* RESET VALUE POINTER     */
         ST    R2,SNTVLPTR(,R6)                                    0971
*                             MVCL(R2,R4);/* COPY INT NEW TABLE      */
         MVCL  R2,R4                                               0972
*                           END;                                   0973
*                       END;        /*                               */
@RC00963 DS    0H                                                  0975
*                     IF SNTLAST=YES THEN/* IF LAST IN TABLE         */
@RF00961 L     R1,SNTELPTR(,COMONPTR)                              0975
         TM    SNTLAST(R1),B'00000001'                             0975
         BNO   @RF00975                                            0975
*                       IF SNTABNXT=F0 THEN/* AND NO MORE TABLES     */
         L     R1,SNTABPTR                                         0976
         L     R6,SNTABNXT(,R1)                                    0976
         LTR   R6,R6                                               0976
         BNZ   @RF00976                                            0976
*                         EXITC=YES;/* END COPY                      */
         OI    EXITC(COMONPTR),B'00000001'                         0977
*                       ELSE        /* RESET TO NEW TABLE            */
*                         DO;       /*                               */
         B     @RC00976                                            0978
@RF00976 DS    0H                                                  0979
*                           SNTABPTR=SNTABNXT;/* RESET BASE          */
         L     R1,SNTABPTR                                         0979
         L     R6,SNTABNXT(,R1)                                    0979
         ST    R6,SNTABPTR                                         0979
*                           SNTELPTR=ADDR(SNTELFST);/* RESET EL PTR  */
         LA    R7,SNTELFST(,R6)                                    0980
         ST    R7,SNTELPTR(,COMONPTR)                              0980
*                         END;      /*                               */
*                     ELSE          /* OTHERWISE                     */
*                       SNTELPTR=SNTELPTR+SNTLNG+LENGTH(SNTELEM);  0982
         B     @RC00975                                            0982
@RF00975 L     R14,SNTELPTR(,COMONPTR)                             0982
         LH    R15,SNTLNG(,R14)                                    0982
         ALR   R15,R14                                             0982
         AL    R15,FW8                                             0982
         ST    R15,SNTELPTR(,COMONPTR)                             0982
*                   END;            /*                               */
@RC00975 DS    0H                                                  0983
@DE00960 TM    EXITC(COMONPTR),B'00000001'                         0983
         BZ    @DL00960                                            0983
*                   RFY                                            0984
*                    (R2,                                          0984
*                     R3,                                          0984
*                     R4,                                          0984
*                     R5) UNRSTD;   /* RELEASE REGS                  */
*                   SNTELPTR=SNTSAVE;/* RESTORE SNTELPTR             */
         L     R1,SNTSAVE                                          0985
         ST    R1,SNTELPTR(,COMONPTR)                              0985
*                   SNTABPTR=SNTABFST;/* RESTORE TABLE POINTER       */
         L     R1,EXECDPTR(,COMONPTR)                              0986
         L     R1,SNTABFST(,R1)                                    0986
         ST    R1,SNTABPTR                                         0986
*
*           SNTVLPTR=SVTABPTR+SVTABUSE;/* ASSIGN NEW SLOT            */
@RF00927 EQU   *
         L     R2,SVTABPTR                                         0999
         L     R3,SVTABUSE(,R2)                                    0999
         L     R4,@PC00024+4
         L     R1,VALLEN(,R4)
         LR    R0,R3                                               0999
         ALR   R0,R1                                               0999
*        ST    R0,SNTVLPTR(,R1)                                    0999
         AL    R0,FW4
         C     R0,SVTABLNG(,R2)
         BH    LBL01EDE
         L     R4,SNTELPTR(,COMONPTR)
         ALR   R3,R2
         ST    R3,SNTVLPTR(,R4)
         STH   R1,SVTORIG(,R3)
         STH   R1,SVTLNG(,R3)
         ST    R0,SVTABUSE(,R2)
*           SVTORIG=VALLEN;         /* SET LENGTH OF VALUE           */
*        L     R1,@PC00024+4                                       1000
*        L     R1,VALLEN(,R1)                                      1000
*        LR    R7,R0                                               1000
*        STH   R1,SVTORIG(,R7)                                     1000
*           SVTLNG=VALLEN;          /*                               */
*        LR    R7,R0                                               1001
*        STH   R1,SVTLNG(,R7)                                      1001
*           SVTABUSE=SVTABUSE+VALLEN+LENGTH(SVTELEM);/*              */
*        ALR   R14,R1                                              1002
*        AL    R14,FW4                                             1002
*        ST    R14,SVTABUSE(,R15)                                  1002
*         END;                      /*                               */
*       SVTABFRE=SVTABFRE+SVTLNG-VALLEN;/* ADJUST FREE AREA          */
@RF00925 L     R5,SVTABPTR                                         1004
         L     R1,SNTELPTR(,COMONPTR)                              1004
         L     R4,SNTVLPTR(,R1)                                    1004
         LH    R6,SVTLNG(,R4)                                      1004
         AL    R6,SVTABFRE(,R5)                                    1004
         L     R7,@PC00024+4                                       1004
         SL    R6,VALLEN(,R7)                                      1004
         ST    R6,SVTABFRE(,R5)                                    1004
*       IF NODATA=YES THEN          /* IF NODATA SPECIFIED           */
         TM    NODATA(COMONPTR),B'01000000'                        1005
         BNO   @RF01005                                            1005
*         SVTLNG=F0;                /* CURRENT LENGTH IS ZERO        */
         SLR   R5,R5                                               1006
         STH   R5,SVTLNG(,R4)                                      1006
*       ELSE                        /* OTHERWISE                     */
*         SVTLNG=VALLEN;            /* SET NEW LENGTH                */
         B     @RC01005                                            1007
@RF01005 L     R1,SNTELPTR(,COMONPTR)                              1007
         L     R1,SNTVLPTR(,R1)                                    1007
         L     R2,@PC00024+4                                       1007
         L     R0,VALLEN(,R2)                                      1007
         STH   R0,SVTLNG(,R1)                                      1007
*       RFY                                                        1008
*        (R2,                                                      1008
*         R3,                                                      1008
*         R4,                                                      1008
*         R5) RSTD;                 /* RESTRICT REGS                 */
@RC01005 DS    0H                                                  1009
*       R2=ADDR(SVTDATA);           /*                               */
         L     R1,SNTELPTR(,COMONPTR)                              1009
         L     R6,SNTVLPTR(,R1)                                    1009
         LA    R2,SVTDATA(,R6)                                     1009
*       R3=SVTORIG;                 /* ALWAYS CLEAR OR MOVE TO ALL   */
         LH    R3,SVTORIG(,R6)                                     1010
*       R4=ADDR(VALUE);             /* POINT TO NEW DATA             */
         L     R4,@PC00024                                         1011
*       R5=SVTLNG BLANKS;           /* ONLY MOVE WHAT SVTLNG SAYS    */
         LH    R5,SVTLNG(,R6)                                      1012
         O     R5,HIBLANK                                          1012
*       MVCL(R2,R4);                /* MOVE NEW DATA                 */
         MVCL  R2,R4                                               1013
@RC00922 L      R1,SVTABPTR
         L      R2,SVTABNXT(,R1)
         ST     R2,SVTWOPTR
         B      LBL02164
*                   DO;             /* FREEMAIN                    0987
*                                      LV(SVTABNXT->SVTABLNG)      0987
*                                      A(SVTABNXT) SP(78)          0987
*                                      MF(E,FRELST)                  */
*                     RESPECIFY                                    0988
*                      (R1) RESTRICTED;                            0988
*                     FRELST02=SVTABNXT->SVTABLNG;/* LENGTH          */
LBL02138 L     R3,SVTWOPTR
         L     R2,SVTABNXT(,R3)
         ST    R2,SVONEPTR
         MVC   FRELST02(3),SVTABLNG+1(R3)                          0989
         LA    R0,SVTWOPTR
*                     FRELST04=ADDR(SVTABNXT);/* ADDR OF ADDR LIST   */
         STCM  R0,7,FRELST04                                       0990
*                     FRELST06=78;  /* SUBPOOL VALUE                 */
         MVI   FRELST06,X'4E'                                      0991
*                     R1=ADDR(FRELST);/* REG1 POINTS TO LIST         */
         LA    R1,FRELST                                           0992
*                     SVC(5);       /* STANDARD-FORM OF GETMAIN      */
         SVC   5                                                   0993
*                     RESPECIFY                                    0994
*                      (R1) UNRESTRICTED;                          0994
*                   END;            /* FREEMAIN                    0995
*                                      LV(SVTABNXT->SVTABLNG)      0995
*                                      A(SVTABNXT) SP(78)          0995
*                                      MF(E,FRELST)                  */
*                   SVTABNXT=F0;    /* BREAK CHAIN                   */
*        L     R1,SVTABPTR                                         0996
         L     R3,SVONEPTR
         ST    R3,SVTWOPTR
LBL02164 SLR   R4,R4                                               0996
         C     R4,SVTWOPTR
         BNE   LBL02138
         L     R5,SVTABPTR
         ST    R4,SVTABNXT(,R5)                                    0996
*       RFY                                                        1014
*        (R2,                                                      1014
*         R3,                                                      1014
*         R4,                                                      1014
*         R5) UNRSTD;               /* RELEASE REGS                  */
*     END;                          /*                               */
*   EXECDPTR=LSDEXEC;               /* RESET EXEC DATA POINTER       */
         L     R1,LSDPTR(,COMONPTR)                                1016
         L     R1,LSDEXEC-LSD(,R1)                                 1016
         ST    R1,EXECDPTR(,COMONPTR)                              1016
*   ERROR=ERCOM;                                                   1017
         L     R3,ERCOM(,COMONPTR)                                 1017
         ST    R3,ERROR                                            1017
*   END UPDATE;                     /* END OF UPDATE                 */
         B     @EL00024                                            1018
*
*                 END;              /*                               */
*             END;                  /*                               */
*           SNTVLPTR=SVTABPTR+SVTABUSE;/* ASSIGN NEW SLOT            */
*RF00927 L     R1,SNTELPTR(,COMONPTR)                              0999
*        L     R15,SVTABPTR                                        0999
*        L     R14,SVTABUSE(,R15)                                  0999
*        LR    R0,R15                                              0999
*        ALR   R0,R14                                              0999
*        ST    R0,SNTVLPTR(,R1)                                    0999
*           SVTORIG=VALLEN;         /* SET LENGTH OF VALUE           */
*        L     R1,@PC00024+4                                       1000
*        L     R1,VALLEN(,R1)                                      1000
*        LR    R7,R0                                               1000
*        STH   R1,SVTORIG(,R7)                                     1000
*           SVTLNG=VALLEN;          /*                               */
*        LR    R7,R0                                               1001
*        STH   R1,SVTLNG(,R7)                                      1001
*           SVTABUSE=SVTABUSE+VALLEN+LENGTH(SVTELEM);/*              */
*        ALR   R14,R1                                              1002
*        AL    R14,FW4                                             1002
*        ST    R14,SVTABUSE(,R15)                                  1002
*         END;                      /*                               */
*                                                                  1019
*   /*****************************************************************/
*   /*                                                               */
*   /* RETURN CODE UPDATE                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1019
*RCUPDT:                                                           1019
*   PROC;                                                          1019
RCUPDT   STM   R14,R12,@SA00025                                    1019
*   IF NOLASTCC=YES THEN            /* IF SKIP LASTCC UPDATE @ZA03879*/
         L     R4,EXECDPTR(,COMONPTR)                              1020
         TM    NOLASTCC(R4),B'01000000'                            1020
         BNO   @RF01020                                            1020
*     NOLASTCC=NO;                  /* TURN OFF FLAG         @ZA03879*/
         NI    NOLASTCC(R4),B'10111111'                            1021
*   ELSE                            /*                       @ZA03879*/
*     DO;                           /* NORMAL UPDATE         @ZA03879*/
         B     @RC01020                                            1022
@RF01020 DS    0H                                                  1023
*       IF RCABEND=YES&ERRSAV//F4096=F0 THEN/* IF SYSTEM ABEND CODE  */
         TM    RCABEND(COMONPTR),B'10000000'                       1023
         BNO   @RF01023                                            1023
         L     R5,ERRSAV                                           1023
         LR    R6,R5                                               1023
         SRDA  R6,32                                               1023
         D     R6,FW4096                                           1023
         LTR   R6,R6                                               1023
         BNZ   @RF01023                                            1023
*         DO;                       /* DETRIMINE TYPE                */
*           AREA(1:4)=ERRSAV;       /* PREPARE FOR UNPAK             */
         STCM  R5,15,AREA                                          1025
*           UNPK(OUTA1(2:8),AREA(1:5));/* UNPACK ABEND CODE          */
         UNPK  OUTA1+1(7),AREA(5)                                  1026
*           DO I=F7 TO F2 BY-F1;    /* TRANSLATE FA-FF TO C1-C5      */
         LA    R6,7                                                1027
         ST    R6,I(,COMONPTR)                                     1027
@DL01027 DS    0H                                                  1028
*             IF OUTFX(I)>F249 THEN /* DECIMAL OF F9 (FA-FF IS >)    */
         L     R7,I(,COMONPTR)                                     1028
         SLR   R6,R6                                               1028
         IC    R6,OUTFX-1(R7)                                      1028
         C     R6,FW249                                            1028
         BNH   @RF01028                                            1028
*               OUTFX(I)=OUTFX(I)-F57;/* SUBTRACT 39 (FA-39=C1)      */
         SL    R6,FW57                                             1029
         STC   R6,OUTFX-1(R7)                                      1029
*           END;                    /* DONE TRANSLATE                */
@RF01028 SLR   R6,R6                                               1030
         BCTR  R6,0                                                1030
         AL    R6,I(,COMONPTR)                                     1030
         ST    R6,I(,COMONPTR)                                     1030
         C     R6,FW2                                              1030
         BNL   @DL01027                                            1030
*           OUTA1(1)=CCS;           /* INSERT S FOR SYSTEM           */
         MVI   OUTA1,C'S'                                          1031
*           LEN=F4;                 /* LENGTH OF ANSWER              */
         MVC   LEN(4,COMONPTR),FW4                                 1032
*         END;                      /*                               */
*       ELSE                        /* NORMAL PROCESSING             */
*         DO;                       /*                               */
         B     @RC01023                                            1034
@RF01023 DS    0H                                                  1035
*           CVD(ERRSAV,WORKAREA);   /* CONVERT CODE TO PRINTABLE     */
         L     R7,ERRSAV                                           1035
         CVD   R7,WORKAREA                                         1035
*           UNPK(OUTA1(1:16),WORKAREA);/*                            */
         UNPK  OUTA1(16),WORKAREA(8)                               1036
*           OUTA1(16)=OUTA1(16) CC0;/* INSURE PRINTABLE CHARS        */
         OI    OUTA1+15,C'0'                                       1037
*           IF RCABEND=YES THEN     /* IF USER ABEND CODE            */
         TM    RCABEND(COMONPTR),B'10000000'                       1038
         BNO   @RF01038                                            1038
*             DO;                   /*                               */
*               LEN=F5;             /* LENGTH IS FIVE                */
         MVC   LEN(4,COMONPTR),FW5                                 1040
*               OUTA1(1)=CCU;       /* SET U TYPE                    */
         MVI   OUTA1,C'U'                                          1041
*               OUTA1(2:5)=OUTA1(13:16);/* MOVE OVER CHARS           */
         MVC   OUTA1+1(4),OUTA1+12                                 1042
*             END;                  /*                               */
*           ELSE                    /* OTHERWISE GET CODE            */
*             DO LEN=F16 TO F2 BY-F1 WHILE OUTA1(1)=CC0;/* LZ      1044
*                                      SUPPRESS                      */
         B     @RC01038                                            1044
@RF01038 LA    R14,16                                              1044
         ST    R14,LEN(,COMONPTR)                                  1044
@DL01044 CLI   OUTA1,C'0'                                          1044
         BNE   @DC01044                                            1044
*               OUTA1(1:15)=OUTA1(2:16);/* SHIFT DOWN BUFFER         */
         MVC   @TS00001(15),OUTA1+1                                1045
         MVC   OUTA1(15),@TS00001                                  1045
*             END;                  /*                               */
         SLR   R14,R14                                             1046
         BCTR  R14,0                                               1046
         AL    R14,LEN(,COMONPTR)                                  1046
         ST    R14,LEN(,COMONPTR)                                  1046
         C     R14,FW2                                             1046
         BNL   @DL01044                                            1046
@DC01044 DS    0H                                                  1047
*         END;                      /*                               */
@RC01038 DS    0H                                                  1048
*       CALL LOCATE(LASTCC,LENGTH(LASTCC),F0);/* LOCATE LASTCC       */
@RC01023 LA    R1,@AL01048                                         1048
         BAL   R14,LOCATE                                          1048
*       IF ERROR=F0 THEN            /* IF LOCATE OK                  */
         L     R15,ERROR                                           1049
         LTR   R15,R15                                             1049
         BNZ   @RF01049                                            1049
*         DO;                       /* PROCESS UPDATE                */
*           CALL UPDATE(OUTA1,LEN); /* UPDATE LASTCC                 */
         LA    R0,OUTA1                                            1051
         ST    R0,@AL00001                                         1051
         LA    R1,LEN(,COMONPTR)                                   1051
         ST    R1,@AL00001+4                                       1051
         LA    R1,@AL00001                                         1051
         BAL   R14,UPDATE                                          1051
*           CALL LOCATE(MAXCC,LENGTH(MAXCC),F0);/* LOCATE MAXCC      */
         LA    R1,@AL01052                                         1052
         BAL   R14,LOCATE                                          1052
*           IF ERROR=F0&SVTLNG<LEN (SVTLNG=/* MX SMALLER ?           */
*               LEN&SVTDATA(1:LEN)<OUTA1(1:LEN)) THEN/* UPDATE       */
         L     R2,ERROR                                            1053
         LTR   R2,R2                                               1053
         BNZ   @GL00065                                            1053
         L     R1,SNTELPTR(,COMONPTR)                              1053
         L     R1,SNTVLPTR(,R1)                                    1053
         LH    R2,SVTLNG(,R1)                                      1053
         C     R2,LEN(,COMONPTR)                                   1053
         BL    @RT01053                                            1053
@GL00065 L     R1,SNTELPTR(,COMONPTR)                              1053
         L     R2,SNTVLPTR(,R1)                                    1053
         L     R3,LEN(,COMONPTR)                                   1053
         CH    R3,SVTLNG(,R2)                                      1053
         BNE   @RF01053                                            1053
         BCTR  R3,0                                                1053
         EX    R3,@SC01113                                         1053
         BNL   @RF01053                                            1053
@RT01053 DS    0H                                                  1054
*             CALL UPDATE(OUTA1,LEN);/* UPDATE MAXCC                 */
         LA    R4,OUTA1                                            1054
         ST    R4,@AL00001                                         1054
         LA    R5,LEN(,COMONPTR)                                   1054
         ST    R5,@AL00001+4                                       1054
         LA    R1,@AL00001                                         1054
         BAL   R14,UPDATE                                          1054
*         END;                      /*                               */
@RF01053 DS    0H                                                  1056
*       IF ERROR^=F0&ERROR^=E16 THEN/* ERROR FINDIN OR UPDATIN CC S  */
@RF01049 L     R6,ERROR                                            1056
         LTR   R6,R6                                               1056
         BZ    @RF01056                                            1056
         C     R6,FW16                                             1056
         BE    @RF01056                                            1056
*         ERROR=E999;               /* SET INTERNAL CLIST ERROR      */
         MVC   ERROR(4),FW999                                      1057
*     END;                          /* END NORMAL UPDATE     @ZA03879*/
*   END RCUPDT;                                                    1059
@EL00025 DS    0H                                                  1059
@EF00025 DS    0H                                                  1059
@ER00025 LM    R14,R12,@SA00025                                    1059
         BR    R14                                                 1059
*                                                                  1060
*   /*****************************************************************/
*   /*                                                               */
*   /* PRINT SUB FUNCTION                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1060
*PRINT:                                                            1060
*   PROC(LINEADDR,TERMPUT) OPTIONS(SAVEAREA);                      1060
PRINT    STM   R14,R12,12(R13)                                     1060
         ST    R13,@SA00026+4                                      1060
         LA    R14,@SA00026                                        1060
         ST    R14,8(,R13)                                         1060
         LR    R13,R14                                             1060
         MVC   @PC00026(8),0(R1)                                   1060
*   DCL                                                            1061
*     TERMPUT FIXED(31),                                           1061
*     LINEADDR PTR(31),                                            1061
*     ASIS FIXED(31) CONSTANT(2);                                  1061
*   IOPL(4)=ADDR(PLBLK);            /* ADDR OF PARM BLOCK            */
         LA    R7,PLBLK                                            1062
         ST    R7,IOPL+12(,COMONPTR)                               1062
*   CLEN=LINEADDR->LL;              /* SAVE ORIGINAL LENGTH          */
         L     R14,@PC00026                                        1063
         L     R1,LINEADDR(,R14)                                   1063
         LH    R2,LL(,R1)                                          1063
         STH   R2,CLEN(,COMONPTR)                                  1063
*   LINEADDR->LL=LINEADDR->LL-FREEAMT;/* REDUCE TOTAL LENGTH         */
         SL    R2,FREEAMT(,COMONPTR)                               1064
         STH   R2,LL(,R1)                                          1064
*   IF LINEADDR->LL=F4 THEN         /* IF LL=4 THEN SET LL=5         */
         C     R2,FW4                                              1065
         BNE   @RF01065                                            1065
*     LINEADDR->LL=F5;              /* TO FOOL TCAM (FOR C/R)        */
         MVC   LL(2,R1),HW5                                        1066
*   RFY                                                            1067
*    (R1,                                                          1067
*     R4,                                                          1067
*     R15) RSTD;                    /* RESTRICT REGS                 */
@RF01065 DS    0H                                                  1068
*   R15=ADDR(IKJEFT40);             /* ADDR OF T40                   */
         L     R15,IKJEFT40                                        1068
*   R1=ADDR(IOPL);                  /* LOAD REG 1                    */
         LR    R1,COMONPTR                                         1069
*   R4=LINEADDR;                    /* POINTER TO LINE TO PRINT      */
         L     R0,@PC00026                                         1070
         LR    R14,R0                                              1070
         L     R4,LINEADDR(,R14)                                   1070
*   IF TERMPUT=ASIS THEN            /* PUTLINE ASIS                  */
         L     R5,@PC00026+4                                       1071
         CLC   TERMPUT(4,R5),FW2                                   1071
         BNE   @RF01071                                            1071
*     GEN                                                          1072
*(PUTLINE OUTPUT=((R4),DATA),TERMPUT=ASIS,MF=(E,(1)),ENTRY=(15));  1072
         PUTLINE OUTPUT=((R4),DATA),TERMPUT=ASIS,MF=(E,(1)),ENTRY=(15)
*   ELSE                            /* OTHERWISE PRINT EDIT          */
*     GEN                                                          1073
*(PUTLINE OUTPUT=((R4),DATA),TERMPUT=EDIT,MF=(E,(1)),ENTRY=(15));  1073
         B     @RC01071                                            1073
@RF01071 DS    0H                                                  1073
         PUTLINE OUTPUT=((R4),DATA),TERMPUT=EDIT,MF=(E,(1)),ENTRY=(15)
*   RFY                                                            1074
*    (R1,                                                          1074
*     R4,                                                          1074
*     R15) UNRSTD;                  /* FREE REGS                     */
@RC01071 DS    0H                                                  1075
*   LINEADDR->LL=CLEN;              /* RESTORE LENGTH                */
         L     R6,@PC00026                                         1075
         L     R1,LINEADDR(,R6)                                    1075
         LH    R0,CLEN(,COMONPTR)                                  1075
         STH   R0,LL(,R1)                                          1075
*   END PRINT;                                                     1076
@EL00026 L     R13,4(,R13)                                         1076
@EF00026 DS    0H                                                  1076
@ER00026 LM    R14,R12,12(R13)                                     1076
         BR    R14                                                 1076
*                                                                  1077
*   /*****************************************************************/
*   /*                                                               */
*   /* COMMAND LIST ROUTINE                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1077
*CMDLST:                                                           1077
*   PROC;                                                          1077
CMDLST   STM   R14,R12,@SA00027                                    1077
*   IF OPCODE^=IGOTO THEN           /* AS LONG AS NOT INTERNAL GOTO  */
         L     R1,OLDLINE(,COMONPTR)                               1078
         CLI   OPCODE(R1),15                                       1078
         BE    @RF01078                                            1078
*     DO;                           /* LIST COMMAND                  */
*       IF FOLD=NO THEN             /* FOLD INTO UPPER CASE ?        */
         TM    FOLD(COMONPTR),B'00010000'                          1080
         BNZ   @RF01080                                            1080
*         CALL PRINT(BUFBASE,F0);   /* NO- PRINT OK                  */
         LA    R2,BUFBASE(,COMONPTR)                               1081
         ST    R2,@AL00001                                         1081
         LA    R3,FW0                                              1081
         ST    R3,@AL00001+4                                       1081
         LA    R1,@AL00001                                         1081
         BAL   R14,PRINT                                           1081
*       ELSE                        /* YES- FOLD TO UPPER CASE       */
*         DO;                       /* BEFORE PRINTING               */
         B     @RC01080                                            1082
@RF01080 DS    0H                                                  1083
*           DO I=1 TO LL-(F4+FREEAMT);/* FOLD TO UPPER CASE          */
         LA    R4,1                                                1083
         B     @DE01083                                            1083
@DL01083 DS    0H                                                  1084
*             NEWC(I)=C(I);         /* COPY TO NEW BUFFER            */
         L     R5,I(,COMONPTR)                                     1084
         L     R4,NEWBASE(,COMONPTR)                               1084
         ALR   R5,R4                                               1084
         L     R6,I(,COMONPTR)                                     1084
         L     R7,BUFBASE(,COMONPTR)                               1084
         ALR   R7,R6                                               1084
         MVC   NEWC-1(1,R5),C-1(R7)                                1084
*             TR(NEWC(I),TRTABUP);  /* TRANSLATE TO UPPER CASE       */
         ALR   R4,R6                                               1085
         TR    NEWC-1(1,R4),TRTABUP                                1085
*           END;                    /*                               */
         LA    R4,1                                                1086
         AL    R4,I(,COMONPTR)                                     1086
@DE01083 ST    R4,I(,COMONPTR)                                     1086
         L     R5,BUFBASE(,COMONPTR)                               1086
         LA    R6,4                                                1086
         AL    R6,FREEAMT(,COMONPTR)                               1086
         LCR   R6,R6                                               1086
         AH    R6,LL(,R5)                                          1086
         CR    R4,R6                                               1086
         BNH   @DL01083                                            1086
*           CALL PRINT(NEWBASE,F0); /* PRINT FOLDED LINE             */
         LA    R7,NEWBASE(,COMONPTR)                               1087
         ST    R7,@AL00001                                         1087
         LA    R14,FW0                                             1087
         ST    R14,@AL00001+4                                      1087
         LA    R1,@AL00001                                         1087
         BAL   R14,PRINT                                           1087
*         END;                      /*                               */
*     END;                                                         1089
*   END CMDLST;                                                    1090
@EL00027 DS    0H                                                  1090
@EF00027 DS    0H                                                  1090
@ER00027 LM    R14,R12,@SA00027                                    1090
         BR    R14                                                 1090
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* &NRSTR BUILT IN FUNCTION                             ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*
NRSTRRTN OI    LCLFLAGS,$#NRSTR     SET FUNCTION ACTIVE FLAG    ZP60014
*                                   DROP THROUGH TO STRING FUNCTION
*                                                                  1091
*   /*****************************************************************/
*   /*                                                               */
*   /* STRING BUILT IN FUNCTION                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1091
*STRRTN:                                                           1091
*   PROC;                                                          1091
STRRTN   STM   R14,R12,@SA00028                                    1091
*   CALL WAITRTN;                   /* CHECK FOR WAIT                */
         BAL   R14,WAITRTN                                         1092
*   IF WAIT=NO&ERROR=F0 THEN        /* IF OK                         */
         TM    WAIT(COMONPTR),B'00010000'                          1093
         BNZ   @RF01093                                            1093
         L     R15,ERROR                                           1093
         LTR   R15,R15                                             1093
         BNZ   @RF01093                                            1093
*     DO;                           /* PROCESS                       */
*       PCTR=F1;                    /* INIT PCTR                     */
         LA    R0,1                                                1095
         ST    R0,PCTR                                             1095
*       INSRTDA=ADDR(C(I+1));       /* POINT TO VALUE                */
         L     R1,I(,COMONPTR)                                     1096
         L     R2,BUFBASE(,COMONPTR)                               1096
         LA    R3,C(R1,R2)                                         1096
         ST    R3,INSRTDA                                          1096
*       INSRTLN=ADDR(C(M))-INSRTDA; /* FIND LENGTH OF STRING         */
         L     R1,M                                                1097
         LA    R2,C-1(R1,R2)                                       1097
         SLR   R2,R3                                               1097
         ST    R2,INSRTLN                                          1097
*       DO I=1 TO INSRTLN;          /*                               */
         B     @DE01098                                            1098
@DL01098 DS    0H                                                  1099
*         TR(INSRTCHR(I),TRTABDN);  /* FOLD TO SPCL CHR              */
         L     R1,I(,COMONPTR)                                     1099
         L     R2,INSRTDA                                          1099
         ALR   R2,R1                                               1099
         BCTR  R2,0                                                1099
         TR    INSRTCHR(1,R2),TRTABDN                              1099
*       END;                                                       1100
         LA    R0,1                                                1100
         AL    R0,I(,COMONPTR)                                     1100
@DE01098 ST    R0,I(,COMONPTR)                                     1100
         C     R0,INSRTLN                                          1100
         BNH   @DL01098                                            1100
*       I=M+F1;                     /* POINT TO RESUME DATA          */
         LA    R1,1                                                1101
         AL    R1,M                                                1101
         ST    R1,I(,COMONPTR)                                     1101
*     END;                                                         1102
*   END STRRTN;                                                    1103
@EL00028 DS    0H                                                  1103
@EF00028 DS    0H                                                  1103
@ER00028 LM    R14,R12,@SA00028                                    1103
         BR    R14                                                 1103
*                                                                  1104
*   /*****************************************************************/
*   /*                                                               */
*   /* SEPARATOR FLUSH ROUTINE                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1104
*SKIPSEPS:                                                         1104
*   PROC;                                                          1104
SKIPSEPS STM   R14,R12,12(R13)                                     1104
*   SEPS=YES;                       /* SKIP ALL DELIMETERS           */
         OI    SEPS(COMONPTR),B'10000000'                          1105
*   WORDPTR=BUFBASE;                /* POINT TO BUFFER               */
         L     R3,BUFBASE(,COMONPTR)                               1106
         ST    R3,WORDPTR                                          1106
*   WI=I;                           /* GET I                         */
         L     R5,I(,COMONPTR)                                     1107
         ST    R5,WI                                               1107
*   DO WHILE SEPS=YES&WI<=WLL-(F4+FREEAMT);/*                        */
         B     @DE01108                                            1108
@DL01108 DS    0H                                                  1109
*     IF W(WI)^=BLANK&W(WI)^=COMMA&W(WI)^=TAB THEN/* NOT DELIM       */
         L     R6,WORDPTR                                          1109
         L     R7,WI                                               1109
         ALR   R7,R6                                               1109
         CLI   W-1(R7),C' '                                        1109
         BE    @RF01109                                            1109
         L     R7,WI                                               1109
         ALR   R7,R6                                               1109
         CLI   W-1(R7),C','                                        1109
         BE    @RF01109                                            1109
         L     R7,WI                                               1109
         ALR   R7,R6                                               1109
         CLI   W-1(R7),X'05'                                       1109
         BE    @RF01109                                            1109
*       IF WI<WLL-(F4+FREEAMT)&W(WI:WI+F1)=OPENCMT THEN/* CMT        */
         L     R7,WI                                               1110
         LA    R14,4                                               1110
         L     R15,FREEAMT(,COMONPTR)                              1110
         ALR   R15,R14                                             1110
         LCR   R15,R15                                             1110
         AH    R15,WLL(,R6)                                        1110
         CR    R7,R15                                              1110
         BNL   @RF01110                                            1110
         ALR   R6,R7                                               1110
         CLC   W-1(2,R6),SLSHASTR                                  1110
         BNE   @RF01110                                            1110
*         DO;                       /*                               */
*           DO WI=WI+F4 TO WLL-(F4+FREEAMT) WHILE/*                  */
*                 W(WI-F2:WI-F1)^=CLOSCMT;/*                         */
         ALR   R7,R14                                              1112
         B     @DE01112                                            1112
@DL01112 L     R1,WORDPTR                                          1112
         ALR   R1,R7                                               1112
         CLC   W-3(2,R1),ASTRSLSH                                  1112
         BE    @DC01112                                            1112
*           END;                    /* FIND CLOSE COMMENT            */
         AL    R7,FW1                                              1113
@DE01112 ST    R7,WI                                               1113
         LA    R6,4                                                1113
         AL    R6,FREEAMT(,COMONPTR)                               1113
         LCR   R6,R6                                               1113
         L     R1,WORDPTR                                          1113
         AH    R6,WLL(,R1)                                         1113
         CR    R7,R6                                               1113
         BNH   @DL01112                                            1113
@DC01112 DS    0H                                                  1114
*         END;                                                     1114
*       ELSE                        /* IF NOT A DELIMETER, END SKIP  */
*         SEPS=NO;                  /*                               */
         B     @RC01110                                            1115
@RF01110 NI    SEPS(COMONPTR),B'01111111'                          1115
*     ELSE                          /* IF A SEPARATOR, INCREMENT     */
*       WI=WI+F1;                   /* INDEX                         */
         B     @RC01109                                            1116
@RF01109 LA    R2,1                                                1116
         AL    R2,WI                                               1116
         ST    R2,WI                                               1116
*   END;                                                           1117
@RC01109 DS    0H                                                  1117
@DE01108 TM    SEPS(COMONPTR),B'10000000'                          1117
         BNO   @DC01108                                            1117
         LA    R3,4                                                1117
         AL    R3,FREEAMT(,COMONPTR)                               1117
         LCR   R3,R3                                               1117
         L     R2,WORDPTR                                          1117
         AH    R3,WLL(,R2)                                         1117
         C     R3,WI                                               1117
         BNL   @DL01108                                            1117
@DC01108 DS    0H                                                  1118
*   I=WI;                           /* SET I                         */
         L     R4,WI                                               1118
         ST    R4,I(,COMONPTR)                                     1118
*   END SKIPSEPS;                                                  1119
@EL00029 DS    0H                                                  1119
@EF00029 DS    0H                                                  1119
@ER00029 LM    R14,R12,12(R13)                                     1119
         BR    R14                                                 1119
*                                                                  1120
*   /*****************************************************************/
*   /*                                                               */
*   /* FIND A WORD ROUTINE                                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1120
*FINDWORD:                                                         1120
*   PROC;                                                          1120
FINDWORD STM   R14,R12,@SA00030                                    1120
*   CALL SKIPSEPS;                  /* SKIP SEPARATORS               */
         BAL   R14,SKIPSEPS                                        1121
*SCANWORD:                                                         1122
*   ENTRY;                          /* SCANWORD ENTRY                */
         B     @EC01122                                            1122
SCANWORD STM   R14,R12,@SA00030                                    1122
@EC01122 DS    0H                                                  1123
*   BEGIN=I;                        /* BEGIN OF KYWWORD              */
         L     R5,I(,COMONPTR)                                     1123
         ST    R5,BEGIN(,COMONPTR)                                 1123
*   WI=I;                           /* GET I                         */
         ST    R5,WI                                               1124
*   ENDDLM=YES;                     /* INIT ENDDLM TO YES            */
         OI    ENDDLM(COMONPTR),B'01000000'                        1125
*   WORDPTR=BUFBASE;                /* SET BASE                      */
         L     R4,BUFBASE(,COMONPTR)                               1126
         ST    R4,WORDPTR                                          1126
*   IF SEPS=NO THEN                 /* IF ALL SEPARATORS             */
         TM    SEPS(COMONPTR),B'10000000'                          1127
         BNZ   @RF01127                                            1127
*     DO;                                                          1128
*       DO WI=WI TO LL-(F4+FREEAMT) WHILE/* CHECK FOR VALID CHARS    */
*             (W(WI)>=CCA&W(WI)<=CCI) /* A TO I                      */
*             (W(WI)>=CCJ&W(WI)<=CCR) /* J TO R                      */
*             (W(WI)>=CCS&W(WI)<=CCZ) /* S TO Z                      */
*             (WI^=BEGIN&W(WI)>=CC0&W(WI)<=CC9);/* 0 TO 9            */
         B     @DE01129                                            1129
@DL01129 L     R4,WORDPTR                                          1129
         ALR   R5,R4                                               1129
         CLI   W-1(R5),C'A'                                        1129
         BL    @GL00076                                            1129
         L     R5,WI                                               1129
         ALR   R4,R5                                               1129
         CLI   W-1(R4),C'I'                                        1129
         BNH   @DB01129                                            1129
@GL00076 L     R6,WORDPTR                                          1129
         L     R7,WI                                               1129
         ALR   R7,R6                                               1129
         CLI   W-1(R7),C'J'                                        1129
         BL    @GL00075                                            1129
         L     R7,WI                                               1129
         ALR   R6,R7                                               1129
         CLI   W-1(R6),C'R'                                        1129
         BNH   @DB01129                                            1129
@GL00075 L     R14,WORDPTR                                         1129
         L     R15,WI                                              1129
         ALR   R15,R14                                             1129
         CLI   W-1(R15),C'S'                                       1129
         BL    @GL00074                                            1129
         L     R15,WI                                              1129
         ALR   R14,R15                                             1129
         CLI   W-1(R14),C'Z'                                       1129
         BNH   @DB01129                                            1129
@GL00074 L     R0,WI                                               1129
         C     R0,BEGIN(,COMONPTR)                                 1129
         BE    @DC01129                                            1129
         L     R1,WORDPTR                                          1129
         LR    R2,R0                                               1129
         ALR   R2,R1                                               1129
         CLI   W-1(R2),C'0'                                        1129
         BL    @DC01129                                            1129
         ALR   R1,R0                                               1129
         CLI   W-1(R1),C'9'                                        1129
         BH    @DC01129                                            1129
@DB01129 DS    0H                                                  1130
*       END;                        /*                               */
         LA    R5,1                                                1130
         AL    R5,WI                                               1130
         ST    R5,WI                                               1130
@DE01129 L     R1,BUFBASE(,COMONPTR)                               1130
         LA    R4,4                                                1130
         AL    R4,FREEAMT(,COMONPTR)                               1130
         LCR   R4,R4                                               1130
         AH    R4,LL(,R1)                                          1130
         CR    R5,R4                                               1130
         BNH   @DL01129                                            1130
@DC01129 DS    0H                                                  1131
*       IF WI<=WLL-(F4+FREEAMT)&W(WI)^=BLANK&/* NOT END OF BFR AND NT*/
*           W(WI)^=COMMA&W(WI)^=TAB&(OPCODE^=SETCODE W(WI)^=EQUAL)&/**/
*           (WI=WLL-(F4+FREEAMT) W(WI:WI+F1)^=OPENCMT) THEN/* NOT CMT*/
         L     R5,WI                                               1131
         L     R4,WORDPTR                                          1131
         LA    R6,4                                                1131
         AL    R6,FREEAMT(,COMONPTR)                               1131
         LCR   R6,R6                                               1131
         AH    R6,WLL(,R4)                                         1131
         CR    R5,R6                                               1131
         BH    @RF01131                                            1131
         ALR   R5,R4                                               1131
         CLI   W-1(R5),C' '                                        1131
         BE    @RF01131                                            1131
         L     R5,WI                                               1131
         ALR   R5,R4                                               1131
         CLI   W-1(R5),C','                                        1131
         BE    @RF01131                                            1131
         L     R5,WI                                               1131
         ALR   R5,R4                                               1131
         CLI   W-1(R5),X'05'                                       1131
         BE    @RF01131                                            1131
         L     R1,OLDLINE(,COMONPTR)                               1131
         CLI   OPCODE(R1),13                                       1131
         BNE   @GL00082                                            1131
         L     R5,WI                                               1131
         ALR   R4,R5                                               1131
         CLI   W-1(R4),C'='                                        1131
         BE    @RF01131                                            1131
@GL00082 L     R6,WI                                               1131
         L     R7,WORDPTR                                          1131
         LA    R14,4                                               1131
         AL    R14,FREEAMT(,COMONPTR)                              1131
         LCR   R14,R14                                             1131
         AH    R14,WLL(,R7)                                        1131
         CR    R6,R14                                              1131
         BE    @RT01131                                            1131
         ALR   R7,R6                                               1131
         CLC   W-1(2,R7),SLSHASTR                                  1131
         BE    @RF01131                                            1131
@RT01131 DS    0H                                                  1132
*         ENDDLM=NO;                /* SET NOT VALID DELIMETER       */
         NI    ENDDLM(COMONPTR),B'10111111'                        1132
*     END;                          /*                               */
@RF01131 DS    0H                                                  1134
*   I=WI;                           /* RESTORE I                     */
@RF01127 L     R15,WI                                              1134
         ST    R15,I(,COMONPTR)                                    1134
*   LEN=I-BEGIN;                    /* GET LENGTH OF KYWWORD         */
         SL    R15,BEGIN(,COMONPTR)                                1135
         ST    R15,LEN(,COMONPTR)                                  1135
*   END FINDWORD;                                                  1136
@EL00030 DS    0H                                                  1136
@EF00030 DS    0H                                                  1136
@ER00030 LM    R14,R12,@SA00030                                    1136
         BR    R14                                                 1136
*                                                                  1137
*   /*****************************************************************/
*   /*                                                               */
*   /* FIND A FILE ROUTINE                                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  1137
*FINDFILE:                                                         1137
*   PROC;                           /*                       @ZA17843*/
FINDFILE STM   R14,R12,@SA00031                                    1137
*   CALL SKIPSEPS;                  /* SKIP SEPARATORS       @ZA17843*/
         BAL   R14,SKIPSEPS                                        1138
*   BEGIN=I;                        /* BEGIN OF KYWWORD      @ZA17843*/
         L     R0,I(,COMONPTR)                                     1139
         ST    R0,BEGIN(,COMONPTR)                                 1139
*   WI=I;                           /* GET I                 @ZA17843*/
         ST    R0,WI                                               1140
*   ENDDLM=YES;                     /* INIT ENDDLM TO YES    @ZA17843*/
         OI    ENDDLM(COMONPTR),B'01000000'                        1141
*   WORDPTR=BUFBASE;                /* SET BASE              @ZA17843*/
         L     R1,BUFBASE(,COMONPTR)                               1142
         ST    R1,WORDPTR                                          1142
*   IF SEPS=NO THEN                 /* IF ALL SEPARATORS     @ZA17843*/
         TM    SEPS(COMONPTR),B'10000000'                          1143
         BNZ   @RF01143                                            1143
*     DO;                           /*                       @ZA17843*/
*       DO WI=WI TO LL-(F4+FREEAMT) WHILE/* CHAR CHECK       @ZA17843*/
*             (W(WI)>=CCA&W(WI)<=CCI) /* A TO I              @ZA17843*/
*             (W(WI)>=CCJ&W(WI)<=CCR) /* J TO R              @ZA17843*/
*             (W(WI)>=CCS&W(WI)<=CCZ) /* S TO Z              @ZA17843*/
*             (W(WI)=CC$ W(WI)=CC@ W(WI)=CC#) /* NATIONALS   @ZA17843*/
*             (WI^=BEGIN&W(WI)>=CC0&W(WI)<=CC9);/* 0 TO 9    @ZA17843*/
         B     @DE01145                                            1145
@DL01145 L     R1,WORDPTR                                          1145
         LR    R2,R0                                               1145
         ALR   R2,R1                                               1145
         CLI   W-1(R2),C'A'                                        1145
         BL    @GL00092                                            1145
         ALR   R1,R0                                               1145
         CLI   W-1(R1),C'I'                                        1145
         BNH   @DB01145                                            1145
@GL00092 L     R3,WORDPTR                                          1145
         L     R2,WI                                               1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'J'                                        1145
         BL    @GL00091                                            1145
         L     R2,WI                                               1145
         ALR   R3,R2                                               1145
         CLI   W-1(R3),C'R'                                        1145
         BNH   @DB01145                                            1145
@GL00091 L     R3,WORDPTR                                          1145
         L     R2,WI                                               1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'S'                                        1145
         BL    @GL00090                                            1145
         L     R2,WI                                               1145
         ALR   R3,R2                                               1145
         CLI   W-1(R3),C'Z'                                        1145
         BNH   @DB01145                                            1145
@GL00090 L     R3,WORDPTR                                          1145
         L     R2,WI                                               1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'$'                                        1145
         BE    @DB01145                                            1145
         L     R2,WI                                               1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'@'                                        1145
         BE    @DB01145                                            1145
         L     R2,WI                                               1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'#'                                        1145
         BE    @DB01145                                            1145
         L     R2,WI                                               1145
         C     R2,BEGIN(,COMONPTR)                                 1145
         BE    @DC01145                                            1145
         ALR   R2,R3                                               1145
         CLI   W-1(R2),C'0'                                        1145
         BL    @DC01145                                            1145
         L     R2,WI                                               1145
         ALR   R3,R2                                               1145
         CLI   W-1(R3),C'9'                                        1145
         BH    @DC01145                                            1145
@DB01145 DS    0H                                                  1146
*       END;                        /*                       @ZA17843*/
         LA    R0,1                                                1146
         AL    R0,WI                                               1146
         ST    R0,WI                                               1146
@DE01145 L     R1,BUFBASE(,COMONPTR)                               1146
         LA    R2,4                                                1146
         AL    R2,FREEAMT(,COMONPTR)                               1146
         LCR   R2,R2                                               1146
         AH    R2,LL(,R1)                                          1146
         CR    R0,R2                                               1146
         BNH   @DL01145                                            1146
@DC01145 DS    0H                                                  1147
*       IF WI<=WLL-(F4+FREEAMT)&W(WI)^=BLANK&/* BUFFER END?  @ZA17843*/
*           W(WI)^=COMMA&W(WI)^=TAB&(OPCODE^=SETCODE W(WI)^=EQUAL)&/**/
*           (WI=WLL-(F4+FREEAMT) W(WI:WI+F1)^=OPENCMT) THEN/*      1147
*                                                            @ZA17843*/
         L     R3,WI                                               1147
         L     R2,WORDPTR                                          1147
         LA    R4,4                                                1147
         AL    R4,FREEAMT(,COMONPTR)                               1147
         LCR   R4,R4                                               1147
         AH    R4,WLL(,R2)                                         1147
         CR    R3,R4                                               1147
         BH    @RF01147                                            1147
         ALR   R3,R2                                               1147
         CLI   W-1(R3),C' '                                        1147
         BE    @RF01147                                            1147
         L     R3,WI                                               1147
         ALR   R3,R2                                               1147
         CLI   W-1(R3),C','                                        1147
         BE    @RF01147                                            1147
         L     R3,WI                                               1147
         ALR   R3,R2                                               1147
         CLI   W-1(R3),X'05'                                       1147
         BE    @RF01147                                            1147
         L     R1,OLDLINE(,COMONPTR)                               1147
         CLI   OPCODE(R1),13                                       1147
         BNE   @GL00100                                            1147
         L     R3,WI                                               1147
         ALR   R2,R3                                               1147
         CLI   W-1(R2),C'='                                        1147
         BE    @RF01147                                            1147
@GL00100 L     R4,WI                                               1147
         L     R5,WORDPTR                                          1147
         LA    R6,4                                                1147
         AL    R6,FREEAMT(,COMONPTR)                               1147
         LCR   R6,R6                                               1147
         AH    R6,WLL(,R5)                                         1147
         CR    R4,R6                                               1147
         BE    @RT01147                                            1147
         ALR   R5,R4                                               1147
         CLC   W-1(2,R5),SLSHASTR                                  1147
         BE    @RF01147                                            1147
@RT01147 DS    0H                                                  1148
*         ENDDLM=NO;                /* SET NOT VALID DELIMET @ZA17843*/
         NI    ENDDLM(COMONPTR),B'10111111'                        1148
*     END;                          /*                       @ZA17843*/
@RF01147 DS    0H                                                  1150
*   I=WI;                           /* RESTORE I             @ZA17843*/
@RF01143 L     R7,WI                                               1150
         ST    R7,I(,COMONPTR)                                     1150
*   LEN=I-BEGIN;                    /* GET LENGTH OF KYWWORD @ZA17843*/
         SL    R7,BEGIN(,COMONPTR)                                 1151
         ST    R7,LEN(,COMONPTR)                                   1151
*   END FINDFILE;                   /*                       @ZA17843*/
@EL00031 DS    0H                                                  1152
@EF00031 DS    0H                                                  1152
@ER00031 LM    R14,R12,@SA00031                                    1152
         BR    R14                                                 1152
@DATA    DS    0H
*SM01094 MVC   STFLUSH(0),STACKL                                ZP60014
@SM01100 MVC   OUTCD(0),SVTDATA(R2)
@SM01102 MVC   0(0,R1),SVTDATA(R2)
@SM01104 MVC   0(0,R2),ANSWER(R1)
@SC01106 CLC   0(0,R3),C(R1)
@SC01109 CLC   NAME(0,R15),SNTDATA(R6)
@SM01111 MVC   SNTDATA(0,R4),NAME(R6)
@SC01113 CLC   SVTDATA(0,R2),OUTA1
         DS    0F
@AL00155 DC    A($SYSSCAN)             LIST WITH   3 ARGUMENT(S)
         DC    A(FW7)
         DC    A(FW0)
@AL00411 EQU   *                       LIST WITH   3 ARGUMENT(S)
@AL01048 DC    A($LASTCC)              LIST WITH   3 ARGUMENT(S)
         DC    A(FW6)
         DC    A(FW0)
@AL01052 DC    A($MAXCC)               LIST WITH   3 ARGUMENT(S)
         DC    A(FW5)
         DC    A(FW0)
*   DECLARE                         /* GENERAL PURPOSE REGISTERS     */
*     R15 FIXED(31) REG(15),                                       1153
*     R1 PTR(31) REG(1);                                           1153
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       1154
*     I031F FIXED(31) BASED,                                       1154
*     I031P PTR(31) BASED,                                         1154
*     I015F FIXED(15) BASED,                                       1154
*     I015P PTR(15) BASED,                                         1154
*     I008P PTR(8) BASED,                                          1154
*     I001C CHAR(1) BASED;                                         1154
*   END IKJCT433                                                   1155
*                                                                  1155
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IKJUPT  )                                       */
         IKJUPT
*/*%INCLUDE SYSLIB  (IKJECT  )                                       */
         IKJECT
*/*%INCLUDE SYSLIB  (IKJLSD  )                                       */
         IKJLSD
*/*%INCLUDE SYSLIB  (IKJEXEC )                                       */
*                                                                  1155
*       ;                                                          1155
         IHAPSA
         IHAASCB
         IEFZB4D0
         IEFZB4D2
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00001 DS    6F
@SA00028 DS    15F
@SA00015 DS    15F
@SA00008 DS    18F
@SA00012 DS    15F
@SA00013 DS    15F
@SA00014 DS    15F
@SA00016 DS    15F
@SA00030 DS    15F
@SA00026 DS    18F
@PC00026 DS    2F
@SA00021 DS    15F
@PC00021 DS    3F
@PC00024 DS    2F
@SA00031 DS    15F
@PC00007 DS    1F
@SA00025 DS    15F
@PC00005 DS    1F
@SA00027 DS    15F
@SA00004 DS    15F
@SA00003 DS    18F
@SA00011 DS    15F
@SA00020 DS    18F
@AL00001 DS    5A
@AFTEMPS DS    3F
IKJCT433 CSECT
         DS    0F
FW0      DC    F'0'
FW1      DC    F'1'
HW1      EQU   FW1+2
FW2      DC    F'2'
FW3      DC    F'3'
FW4      DC    F'4'
HW4      EQU   FW4+2
FW5      DC    F'5'
HW5      EQU   FW5+2
FW6      DC    F'6'
FW7      DC    F'7'
FW8      DC    F'8'
HW8      EQU   FW8+2
FW10     DC    F'10'
INDX#MAX DC    A(BILTINCT)             ORIGINALLY 10            ZP60014
FW12     DC    F'12'
HW12     EQU   FW12+2
FW16     DC    F'16'
FW20     DC    F'20'
FW31     DC    F'31'
FW57     DC    F'57'
FW200    DC    F'200'
FW220    DC    F'220'
FW249    DC    F'249'
FW252    DC    F'252'
FW300    DC    F'300'
FW312    DC    F'312'
FW900    DC    F'900'
FW904    DC    F'904'
FW908    DC    F'908'
FW912    DC    F'912'
FW916    DC    F'916'
FW920    DC    F'920'
FW924    DC    F'924'
FW932    DC    F'932'
FW936    DC    F'936'
FW940    DC    F'940'
FW944    DC    F'944'
FW960    DC    F'960'
FW999    DC    F'999'
FW4096   DC    F'4096'
FW10K    DC    F'10000'                                         ZP60014
FWNEG10  DC    F'-10'
FWNEG8   DC    F'-8'
FWNEG6   DC    F'-6'
FWNEG5   DC    F'-5'
HIGHBIT  DC    XL4'80000000'
HIBLANK  DC    XL4'40000000'
YRS1900  DC    PL4'1900000'
@DATD    DSECT
         DS    0D
ECTPTR   DS    A
WORDPTR  DS    A
SNTSAVE  DS    A
KPTR     DS    A
K        DS    A
UPTPTR   DS    A
INSRTDA  DS    A
RETURNCD DS    F
ERROR    DS    F
INDEX    DS    F
NEWLNG   DS    F
LEN1     DS    F
WI       DS    F
M        DS    F
N        DS    F
Z        DS    F
ERRSAV   DS    F
INSRTLN  DS    F
V1       DS    F
V2       DS    F
YEAR     DS    F
MONTH    DS    F
DAYS     DS    F
PCTR     DS    F
V        DS    F
SCANLIM  DS    F
HOWMUCH  DS    F
SVONEPTR DS    F
SVTWOPTR DS    F
ECDACBLK DS    AL4
         ORG   ECDACBLK
@NM00003 DS    AL1
ECDACBK2 DS    AL3
         ORG   ECDACBLK+4
SNTABPTR DS    A
SVTABPTR DS    A
NDX      DS    F
@TS00001 DS    CL15
OUTA1    DS    CL17
AREA     DS    CL16
TEMPA    EQU   AREA
TIMEOUT  EQU   AREA+4
HRS      EQU   TIMEOUT
MINUTES  EQU   TIMEOUT+2
SEC      EQU   TIMEOUT+4
DATEOUT  EQU   TIMEOUT
YR       EQU   DATEOUT
DYS      EQU   DATEOUT+4               YR IS 4 BYTES (WAS 2)    ZP60014
CSAVE    DS    CL2
         DS    CL2
WORKAREA DS    D
FRELST   DS    CL10
         ORG   FRELST
FRELST01 DS    AL1
FRELST02 DS    AL3
FRELST03 DS    AL1
FRELST04 DS    AL3
FRELST05 DS    BL1
FRELST06 DS    AL1
         ORG   FRELST+10
LCLFLAGS DS    CL2                     FLAGS IN SPARE BYTE      ZP60014
$#NRSTR  EQU   X'80'                   &NRSTR SPECIFIED         ZP60014
GETLST   DS    CL10
         ORG   GETLST
GETLST01 DS    AL4
GETLST02 DS    CL1
GETLST03 DS    AL3
GETLST04 DS    CL1
GETLST05 DS    AL1
         ORG   GETLST+10
         DS    CL2
MYSEG    DS    CL35
         ORG   MYSEG
@NM00001 DS    FL4
@NM00002 DS    CL26
OUTCD    DS    CL5
         ORG   MYSEG+35+33
TIMEDATE DS    CL8
         ORG   TIMEDATE
OUT1     DS    CL2
DELIM1   DS    CL1
OUT2     DS    CL2
DELIM2   DS    CL1
OUT3     DS    CL2
         ORG   TIMEDATE+8
OLD      DS    5A
OLD2     DS    4A
FREEADDR DS    2F
FREELEN  DS    2F
IKJCT433 CSECT
         DS    0F
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
IKJCT434 DC    V(IKJCT434)
IKJCT436 DC    V(IKJCT436)
IKJEFT30 DC    V(IKJEFT30)
IKJEFT40 DC    V(IKJEFT40)
@ALSO    DC    A(ALSO)
@MSTRLV2 DC    A(MSTRLV2)
@MSTRLV3 DC    A(MSTRLV3)
@MSGTABL DC    A(MSGTABL)
         DS    0D
ED15     DC    X'40202020202020202020202020202120'              ZP60014
$16ZEROS DC    C'0000000000000000'
$SYSSCAN DC    C'SYSSCAN'
$LASTCC  DC    C'LASTCC'
$MAXCC   DC    C'MAXCC'
$THEN    DC    C'THEN'
SLSHASTR DC    C'/*'
ASTRSLSH DC    C'*/'
$XEQUALS DC    C'X='
$DBLAMPR DC    C'&&&&'
$HEX1010 DC    X'1010'
CHR      DC    CL4'CHAR'
NUM      DC    CL3'NUM'
TRTABUP  DC    X'40,4D,5D,50,04,05,06,07,08,09,0A,0B,4C,4D,4E,4F'
         DC    X'50,11,12,13,14,15,16,17,18,19,1A,1B,5C,5D,1E,5F'
         DC    X'60,61,22,23,24,25,26,27,28,29,2A,6B,2C,2D,6E,2F'
         DC    X'30,31,32,33,34,35,36,37,38,39,3A,3B,3C,3D,7E,3F'
         DC    X'40,41,42,43,44,45,46,47,48,49,4A,4B,4C,4D,4E,4F'
         DC    X'50,51,52,53,54,55,56,57,58,59,5A,5B,5C,5D,5E,5F'
         DC    X'60,61,62,63,64,65,66,67,68,69,6A,6B,6C,6D,6E,6F'
         DC    X'70,71,72,73,74,75,76,77,78,79,7A,7B,7C,7D,7E,7F'
         DC    X'80,C1,C2,C3,C4,C5,C6,C7,C8,C9,8A,8B,8C,8D,8E,8F'
         DC    X'90,D1,D2,D3,D4,D5,D6,D7,D8,D9,9A,9B,9C,9D,9E,9F'
         DC    X'A0,A1,E2,E3,E4,E5,E6,E7,E8,E9,AA,AB,AC,AD,AE,AF'
         DC    X'F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,BA,BB,BC,BD,BE,BF'
         DC    X'C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,CA,CB,CC,CD,CE,CF'
         DC    X'D0,D1,D2,D3,D4,D5,D6,D7,D8,D9,DA,DB,DC,DD,DE,DF'
         DC    X'E0,E1,E2,E3,E4,E5,E6,E7,E8,E9,EA,EB,EC,ED,EE,EF'
         DC    X'F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,FA,FB,FC,FD,FE,FF'
TRTABDN  DC    X'00,01,02,03,04,05,06,07,08,09,0A,0B,0C,0D,0E,0F'
         DC    X'10,11,12,13,14,15,16,17,18,19,1A,1B,1C,1D,1E,1F'
         DC    X'20,21,22,23,24,25,26,27,28,29,2A,2B,2C,2D,2E,2F'
         DC    X'30,31,32,33,34,35,36,37,38,39,3A,3B,3C,3D,3E,3F'
         DC    X'00,41,42,43,44,45,46,47,48,49,4A,4B,0C,0D,0E,0F'
         DC    X'10,51,52,53,54,55,56,57,58,59,5A,5B,1C,1D,5E,1F'
         DC    X'20,21,62,63,64,65,66,67,68,69,6A,2B,6C,6D,2E,6F'
         DC    X'70,71,72,73,74,75,76,77,78,79,7A,7B,7C,7D,3E,7F'
         DC    X'80,81,82,83,84,85,86,87,88,89,8A,8B,8C,8D,8E,8F'
         DC    X'90,91,92,93,94,95,96,97,98,99,9A,9B,9C,9D,9E,9F'
         DC    X'A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AA,AB,AC,AD,AE,AF'
         DC    X'B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,BC,BD,BE,BF'
         DC    X'C0,81,82,83,84,85,86,87,88,89,CA,CB,CC,CD,CE,CF'
         DC    X'D0,91,92,93,94,95,96,97,98,99,DA,DB,DC,DD,DE,DF'
         DC    X'E0,E1,A2,A3,A4,A5,A6,A7,A8,A9,EA,EB,EC,ED,EE,EF'
         DC    X'B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,FA,FB,FC,FD,FE,FF'
         DC    CL1' '
         DS    0F
IMMLEN   DC    AL2(3)
IMMDATA  DC    CL10'STR'
VECTOR   DC    AL4(STRRTN)
         DC    AL2(4)
         DC    CL10'EVAL'
         DC    AL4(EVALRTN)
         DC    AL2(6)
         DC    CL10'SUBSTR'
         DC    AL4(SUBSTRTN)
         DC    AL2(7)
         DC    CL10'SYSTIME'
         DC    AL4(TIMERTN)
         DC    AL2(7)
         DC    CL10'SYSDATE'
         DC    AL4(DATERTN)
         DC    AL2(6)
         DC    CL10'LENGTH'
         DC    AL4(LENGRTN)
         DC    AL2(8)
         DC    CL10'DATATYPE'
         DC    AL4(DATYRTN)
         DC    AL2(7)
         DC    CL10'SYSPREF'
         DC    AL4(PREFRTN)
         DC    AL2(7)
         DC    CL10'SYSPCMD'
         DC    AL4(SYSPRTN)
         DC    AL2(7)
         DC    CL10'SYSSCMD'
         DC    AL4(SYSSRTN)
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYSSTIME'                                   ZP60014
         DC    AL4(TIMERTN)                                     ZP60014
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYSSDATE'                                   ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYSJDATE'                                   ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYS4DATE'                                   ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(9)                                           ZP60014
         DC    CL10'SYS4SDATE'                                  ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(9)                                           ZP60014
         DC    CL10'SYS4JDATE'                                  ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(9)                                           ZP60014
         DC    CL10'SYS4IDATE'                                  ZP60014
         DC    AL4(DATERTN)                                     ZP60014
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYSLTERM'                                   ZP60014
         DC    AL4(TERMRTN)                                     ZP60014
         DC    AL2(8)                                           ZP60014
         DC    CL10'SYSWTERM'                                   ZP60014
         DC    AL4(TERMRTN)                                     ZP60014
         DC    AL2(6)                                           ZP60014
         DC    CL10'SYSCPU'                                     ZP60014
         DC    AL4(CPURTN)                                      ZP60014
         DC    AL2(6)                                           ZP60014
         DC    CL10'SYSSRV'                                     ZP60014
         DC    AL4(SRVRTN)                                      ZP60014
         DC    AL2(6)                                           ZP60014
         DC    CL10'SYSDSN'                                     ZP60014
         DC    AL4(DSNRTN)                                      ZP60014
         DC    AL2(5)                                           ZP60014
$NRSTR   DC    CL10'NRSTR'                                      ZP60014
         DC    AL4(NRSTRRTN)                                    ZP60014
BILTINCT EQU   (*-IMMLEN)/16           ORIGINALLY 10            ZP60014
DAYIN    DC    H'31'         JAN
         DC    H'28'         FEB
         DC    H'31'         MAR
         DC    H'30'         APR
         DC    H'31'         MAY
         DC    H'30'         JUN
         DC    H'31'         JUL
         DC    H'31'         AUG
         DC    H'30'         SEP
         DC    H'31'         OCT
         DC    H'30'         NOV
         DC    H'31'         DEC
ROUTINES DC    A(FINDWORD)
         DC    A(SCANWORD)
         DC    A(SKIPSEPS)
         DC    A(PRINT)
         DC    A(LOCATE)
         DC    A(GLOCATE)
         DC    A(CONVERT)
         DC    A(UPDATE)
         DC    A(FINDFILE)
IKJCT433 CSECT
STACKL   STACK   DELETE=ALL,MF=L
         PUTLINE MF=L
         PUTLINE MF=L
         STAX    DEFER=NO,MF=L
         STAX    DEFER=YES,MF=L
*MACLEN  DC      A(MACLEN-STACKL)                               ZP60014
MACLEN   EQU     *-STACKL                                       ZP60014
@DATD    DSECT
STFLUSH  STACK   DELETE=ALL,MF=L
PLBLK    PUTLINE MF=L
PUTMLVL  PUTLINE MF=L
DEFERNO  STAX    DEFER=NO,MF=L
DEFERYES STAX    DEFER=YES,MF=L
         DS      0F                                             ZP60014
DSNBUFER DS      CL80                                           ZP60014
IKJCT433 CSECT
         DS    0H
PATCH    DC    ((@DATA-@PSTART)/20)X'00'
*
*   /*****************************************************************/
*   /*                                                      ZP60014  */
*   /* SYSDSN BUILT IN FUNCTION                             ZP60014  */
*   /*                                                      ZP60014  */
*   /*****************************************************************/
*
         USING DSNRTN,R7
DSNRTN   STM   R14,R12,12(R13)
         LR    R7,R15
         LA    R0,DSNWKLEN
         GETMAIN RC,LV=(0),SP=1
         LTR   R15,R15
         BNZ   DSNERR16
         LR    R14,R1
         LA    R15,DSNWKLEN
         SR    R5,R5
         MVCL  R14,R4                  CLEAR NEW WORKING STORAGE
         ST    R13,4(,R1)
         ST    R1,8(,R13)
         LR    R13,R1
         USING DSNWK,R13
         BAL   R14,WAITRTN
         TM    WAIT(COMONPTR),B'00010000'
         BNZ   DSNEXIT
         L     R15,ERROR
         LTR   R15,R15
         BNZ   DSNEXIT
         L     R1,I(,COMONPTR)
         L     R3,BUFBASE(,COMONPTR)
         LA    R2,C(R1,R3)             POINT TO VALUE
         L     R1,M
         LA    R3,C-1(R1,R3)
         SR    R3,R2                   GET LENGTH OF VALUE
         STM   R2,R3,DSNDTLS           SAVE VALUE DETAILS
         LA    R0,1
         ALR   R1,R0                   GET M + 1
         ST    R1,I(,COMONPTR)         POINT TO RESUME DATA
         LTR   R3,R3                   ANY VALUE TO PROCESS?
         BNP   DSNMISS                 NO, IT IS MISSING
         LA    R4,DSNAME               POINT TO DSNAME AREA
         LA    R5,DSNAME+44            POINT PAST DSNAME FIELD
         CLI   0(R2),X'7D'             LEADING QUOTE?
         BE    FULLDSN                 YES
         L     R14,@PC00001            NO, POINT TO THE UPT
         USING UPT,R14
         SLR   R0,R0
         ICM   R0,1,UPTPREFL           GET PREFIX LENGTH
         BZ    LOADDSN                 NO PERIOD IF PROFILE NOPREFIX
         MVC   0(7,R4),UPTPREFX        COPY THE PREFIX
         AR    R4,R0                   POINT PAST COPIED PREFIX
         MVI   0(R4),C'.'              SUPPLY A PERIOD
         LA    R4,1(,R4)               POINT PAST PERIOD
         B     LOADDSN
         DROP  R14                     UPT
FULLDSN  LA    R2,1(,R2)               POINT PAST LEADING QUOTE
         BCTR  R3,0                    DECREMENT LENGTH
         BCTR  R3,0                    DROP OFF TRAILING QUOTE
         LTR   R3,R3
         BNP   DSNBAD                  NO DATA TO PROCESS
LOADDSN  CLI   0(R2),C'('              MEMBER NAME COMING UP?
         BE    DOMEMBER                YES
         CR    R4,R5                   RUN OUT OF ROOM?
         BNL   DSNBAD                  YES, NAME TOO LONG
         MVC   0(1,R4),0(R2)           NO, COPY NAME CHARACTER
         LA    R4,1(,R4)               POINT TO NEXT TARGET BYTE
         LA    R2,1(,R2)               POINT TO NEXT SOURCE BYTE
         BCT   R3,LOADDSN
         B     FOLDNAME                REACHED END OF SUPPLIED NAME
DOMEMBER LA    R4,DSMEMBER             POINT TO MEMBER AREA
         LA    R5,DSMEMBER+8           POINT PAST MEMBER AREA
         LA    R2,1(,R2)               POINT PAST '('
         BCT   R3,LOADMEM              DECREMENT LENGTH FOR '('
         B     DSNBAD                  '(' WAS LAST CHARACTER
LOADMEM  CLI   0(R2),C')'              END OF NAME REACHED?
         BE    CHECKEND                YES
         CR    R4,R5                   RUN OUT OF ROOM?
         BNL   DSNBAD                  YES, NAME TOO LONG
         MVC   0(1,R4),0(R2)           NO, COPY NAME CHARACTER
         LA    R4,1(,R4)               POINT TO NEXT TARGET BYTE
         LA    R2,1(,R2)               POINT TO NEXT SOURCE BYTE
         BCT   R3,LOADMEM
         B     DSNBAD                  NO ')' FOUND
CHECKEND BCTR  R3,0                    DECREMENT LENGTH FOR ')'
         LTR   R3,R3                   LAST CHARACTER?
         BNZ   DSNBAD                  NO
FOLDNAME TR    DSMEMBER(8+44),TRTABUP  FOLD OUT NULLS AND LOWER CASE
         LA    R6,$S99RB
         ST    R6,$S99RBP              SET PARAMETER ADDRESS
         OI    $S99RBP,S99RBPND        MARK AS LAST
         USING S99RB,R6
         MVI   S99RBLN,S99RBEND-S99RB  SET LENGTH
         MVI   S99VERB,S99VRBAL        REQUEST ALLOCATION
         MVI   S99FLG11,S99ONCNV+S99NOCNV+S99NOMNT
         LA    R0,$TUPTR1
         ST    R0,S99TXTPP             POINT TO T.U. POINTERS
         LA    R2,TXTUNITS
         MVC   0(2,R2),KEYRTDDN        KEY
         MVC   2(2,R2),HW1             COUNT
         MVC   4(2,R2),HW8             LENGTH
         TR    6(8,R2),TRTABUP         BLANK OUT DDNAME SLOT
         ST    R2,$TUPTR1
         LA    R2,14(,R2)              POINT PAST DDNAME SLOT
         MVC   0(2,R2),KEYDSNAM        KEY
         MVC   2(2,R2),HW1             COUNT
         MVC   4(2,R2),HW44            LENGTH
         MVC   6(44,R2),DSNAME         VALUE
         ST    R2,$TUPTR2
         LA    R2,50(,R2)              POINT PAST DSNAME SPEC
         MVC   0(2,R2),KEYSTATS        KEY
         MVC   2(2,R2),HW1             COUNT
         MVC   4(2,R2),HW1             LENGTH
         MVI   6(R2),X'08'             VALUE (SHR)
         ST    R2,$TUPTR3
         OI    $TUPTR3,S99TUPLN        FLAG AS LAST
         LA    R1,$S99RBP              POINT TO PLIST
         DYNALLOC
         LTR   R15,R15                 SUCCESS?
         BNZ   DSNNTALC                NO, NOT ALLOCATED
         L     R14,PSATOLD             POINT TO TCB
         L     R14,12(,R14)            POINT TO TIOT
         LA    R14,24(,R14)            POINT TO FIRST DD ENTRY
DSNDDLP  CLI   0(R14),0                END OF TIOT?
         BE    DSNERROR                YES, SHOULD NOT HAPPEN
         CLC   4(8,R14),TXTUNITS+6     FOUND THE NEW DD ENTRY?
         BE    DSNGOTDD                YES
         IC    R15,0(,R14)             NO, GET LENGTH OF THIS ENTRY
         AR    R14,R15                 POINT TO NEXT ENTRY
         B     DSNDDLP                 KEEP LOOKING
DSNGOTDD L     R15,16(,R14)            POINT TO THE (FIRST) UCB
         LA    R0,X'C1'                GET FLAGS FOR OBTAIN SEARCH
         SLL   R0,24                   PROMOTE TO FIRST BYTE
         LA    R1,DSNAME               POINT TO DATA SET NAME
         LA    R2,28(,R15)             POINT TO UCBVOLI
         LA    R3,DSDSCB1              POINT TO OUTPUT AREA
         STM   R0,R3,DSNBUFER          CREATE OBTAIN PLIST
         OBTAIN DSNBUFER               FETCH THE VTOC ENTRY
         STC   R15,DSNFLAGS            SAVE OBTAIN RETURN CODE
         LTR   R15,R15                 SUCCESS?
         BNZ   DSNDEALC                NO, NO DSCB SO WIND IT UP
         CLI   DSMEMBER,C' '           ANY MEMBER SPECIFIED?
         BE    DSNDEALC                NO, ALL DONE
         MVI   DSNFLAGS+1,X'FF'        PREPARE FOR NOT DSORG=PO
         TM    DS1DSORG,DS1DSGPO       PARTITIONED ORG?
         BNO   DSNDEALC                NO
         MVI   DSNFLAGS+1,0            YES, NO OPEN PROBLEM YET
         MVC   DSDYNDCB,DSPDSDCB       LOAD DCB AND DDNAME
         MVC   DSDYNDCB+40(8),TXTUNITS+6
         MVI   DSOPENWK,X'80'          PREPARE TO OPEN ONE DCB
         OPEN  (DSDYNDCB,INPUT),MF=(E,DSOPENWK)
         CLI   DSNFLAGS+1,C'A'         OPEN ABEND?
         BE    DSNDEALC                YES
         STC   R15,DSNFLAGS+1          NO, SAVE OPEN RETURN CODE
         LTR   R15,R15                 DID THE OPEN WORK?
         BNZ   DSNDEALC                NO
         XC    DSNBUFER,DSNBUFER       YES
         MVI   DSNBUFER+1,1
         MVI   DSNBUFER+3,76
         MVC   DSNBUFER+4(8),DSMEMBER  PREPARE FOR BLDL
         BLDL  DSDYNDCB,DSNBUFER
         STC   R15,DSNFLAGS+2          SAVE BLDL RETURN CODE
         MVI   DSOPENWK,X'80'          PREPARE TO CLOSE ONE DCB
         CLOSE DSDYNDCB,MF=(E,DSOPENWK)
DSNDEALC MVI   S99VERB,S99VRBUN        REQUEST UNALLOCATION
         OI    $TUPTR1,S99TUPLN        FLAG FIRST TEXT UNIT AS LAST
         MVC   TXTUNITS(2),KEYDDNAM    SET CORRECT KEY
         LA    R1,$S99RBP              POINT TO PLIST
         DYNALLOC
         CLI   DSNFLAGS,0              CHECK OBTAIN RESULT
         BE    DSNOBTOK                VTOC ENTRY FOUND
         CLI   DSNFLAGS,8              CHECK OBTAIN RESULT
         BE    DSNNTFND                NOT ON VOLUME
         B     DSNERROR                I/O OR OTHER ERROR
DSNOBTOK CLI   DSNFLAGS+1,X'FF'        MEMBER FOR NON-PDS?
         BE    DSNNTPDS                YES
         CLI   DSNFLAGS+1,0            OPEN PROBLEM?
         BNE   DSNERROR                YES
         CLI   DSNFLAGS+2,4            CHECK BLDL RESULT
         BE    DSNNOMEM                MEMBER NOT IN PDS
         BH    DSNERROR                I/O ERROR
         LA    R3,$OK
         LA    R2,L'$OK
         B     DSNEXIT$                RETURN RESULT STRING
DSNNTALC CLC   DSNX35C(4),S99ERROR     INVALID DSNAME?
         BE    DSNBAD                  YES (S99INFO HAS T.U.)
         TM    S99ERROR,X'07'          CATALOGED?
         BO    DSNNTFND                NO
         CLI   S99ERROR,X'02'          SYSTEM ERROR?
         BNE   DSNERROR                YES
         CLI   S99ERROR+1,X'10'        DATA SET IN USE?
         BE    DSNINUSE                YES
         BH    DSNOFFLN                NO, NOT ON SYSTEM
DSNERROR LA    R3,$ERRPROC
         LA    R2,L'$ERRPROC
         B     DSNEXIT$                RETURN RESULT STRING
DSNINUSE LA    R3,$UNAVAIL
         LA    R2,L'$UNAVAIL
         B     DSNEXIT$                RETURN RESULT STRING
DSNNTFND LA    R3,$NTFOUND
         LA    R2,L'$NTFOUND
         B     DSNEXIT$                RETURN RESULT STRING
DSNOFFLN LA    R3,$NOTVOL
         LA    R2,L'$NOTVOL
         B     DSNEXIT$                RETURN RESULT STRING
DSNNTPDS LA    R3,$MEMSPEC
         LA    R2,L'$MEMSPEC
         B     DSNEXIT$                RETURN RESULT STRING
DSNNOMEM LA    R3,$NOMEM
         LA    R2,L'$NOMEM
         B     DSNEXIT$                RETURN RESULT STRING
DSNBAD   MVC   DSNBUFER(L'$BADNAME),$BADNAME
         LA    R14,DSNBUFER+L'$BADNAME
         LA    R15,L'DSNBUFER-L'$BADNAME
         LM    R0,R1,DSNDTLS
         ICM   R1,8,TRTABUP            SET PAD AS BLANK
         MVCL  R14,R0                  CONSTRUCT FINAL STRING
         LA    R3,DSNBUFER
         LA    R2,L'DSNBUFER
         B     DSNEXIT$                RETURN RESULT STRING
DSNMISS  LA    R3,$MISSDSN
         LA    R2,L'$MISSDSN
DSNEXIT$ ST    R3,INSRTDA
         ST    R2,INSRTLN
DSNEXIT  LA    R0,DSNWKLEN
         ICM   R0,8,HW1+1              SP=1
         LR    R1,R13
         L     R13,4(,R13)
         FREEMAIN R,LV=(0),A=(1)
         LM    R14,R12,12(R13)
         BR    R14
DSNERR16 MVC   ERROR(4),FW16           GETMAIN FAILED
         LM    R14,R12,12(R13)
         BR    R14
*                                      DCB ABEND EXIT
DSNABEND TM    3(R1),B'00001110'       ANY PROCESSING ALLOWED?
         BZR   R14                     NO
         MVI   DSNBUFER+1,C'A'         INDICATE OPEN ABENDED
         MVI   3(R1),4                 IGNORE THE ABEND
         BR    R14
         DROP  R7,R13                  DSNRTN, DSNWK
HW44     DC    H'44'
DSNX35C  DC    X'035C'                 THESE 2
KEYDSNAM DC    Y(DALDSNAM)             TOGETHER
KEYRTDDN DC    Y(DALRTDDN)
KEYSTATS DC    Y(DALSTATS)
KEYDDNAM DC    Y(DALDDNAM)
$OK      DC    C'OK'
$UNAVAIL DC    C'UNAVAILABLE DATASET'
$NTFOUND DC    C'DATASET NOT FOUND'
$NOMEM   DC    C'MEMBER NOT FOUND'
$MEMSPEC DC    C'MEMBER SPECIFIED, BUT DATASET IS NOT PARTITIONED'
$NOTVOL  DC    C'VOLUME NOT ON SYSTEM'
$MISSDSN DC    C'MISSING DATASET NAME'
$ERRPROC DC    C'ERROR PROCESSING REQUESTED DATASET'
$BADNAME DC    C'INVALID DATASET NAME, '
         DC    0F'0'
DSNEXLST DC    AL1(128+X'11'),AL3(DSNABEND)
DSPDSDCB DCB   DSORG=PO,MACRF=(R),EXLST=DSNEXLST
$DSDCBL  EQU   *-DSPDSDCB
         DC    0D'0'                   ALIGN END OF CSECT       ZP60014
DSNWK    DSECT
         DS    18F                     REGISTER SAVE AREA
DSOPENWK DS    F                       OPEN/CLOSE PLIST
DSNFLAGS DS    XL4                     FLAG BYTES
DSDYNDCB DS    XL($DSDCBL)             DCB AREA
DSNDTLS  DS    2F                      ADDRESS AND LENGTH OF NAME
DSMEMBER DS    CL8                     MEMBER NAME AREA
DSNAME   DS    CL44                    DATA SET NAME AREA
DSDSCB1  DS    CL140                   FORMAT-1 DSCB AREA
$S99RBP  DS    F                       PARAMETER LIST
$S99RB   DS    XL(S99RBEND-S99RB)      REQUEST BLOCK
$TUPTR1  DS    A                       POINTERS TO TEXT UNITS
$TUPTR2  DS    A
$TUPTR3  DS    A
$TUPTR4  DS    A
$TUPTR5  DS    A
TXTUNITS DS    0D,XL256                TEXT UNITS
         DS    0D                      ALIGN END OF DSECT
DSNWKLEN EQU   *-DSNWK
         ORG   DSNAME
         IECSDSL1 (1)
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
         DS    0D
@ENDDATD EQU   *
IKJCT433 CSECT
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
@00      EQU   R0                      EQUATES FOR REGISTERS 0-15
@01      EQU   R1
@02      EQU   R2
@03      EQU   R3
@04      EQU   R4
@05      EQU   R5
@06      EQU   R6
@07      EQU   R7
@08      EQU   R8
@09      EQU   R9
@10      EQU   R10
@11      EQU   R11
@12      EQU   R12
@13      EQU   R13
@14      EQU   R14
@15      EQU   R15
COMONPTR EQU   R8
         ENTRY ROUTINES
         ENTRY TRTABUP
         EXTRN ALSO
         EXTRN MSTRLV2
         EXTRN MSTRLV3
         EXTRN MSGTABL
INSRTCHR EQU   0
ANSWER   EQU   0
BUF      EQU   0
ROUTINE  EQU   0
WLL      EQU   0
W        EQU   WLL+4
UPVAL    EQU   0
LCVAL    EQU   0
EXECDATA EQU   0
SNTABFST EQU   EXECDATA
SVTABFST EQU   EXECDATA+4
GEXECDAT EQU   EXECDATA+8
LASTTSO  EQU   EXECDATA+12
ERACTSTR EQU   EXECDATA+20
ERACTEND EQU   EXECDATA+24
RETPTR   EQU   EXECDATA+28
EXDATFLG EQU   EXECDATA+32
STAECNT  EQU   EXECDATA+34
CONLST   EQU   EXDATFLG                B'10000000'
ERRCMD   EQU   EXDATFLG                B'01000000'
NOFLUSH  EQU   EXDATFLG                B'00100000'
SYMLST   EQU   EXDATFLG                B'00010000'
ERINCNTL EQU   EXDATFLG                B'00001000'
CMAIN    EQU   EXDATFLG                B'00000100'
NOMSG    EQU   EXDATFLG                B'00000010'
ATTNCMD  EQU   EXDATFLG                B'00000001'  UNREFD
ATINCNTL EQU   EXDATFLG+1              B'10000000'
NOLASTCC EQU   EXDATFLG+1              B'01000000'
ATACTSTR EQU   EXECDATA+44
ATACTEND EQU   EXECDATA+48
RETPTR2  EQU   EXECDATA+52
COMPROC  EQU   0
COMPRPTR EQU   COMPROC
COMPRID  EQU   COMPRPTR
COMPRNXT EQU   COMPRPTR+1
COMPRLNG EQU   COMPROC+4
SNTAB    EQU   0
SNTABNXT EQU   SNTAB
SNTABLNG EQU   SNTAB+4
SNTABUSE EQU   SNTAB+8
SNTELFST EQU   SNTAB+12
SNTELEM  EQU   0
SNTVLPTR EQU   SNTELEM
SNTGVAL  EQU   SNTVLPTR
SNTFLAGS EQU   SNTELEM+4
SNTPOSIT EQU   SNTFLAGS                B'10000000'  UNREFD
SNTKEY   EQU   SNTFLAGS                B'01000000'  UNREFD
SNTKEYW  EQU   SNTFLAGS                B'00100000'
SNTLABEL EQU   SNTFLAGS                B'00010000'
SNTNOSCN EQU   SNTFLAGS                B'00001000'
SNTNAUTH EQU   SNTFLAGS                B'00000100'
SNTEVAL  EQU   SNTFLAGS                B'00000010'
SNTLAST  EQU   SNTFLAGS                B'00000001'
SNTGLOB  EQU   SNTFLAGS+1              B'10000000'
SNTLNG   EQU   SNTELEM+6
SNTDATA  EQU   SNTELEM+8
SVTAB    EQU   0
SVTABNXT EQU   SVTAB
SVTABLNG EQU   SVTAB+4
SVTABUSE EQU   SVTAB+8
SVTABFRE EQU   SVTAB+12
SVTELFST EQU   SVTAB+16
SVTELEM  EQU   0
SVTLNG   EQU   SVTELEM
SVTORIG  EQU   SVTELEM+2
SVTDATA  EQU   SVTELEM+4
@NM00026 EQU   0
LL       EQU   @NM00026
OO       EQU   @NM00026+2
O2       EQU   @NM00026+4
C        EQU   O2
@NM00027 EQU   0
NEWLL    EQU   @NM00027
NEWOO    EQU   @NM00027+2
NEWC     EQU   @NM00027+4
@NM00029 EQU   0
INSCODE  EQU   @NM00029
INSLIST  EQU   INSCODE
INSADLSD EQU   @NM00029+1
@NM00032 EQU   0
IOSTELM  EQU   @NM00032
@NM00034 EQU   0
OPCODE   EQU   @NM00034+6
COMAREA  EQU   0
IOPL     EQU   COMAREA
BUFPTR   EQU   COMAREA+16
SNTELPTR EQU   COMAREA+20
EXECDPTR EQU   COMAREA+24
OLDLINE  EQU   COMAREA+28
BUFBASE  EQU   COMAREA+32
NEWBASE  EQU   COMAREA+36
LSDPTR   EQU   COMAREA+40
TYPE     EQU   COMAREA+44
CNVTANS  EQU   COMAREA+48
CLEN     EQU   COMAREA+52
MYO2     EQU   COMAREA+56
BEGIN    EQU   COMAREA+60
I        EQU   COMAREA+64
LEN      EQU   COMAREA+68
FREEAMT  EQU   COMAREA+72
ERCOM    EQU   COMAREA+76
REGAREA  EQU   COMAREA+80
SWSAREA  EQU   COMAREA+112
SW2AREA  EQU   COMAREA+117
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
MSGDIM   EQU   MSGTABL
@NM00009 EQU   MSGTABL+2
IDA      EQU   @NM00009
IDC      EQU   @NM00009+2
@NM00010 EQU   @NM00009+4
SECLEVEL EQU   @NM00010
MSGSEG1  EQU   @NM00009+5
SEGNUM   EQU   @NM00009+8
MSGSEG2  EQU   @NM00009+9
ECB      EQU   0
ECBCBIT  EQU   ECB
GTPB     EQU   0
GTPBLINE EQU   GTPB+4
PARM433  EQU   0
PARMDEFR EQU   PARM433
EOCLIST  EQU   PARM433
UPLIST   EQU   0
LCVALPTR EQU   UPLIST
LCVALLEN EQU   UPLIST+4
UPVALPTR EQU   UPLIST+8
UPVALLEN EQU   UPLIST+12
MINIMUM  EQU   0
NEXTCMD  EQU   0
NAME     EQU   0
NAML     EQU   0
FUNC     EQU   0
VALUE    EQU   0
VALLEN   EQU   0
LINEADDR EQU   0
TERMPUT  EQU   0
OUTFX    EQU   OUTA1
NULELEM  EQU   SVTELFST
ANSPTR   EQU   TYPE
CURID    EQU   LSDANEXT-LSD
CURADDR  EQU   LSDANEXT-LSD+1
SWS      EQU   SWSAREA
EXITA    EQU   SWS                     B'10000000'
EXITB    EQU   SWS                     B'01000000'
FREESW   EQU   SWS                     B'00100000'
WAIT     EQU   SWS                     B'00010000'
RANGE    EQU   SWS                     B'00001000'
LEAP     EQU   SWS                     B'00000100'
ALPHA    EQU   SWS                     B'00000010'
EXITC    EQU   SWS                     B'00000001'
SEPS     EQU   SWS+1                   B'10000000'
ENDDLM   EQU   SWS+1                   B'01000000'
QUIT     EQU   SWS+1                   B'00100000'
MATCH    EQU   SWS+1                   B'00010000'
SKPCOMMA EQU   SWS+1                   B'00001000'  UNREFD
SYSUPDTE EQU   SWS+1                   B'00000100'
NOAMPER  EQU   SWS+1                   B'00000010'
INSTHEN  EQU   SWS+1                   B'00000001'
SENTINAL EQU   SWS+2                   B'10000000'
SKIP1    EQU   SWS+2                   B'01000000'  UNREFD
LISTONLY EQU   SWS+2                   B'00100000'
FOLD     EQU   SWS+2                   B'00010000'
LISTED   EQU   SWS+2                   B'00001000'
ERRRANGE EQU   SWS+2                   B'00000100'
ATRANGE  EQU   SWS+2                   B'00000010'
BACKUP   EQU   SWS+2                   B'00000001'
RCABEND  EQU   SWS+3                   B'10000000'
NODATA   EQU   SWS+3                   B'01000000'
STAECNTL EQU   SWS+3                   B'00100000'  UNREFD
STABND   EQU   SWS+3                   B'00010000'  UNREFD
SW2      EQU   SW2AREA
CORE     EQU   SW2                     B'10000000'
CONTINU  EQU   SW2                     B'01000000'
RECURSE  EQU   SW2                     B'00100000'
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
SUSPENDI EQU   PARM433+4
EXITSTMT EQU   PARM433
DYNAREA  EQU   COMAREA+300
SYNREGS  EQU   COMAREA+182
STAEREGS EQU   COMAREA+118
REGAREA2 EQU   COMAREA+96
FLSEPATH EQU   @NM00034+7
INSPROM  EQU   INSCODE
INSEXEC  EQU   INSCODE
INSTERM  EQU   INSCODE
COMRCDS  EQU   COMPROC+12
COMPRUSE EQU   COMPROC+8
FILEDCBS EQU   EXECDATA+56
EXDLMPTR EQU   EXECDATA+40
GEXECCNT EQU   EXECDATA+36
EXINSAVE EQU   EXECDATA+16
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RT00148 EQU   FASTEXIT
@RF00162 EQU   @RC00156
@RC00181 EQU   @RC00174
@RC00255 EQU   @RC00254
@RC00289 EQU   @RC00282
@RC00283 EQU   @RC00282
@RF00412 EQU   @RC00404
@RC00415 EQU   @RC00404
@RF00440 EQU   @EL00004
@RC00459 EQU   @EL00004
@RC00536 EQU   @EL00005
@RF00565 EQU   @EL00008
@RF00580 EQU   @EL00008
@RF00590 EQU   @EL00008
@RC00618 EQU   @EL00008
@RF00617 EQU   @EL00008
@RF00651 EQU   @RC00648
@RF00659 EQU   @RC00656
@RF00668 EQU   @EL00009
@RC00699 EQU   @EL00011
@RF00753 EQU   @EL00014
@RF00768 EQU   @EL00015
@RF00778 EQU   @EL00016
@RF00813 EQU   @EL00020
@RC00839 EQU   @RC00838
@RC00873 EQU   @RC00860
@RF00886 EQU   @EL00022
@PB00022 EQU   @EL00021
@RC00909 EQU   @RC00908
@RC00976 EQU   @RC00975
@RC01020 EQU   @EL00025
@RF01056 EQU   @EL00025
@RF01078 EQU   @EL00027
@RC01080 EQU   @EL00027
@RF01093 EQU   @EL00028
@RC01110 EQU   @RC01109
@RF00464 EQU   @RC00459
@RC00490 EQU   @RC00459
@RC00840 EQU   @RC00839
@ENDDATA EQU   *
         END   IKJCT433,(C'PLS-III',0300,88013)
/*
//STEP05  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJCT433('ZP60014')
++MOD(IKJEFT56) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//STEP06  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IKJEFT56, TERMINAL OUTPUT ROUTINE                    '
*
*   MODIFIED BY GREG PRICE SEPTEMBER 2005 FOR USERMOD ZP60014
*
IKJEFT56 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(24)                                             0001
         DC    C'IKJEFT56  73.151'                                 0001
         DC    C' ZP60014 '
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                     0001
         BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         L     R0,@SIZDATD                                         0001
         GETMAIN R,LV=(0)
         LR    R11,R1                                              0001
         USING @DATD,R11                                           0001
         ST    R13,@SA00001+4                                      0001
         LM    R0,R1,20(R13)                                       0001
         ST    R11,8(,R13)                                         0001
         LR    R13,R11                                             0001
         MVC   @PC00001(16),0(R1)                                  0001
*                                                                  0017
*   /*****************************************************************/
*   /*                                                               */
*   /* START EXECUTABLE CODE                                         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0017
*   SAVEREG1=R1;                    /* SAVE REGISTER ONE             */
         LR    SAVEREG1,R1                                         0017
*   PTPBPTR=PTPBPTRX;               /* ESTABLISH REG POINTER         */
         L     PTPBPTR,PTPBPTRX(,R1)                               0018
*   IF TPUTTGET='1'B THEN           /* IF TGET IS INDICATED          */
         TM    TPUTTGET(PTPBPTR),B'10000000'                       0019
         BO    INVALID                                             0019
*     GOTO INVALID;                                                0020
*   TPUTOPTN=PARMOPTN;              /* FILL IN TPUT OPTON BYTE       */
         MVC   TPUTOPTN(1),PARMOPTN(PTPBPTR)                       0021
*   APOINTR=0;                      /* ZERO OUT TEMP POINTER         */
         SR    R14,R14                                             0022
         ST    R14,APOINTR                                         0022
*   IF PARMMLIN='1'B THEN           /* IS MULTILINE BIT ON ?         */
         TM    PARMMLIN(PTPBPTR),B'00001000'                       0023
         BNO   @RF00023                                            0023
*     DO;                                                          0024
*MULTLINE:                                                         0025
*       APOINTR=CHAINPTR;           /* POINT TO NEXT LINE            */
MULTLINE L     R14,PARMAOUT(,PTPBPTR)                              0025
         MVC   APOINTR(4),CHAINPTR(R14)                            0025
*       IF PARMDTMS='1'B THEN       /* IF THIS IS A DATA             */
         TM    PARMDTMS(PTPBPTR),B'00100000'                       0026
         BO    MDATA                                               0026
*         GOTO MDATA;                                              0027
*       MSGLEN=PARMAOUT->CHAINLL-4; /* CALCULATE TEXT LENGTH         */
         LH    MSGLEN,CHAINLL(,R14)                                0028
         SH    MSGLEN,HW4                                          0028
*       DO MSGIDLEN=1 TO MSGLEN;    /* SEARCH FOR FIRST BLANK        */
         LA    MSGIDLEN,1                                          0029
         B     @DE00029                                            0029
@DL00029 DS    0H                                                  0030
*         IF PARMAOUT->CHAINTXT(MSGIDLEN)=' ' THEN                 0030
         L     R14,PARMAOUT(,PTPBPTR)                              0030
         ALR   R14,MSGIDLEN                                        0030
         CLI   CHAINTXT-1(R14),C' '                                0030
         BE    CONTINUE                                            0030
*           GOTO CONTINUE;          /* IF BLANK IS FOUND EXIT        */
*       END;                        /* END MSG-ID LENGTH LOOP        */
         AH    MSGIDLEN,HW1                                        0032
@DE00029 CR    MSGIDLEN,MSGLEN                                     0032
         BNH   @DL00029                                            0032
*       GOTO INVALID;               /* IF NO BLANK FOUND             */
         B     INVALID                                             0033
*CONTINUE:                                                         0034
*       IF MSGIDLEN=1 THEN          /* IF NO MSG ID INDICATION       */
CONTINUE CH    MSGIDLEN,HW1                                        0034
         BE    STRIPID1                                            0034
*         GOTO STRIPID1;                                           0035
*       IF UPTMID='1'B THEN         /* IF NO MESSAGE I.D.            */
         L     R14,UPTPTR(,R1)                                     0036
         TM    UPTSWS-UPT(R14),UPTMID                              0036
         BNO   @RF00036                                            0036
*         DO;                       /* STRIPPING IS REQUESTED.       */
*MDATA:                                                            0038
*           LENGTH=CHAINLL-4;       /* USE LENGTH IN HEADER-4        */
MDATA    L     R14,PARMAOUT(,PTPBPTR)                              0038
         LH    R10,CHAINLL(,R14)                                   0038
         SH    R10,HW4                                             0038
         ST    R10,LENGTH                                          0038
*           TBUFADR=PARMAOUT+(4+4); /* TEXT BEGINS 8 BYTES BEGINNING
*                                      OF CHAINELEM                  */
         AH    R14,HW8                                             0039
         STCM  R14,7,TBUFADR                                       0039
*           GOTO SETUPPUT;                                         0040
         B     SETUPPUT                                            0040
*         END;                      /* END NO-STRIP PROCESSING       */
*STRIPID1:                                                         0042
*       LENGTH=CHAINLL-(4+MSGIDLEN);/* LENGTH OF OUTPUT LINE = THE 0042
*                                      LENGTH OF ENTIRE RECORD - THE
*                                      THE LENGTH OF THE MSG-ID + 4  */
@RF00036 DS    0H                                                  0042
STRIPID1 L     R14,PARMAOUT(,PTPBPTR)                              0042
         LA    R10,4                                               0042
         AR    R10,MSGIDLEN                                        0042
         LCR   R10,R10                                             0042
         AH    R10,CHAINLL(,R14)                                   0042
         ST    R10,LENGTH                                          0042
*       TBUFADR=PARMAOUT+(4+4+MSGIDLEN);/* THE ADDRESS OF THE OUT PUT
*                                      LINE = THE ADDRESS OF THE   0043
*                                      CHAIN ELEMENT + THE LENGTH OF
*                                      THE CHAIN FORWARD POINTER + 0043
*                                      THE LENGTH OF THE MSG-ID + 4
*                                      (HEADR OF VRIABLE RCRD        */
         LA    R10,8                                               0043
         AR    R10,MSGIDLEN                                        0043
         AR    R14,R10                                             0043
         STCM  R14,7,TBUFADR                                       0043
*       GOTO SETUPPUT;                                             0044
*                                                                  0044
         B     SETUPPUT                                            0044
*       /*************************************************************/
*       /*                                                           */
*       /* END STRIP ID PROCESSING                                   */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0045
*     END;                          /* END MULT-LINE PROCESING       */
*   IF PARMDTMS='1'B THEN           /* IF THIS IS A DATA LINE        */
@RF00023 TM    PARMDTMS(PTPBPTR),B'00100000'                       0046
         BO    DATALINE                                            0046
*     GOTO DATALINE;                                               0047
*                                                                  0047
*   /*****************************************************************/
*   /*                                                               */
*   /* SINGLE LEVEL MESSAGE PROCESSING                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0048
*   MSGLEN=PARMAOUT->MSGLL-4;                                      0048
         L     R14,PARMAOUT(,PTPBPTR)                              0048
         LH    MSGLEN,MSGLL(,R14)                                  0048
         SH    MSGLEN,HW4                                          0048
*   DO MSGIDLEN=1 TO MSGLEN;        /* FIND FIRST BLANK              */
         LA    MSGIDLEN,1                                          0049
         B     @DE00049                                            0049
@DL00049 DS    0H                                                  0050
*     IF PARMAOUT->MSGTEXT(MSGIDLEN)=' ' THEN/* BLANK FOUND BRANCH   */
         L     R14,PARMAOUT(,PTPBPTR)                              0050
         ALR   R14,MSGIDLEN                                        0050
         CLI   MSGTEXT-1(R14),C' '                                 0050
         BE    CONTINU2                                            0050
*       GOTO CONTINU2;                                             0051
*   END;                            /* END DETERMINE LENGTH LOOP     */
         AH    MSGIDLEN,HW1                                        0052
@DE00049 CR    MSGIDLEN,MSGLEN                                     0052
         BNH   @DL00049                                            0052
*   GOTO INVALID;                   /* IF NO BLANK IS FOUND          */
         B     INVALID                                             0053
*CONTINU2:                                                         0054
*   IF MSGIDLEN=1 THEN                                             0054
CONTINU2 CH    MSGIDLEN,HW1                                        0054
         BE    STRIPID                                             0054
*     GOTO STRIPID;                                                0055
*   IF UPTMID='0'B THEN             /* DISPLAY ID BIT OFF            */
         L     R14,UPTPTR(,R1)                                     0056
         TM    UPTSWS-UPT(R14),UPTMID                              0056
         BNZ   @RF00056                                            0056
*     DO;                                                          0057
*STRIPID:                                                          0058
*       LENGTH=MSGLL-(4+MSGIDLEN);  /* LENGTH OF OUTPUT LINE = LENGHT
*                                      OF MESSAGE BUFFER - THE LENGTH
*                                      OF THE MSG ID + 4             */
STRIPID  L     R14,PARMAOUT(,PTPBPTR)                              0058
         LA    R10,4                                               0058
         AR    R10,MSGIDLEN                                        0058
         LH    R4,MSGLL(,R14)                                      0058
         SR    R4,R10                                              0058
         ST    R4,LENGTH                                           0058
*       TBUFADR=PARMAOUT+(4+MSGIDLEN);/* ADDRESS OF THE OUTPUT LINE =
*                                      ADDRESS OF MSG BUFFER + THE 0059
*                                      LENGTH OF THE MSGID + 4       */
         AR    R14,R10                                             0059
         STCM  R14,7,TBUFADR                                       0059
*       GOTO SETUPPUT;                                             0060
         B     SETUPPUT                                            0060
*     END;                                                         0061
*                                                                  0061
*   /*****************************************************************/
*   /*                                                               */
*   /* NO MESSAGE I.D. STRIPPING REQUIRED                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0062
*   ELSE                                                           0062
*     DO;                                                          0062
@RF00056 DS    0H                                                  0063
*DATALINE:                                                         0063
*       LENGTH=MSGLL-4;             /* LENGTH OF OUTPUT LINE = LENGTH
*                                      OF OUTPUT RECORD - 4          */
DATALINE LA    R14,4                                               0063
         L     R10,PARMAOUT(,PTPBPTR)                              0063
         LH    R4,MSGLL(,R10)                                      0063
         SR    R4,R14                                              0063
         ST    R4,LENGTH                                           0063
*       TBUFADR=PARMAOUT+4;         /* ADDRESS OF OUTPUT LINE =    0064
*                                      ADDRESS OF OUTPUT RECORD + 4  */
         AR    R10,R14                                             0064
         STCM  R10,7,TBUFADR                                       0064
*     END;                                                         0065
*SETUPPUT:                                                         0066
*   IF ECBCBIT='1'B THEN            /* FLUSH BIT ON                  */
SETUPPUT L     R14,@PC00001+8                                      0066
         TM    ECBCBIT(R14),B'01000000'                            0066
         BNO   @RF00066                                            0066
*     DO;                                                          0067
*       RETURNCD=4;                 /* SET RETURN CODE               */
         LA    RETURNCD,4                                          0068
*       GOTO EXIT;                                                 0069
         B     EXIT                                                0069
*     END;                                                         0070
@RF00066 EQU   *                                                ZP60014
         L     R14,ECTPTR(,R1)         --> ECT                  ZP60014
         L     R14,ECTIOWA-ECT(,R14)   --> STPB                 ZP60014
         L     R14,IOSTELM(,R14)       --> STACK ELEMENT        ZP60014
         ICM   R0,7,1(R14)             --> LSD                  ZP60014
         BZ    CHEKGRND                NOT IN CLIST             ZP60014
         TM    IOSTELM(R14),X'08'      INSEXEC SET?             ZP60014
         BZ    CHEKGRND                NO, NOT IN CLIST         ZP60014
         LA    R0,SOUTTRAP             POINT TO NAME            ZP60014
         ST    R0,LCVALPTR             SET NAME ADDRESS         ZP60014
         LA    R0,10                   GET NAME LENGTH          ZP60014
         ST    R0,LCVALLEN             SET NAME LENGTH          ZP60014
         MVC   @AL00001(12),@PC00001   UPT,ECT,ECB ADDRESSES    ZP60014
         LA    R0,UPLIST                                        ZP60014
         ST    R0,@AL00001+12          COMPLETE PLIST           ZP60014
         LA    R1,@AL00001             POINT TO PLIST           ZP60014
         L     R15,IKJGET#                                      ZP60014
         BALR  R14,R15                 GET SYSOUTTRAP VALUE     ZP60014
         LR    R1,SAVEREG1             RESTORE PARM REG         ZP60014
         L     R14,ECTPTR(,R1)         --> ECT                  ZP60014
         LTR   R15,R15                 POSITIVE VALUE?          ZP60014
         BP    TRAPLINE                YES                      ZP60014
         SR    R0,R0                   NO                       ZP60014
         ST    R0,44(,R14)             RESET ECTNUM             ZP60014
         B     CHEKGRND                GO OUTPUT DISPLAY LINE   ZP60014
TRAPLINE L     R0,44(,R14)             GET LATEST OUTLINE       ZP60014
         CR    R0,R15                  TRAPPING THIS LINE?      ZP60014
         BNL   CHEKGRND                NO, GO DISPLAY IT        ZP60014
         A     R0,FW1                  YES, INCREMENT COUNT     ZP60014
         ST    R0,44(,R14)             UPDATE ECTNUM            ZP60014
         CVD   R0,DECDBL               GET NEW COUNT IN DECIMAL ZP60014
         OI    DECDBL+7,X'0F'          PREPARE FOR UNPACK       ZP60014
         MVC   LINENAME(10),SOUTLINE   LOAD NAME BASE           ZP60014
         UNPK  LINENAME+10(10),DECDBL  ADD NUMERIC PART         ZP60014
         LA    R0,20                   GET NAME LENGTH          ZP60014
NAME0LP  CLI   LINENAME+10,C'0'        LEADING ZERO?            ZP60014
         BNE   NAMERDY                 NO, FINISHED NAME EDIT   ZP60014
         MVC   LINENAME+10(9),LINENAME+11                       ZP60014
         BCT   R0,NAME0LP              SUPPRESS LEADING ZERO    ZP60014
NAMERDY  ST    R0,LCVALLEN             SET NAME LENGTH          ZP60014
         LA    R1,LINENAME             POINT TO NAME            ZP60014
         ST    R1,LCVALPTR             SET NAME ADDRESS         ZP60014
         L     R0,LENGTH               GET DATA LENGTH          ZP60014
         ST    R0,UPVALLEN             SET DATA LENGTH          ZP60014
         L     R1,TPUTPARM             GET DATA ADDRESS         ZP60014
         LA    R1,0(,R1)               CLEAR FLAG BYTE          ZP60014
         ST    R1,UPVALPTR             SET DATA ADDRESS         ZP60014
         LA    R1,@AL00001             POINT TO PLIST           ZP60014
         L     R15,IKJINIT                                      ZP60014
         BALR  R14,R15                 CREATE SYSOUTLINE####    ZP60014
         LR    R1,SAVEREG1             RESTORE PARM REG         ZP60014
         LTR   R15,R15                 SUCCESS?                 ZP60014
         BNZ   CHEKGRND                NO, BETTER SHOW LINE     ZP60014
         L     R14,LCVALLEN            GET NAME LENGTH          ZP60014
         LA    R0,10                                            ZP60014
         SR    R14,R0                  GET LENGTH OF NUMBER     ZP60014
         ST    R14,UPVALLEN            SET NEW DATA LENGTH      ZP60014
         ST    R0,LCVALLEN             SET NAME LENGTH          ZP60014
         LA    R1,LINENAME+10          POINT TO NUMBER          ZP60014
         ST    R1,UPVALPTR             SET NEW DATA ADDRESS     ZP60014
         LA    R1,@AL00001             POINT TO PLIST           ZP60014
         L     R15,IKJUPDT                                      ZP60014
         BALR  R14,R15                 UPDATE SYSOUTLINE        ZP60014
         LR    R1,SAVEREG1             RESTORE PARM REG         ZP60014
         LTR   R15,R15                 SUCCESS?                 ZP60014
         BZ    @RF00080                YES, LINE PROCESSED OK   ZP60014
CHEKGRND EQU   *                                                ZP60014
*   IF INSOTDD='1'B THEN            /* IF IN BACKGROUND MODE         */
         L     R14,ECTPTR(,R1)                                     0071
         L     R10,ECTIOWA-ECT(,R14)                               0071
         L     R10,IOSTELM(,R10)                                   0071
         TM    INSOTDD(R10),B'00010000'                            0071
         BNO   @RF00071                                            0071
*     CALL IKJRBBMC(TBUFADR,LENGTH,ECT,1);                         0072
         LA    R10,TBUFADR                                         0072
         ST    R10,@AL00001                                        0072
         LA    R10,LENGTH                                          0072
         ST    R10,@AL00001+4                                      0072
         ST    R14,@AL00001+8                                      0072
         LA    R14,FW1                                             0072
         ST    R14,@AL00001+12                                     0072
         L     R15,IKJRBBMC                                        0072
         LA    R1,@AL00001                                         0072
         BALR  R14,R15                                             0072
*   ELSE                                                           0073
*     DO;                                                          0073
         B     @RC00071                                            0073
@RF00071 DS    0H                                                  0074
*       R0=LENGTH;                  /* FILL IN PARM REGISTERS        */
         L     R0,LENGTH                                           0074
*       R1=TPUTPARM;                                               0075
         L     R1,TPUTPARM                                         0075
*       GEN(TPUT (1),(0),R);        /* TPUT LINE TO TERMINAL         */
         TPUT (1),(0),R
*     END;                                                         0077
*   TPUTCODE=R15;                   /* SAVE RETURN CODE              */
@RC00071 LR    TPUTCODE,R15                                        0078
*   R1=SAVEREG1;                    /* RESTORE PARM REG              */
         LR    R1,SAVEREG1                                         0079
*   IF TPUTCODE^=0                  /* IF ERROR FROM TPUT      Y02996*/
*     THEN                                                         0080
         LTR   TPUTCODE,TPUTCODE                                   0080
         BZ    @RF00080                                            0080
*     DO;                                                          0081
*       IF TPUTCODE=8               /* ATTN INTERRUPT          Y02996*/
*         THEN                                                     0082
         CH    TPUTCODE,HW8                                        0082
         BNE   @RF00082                                            0082
*         DO;                                                      0083
*           RETURNCD=4;             /* SET ATTN RETURNCD       Y02996*/
         LA    RETURNCD,4                                          0084
*           GO TO EXIT;             /* END                     Y02996*/
         B     EXIT                                                0085
*         END;                                                     0086
*       IF TPUTCODE=4 THEN          /* NO WAIT ERROR                 */
@RF00082 CH    TPUTCODE,HW4                                        0087
         BNE   @RF00087                                            0087
*         DO;                                                      0088
*           RETURNCD=8;             /* INDICATE NOWAIT ERROR         */
         LA    RETURNCD,8                                          0089
*           GOTO EXIT;                                             0090
         B     EXIT                                                0090
*         END;                                                     0091
*       IF TPUTCODE=20              /* IF LINEDROP SET         Y02996*/
*         THEN                                                     0092
@RF00087 LA    R14,20                                              0092
         CR    TPUTCODE,R14                                        0092
         BNE   @RF00092                                            0092
*         RETURNCD=20;              /* LINEDROP RC             Y02996*/
         LR    RETURNCD,R14                                        0093
*       ELSE                        /* ELSE SET INVAL          Y02996*/
*INVALID:                                                          0094
*         RETURNCD=12;              /* PARMS RC                Y02996*/
         B     EXIT                                                0094
@RF00092 DS    0H                                                  0094
INVALID  LA    RETURNCD,12                                         0094
*       GOTO EXIT;                  /* END                     Y02996*/
         B     EXIT                                                0095
*     END;                                                         0096
*   IF APOINTER^=0 THEN             /* END OF MULTI-LINE CHAIN       */
@RF00080 ICM   R14,7,APOINTER                                      0097
         BZ    @RF00097                                            0097
*     DO;                                                          0098
*       PARMAOUT=APOINTR;           /* POINT TO NEXT LINE            */
         MVC   PARMAOUT(4,PTPBPTR),APOINTR                         0099
*       GOTO MULTLINE;                                             0100
         B     MULTLINE                                            0100
*     END;                                                         0101
*   RETURNCD=0;                     /* INDICATE NORMAL END           */
@RF00097 SR    RETURNCD,RETURNCD                                   0102
*EXIT:                                                             0103
*   RETURN CODE(RETURNCD);                                         0103
EXIT     L     R13,4(,R13)                                         0103
         L     R0,@SIZDATD                                         0103
         LR    R1,R11                                              0103
         FREEMAIN R,LV=(0),A=(1)
         LR    R15,RETURNCD                                        0103
         L     R14,12(,R13)                                        0103
         LM    R0,R12,20(R13)                                      0103
         BR    R14                                                 0103
*   END IKJEFT56                                                   0104
@DATA    DS    0H
HW4      DC    H'4'
HW8      DC    H'8'
         DS    0F
FW1      DC    F'1'
HW1      EQU   FW1+2
SOUTLINE DC    CL10'SYSOUTLINE'                                 ZP60014
SOUTTRAP DC    CL10'SYSOUTTRAP'                                 ZP60014
         DS    0F
@SIZDATD DC    AL1(1)
         DC    AL3(@ENDDATD-@DATD)
IKJRBBMC DC    V(IKJRBBMC)
IKJUPDT  DC    V(IKJUPDT)                                       ZP60014
IKJINIT  DC    V(IKJINIT)                                       ZP60014
IKJGET#  DC    V(IKJGET#)                                       ZP60014
PATCH    DC    5D'0'
*                                                                  0104
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IKJUPT  )                                       */
         IKJUPT
*/*%INCLUDE SYSLIB  (IKJECT  )                                       */
         IKJECT
*                                                                  0104
*       ;                                                          0104
@DATD    DSECT
@SA00001 DS    18F
@PC00001 DS    4F
@AL00001 DS    4A
         DS    0D
TPUTPARM DS    AL4
         ORG   TPUTPARM
TPUTOPTN DS    AL1
TBUFADR  DS    AL3
         ORG   TPUTPARM+4
APOINTR  DS    AL4
         ORG   APOINTR
@NM00007 DS    AL1
APOINTER DS    AL3
         ORG   APOINTR+4
LENGTH   DS    F
UPLIST   EQU   *                                                ZP60014
LCVALPTR DS    A                                                ZP60014
LCVALLEN DS    F                                                ZP60014
UPVALPTR DS    A                                                ZP60014
UPVALLEN DS    F                                                ZP60014
LINENAME DS    CL20                                             ZP60014
DECDBL   DS    D                                                ZP60014
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
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
RETURNCD EQU   R2
SAVEREG1 EQU   R5
MSGIDLEN EQU   R3
MSGLEN   EQU   R4
TPUTCODE EQU   R3
PTPBPTR  EQU   R2
XPRMLIST EQU   0
UPTPTR   EQU   XPRMLIST
ECTPTR   EQU   XPRMLIST+4
ECBPTR   EQU   XPRMLIST+8
PTPBPTRX EQU   XPRMLIST+12
PTPB     EQU   0
PARMCNTL EQU   PTPB
PARMDTMS EQU   PARMCNTL
PARMMLIN EQU   PARMCNTL
PARMTPUT EQU   PTPB+2
PARMOPTN EQU   PARMTPUT
TPUTTGET EQU   PARMOPTN
PARMAOUT EQU   PTPB+4
CHAIN    EQU   0
CHAINPTR EQU   CHAIN
CHAINLL  EQU   CHAIN+4
CHAINTXT EQU   CHAIN+8
MESSAGE  EQU   0
MSGLL    EQU   MESSAGE
MSGTEXT  EQU   MESSAGE+4
IOSRL    EQU   0
IOSTELM  EQU   IOSRL
INSTACK  EQU   0
INSCODE  EQU   INSTACK
INSOTDD  EQU   INSCODE
UPTX     EQU   0
ECTX     EQU   0
ECB      EQU   0
ECBCBIT  EQU   ECB
PTPBX    EQU   0
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
ECBCOMPC EQU   ECB
ECBWBIT  EQU   ECB
INSADLSD EQU   INSTACK+1
INSLIST  EQU   INSCODE
INSPROC  EQU   INSCODE
INSPROM  EQU   INSCODE
INSRSVD  EQU   INSCODE
INSINDD  EQU   INSCODE
INSSTOR  EQU   INSCODE
INSTERM  EQU   INSCODE
IOSUNUSD EQU   IOSRL+12
IOSNELM  EQU   IOSRL+10
IOSTLEN  EQU   IOSRL+8
IOSBELM  EQU   IOSRL+4
MSGOO    EQU   MESSAGE+2
CHAINOO  EQU   CHAIN+6
@NM00006 EQU   PARMTPUT+1
PARMFORM EQU   PARMCNTL+1
PARMDMND EQU   PARMCNTL+1
PARMMODE EQU   PARMCNTL+1
PARMPRMT EQU   PARMCNTL
PARMIFOR EQU   PARMCNTL
PARMMLEV EQU   PARMCNTL
PARMSNGL EQU   PARMCNTL
PARMPUT  EQU   PARMCNTL
PARMPBT0 EQU   PARMCNTL
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@ENDDATA EQU   *
         END   IKJEFT56
/*
//STEP07  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKJEFT56('ZP60014')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//STEP08  EXEC SMPREC
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE SELECT(ZP60014).
/*
//STEP09  EXEC SMPAPP
//SMPCNTL  DD  *
  APPLY SELECT(ZP60014) DIS(WRITE).
/*
//
