//ZP60037  JOB (SYSGEN),'J02 M48: ZP60037',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  SUPPORT ANY MVS-SUPPORTED DASD FOR LOGREC - SYSMOD 3 OF 3
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60037)          /* FIX LOGREC DEVICE TYPE SUPPORT */  .
++VER(Z038) FMID(EER1400)
 /*
   PROBLEM DESCRIPTION:
     MVS DOES NOT SUPPORT SYS1.LOGREC ON ALL DASD DEVICE TYPES.
       THE DASD DEVICE TYPES THAT EREP I/O SERVICES SUPPORTS IS
       HARD-CODED WITHIN THE MODULE.  THIS MEANS THAT IN ORDER TO
       MAINTAIN A FUNCTIONING LOGREC FACILTY THE SYSTEM RESIDENCE
       VOLUME IS LIMITED TO THE ORIGINAL SET OF DASD DEVICE TYPES
       AND CANNOT BE MIGRATED TO EXPLOIT LARGER VOLUME GEOMETRIES
       AS SUPPORT FOR NEWER DEVICES IS ADDED TO MVS.

       THIS USERMOD SHIPS A VERSION OF IFCIOHND WHICH USES THE MVS
       TRKCALC SERVICE TO OBTAIN DASD GEOMETRY DEPENDENT DETAILS
       INSTEAD OF PRE-CODED VALUES SO THAT NOW IT SUPPORTS ANY
       DASD DEVICE TYPE SUPPORTED BY THE SYSTEM.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 37.

     THIS IS SYSMOD NUMBER 3 OF 3 IN A PACKAGE TO GENERALIZE LOGREC
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
       IFCIOHND
 */.
++MOD(IFCIOHND) DISTLIB(AOSCD).
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
         TITLE 'IFCIOHND - IO SERVICES MODULE FOR IFCEREP1'             00000100
*                                                                       00000200
*        NAME - IFCIOHND                                                00000300
*                                                                       00000400
*        STATUS -                                                       00000500
*        SOURCE CODE AS DISTRIBUTED WAS UPDATED BY DISASSEMBLY          00000600
*        TO REFLECT THE OWNING FMID OF EER1400. THE LOAD MODULE         00000700
*        ASSEMBLED FROM THE UPDATED SOURCE MATCHED THE MVS 3.8J         00000800
*        DISTRIBUTION MODULE PRIOR TO THE ENHANCEMENTS AS LISTED        00000900
*        BELOW                                                          00001000
*                                                                       00001100
*        ENHANCEMENTS -                                                 00001200
*        1. REMOVAL OF THE DASD GEOMETRY DATA TABLE                     00001300
*        2. ALL DASD GEOMETRY DATA IS OBTAINED DYNAMICALLY              00001400
*           FROM IECZDTAB AND/OR CALCULATED BY THE TRKCALC SERVICE      00001500
*        3. IFCIOHND WILL RE-INITIALIZE A LOGREC DATASET ON             00001600
*           ANY DASD SUPPORTED BY THE EXECUTING MVS 3.8 SYSTEM          00001700
*        4. USE OF STANDARD SYSTEM MAPPING MACROS                       00001800
*        5. NUMEROUS CHANGES TO IMPROVE SOURCE READABILITY              00001900
*        6. STANDARDIZED USE OF BASE REGISTERS                          00002000
*        7. Z/OS APAR #IO19631 HAS BEEN RETROFITTED TO IFCIOHND.        00002100
*           USERS WITH READ-ONLY ACCESS CAN NOW RUN IFCEREP1 IF         00002200
*           ZERO=N SPECIFIED OR DEFAULTED. WITH READ-ONLY ACCESS        00002300
*           AN ATTEMPT TO UPDATE THE LOGREC DATA SET BY SPECIFYING      00002400
*           ZERO=Y WILL RECEIVE A RACF ABENDS913.                       00002500
*                                                                       00002600
IFCIOHND CSECT                                                          00002700
*                                                                       00002800
         SAVE  (14,12),,'IFCIOHND ZP60037 &SYSDATE &SYSTIME'            00002900
*                                                                       00003000
         LA    R8,0(,R15)                                               00003100
         LA    R15,1                                                    00003200
         LA    R9,4095(R15,R8)                                          00003300
         LA    R10,4095(R15,R9)                                         00003400
         LA    R11,4095(R15,R10)                                        00003500
*                                                                       00003600
         USING IFCIOHND,R8,R9,R10,R11                                   00003700
*                                                                       00003800
         ST    R13,@SA00001+4                                           00003900
         LA    R14,@SA00001                                             00004000
         ST    R14,8(,R13)              CHAIN SAVE AREAS                00004100
         LR    R13,R14                                                  00004200
*DCL ADDNUM(1) CHAR(1) BASED(ADDNXT);                                   00004300
*DCL NEXTBY FIXED(31) INIT(0);                                          00004400
*DCL ADDNXT FIXED(31) INIT(0);                                          00004500
*DCL NUMBER FIXED(31) INIT(0);                                          00004600
*DCL GETTIT CHAR(4) DEFINED(GETHIS); /*ALPHA ACCESS OF GETHIS*/         00004700
*DCL LISTPTR PTR(31) BASED(PTRSPOT); /*POINTER TO LIST PARAMETER*/      00004800
*DCL INARG  FIXED(15) BASED(LISTPTR); /*INPUT PARAMETER WITH         */ 00004900
*                                     /*SERVICE REQUESTED CODE*/        00005000
*DCL LINECTT FIXED(15) INIT(0); /*RUNNING LINE COUNT FOR PAGE SKIPIN */ 00005100
*DCL LINEMAX FIXED(15) INIT(0); /*MAXIMUM LINES PER PAGE, TOURIST*/     00005200
*DCL CCSAVE  CHAR(1) INIT(' '); /*CARRIAGE CONTROL CHAR SAVE SPOT*/     00005300
*DCL BLANKS CHAR(134) INIT(' '); /*BLANKS FOR PRINT LINE                00005400
*DCL MSGNO  FIXED(31)  INIT(0); /*MESSAGE TEXT INDEX                    00005500
*DCL BADNEWS FIXED(31) INIT(0); /*RETURN CODE SAVE ITEM                 00005600
*DCL PACKPAS  CHAR(4) INIT('    ');                                     00005700
*DCL CHARPAS  CHAR(8) INIT('        ');                                 00005800
*DCL PASSNUM  FIXED(31) INIT(0);                                        00005900
*DCL EDIFILD(9) CHAR(1);                                                00006000
*DCL EDFLD CHAR(9) DEFINED(EDIFILD);                                    00006100
*DCL CTPTR(10) FIXED(31) DEFINED(CONTPTR);                              00006200
*/********************************************************************/ 00006300
*DCL CLOSEALL BIT(16) CONSTANT('FFFF'X); /*EOF ALL CLOSED MASK*/        00006400
*DCL SKIPNEW BIT(8) CONSTANT('8B'X);          /* SKIP TO CHANNEL ONE*/  00006500
*DCL NODSETS BIT(16) CONSTANT('0000'X); /*NO DATA SETS ASKED OPEN*/     00006600
*DCL DBLWORD BIT(32) CONSTANT('00000007'X); /*NO DOUBLE WORD MASK*/     00006700
*DCL NULL    BIT(32) CONSTANT('00000000'X);                             00006800
*DCL CHARZERO BIT(8) CONSTANT('F0'X);                                   00006900
*DCL BLANK    BIT(8) CONSTANT('40'X);                                   00007000
*/********************************************************************/ 00007100
*DCL  1  IOPLINE  CHAR(134)  BASED(PRINTADR), /*PRINT WORK AREA DSECT*/ 00007200
*      2 PRINTDS  CHAR(1), /*DATA SET IS ERREPT IF F0                */ 00007300
*                          /*DATA SET IS TOURIST IF F1               */ 00007400
*      2 PRINTCC  CHAR(1), /*CARRIAGE CONTROL CHARACTER, IGNORED*/      00007500
*                          /*IF FROM USER FOR TOURIST DATA SET*/        00007600
*      2 PRINTBDY CHAR(132); /*LINE TO BE PRINTED, WILL BE CLEARED*/    00007700
*                            /*TO BLANKS BEFORE RETURN TO USER*/        00007800
*/********************************************************************/ 00007900
*DCL PRNTBF CHAR(134) BDY(DWORD); /*INTERNAL PRINT LINE BUFFER*/        00008000
*/********************************************************************/ 00008100
*DCL 1  BUF1 BDY(DWORD), /*WORK AREA FOR QSAM RECORDS                */ 00008200
*     2 BUF1RDW, /*RECORD DESCRIPTOR WORD                            */ 00008300
*     3 BUF1RDWL FIXED(15) INIT(0), /*QSAM RECORD LENGTH             */ 00008400
*     3 BUF1L    FIXED(15) INIT(0), /*QSAM ZEROS                     */ 00008500
*     2 BUF1DTA  CHAR(2000);    /*MAXIMUM SYS1.LOGREC RECORD LENGTH*/   00008600
*/*******************************************************************/  00008700
*DCL  1 RECTB BASED(RUNPTR), /*ZEROALL RECORD SAVE AREA DSECT*/         00008800
*     2 FORPTR  PTR(31),                                                00008900
*     2 JUNK,                                                           00009000
*     3 JUNK1   CHAR(5),                                                00009100
*     3 JUNK2   CHAR(1),                                                00009200
*     3 JUNK3   FIXED(15),                                              00009300
*     2 RECSPT  CHAR(1894);                                             00009400
*/*******************************************************************/  00009500
* DCL 1 IOSWTCH CHAR(3) INIT('000000'X), /*NEW SWITCHES              */ 00009600
*     2 HISTOPN BIT(1), /*OUTPUT HIST IS OPENED FLAG                 */ 00009700
*     2 SECGET   BIT(1), /*ZEROALL GETMAIN SWITCH                    */ 00009800
*     2 FLIPFLOP BIT(1), /*OFF SERLOG READSEQ, ON ACCIN READSEQ      */ 00009900
*     2 HISTYES BIT(1), /*INDICATES ACCIN INPUT SPECIFIED            */ 00010000
*     2 DBLYES  BIT(1), /*DOUBLE WORD CREATION LOOP EXIT SWITCH      */ 00010100
*     2 LASTLOOP BIT(1),  /*ZEROALL PROCESSING                       */ 00010200
*     2 TOURERR BIT(1), /*ERROR WITH TOURIST DATA SET                */ 00010300
*     2 INHISO  BIT(1), /*INPUT HIST IS IPENED FLAG                  */ 00010400
*     2 SERLO   BIT(1), /*SERLOG IS OPENED FLAG                      */ 00010500
*     2 TWOTRY  BIT(1), /*OFF IMPLIES 1ST EOF READING SERLOG SEQ     */ 00010600
*     2 INDIR   BIT(1), /*DIRECTWK IS OPENED FOR SEQ WRITE           */ 00010700
*     2 NONBLANK BIT(1), /*NONBLANK LOOP FINDER SWITCH               */ 00010800
*     2 CLOSE2  BIT(1), /*DIRECTWK IS OPENED FOR DIR WRITE           */ 00010900
*     2 SYSOPN  BIT(1), /*SYSIN IS OPENED SWITCH                     */ 00011000
*     2 TOUROPN  BIT(1), /*TOURIST DS IS OPENED SWITCH               */ 00011100
*     2 ERPTOPN  BIT(1), /*REGULAR PRINT D.S. HAS BEEN OPENED        */ 00011200
*     2 READFR   BIT(1), /*FRAMES ARE BEING READ SWITCH              */ 00011300
*     2 SCRATCH  BIT(1), /*LAST OPERATION WAS READ DIRECT            */ 00011400
*     2 SNAPOP  BIT(1); /*SNAP DS HAS BEEN OPENED SWITCH             */ 00011500
*/********************************************************************/ 00011600
*DCL    1   HDRBUFF   CHAR(64) BDY(DWORD),   /* EXTRACTOR WORK AREA  */ 00011700
*          2   HDRSEEK             CHAR(8),  /* SEEK FIELD FOR I/O   */ 00011800
*             3   HDRM             CHAR(1) INIT('00'X),                 00011900
*             3   HDRBB            CHAR(2) INIT('0000'X),               00012000
*             3   HDRCC            CHAR(2) INIT('0000'X),               00012100
*             3   HDRHH            CHAR(2) INIT('0000'X),               00012200
*             3   HDRR             CHAR(1) INIT('00'X),                 00012300
*                                                                       00012400
*        LOGREC HEADER RECORD -                                         00012500
*                                                                       00012600
*       *************************************************************** 00012700
*       *              *                                *     HIGH    * 00012800
*  +0   *  F F F F     *          LOW EXTENT            *   EXTENT    * 00012900
*       *              *            (CCHH)              *      (CC)   * 00013000
*       *************************************************************** 00013100
*       * HIGH EXTENT  *       *         RECORD ENTRY AREA ADDRESS    * 00013200
*  +8   *   (CONT.)    * SPARE *                (BBCCHHR)             * 00013300
*       *    (HH)      *       *                                      * 00013400
*       *************************************************************** 00013500
*       *  REC. ENTRY  *  REMAINING     *  TOTAL BYTES  *  ADDRESS OF * 00013600
*  +16  * AREA ADDR.   *   BYTES ON     *      ON       *  LAST RECORD* 00013700
*       *    (CONT.)   *     TRACK      *    TRACK      *    WRITTEN  * 00013800
*       *************************************************************** 00013900
*       *           ADDRESS OF LAST RECORD       * TRACKS      * EARLY* 00014000
*  +24  *                WRITTEN (CONT.)         *    PER      * WARN.* 00014100
*       *                                        * CYLINDER    * 90%  * 00014200
*       *************************************************************** 00014300
*       * EARLY * DEV- *     EARLY WARNING              * EARLY*  CK. * 00014400
*  +32  * WARN. *  ICE *               MESSAGE TRACK    * WARN.* BYTE * 00014500
*       * L TRK * CODE *                                * SWT. * (FF) * 00014600
*       *************************************************************** 00014700
*                                                                       00014800
*                                                                       00014900
*          2   PREVSEEK            CHAR(8),  /* PREVIOUS SEEK FIELD  */ 00015000
*             3   PREVM            CHAR(1) INIT('FF'X), /* FF=CLOSED */ 00015100
*             3   PREVBB           CHAR(2),                             00015200
*             3   PREVCC           CHAR(2),                             00015300
*             3   PREVHH           CHAR(2),                             00015400
*             3   PREVR            CHAR(1);                             00015500
*DCL     LOGRECA    CHAR(11) INIT('SYS1.LOGREC'); /* ENQUE/DEQUE     */ 00015600
*DCL     LOGRECB    CHAR(8)  INIT('RECORDER');    /* NAMES FOR SYSTEM*/ 00015700
*DCL     RESID      CHAR(8)  INIT('SERLOG');      /* & STEP ENQ/DEQ  */ 00015800
*DCL     RESIDII    CHAR(8)  INIT('LOGREC');      /*                 */ 00015900
*DCL     SDRK  FIXED(15) INIT(0);                                       00016000
*DCL     ZEROA CHAR(24);                                                00016100
*DCL 1 SEEKSAVE CHAR(8) INIT('0000000000000000'X), /*BEGINNING OF LOG*/ 00016200
*    2 *      CHAR(7),                                                  00016300
*    2 SKSAVREC CHAR(1); /*R PORTION OF MBBCCHHR                     */ 00016400
*/********************************************************************/ 00016500
*/*                                                                  */ 00016600
*/*      CONSTANTS & WORK AREAS FOR TIME/DATE CONVERSION             */ 00016700
*/*                                                                  */ 00016800
*/********************************************************************/ 00016900
*/*                                                                  */ 00017000
*/*      DSECT OF INPUT BUFFER                                       */ 00017100
*/*                                                                  */ 00017200
*DCL    1   BUFFER  DEFINED(BUF1), /*USE FOR SERLOP EXCP READS       */ 00017300
*          2   BCOUNT              CHAR(8),  /* COUNT AREA           */ 00017400
*             3   BCCHHR           CHAR(5),  /* COUNT FROM REC       */ 00017500
*             3   BKEYL            CHAR(1),  /* KEY LENGTH=0         */ 00017600
*             3   BSIZE            CHAR(2),  /* SIZE OF RECORD READ  */ 00017700
*          2   BDATA                 CHAR(1900);                        00017800
*DCL  BSZ FIXED(15) DEFINED(BSIZE); /*ACCESS BSIZE AS NUMERIC        */ 00017900
*DCL SAVELENT FIXED(15) INIT(0); /*DATA LENGTH + 4 FROM READSEQ      */ 00018000
*DCL  BBRDW CHAR(2) DEFINED(BCCHHR) POSITION(5); /*USE TO SHUFFLE    */ 00018100
*                                /*LOGREC RECORD INTO V FORMAT*/        00018200
*/*                                                                     00018300
*/*      DSCECT OF AN INPUT RECORD                                      00018400
*/*                                                                     00018500
*DCL    1   RECAREA              BASED(TIMEBASE),                       00018600
*          2   RECKEY              CHAR(1),                             00018700
*          2   *                   CHAR(1),                             00018800
*          2   *                   CHAR(6),                             00018900
*             3   *                 BIT(1),                             00019000
*             3   NSSWITCH          BIT(1),  /* NS TIME INDICATOR    */ 00019100
*             3   *                 BIT(2),                             00019200
*             3   LCSWITCH          BIT(1),  /* LOCAL TIME  INDICATOR*/ 00019300
*             3   *                 BIT(3),                             00019400
*          2   DATETIME            CHAR(8),                             00019500
*             3   DATE             CHAR(4),                             00019600
*                4   DATE0         CHAR(1),                             00019700
*                   5   GMTSW       BIT(1),                             00019800
*                   5   *           BIT(7),                             00019900
*                4   YEAR          CHAR(1),                             00020000
*                4   DAYS          CHAR(2),                             00020100
*             3   TIME             CHAR(4),                             00020200
*                4   HOURS         CHAR(1),                             00020300
*                4   MINS          CHAR(1),                             00020400
*                4   SECS          CHAR(1),                             00020500
*                4   TNTHS         CHAR(1);                             00020600
*                                                                       00020700
*/********************************************************************/ 00020800
*/*                                                                  */ 00020900
*/*      STARTING LOCATION FOR IFCIOHND                              */ 00021000
*/*                                                                  */ 00021100
*/********************************************************************/ 00021200
*                                      /*R2 HAS COMTABLE ADDRESS     */ 00021300
*PTRSPOT = R1; /*GET PARAMETER LIST ADDRESS OF ADDRESS               */ 00021400
         ST    R1,PTRSPOT                                               00021500
*SAVER2 = R2; /*PRESUME THAT R2 HAS ADDRESS OF COMTABLE AT THIS POINT*/ 00021600
         ST    R2,SAVER2                                                00021700
         USING IHADCB,R12      USE R12 FOR ADDRESSING DCBS              00021800
*BADNEWS = ZERO; /*RETURN CODE SAVE ITEM                             */ 00021900
         SLR   R3,R3                                                    00022000
         ST    R3,BADNEWS                                               00022100
*                                                                       00022200
*        DETERMINE THE SERVICE THE CALLER HAS REQUESTED AND             00022300
*        BRANCH TO APPROPRIATE ROUTINE                                  00022400
*                                                                       00022500
*IF INARG = READSEQ THEN /*IF READ OF SERLOG OR ACCIN REQUESTED*/       00022600
         L     R6,PTRSPOT                                               00022700
         L     R12,0(,R6)                                               00022800
         CLC   0(2,R12),KH3                                             00022900
         BNE   @RF00136                                                 00023000
*  CALL READSIT;                                                        00023100
         BAL   R14,READSIT                                              00023200
         B     GOBACK                                                   00023300
*ELSE                                                                   00023400
*  DO;                                                                  00023500
*    IF INARG = READIRCT THEN                                           00023600
@RF00136 L     R6,PTRSPOT                                               00023700
         L     R12,0(,R6)                                               00023800
         CLC   0(2,R12),KH6                                             00023900
         BNE   @RF00139                                                 00024000
*      CALL DIRECTIT; /*EXCP READ OF SERLOG OR POINT SEQ READ OF WORK*/ 00024100
         BAL   R14,DIRECTIT                                             00024200
         B     GOBACK                                                   00024300
*    ELSE                                                               00024400
*      DO;                                                              00024500
*        IF INARG = RITEHIST THEN                                       00024600
@RF00139 L     R6,PTRSPOT                                               00024700
         L     R12,0(,R6)                                               00024800
         CLC   0(2,R12),KH4                                             00024900
         BNE   @RF00142                                                 00025000
*          CALL HISTIT; /*QSAM WRITE TO ACCDEV, HISTORY OUTPUT*/        00025100
         BAL   R14,HISTIT                                               00025200
         B     GOBACK                                                   00025300
*        ELSE                                                           00025400
*          DO;                                                          00025500
*            IF INARG = RITESCRT THEN /*SEQUENTIAL NOTE WRITE TO WORK*/ 00025600
@RF00142 L     R6,PTRSPOT                                               00025700
         L     R12,0(,R6)                                               00025800
         CLC   0(2,R12),KH5                                             00025900
         BNE   @RF00145                                                 00026000
*              CALL SCRCHIT;                                            00026100
         BAL   R14,SCRCHIT                                              00026200
         B     GOBACK                                                   00026300
*            ELSE                                                       00026400
*              DO;                                                      00026500
*                IF INARG = RITEPRTR THEN                               00026600
@RF00145 L     R6,PTRSPOT                                               00026700
         L     R12,0(,R6)                                               00026800
         CLC   0(2,R12),KH2                                             00026900
         BNE   @RF00148                                                 00027000
*                  CALL PRINTIT;                                        00027100
         BAL   R14,PRINTIT                                              00027200
         B     GOBACK                                                   00027300
*                ELSE                                                   00027400
*                  DO;                                                  00027500
*                    IF INARG = IOOPEN THEN                             00027600
@RF00148 L     R6,PTRSPOT                                               00027700
         L     R12,0(,R6)                                               00027800
         CLC   0(2,R12),KH1                                             00027900
         BNE   @RF00151                                                 00028000
*                      CALL OPENIT;                                     00028100
         BAL   R14,OPENIT                                               00028200
         B     GOBACK                                                   00028300
*                    ELSE                                               00028400
*                      DO;                                              00028500
*                        IF INARG = IOCLOSE THEN                        00028600
@RF00151 L     R6,PTRSPOT                                               00028700
         L     R12,0(,R6)                                               00028800
         CLC   0(2,R12),KH7                                             00028900
         BNE   @RF00154                                                 00029000
*                          CALL CLOSIT;                                 00029100
         BAL   R14,CLOSIT                                               00029200
         B     GOBACK                                                   00029300
*                        ELSE                                           00029400
*                          DO;                                          00029500
*                                IF INARG = READCARD THEN               00029600
@RF00154 L     R6,PTRSPOT                                               00029700
         L     R12,0(,R6)                                               00029800
         CLC   0(2,R12),KH9                                             00029900
         BNE   @RF00157                                                 00030000
*                                  CALL REEDSYS;                        00030100
         BAL   R14,REEDSYS                                              00030200
         B     GOBACK                                                   00030300
*                                ELSE                                   00030400
*                                  DO;                                  00030500
*                                     MSGNO = 1;                        00030600
@RF00157 MVC   MSGNO(4),KF1                                             00030700
*                                     CALL ERRMSG;                      00030800
         BAL   R14,ERRMSG                                               00030900
*                                  END;                                 00031000
*                           END;                                        00031100
*                       END;                                            00031200
*                    END;                                               00031300
*                END;                                                   00031400
*             END;                                                      00031500
*          END;                                                         00031600
*       END;                                                            00031700
*GOBACK: ;                                                              00031800
*R15 = BADNEWS;                                                         00031900
GOBACK   L     R15,BADNEWS                                              00032000
*IF R15 > FOUR THEN /*FIND OUT IF USER CHECKING RETURN CODE          */ 00032100
         C     R15,KF4                                                  00032200
         BNH   @EL00001                                                 00032300
*  ERRORENT = ERRORENT + 1; /*USER BUNGLE IF MORE THAN ONE           */ 00032400
         LA    R6,1                                                     00032500
         AL    R6,ERRORENT                                              00032600
         ST    R6,ERRORENT                                              00032700
*RETURN;                                                                00032800
@EL00001 L     R13,4(,R13)                                              00032900
@ER00001 L     R14,12(,R13)                                             00033000
         LM    R0,R6,20(R13)             NOTE R7 NOT RESTORED           00033100
         LM    R8,R12,52(R13)                                           00033200
         BR    R14                                                      00033300
*                                                                       00033400
*        THIS IS A STRUCTURED PROC TO REPLACE EXCPLOOP                  00033500
*                                                                       00033600
*        OTHERWISE, THE ONLY CHANGES ARE THAT ALL REFERENCES TO         00033700
*        ZEROING INDIVIDUAL RECORDS HAVE BEEN REMOVED AND THE           00033800
*        LOCATION OF THE BEGINNING OF LOGREC FROM THE DEB IS            00033900
*        SAVED FOR INITIALIZATION TO READ FRAMES EXCP LOOP THAT         00034000
*        PERFORMS ALL IO OPERATIONS TO LOGREC.                          00034100
*                                                                       00034200
*        ONE CCW CHAIN IS USE FOR ALL THREE TYPES OF I-O                00034300
*        PERFORMED.                                                     00034400
*        IN ALL CASES, THE FIRST CCW IS A SEARCH FOR A                  00034500
*        PARTICULAR MBBCCHHR ACTUAL PHYSICAL LOCATION OF A RECORD       00034600
*        ON LOGREC ACCROSS THE ENTIRE EXTENT ALLOCATED TO               00034700
*        LOGREC.                                                        00034800
*        THE NEXT CCW IS A TIC WHICH LOOPS BACK TO THE SEARCH IF        00034900
*        THE SEARCH FOR A PARTICULAR RECORD WAS UNSUCCESSFUL            00035000
*        THE TYPES OF OPERATIONS WHICH ARE PERFORMED ARE:               00035100
*        1. CC= 6, READ LOGREC HEADER BY GIVING MBBCCHHR OF             00035200
*                  HEADER                                               00035300
*        2. CC= 5, WRITE HEADER OF SAME LENGTH BACK TO EXACT            00035400
*                  LOCATION SPECIFIED IN GIVEN MBBCCHHR.                00035500
*        3. CC= 158, READ THE NEXT ERROR RECORD AFTER THE RECORD        00035600
*                  WHOSE MBBCCHHR IS GIVEN IN THE SEARCH                00035700
*        R7 -> AREA TO PLACE THE RECORD READ                            00035800
*                                                                       00035900
*EXCPLOOP: PROC;                                                        00036000
EXCPLOOP STM   R14,R12,@SA00002                                         00036100
*CKDAREA = R7; /*SET UP CCW3 CORE ADDRESS TO READ RECORD TO          */ 00036200
         STCM  R7,7,CKDAREA                                             00036300
*AREAADR = ADDR(HDRCC); /*SEARCH ID FOR FIRST CCW                    */ 00036400
         LA    R14,HDRCC                                                00036500
         STCM  R14,7,AREAADR                                            00036600
*IF SBT4 = ON THEN     /*IF HEADER I/O HAS BEEN REQUESTED            */ 00036700
         TM    SBT4,B'00010000'                                         00036800
         BNO   @RF00180                                                 00036900
*  DO;                                                                  00037000
*    CKDCOUNT = 40;   /*3RD CCW NUMBER OF BYTES IN HEADER TO BE READ*/  00037100
         MVC   CKDCOUNT(2),KH40                                         00037200
*    IF SBT2 = ON THEN /*IF HEADER IS TO BE WRITTEN THEN             */ 00037300
         TM    SBT2,B'01000000'                                         00037400
         BNO   @RF00183                                                 00037500
*      CKDCOM = '05'X; /*SET 3RD CCW CHANNEL COMMAND TO WRITE IN PLACE* 00037600
         MVI   CKDCOM,X'05'                                             00037700
         B     @RC00183                                                 00037800
*                                                                       00037900
*    ELSE              /*IF HEADER IS TO BE READ                     */ 00038000
*      CKDCOM = '06'X; /*SET 3RD CCW CHANNEL COMMAND TO READ SAME    */ 00038100
*                      /*RECORD AS SEARCH ID EQUAL HAS FOUND         */ 00038200
@RF00183 MVI   CKDCOM,X'06'                                             00038300
*    HDRM = '00'X;                                                      00038400
@RC00183 MVI   HDRM,X'00'                                               00038500
*    HDRR = '01'X;    /*HEADER RECORD IS RECORD 1                    */ 00038600
         MVI   HDRR,X'01'                                               00038700
*                        /*SET LOGREC DCB DSECT BASE ADDRESS         */ 00038800
         LA    R12,LOGREC                                               00038900
*                         /*LOAD ADDRESS OF DEB FROM DCB FOR LOGREC*/   00039000
         L     R5,DCBDEBAD                                              00039100
*                                /*GET BBCCHH OF BEGINNING OF LOGREC*/  00039200
         MVC   HDRBB(6),DEBBINUM-DEBDASD+DEBBASND-DEBBASIC(R5)          00039300
*      /*FROM FIRST EXTENT DIRECT ACCESS EXTENSION TO THE DEB*/         00039400
*    SEEKSAVE = HDRSEEK; /*SAVE FOR USE WHEN READING FRAMES          */ 00039500
         MVC   SEEKSAVE(8),HDRSEEK                                      00039600
*    SKSAVREC = '02'X; /*INDICATE TIME STAMP AS RECORD BEFORE 1ST FRM*/ 00039700
         MVI   SEEKSAVE+7,X'02'                                         00039800
         B     @RC00180                                                 00039900
*  END;                                                                 00040000
*ELSE  /*IF NORMAL, NON HEADER, READ OF LOGREC                       */ 00040100
*  DO;                                                                  00040200
*    HDRSEEK = PREVSEEK; /*SET ADDRESS TO SEEK FROM COUNT FIELD OF */   00040300
*                        /*PREVIOUS RECORD                           */ 00040400
@RF00180 MVC   HDRSEEK(8),PREVSEEK                                      00040500
*    CKDCOM = '9E'X;  /*SET THIRD CCW CHANNEL COMMAND TO SEEK NEXT */   00040600
*                     /*RECORD AFTER RECORD WHOSE SEEK ID EQUAL IS */   00040700
*                     /*FOUND AS RESULT OF CCW1. DONE FOR SPEED*/       00040800
         MVI   CKDCOM,X'9E'                                             00040900
*    CKDCOUNT = 1944; /*SET NUMBER OF BYTES TO READ. INCORRECT LENGTH*/ 00041000
*                     /*ERRORS ARE SUPPRESSED                        */ 00041100
         MVC   CKDCOUNT(2),KH1944                                       00041200
*  END;                                                                 00041300
*XIOBSEEK = HDRSEEK; /*SET IOB SEEK ADDRESS                          */ 00041400
@RC00180 MVC   IOBSEEK,HDRSEEK                                          00041500
*XWRECB = 0;         /*ZERO THE EVENT CONTROL BLOCK                  */ 00041600
         SLR   R4,R4                                                    00041700
         ST    R4,XWRECB                                                00041800
*DO UNTIL POST Œ= '44'X; /*LOOP UNTIL IO REQUEST NOT INTERCEPTED*/      0004190
*                  /*ISSUE SVC 0                                     */ 00042000
@DL00201 EXCP  XIOB                                                     00042100
*                        /*WAIT FOR COMPLETION OF IO                 */ 00042200
         WAIT  ECB=XWRECB                                               00042300
*END;                                                                   00042400
         CLI   POST,X'44'                                               00042500
         BE    @DL00201                                                 00042600
*END;                                                                   00042700
         LM    R14,R12,@SA00002                                         00042800
         BR    R14                                                      00042900
*                                                                       00043000
*READSER: PROC OPTIONS (DONTSAVE(7)); /*READ SYS1.LOGREC SEQUENTIALLY*/ 00043100
READSER  STM   R14,R6,@SA00003                                          00043200
         STM   R8,R12,@SA00003+36                                       00043300
*                                                                       00043400
*        PROC HAS BEEN REWRITTEN TO HANDLE READING OF FRAMES AT         00043500
*        END OF OF FILE FOR ERROR RECORDS, IF FRAMES EXIST.             00043600
*        THE PROC IS NOW STRUCTURED                                     00043700
*                                                                       00043800
*RSEQCNT = RSEQCNT + 1; /*COUNT NUMBER OF READ ATTEMPTS              */ 00043900
         LA    R6,1                                                     00044000
         AL    R6,RSEQCNT                                               00044100
         ST    R6,RSEQCNT                                               00044200
*IF SERLO = YES THEN    /*IF SYS1.LOGREC IS OPEN THEN                */ 00044300
         TM    IOSWTCH+1,SERLO                                          00044400
         BNO   @RF00210                                                 00044500
*  DO;                                                                  00044600
*    IF READFR = OFF THEN /*IF READING ERROR RECORDS                 */ 00044700
         TM    IOSWTCH+2,READFR                                         00044800
         BNZ   @RF00212                                                 00044900
*      DO;                                                              00045000
*        IF PREVSEEK(2:8) = HDRLASTR THEN /*IF CURRENT SEEK ADDRESS */  00045100
*      /*IS EQUAL TO LAST ACTIVE RECORD SEEK ADDRESS, IMPLICATION IS */ 00045200
*         /*THAT THIS LAST RECORD HAS ALREADY BEEN READ              */ 00045300
         CLC   PREVSEEK+1(7),HDRLASTR                                   00045400
         BNE   @RC00212                                                 00045500
*          DO;                                                          00045600
*            IF TWOTRY = NO & /*SECOND TRY FOR EOF ON SEQUENTIAL INPT*/ 00045700
*               LASTLOOP = NO THEN /*NOT AN END OF FILE SAVING OF    */ 00045800
         TM    IOSWTCH+1,TWOTRY                                         00045900
         BNZ   @RF00216                                                 00046000
         TM    IOSWTCH,LASTLOOP                                         00046100
         BNZ   @RF00216                                                 00046200
*                                  /*RECORDS DURING ZERO ALL PROCESS*/  00046300
*              DO;                                                      00046400
*                TWOTRY = YES; /*SET SECOND SEQUENTIAL READ TRY FLAG*/  00046500
         OI    IOSWTCH+1,TWOTRY                                         00046600
*                CALL CHECKHDR; /*ANY RECORDS WRITTEN WHILE READING?*/  00046700
         BAL   R14,CHECKHDR                                             00046800
*                IF BADNEWS = FOUR &    /*IF NO NEW RECORDS WRITTEN*/   00046900
*                   FRAMES  = YES THEN  /*AND FRAMES EXIST ON LOGREC*/  00047000
         CLC   BADNEWS(4),KF4                                           00047100
         BNE   @RC00212                                                 00047200
         TM    HDREWSW,FRAMES                                           00047300
         BNO   @RC00212                                                 00047400
*                  DO;                                                  00047500
*                    BADNEWS = ZERO; /*INDICATE NO END OF FILE YET*/    00047600
         SLR   R12,R12                                                  00047700
         ST    R12,BADNEWS                                              00047800
*                    READFR = YES; /*SET READ FRAME SWITCH           */ 00047900
         OI    IOSWTCH+2,READFR                                         00048000
*                    IF DEBUG15 = ON THEN /*IF DEBUG 15 MONITORING*/    00048100
         L     R14,SAVER2                                               00048200
         TM    DEBUG15(R14),B'00000001'                                 00048300
         BNO   @RF00224                                                 00048400
*                      DO;                                              00048500
*                        MSGNO = 70; /*START OF FRAME READ MONITOR*/    00048600
         MVC   MSGNO(4),KF70                                            00048700
*                        CALL ERRMSG; /*PRINT MESSAGE TO TOURIST*/      00048800
         BAL   R14,ERRMSG                                               00048900
*                      END;                                             00049000
*                    PREVSEEK = SEEKSAVE; /*GET TIME STAMP LOCATION*/   00049100
@RF00224 MVC   PREVSEEK(8),SEEKSAVE                                     00049200
         B     @RC00212                                                 00049300
*                                                                       00049400
*                  END;                                                 00049500
*              END;                                                     00049600
*            ELSE  /*EITHER SECOND END OF FILE OR ZEROALL PROCESSING*/  00049700
*              DO;                                                      00049800
*                IF FRAMES = YES &    /*DO FRAMES EXIST ON SERLOG*/     00049900
*                   LASTLOOP = NO THEN /*NOT ZEROALL PROCESSING  */     00050000
@RF00216 TM    HDREWSW,FRAMES                                           00050100
         BNO   @RF00233                                                 00050200
         TM    IOSWTCH,LASTLOOP                                         00050300
         BNZ   @RF00233                                                 00050400
*                  DO;                                                  00050500
*                    READFR = YES; /*SET READING FRAMES SWITCH*/        00050600
         OI    IOSWTCH+2,READFR                                         00050700
*                    IF DEBUG15 = ON THEN /*IF DEBUG15 MONITORING*/     00050800
         L     R6,SAVER2                                                00050900
         TM    DEBUG15(R6),B'00000001'                                  00051000
         BNO   @RF00236                                                 00051100
*                      DO;                                              00051200
*                        MSGNO = 70; /*START OF FRAME READ MESSAGE*/    00051300
         MVC   MSGNO(4),KF70                                            00051400
*                        CALL ERRMSG; /*WRITE MESSAGE TO TOURIST*/      00051500
         BAL   R14,ERRMSG                                               00051600
*                      END;                                             00051700
*                    PREVSEEK = SEEKSAVE; /*GET STARTING LOCATION*/     00051800
*                    /*OF LOGREC FROM DEB. IMPLICATION IS THAT   */     00051900
*                    /*1ST FRAME MUST BE ON SAME TRACK AS HEADER */     00052000
@RF00236 MVC   PREVSEEK(8),SEEKSAVE                                     00052100
*                   END;                                                00052200
*                 ELSE  /*LEGITIMATE END OF FILE                     */ 00052300
*                   BADNEWS = FOUR; /*INDICATE EOF TO WORLD          */ 00052400
         B     @RC00212                                                 00052500
*                                                                       00052600
@RF00233 MVC   BADNEWS(4),KF4                                           00052700
         B     @RC00212                                                 00052800
*               END;                                                    00052900
*           END;                                                        00053000
*      END;                                                             00053100
*    ELSE  /*FRAMES ARE BEING READ                                   */ 00053200
*      DO;                                                              00053300
*        IF PREVSEEK(2:8) = HDRREA THEN /*IF SEEK ADDRESS FOR        */ 00053400
*          /*PREVIOUS RECORD IS EQUAL TO SEEK ADDRESS OF LAST FRAME  */ 00053500
*          /*THEN END OF FILE ON LOGREC HAS BEEN REACHED             */ 00053600
@RF00212 CLC   PREVSEEK+1(7),HDRREA                                     00053700
         BNE   @RF00248                                                 00053800
*          BADNEWS = FOUR; /*INDICATE END OF FILE                    */ 00053900
         MVC   BADNEWS(4),KF4                                           00054000
         B     @RC00212                                                 00054100
*                                                                       00054200
*        ELSE  /*NOT LAST FRAME                                      */ 00054300
*          RFRCOUNT = RFRCOUNT + 1; /*COUNT FRAMES FOR DEBUG16 TOTAL*/  00054400
@RF00248 LA    R12,1                                                    00054500
         AL    R12,RFRCOUNT                                             00054600
         ST    R12,RFRCOUNT                                             00054700
*      END;                                                             00054800
*    IF BADNEWS = ZERO THEN /*IF NO EOF OR ERROR CONDITIONS THEN*/      00054900
@RC00212 L     R14,BADNEWS                                              00055000
         LTR   R14,R14                                                  00055100
         BNZ   @ER00003                                                 00055200
*      DO;                                                              00055300
*        R7 = ADDR(BUF1); /*ADDRESS READ BUFFER                      */ 00055400
         LA    R7,BUF1                                                  00055500
*        CALL EXCPLOOP; /*ISSUE EXCP READ                            */ 00055600
         BAL   R14,EXCPLOOP                                             00055700
*        IF POST \= '7F'X THEN /*IF UNSUCCESSFULL READ               */ 00055800
         CLI   POST,X'7F'                                               00055900
         BE    @RF00256                                                 00056000
*          DO;                                                          00056100
*            IF POST = '42'X THEN /*OUT OF LOGREC EXTENT ERROR*/        00056200
         CLI   POST,X'42'                                               00056300
         BNE   @RF00258                                                 00056400
*              MSGNO = 14;                                              00056500
         MVC   MSGNO(4),KF14                                            00056600
         B     @RC00258                                                 00056700
*                                                                       00056800
*            ELSE   /* HARD I-O ERROR                                */ 00056900
*              MSGNO = 39;                                              00057000
@RF00258 MVC   MSGNO(4),KF39                                            00057100
*            CALL ERRMSG;                                               00057200
@RC00258 BAL   R14,ERRMSG                                               00057300
*            BADNEWS = TWELVE;                                          00057400
         MVC   BADNEWS(4),KF12                                          00057500
         B     @ER00003                                                 00057600
*                                                                       00057700
*          END;                                                         00057800
*        ELSE /*SUCCESSFUL READ                                      */ 00057900
*          DO;                                                          00058000
*            PREVCC(1:5) = BCOUNT; /*DIRECT ACCESS ADDRESS,CCHHR,*/     00058100
*           /*OF RECORD LAST READ FROM COUNT RECORD PRECEEDING DATA*/   00058200
*           /*RECORD ON COUNT,DATA FORMATTED DIRECT ACCESS FILE*/       00058300
@RF00256 MVC   PREVCC(5),BCOUNT                                         00058400
*            FLIPFLOP = NO; /*INDICATE SEQUENTIAL INPUT FROM LOGREC,*/  00058500
*                           /* AND NOT FROM HISTORY INPUT            */ 00058600
         NI    IOSWTCH,255-FLIPFLOP                                     00058700
*            RECCCHHR = BCOUNT; /*PASS SAME VALUE TO USER VIA PARM*/    00058800
         L     R12,SAVER2                                               00058900
         MVC   RECCCHHR(5,R12),BCOUNT                                   00059000
*            RECLNGTH = BSIZE; /*PASS RECORD LENGTH FROM COUNT RECRD*/  00059100
         LH    R14,BSIZE                                                00059200
         N     R14,KX00FFFF                                             00059300
         STH   R14,RECLNGTH(,R12)                                       00059400
*            SAVELENT = BSIZE + 4; /*SAVE RECORD QSAM LENGTH FOR */     00059500
         LA    R14,4(,R14)                                              00059600
         STH   R14,SAVELENT                                             00059700
*                                  /*RITESCRT,RITEHIST               */ 00059800
*            R7 = ADDR(BDATA); /*BKEY IS DATA PORTION OF RECORD SINCE*/ 00059900
         LA    R7,BDATA                                                 00060000
*            TIMEBASE = R7; /*TELL TIME CONVERSION WHERE RECORD IS*/    00060100
         ST    R7,TIMEBASE                                              00060200
*            CALL CONVTIME; /*CONVERT TIME, IF NECESSARY             */ 00060300
         BAL   R14,CONVTIME                                             00060400
         B     @ER00003                                                 00060500
*                                                                       00060600
*          END;                                                         00060700
*      END;                                                             00060800
*  END;                                                                 00060900
*ELSE /*IF LOGREC HAS NOT BEEN OPENED                                */ 00061000
*  DO;                                                                  00061100
*    MSGNO = 47;                                                        00061200
@RF00210 MVC   MSGNO(4),KF47                                            00061300
*    CALL ERRMSG;                                                       00061400
         BAL   R14,ERRMSG                                               00061500
*    BADNEWS = TWELVE;                                                  00061600
         MVC   BADNEWS(4),KF12                                          00061700
*  END;                                                                 00061800
*END; /*END OF PROC READSER                                          */ 00061900
@ER00003 LM    R14,R6,@SA00003                                          00062000
         LM    R8,R12,@SA00003+36                                       00062100
         BR    R14                                                      00062200
*                                                                       00062300
*DIRRDSER: PROC OPTIONS(DONTSAVE(R7)); /*READ SERLOG DIRECTLY*/         00062400
DIRRDSER STM   R14,R6,@SA00004                                          00062500
         STM   R8,R12,@SA00004+36                                       00062600
*IF SERLO = YES THEN /*DO ONLY IF SERLOG IS OPEN                     */ 00062700
         TM    IOSWTCH+1,SERLO                                          00062800
         BNO   @RF00284                                                 00062900
*  DO;                                                                  00063000
*    RSERDCNT = RSERDCNT + 1; /*COUNT NUMBER OF READ ATTEMPTS*/         00063100
         LA    R12,1                                                    00063200
         AL    R12,RSERDCNT                                             00063300
         ST    R12,RSERDCNT                                             00063400
*    PREVCC(1:5) = RECCCHHR;                                            00063500
         L     R14,SAVER2                                               00063600
         MVC   PREVCC(5),RECCCHHR(R14)                                  00063700
*    R7 = ADDR(BUF1);                                                   00063800
         LA    R7,BUF1                                                  00063900
*    PREVR = PREVR - 1;                                                 00064000
         SLR   R12,R12                                                  00064100
         IC    R12,PREVR                                                00064200
         BCTR  R12,0                                                    00064300
         STC   R12,PREVR                                                00064400
*    CALL EXCPLOOP;                                                     00064500
         BAL   R14,EXCPLOOP                                             00064600
*    IF POST Œ= '7F'X THEN                                              0006470
         CLI   POST,X'7F'                                               00064800
         BE    @RF00291                                                 00064900
*      DO;                                                              00065000
*        MSGNO = 25;                                                    00065100
         MVC   MSGNO(4),KF25                                            00065200
*        CALL ERRMSG;                                                   00065300
         BAL   R14,ERRMSG                                               00065400
*        BADNEWS = TWELVE;                                              00065500
         MVC   BADNEWS(4),KF12                                          00065600
*        RETURN;                                                        00065700
@ER00004 LM    R14,R6,@SA00004                                          00065800
         LM    R8,R12,@SA00004+36                                       00065900
         BR    R14                                                      00066000
*                                                                       00066100
*      END;                                                             00066200
*     PREVCC(1:5) = BCOUNT;                                             00066300
@RF00291 MVC   PREVCC(5),BCOUNT                                         00066400
*     RECLNGTH = BSZ;                                                   00066500
         LH    R6,BSZ                                                   00066600
         L     R12,SAVER2                                               00066700
         STH   R6,RECLNGTH(,R12)                                        00066800
*     R7 = ADDR(BDATA);                                                 00066900
         LA    R7,BDATA                                                 00067000
*     TIMEBASE = R7;                                                    00067100
         ST    R7,TIMEBASE                                              00067200
*     CALL CONVTIME;                                                    00067300
         BAL   R14,CONVTIME                                             00067400
         B     @ER00004                                                 00067500
*                                                                       00067600
*  END;                                                                 00067700
*ELSE /*SERLOG NOT OPEN                                                 00067800
*  DO;                                                                  00067900
*    MSGNO = 47;                                                        00068000
@RF00284 MVC   MSGNO(4),KF47                                            00068100
*    CALL ERRMSG;                                                       00068200
         BAL   R14,ERRMSG                                               00068300
*    BADNEWS = TWELVE;                                                  00068400
         MVC   BADNEWS(4),KF12                                          00068500
*  END;                                                                 00068600
*END; /*CLOSING END TO DIRRDSER PROC */                                 00068700
         B     @ER00004                                                 00068800
*                                                                       00068900
*/********************************************************************/ 00069000
*/*                                                                  */ 00069100
*/*      RESET HEADER TO INDICATE AN EMPTY LOGREC DATA SET           */ 00069200
*/*                                                                  */ 00069300
*/********************************************************************/ 00069400
*RESETHDR: PROC;                                                        00069500
RESETHDR STM   R14,R12,@SA00005                                         00069600
*        IF DEBUG15 = ON THEN                                           00069700
         L     R12,SAVER2                                               00069800
         TM    DEBUG15(R12),B'00000001'                                 00069900
         BNO   @RF00313                                                 00070000
*          DO; /*PRINT TO TOURIST ATTEMPT TO RESET HEADER            */ 00070100
*            MSGNO = 55;                                                00070200
         MVC   MSGNO(4),KF55                                            00070300
*            CALL ERRMSG;                                               00070400
         BAL   R14,ERRMSG                                               00070500
*          END;                                                         00070600
*        R7=ADDR(HEADER);               /* POINT TO HEADER AREA      */ 00070700
@RF00313 LA    R7,HEADER                                                00070800
*        SBT2='1'B;                     /* SET HEADER & WRITE        */ 00070900
*        SBT4='1'B;                     /* SWITCHES ON               */ 00071000
         OI    SBT2,B'01010000'                                         00071100
*        HDRLASTR=HDRREA;               /* RESET LAST REC WRITTEN    */ 00071200
         MVC   HDRLASTR(7),HDRREA                                       00071300
*       IF FRAMES = YES THEN /*IF FRAMES TO BE SAVED FOR MERIDIAN*/     00071400
         TM    HDREWSW,FRAMES                                           00071500
         BNO   RESETNF                     NO FRAMES, BRANCH            00071600
*         HDRTBAL = 20; /*DONT LET RECORDS ON LAST FRAME TRACK*/        00071700
         MVC   HDRTBAL(2),KH20             MAKE TRACK BALANCE TOO       00071800
*                                          SMALL FOR ANY RECORD TO FIT  00071900
         B     @RF00326                                                 00072000
*                                                                       00072100
*        CALCULATE THE TRACK BALANCE ALLOWING FOR THE HEADER            00072200
*        RECORD AND OUTAGE RECORD LOGREC DATASET                        00072300
*                                                                       00072400
RESETNF  SR    R4,R4                                                    00072500
         ICM   R4,B'0011',HDRTRKCP     DEVICE TRACK CAPACITY            00072600
         LA    R12,LOGREC              R12 -> LOGREC DCB                00072700
         L     R7,DCBDEBAD             R7 -> LOGREC DEB                 00072800
*                                      R7 -> LOGREC UCB                 00072900
         L     R7,DEBUCBAD-DEBDASD+DEBBASND-DEBBASIC(,R7)               00073000
         LA    R5,L'HEADER             L'RECORD                         00073100
         ICM   R5,B'1000',=X'01'       SET RECORD NO = 1 FOR HDRREC     00073200
*                                                                       00073300
         TRKCALC FUNCTN=TRKBAL,REMOVE=NO,UCB=(7),                      X00073400
               BALANCE=(4),RKDD=(5),REGSAVE=YES                         00073500
*                                                                       00073600
         LTR   R15,R15                 TRKCALC OK ?                     00073700
         BNZ   RESETBAD                NO, BRANCH                       00073800
         LR    R4,R0                   YES, SET TRACK BALANCE           00073900
         ICM   R5,B'1000',=X'02'       SET RECORD NO = 2 FOR OUTAGE REC 00074000
*                                                                       00074100
         TRKCALC FUNCTN=TRKBAL,REMOVE=NO,UCB=(7),                      X00074200
               BALANCE=(4),RKDD=(5),REGSAVE=YES                         00074300
*                                                                       00074400
         LTR   R15,R15                 TRKCALC OK ?                     00074500
         BNZ   RESETBAD                NO, BRANCH                       00074600
         STCM  R0,B'0011',HDRTBAL      UPDATE HDRREC TRACK BALANCE      00074700
*                                                                       00074800
*        HDRWNGBT='0'B;                 /* TURN OFF EARLY WARN SWITCH*/ 00074900
@RF00326 NI    HDREWSW,255-HDRWNGBT                                     00075000
*        HDRCOUNT = '00'X;             /* RESET THE MSG FIELD */        00075100
         MVI   HDRCOUNT,X'00'                                           00075200
*        CALL EXCPLOOP;                                                 00075300
         BAL   R14,EXCPLOOP                                             00075400
*        IF POST\='7F'X THEN            /* TEST FOR NORMAL           */ 00075500
         CLI   POST,X'7F'                                               00075600
         BE    @ER00005                                                 00075700
*          DO;                                                          00075800
*            MSGNO = 48;                                                00075900
RESETBAD MVC   MSGNO(4),KF48                                            00076000
*            CALL ERRMSG;                                               00076100
         BAL   R14,ERRMSG                                               00076200
*            BADNEWS = TWELVE;                                          00076300
         MVC   BADNEWS(4),KF12                                          00076400
*            R2 = SAVER2;               /*INSURE COMTBL ADDR BY R2   */ 00076500
         L     R12,SAVER2                                               00076600
         LR    R2,R12                                                   00076700
*            CALL IFCMSG(201,ADDR(PRNTBF));                             00076800
         LA    R14,PRNTBF                                               00076900
         ST    R14,@AFTEMPS                                             00077000
         L     R15,ADIFCMSG(,R12)                                       00077100
         LA    R1,@AL00337                                              00077200
         BALR  R14,R15                                                  00077300
*            RETURN;                                                    00077400
@ER00005 LM    R14,R12,@SA00005                                         00077500
         BR    R14                                                      00077600
*          END;                                                         00077700
*END; /*CLOSING END TO PROC RESETHDR                                 */ 00077800
         B     @ER00005                                                 00077900
*                                                                       00078000
*/********************************************************************/ 00078100
*/*                                                                  */ 00078200
*/*      OPEN SYS1.LOGREC                                            */ 00078300
*/*                                                                  */ 00078400
*/********************************************************************/ 00078500
*OPENSERL: PROC;                                                        00078600
OPENSERL STM   R14,R12,@SA00006                                         00078700
*IF DEBUG15 = ON THEN                                                   00078800
         L     R6,SAVER2                                                00078900
         TM    DEBUG15(R6),B'00000001'                                  00079000
         BNO   @RF00344                                                 00079100
*  DO;                                                                  00079200
*    MSGNO = 53;                                                        00079300
         MVC   MSGNO(4),KF53                                            00079400
*    CALL ERRMSG;                                                       00079500
         BAL   R14,ERRMSG                                               00079600
*  END;                                                                 00079700
@RF00344 LA    R1,LOGREC                                                00079800
         ST    R1,IOBDCBPT                                              00079900
*IF SERLO = YES THEN /*IF SERLOG ALREADY OPEN THEN                   */ 00080000
         TM    IOSWTCH+1,SERLO                                          00080100
         BNO   @RF00350                                                 00080200
*  DO;                                                                  00080300
*    MSGNO = 41;                                                        00080400
         MVC   MSGNO(4),KF41                                            00080500
*    CALL ERRMSG;                                                       00080600
         BAL   R14,ERRMSG                                               00080700
*    BADNEWS = TWELVE;                                                  00080800
         MVC   BADNEWS(4),KF12                                          00080900
*    RETURN;                                                            00081000
@ER00006 LM    R14,R12,@SA00006                                         00081100
         BR    R14                                                      00081200
*  END;                                                                 00081300
*/*                                                                  */ 00081400
*ZEROA = (ZEROA && ZEROA); /*CLEAR FIELD                                00081500
@RF00350 XC    ZEROA,ZEROA                                              00081600
*IF NOSDR = NO THEN /*IF USER WANTS SDR COUNTERS DUMPED              */ 00081700
         L     R14,SAVER2                                               00081800
         TM    NOSDR(R14),B'00000001'                                   00081900
         BNZ   @RF00358                                                 00082000
*DO;                                                                    00082100
         LA    R0,4                     FUNCTION CODE TO DUMP SDR'S     00082200
         SVC   76                       ISSUE SVC TO DUMP SDR'S         00082300
*END;                                                                   00082400
@RF00358 ENQ   (RESID,LOGRECA,E,11,STEP) ENQ TO PREVENT OTHER ACCESS    00082500
*                                        FROM ANOTHER IFCEREP0 TASK     00082600
         L     R14,SAVER2                                               00082700
         TM    ZEROALL(R14),B'01000000'  LOGREC TO BE ZEROED?  #IO19631 00082800
         BO    @RF00359                  YES, BRANCH           #IO19631 00082900
         OPEN  (LOGREC,(INPUT))          NO, OPEN FOR INPUT    #IO19631 00083000
         B     @RF00360                                        #IO19631 00083100
@RF00359 OPEN  (LOGREC,(OUTPUT))                               #IO19631 00083200
*                                                                       00083300
@RF00360 LA    R12,LOGREC               R12 -> LOGREC DCB FOR DSECT     00083400
         TM    DCBOFLGS,DCBOFOPN        DID OPEN GO OK ?                00083500
         BZ    BADOPENA                 BRANCH,  BAD OPEN               00083600
*        SERLO = YES;                   /* TURN ON OPEN DISK IND     */ 00083700
         OI    IOSWTCH+1,SERLO                                          00083800
*        SBT4='1'B;                     /* TURN ON HDR READ SW       */ 00083900
         OI    SBT4,B'00010000'                                         00084000
*        R7=ADDR(HEADER);               /* POINT TO HEADER AREA      */ 00084100
         LA    R7,HEADER                                                00084200
*        CALL EXCPLOOP;                 /* GO DO I/O TO READ HDR     */ 00084300
         BAL   R14,EXCPLOOP                                             00084400
*        SBT4='0'B;                     /* TURN OFF HDR READ SW      */ 00084500
         NI    SBT4,B'11101111'                                         00084600
*        IF POST\='7F'X                 /* TEST FOR NORMAL            / 00084700
*              THEN                     /* RETURN FROM I/O            / 00084800
         CLI   POST,X'7F'                                               00084900
         BNE   HDRERR0                                                  00085000
*                   GOTO HDRERR0;       /* IF NOT GOTO ERROR EXIT    */ 00085100
*        IF HDRSAFE\='FF'X              /* TEST FOR SAFETY BYTE      */ 00085200
*              THEN                     /* IN HEADER RECORD          */ 00085300
         CLI   HDRSAFE,X'FF'                                            00085400
         BNE   HDRERR1                                                  00085500
*                   GOTO HDRERR1;       /* IF NOT TAKE ERROR EXIT    */ 00085600
         MVC   DCBFDAD+3(4),HDRECC      SET DCB HIGH EXTENT FIELD       00085700
         XC    DCBTRBAL,DCBTRBAL        ZERO THE TRACK BALANCE FIELD    00085800
*PREVSEEK(2:8) = HDRREA; /*SET START SEEK AT LAST FRAME LOCATION*/      00085900
         MVC   PREVSEEK+1(7),HDRREA                                     00086000
*PREVM = '00'X; /*INDICATE DATA SET OPEN                             */ 00086100
         MVI   PREVM,X'00'                                              00086200
*        RETURN;                        /* TAKE NORMAL EXIT          */ 00086300
         B     @ER00006                                                 00086400
*                                                                       00086500
*HDRERR0:     ;                                                         00086600
*        IF POST='42'X                  /* TEST FOR OUT OF EXTENT    */ 00086700
*              THEN                     /* IF YES                    */ 00086800
HDRERR0  CLI   POST,X'42'                                               00086900
         BNE   @RF00377                                                 00087000
*                MSGNO = 14;                                            00087100
         MVC   MSGNO(4),KF14                                            00087200
         B     @RC00377                                                 00087300
*                                                                       00087400
*              ELSE                                                     00087500
*                MSGNO = 15;                                            00087600
@RF00377 MVC   MSGNO(4),KF15                                            00087700
*         CALL ERRMSG;                                                  00087800
@RC00377 BAL   R14,ERRMSG                                               00087900
*         BADNEWS = TWELVE;                                             00088000
         MVC   BADNEWS(4),KF12                                          00088100
*         RETURN;                                                       00088200
         B     @ER00006                                                 00088300
*                                                                       00088400
*HDRERR1:     ;                                                         00088500
*      MSGNO = 16;                                                      00088600
HDRERR1  MVC   MSGNO(4),KF16                                            00088700
*      CALL ERRMSG;                                                     00088800
         BAL   R14,ERRMSG                                               00088900
*      BADNEWS = TWELVE;                                                00089000
         MVC   BADNEWS(4),KF12                                          00089100
*END; /*CLOSING END TO PROC OPENSERL, OPEN SERLOG DATA SET           */ 00089200
         B     @ER00006                                                 00089300
*                                                                       00089400
*/********************************************************************/ 00089500
*/*                                                                  */ 00089600
*/*      CHECK HEADER FOR MORE RECORDS WRITTEN WHILE PROCESSING      */ 00089700
*/*                                                                  */ 00089800
*/********************************************************************/ 00089900
*                                                                       00090000
*CHECKHDR: PROC;                                                        00090100
CHECKHDR STM   R14,R12,@SA00007                                         00090200
*        IF DEBUG15 = ON THEN                                           00090300
         L     R12,SAVER2                                               00090400
         TM    DEBUG15(R12),B'00000001'                                 00090500
         BNO   @RF00391                                                 00090600
*          DO;                                                          00090700
*            MSGNO = 54;                                                00090800
         MVC   MSGNO(4),KF54                                            00090900
*            CALL ERRMSG;                                               00091000
         BAL   R14,ERRMSG                                               00091100
*          END;                                                         00091200
*        IF SERLO = NO THEN             /* TEST FOR DISK OPEN        */ 00091300
@RF00391 TM    IOSWTCH+1,SERLO                                          00091400
         BNZ   @RF00396                                                 00091500
*          DO;                                                          00091600
*            MSGNO = 49;                                                00091700
         MVC   MSGNO(4),KF49                                            00091800
*            CALL ERRMSG;                                               00091900
         BAL   R14,ERRMSG                                               00092000
*            BADNEWS = TWELVE;                                          00092100
         MVC   BADNEWS(4),KF12                                          00092200
*            RETURN;                                                    00092300
@ER00007 LM    R14,R12,@SA00007                                         00092400
         BR    R14                                                      00092500
*          END;                                                         00092600
*                                       /* GET STORAGE TO HOLD HEADER*/ 00092700
@RF00396 GETMAIN R,LV=40                                                00092800
*        R4=R1;                         /* SET BASE FOR OLD HEADER   */ 00092900
         LR    R4,R1                                                    00093000
*        OHEADER=HEADER;                /* MOVE HEADER TO GOTTEN CORE*/ 00093100
         MVC   OHEADER(40,R4),HEADER                                    00093200
*        R7=ADDR(HEADER);               /* POINT TO BUFF FOR READ    */ 00093300
         LA    R7,HEADER                                                00093400
*        SBT4='1'B;                     /* TURN ON READ HEADER       */ 00093500
         OI    SBT4,B'00010000'                                         00093600
*        IF LASTLOOP = YES THEN /*DONT ENQUEUE EXCEPT FOR ZEROALL PRO*/ 00093700
*        DO; /*CESSING. MAYBE EXTRA RECORD WRITTEN AT EOF FOR READSEQ*/ 00093800
         TM    IOSWTCH,LASTLOOP                                         00093900
         BNO   @RF00408                                                 00094000
*        SBT7='1'B;                     /* TURN ON INTERNAL ENQ SW   */ 00094100
         OI    SBT7,B'00001000'                                         00094200
*                                                                       00094300
         ENQ  (RESIDII,LOGRECB,E,8,SYSTEM)                              00094400
*                                                                       00094500
*        END;                                                           00094600
*        CALL EXCPLOOP;                 /* GO DO I/O READ HEADER     */ 00094700
@RF00408 BAL   R14,EXCPLOOP                                             00094800
*CHK01  :     ;                                                         00094900
*        IF POST\='7F'X                 /* TEST POST CODE FOR NORMAL */ 00095000
*              THEN                     /* RETURN IF NOT             */ 00095100
CHK01    CLI   POST,X'7F'                                               00095200
         BNE   CHK03                                                    00095300
*                   GOTO CHK03;         /* TAKE ERROR EXIT           */ 00095400
*        IF HDRSAFE\='FF'X              /* TEST HDR REC JUST READ FOR*/ 00095500
*              THEN                     /* SAFETY BYTE               */ 00095600
         CLI   HDRSAFE,X'FF'                                            00095700
         BNE   CHK04                                                    00095800
*                   GOTO CHK04;         /* IF NOT FF TAKE ERR EXIT   */ 00095900
*        IF HDRLASTR=OHDRREC2           /* TEST LAST REC WRITTEN FLDS*/ 00096000
*              THEN                     /* IF THEY MATCH             */ 00096100
         CLC   HDRLASTR(7),OHDRREC2(R4)                                 00096200
         BNE   @RF00419                                                 00096300
*                   DO;                 /* THEN                      */ 00096400
*                        BADNEWS = FOUR; /*INDICATE REAL END OF FILE */ 00096500
         MVC   BADNEWS(4),KF4                                           00096600
*                        GOTO CHK02;    /* GO ON                     */ 00096700
         B     CHK02                                                    00096800
*                   END;                                                00096900
*        PREVSEEK(2:8)=OHDRREC2  ;                                      00097000
@RF00419 MVC   PREVSEEK+1(7),OHDRREC2(R4)                               00097100
*CHK02  :     ;                                                         00097200
*        SBT4='0'B;                     /* TURN OF HEADER SW         */ 00097300
CHK02    NI    SBT4,B'11101111'                                         00097400
*        R1=R4;                         /* SET R1 FOR FREE MAIN      */ 00097500
         LR    R1,R4                                                    00097600
*                                       /* FREE MAIN OF GOTTEN CORE  */ 00097700
         FREEMAIN R,LV=40,A=(1)                                         00097800
*          RETURN; /*GO BACK TO CALLER                               */ 00097900
         B     @ER00007                                                 00098000
*                                                                       00098100
*CHK03  :     ;                                                         00098200
*        IF POST = '42'X THEN /*42 MEANS OUT OF EXTENT                  00098300
CHK03    CLI   POST,X'42'                                               00098400
         BNE   @RF00431                                                 00098500
*          MSGNO = 14;                                                  00098600
         MVC   MSGNO(4),KF14                                            00098700
         B     @RC00431                                                 00098800
*                                                                       00098900
*        ELSE                                                           00099000
*          MSGNO = 15;   /*NOT 42 MEANS IO ERROR                     */ 00099100
@RF00431 MVC   MSGNO(4),KF15                                            00099200
*        CALL ERRMSG;                                                   00099300
@RC00431 BAL   R14,ERRMSG                                               00099400
*        BADNEWS = TWELVE;                                              00099500
         MVC   BADNEWS(4),KF12                                          00099600
*        GOTO  CHK02;     /*GO BACK TO FREE EXCESS CORE                 00099700
         B     CHK02                                                    00099800
*CHK04  :     ;                                                         00099900
*        MSGNO = 16; /*HEADER CHECK BYTE BAD                            00100000
CHK04    MVC   MSGNO(4),KF16                                            00100100
*        CALL ERRMSG;                                                   00100200
         BAL   R14,ERRMSG                                               00100300
*        BADNEWS = TWELVE;                                              00100400
         MVC   BADNEWS(4),KF12                                          00100500
*        GOTO CHK02;                     /*GO BACK TO DEQ            */ 00100600
         B     CHK02                                                    00100700
*                                                                       00100800
*END; /*CLOSING END TO PROC CHECK HEADER                             */ 00100900
*                                                                       00101000
*/********************************************************************/ 00101100
*/*                                                                  */ 00101200
*/*      CONVERT TIME ROUTINE                                        */ 00101300
*/*                                                                  */ 00101400
*/********************************************************************/ 00101500
*                                                                       00101600
*CONVTIME: PROC;                                                        00101700
CONVTIME STM   R14,R12,@SA00008                                         00101800
*        R7SAVE = R7;  /*SAVE RECORD ADDRESS                         */ 00101900
         ST    R7,R7SAVE                                                00102000
*        IF LCSWITCH='1'B               /*  HAS TIME BEEN CONVERTED  */ 00102100
*              THEN                     /*  OR IS IT IN PROPER FORM  */ 00102200
         L     R12,TIMEBASE                                             00102300
         TM    LCSWITCH(R12),B'00001000'                                00102400
         BO    @ER00008                                                 00102500
*                   RETURN;               /*  YES EXIT TO CALLER   */   00102600
*        CONVTCT = CONVTCT + 1;         /*NUMBER OF CONVERSIONS DONE*/  00102700
         LA    R14,1                                                    00102800
         AL    R14,CONVTCT                                              00102900
         ST    R14,CONVTCT                                              00103000
*/********************************************************************/ 00103100
*/*      NEW SYSTEM - TO YYDDD & HHMMSS                              */ 00103200
*/********************************************************************/ 00103300
*CONVNS :     ;                                                         00103400
*        WORK8=DATETIME;                /* MOVE DATE & TIME TO WORK  */ 00103500
CONVNS   L     R2,TIMEBASE                                              00103600
         MVC   WORK8(8),DATETIME(R2)                                    00103700
*        NSSWITCH='0'B;                 /* TURN OFF NS SWITCH        */ 00103800
*        LCSWITCH='1'B ;                /* TURN ON LOCAL SW          */ 00103900
         OI    LCSWITCH(R2),B'00001000'                                 00104000
         NI    NSSWITCH(R2),B'10111111'                                 00104100
*        IF WORK=0 &                    /* TEST FOR                  */ 00104200
*           WORKB=0                     /* ZERO FIELD                */ 00104300
*              THEN                     /* IF YES EXIT TO CALLER     */ 00104400
         SLR   R12,R12                                                  00104500
         C     R12,WORK                                                 00104600
         BNE   @RF00454                                                 00104700
         C     R12,WORKB                                                00104800
         BNE   @RF00454                                                 00104900
*                DO;                                                    00105000
*                  R7 = R7SAVE;                                         00105100
         L     R7,R7SAVE                                                00105200
*                   RETURN;                                             00105300
@ER00008 LM    R14,R12,@SA00008                                         00105400
         BR    R14                                                      00105500
*                END;                                                   00105600
*        R4=WORK ;                      /* PICK UP DATE/TIME         */ 00105700
@RF00454 L     R4,WORK                                                  00105800
*        R5=WORKB;                      /* INTO WORK REGS            */ 00105900
         L     R5,WORKB                                                 00106000
*                                       /* CONVERT FROM NANO TO MICRO*/ 00106100
         SRDL  R4,12                                                    00106200
*                                       /* DIVIDE TO GET # OF MINUTES*/ 00106300
         D     R4,SIXTYMIL                                              00106400
         SLR   R6,R6                                                    00106500
*                                       /* MOVE REMAINDER TO OTHER RG*/ 00106600
         LR    R7,R4                                                    00106700
*                                       /* DIVIDE TO GET SECONDS     */ 00106800
         D     R6,MILLION                                               00106900
*                                       /* CONVERT # OF SECONDS      */ 00107000
         CVD   R7,WORK8                                                 00107100
*                                       /* PICK IT UP TO WORK REG    */ 00107200
         L     R4,WORKB                                                 00107300
*                                       /* MOVE IT TO HI ORDER       */ 00107400
         SRL   R4,4                                                     00107500
*                                       /* PUT BYTE INTO RECORD      */ 00107600
         L     R12,TIMEBASE                                             00107700
         STC   R4,SECS(,R12)                                            00107800
         SLR   R4,R4                                                    00107900
         LR    R7,R6                                                    00108000
*        R6= 0 ;                                                        00108100
         SLR   R6,R6                                                    00108200
         D     R6,THCONV                                                00108300
         CVD   R7,WORK8                                                 00108400
*        R6=WORKB ;                                                     00108500
         L     R6,WORKB                                                 00108600
*        SRL (R6,FOUR);                                                 00108700
         SRL   R6,4                                                     00108800
*        TNTHS=R6 ;                                                     00108900
         L     R12,TIMEBASE                                             00109000
         STC   R6,TNTHS(,R12)                                           00109100
*                                       /* DIVIDE MIN TO GET HOURS   */ 00109200
         D     R4,SIXTY                                                 00109300
*                                       /* CONVERT # MINUTES         */ 00109400
         CVD   R4,WORK8                                                 00109500
*        R4=WORKB;                      /* PICK UP CONVERTED #       */ 00109600
         L     R4,WORKB                                                 00109700
*        SRL (R4,FOUR);                 /* POSITION IT               */ 00109800
         SRL   R4,4                                                     00109900
*        MINS=R4;                       /* SET IT INTO REC AS # MINS */ 00110000
         L     R12,TIMEBASE                                             00110100
         STC   R4,MINS(,R12)                                            00110200
*        R4=0 ;                         /*                           */ 00110300
         SLR   R4,R4                                                    00110400
*                                       /* DIVIDE TO GET HOURS       */ 00110500
         D     R4,TWENTY4                                               00110600
*                                       /* CONVERT # OF HOURS        */ 00110700
         CVD   R4,WORK8                                                 00110800
*        R4=WORKB;                      /* PICK UP VALUE             */ 00110900
         L     R4,WORKB                                                 00111000
*        SRL (R4,FOUR);                 /* POSITION IT IN REG        */ 00111100
         SRL   R4,4                                                     00111200
*        HOURS=R4;                      /* PUT VALUE INTO RECORD     */ 00111300
         L     R12,TIMEBASE                                             00111400
         STC   R4,HOURS(,R12)                                           00111500
*        IF GMTSW='1'B                  /* TEST FOR BASE VALUE TO BE */ 00111600
*              THEN                     /* 1900 OR 1960              */ 00111700
         TM    GMTSW(R12),B'10000000'                                   00111800
         BNO   @RF00489                                                 00111900
*                DO;                                                    00112000
*                  R4 = 0;              /* BIT ON BASE IS 00         */ 00112100
         SLR   R4,R4                                                    00112200
*                  R5 = R5 + 1;         /* INCREMENT DAY COUNT          00112300
*                                          SINCE 1900 IS NOT            00112400
*                                          A LEAP YEAR AND              00112500
*                                          LOOP ASSUMES IT IS        */ 00112600
         AL    R5,KF1                                                   00112700
         B     LEAPYEAR                                                 00112800
*                                                                       00112900
*                END;                                                   00113000
*              ELSE                     /* IF BIT OFF SET BASE TO 60 */ 00113100
*                   R4=60;                                              00113200
@RF00489 LA    R4,60                                                    00113300
*LEAPYEAR:    ;                         /* R5 HAS NUMBER OF DAYS     */ 00113400
*        R5=R5-366;                     /* DECREMENT BY A LEAP YEAR  */ 00113500
LEAPYEAR LA    R12,366                                                  00113600
         SLR   R5,R12                                                   00113700
*        IF R5 < ZERO                   /* IF TOO MUCH               */ 00113800
*              THEN                     /* THEN DO THE FOLLOWING     */ 00113900
         LTR   R5,R5                                                    00114000
         BNM   @RF00497                                                 00114100
*                   DO;                 /*                           */ 00114200
*                        R5=R5+366;     /* BUMP BACK # OF DAYS BY 366*/ 00114300
         ALR   R5,R12                                                   00114400
*                        GOTO CONVNSDN; /* EXIT CONVERSION DONE      */ 00114500
         B     CONVNSDN                                                 00114600
*                   END;                                                00114700
*        R4=R4+1;                       /* ADD 1 FOR YEAR            */ 00114800
@RF00497 LA    R14,1                                                    00114900
         ALR   R4,R14                                                   00115000
*                                       /*                           */ 00115100
*                                       /*  DO LOOP FOR REG YEARS    */ 00115200
*        DO A=1 TO 3 BY 1;                                              00115300
         ST    R14,A                                                    00115400
*              R5=R5-365;               /* DECREMENT BY 365 DAYS     */ 00115500
@DL00503 LA    R2,365                                                   00115600
         SLR   R5,R2                                                    00115700
*              IF R5 < ZERO             /* HAS VALUE                 */ 00115800
*                   THEN                /* GON NEGATIVE              */ 00115900
         LTR   R5,R5                                                    00116000
         BNM   @RF00505                                                 00116100
*                        DO;            /* IF YES THEN               */ 00116200
*                             R5=R5+365;     /* ADD BACK 365 DAYS    */ 00116300
         ALR   R5,R2                                                    00116400
*                             GOTO CONVNSDN; /* & EXIT TO FINISH UP  */ 00116500
         B     CONVNSDN                                                 00116600
*                        END;                                           00116700
*              R4=R4+1;                 /* ADD 1 YEAR                */ 00116800
@RF00505 AL    R4,KF1                                                   00116900
*        END ;                          /* END OF DO LOOP REPEAT 3X  */ 00117000
         LA    R14,1                                                    00117100
         AL    R14,A                                                    00117200
         ST    R14,A                                                    00117300
         C     R14,KF3                                                  00117400
         BNH   @DL00503                                                 00117500
*        GOTO LEAPYEAR;                                                 00117600
         B     LEAPYEAR                                                 00117700
*                                                                       00117800
*CONVNSDN:    ;                         /* R4 HAS YEAR               */ 00117900
*        R5=R5+1;                       /* R5 HAS DAYS ADD 1 TO YEAR */ 00118000
CONVNSDN AL    R5,KF1                                                   00118100
*                                       /* CONVERT YEARS TO DEC      */ 00118200
         CVD   R4,WORK8                                                 00118300
*        R4=WORKB;                      /* PICK UP VALUE             */ 00118400
         L     R4,WORKB                                                 00118500
*        SRL (R4,FOUR);                 /* POSITION IT IN REG        */ 00118600
         SRL   R4,4                                                     00118700
*        YEAR=R4;                       /* PUT YEAR IN RECORD        */ 00118800
         L     R12,TIMEBASE                                             00118900
         STC   R4,YEAR(,R12)                                            00119000
*                                       /* CONVERT THE NO OF DAYS    */ 00119100
         CVD   R5,WORK8                                                 00119200
*        DAYS=(WDAYS | '000F'X);        /* PUT DAYS INTO REC WTH SIGN*/ 00119300
         L     R14,TIMEBASE                                             00119400
         MVC   DAYS(2,R14),WDAYS                                        00119500
         OC    DAYS(2,R14),@CB00983                                     00119600
*        DATE0='00'X;                   /* SET HI DIGITS TO ZERO     */ 00119700
         MVI   DATE0(R14),X'00'                                         00119800
*        R7 = R7SAVE;                                                   00119900
         L     R7,R7SAVE                                                00120000
* END; /*CLOSING END TO PROC CONVTIME                                */ 00120100
         B     @ER00008                                                 00120200
*                                                                       00120300
*OPENIT: PROC;                                                          00120400
OPENIT   STM   R14,R12,@SA00009                                         00120500
*IF OTOURIST = YES THEN                                                 00120600
         L     R12,SAVER2                                               00120700
         TM    OTOURIST(R12),B'10000000'                                00120800
         BNO   @RF00526                                                 00120900
*  CALL OPENTOUR;                                                       00121000
         BAL   R14,OPENTOUR                                             00121100
*IF OEREPPT = YES THEN                                                  00121200
@RF00526 L     R14,SAVER2                                               00121300
         TM    OEREPPT(R14),B'01000000'                                 00121400
         BNO   @RF00528                                                 00121500
*  CALL OPENERPT;                                                       00121600
         BAL   R14,OPENERPT                                             00121700
*IF OSERLOG = YES THEN                                                  00121800
@RF00528 L     R12,SAVER2                                               00121900
         TM    OSERLOG(R12),B'00100000'                                 00122000
         BNO   @RF00530                                                 00122100
*  CALL OPENSERL;                                                       00122200
         BAL   R14,OPENSERL                                             00122300
*IF OACCIN = YES THEN                                                   00122400
@RF00530 L     R14,SAVER2                                               00122500
         TM    OACCIN(R14),B'00010000'                                  00122600
         BNO   @RF00532                                                 00122700
*  CALL OPENHIST;                                                       00122800
         BAL   R14,OPENHIST                                             00122900
*IF OACCDEV = YES THEN                                                  00123000
@RF00532 L     R12,SAVER2                                               00123100
         TM    OACCDEV(R12),B'00001000'                                 00123200
         BNO   @RF00534                                                 00123300
*  CALL OPENDEV;                                                        00123400
         BAL   R14,OPENDEV                                              00123500
*IF OSYSIN = YES THEN                                                   00123600
@RF00534 L     R14,SAVER2                                               00123700
         TM    OSYSIN(R14),B'00000100'                                  00123800
         BNO   @RF00536                                                 00123900
*  CALL OPENSYS;                                                        00124000
         BAL   R14,OPENSYS                                              00124100
*IF ODRCTWRK = YES THEN                                                 00124200
@RF00536 L     R12,SAVER2                                               00124300
         TM    ODRCTWRK(R12),B'00000010'                                00124400
         BNO   @RF00538                                                 00124500
*  CALL OPENDIR;                                                        00124600
         BAL   R14,OPENDIR                                              00124700
*IF OPENIO = NODSETS THEN                                               00124800
@RF00538 L     R14,SAVER2                                               00124900
         CLC   OPENIO(2,R14),KXNULL                                     00125000
         BNE   @ER00009                                                 00125100
*  DO;                                                                  00125200
*    MSGNO = 17;                                                        00125300
         MVC   MSGNO(4),KF17                                            00125400
*    CALL ERRMSG;                                                       00125500
         BAL   R14,ERRMSG                                               00125600
*    BADNEWS = TWELVE;                                                  00125700
         MVC   BADNEWS(4),KF12                                          00125800
*  END;                                                                 00125900
*END; /*CLOSING END TO PROC OPENIT                                   */ 00126000
@ER00009 LM    R14,R12,@SA00009                                         00126100
         BR    R14                                                      00126200
*                                                                       00126300
*CLOSIT: PROC; /*SELECT DATA SET SPECIFIED AND CLOSE IT              */ 00126400
CLOSIT   STM   R14,R12,@SA00010                                         00126500
*IF CEREPPT = YES THEN                                                  00126600
         L     R12,SAVER2                                               00126700
         TM    CEREPPT(R12),B'01000000'                                 00126800
         BNO   @RF00548                                                 00126900
*  CALL CLOSERPT;                                                       00127000
         BAL   R14,CLOSERPT                                             00127100
*IF CACCIN = YES THEN                                                   00127200
@RF00548 L     R14,SAVER2                                               00127300
         TM    CACCIN(R14),B'00010000'                                  00127400
         BNO   @RF00550                                                 00127500
*  CALL CLOSHIST;                                                       00127600
         BAL   R14,CLOSHIST                                             00127700
*IF CACCDEV = YES THEN                                                  00127800
@RF00550 L     R12,SAVER2                                               00127900
         TM    CACCDEV(R12),B'00001000'                                 00128000
         BNO   @RF00552                                                 00128100
*  CALL CLOSEDEV;                                                       00128200
         BAL   R14,CLOSEDEV                                             00128300
*IF CSYSIN = ON THEN                                                    00128400
@RF00552 L     R14,SAVER2                                               00128500
         TM    CSYSIN(R14),B'00000100'                                  00128600
         BNO   @RF00554                                                 00128700
*  CALL CLOSSYS;                                                        00128800
         BAL   R14,CLOSSYS                                              00128900
*IF CDRCTWRK = YES THEN                                                 00129000
@RF00554 L     R12,SAVER2                                               00129100
         TM    CDRCTWRK(R12),B'00000010'                                00129200
         BNO   @RF00556                                                 00129300
*  CALL CLOSDIR;                                                        00129400
         BAL   R14,CLOSDIR                                              00129500
*IF CSERLOG = YES THEN                                                  00129600
@RF00556 L     R14,SAVER2                                               00129700
         TM    CSERLOG(R14),B'00100000'                                 00129800
         BNO   @RF00558                                                 00129900
*  CALL CLOSSERL;                                                       00130000
         BAL   R14,CLOSSERL                                             00130100
*IF CTOURIST = YES THEN                                                 00130200
@RF00558 L     R12,SAVER2                                               00130300
         TM    CTOURIST(R12),B'10000000'                                00130400
         BNO   @RF00560                                                 00130500
*  CALL CLOSTOUR;                                                       00130600
         BAL   R14,CLOSTOUR                                             00130700
*                                                                       00130800
*        MUST CLOSE TOURIST LAST TO GET ALL POSSIBLE ERROR MESSAGES     00130900
*        MUST CLOSE SERLOG SECOND TO LAST IN ORDER TO FREE UP AS        00131000
*        MUCH BUFFER SPACE IN ORDER TO DO A POTENTIAL ZEROALL,          00131100
*        WHICH IMPLIES RECORDS WRITTEN WHILE PROCESSING MUST BE         00131200
*        SAVED IN CORE.                                                 00131300
*        I HOPE THAT EREP1 HAD ENOUGH SENSE TO FREE UP THE              00131400
*        SORTABLE OR ITS EQUIVALENT                                     00131500
*                                                                       00131600
*IF CLOSEIO = NODSETS THEN                                              00131700
@RF00560 L     R14,SAVER2                                               00131800
         CLC   CLOSEIO(2,R14),KXNULL                                    00131900
         BNE   @ER00010                                                 00132000
*  DO;                                                                  00132100
*    MSGNO = 32;                                                        00132200
         MVC   MSGNO(4),KF32                                            00132300
*    CALL ERRMSG;                                                       00132400
         BAL   R14,ERRMSG                                               00132500
*    BADNEWS = TWELVE;                                                  00132600
         MVC   BADNEWS(4),KF12                                          00132700
*  END;                                                                 00132800
*END; /*CLOSING END TO CLOSIT                                        */ 00132900
@ER00010 LM    R14,R12,@SA00010                                         00133000
         BR    R14                                                      00133100
*                                                                       00133200
*CLOSSERL: PROC; /*CLOSE THE SERLOG DATA SET AND ALL THAT IMPLIES*/     00133300
CLOSSERL STM   R14,R12,@SA00011                                         00133400
*IF SERLO = ON THEN /*ONLY ATTEMPT TO CLOSE LOGREC IF ALREADY OPEN*/    00133500
         TM    IOSWTCH+1,SERLO                                          00133600
         BNO   @RF00571                                                 00133700
*DO;                                                                    00133800
*IF DEBUG15 = ON THEN                                                   00133900
         L     R12,SAVER2                                               00134000
         TM    DEBUG15(R12),B'00000001'                                 00134100
         BNO   @RF00573                                                 00134200
*  DO;                                                                  00134300
*    MSGNO = 56;                                                        00134400
         MVC   MSGNO(4),KF56                                            00134500
*    CALL ERRMSG;                                                       00134600
         BAL   R14,ERRMSG                                               00134700
*  END;                                                                 00134800
*                                                                       00134900
*        ZEROALL MEANS RESET THE HEADER TO INDICATE NO DATA             00135000
*        1. ANY RECORDS WRITTEN WHILE PROCESSING ARE READ               00135100
*           INTO CORE                                                   00135200
*        2. THE HEADER IS RESET                                         00135300
*        3. SVC 76 IS ISSUED TO REWRITE THE RECORDS BACK                00135400
*           TO THE NEW SERLOG                                           00135500
*                                                                       00135600
*IF ZEROALL = YES                                                       00135700
*  THEN DO;                                                             00135800
@RF00573 L     R14,SAVER2                                               00135900
         TM    ZEROALL(R14),B'01000000'                                 00136000
         BNO   @RF00578                                                 00136100
*    IF DEBUG15 = ON THEN                                               00136200
         TM    DEBUG15(R14),B'00000001'                                 00136300
         BNO   @RF00580                                                 00136400
*      DO;                                                              00136500
*        MSGNO = 57;                                                    00136600
         MVC   MSGNO(4),KF57                                            00136700
*        CALL ERRMSG;                                                   00136800
         BAL   R14,ERRMSG                                               00136900
*      END;                                                             00137000
*    LASTLOOP = YES; /*INDICATE ZEROALL PROCESSING                   */ 00137100
@RF00580 OI    IOSWTCH,LASTLOOP                                         00137200
*    READFR = OFF;  /* DO NOT READ FRAMES DURING ZEROALL             */ 00137300
         NI    IOSWTCH+2,255-READFR                                     00137400
*    CALL CHECKHDR; /*SET UP TO READ RECORDS WRITTEN TO LOGREC WHILE*/  00137500
*                   /*EREP HAS BEEN PROCESSING                       */ 00137600
         BAL   R14,CHECKHDR                                             00137700
*    IF BADNEWS = ZERO THEN /*NO PROBLEMS, YES THERE ARE MORE RECORDS*/ 00137800
         L     R12,BADNEWS                                              00137900
         LTR   R12,R12                                                  00138000
         BNZ   @RF00588                                                 00138100
*      DO;                                                              00138200
*        CALL SAVEM; /*READ RECORDS INTO CORE                        */ 00138300
         BAL   R14,SAVEM                                                00138400
*        CALL RESETHDR;                                                 00138500
         BAL   R14,RESETHDR                                             00138600
*                                                                       00138700
         DEQ  (RESIDII,LOGRECB,8,SYSTEM)                                00138800
*                                                                       00138900
*        RUNPTR = FRSPTR;                                               00139000
         L     R14,FRSPTR                                               00139100
         ST    R14,RUNPTR                                               00139200
*        IF FRSPTR Œ= ZERO THEN                                         0013930
         LTR   R14,R14                                                  00139400
         BZ    @RF00578                                                 00139500
*         DO UNTIL RUNPTR = ZERO;                                       00139600
*           CALL ISSUE76;                                               00139700
@DL00595 BAL   R14,ISSUE76                                              00139800
*           RUNPTR = FORPTR;                                            00139900
         L     R12,RUNPTR                                               00140000
         L     R14,FORPTR(,R12)                                         00140100
         ST    R14,RUNPTR                                               00140200
*         END;                                                          00140300
         LTR   R14,R14                                                  00140400
         BNZ   @DL00595                                                 00140500
         B     @RF00578                                                 00140600
*                                                                       00140700
*      END;                                                             00140800
*    ELSE                                                               00140900
*      DO;     /* NO RECORDS WRITTEN WHILE PROCESSING?               */ 00141000
*        IF BADNEWS = FOUR THEN                                         00141100
@RF00588 CLC   BADNEWS(4),KF4                                           00141200
         BNE   @RF00601                                                 00141300
*          DO;                                                          00141400
*            BADNEWS=ZERO;                /* RESET RTN CODE          */ 00141500
         SLR   R12,R12                                                  00141600
         ST    R12,BADNEWS                                              00141700
*            CALL RESETHDR;               /* RESET HEADER            */ 00141800
         BAL   R14,RESETHDR                                             00141900
*          END;                                                         00142000
*                                                                       00142100
@RF00601 DEQ  (RESIDII,LOGRECB,8,SYSTEM)                                00142200
*                                                                       00142300
*      END;                                                             00142400
*  END;                                                                 00142500
*                                    /*LET OTHER EREPS AT LOGREC*/      00142600
*                                                                       00142700
@RF00578 DEQ   (RESID,LOGRECA,11,STEP)                                  00142800
*                      /*CLOSE THE DCB                               */ 00142900
         CLOSE (LOGREC)                                                 00143000
*                                                                       00143100
*SERLO = OFF; /*INDICATE THAT SERLOG IS CLOSED                       */ 00143200
         NI    IOSWTCH+1,255-SERLO                                      00143300
         B     @ER00011                                                 00143400
*                                                                       00143500
*END;                                                                   00143600
*ELSE /*IF SERLOG WAS NOT OPEN                                          00143700
*  DO;                                                                  00143800
*    IF CLOSEIO \= CLOSEALL THEN /*END OF JOB WRAPUP CLOSEALL ALLOWED*/ 00143900
@RF00571 L     R14,SAVER2                                               00144000
         CLC   CLOSEIO(2,R14),@CB00262                                  00144100
         BE    @ER00011                                                 00144200
*      DO;                                                              00144300
*        MSGNO = 51;                                                    00144400
         MVC   MSGNO(4),KF51                                            00144500
*        CALL ERRMSG;                                                   00144600
         BAL   R14,ERRMSG                                               00144700
*        BADNEWS = TWELVE;                                              00144800
         MVC   BADNEWS(4),KF12                                          00144900
*      END;                                                             00145000
*  END;                                                                 00145100
*END; /*CLOSING END TO PROC CLOSSERL                                 */ 00145200
@ER00011 LM    R14,R12,@SA00011                                         00145300
         BR    R14                                                      00145400
*                                                                       00145500
*ISSUE76: PROC;                                                         00145600
ISSUE76  STM   R14,R12,@SA00012                                         00145700
*IF DEBUG15 = ON THEN                                                   00145800
         L     R12,SAVER2                                               00145900
         TM    DEBUG15(R12),B'00000001'                                 00146000
         BNO   @RF00625                                                 00146100
*  DO;                                                                  00146200
*    MSGNO = 58;                                                        00146300
         MVC   MSGNO(4),KF58                                            00146400
*    CALL ERRMSG;                                                       00146500
         BAL   R14,ERRMSG                                               00146600
*  END;                                                                 00146700
*R0 =JUNK3;                                                             00146800
@RF00625 L     R12,RUNPTR                                               00146900
         LH    R0,JUNK3(,R12)                                           00147000
*R1 = ADDR(RECSPT);                                                     00147100
         LA    R1,RECSPT(,R12)                                          00147200
*R0 = R0 * (-1);                                                        00147300
         LR    R12,R0                                                   00147400
         MH    R12,KHM1                                                 00147500
         LR    R0,R12                                                   00147600
*SVC(76);                                                               00147700
         SVC   76                                                       00147800
*END; /*CLOSING END TO PROC ISSUE76                                  */ 00147900
@ER00012 LM    R14,R12,@SA00012                                         00148000
         BR    R14                                                      00148100
*                                                                       00148200
*SAVEM: PROC;                                                           00148300
SAVEM    STM   R14,R12,@SA00013                                         00148400
*DO WHILE BADNEWS = ZERO;                                               00148500
         B     @DE00638                                                 00148600
*                                                                       00148700
*  CALL READSER;                                                        00148800
@DL00638 BAL   R14,READSER                                              00148900
*  IF BADNEWS = ZERO THEN                                               00149000
         L     R14,BADNEWS                                              00149100
         LTR   R14,R14                                                  00149200
         BNZ   @DE00638                                                 00149300
*    DO;                                                                00149400
*      DBLYES = NO;                                                     00149500
         NI    IOSWTCH,255-DBLYES                                       00149600
*      GETHIS = BSZ;                                                    00149700
         LH    R7,BSZ                                                   00149800
         ST    R7,GETHIS                                                00149900
*      GETHIS = GETHIS + 12;                                            00150000
         AL    R7,KF12                                                  00150100
         ST    R7,GETHIS                                                00150200
*      DO WHILE DBLYES = NO;                                            00150300
         B     @DE00645                                                 00150400
*                                                                       00150500
*        IF (GETTIT & DBLWORD) \= NULL THEN                             00150600
@DL00645 MVC   WORKD(4),GETTIT                                          00150700
         NC    WORKD(4),KX07                                            00150800
         CLC   WORKD(4),KXNULL                                          00150900
         BE    @RF00646                                                 00151000
*          GETHIS = GETHIS + 1;                                         00151100
         LA    R12,1                                                    00151200
         AL    R12,GETHIS                                               00151300
         ST    R12,GETHIS                                               00151400
         B     @DE00645                                                 00151500
*                                                                       00151600
*        ELSE                                                           00151700
*          DBLYES = YES;                                                00151800
@RF00646 OI    IOSWTCH,DBLYES                                           00151900
*      END;                                                             00152000
@DE00645 TM    IOSWTCH,DBLYES                                           00152100
         BZ    @DL00645                                                 00152200
*      R6 = GETTIT; /*NUMBER OF BYTES NEEDED IN R6                   */ 00152300
         L     R6,GETTIT                                                00152400
*                                          /*STORAGE FOR RECORD*/       00152500
         GETMAIN  EC,LV=(R6),A=SAVEPTR                                  00152600
*                                                                       00152700
*      NUMRECSV = NUMRECSV + 1;                                         00152800
         LA    R7,1                                                     00152900
         AL    R7,NUMRECSV                                              00153000
         ST    R7,NUMRECSV                                              00153100
*      IF R15 = ZERO THEN                                               00153200
         LTR   R15,R15                                                  00153300
         BNZ   @RF00653                                                 00153400
*        DO;                                                            00153500
*          IF SECGET = NO THEN /*IF FIRST RECORD SAVED THEN          */ 00153600
         TM    IOSWTCH,SECGET                                           00153700
         BNZ   @RF00655                                                 00153800
*            DO;                                                        00153900
*              SECGET = YES;                                            00154000
         OI    IOSWTCH,SECGET                                           00154100
*              FRSPTR = SAVEPTR; /*POINTER TO FIRST RECORD SAVED*/      00154200
         L     R12,SAVEPTR                                              00154300
         ST    R12,FRSPTR                                               00154400
*              RUNPTR = SAVEPTR; /*ESTABLISH ADDRESSABILITY          */ 00154500
         ST    R12,RUNPTR                                               00154600
         B     @RC00655                                                 00154700
*                                                                       00154800
*            END;                                                       00154900
*           ELSE /*IF SAVING SECOND RECORD THEN                      */ 00155000
*            DO;                                                        00155100
*              FORPTR = SAVEPTR; /*SET FORWARD POINTER FROM OLD REC*/   00155200
@RF00655 L     R14,SAVEPTR                                              00155300
         L     R7,RUNPTR                                                00155400
         ST    R14,FORPTR(,R7)                                          00155500
*                                /*TO LOCATION OF NEW RECORD SAVE ARR*/ 00155600
*              RUNPTR = SAVEPTR; /*ESTABLISH ADDRESS OF NEW RECORD*/    00155700
         ST    R14,RUNPTR                                               00155800
*            END;                                                       00155900
*          R2 = ADDR(JUNK);                                             00156000
@RC00655 L     R2,RUNPTR                                                00156100
         LA    R2,JUNK(,R2)                                             00156200
*          R3 = GETHIS - 4;                                             00156300
         L     R3,GETHIS                                                00156400
         SL    R3,KF4                                                   00156500
*          R4 = ADDR(BCOUNT); /*MAY BE WRONG                            00156600
         LA    R4,BCOUNT                                                00156700
*          R5 = R3;                                                     00156800
         LR    R5,R3                                                    00156900
*          MVCL(R2,R4);                                                 00157000
         MVCL  R2,R4                                                    00157100
*          FORPTR = ZERO;                                               00157200
         L     R7,RUNPTR                                                00157300
         SLR   R12,R12                                                  00157400
         ST    R12,FORPTR(,R7)                                          00157500
         B     @DE00638                                                 00157600
*                                                                       00157700
*        END;                                                           00157800
*      ELSE                                                             00157900
*        DO;                                                            00158000
*          MSGNO = 50;                                                  00158100
@RF00653 MVC   MSGNO(4),KF50                                            00158200
*          CALL ERRMSG;                                                 00158300
         BAL   R14,ERRMSG                                               00158400
*          BADNEWS = TWELVE;                                            00158500
         MVC   BADNEWS(4),KF12                                          00158600
*        END;                                                           00158700
*     END;                                                              00158800
*   END;                                                                00158900
@DE00638 L     R14,BADNEWS                                              00159000
         LTR   R14,R14                                                  00159100
         BZ    @DL00638                                                 00159200
*END; /*CLOSING END TO PROC SAVEM                                    */ 00159300
@ER00013 LM    R14,R12,@SA00013                                         00159400
         BR    R14                                                      00159500
*                                                                       00159600
*ERRMSG: PROC; /*WRITE ERROR MESSAGES TO THE TOURIST DATA SET*/         00159700
*                                                                       00159800
*        IF THE PROBLEM EXISTS WITH THE TOURIST DATA SET THEN           00159900
*        WRITE TO THE OPERATOR. IF THE OPERATOR CANNOT BE               00160000
*        REACHED, THEN YOU ARE ASKING TOO MUCH OF THIS PROGRAM          00160100
*                                                                       00160200
ERRMSG   STM   R14,R12,@SA00014                                         00160300
*                             /*CLEAR INTERNAL PRINT BUFFER          */ 00160400
         MVC   PRNTBF(134),BLANKS                                       00160500
*IF MSGNO <= 52 THEN            /* SYSTEM ERROR MESSAGE ?            */ 00160600
         L     R4,MSGNO                                                 00160700
         C     R4,KF52                                                  00160800
         BH    @RF00684                                                 00160900
*  DO;                                                                  00161000
*    R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2               */ 00161100
         L     R6,SAVER2                                                00161200
         LR    R2,R6                                                    00161300
*    CALL IFCMSG(MSGNO,ADDR(PRNTBF)+2);                                 00161400
         LA    R4,PRNTBF                                                00161500
         AL    R4,KF2                                                   00161600
         ST    R4,@AFTEMPS+4                                            00161700
         L     R15,ADIFCMSG(,R6)                                        00161800
         LA    R1,@AL00687                                              00161900
         BALR  R14,R15                                                  00162000
*    PRNTBF(1:2) = '1 ';                                                00162100
         MVC   PRNTBF(2),KC1B                                           00162200
         B     @RC00684                                                 00162300
*                                                                       00162400
*  END;                                                                 00162500
*ELSE                                                                   00162600
*  DO;                                                                  00162700
*    MSGNO = MSGNO - 52;                                                00162800
@RF00684 L     R4,MSGNO                                                 00162900
         SL    R4,KF52                                                  00163000
         ST    R4,MSGNO                                                 00163100
*    R5 = ADDR(ERRLINE) + (51 * (MSGNO - 1)); /*POINT TO MESSAGE TEXT*/ 00163200
         LA    R5,ERRLINE                                               00163300
         BCTR  R4,0                                                     00163400
         MH    R4,KH51                                                  00163500
         ALR   R5,R4                                                    00163600
*                               /*MOVE TEXT TO INTERNAL PRINT BUFFER*/  00163700
         MVC   PRNTBF(51),0(R5)                                         00163800
*  END;                                                                 00163900
*PRINTADR = ADDR(PRNTBF); /*ADDRESS THE DATA SET TYPE                */ 00164000
@RC00684 LA    R6,PRNTBF                                                00164100
         L     R4,SAVER2                                                00164200
         ST    R6,PRINTADR(,R4)                                         00164300
*IF TOUROPN = YES                 THEN /*IF TOURIST DATA SET IS OPEN*/  00164400
         TM    IOSWTCH+1,TOUROPN                                        00164500
         BNO   @RF00696                                                 00164600
*  DO;                                                                  00164700
*     R7 = PRINTADR + 1; /*POINT PAST DATA SET INDICATOR             */ 00164800
         LA    R7,1                                                     00164900
         ALR   R7,R6                                                    00165000
*     CCSAVE = PRINTCC; /*SAVE CARRIAGE CONTROL FROM CALLER          */ 00165100
         MVC   CCSAVE(1),PRINTCC(R6)                                    00165200
*     PRINTCC = '09'X; /*CARRIAGE CONTROL TO PRINT ONE LINE          */ 00165300
         MVI   PRINTCC(R6),X'09'                                        00165400
*                          /*ADDRESS TOURIST DCB                     */ 00165500
         LA    R12,TOURDCB                                              00165600
*                                                                       00165700
         PUT   (12),(R7)                                                00165800
*                                                                       00165900
*     PRINTCC = CCSAVE; /*RESTORE CALLERS CARRIAGE CONTROL           */ 00166000
         L     R4,SAVER2                                                00166100
         L     R6,PRINTADR(,R4)                                         00166200
         MVC   PRINTCC(1,R6),CCSAVE                                     00166300
*     PRINTBDY = BLANKS; /*CLEAR PRINT LINE FOR CALLER               */ 00166400
         MVC   PRINTBDY(132,R6),BLANKS                                  00166500
*     LINECTT = LINECTT + 1;                                            00166600
         LA    R14,1                                                    00166700
         AH    R14,LINECTT                                              00166800
         STH   R14,LINECTT                                              00166900
*     IF LINECTT = LINECT THEN                                          00167000
         CH    R14,LINECT(,R4)                                          00167100
         BNE   @ER00014                                                 00167200
*       DO;                                                             00167300
*         LINECTT = 0;                                                  00167400
         SLR   R4,R4                                                    00167500
         STH   R4,LINECTT                                               00167600
*         CCSAVE = PRINTCC;                                             00167700
         MVC   CCSAVE(1),PRINTCC(R6)                                    00167800
*         PRINTCC = SKIPNEW;                                            00167900
         MVI   PRINTCC(R6),X'8B'                                        00168000
*                                                                       00168100
         PUT   (R12),(R7)                                               00168200
*         PRINTCC = CCSAVE;                                             00168300
         L     R6,SAVER2                                                00168400
         L     R4,PRINTADR(,R6)                                         00168500
         MVC   PRINTCC(1,R4),CCSAVE                                     00168600
         B     @ER00014                                                 00168700
*                                                                       00168800
*       END;                                                            00168900
*  END;                                                                 00169000
* ELSE /*IF TOURIST NOT OPEN THEN GO TO OPERATOR                        00169100
*   DO;                                                                 00169200
*    R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2               */ 00169300
@RF00696 L     R6,SAVER2                                                00169400
         LR    R2,R6                                                    00169500
*    CALL IFCMSG(203,ADDR(PRNTBF));                                     00169600
         LA    R4,PRNTBF                                                00169700
         ST    R4,@AFTEMPS+4                                            00169800
         L     R15,ADIFCMSG(,R6)                                        00169900
         LA    R1,@AL00717                                              00170000
         BALR  R14,R15                                                  00170100
*   END;                                                                00170200
* END; /*CLOSING END TO ERRMSG PROC                                  */ 00170300
@ER00014 LM    R14,R12,@SA00014                                         00170400
         BR    R14                                                      00170500
*                                                                       00170600
*PRINTIT: PROC; /*SELECT PRINT DATA SET AND PRINT LINE REQUESTED*/      00170700
PRINTIT  STM   R14,R12,@SA00015                                         00170800
*IF PRINTDS = 'F0'X THEN /*IF PRINT TO ERREPT THEN                   */ 00170900
         L     R6,SAVER2                                                00171000
         L     R6,PRINTADR(,R6)                                         00171100
         CLI   PRINTDS(R6),X'F0'                                        00171200
         BNE   @RF00723                                                 00171300
*  DO;                                                                  00171400
*    IF ERPTOPN = ON THEN /*IF ERREPT HAS BEEN OPENED THEN           */ 00171500
         TM    IOSWTCH+1,ERPTOPN                                        00171600
         BNO   @RF00725                                                 00171700
*      DO;                                                              00171800
*                              /*ADDRESS ERREPT DCB                  */ 00171900
         LA    R12,EREPTDCB                                             00172000
*        R7 = PRINTADR + 1; /*POINT PAST THE DATA SET DEFINER*/         00172100
         LA    R7,1                                                     00172200
         L     R6,SAVER2                                                00172300
         AL    R7,PRINTADR(,R6)                                         00172400
*                                  /* ENSURE VALID CODE              */ 00172500
         TR    0(1,R7),MACHCODE                                         00172600
*                                                                       00172700
         PUT (12),(7)                                                   00172800
*                                                                       00172900
         L     R6,SAVER2                                                00173000
         L     R1,PRINTADR(,R6)                                         00173100
*        PRINTBDY = BLANKS; /*CLEAR PRINT LINE FOR CALLER            */ 00173200
         MVC   PRINTBDY(132,R1),BLANKS                                  00173300
         LH    R2,LINECTSV(,R6)                                         00173400
         LTR   R2,R2                                                    00173500
         BP    IFCD0C86                                                 00173600
         LH    R4,LINECT(,R6)                                           00173700
         STH   R4,LINECTSV(,R6)                                         00173800
IFCD0C86 L     R6,SAVER2                                                00173900
         L     R1,PRINTADR(,R6)                                         00174000
         CLI   1(R1),X'1B'             CC = SPACE 3 AFTER WRITE ?       00174100
         BNH   IFCD0CA2                                                 00174200
         LH    R3,LINECT(,R6)                                           00174300
         STH   R3,LINECTSV(,R6)                                         00174400
         B     IFCD0D3C                                                 00174500
*                                                                       00174600
IFCD0CA2 L     R6,SAVER2                                                00174700
         L     R1,PRINTADR(,R6)                                         00174800
         CLI   1(R1),X'01'             CC = SPACE 0 AFTER WRITE ?       00174900
         BNE   IFCD0CBE                                                 00175000
         LH    R2,LINECTSV(,R6)                                         00175100
         STH   R2,LINECTSV(,R6)                                         00175200
         B     IFCD0D3C                                                 00175300
*                                                                       00175400
IFCD0CBE L     R6,SAVER2                                                00175500
         L     R6,PRINTADR(,R6)                                         00175600
         CLI   1(R6),X'09'             CC = SPACE 1 AFTER WRITE         00175700
         BE    IFCD0CD6                                                 00175800
         CLI   1(R6),X'0B'             CC = SPACE 1 IMMEDIATE ?         00175900
         BNE   IFCD0CE8                                                 00176000
IFCD0CD6 L     R6,SAVER2                                                00176100
         LH    R14,LINECTSV(,R6)                                        00176200
         BCTR  R14,0                                                    00176300
         STH   R14,LINECTSV(,R6)                                        00176400
         B     IFCD0D3C                                                 00176500
*                                                                       00176600
IFCD0CE8 L     R6,SAVER2                                                00176700
         L     R6,PRINTADR(,R6)                                         00176800
         CLI   1(R6),X'11'             CC = SPACE 2 AFTER WRITE ?       00176900
         BE    IFCD0D00                                                 00177000
         CLI   1(R6),X'13'             CC = SPACE 2 IMMEDIATE ?         00177100
         BNE   IFCD0D14                                                 00177200
IFCD0D00 L     R6,SAVER2                                                00177300
         LH    R14,LINECTSV(,R6)                                        00177400
         BCTR  R14,R0                                                   00177500
         BCTR  R14,R0                                                   00177600
         STH   R14,LINECTSV(,R6)                                        00177700
         B     IFCD0D3C                                                 00177800
*                                                                       00177900
IFCD0D14 L     R6,SAVER2                                                00178000
         L     R6,PRINTADR(,R6)                                         00178100
         CLI   1(R6),X'19'             CC = SPACE 3 AFTER WRITE ?       00178200
         BE    IFCD0D2C                                                 00178300
         CLI   1(R6),X'1B'             CC = SPACE 3 IMMEDIATE ?         00178400
         BNE   IFCD0D3C                                                 00178500
IFCD0D2C L     R6,SAVER2                                                00178600
         LH    R14,LINECTSV(,R6)                                        00178700
         SL    R14,KF3                                                  00178800
         STH   R14,LINECTSV(,R6)                                        00178900
IFCD0D3C L     R6,SAVER2                                                00179000
         L     R6,PRINTADR(,R6)                                         00179100
         MVI   1(R6),X'09'             SET CC = SPACE 1 AFTER WRITE     00179200
         B     @ER00015                                                 00179300
*                                                                       00179400
*      END;                                                             00179500
*    ELSE /*REQUEST TO WRITE TO DATA SET NOT OPEN                    */ 00179600
*      DO;                                                              00179700
*        BADNEWS = TWELVE;                                              00179800
@RF00725 MVC   BADNEWS(4),KF12                                          00179900
*        MSGNO = 19;                                                    00180000
         MVC   MSGNO(4),KF19                                            00180100
*        CALL ERRMSG;                                                   00180200
         BAL   R14,ERRMSG                                               00180300
         B     @ER00015                                                 00180400
*                                                                       00180500
*      END;                                                             00180600
*  END;                                                                 00180700
*ELSE /*REQUEST FOR WRITE TO TOURIST DATA SET                        */ 00180800
*  DO;                                                                  00180900
*    IF TOUROPN = YES THEN /*IF TOURIST DATA SET IS OPEN THEN*/         00181000
@RF00723 TM    IOSWTCH+1,TOUROPN                                        00181100
         BNO   @RF00740                                                 00181200
*      DO;                                                              00181300
*        R7 = PRINTADR + 1; /* POINT PAST DATA SET ID                */ 00181400
         L     R1,SAVER2                                                00181500
         L     R2,PRINTADR(,R1)                                         00181600
         LA    R7,1                                                     00181700
         ALR   R7,R2                                                    00181800
*        CCSAVE = PRINTCC;                                              00181900
         MVC   CCSAVE(1),PRINTCC(R2)                                    00182000
*        PRINTCC = '09'X; /*SET TO PRINT AND SKIP ONE                */ 00182100
         MVI   PRINTCC(R2),X'09'                                        00182200
*                             /*ADDRESS TOURIST DCB                  */ 00182300
         LA    R12,TOURDCB                                              00182400
*                           /*PUT TO TOURIST                         */ 00182500
         PUT   (12),(7)                                                 00182600
*                                                                       00182700
*        PRINTCC = CCSAVE; /*RESTORE USERS CARRIAGE CONTROL          */ 00182800
         L     R6,SAVER2                                                00182900
         L     R14,PRINTADR(,R6)                                        00183000
         MVC   PRINTCC(1,R14),CCSAVE                                    00183100
*        PRINTBDY = BLANKS; /*BLANKS TO CALLERS LINE                 */ 00183200
         MVC   PRINTBDY(132,R14),BLANKS                                 00183300
*        LINECTT = LINECTT + 1;                                         00183400
         LA    R15,1                                                    00183500
         AH    R15,LINECTT                                              00183600
         STH   R15,LINECTT                                              00183700
*        IF LINECTT = LINECT THEN                                       00183800
         CH    R15,LINECT(,R6)                                          00183900
         BNE   @ER00015                                                 00184000
*          DO;                                                          00184100
*            LINECTT = 0;                                               00184200
         SLR   R6,R6                                                    00184300
         STH   R6,LINECTT                                               00184400
*            CCSAVE = PRINTCC;                                          00184500
         MVC   CCSAVE(1),PRINTCC(R14)                                   00184600
*            PRINTCC = SKIPNEW;                                         00184700
         MVI   PRINTCC(R14),X'8B'                                       00184800
*                                                                       00184900
         PUT   (R12),(R7)                                               00185000
*            PRINTCC = CCSAVE;                                          00185100
         L     R6,SAVER2                                                00185200
         L     R6,PRINTADR(,R6)                                         00185300
         MVC   PRINTCC(1,R6),CCSAVE                                     00185400
         B     @ER00015                                                 00185500
*                                                                       00185600
*          END;                                                         00185700
*     END;                                                              00185800
*   ELSE  /*IMPLIES TOURIST DATA SET NOT OPEN                        */ 00185900
*     DO;                                                               00186000
*       R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2            */ 00186100
@RF00740 L     R6,SAVER2                                                00186200
         LR    R2,R6                                                    00186300
*       CALL IFCMSG(203,ADDR(PRNTBF));                                  00186400
         LA    R14,PRNTBF                                               00186500
         ST    R14,@AFTEMPS+8                                           00186600
         L     R15,ADIFCMSG(,R6)                                        00186700
         LA    R1,@AL00761                                              00186800
         BALR  R14,R15                                                  00186900
*       BADNEWS = TWELVE;                                               00187000
         MVC   BADNEWS(4),KF12                                          00187100
*     END;                                                              00187200
*  END;                                                                 00187300
*END; /*CLOSING END TO PROC PRINTIT                                  */ 00187400
@ER00015 LM    R14,R12,@SA00015                                         00187500
         BR    R14                                                      00187600
*                                                                       00187700
*OPENSYS: PROC; /*OPEN SYSIN DATA SET TO READ CONTROL STATEMENTS     */ 00187800
OPENSYS  STM   R14,R12,@SA00016                                         00187900
*IF SYSOPN = NO THEN /* IF ALREADY OPEN IS NO NO                     */ 00188000
         TM    IOSWTCH+1,SYSOPN                                         00188100
         BNZ   @RF00769                                                 00188200
*  DO;                                                                  00188300
*    IF DEBUG15 = ON THEN                                               00188400
         L     R1,SAVER2                                                00188500
         TM    DEBUG15(R1),B'00000001'                                  00188600
         BNO   @RF00771                                                 00188700
*      DO;                                                              00188800
*        MSGNO = 59;                                                    00188900
         MVC   MSGNO(4),KF59                                            00189000
*        CALL ERRMSG;                                                   00189100
         BAL   R14,ERRMSG                                               00189200
*      END;                                                             00189300
@RF00771 LA    R12,SYSDCB                                               00189400
*                                                                       00189500
         OPEN  ((R12),(INPUT))                                          00189600
*                                                                       00189700
         TM    DCBOFLGS,DCBOFOPN                                        00189800
         BZ    SYSOBAD                                                  00189900
*   SYSOPN = YES;                                                       00190000
         OI    IOSWTCH+1,SYSOPN                                         00190100
         B     @ER00016                                                 00190200
*                                                                       00190300
*  END;                                                                 00190400
*ELSE                                                                   00190500
*  DO;                                                                  00190600
*    MSGNO = 33;                                                        00190700
@RF00769 MVC   MSGNO(4),KF33                                            00190800
*    CALL ERRMSG;                                                       00190900
         BAL   R14,ERRMSG                                               00191000
*    BADNEWS = TWELVE;                                                  00191100
         MVC   BADNEWS(4),KF12                                          00191200
*  END;                                                                 00191300
*END; /*CLOSING END TO PROC OPENSYS                                  */ 00191400
@ER00016 LM    R14,R12,@SA00016                                         00191500
         BR    R14                                                      00191600
*                                                                       00191700
*CLOSSYS: PROC; /*CLOSE SYSIN DATA SET FOR READING CONTROL STATEMENTS*/ 00191800
CLOSSYS  STM   R14,R12,@SA00017                                         00191900
*IF SYSOPN = YES THEN                                                   00192000
         TM    IOSWTCH+1,SYSOPN                                         00192100
         BNO   @RF00788                                                 00192200
*  DO;                                                                  00192300
*    IF DEBUG15 = ON THEN                                               00192400
         L     R1,SAVER2                                                00192500
         TM    DEBUG15(R1),B'00000001'                                  00192600
         BNO   @RF00790                                                 00192700
*      DO;                                                              00192800
*        MSGNO = 60;                                                    00192900
         MVC   MSGNO(4),KF60                                            00193000
*        CALL ERRMSG;                                                   00193100
         BAL   R14,ERRMSG                                               00193200
*      END;                                                             00193300
*                                                                       00193400
@RF00790 CLOSE (SYSDCB)                                                 00193500
*                                                                       00193600
*    SYSOPN = NO;                                                       00193700
         NI    IOSWTCH+1,255-SYSOPN                                     00193800
         B     @ER00017                                                 00193900
*                                                                       00194000
*  END;                                                                 00194100
*ELSE                                                                   00194200
*  DO;                                                                  00194300
*    IF CLOSEIO Œ= CLOSEALL THEN                                        0019440
@RF00788 L     R1,SAVER2                                                00194500
         CLC   CLOSEIO(2,R1),@CB00262                                   00194600
         BE    @ER00017                                                 00194700
*      DO;                                                              00194800
*        MSGNO = 34;                                                    00194900
         MVC   MSGNO(4),KF34                                            00195000
*        CALL ERRMSG;                                                   00195100
         BAL   R14,ERRMSG                                               00195200
*        BADNEWS = 12;                                                  00195300
         MVC   BADNEWS(4),KF12                                          00195400
*      END;                                                             00195500
*   END;                                                                00195600
*END; /*CLOSING END TO PROC CLOSSYS                                  */ 00195700
@ER00017 LM    R14,R12,@SA00017                                         00195800
         BR    R14                                                      00195900
*                                                                       00196000
*REEDSYS: PROC OPTIONS(DONTSAVE(R7)); /*READ THE SYSIN CONTROL STMTS */ 00196100
REEDSYS  STM   R14,R6,@SA00018                                          00196200
         STM   R8,R12,@SA00018+36                                       00196300
*IF SYSOPN = YES THEN                                                   00196400
         TM    IOSWTCH+1,SYSOPN                                         00196500
         BNO   @RF00810                                                 00196600
*  DO;                                                                  00196700
*    SYSRDCT = SYSRDCT + 1; /*NUMBER OF READS ATTEMPTED              */ 00196800
         LA    R6,1                                                     00196900
         AL    R6,SYSRDCT                                               00197000
         ST    R6,SYSRDCT                                               00197100
         LA    R7,CARDSPOT             R7  -> READIN AREA               00197200
         LA    R12,SYSDCB              R12 -> DCB                       00197300
*                                                                       00197400
         GET   (12),(7)                                                 00197500
*                                                                       00197600
         B     @ER00018                                                 00197700
*                                                                       00197800
*  END;                                                                 00197900
*ELSE                                                                   00198000
*  DO;                                                                  00198100
*    MSGNO = 35;                                                        00198200
@RF00810 MVC   MSGNO(4),KF35                                            00198300
*    CALL ERRMSG;                                                       00198400
         BAL   R14,ERRMSG                                               00198500
*    BADNEWS = TWELVE;                                                  00198600
         MVC   BADNEWS(4),KF12                                          00198700
*  END;                                                                 00198800
*END; /*CLOSING END TO REEDSYS PROC                                  */ 00198900
@ER00018 LM    R14,R6,@SA00018                                          00199000
         LM    R8,R12,@SA00018+36                                       00199100
         BR    R14                                                      00199200
*                                                                       00199300
*SCRCHIT: PROC; /*WRITE TO DIRECTWK DATA SET SEQUENTIALLY            */ 00199400
SCRCHIT  STM   R14,R12,@SA00019                                         00199500
*IF INDIR = YES & /*IF DATA SET IS OPEN AND OPEN FOR SEQUENTIAL*/       00199600
*   CLOSE2 = NO THEN /*WRITING THEN                                  */ 00199700
         TM    IOSWTCH+1,INDIR                                          00199800
         BNO   @RF00824                                                 00199900
         TM    IOSWTCH+1,CLOSE2                                         00200000
         BNZ   @RF00824                                                 00200100
*  DO;                                                                  00200200
*    RITEWKCT = RITEWKCT + 1; /*NUMBER OF WRITE ATTEMPTS             */ 00200300
         LA    R0,1                                                     00200400
         AL    R0,RITEWKCT                                              00200500
         ST    R0,RITEWKCT                                              00200600
         LA    R7,BUF1     GET ADDRESS OF RECORD                        00200700
*        IF FLIPFLOP = OFF THEN /*MUST REARRANGE SERLOG PRE RECORD*/    00200800
         TM    IOSWTCH,FLIPFLOP                                         00200900
         BNZ   @RF00828                                                 00201000
         MVC   SDRK,OUTHIST+DCBLRECL-IHADCB                             00201100
*          DO;                                                          00201200
*            BBRDW = SAVELENT; /*GET UNMODIFIED QSAM RECORD LENGTH*/    00201300
         LH    R6,SDRK                                                  00201400
         CH    R6,SAVELENT                                              00201500
         BNL   IFCD0F4C                                                 00201600
         STH   R6,SAVELENT                                              00201700
IFCD0F4C LH    R6,SAVELENT                                              00201800
         STH   R6,BBRDW                                                 00201900
*            BSIZE = '0000'X;                                           00202000
         XC    BSIZE(2),BSIZE                                           00202100
*            R7 = ADDR(BBRDW);                                          00202200
         LA    R7,BBRDW                                                 00202300
*          END;                                                         00202400
@RF00828 LA    R12,WORKWDCB   GET DCB ADDRESS                           00202500
*                                                                       00202600
         WRITE WORKWDEC,SF,(12),(7)  ISSUE WRITE                        00202700
*                                                                       00202800
         CHECK WORKWDEC     MAKE SURE WRITE COMPLETES BEFORE PROCEED    00202900
*                                                                       00203000
         NOTE  (12)     RETRIEVE RELATIVE RECORD LOCATION ON DIRECTWK   00203100
*                                                                       00203200
         ST    R1,TTRSAVE  SAVE TTR FOR SORTABLE                        00203300
         CLC   TTRSAVE(2),MAXTRK         ON LAST TRACK OF DATA SET ?    00203400
         BNL   OVERFLOW    GO TELL OF OVERFLOW CONDITION                00203500
*RECCCHHR = TTRSAVE; /*PUT IN COMTABLE FOR USER ACCESS               */ 00203600
         L     R6,SAVER2                                                00203700
         MVI   RECCCHHR+4(R6),C' '                                      00203800
         MVC   RECCCHHR(4,R6),TTRSAVE                                   00203900
         B     @ER00019                                                 00204000
*                                                                       00204100
*END;                                                                   00204200
*ELSE /*IF DATA SET IS NOT OPEN                                      */ 00204300
*  DO;                                                                  00204400
*    MSGNO = 28;                                                        00204500
@RF00824 MVC   MSGNO(4),KF28                                            00204600
*    CALL ERRMSG;                                                       00204700
         BAL   R14,ERRMSG                                               00204800
*    BADNEWS = TWELVE;                                                  00204900
         MVC   BADNEWS(4),KF12                                          00205000
*  END;                                                                 00205100
*END; /*CLOSING END TO SCRSCHT PROC                                  */ 00205200
@ER00019 LM    R14,R12,@SA00019                                         00205300
         BR    R14                                                      00205400
*                                                                       00205500
*DIRRDSCR: PROC OPTIONS(DONTSAVE(R7)); /*READ DIRECTWK SEQUENTIALLY*/   00205600
DIRRDSCR STM   R14,R6,@SA00020                                          00205700
         STM   R8,R12,@SA00020+36                                       00205800
*                                     /*USING POINT                  */ 00205900
*IF INDIR = YES &  /*IF DATA SET OPEN AND OPEN FOR SEQUENTIAL*/         00206000
*   CLOSE2 = YES THEN /*POINT READING                                */ 00206100
         TM    IOSWTCH+1,INDIR+CLOSE2                                   00206200
         BNO   @RF00846                                                 00206300
*  DO;                                                                  00206400
*    READWKCT = READWKCT + 1;                                           00206500
         LA    R14,1                                                    00206600
         AL    R14,READWKCT                                             00206700
         ST    R14,READWKCT                                             00206800
         LA    R7,BUF1                                                  00206900
         LA    R12,WORKRDCB                                             00207000
         LA    R6,RECCCHHR                                              00207100
         A     R6,SAVER2                                                00207200
*                                                                       00207300
         POINT (12),(6)                                                 00207400
*                                                                       00207500
         READ  WORKRDEC,SF,(12),(7)                                     00207600
*                                                                       00207700
         CHECK WORKRDEC                                                 00207800
*                                                                       00207900
*        R7 = R7 + 4; /*POINT PAST RDW FOR RETURN TO CALLER          */ 00208000
         LA    R15,4                                                    00208100
         ALR   R7,R15                                                   00208200
*        BUF1L = BUF1RDWL; /*MOVE RECORD LENGTH TO 2 BYTES BEFORE REC*/ 00208300
         LH    R14,BUF1RDWL                                             00208400
         STH   R14,BUF1L                                                00208500
*        BUF1L = BUF1L - 4; /*RECORD DATA LENGTH ONLY                */ 00208600
         SLR   R14,R15                                                  00208700
         STH   R14,BUF1L                                                00208800
*        SCRATCH = YES; /*JUST READ CURRENT RECORD OFF DIRECTWK*/       00208900
         OI    IOSWTCH+2,SCRATCH                                        00209000
*        RECLNGTH = BUF1L;                                              00209100
         L     R15,SAVER2                                               00209200
         STH   R14,RECLNGTH(,R15)                                       00209300
         B     @ER00020                                                 00209400
*                                                                       00209500
*  END;                                                                 00209600
*ELSE /*DATA SET NOT OPEN                                            */ 00209700
*  DO;                                                                  00209800
*    MSGNO = 46;                                                        00209900
@RF00846 MVC   MSGNO(4),KF46                                            00210000
*    CALL ERRMSG;                                                       00210100
         BAL   R14,ERRMSG                                               00210200
*    BADNEWS = TWELVE;                                                  00210300
         MVC   BADNEWS(4),KF12                                          00210400
*  END;                                                                 00210500
*END; /*CLOSING END TO PROC DIRRDSER                                 */ 00210600
@ER00020 LM    R14,R6,@SA00020                                          00210700
         LM    R8,R12,@SA00020+36                                       00210800
         BR    R14                                                      00210900
*                                                                       00211000
*DIRECTIT: PROC OPTIONS(DONTSAVE(R7)); /*CHOOSE DIRECT READ OF SERLOG*/ 00211100
DIRECTIT STM   R14,R6,@SA00021                                          00211200
         STM   R8,R12,@SA00021+36                                       00211300
*                                     /*OR DIRECTWK                  */ 00211400
*IF HISTYES = YES THEN /*IS DIRECTWK DATA SET                        */ 00211500
         TM    IOSWTCH,HISTYES                                          00211600
         BNO   @RF00865                                                 00211700
*  CALL DIRRDSCR;                                                       00211800
         BAL   R14,DIRRDSCR                                             00211900
         B     @ER00021                                                 00212000
*                                                                       00212100
*ELSE /*IS SERLOG BY DEFAULT                                         */ 00212200
*  CALL DIRRDSER;                                                       00212300
@RF00865 BAL   R14,DIRRDSER                                             00212400
*END; /*CLOSING END TO DIRECTIT                                      */ 00212500
@ER00021 LM    R14,R6,@SA00021                                          00212600
         LM    R8,R12,@SA00021+36                                       00212700
         BR    R14                                                      00212800
*                                                                       00212900
*CLOSDIR: PROC; /*CLOSE DIRECTWK DATA SET                            */ 00213000
CLOSDIR  STM   R14,R12,@SA00022                                         00213100
*IF INDIR = YES THEN /*IF DATA SET IS OPEN THEN                      */ 00213200
         TM    IOSWTCH+1,INDIR                                          00213300
         BNO   @RF00872                                                 00213400
*  DO;                                                                  00213500
*    IF CLOSE2 = NO THEN /*IF CLOSING DIRECTWK FOR WRITING           */ 00213600
         TM    IOSWTCH+1,CLOSE2                                         00213700
         BNZ   @RF00874                                                 00213800
*      DO;                                                              00213900
*        IF DEBUG15 = ON THEN                                           00214000
         L     R1,SAVER2                                                00214100
         TM    DEBUG15(R1),B'00000001'                                  00214200
         BNO   @RF00876                                                 00214300
*          DO;                                                          00214400
*            MSGNO = 61;                                                00214500
         MVC   MSGNO(4),KF61                                            00214600
*            CALL ERRMSG;                                               00214700
         BAL   R14,ERRMSG                                               00214800
*          END;                                                         00214900
*        CLOSE2 = YES; /*INDICATE CLOSED FOR WRITING                 */ 00215000
@RF00876 OI    IOSWTCH+1,CLOSE2                                         00215100
*                                                                       00215200
         CLOSE (WORKWDCB)           CLOSE WRITE DCB                     00215300
*                                                                       00215400
         LA    R12,WORKRDCB         GET READING DCB                     00215500
*                                                                       00215600
         OPEN  ((R12),(INPUT))      OPEN FOR READING DIRECTWK           00215700
*                                                                       00215800
         TM    DCBOFLGS,DCBOFOPN    IF OPEN SUCCESSFUL                  00215900
         BZ    BADBRSEQ     IF FAILS,GO PRINT MESSAGE;                  00216000
*    INDIR = YES; /*INDICATE OPEN SUCCESSFUL                         */ 00216100
         OI    IOSWTCH+1,INDIR                                          00216200
         L     R1,SAVER2                                                00216300
         TM    DEBUG15(R1),B'00000001'                                  00216400
         BNO   @ER00022                                                 00216500
         MVC   MSGNO(4),KF82                                            00216600
         BAL   R14,ERRMSG                                               00216700
         B     @ER00022                                                 00216800
*                                                                       00216900
*   END;                                                                00217000
*  ELSE /*CLOSE FOR SEQUENTIAL POINT READING                         */ 00217100
*    DO;                                                                00217200
*      IF DEBUG15 = ON THEN                                             00217300
@RF00874 L     R1,SAVER2                                                00217400
         TM    DEBUG15(R1),B'00000001'                                  00217500
         BNO   @RF00886                                                 00217600
*        DO;                                                            00217700
*          MSGNO = 62;                                                  00217800
         MVC   MSGNO(4),KF62                                            00217900
*          CALL ERRMSG;                                                 00218000
         BAL   R14,ERRMSG                                               00218100
*        END;                                                           00218200
*                             /*ISSUE CLOSE SVC                      */ 00218300
@RF00886 CLOSE (WORKRDCB)                                               00218400
*                                                                       00218500
         B     @ER00022                                                 00218600
*                                                                       00218700
*    END;                                                               00218800
*   END;                                                                00218900
* ELSE  /*CLOSE REQUESTED WHEN NOT OPEN                              */ 00219000
*   DO;                                                                 00219100
*     IF CLOSEIO Œ= CLOSEALL THEN /*END OF JOB WRAPUP IS ALLOWED*/      0021920
@RF00872 L     R1,SAVER2                                                00219300
         CLC   CLOSEIO(2,R1),@CB00262                                   00219400
         BE    @ER00022                                                 00219500
*       DO;                                                             00219600
*         MSGNO = 29;                                                   00219700
         MVC   MSGNO(4),KF29                                            00219800
*         CALL ERRMSG;                                                  00219900
         BAL   R14,ERRMSG                                               00220000
*         BADNEWS = TWELVE;                                             00220100
         MVC   BADNEWS(4),KF12                                          00220200
*       END;                                                            00220300
*   END;                                                                00220400
*END; /*CLOSING END TO PROC CLOSDIR                                  */ 00220500
@ER00022 LM    R14,R12,@SA00022                                         00220600
         BR    R14                                                      00220700
*                                                                       00220800
*READSIT: PROC OPTIONS(DONTSAVE(R7)); /*READ FROM SERLOG OR ACCIN*/     00220900
*                                     /*SEQUENTIALLY                 */ 00221000
READSIT  STM   R14,R6,@SA00023                                          00221100
         STM   R8,R12,@SA00023+36                                       00221200
*IF INHISO = YES THEN /*IF ACCIN IS OPEN, READ IT. IF BOTH SERLOG */    00221300
         TM    IOSWTCH,INHISO                                           00221400
         BNO   @RF00906                                                 00221500
*  CALL READHIST;     /*AND ACCIN ARE OPEN, READ ACCIN FIRST         */ 00221600
         BAL   R14,READHIST                                             00221700
         B     @ER00023                                                 00221800
*                                                                       00221900
*ELSE           /*IMPLIES HISTORY NOT OPEN                           */ 00222000
*  DO;                                                                  00222100
*    IF SERLO = YES THEN /*IF SERLOG IS OPEN THEN IMPLIES ACCIN*/       00222200
@RF00906 TM    IOSWTCH+1,SERLO                                          00222300
         BNO   @RF00909                                                 00222400
*      CALL READSER;    /*NEVER OPEN OR ACCIN HAS BEEN READ, CLOSED*/   00222500
         BAL   R14,READSER                                              00222600
         B     @ER00023                                                 00222700
*                                                                       00222800
*    ELSE                /*NEITHER DATA SET OPEN IMPLIED             */ 00222900
*      DO;                                                              00223000
*        BADNEWS = TWELVE; /*SET BAD RETURN CODE                     */ 00223100
@RF00909 MVC   BADNEWS(4),KF12                                          00223200
*        MSGNO = 21;     /*INDICATE ERROR MESSAGE TEXT NUMBER*/         00223300
         MVC   MSGNO(4),KF21                                            00223400
*        CALL ERRMSG; /*PRINT THE ERROR TO TOURIST                   */ 00223500
         BAL   R14,ERRMSG                                               00223600
*      END;                                                             00223700
*   END;                                                                00223800
* END; /*CLOSING END TO READSIT PROC                                 */ 00223900
@ER00023 LM    R14,R6,@SA00023                                          00224000
         LM    R8,R12,@SA00023+36                                       00224100
         BR    R14                                                      00224200
*                                                                       00224300
*READHIST: PROC OPTIONS(DONTSAVE(R7)); /*READ ACCIN SEQUENTIALLY*/      00224400
READHIST STM   R14,R6,@SA00024                                          00224500
         STM   R8,R12,@SA00024+36                                       00224600
*    ACIRDCT = ACIRDCT + 1; /*ACCIN READ ATTEMPTS                    */ 00224700
         LA    R6,1                                                     00224800
         AL    R6,ACIRDCT                                               00224900
         ST    R6,ACIRDCT                                               00225000
*    R7 = ADDR(BUF1RDW); /*GET ADDRESS OF WORKAREA                   */ 00225100
         LA    R7,BUF1RDW                                               00225200
*                        /*GET ADDRESS OF DCB                        */ 00225300
         LA    R12,INHIST                                               00225400
*                         /*RETRIEVE A RECORD. NOTE THAT RECORD WILL*/  00225500
         GET   (R12),(R7)                                               00225600
*                       /*WILL BE HERE FOR RITE TO SCRATCH AND FOR*/    00225700
*                       /*WRITE TO ACCDEV                            */ 00225800
*    R7 = R7 + 4; /*POINT PAST RDW FOR RETURN TO CALLER              */ 00225900
         LA    R6,4                                                     00226000
         ALR   R7,R6                                                    00226100
*    FLIPFLOP = YES;  /*TELL READIRCT WHAT STORY IS                  */ 00226200
         OI    IOSWTCH,FLIPFLOP                                         00226300
*    RECCCHHR = (RECCCHHR && RECCCHHR); /*SET TO ACCIN INPUT IN PARM*/  00226400
         L     R14,SAVER2                                               00226500
         XC    RECCCHHR(5,R14),RECCCHHR(R14)                            00226600
*    RECLNGTH = BUF1RDWL-4;  /*SET REC LTH IN PARM TABLE             */ 00226700
         LCR   R6,R6                                                    00226800
         AH    R6,BUF1RDWL                                              00226900
         STH   R6,RECLNGTH(,R14)                                        00227000
*END; /*CLOSING END TO READHIST PROC                                 */ 00227100
@ER00024 LM    R14,R6,@SA00024                                          00227200
         LM    R8,R12,@SA00024+36                                       00227300
         BR    R14                                                      00227400
*                                                                       00227500
*OPENDIR : PROC; /*OPEN DIRECTWK DATA SET FOR SEQUENTIAL LOAD*/         00227600
OPENDIR  STM   R14,R12,@SA00025                                         00227700
*IF INDIR  = NO THEN /*IF IT HASNT BEEN OPENED BEFORE                */ 00227800
         TM    IOSWTCH+1,INDIR                                          00227900
         BNZ   @RF00933                                                 00228000
*  DO;                                                                  00228100
*    IF DEBUG15 = ON THEN                                               00228200
         L     R15,SAVER2                                               00228300
         TM    DEBUG15(R15),B'00000001'                                 00228400
         BNO   @RF00935                                                 00228500
*      DO;                                                              00228600
*        MSGNO = 63;                                                    00228700
         MVC   MSGNO(4),KF63                                            00228800
*        CALL ERRMSG;                                                   00228900
         BAL   R14,ERRMSG                                               00229000
*      END;                                                             00229100
*                          /*GET ADDRESS OF DCB                      */ 00229200
@RF00935 LA    R12,WORKWDCB                                             00229300
*                                /*OPEN DIRECTWK TO LOAD SEQUENTIALLY*/ 00229400
         OPEN  ((R12),(OUTPUT))                                         00229500
*                                    /*WAS OPEN SUCCESSFUL           */ 00229600
         TM    DCBOFLGS,DCBOFOPN                                        00229700
*                       /*SET BAD RETURN CODE AND PRINT ERRMSG*/        00229800
         BZ    BADBWSEQ                                                 00229900
         L     R1,DCBDEBAD  R1 -> DEB                                   00230000
*                           LOAD NUMBER OF TRACKS IN THIS EXTENT        00230100
         LH    R1,DEBNMTRK-DEBDASD+DEBBASND-DEBBASIC(R1)                00230200
         BCTR  R1,0         SUBTRACT ONE FOR RELATIVE TRACK NUMBER      00230300
         STH   R1,MAXTRK    SAVE NUMBER OF TRACKS OF OVERFLOW CHECK     00230400
*INDIR = YES; /*SET SUCCESSFUL OPEN OF DIRECTWK FLAG                 */ 00230500
         OI    IOSWTCH+1,INDIR                                          00230600
         B     @ER00025                                                 00230700
*                                                                       00230800
*END;                                                                   00230900
*ELSE /*IF DATA SET IS ALREADY OPEN                                  */ 00231000
*  DO;                                                                  00231100
*    BADNEWS = TWELVE; /*SET RETURN CODE TO ERROR                    */ 00231200
@RF00933 MVC   BADNEWS(4),KF12                                          00231300
*    MSGNO = 31;                                                        00231400
         MVC   MSGNO(4),KF31                                            00231500
*    CALL ERRMSG; /*WRITE MESSAGE TO TOURIST DATA SET                */ 00231600
         BAL   R14,ERRMSG                                               00231700
*  END;                                                                 00231800
*END; /*CLOSING END TO OPENDIR PROC                                  */ 00231900
@ER00025 LM    R14,R12,@SA00025                                         00232000
         BR    R14                                                      00232100
*                                                                       00232200
*OPENHIST: PROC; /*OPEN ACCIN DATA SET                               */ 00232300
OPENHIST STM   R14,R12,@SA00026                                         00232400
*IF INHISO = NO THEN /*IF IT HASNT BEEN OPENED BEFORE                */ 00232500
         TM    IOSWTCH,INHISO                                           00232600
         BNZ   @RF00956                                                 00232700
*  DO;                                                                  00232800
*    IF DEBUG15 = ON THEN                                               00232900
         L     R1,SAVER2                                                00233000
         TM    DEBUG15(R1),B'00000001'                                  00233100
         BNO   @RF00958                                                 00233200
*      DO;                                                              00233300
*        MSGNO = 64;                                                    00233400
         MVC   MSGNO(4),KF64                                            00233500
*        CALL ERRMSG;                                                   00233600
         BAL   R14,ERRMSG                                               00233700
*      END;                                                             00233800
*                        /*GET ADDRESS OF DCB                        */ 00233900
@RF00958 LA    R12,INHIST                                               00234000
*                               /*OPEN ACCIN                         */ 00234100
         OPEN  ((R12),(INPUT))                                          00234200
*                             /*WAS OPEN SUCCESSFUL ?                */ 00234300
         TM    DCBOFLGS,DCBOFOPN                                        00234400
*                      /*SET BAD RETURN CODE AND PRINT ERRMSG*/         00234500
         BZ    BDOIHST                                                  00234600
*                              /*IS CORRECT FORMAT                   */ 00234700
         TM    DCBRECFM,DCBRECV                                         00234800
*                      /*IF NOT, GO TELL WORLD, SET BAD R.C.         */ 00234900
         BZ    BDIST                                                    00235000
*                              /*IS RECFM U                          */ 00235100
         TM    DCBRECFM,DCBRECU                                         00235200
         BO    BDIST                                                    00235300
*    INHISO = YES; /*TELL OF SUCCESSFUL OPEN OF ACCIN                */ 00235400
*    HISTYES = YES;                                                     00235500
         OI    IOSWTCH,HISTYES+INHISO                                   00235600
         B     @ER00026                                                 00235700
*                                                                       00235800
*  END;                                                                 00235900
*ELSE /*IF OPEN ASKED WHEN ALREADY OPEN                                 00236000
*  DO;                                                                  00236100
*    MSGNO = 11; /*TELL WHAT MESSAGE NUMBER TO PRINT                 */ 00236200
@RF00956 MVC   MSGNO(4),KF11                                            00236300
*    CALL ERRMSG; /*GO SELECT MESSAGE AND PRINT TO TOURIST           */ 00236400
         BAL   R14,ERRMSG                                               00236500
*    BADNEWS = TWELVE; /*TELL USER BAD NEWS                          */ 00236600
         MVC   BADNEWS(4),KF12                                          00236700
*  END;                                                                 00236800
*END; /*CLOSING END TO OPEN HISTORY INPUT DATA SET PROC              */ 00236900
@ER00026 LM    R14,R12,@SA00026                                         00237000
         BR    R14                                                      00237100
*                                                                       00237200
*CLOSHIST: PROC; /*CLOSE ACCIN DATA SET                              */ 00237300
CLOSHIST STM   R14,R12,@SA00027                                         00237400
*IF INHISO = YES THEN /*IF DATA SET IS OPEN THEN                     */ 00237500
         TM    IOSWTCH,INHISO                                           00237600
         BNO   @RF00983                                                 00237700
*  DO;                                                                  00237800
*    IF DEBUG15 = ON THEN                                               00237900
         L     R1,SAVER2                                                00238000
         TM    DEBUG15(R1),B'00000001'                                  00238100
         BNO   @RF00985                                                 00238200
*      DO;                                                              00238300
*        MSGNO = 65;                                                    00238400
         MVC   MSGNO(4),KF65                                            00238500
*        CALL ERRMSG;                                                   00238600
         BAL   R14,ERRMSG                                               00238700
*      END;                                                             00238800
*                         /*GET ACCIN DCB ADDRESS                    */ 00238900
@RF00985 LA    R12,INHIST                                               00239000
*                                  /*ISSUE CLOSE SVC                 */ 00239100
         CLOSE  (INHIST)                                                00239200
*                          /*GET RID OF ACCIN BUFFERS                */ 00239300
         FREEPOOL  (R12)                                                00239400
*    INHISO = NO; /*SAY THAT ACCIN IS CLOSED                         */ 00239500
         NI    IOSWTCH,255-INHISO                                       00239600
         B     @ER00027                                                 00239700
*                                                                       00239800
*  END;                                                                 00239900
*ELSE  /*IF DATA SET IS ALREADY CLOSED                               */ 00240000
*  DO;                                                                  00240100
*    IF CLOSEIO Œ= CLOSEALL THEN                                        0024020
@RF00983 L     R1,SAVER2                                                00240300
         CLC   CLOSEIO(2,R1),@CB00262                                   00240400
         BE    @ER00027                                                 00240500
*    DO;                                                                00240600
*    MSGNO = 12;                                                        00240700
         LA    R2,12                                                    00240800
         ST    R2,MSGNO                                                 00240900
*    BADNEWS = TWELVE;                                                  00241000
         ST    R2,BADNEWS                                               00241100
*    CALL ERRMSG;                                                       00241200
         BAL   R14,ERRMSG                                               00241300
*    END;                                                               00241400
*  END;                                                                 00241500
*END; /*CLOSING END TO CLOSE ACCIN DATA SET PROC                     */ 00241600
@ER00027 LM    R14,R12,@SA00027                                         00241700
         BR    R14                                                      00241800
*                                                                       00241900
*CLOSEDEV: PROC; /*PROC TO CLOSE ACCDEV DATA SET                     */ 00242000
CLOSEDEV STM   R14,R12,@SA00028                                         00242100
*IF HISTOPN = YES THEN                                                  00242200
         TM    IOSWTCH,HISTOPN                                          00242300
         BNO   @RF01007                                                 00242400
*  DO;                                                                  00242500
*    IF DEBUG15 = ON THEN                                               00242600
         L     R1,SAVER2                                                00242700
         TM    DEBUG15(R1),B'00000001'                                  00242800
         BNO   @RF01009                                                 00242900
*      DO;                                                              00243000
*        MSGNO = 66;                                                    00243100
         MVC   MSGNO(4),KF66                                            00243200
*        CALL ERRMSG;                                                   00243300
         BAL   R14,ERRMSG                                               00243400
*      END;                                                             00243500
*                         /*FIND ADDRESS OF DCB                      */ 00243600
@RF01009 LA    R12,OUTHIST                                              00243700
*                                /*ISSUE CLOSE SVC                   */ 00243800
         CLOSE (OUTHIST)                                                00243900
*                          /*GET RID OF ACCDEV BUFFERS               */ 00244000
         FREEPOOL  (R12)                                                00244100
*    HISTOPN = NO; /*SAY THAT DATA SET IS CLOSED                     */ 00244200
         NI    IOSWTCH,255-HISTOPN                                      00244300
         B     @ER00028                                                 00244400
*                                                                       00244500
*  END;                                                                 00244600
*ELSE /*IF ACCDEV IS ALREADY CLOSED                                     00244700
* DO;                                                                   00244800
*   IF CLOSEIO \= CLOSEALL /*END OF JOB STEP VALID REQUEST           */ 00244900
*     THEN DO;                                                          00245000
@RF01007 L     R1,SAVER2                                                00245100
         CLC   CLOSEIO(2,R1),@CB00262                                   00245200
         BE    @ER00028                                                 00245300
*       MSGNO = 8;                                                      00245400
         MVC   MSGNO(4),KF8                                             00245500
*       BADNEWS = TWELVE;                                               00245600
         MVC   BADNEWS(4),KF12                                          00245700
*       CALL ERRMSG;                                                    00245800
         BAL   R14,ERRMSG                                               00245900
*     END;                                                              00246000
* END;                                                                  00246100
*END; /*CLOSING END TO PROC CLOSEDEV                                 */ 00246200
@ER00028 LM    R14,R12,@SA00028                                         00246300
         BR    R14                                                      00246400
*                                                                       00246500
*OPENTOUR : PROC; /*OPEN THE TOURIST DATA SET                        */ 00246600
OPENTOUR STM   R14,R12,@SA00029                                         00246700
*  IF TOUROPN = NO THEN /*ILLEGAL TO OPEN AN OPEN DATA SET           */ 00246800
         TM    IOSWTCH+1,TOUROPN                                        00246900
         BNZ   @RF01031                                                 00247000
*    DO;                                                                00247100
         LA    R12,TOURDCB                                              00247200
*                                                                       00247300
         OPEN  ((R12),(OUTPUT))                                         00247400
*                                                                       00247500
         TM    DCBOFLGS,DCBOFOPN  CHECK FOR SUCCESS                     00247600
         BZ    BADOTOUR                                                 00247700
*      TOUROPN = YES; /*HURRAY FOR SUCCESSFUL OPEN                   */ 00247800
         OI    IOSWTCH+1,TOUROPN                                        00247900
*      PRINTADR = ADDR(PRNTBF);                                         00248000
         LA    R6,PRNTBF                                                00248100
         L     R1,SAVER2                                                00248200
         ST    R6,PRINTADR(,R1)                                         00248300
*      R7 = PRINTADR + 1;                                               00248400
         LA    R7,1                                                     00248500
         ALR   R7,R6                                                    00248600
*      PRINTCC = SKIPNEW;                                               00248700
         MVI   PRINTCC(R6),X'8B'                                        00248800
*                            /*INITIAL SKIP TO NEW PAGE              */ 00248900
         PUT  (R12),(R7)                                                00249000
*                                                                       00249100
         B     @ER00029                                                 00249200
*    END;                                                               00249300
* ELSE  /*IF TOURIST ALREADY OPEN THEN                                  00249400
*   DO;                                                                 00249500
*     MSGNO = 23;                                                       00249600
@RF01031 MVC   MSGNO(4),KF23                                            00249700
*     CALL ERRMSG;                                                      00249800
         BAL   R14,ERRMSG                                               00249900
*     BADNEWS = TWELVE;                                                 00250000
         MVC   BADNEWS(4),KF12                                          00250100
*   END;                                                                00250200
*END; /*CLOSING END TO PROC OPENTOUR                                 */ 00250300
@ER00029 LM    R14,R12,@SA00029                                         00250400
         BR    R14                                                      00250500
*                                                                       00250600
*CLOSTOUR: PROC; /*CLOSE THE TOURIST DATA SET                        */ 00250700
CLOSTOUR STM   R14,R12,@SA00030                                         00250800
*IF TOUROPN = YES THEN /*IF REALLY NEEDS CLOSING                     */ 00250900
         TM    IOSWTCH+1,TOUROPN                                        00251000
         BNO   @RF01049                                                 00251100
*  DO;                                                                  00251200
*    IF DEBUG15 = ON THEN /*PRINT SUMMARY OF IO OPERATIONS DURING RUN*/ 00251300
         L     R1,SAVER2                                                00251400
         TM    DEBUG15(R1),B'00000001'                                  00251500
         BNO   @RF01051                                                 00251600
*             DO J = 1 TO 11; /*PRINT ALL DEBUG15 TOTALS             */ 00251700
         LA    R2,1                                                     00251800
         ST    R2,J                                                     00251900
*           MSGNO = J + 70; /*INCREMENT PAST SYSTEM MESSAGES         */ 00252000
@DL01052 AL    R2,KF70                                                  00252100
         ST    R2,MSGNO                                                 00252200
*          CALL ERRMSG; /*PRINT OUT SUMMARY MESSAGE TEXT             */ 00252300
         BAL   R14,ERRMSG                                               00252400
*          PASSNUM = CTPTR(J); /*ACCESS PARTICULAR COUNT             */ 00252500
         L     R3,J                                                     00252600
         SLA   R3,2                                                     00252700
         L     R4,CTPTR-4(R3)                                           00252800
         ST    R4,PASSNUM                                               00252900
*          CVD(PASSNUM,PACKPAS); /*CONVERT BINARY TO PACKED          */ 00253000
         CVD   R4,WORKD                                                 00253100
         MVC   PACKPAS(4),WORKD+4                                       00253200
*          PACKPAS = (PACKPAS | '0000000F'X); /*TURN C TO F          */ 00253300
         OC    PACKPAS(4),@CB01094                                      00253400
*          UNPK(EDFLD,PACKPAS); /*CHANGE PACKED TO PRINTABLE         */ 00253500
         UNPK  EDFLD(9),PACKPAS(4)                                      00253600
*          NONBLANK = OFF;                                              00253700
         NI    IOSWTCH+1,255-NONBLANK                                   00253800
*          DO K = 1 BY 1 TO 8 WHILE NONBLANK = OFF;                     00253900
         LA    R5,1                                                     00254000
         ST    R5,K                                                     00254100
@DL01060 TM    IOSWTCH+1,NONBLANK                                       00254200
         BNZ   @DC01060                                                 00254300
*            IF EDIFILD(K) = CHARZERO THEN                              00254400
         LA    R4,EDIFILD-1(R5)                                         00254500
         CLI   0(R4),X'F0'                                              00254600
         BNE   @RF01061                                                 00254700
*              EDIFILD(K) = BLANK;                                      00254800
         LA    R4,EDIFILD-1(R5)                                         00254900
         MVI   0(R4),X'40'                                              00255000
         B     @RC01061                                                 00255100
*                                                                       00255200
*            ELSE                                                       00255300
*              NONBLANK = ON;                                           00255400
@RF01061 OI    IOSWTCH+1,NONBLANK                                       00255500
*          END;                                                         00255600
@RC01061 LA    R5,1                                                     00255700
         AL    R5,K                                                     00255800
         ST    R5,K                                                     00255900
         C     R5,KF8                                                   00256000
         BNH   @DL01060                                                 00256100
*          PRINTADR = ADDR(PRNTBF);                                     00256200
@DC01060 LA    R6,PRNTBF                                                00256300
         L     R7,SAVER2                                                00256400
         ST    R6,PRINTADR(,R7)                                         00256500
*          PRINTDS = '1'; /*INDICATE PRINT TO TOURIST                */ 00256600
         MVI   PRINTDS(R6),C'1'                                         00256700
*     PRINTBDY = BLANKS; /*CLEAR PRINT LINE FOR CALLER               */ 00256800
         MVC   PRINTBDY(132,R6),BLANKS                                  00256900
         MVC   PRNTBF+10(9),EDFLD                                       00257000
*          CALL PRINTIT;                                                00257100
         BAL   R14,PRINTIT                                              00257200
*        END;                                                           00257300
         LA    R2,1                                                     00257400
         AL    R2,J                                                     00257500
         ST    R2,J                                                     00257600
         C     R2,KF11                                                  00257700
         BNH   @DL01052                                                 00257800
@RF01051 LA    R12,TOURDCB                                              00257900
*                                                                       00258000
         CLOSE (TOURDCB)                                                00258100
*                                                                       00258200
         FREEPOOL (12)                                                  00258300
*    TOUROPN = NO; /*INDICATE CLOSE                                  */ 00258400
         NI    IOSWTCH+1,255-TOUROPN                                    00258500
         B     @ER00030                                                 00258600
*                                                                       00258700
* END;                                                                  00258800
*ELSE /*IF ALREADY CLOSED THEN                                          00258900
* DO;                                                                   00259000
*   IF CLOSEIO Œ= CLOSEALL THEN /*LEGAL TO DO END OF JOB WRAPUP*/       0025910
@RF01049 L     R3,SAVER2                                                00259200
         CLC   CLOSEIO(2,R3),@CB00262                                   00259300
         BE    @ER00030                                                 00259400
*     DO;                                                               00259500
*       R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2            */ 00259600
         LR    R2,R3                                                    00259700
*       CALL IFCMSG(204,ADDR(PRNTBF));                                  00259800
         LA    R2,PRNTBF                                                00259900
         ST    R2,@AFTEMPS+12                                           00260000
         L     R15,ADIFCMSG(,R3)                                        00260100
         LA    R1,@AL01078                                              00260200
         BALR  R14,R15                                                  00260300
*       BADNEWS = TWELVE;                                               00260400
         MVC   BADNEWS(4),KF12                                          00260500
*     END;                                                              00260600
* END;                                                                  00260700
*END; /*CLOSING END TO CLOSTOUR PROCEDURE                            */ 00260800
@ER00030 LM    R14,R12,@SA00030                                         00260900
         BR    R14                                                      00261000
*                                                                       00261100
*OPENERPT: PROC; /*OPEN THE ERREPT D.S. FOR PRINTING                 */ 00261200
OPENERPT STM   R14,R12,@SA00031                                         00261300
*IF ERPTOPN = NO THEN                                                   00261400
         TM    IOSWTCH+1,ERPTOPN                                        00261500
         BNZ   @RF01086                                                 00261600
*  DO;                                                                  00261700
*    IF DEBUG15 = ON THEN                                               00261800
         L     R1,SAVER2                                                00261900
         TM    DEBUG15(R1),B'00000001'                                  00262000
         BNO   @RF01088                                                 00262100
*      DO;                                                              00262200
*        MSGNO = 67;                                                    00262300
         MVC   MSGNO(4),KF67                                            00262400
*        CALL ERRMSG;                                                   00262500
         BAL   R14,ERRMSG                                               00262600
*      END;                                                             00262700
@RF01088 LA    R12,EREPTDCB                                             00262800
*                                                                       00262900
         OPEN  ((R12),(OUTPUT))                                         00263000
*                                                                       00263100
         TM    DCBOFLGS,DCBOFOPN                                        00263200
         BZ    BADORPT                                                  00263300
*    ERPTOPN = YES; /*INDICATE SUCCESSFUL OPEN                       */ 00263400
         OI    IOSWTCH+1,ERPTOPN                                        00263500
*  END;                                                                 00263600
*ELSE /*IF OPEN REQUESTED WHEN DATA SET ALREADY OPEN                    00263700
*  DO;                                                                  00263800
         B     @ER00031                                                 00263900
@RF01086 DS    0H                                                       00264000
*    MSGNO = 45;                                                        00264100
         MVC   MSGNO(4),KF45                                            00264200
*    CALL ERRMSG;                                                       00264300
         BAL   R14,ERRMSG                                               00264400
*    BADNEWS = TWELVE;                                                  00264500
         MVC   BADNEWS(4),KF12                                          00264600
*  END;                                                                 00264700
*END; /*CLOSING END TO PROC OPENERPT                                 */ 00264800
@ER00031 LM    R14,R12,@SA00031                                         00264900
         BR    R14                                                      00265000
*                                                                       00265100
*CLOSERPT : PROC; /*OPEN ERREPT DATA SET                             */ 00265200
CLOSERPT STM   R14,R12,@SA00032                                         00265300
*IF ERPTOPN = YES THEN /*MUST BE OPEN IN ORDER TO CLOSE              */ 00265400
         TM    IOSWTCH+1,ERPTOPN                                        00265500
         BNO   @RF01105                                                 00265600
*  DO;                                                                  00265700
*    IF DEBUG15 = ON THEN                                               00265800
         L     R1,SAVER2                                                00265900
         TM    DEBUG15(R1),B'00000001'                                  00266000
         BNO   @RF01107                                                 00266100
*      DO;                                                              00266200
*        MSGNO = 68;                                                    00266300
         MVC   MSGNO(4),KF68                                            00266400
*        CALL ERRMSG;                                                   00266500
         BAL   R14,ERRMSG                                               00266600
*      END;                                                             00266700
@RF01107 LA    R12,EREPTDCB                                             00266800
*                                                                       00266900
         CLOSE (EREPTDCB)                                               00267000
*                                                                       00267100
         FREEPOOL (R12)                                                 00267200
*    ERPTOPN = NO; /*INDICATE ERREPT IS CLOSED                       */ 00267300
         NI    IOSWTCH+1,255-ERPTOPN                                    00267400
         B     @ER00032                                                 00267500
*                                                                       00267600
*  END;                                                                 00267700
*ELSE /*IF CLOSE REQUESTED WHEN NOT OPEN THEN                           00267800
* DO;                                                                   00267900
*   IF CLOSEIO \= CLOSEALL THEN  /*VALID POTPOURI OF CLOSES          */ 00268000
@RF01105 L     R1,SAVER2                                                00268100
         CLC   CLOSEIO(2,R1),@CB00262                                   00268200
         BE    @ER00032                                                 00268300
*     DO;                                                               00268400
*       MSGNO = 22;                                                     00268500
         MVC   MSGNO(4),KF22                                            00268600
*       CALL ERRMSG;                                                    00268700
         BAL   R14,ERRMSG                                               00268800
*       BADNEWS = TWELVE;                                               00268900
         MVC   BADNEWS(4),KF12                                          00269000
*     END;                                                              00269100
*   END;                                                                00269200
*END; /*CLOSING END TO CLOSERPT PROC                                 */ 00269300
@ER00032 LM    R14,R12,@SA00032                                         00269400
         BR    R14                                                      00269500
*                                                                       00269600
*OPENDEV: PROC; /*OPEN ACCDEV DATA SET                               */ 00269700
OPENDEV  STM   R14,R12,@SA00033                                         00269800
*IF HISTOPN = NO THEN /*IF DATA SET HAS NOT ALREADY BEEN OPENED*/       00269900
         TM    IOSWTCH,HISTOPN                                          00270000
         BNZ   @RF01127                                                 00270100
*  DO;                                                                  00270200
*    IF DEBUG15 = ON THEN                                               00270300
         L     R1,SAVER2                                                00270400
         TM    DEBUG15(R1),B'00000001'                                  00270500
         BNO   @RF01129                                                 00270600
*      DO;                                                              00270700
*        MSGNO = 69;                                                    00270800
         MVC   MSGNO(4),KF69                                            00270900
*        CALL ERRMSG;                                                   00271000
         BAL   R14,ERRMSG                                               00271100
*      END;                                                             00271200
*                           /*FIND ADDRESS OF DCB                    */ 00271300
@RF01129 LA    R12,OUTHIST                                              00271400
*                                     /*ISSUE OPEN SVC               */ 00271500
         OPEN  ((R12),(OUTPUT,DISP))                                    00271600
*                             /*WAS OPEN SUCCESSFULL                 */ 00271700
         TM    DCBOFLGS,DCBOFOPN                                        00271800
*                      /*GO TO ERROR ROUTINE                         */ 00271900
         BZ    BADOOHST                                                 00272000
*    HISTOPN = YES;                                                     00272100
         OI    IOSWTCH,HISTOPN                                          00272200
         B     @ER00033                                                 00272300
*                                                                       00272400
*  END;                                                                 00272500
*ELSE                                                                   00272600
*  DO;                                                                  00272700
*    MSGNO = 2;                                                         00272800
@RF01127 MVC   MSGNO(4),KF2                                             00272900
*    CALL ERRMSG;                                                       00273000
         BAL   R14,ERRMSG                                               00273100
*    BADNEWS = TWELVE; /*ABORT RETURN CODE                              00273200
         MVC   BADNEWS(4),KF12                                          00273300
*  END;                                                                 00273400
*END; /*CLOSING END TO PROC OPENDEV                                  */ 00273500
@ER00033 LM    R14,R12,@SA00033                                         00273600
         BR    R14                                                      00273700
*                                                                       00273800
*HISTIT: PROC; /*WRITE TO ACCDEV USING QSAM                          */ 00273900
HISTIT   STM   R14,R12,@SA00034                                         00274000
*IF HISTOPN = YES THEN                                                  00274100
         TM    IOSWTCH,HISTOPN                                          00274200
         BNO   @RF01149                                                 00274300
*  DO;                                                                  00274400
*    DEVCT = DEVCT + 1;                                                 00274500
         LA    R6,1                                                     00274600
         AL    R6,DEVCT                                                 00274700
         ST    R6,DEVCT                                                 00274800
*                           /*GET DCB ADDRESS                        */ 00274900
         LA    R12,OUTHIST                                              00275000
*    R7 = ADDR(BUF1RDW); /*ADDRESS THE RECORD DESCRIPTOR WORD*/         00275100
         LA    R7,BUF1RDW                                               00275200
*    IF SCRATCH = YES THEN /*IF FRAMES FROM DIRECTWK, MUST FIX REC*/    00275300
*      DO;                 /*ORD DESCRIPTOR WORD BECAUSE IT WAS   */    00275400
*                          /*MANGLED FOR EDSUM MODULES               */ 00275500
         TM    IOSWTCH+2,SCRATCH                                        00275600
         BNO   @RF01154                                                 00275700
*        BUF1RDWL = BUF1L + 4; /*RE-ADD RDW LENGTH, MOVE TO RDW   */    00275800
         LH    R6,BUF1L                                                 00275900
         LA    R6,4(,R6)                                                00276000
         STH   R6,BUF1RDWL                                              00276100
*        BUF1L = '0000'X;      /*FINISH RDW RIGHTMOST TWO BYTES   */    00276200
         SLR   R6,R6                                                    00276300
         STH   R6,BUF1L                                                 00276400
*        R7 = ADDR(BUF1);      /*POINT TO RECORD FOR QSAM PUT     */    00276500
         LA    R7,BUF1                                                  00276600
         B     @RC01154                                                 00276700
*                                                                       00276800
*      END;                                                             00276900
*    ELSE   /*RECORD NOT RETRIEVED FROM DIRECTWK                     */ 00277000
*      IF FLIPFLOP = OFF THEN /*IF RECORD FROM READSEQ OF LOGREC*/      00277100
@RF01154 TM    IOSWTCH,FLIPFLOP                                         00277200
         BNZ   @RC01154                                                 00277300
*        DO;                                                            00277400
*          BBRDW = SAVELENT; /*GET UNMODIFIED QSAM LENGTH            */ 00277500
         MVC   SDRK,OUTHIST+DCBLRECL-IHADCB                             00277600
         LH    R6,SDRK                                                  00277700
         CH    R6,SAVELENT                                              00277800
         BNL   HISTIT01                                                 00277900
         STH   R6,SAVELENT                                              00278000
HISTIT01 LH    R6,SAVELENT                                              00278100
         STH   R6,BBRDW                                                 00278200
*          BSIZE = '0000'X; /*COMPLETE RDW                           */ 00278300
         XC    BSIZE(2),BSIZE                                           00278400
*          R7 = ADDR(BBRDW);                                            00278500
         LA    R7,BBRDW                                                 00278600
*        END;                                                           00278700
*                          /*ISSUE WRITE USING QSAM                  */ 00278800
@RC01154 PUT  (R12),(R7)                                                00278900
         B    @ER00034                                                  00279000
*                                                                       00279100
*  END;                                                                 00279200
*ELSE                                                                   00279300
*  DO;                                                                  00279400
*    BADNEWS = TWELVE;                                                  00279500
@RF01149 MVC   BADNEWS(4),KF12                                          00279600
*    MSGNO = 4;                                                         00279700
         MVC   MSGNO(4),KF4                                             00279800
*    CALL ERRMSG;                                                       00279900
         BAL   R14,ERRMSG                                               00280000
*  END;                                                                 00280100
*END; /*CLOSING END TO PROC HISTIT                                   */ 00280200
@ER00034 LM    R14,R12,@SA00034                                         00280300
         BR    R14                                                      00280400
*                                                                       00280500
*        PROC TO CONVERT HEX DATE INTO PRINTABLE FORMAT                 00280600
*                                                                       00280700
*        NUMBER  = NUMBER OF BYTES TO BE CONVERTED                      00280800
*        NEXTBY -> THE DATA TO BE CONVERTED                             00280900
*        ADDNXT -> WHERE THE CONVERTED DATA IS TO BE PUT                00281000
*                                                                       00281100
*CONVERT: PROC;                                                         00281200
CONVERT  STM   R14,R12,12(R13)                                          00281300
*DO J = NUMBER BY -1 TO 1;                                              00281400
         L     R12,NUMBER                                               00281500
         ST    R12,J                                                    00281600
         B     @DE01177                                                 00281700
*                                                                       00281800
*  R2 = NEXTBYTE(J);                                                    00281900
@DL01177 L     R1,NEXTBY                                                00282000
         BCTR  R1,0                                                     00282100
         SLR   R2,R2                                                    00282200
         IC    R2,NEXTBYTE(R12,R1)                                      00282300
*  K  = 2 * J;                                                          00282400
         LR    R4,R12                                                   00282500
         ALR   R4,R4                                                    00282600
         ST    R4,K                                                     00282700
*  SRDL(R2,4);                                                          00282800
         SRDL  R2,4                                                     00282900
*  SRL(R3,28);                                                          00283000
         SRL   R3,28                                                    00283100
*  R3 = R3 + 1;                                                         00283200
         LA    R5,1                                                     00283300
         ALR   R3,R5                                                    00283400
*  R2 = R2 + 1;                                                         00283500
         ALR   R2,R5                                                    00283600
*  ADDNUM(K-1) = CHARS(R2);                                             00283700
         L     R5,ADDNXT                                                00283800
         ALR   R4,R5                                                    00283900
         BCTR  R4,0                                                     00284000
         BCTR  R4,0                                                     00284100
         LA    R1,CHARS-1(R2)                                           00284200
         MVC   ADDNUM(1,R4),0(R1)                                       00284300
*  ADDNUM(K) = CHARS(R3);                                               00284400
         L     R4,K                                                     00284500
         ALR   R5,R4                                                    00284600
         BCTR  R5,0                                                     00284700
         LA    R4,CHARS-1(R3)                                           00284800
         MVC   ADDNUM(1,R5),0(R4)                                       00284900
*END;                                                                   00285000
         BCTR  R12,0                                                    00285100
         ST    R12,J                                                    00285200
@DE01177 LTR   R12,R12                                                  00285300
         BP    @DL01177                                                 00285400
*END; /*CLOSING END TO PROC CONVERT                                  */ 00285500
@ER00035 LM    R14,R12,12(R13)                                          00285600
         BR    R14                                                      00285700
*                                                                       00285800
*BADORPT:  ; /*ERREPT  UNSUCCESSFUL OPEN                             */ 00285900
*MSGNO = 44;                                                            00286000
BADORPT  MVC   MSGNO(4),KF44                                            00286100
*CALL ERRMSG;                                                           00286200
         BAL   R14,ERRMSG                                               00286300
*BADNEWS = TWELVE;                                                      00286400
         MVC   BADNEWS(4),KF12                                          00286500
*GOTO GOBACK;                                                           00286600
         B     GOBACK                                                   00286700
*                                                                       00286800
*BADOTOUR: ; /*TOURIST UNSUCCESSFUL OPEN                             */ 00286900
*R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2                   */ 00287000
BADOTOUR L     R12,SAVER2                                               00287100
         LR    R2,R12                                                   00287200
* CALL IFCMSG(205,ADDR(PRNTBF));                                        00287300
         LA    R14,PRNTBF                                               00287400
         ST    R14,@AFTEMPS+16                                          00287500
         L     R15,ADIFCMSG(,R12)                                       00287600
         LA    R1,@AL01196                                              00287700
         BALR  R14,R15                                                  00287800
*BADNEWS = TWELVE;                                                      00287900
         MVC   BADNEWS(4),KF12                                          00288000
*GOTO GOBACK;                                                           00288100
         B     GOBACK                                                   00288200
*                                                                       00288300
*HISTBAD:  ; /*ERROR EXIT WRITING TO ACCDEV                          */ 00288400
*                                   /* GET OWN SAVE AREA             */ 00288500
HISTBAD  SYNADAF  ACSMETH=QSAM                                          00288600
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00288700
         ST    R14,SAVER14                                              00288800
*MSGNO = 6;                                                             00288900
         MVC   MSGNO(4),KF6                                             00289000
*CALL ERRMSG;                                                           00289100
         BAL   R14,ERRMSG                                               00289200
*BADNEWS = TWELVE;                                                      00289300
         MVC   BADNEWS(4),KF12                                          00289400
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00289500
         B     GOOUT                                                    00289600
*                                                                       00289700
*HISTEND:  ; /*EOF EXIT FOR READING ACCIN                            */ 00289800
*CALL CLOSHIST;                                                         00289900
HISTEND  BAL   R14,CLOSHIST                                             00290000
*IF SERLO = ON THEN /*IF SERLOG IS ALSO ON WE MUST READ IT ALSO*/       00290100
         TM    IOSWTCH+1,SERLO                                          00290200
         BNO   @RF01208                                                 00290300
* DO;                                                                   00290400
*  CALL READSER; /*WILL START READING SERLOG SEQUENTIALLY THERE*/       00290500
         BAL   R14,READSER                                              00290600
*  GOTO GOBACK;   /*RETURN TO CALLER                                 */ 00290700
         B     GOBACK                                                   00290800
*                                                                       00290900
* END;                                                                  00291000
*ELSE                                                                   00291100
*  DO;                                                                  00291200
*    BADNEWS = FOUR; /*INDICATE EOF ON SEQUENTIAL DATA                  00291300
@RF01208 MVC   BADNEWS(4),KF4                                           00291400
*    GOTO GOBACK;                                                       00291500
         B     GOBACK                                                   00291600
*                                                                       00291700
*  END;                                                                 00291800
*HISTINBD:  ; /*ERROR EXIT FOR READING ACCIN                         */ 00291900
*                                   /* GET OWN SAVE AREA             */ 00292000
HISTINBD SYNADAF  ACSMETH=QSAM                                          00292100
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00292200
         ST    R14,SAVER14                                              00292300
*BADNEWS = TWELVE;                                                      00292400
         MVC   BADNEWS(4),KF12                                          00292500
*MSGNO =7;                                                              00292600
         MVC   MSGNO(4),KF7                                             00292700
*CALL ERRMSG;                                                           00292800
         BAL   R14,ERRMSG                                               00292900
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00293000
         B     GOOUT                                                    00293100
*                                                                       00293200
*BADBRSEQ: ; /*CANT OPEN DIRECTWK FOR READ EXIT                      */ 00293300
*MSGNO = 30;                                                            00293400
BADBRSEQ MVC   MSGNO(4),KF30                                            00293500
*CALL ERRMSG;                                                           00293600
         BAL   R14,ERRMSG                                               00293700
*BADNEWS = TWELVE;                                                      00293800
         MVC   BADNEWS(4),KF12                                          00293900
*GOTO GOBACK;                                                           00294000
         B     GOBACK                                                   00294100
*                                                                       00294200
*BADOPENA: ; /*SERLOG OPEN FAILED EXIT                               */ 00294300
*                                    /*STEP DEQUEUE                  */ 00294400
BADOPENA DEQ   (RESID,LOGRECA,11,STEP)                                  00294500
*MSGNO = 13;                                                            00294600
         MVC   MSGNO(4),KF13                                            00294700
*CALL ERRMSG;                                                           00294800
         BAL   R14,ERRMSG                                               00294900
*BADNEWS = TWELVE;                                                      00295000
         MVC   BADNEWS(4),KF12                                          00295100
*GOTO GOBACK;                                                           00295200
         B     GOBACK                                                   00295300
*                                                                       00295400
*BADBWSEQ:  ; /*ERROR EXIT IF CANT OPEN DIRECTWK FOR WRITING         */ 00295500
*BADNEWS = TWELVE;                                                      00295600
BADBWSEQ MVC   BADNEWS(4),KF12                                          00295700
*MSGNO = 27;                                                            00295800
         MVC   MSGNO(4),KF27                                            00295900
*CALL ERRMSG;                                                           00296000
         BAL   R14,ERRMSG                                               00296100
*GOTO GOBACK;                                                           00296200
         B     GOBACK                                                   00296300
*                                                                       00296400
*BDIST: ; /*IMPROPER RECORD FORMAT EXIT FOR ACCIN                    */ 00296500
*BADNEWS = TWELVE;                                                      00296600
BDIST    MVC   BADNEWS(4),KF12                                          00296700
*MSGNO = 10;                                                            00296800
         MVC   MSGNO(4),KF10                                            00296900
*CALL ERRMSG;                                                           00297000
         BAL   R14,ERRMSG                                               00297100
*GOTO GOBACK;                                                           00297200
         B     GOBACK                                                   00297300
*                                                                       00297400
*BDOIHST: ;  /*CANT OPEN ACCIN EXIT                                  */ 00297500
*MSGNO = 9;                                                             00297600
BDOIHST  MVC   MSGNO(4),KF9                                             00297700
*CALL ERRMSG;                                                           00297800
         BAL   R14,ERRMSG                                               00297900
*BADNEWS = TWELVE;                                                      00298000
         MVC   BADNEWS(4),KF12                                          00298100
*GOTO GOBACK;                                                           00298200
         B     GOBACK                                                   00298300
*                                                                       00298400
*BADRDIR: ; /*SYNAD FOR READING DIRECTWK D.S. SEQUENTIALLY WITH POINT*/ 00298500
*                                   /* GET OWN SAVE AREA             */ 00298600
BADRDIR  SYNADAF  ACSMETH=BSAM                                          00298700
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00298800
         ST    R14,SAVER14                                              00298900
*MSGNO = 42;                                                            00299000
         MVC   MSGNO(4),KF42                                            00299100
*CALL ERRMSG;                                                           00299200
         BAL   R14,ERRMSG                                               00299300
*BADNEWS = TWELVE;                                                      00299400
         MVC   BADNEWS(4),KF12                                          00299500
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00299600
         B     GOOUT                                                    00299700
*                                                                       00299800
*BADOOHST: ; /*CANT OPEN ACCDEV EXIT                                 */ 00299900
*MSGNO = 3;                                                             00300000
BADOOHST MVC   MSGNO(4),KF3                                             00300100
*CALL ERRMSG;                                                           00300200
         BAL   R14,ERRMSG                                               00300300
*BADNEWS = TWELVE;                                                      00300400
         MVC   BADNEWS(4),KF12                                          00300500
*GOTO GOBACK;                                                           00300600
         B     GOBACK                                                   00300700
*                                                                       00300800
*SYSOBAD: ;                                                             00300900
*BADNEWS = TWELVE;                                                      00301000
SYSOBAD  MVC   BADNEWS(4),KF12                                          00301100
*GOTO GOBACK;                                                           00301200
         B     GOBACK                                                   00301300
*                                                                       00301400
*SYSBAD: ;                                                              00301500
*                                   /* GET OWN SAVE AREA             */ 00301600
SYSBAD   SYNADAF  ACSMETH=QSAM                                          00301700
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00301800
         ST    R14,SAVER14                                              00301900
*MSGNO = 36; /*SYSIN READ ERROR                                      */ 00302000
         MVC   MSGNO(4),KF36                                            00302100
*CALL ERRMSG;                                                           00302200
         BAL   R14,ERRMSG                                               00302300
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00302400
         B     GOOUT                                                    00302500
*                                                                       00302600
*OVERFLOW: ;                                                            00302700
*MSGNO = 37;                                                            00302800
OVERFLOW MVC   MSGNO(4),KF37                                            00302900
*CALL ERRMSG;                                                           00303000
         BAL   R14,ERRMSG                                               00303100
*MSGNO = 38;                                                            00303200
         MVC   MSGNO(4),KF38                                            00303300
*CALL ERRMSG;                                                           00303400
         BAL   R14,ERRMSG                                               00303500
*PRINTADR = ADDR(PRNTBF);                                               00303600
         LA    R12,PRNTBF                                               00303700
         L     R1,SAVER2                                                00303800
         ST    R12,PRINTADR(,R1)                                        00303900
*PRINTDS = 'F1'X;                                                       00304000
         MVI   PRINTDS(R12),C'1'                                        00304100
*PRINTCC = '09'X;                                                       00304200
         MVI   PRINTCC(R12),X'09'                                       00304300
*NUMBER = 40; /*NUMBER OF BYTES OF LAST RECORD WRITTEN TO BE PRINTED*/  00304400
         MVC   NUMBER(4),KF40                                           00304500
*NEXTBY = R7 + 4; /*ADDRESS OF RECORD PAST RECORD LENGTH             */ 00304600
         LA    R6,4                                                     00304700
         ALR   R6,R7                                                    00304800
         ST    R6,NEXTBY                                                00304900
*ADDNXT = PRINTADR + 2; /*PUT PRINTABLE NUMBER HERE IN PRINT BUFFER*/   00305000
         AL    R12,KF2                                                  00305100
         ST    R12,ADDNXT                                               00305200
*CALL CONVERT; /*CONVERT HEXIDECIMAL RECORD HEADER TO PRINTABLE*/       00305300
         BAL   R14,CONVERT                                              00305400
*CALL PRINTIT; /*WRITE RECORD LAST PROCESSED TO THE TOURIST DATA SET*/  00305500
         BAL   R14,PRINTIT                                              00305600
*BADNEWS = 8;                                                           00305700
         MVC   BADNEWS(4),KF8                                           00305800
*GOTO GOBACK;                                                           00305900
         B     GOBACK                                                   00306000
*                                                                       00306100
*SYSEOF: ;                                                              00306200
*BADNEWS = FOUR;                                                        00306300
SYSEOF   MVC   BADNEWS(4),KF4                                           00306400
*GOTO GOBACK;                                                           00306500
         B     GOBACK                                                   00306600
*                                                                       00306700
*PRINTBAD: ;                                                            00306800
*                                   /* GET OWN SAVE AREA             */ 00306900
PRINTBAD SYNADAF  ACSMETH=QSAM                                          00307000
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00307100
         ST    R14,SAVER14                                              00307200
*MSGNO = 18;                                                            00307300
         MVC   MSGNO(4),KF18                                            00307400
*CALL ERRMSG;                                                           00307500
         BAL   R14,ERRMSG                                               00307600
*BADNEWS = TWELVE;                                                      00307700
         MVC   BADNEWS(4),KF12                                          00307800
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00307900
         B     GOOUT                                                    00308000
*                                                                       00308100
*TOURBAD: ;                                                             00308200
*                                   /* GET OWN SAVE AREA             */ 00308300
TOURBAD  SYNADAF  ACSMETH=QSAM                                          00308400
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00308500
         ST    R14,SAVER14                                              00308600
*BADNEWS = TWELVE;                                                      00308700
         MVC   BADNEWS(4),KF12                                          00308800
*R2 = SAVER2;    /*INSURE COMTABLE ADDRESSED BY R2                   */ 00308900
         L     R12,SAVER2                                               00309000
         LR    R2,R12                                                   00309100
*CALL IFCMSG(202,ADDR(PRNTBF));                                         00309200
         LA    R14,PRNTBF                                               00309300
         ST    R14,@AFTEMPS+16                                          00309400
         L     R15,ADIFCMSG(,R12)                                       00309500
         LA    R1,@AL01302                                              00309600
         BALR  R14,R15                                                  00309700
* GOTO GOOUT;                       /* RETURN TO SUPERVISOR          */ 00309800
         B     GOOUT                                                    00309900
*                                                                       00310000
*WRITEERR:;                                                             00310100
*                                   /* GET OWN SAVE AREA             */ 00310200
WRITEERR SYNADAF  ACSMETH=BSAM                                          00310300
* SAVER14 = R14;                    /* SAVE RETURN ADDRESS           */ 00310400
         ST    R14,SAVER14                                              00310500
* GOOUT:;                           /* RETURN TO SUPERVISOR          */ 00310600
*                                   /* USE R15 FOR RET CODE          */ 00310700
*  R15 = BADNEWS;                   /* GET RETURN CODE               */ 00310800
GOOUT    L     R15,BADNEWS                                              00310900
*  IF  R15 > FOUR THEN              /* MORE THAN ONE REQ             */ 00311000
         C     R15,KF4                                                  00311100
         BNH   @RF01310                                                 00311200
*   ERRORENT = ERRORENT + 1;        /* CAN NOT DO                    */ 00311300
         LA    R12,1                                                    00311400
         AL    R12,ERRORENT                                             00311500
         ST    R12,ERRORENT                                             00311600
*  R14 = SAVER14;                   /* RESTORE SPVR RET ADR          */ 00311700
@RF01310 L     R14,SAVER14                                              00311800
*                                   /* RSTR SPVR REGS                */ 00311900
         SYNADRLS                                                       00312000
* RETURN;                           /* RETURN TO SUPERVISOR          */ 00312100
         B     @EL00001                                                 00312200
*                                                                       00312300
*        DCBS                                                           00312400
*                                                                       00312500
         PRINT NOGEN                                                    00312600
*                                                                       00312700
OUTHIST  DCB   MACRF=(PM),SYNAD=HISTBAD,LRECL=2000,                    C00312800
               DSORG=PS,DDNAME=ACCDEV,BUFNO=2                           00312900
*                                                                       00313000
INHIST   DCB   MACRF=(GM),DSORG=PS,DDNAME=ACCIN,LRECL=2000,            C00313100
               EODAD=HISTEND,SYNAD=HISTINBD,BUFNO=2                     00313200
*                                                                       00313300
LOGREC   DCB   MACRF=(E),IOBAD=XIOB,DSORG=DA,DEVD=DA,RECFM=U,          C00313400
               DDNAME=SERLOG                                            00313500
*                                                                       00313600
*        DIRECTWK IS READ WITH THE FIRST DCB USING NOTE LOGIC           00313700
*        AND READ WITH THE SECOND DCB USING POINT LOGIC.                00313800
*        A CLOSE TO THE FIRST DCB MUST BE ISSUED BEFORE USING THE       00313900
*        SECOND                                                         00314000
*                                                                       00314100
WORKWDCB DCB   DDNAME=DIRECTWK,DEVD=DA,DSORG=PS,BLKSIZE=2000,          C00314200
               MACRF=(WP),RECFM=V,SYNAD=WRITEERR                        00314300
*                                                                       00314400
WORKRDCB DCB   DDNAME=DIRECTWK,DEVD=DA,DSORG=PS,BLKSIZE=2000,          C00314500
               MACRF=(RP),RECFM=V,SYNAD=BADRDIR                         00314600
*                                                                       00314700
SYSDCB   DCB   DDNAME=SYSIN,DEVD=DA,DSORG=PS,EODAD=SYSEOF,             C00314800
               LRECL=80,MACRF=GM,SYNAD=SYSBAD,EXLST=BLKEXIT             00314900
*                                                                       00315000
*        NOTE THAT EREPPT COMMAND CHARACTERS HAVE FLUCTUATED            00315100
*        BETWEEN ANSI AND MACHINE CHARACTERS                            00315200
*                                                                       00315300
EREPTDCB DCB   MACRF=(PM),SYNAD=PRINTBAD,LRECL=133,DDNAME=EREPPT,      C00315400
               DEVD=DA,DSORG=PS,RECFM=FBM,EXLST=BLKEXIT                 00315500
*                                                                       00315600
TOURDCB  DCB   MACRF=(PM),SYNAD=TOURBAD,LRECL=133,DDNAME=TOURIST,      C00315700
               DEVD=DA,DSORG=PS,RECFM=FBM,EXLST=BLKEXIT                 00315800
*                                                                       00315900
         PRINT GEN                                                      00316000
*                                                                       00316100
BLKEXIT  DC    0F'0'                      ALIGN ON A FULLWORD           00316200
         DC    AL1(128+5),AL3(ADDBLKSZ)   DCB OPEN EXIT                 00316300
*                                                                       00316400
*        DCB OPEN EXIT                                                  00316500
*                                                                       00316600
         DROP  R12                                                      00316700
         USING IHADCB,R1                                                00316800
ADDBLKSZ OC    DCBBLKSI,DCBBLKSI      BLKSIZE SPECIFIED ?               00316900
         BNZR  R14                    YES, RETURN                       00317000
         MVC   DCBBLKSI,DCBLRECL      DEFAULT BLKSIZE = LRECL ?         00317100
         BR    R14                    RETURN                            00317200
         DROP  R1                                                       00317300
*                                                                       00317400
*/********************************************************************/ 00317500
*        END  /*CLOSING END TO EXTERNAL PROC IFCIOHND                */ 00317600
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM      */ 00317700
*/*%INCLUDE SYSLIB  (COMTABLE)                                       */ 00317800
*;                                                                      00317900
KH5      DC    H'5'                                                     00318000
KH20     DC    H'20'                                                    00318100
KH1944   DC    H'1944'                                                  00318200
KHM1     DC    H'-1'                                                    00318300
*                                                                       00318400
*        PARAMETER LISTS                                                00318500
*                                                                       00318600
@AL00337 DC    A(KF201)                LIST WITH   2 ARGUMENT(S)        00318700
         DC    A(@AFTEMPS)                                              00318800
@AL00687 DC    A(MSGNO)                LIST WITH   2 ARGUMENT(S)        00318900
         DC    A(@AFTEMPS+4)                                            00319000
@AL00717 DC    A(KF202)                LIST WITH   2 ARGUMENT(S)        00319100
         DC    A(@AFTEMPS+4)                                            00319200
@AL00761 DC    A(KF203)                LIST WITH   2 ARGUMENT(S)        00319300
         DC    A(@AFTEMPS+8)                                            00319400
@AL01078 DC    A(KF204)                LIST WITH   2 ARGUMENT(S)        00319500
         DC    A(@AFTEMPS+12)                                           00319600
@AL01196 DC    A(KF205)                LIST WITH   2 ARGUMENT(S)        00319700
         DC    A(@AFTEMPS+16)                                           00319800
@AL01302 DC    A(KF202)                LIST WITH   2 ARGUMENT(S)        00319900
         DC    A(@AFTEMPS+16)                                           00320000
*                                                                       00320100
*        SAVEAREAS                                                      00320200
*                                                                       00320300
@SA00001 DC    18F'0'                                                   00320400
@SA00023 DC    14F'0'                                                   00320500
@SA00021 DC    14F'0'                                                   00320600
@SA00034 DC    15F'0'                                                   00320700
@SA00019 DC    15F'0'                                                   00320800
@SA00015 DC    15F'0'                                                   00320900
@SA00009 DC    15F'0'                                                   00321000
@SA00010 DC    15F'0'                                                   00321100
@SA00018 DC    14F'0'                                                   00321200
@SA00014 DC    15F'0'                                                   00321300
@SA00002 DC    15F'0'                                                   00321400
@SA00003 DC    14F'0'                                                   00321500
@SA00007 DC    15F'0'                                                   00321600
@SA00008 DC    15F'0'                                                   00321700
@SA00004 DC    14F'0'                                                   00321800
@SA00005 DC    15F'0'                                                   00321900
@SA00006 DC    15F'0'                                                   00322000
@SA00029 DC    15F'0'                                                   00322100
@SA00031 DC    15F'0'                                                   00322200
@SA00026 DC    15F'0'                                                   00322300
@SA00033 DC    15F'0'                                                   00322400
@SA00016 DC    15F'0'                                                   00322500
@SA00025 DC    15F'0'                                                   00322600
@SA00032 DC    15F'0'                                                   00322700
@SA00027 DC    15F'0'                                                   00322800
@SA00028 DC    15F'0'                                                   00322900
@SA00017 DC    15F'0'                                                   00323000
@SA00022 DC    15F'0'                                                   00323100
@SA00011 DC    15F'0'                                                   00323200
@SA00030 DC    15F'0'                                                   00323300
@SA00013 DC    15F'0'                                                   00323400
@SA00012 DC    15F'0'                                                   00323500
@SA00020 DC    14F'0'                                                   00323600
@SA00024 DC    14F'0'                                                   00323700
@AFTEMPS DC    5F'0'                                                    00323800
*                                                                       00323900
KF1      DC    F'1'                                                     00324000
KH1      EQU   KF1+2                                                    00324100
KF2      DC    F'2'                                                     00324200
KH2      EQU   KF2+2                                                    00324300
KF3      DC    F'3'                                                     00324400
KH3      EQU   KF3+2                                                    00324500
KF4      DC    F'4'                                                     00324600
KH4      EQU   KF4+2                                                    00324700
KF6      DC    F'6'                                                     00324800
KH6      EQU   KF6+2                                                    00324900
KF7      DC    F'7'                                                     00325000
KH7      EQU   KF7+2                                                    00325100
KF8      DC    F'8'                                                     00325200
KF9      DC    F'9'                                                     00325300
KH9      EQU   KF9+2                                                    00325400
KF10     DC    F'10'                                                    00325500
KF11     DC    F'11'                                                    00325600
KF12     DC    F'12'                                                    00325700
KF13     DC    F'13'                                                    00325800
KF14     DC    F'14'                                                    00325900
KF15     DC    F'15'                                                    00326000
KF16     DC    F'16'                                                    00326100
KF17     DC    F'17'                                                    00326200
KF18     DC    F'18'                                                    00326300
KF19     DC    F'19'                                                    00326400
KF21     DC    F'21'                                                    00326500
KF22     DC    F'22'                                                    00326600
KF23     DC    F'23'                                                    00326700
KF25     DC    F'25'                                                    00326800
KF27     DC    F'27'                                                    00326900
KF28     DC    F'28'                                                    00327000
KF29     DC    F'29'                                                    00327100
KF30     DC    F'30'                                                    00327200
KF31     DC    F'31'                                                    00327300
KF32     DC    F'32'                                                    00327400
KF33     DC    F'33'                                                    00327500
KF34     DC    F'34'                                                    00327600
KF35     DC    F'35'                                                    00327700
KF36     DC    F'36'                                                    00327800
KF37     DC    F'37'                                                    00327900
KF38     DC    F'38'                                                    00328000
KF39     DC    F'39'                                                    00328100
KF40     DC    F'40'                                                    00328200
KH40     EQU   KF40+2                                                   00328300
KF41     DC    F'41'                                                    00328400
KF42     DC    F'42'                                                    00328500
KF44     DC    F'44'                                                    00328600
KF45     DC    F'45'                                                    00328700
KF46     DC    F'46'                                                    00328800
KF47     DC    F'47'                                                    00328900
KF48     DC    F'48'                                                    00329000
KF49     DC    F'49'                                                    00329100
KF50     DC    F'50'                                                    00329200
KF51     DC    F'51'                                                    00329300
KH51     EQU   KF51+2                                                   00329400
KF52     DC    F'52'                                                    00329500
KF53     DC    F'53'                                                    00329600
KF54     DC    F'54'                                                    00329700
KF55     DC    F'55'                                                    00329800
KF56     DC    F'56'                                                    00329900
KF57     DC    F'57'                                                    00330000
KF58     DC    F'58'                                                    00330100
KF59     DC    F'59'                                                    00330200
KF60     DC    F'60'                                                    00330300
KF61     DC    F'61'                                                    00330400
KF62     DC    F'62'                                                    00330500
KF63     DC    F'63'                                                    00330600
KF64     DC    F'64'                                                    00330700
KF65     DC    F'65'                                                    00330800
KF66     DC    F'66'                                                    00330900
KF67     DC    F'67'                                                    00331000
KF68     DC    F'68'                                                    00331100
KF69     DC    F'69'                                                    00331200
KF70     DC    F'70'                                                    00331300
KF82     DC    F'82'                                                    00331400
KF201    DC    F'201'                                                   00331500
KF202    DC    F'202'                                                   00331600
KF203    DC    F'203'                                                   00331700
KF204    DC    F'204'                                                   00331800
KF205    DC    F'205'                                                   00331900
KX00FFFF DC    XL4'0000FFFF'                                            00332000
         DC    0D'0'                                                    00332100
SAVER2   DC    A(0)                    -> COM TABLE                     00332200
PTRSPOT  DC    A(0)                    -> HALF WORD FUNCTION REQUEST    00332300
SAVEPTR  DC    A(0)                                                     00332400
FRSPTR   DC    A(0)                                                     00332500
RUNPTR   DC    A(0)                                                     00332600
SAVER14  DC    A(0)                                                     00332700
A        DC    F'0'                                                     00332800
K        DC    F'0'                                                     00332900
J        DC    F'0'                                                     00333000
GETHIS   DC    F'0'                                                     00333100
R7SAVE   DC    F'0'                                                     00333200
TIMEBASE DC    A(0)                                                     00333300
NEXTBY   DC    F'0'                                                     00333400
ADDNXT   DC    F'0'                                                     00333500
NUMBER   DC    F'0'                                                     00333600
MSGNO    DC    F'0'                                                     00333700
BADNEWS  DC    F'0'                                                     00333800
PASSNUM  DC    F'0'                                                     00333900
*                                                                       00334000
XWRECB   DC    F'0'                                                     00334100
         ORG   XWRECB                                                   00334200
POST     DS    CL1                                                      00334300
         DS    CL3                                                      00334400
         ORG   XWRECB+4                                                 00334500
*                                                                       00334600
MILLION  DC    F'1000000'                                               00334700
TWENTY4  DC    F'24'                                                    00334800
SIXTY    DC    F'60'                                                    00334900
CONSTG   DC    XL4'30040'                                               00335000
CONSTH   DC    XL4'24AE062'                                             00335100
SIXTYMIL DC    F'60000000'                                              00335200
THCONV   DC    F'10000'                                                 00335300
TIMDIV   DC    F'0'                                                     00335400
SAVELNGT DC    H'0'                                                     00335500
LINECTT  DC    H'0'                                                     00335600
LINEMAX  DC    H'0'                                                     00335700
SDRK     DC    H'0'                                                     00335800
         DC    H'0'                                                     00335900
SAVELENT DC    H'0'                                                     00336000
*                                                                       00336100
WORKD    DC    D'0'                                                     00336200
KC1B     DC    C'1 '                                                    00336300
KXNULL   DC    X'00000000'                                              00336400
KX07     DC    X'00000007'                                              00336500
@CB01094 DC    X'0000000F'                                              00336600
@CB00262 DC    X'FFFF'                                                  00336700
@CB00983 DC    X'000F'                                                  00336800
CARDSPOT DC    CL80' '                 CONTROL STMT READIN AREA         00336900
         DC    0F'0'                                                    00337000
MAXTRK   DC    X'00000000'                                              00337100
LOOKATIT DC    CL40'XXXXXXXXOPERATION COUNTS IFCEREP1XXXXXXX'           00337200
*                                                                       00337300
CONTPTR  DS    CL44                                                     00337400
         ORG   CONTPTR                                                  00337500
ERRORENT DC    F'0'                                                     00337600
RSEQCNT  DC    F'0'                                                     00337700
RSERDCNT DC    F'0'                                                     00337800
CONVTCT  DC    F'0'                                                     00337900
NUMRECSV DC    F'0'                                                     00338000
SYSRDCT  DC    F'0'                                                     00338100
RITEWKCT DC    F'0'                                                     00338200
READWKCT DC    F'0'                                                     00338300
ACIRDCT  DC    F'0'                                                     00338400
DEVCT    DC    F'0'                                                     00338500
RFRCOUNT DC    F'0'                                                     00338600
         ORG   CONTPTR+44                                               00338700
*                                                                       00338800
TTRSAVE  DC    X'00000000'                                              00338900
CCSAVE   DC    CL1' '                                                   00339000
BLANKS   DC    CL134' '                                                 00339100
PACKPAS  DC    CL4'    '                                                00339200
CHARPAS  DC    CL8'        '                                            00339300
CHARS    DC    CL16'0123456789ABCDEF'                                   00339400
         DC    0D'0'                                                    00339500
PRNTBF   DC    CL134' '                                                 00339600
         DC    XL2'00'                                                  00339700
*                                                                       00339800
BUF1     DS    CL2004                                                   00339900
         ORG   BUF1                                                     00340000
BUF1RDW  DS    CL4                                                      00340100
         ORG   BUF1RDW                                                  00340200
BUF1RDWL DC    H'0'                                                     00340300
BUF1L    DC    H'0'                                                     00340400
         ORG   BUF1+4                                                   00340500
BUF1DTA  DS    CL2000                                                   00340600
         ORG   BUF1+2004                                                00340700
*                                                                       00340800
*        SWITCHES                                                       00340900
*                                                                       00341000
IOSWTCH  DC    X'000000'                                                00341100
*                                      +00                              00341200
HISTOPN  EQU   X'80'                                                    00341300
SECGET   EQU   X'40'                                                    00341400
FLIPFLOP EQU   X'20'                                                    00341500
HISTYES  EQU   X'10'                                                    00341600
DBLYES   EQU   X'08'                                                    00341700
LASTLOOP EQU   X'04'                                                    00341800
TOURERR  EQU   X'02'                                                    00341900
INHISO   EQU   X'01'                                                    00342000
*                                      +01                              00342100
SERLO    EQU   X'80'                                                    00342200
TWOTRY   EQU   X'40'                                                    00342300
INDIR    EQU   X'20'                                                    00342400
NONBLANK EQU   X'10'                                                    00342500
CLOSE2   EQU   X'08'                                                    00342600
SYSOPN   EQU   X'04'                                                    00342700
TOUROPN  EQU   X'02'                                                    00342800
ERPTOPN  EQU   X'01'                                                    00342900
*                                      +02                              00343000
READFR   EQU   X'80'                                                    00343100
SCRATCH  EQU   X'40'                                                    00343200
*                                                                       00343300
         DC    X'00'                                                    00343400
*                                                                       00343500
HDRBUFF  DC    XL64'00'                                                 00343600
         ORG   HDRBUFF                                                  00343700
HDRSEEK  DS    0CL8                                                     00343800
HDRM     DC    X'00'                                                    00343900
HDRBB    DC    X'0000'                                                  00344000
HDRCC    DC    X'0000'                                                  00344100
HDRHH    DC    X'0000'                                                  00344200
HDRR     DC    X'00'                                                    00344300
         ORG   HDRBUFF+8                                                00344400
*                                                                       00344500
*        LOGREC HEADER RECORD                                           00344600
*                                                                       00344700
HEADER   DC    XL40'00'                                                 00344800
         ORG   HEADER                                                   00344900
HDRKEY   DS    CL2                +00 RECORD ID X'FFFF'                 00345000
HDRSCC   DS    CL2                +02 START OF LOGREC EXTENT CCHH       00345100
HDRSHH   DS    CL2                                                      00345200
HDRECC   DS    CL2                +06 END OF LOGREC EXTENT CCHH         00345300
HDREHH   DS    CL2                                                      00345400
HDRCOUNT DS    CL1                +10 COUNT OF LOGREC FULL MSG          00345500
HDRREA   DS    0CL7               +11 RECORD ENTRY AREA (BBCCHHR)       00345600
HDR1BB   DS    CL2                    START OF LOGREC RECORDS           00345700
HDR1CC   DS    CL2                                                      00345800
HDR1HH   DS    CL2                                                      00345900
HDR1R    DS    CL1                                                      00346000
HDRTBAL  DS    CL2                +18 TRACK BALANCE                     00346100
HDRTRKCP DS    CL2                +20 NO OF BYTES PER TRACK             00346200
HDRLASTR DS    0CL7               +22 LAST RECORD WRITTEN (BBCCHHR)     00346300
HDR2BB   DS    CL2                                                      00346400
HDR2CC   DS    CL2                                                      00346500
HDR2HH   DS    CL2                                                      00346600
HDR2R    DS    CL1                                                      00346700
HDRTCYL  DS    CL2                +29 TRKS PER CYL(HIGHEST TRK ON CYL)  00346800
HDREXSZE DS    CL2                +31 EARLY WARNING TRACK CAPACITY      00346900
*                                     SET AT 90% OF LAST TRACK          00347000
HDRDEVT  DS    CL1                +33 UNIT DEVICE TYPE (LOW NIBBLE)     00347100
HDREWMT  DS    CL4                +34 EARLY WARNING TRK (CCHH)          00347200
*                                     SET AT 90% OF ALL TRACKS          00347300
HDREWSW  DS    XL1                                                      00347400
HDRWNGBT EQU   X'80'                                                    00347500
FRAMES   EQU   X'20'                                                    00347600
*                                                                       00347700
HDRSAFE  DS    CL1                +39 CHECK BYTE X'FF'                  00347800
*                                                                       00347900
PREVSEEK DS    0CL8                                                     00348000
         ORG   PREVSEEK                                                 00348100
PREVM    DC    X'FF'                                                    00348200
PREVBB   DS    CL2                                                      00348300
PREVCC   DS    CL2                                                      00348400
PREVHH   DS    CL2                                                      00348500
PREVR    DS    CL1                                                      00348600
         ORG   HDRBUFF+64                                               00348700
*                                                                       00348800
LOGRECA  DC    C'SYS1.LOGREC'                                           00348900
LOGRECB  DC    CL8'RECORDER'                                            00349000
RESID    DC    CL8'SERLOG'                                              00349100
RESIDII  DC    CL8'SYSZLOGR'                                            00349200
*                                                                       00349300
ZEROA    DC    XL24'00'                                                 00349400
*                                                                       00349500
SEEKSAVE DC    XL8'00'                   SAVE SEEK ADDRESS              00349600
*                                                                       00349700
*        IOB                                                            00349800
*                                                                       00349900
XIOB     DC    5D'00'                                                   00350000
         ORG   XIOB-16                                                  00350100
*                                                                       00350200
         PRINT NOGEN                                                    00350300
*                                                                       00350400
         IEZIOB DSECT=NO                  MAP IOB                       00350500
*                                                                       00350600
         PRINT GEN                                                      00350700
*                                                                       00350800
         ORG   IOBFLAG1                                                 00350900
         DC    AL1(IOBCMDCH+IOBUNREL)     INIT IOBFLAG1                 00351000
         ORG   IOBECBPT                                                 00351100
         DC    AL4(XWRECB)                INIT IOBECBPT                 00351200
         ORG   IOBSTART                                                 00351300
         DC    AL4(SEQ)                   INIT IOBSTART                 00351400
         ORG   XIOB+40                                                  00351500
*                                                                       00351600
*        CCWS                                                           00351700
*                                                                       00351800
SEQ      DS    CL24                                                     00351900
         ORG   SEQ                                                      00352000
SID1     DS    CL8                                                      00352100
         ORG   SID1                                                     00352200
@NM00018 DC    X'31'                                                    00352300
AREAADR  DS    AL3                                                      00352400
@NM00019 DC    X'4000'                                                  00352500
@NM00020 DC    H'5'                                                     00352600
         ORG   SEQ+8                                                    00352700
TIC1     DS    CL8                                                      00352800
         ORG   TIC1                                                     00352900
@NM00021 DC    X'08'                                                    00353000
@NM00022 DC    AL3(SID1)                                                00353100
@NM00023 DC    X'4000'                                                  00353200
@NM00024 DC    H'1'                                                     00353300
         ORG   SEQ+16                                                   00353400
CKD1     DS    CL8                                                      00353500
         ORG   CKD1                                                     00353600
CKDCOM   DS    CL1                                                      00353700
CKDAREA  DS    AL3                                                      00353800
CKDCHAIN DC    X'20'                                                    00353900
@NM00025 DC    X'00'                                                    00354000
CKDCOUNT DS    FL2                                                      00354100
         ORG   SEQ+24                                                   00354200
*                                                                       00354300
SWITCH   DC    X'00'                                                    00354400
         ORG   SWITCH                                                   00354500
SBT0     DS    BL1                                                      00354600
SBT2     EQU   SWITCH+0                                                 00354700
SBT3     EQU   SWITCH+0                                                 00354800
SBT4     EQU   SWITCH+0                                                 00354900
SBT7     EQU   SWITCH+0                                                 00355000
         ORG   SWITCH+1                                                 00355100
         DC    XL7'00'                                                  00355200
WORK8    DC    D'0'                                                     00355300
         ORG   WORK8                                                    00355400
WORK     DS    FL4                                                      00355500
WORKB    DS    FL4                                                      00355600
         ORG   WORKB                                                    00355700
WHOURS   DS    CL1                                                      00355800
WMINS    DS    CL1                                                      00355900
WDAYS    DS    CL2                                                      00356000
         ORG   WDAYS                                                    00356100
WSECS    DS    CL1                                                      00356200
WTNTHS   DS    CL1                                                      00356300
         ORG   WORK8+8                                                  00356400
*                                                                       00356500
*DCL 1 ERRLINE(29), /*INTERNAL DEBUG15 MESSAGES                      */ 00356600
*    2 ERRDS CHAR(1) INIT((29)'1'),                                     00356700
*    2 ERRCS CHAR(1) INIT((29)' '),                                     00356800
*    2 PRINTNM CHAR(9) INIT((29)'DEBUG15  '),                           00356900
*    2 PRNTXT CHAR(40)                                                  00357000
*INIT('SERLOG OPEN ATTEMPTED                   ', /*53               */ 00357100
*     'CHECK HEADER FOR MORE RECORDS ATTEMPTED ', /*54               */ 00357200
*     'RESET OF SERLOG HEADER ATTEMPTED        ', /*55               */ 00357300
*     'SERLOG CLOSE ATTEMPTED                  ', /*56               */ 00357400
*     'ZEROALL ATTEMPTED                       ', /*57               */ 00357500
*     'SDR COUNTER DUMP ATTEMPTED              ', /*58               */ 00357600
*     'SYSIN OPEN ATTEMPTED                    ', /*59               */ 00357700
*     'SYSIN CLOSE ATTEMPTED                   ', /*60               */ 00357800
*     'DIRECTWK CLOSE FOR WRITING ATTEMPTED    ', /*61               */ 00357900
*     'DIRECTWK CLOSE FOR READING ATTEMPTED    ', /*62               */ 00358000
*     'DIRECTWK OPEN FOR WRITE ATTEMPTED       ', /*63               */ 00358100
*     'ACCIN OPEN ATTEMPTED                    ', /*64               */ 00358200
*     'ACCIN CLOSE ATTEMPTED                   ', /*65               */ 00358300
*     'ACCDEV CLOSE ATTEMPTED                  ', /*66               */ 00358400
*     'EREPPT OPEN ATTEMPTED                   ', /*67               */ 00358500
*     'EREPPT CLOSE ATTEMPTED                  ', /*68               */ 00358600
*     'ACCDEV OPEN ATTEMPTED                   ', /*69               */ 00358700
*     'FIRST FRAME READ ATTEMPT                ',                       00358800
*     'TIMES IFCIOHND EXITED WITH R.C.12       ', /*70               */ 00358900
*     'SERLOG SEQUENTIAL READ ATTEMPTS         ', /*71               */ 00359000
*     'SERLOG DIRECT READ ATTEMPTS             ', /*72               */ 00359100
*     'TIME CONVERSION NECESSARY NUM. TIMES    ', /*73               */ 00359200
*     'RECORDS SAVED DURING ZEROALL PROCESSING ', /*74               */ 00359300
*     'SYSIN READ ATTEMPTS                     ', /*75               */ 00359400
*     'DIRECTWK WRITE ATTEMPTS                 ', /*76               */ 00359500
*     'DIRECTWK READ ATTEMPTS                  ', /*77               */ 00359600
*     'ACCIN SEQUENTIAL READ ATTEMPTS          ', /*78               */ 00359700
*     'ACCDEV WRITE ATTEMPTS                   ', /*79               */ 00359800
*     'FRAME READ ATTEMPTS OF SYS1.LOGREC      ');/*80               */ 00359900
EDIFILD  DC    XL9'00'                                                  00360000
ERRLINE  DC    CL51' '                                                  00360100
         ORG   ERRLINE                                                  00360200
ERRDS    DC    CL1'1'                                                   00360300
ERRCS    DC    CL1' '                                                   00360400
PRINTNM  DC    CL9'DEBUG15  '                                           00360500
PRNTXT   DC    CL40'SERLOG OPEN ATTEMPTED                   '           00360600
         DC    C'1 '                                                    00360700
         DC    CL9'DEBUG15  '                                           00360800
         DC    CL40'CHECK HEADER FOR MORE RECORDS ATTEMPTED '           00360900
         DC    C'1 '                                                    00361000
         DC    CL9'DEBUG15  '                                           00361100
         DC    CL40'RESET OF SERLOG HEADER ATTEMPTED        '           00361200
         DC    C'1 '                                                    00361300
         DC    CL9'DEBUG15  '                                           00361400
         DC    CL40'SERLOG CLOSE ATTEMPTED                  '           00361500
         DC    C'1 '                                                    00361600
         DC    CL9'DEBUG15  '                                           00361700
         DC    CL40'ZEROALL ATTEMPTED                       '           00361800
         DC    C'1 '                                                    00361900
         DC    CL9'DEBUG15  '                                           00362000
         DC    CL40'SDR COUNTER DUMP ATTEMPTED              '           00362100
         DC    C'1 '                                                    00362200
         DC    CL9'DEBUG15  '                                           00362300
         DC    CL40'SYSIN OPEN ATTEMPTED                    '           00362400
         DC    C'1 '                                                    00362500
         DC    CL9'DEBUG15  '                                           00362600
         DC    CL40'SYSIN CLOSE ATTEMPTED                   '           00362700
         DC    C'1 '                                                    00362800
         DC    CL9'DEBUG15  '                                           00362900
         DC    CL40'DIRECTWK CLOSE FOR WRITING ATTEMPTED    '           00363000
         DC    C'1 '                                                    00363100
         DC    CL9'DEBUG15  '                                           00363200
         DC    CL40'DIRECTWK CLOSE FOR READING ATTEMPTED    '           00363300
         DC    C'1 '                                                    00363400
         DC    CL9'DEBUG15  '                                           00363500
         DC    CL40'DIRECTWK OPEN FOR WRITE ATTEMPTED      '            00363600
         DC    C'1 '                                                    00363700
         DC    CL9'DEBUG15  '                                           00363800
         DC    CL40'ACCIN OPEN ATTEMPTED                    '           00363900
         DC    C'1 '                                                    00364000
         DC    CL9'DEBUG15  '                                           00364100
         DC    CL40'ACCIN CLOSE ATTEMPTED                   '           00364200
         DC    C'1 '                                                    00364300
         DC    CL9'DEBUG15  '                                           00364400
         DC    CL40'ACCDEV CLOSE ATTEMPTED                  '           00364500
         DC    C'1 '                                                    00364600
         DC    CL9'DEBUG15  '                                           00364700
         DC    CL40'EREPPT OPEN ATTEMPTED                   '           00364800
         DC    C'1 '                                                    00364900
         DC    CL9'DEBUG15  '                                           00365000
         DC    CL40'EREPPT CLOSE ATTEMPTED                  '           00365100
         DC    C'1 '                                                    00365200
         DC    CL9'DEBUG15  '                                           00365300
         DC    CL40'ACCDEV OPEN ATTEMPTED                   '           00365400
         DC    C'1 '                                                    00365500
         DC    CL9'DEBUG15  '                                           00365600
         DC    CL40'FIRST FRAME READ ATTEMPT                '           00365700
         DC    C'1 '                                                    00365800
         DC    CL9'DEBUG15  '                                           00365900
         DC    CL40'TIMES IFCIOHND EXITED WITH R.C.12       '           00366000
         DC    C'1 '                                                    00366100
         DC    CL9'DEBUG15  '                                           00366200
         DC    CL40'SERLOG SEQUENTIAL READ ATTEMPTS         '           00366300
         DC    C'1 '                                                    00366400
         DC    CL9'DEBUG15  '                                           00366500
         DC    CL40'SERLOG DIRECT READ ATTEMPTS             '           00366600
         DC    C'1 '                                                    00366700
         DC    CL9'DEBUG15  '                                           00366800
         DC    CL40'TIME CONVERSION NECESSARY NUM. TIMES    '           00366900
         DC    C'1 '                                                    00367000
         DC    CL9'DEBUG15  '                                           00367100
         DC    CL40'RECORDS SAVED DURING ZEROALL PROCESSING '           00367200
         DC    C'1 '                                                    00367300
         DC    CL9'DEBUG15  '                                           00367400
         DC    CL40'SYSIN READ ATTEMPTS                     '           00367500
         DC    C'1 '                                                    00367600
         DC    CL9'DEBUG15  '                                           00367700
         DC    CL40'DIRECTWK WRITE ATTEMPTS                 '           00367800
         DC    C'1 '                                                    00367900
         DC    CL9'DEBUG15  '                                           00368000
         DC    CL40'DIRECTWK READ ATTEMPTS                  '           00368100
         DC    C'1 '                                                    00368200
         DC    CL9'DEBUG15  '                                           00368300
         DC    CL40'ACCIN SEQUENTIAL READ ATTEMPTS          '           00368400
         DC    C'1 '                                                    00368500
         DC    CL9'DEBUG15  '                                           00368600
         DC    CL40'ACCDEV WRITE ATTEMPTS                   '           00368700
         DC    C'1 '                                                    00368800
         DC    CL9'DEBUG15  '                                           00368900
         DC    CL40'FRAME READ ATTEMPTS OF SYS1.LOGREC      '           00369000
         DC    C'1 '                                                    00369100
         DC    CL9'DEBUG15  '                                           00369200
         DC    CL40'DIRECTWK OPEN FOR READING               '           00369300
*                                                                       00369400
*        MACHINE CONTROL CHARACTER TRANSLATE TABLE                      00369500
*                                                                       00369600
MACHCODE DC    X'0901090909090909'                                      00369700
         DC    X'0909090B09090909'                                      00369800
         DC    X'0911091309090909'                                      00369900
         DC    X'0919091B09090909'                                      00370000
         DC    X'0909090909090909'                                      00370100
         DC    X'0909090909090909'                                      00370200
         DC    X'0909090909090909'                                      00370300
         DC    X'0909090909090909'                                      00370400
         DC    X'0909090909090909'                                      00370500
         DC    X'0909090909090909'                                      00370600
         DC    X'0909090909090909'                                      00370700
         DC    X'0909090909090909'                                      00370800
         DC    X'0909090909090909'                                      00370900
         DC    X'0909090909090909'                                      00371000
         DC    X'0909090909090909'                                      00371100
         DC    X'0909090909090909'                                      00371200
         DC    X'0909090909090909'                                      00371300
         DC    X'0989098B09090909'                                      00371400
         DC    X'0991099309090909'                                      00371500
         DC    X'0999099B09090909'                                      00371600
         DC    X'09A109A309090909'                                      00371700
         DC    X'090909AB09090909'                                      00371800
         DC    X'09B109B309090909'                                      00371900
         DC    X'09B909BB09090909'                                      00372000
         DC    X'09C109C309090909'                                      00372100
         DC    X'090909CB09090909'                                      00372200
         DC    X'09D109D309090909'                                      00372300
         DC    X'09D909DB09090909'                                      00372400
         DC    X'09E109E309090909'                                      00372500
         DC    X'0909090909090909'                                      00372600
         DC    X'0909090909090909'                                      00372700
         DC    X'0909090909090909'                                      00372800
*                                                                       00372900
*        LTORG                                                          00373000
*                                                                       00373100
         LTORG                                                          00373200
*                                                                       00373300
         PRINT NOGEN                                                    00373400
*                                                                       00373500
         DCBD  DSORG=PS,DEVD=DA        DCB DSECT                        00373600
*                                                                       00373700
*        IEZDEB                                                         00373800
*                                                                       00373900
         IEZDEB                                                         00374000
*                                                                       00374100
         PRINT GEN                                                      00374200
*                                                                       00374300
IFCIOHND CSECT                                                          00374400
*                                                                       00374500
R0       EQU   00                      EQUATES FOR REGISTERS 0-15       00374600
R1       EQU   01                                                       00374700
R2       EQU   02                                                       00374800
R3       EQU   03                                                       00374900
R4       EQU   04                                                       00375000
R5       EQU   05                                                       00375100
R6       EQU   06                                                       00375200
R7       EQU   07                                                       00375300
R8       EQU   08                                                       00375400
R9       EQU   09                                                       00375500
R10      EQU   10                                                       00375600
R11      EQU   11                                                       00375700
R12      EQU   12                                                       00375800
R13      EQU   13                                                       00375900
R14      EQU   14                                                       00376000
R15      EQU   15                                                       00376100
*                                                                       00376200
PARM     EQU   0                                                        00376300
RECTYPE  EQU   PARM                                                     00376400
SWITCHES EQU   PARM+2                                                   00376500
NOSDR    EQU   SWITCHES                                                 00376600
ZEROALL  EQU   SWITCHES+1                                               00376700
DEBUG    EQU   PARM+4                                                   00376800
DEBUG15  EQU   DEBUG+1                                                  00376900
DEBUGA   EQU   PARM+8                                                   00377000
MISC     EQU   PARM+12                                                  00377100
PRINTES  EQU   MISC                                                     00377200
OPENCLSE EQU   PARM+13                                                  00377300
OPENIO   EQU   OPENCLSE                                                 00377400
OTOURIST EQU   OPENIO                                                   00377500
OEREPPT  EQU   OPENIO                                                   00377600
OSERLOG  EQU   OPENIO                                                   00377700
OACCIN   EQU   OPENIO                                                   00377800
OACCDEV  EQU   OPENIO                                                   00377900
OSYSIN   EQU   OPENIO                                                   00378000
ODRCTWRK EQU   OPENIO                                                   00378100
CLOSEIO  EQU   OPENCLSE+2                                               00378200
CTOURIST EQU   CLOSEIO                                                  00378300
CEREPPT  EQU   CLOSEIO                                                  00378400
CSERLOG  EQU   CLOSEIO                                                  00378500
CACCIN   EQU   CLOSEIO                                                  00378600
CACCDEV  EQU   CLOSEIO                                                  00378700
CSYSIN   EQU   CLOSEIO                                                  00378800
CDRCTWRK EQU   CLOSEIO                                                  00378900
DATENOW  EQU   PARM+17                                                  00379000
STDT     EQU   PARM+23                                                  00379100
STDATE   EQU   STDT                                                     00379200
PSTD     EQU   STDATE+1                                                 00379300
ENDDT    EQU   PARM+31                                                  00379400
ENDDATE  EQU   ENDDT                                                    00379500
PEND     EQU   ENDDATE+1                                                00379600
STTIME1  EQU   PARM+39                                                  00379700
ENDTIME1 EQU   PARM+43                                                  00379800
STTIME2  EQU   PARM+47                                                  00379900
ENDTIME2 EQU   PARM+51                                                  00380000
STRTDATE EQU   PARM+55                                                  00380100
@NM00006 EQU   PARM+72                                                  00380200
@NM00007 EQU   PARM+75                                                  00380300
ERRID    EQU   PARM+92                                                  00380400
IDTIMEA  EQU   ERRID+6                                                  00380500
IDTIME   EQU   IDTIMEA                                                  00380600
CCHHRLNG EQU   PARM+112                                                 00380700
RECCCHHR EQU   CCHHRLNG                                                 00380800
RECLNGTH EQU   CCHHRLNG+6                                               00380900
LINECT   EQU   PARM+120                                                 00381000
LINECTSV EQU   PARM+122                                                 00381100
ADIOHND  EQU   PARM+124                                                 00381200
ADIFCMSG EQU   PARM+128                                                 00381300
ADEDITB  EQU   PARM+132                                                 00381400
EVPTR    EQU   PARM+136                                                 00381500
SORTADR  EQU   PARM+140                                                 00381600
PRINTADR EQU   PARM+148                                                 00381700
CUAPTR   EQU   PARM+152                                                 00381800
DEVPTR   EQU   PARM+156                                                 00381900
MODPTR   EQU   PARM+160                                                 00382000
SERPTR   EQU   PARM+164                                                 00382100
VOLPTR   EQU   PARM+168                                                 00382200
CPUPTR   EQU   PARM+172                                                 00382300
CPCUAPTR EQU   PARM+176                                                 00382400
LIBADPTR EQU   PARM+180                                                 00382500
SYMCDPTR EQU   PARM+184                                                 00382600
SYSUMPTR EQU   PARM+188                                                 00382700
TRENPTR  EQU   PARM+192                                                 00382800
SHAREPTR EQU   PARM+196                                                 00382900
MERDCPTR EQU   PARM+208                                                 00383000
DUMPTABL EQU   PARM+228                                                 00383100
DUMPINFO EQU   DUMPTABL+4                                               00383200
TRENDONE EQU   0                                                        00383300
TREGEN   EQU   TRENDONE+8                                               00383400
TRECCH0  EQU   TREGEN+8                                                 00383500
TREND2   EQU   0                                                        00383600
TRENDKEY EQU   TREND2                                                   00383700
TRENDRT  EQU   TRENDKEY+1                                               00383800
TRENCPUS EQU   TRENDKEY+3                                               00383900
TRENDITM EQU   TRENDKEY+4                                               00384000
EVTABLE  EQU   0                                                        00384100
EVSTOP   EQU   EVTABLE                                                  00384200
EVKEY    EQU   EVTABLE+6                                                00384300
SORTABLE EQU   0                                                        00384400
SORTKEY  EQU   SORTABLE                                                 00384500
SORTCCHR EQU   SORTABLE+5                                               00384600
SYSUMONE EQU   0                                                        00384700
SYSUM2   EQU   0                                                        00384800
SYSUMKEY EQU   SYSUM2                                                   00384900
SYSRT    EQU   SYSUMKEY+1                                               00385000
SYSCPUS  EQU   SYSUMKEY+3                                               00385100
SYSITEM  EQU   SYSUMKEY+4                                               00385200
SHARE    EQU   0                                                        00385300
SHAREIO  EQU   SHARE+2                                                  00385400
SHRBASE  EQU   SHAREIO+1                                                00385500
SHRCUCPU EQU   SHAREIO+8                                                00385600
EDITLOAD EQU   0                                                        00385700
EDITABLE EQU   EDITLOAD+4                                               00385800
CUA      EQU   0                                                        00385900
CUAVAL   EQU   CUA+2                                                    00386000
DEVICE   EQU   0                                                        00386100
MODELS   EQU   0                                                        00386200
DEVSERAL EQU   0                                                        00386300
VOLUME   EQU   0                                                        00386400
CPUS     EQU   0                                                        00386500
CPUCUAS  EQU   0                                                        00386600
CPUCUAV  EQU   CPUCUAS+2                                                00386700
LIBADR   EQU   0                                                        00386800
SYMCODE  EQU   0                                                        00386900
SYMVAL   EQU   SYMCODE+2                                                00387000
MERIDIAN EQU   0                                                        00387100
NEXTBYTE EQU   0                                                        00387200
ADDNUM   EQU   0                                                        00387300
*                                                                       00387400
IOPLINE  EQU   0                                                        00387500
PRINTDS  EQU   IOPLINE                                                  00387600
PRINTCC  EQU   IOPLINE+1                                                00387700
PRINTBDY EQU   IOPLINE+2                                                00387800
*                                                                       00387900
RECTB    EQU   0                                                        00388000
FORPTR   EQU   RECTB                                                    00388100
JUNK     EQU   RECTB+4                                                  00388200
JUNK3    EQU   JUNK+6                                                   00388300
RECSPT   EQU   RECTB+12                                                 00388400
OHEADER  EQU   0                                                        00388500
OHDRREC1 EQU   OHEADER+11                                               00388600
OHDRREC2 EQU   OHEADER+22                                               00388700
RECAREA  EQU   0                                                        00388800
@NM00028 EQU   RECAREA+2                                                00388900
NSSWITCH EQU   @NM00028                                                 00389000
LCSWITCH EQU   @NM00028                                                 00389100
DATETIME EQU   RECAREA+8                                                00389200
DATE     EQU   DATETIME                                                 00389300
DATE0    EQU   DATE                                                     00389400
GMTSW    EQU   DATE0                                                    00389500
YEAR     EQU   DATE+1                                                   00389600
DAYS     EQU   DATE+2                                                   00389700
TIME     EQU   DATETIME+4                                               00389800
HOURS    EQU   TIME                                                     00389900
MINS     EQU   TIME+1                                                   00390000
SECS     EQU   TIME+2                                                   00390100
TNTHS    EQU   TIME+3                                                   00390200
GETTIT   EQU   GETHIS                                                   00390300
EDFLD    EQU   EDIFILD                                                  00390400
CTPTR    EQU   CONTPTR                                                  00390500
BUFFER   EQU   BUF1                                                     00390600
BCOUNT   EQU   BUFFER                                                   00390700
BCCHHR   EQU   BCOUNT                                                   00390800
BSIZE    EQU   BCOUNT+6                                                 00390900
BDATA    EQU   BUFFER+8                                                 00391000
BSZ      EQU   BSIZE                                                    00391100
BBRDW    EQU   BCCHHR+4                                                 00391200
*                                                                       00391300
         END   IFCIOHND                                                 00391400
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IFCIOHND('ZP60037')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60037)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60037)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60037)
        DIS(WRITE)
        .
/*
//
