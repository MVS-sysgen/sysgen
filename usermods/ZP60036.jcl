//ZP60036  JOB (SYSGEN),'J01 M47: ZP60036',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  SUPPORT ANY MVS-SUPPORTED DASD FOR LOGREC - SYSMOD 2 OF 3
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60036)          /* FIX LOGREC DEVICE TYPE SUPPORT */  .
++VER(Z038) FMID(FBB1221) PRE(UZ53051)
 /*
   PROBLEM DESCRIPTION:
     MVS DOES NOT SUPPORT SYS1.LOGREC ON ALL DASD DEVICE TYPES.
       THE DASD DEVICE TYPES THAT THE LOGREC INITIALIZATION
       SUPPORTS IS HARD-CODED WITHIN THE MODULE.  THIS MEANS THAT
       IN ORDER MAINTAIN A FUNCTIONING LOGREC FACILTY THE SYSTEM
       RESIDENCE VOLUME IS LIMITED TO THE ORIGINAL SET OF DASD
       DEVICE TYPES AND CANNOT BE MIGRATED TO EXPLOIT LARGER VOLUME
       GEOMETRIES AS SUPPORT FOR NEWER DEVICES IS ADDED TO MVS.

       THIS USERMOD SHIPS A VERSION OF IFCDIP00 WHICH USES THE MVS
       TRKCALC SERVICE TO OBTAIN DASD GEOMETRY DEPENDENT DETAILS
       INSTEAD OF PRE-CODED VALUES SO THAT NOW IT SUPPORTS ANY
       DASD DEVICE TYPE SUPPORTED BY THE SYSTEM.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 36.

     THIS IS SYSMOD NUMBER 2 OF 3 IN A PACKAGE TO GENERALIZE LOGREC
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
       IFCDIP00
 */.
++MOD(IFCDIP00) DISTLIB(AOSCD).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//MACLIB   EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* Create a small macro library containing only BASR which is        *
//* required for successful assembly in the next step. Reference:     *
//* http://www.jaymoseley.com/hercules/compiling/spopcodes.htm        *
//* ***************************************************************** *
//*
//SYSUT2   DD  DSN=&&BASRMAC,DISP=(NEW,PASS),
//             DCB=SYS1.MACLIB,UNIT=SYSDA,SPACE=(TRK,(10,,1))
//SYSPRINT DD  SYSOUT=*
//SYSIN DD DATA,DLM=')('
./ ADD NAME=BASR
         MACRO
&NAME    BASR  &R1,&R2
&NAME    DC    0H'0',X'0D',AL.4(&R1,&R2)
         MEND
)(
/*
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=*.MACLIB.SYSUT2,DISP=(OLD,PASS)
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
IFC      TITLE 'IFCDIP00 - SYS1.LOGREC INITIALIZATION'                  00000100
*                                                                       00000200
*        NAME - IFCDIP00                                                00000300
*                                                                       00000400
*        STATUS -                                                       00000500
*        AVAILABLE SOURCE CODE WAS UPDATED BY DISASSEMBLY TO            00000600
*        REFLECT THE OWNING FMID OF FBB1221. THE LOAD MODULE            00000700
*        ASSEMBLED FROM THE UPDATED SOURCE MATCHED THE MVS 3.8J         00000800
*        DISTRIBUTION MODULE PRIOR TO THE ENHANCEMENTS AS LISTED        00000900
*        BELOW                                                          00001000
*                                                                       00001100
*        ENHANCEMENTS -                                                 00001200
*        1. REMOVAL OF THE DASD GEOMETRY DATA TABLE                     00001300
*           EXCEPT FOR A TABLE OF DEVICE UNIT NAMES USED TO             00001400
*           IDENTIFY THE LOGREC DEVICE UNIT NAME IN MSG IFC001I         00001500
*        2. ALL DASD GEOMETRY DATA IS OBTAINED DYNAMICALLY              00001600
*           FROM IECZDTAB OR CALCULATED BY THE TRKCALC SERVICE          00001700
*        3. IFCDIP00 WILL INITIALIZE A LOGREC DATASET ON                00001800
*           ANY DASD SUPPORTED BY THE EXECUTING MVS SYSTEM              00001900
*        4. ADDITIONAL IFC001I MESSAGE IDENTIFYING THE DATASET          00002000
*           NAME AND VOLSER OF THE INITIALIZED LOGREC DATASET           00002100
*        5. USE OF STANDARD SYSTEM MAPPING MACROS                       00002200
*        6. NUMEROUS CHANGES TO IMPROVE SOURCE READABILITY              00002300
*                                                                       00002400
*        FUNCTION -                                                     00002500
*        IFCDIP00 PERFORMS THE FOLLOWING FUNCTIONS -                    00002600
*                 (1) INITIALIZE THE SYS1.LOGREC DATASET                00002700
*                     BY WRITING A HEADER RECORD DESCRIBING             00002800
*                     LOCATION OF THE DATASET AND THE DEVICE            00002900
*                     GEOMETRY OF THE DASD DEVICE.                      00003000
*                     ADDITIONALLY A SECOND RECORD IS WRITTEN           00003100
*                     WHICH IS UPDATED BY SVC 76 UPON SVC 76            00003200
*                     RECEIVING AN OUTAGE REQUEST.                      00003300
*                 (2) SUPPORT FOR THE 303X PROCESSORS.                  00003400
*                     EREP PROCESSING IS NOT DEPENDENT ON THE           00003500
*                     CONTENT OR ENGINEERING CHANGE (EC) LEVEL OF       00003600
*                     THE PROCESSOR LOGOUTS TO FORMAT MACHINE           00003700
*                     CHECK AND CHANNEL CHECK RECORDS. INSTEAD,         00003800
*                     THE 7443 SERVICE RECORD FILE (SRF) DEVICE         00003900
*                     PROVIDES FORMAT AND CONTENT INFORMATION           00004000
*                     CONTAINED IN FRAMES ON DISKETTE TO FORMAT         00004100
*                     MCH AND CCH RECORDS. IN A 303X ATTACHED           00004200
*                     PROCESSOR AND MULTIPROCESSOR ENVIRONMENT,         00004300
*                     EACH PROCESSOR HAS ITS OWN SRF DEVICE.            00004400
*                     CUSTOMER ENGINEERING MAINTAINS THE SRF            00004500
*                     FRAMES (RECORDS CONTAINING TEXT AND SCAN          00004600
*                     BUFFER CODES TO FORMAT MCH AND CCH RECORDS)       00004700
*                     ON EACH SRF DEVICE.                               00004800
*                     UP TO 16 FRAMESXX DD STATEMENTS ARE SUPPORTED     00004900
*                                                                       00005000
*       NOTE -                                                          00005100
*       IFCDIP00 MUST RUN AUTHORIZED BECAUSE IT ISSUES AN ENQ ON THE    00005200
*       MAJOR NAME OF SYSZLOGR AND THE MINOR NAME OF RECORDER. THIS     00005300
*       ENQ IS TO PREVENT IFBSVC76 UPDATING THE LOGREC DATASET          00005400
*       WHILST IT IS BEING INITIALIZED BY IFCDIP00.                     00005500
*       ENQS WITH A MAJOR NAME BEGINNNING WITH SYSZ CAN ONLY BE         00005600
*       ISSUED BY PROGRAMS RUNNING AUTHORIZED.                          00005700
*                                                                       00005800
*       HEADER RECORD -                                                 00005900
*                                                                       00006000
*       *************************************************************** 00006100
*       *              *                                *     HIGH    * 00006200
*  +0   *  F F F F     *          LOW EXTENT            *   EXTENT    * 00006300
*       *              *            (CCHH)              *      (CC)   * 00006400
*       *************************************************************** 00006500
*       * HIGH EXTENT  *       *         RECORD ENTRY AREA ADDR       * 00006600
*  +8   *   (CONT.)    * SPARE *                (BBCCHHR)             * 00006700
*       *    (HH)      *       *                                      * 00006800
*       *************************************************************** 00006900
*       *  REC. ENTRY  *  REMAINING     *  TOTAL BYTES  *  ADDR OF    * 00007000
*  +16  * AREA ADDR.   *   BYTES ON     *      ON       *  LAST RECORD* 00007100
*       *    (CONT.)   *     TRACK      *    TRACK      *    WRITTEN  * 00007200
*       *************************************************************** 00007300
*       *           ADDR OF LAST RECORD          * TRACKS      * EARLY* 00007400
*  +24  *                WRITTEN (CONT.)         *    PER      * WARN.* 00007500
*       *                                        * CYLINDER    *  CNT.* 00007600
*       *************************************************************** 00007700
*       * EARLY * DEV- *     EARLY WARNING              * EARLY*  CK. * 00007800
*  +32  * WARN. *  ICE *     MESSAGE TRACK              * WARN.* BYTE * 00007900
*       * CNT.  * TYPE *     (CCHH)                     * SWT. * (FF) * 00008000
*       *************************************************************** 00008100
*                                                                       00008200
IFCDIP00 CSECT                                                          00008300
*                                                                       00008400
         SAVE  (14,12),,'IFCDIP00 ZP60036 &SYSDATE &SYSTIME'            00008500
*                                                                       00008600
         LA    R10,0(,R15)                                              00008700
         LA    R11,2048(,R10)                                           00008800
         LA    R11,2048(,R11)                                           00008900
         LA    R12,2048(,R11)                                           00009000
         LA    R12,2048(,R12)                                           00009100
*                                                                       00009200
         USING IFCDIP00,R10,R11,R12                                     00009300
*                                                                       00009400
         ST    R13,@SA00001+4          CHAIN SAVE AREAS                 00009500
         LA    R14,@SA00001                                             00009600
         ST    R14,8(,R13)                                              00009700
         LR    R13,R14                                                  00009800
*                                                                       00009900
*RETCODE = 0; /*INITIALIZE RETURN CODE TO SUCCESSFULL                */ 00010000
         SLR   R15,R15                                                  00010100
         ST    R15,RETCODE                                              00010200
         ST    R15,HIGHRETC            ZERO HIGHRETC                    00010300
*                                                                       00010400
*********************************************************************** 00010500
*                                                                       00010600
*        PROCESS JCL PARM                                               00010700
*                                                                       00010800
*********************************************************************** 00010900
*                                                                       00011000
*PARMPTR = R1; /*ESTABLISH ADDRESSABILITY TO PARM FIELD              */ 00011100
*IF PARMLEN = ZERO THEN /*IF PARM FIELD EXISTS                      */  00011200
         L     R14,0(,R1)              R14 -> PARM ADDR                 00011300
         LH    R9,0(,R14)                                               00011400
         LTR   R9,R9                   L'PARM = 0 ?                     00011500
         BE    @RF00071                YES, BRANCH, NO PARM TO PROCESS  00011600
*  DO;                                                                  00011700
*    IF PARMLEN = SIX THEN  /*IF PARAMETER PROPER LENGTH THEN*/         00011800
         C     R9,KF6                                                   00011900
         BNE   @RF00073                                                 00012000
*     DO;                                                               00012100
*      IF PARM = FRAMES THEN  /*IF PARAMETER IS FRAMES              */  00012200
         CLC   2(L'KFRAMES,R14),KFRAMES  PARM=FRAMES ?                  00012300
         BNE   @RF00075                                                 00012400
*        FRAMEYES = ON;                                                 00012500
         OI    SWITCH,FRAMEYES         SET FRAMES REQUESTED             00012600
         B     @RF00071                                                 00012700
*                                                                       00012800
*      ELSE /*PARM IS NOT FRAMES                                    */  00012900
*       DO;                                                             00013000
*        CALL MSGWTR(11);                                               00013100
@RF00075 LA    R1,MSG11                                                 00013200
         BAL   R14,MSGWTR                                               00013300
*        RETCODE = SIXTEEN;                                             00013400
         MVC   RETCODE,KF16                                             00013500
         B     @RF00071                                                 00013600
*                                                                       00013700
*       END;                                                            00013800
*     END;                                                              00013900
*    ELSE /*NOT PROPER LENGTH                                           00014000
*     DO;                                                               00014100
*      CALL MSGWTR(11); /*ISSUE IMPROPER PARM FIELD MESSAGE          */ 00014200
@RF00073 LA    R1,MSG11                                                 00014300
         BAL   R14,MSGWTR                                               00014400
*      RETCODE = SIXTEEN;                                               00014500
         MVC   RETCODE,KF16                                             00014600
*     END;                                                              00014700
*  END;                                                                 00014800
*                                                                       00014900
*********************************************************************** 00015000
*                                                                       00015100
*        SETUP FOR PROCESSING LOGREC DATASET                            00015200
*                                                                       00015300
*********************************************************************** 00015400
*                                                                       00015500
*IF RETCODE = SUCCESS THEN                                              00015600
@RF00071 ICM   R15,B'1111',RETCODE     HAS THERE BEEN A BAD PARM ?      00015700
         BNZ   @RF00087                YES, BRANCH                      00015800
*DO;                                   NO, PROCEED                      00015900
*                           /*OPEN SYS1.LOGREC                       */ 00016000
         OPEN (SERERDS,OUTPUT)                                          00016100
*                                                                       00016200
*IF (OPENOK) = OPENOK THEN            /* SUCCESSFULL OPEN ?          */ 00016300
         TM    SERERDS+DCBOFLGS-IHADCB,DCBOFOPN                         00016400
         BNO   @RF00090                 DCB FAILED TO OPEN, BRANCH      00016500
*  DO;                                                                  00016600
*                                                                       00016700
*        ENQUEUE SYS1.LOGREC                                            00016800
*        LOCK OUT ALL OTHER LOGREC (SVC 76) USERS IN THE SYSTEM         00016900
*                                                                       00017000
         ENQ   (SYSZLOGR,LOGRECA,E,8,SYSTEM)                            00017100
*                                                                       00017200
*        READ THE JFCB TO GET LOGREC DSNAME AND VOLSER                  00017300
*                                                                       00017400
         RDJFCB (SERERDS)              DATASET HAS BEEN OPENED SO OK    00017500
*                                                                       00017600
*        LOCATE LOGREC DATASET IN THE TIOT TO GET THE UCB ADDR          00017700
*        AND HENCE THE UCBTYPE                                          00017800
*        THE DATASET HAS BEEN SUCCCESSFULLY OPENED SO THE TIOT          00017900
*        ENTRY WILL BE FOUND                                            00018000
*                                                                       00018100
         L     R1,CVTPTR                                                00018200
         USING CVTMAP,R1                                                00018300
         L     R1,CVTTCBP              R1 -> TCB PTR                    00018400
         DROP  R1                                                       00018500
         L     R1,4(,R1)               R1 -> CURRENT TCB                00018600
         USING TCB,R1                                                   00018700
         ICM   R1,B'1111',TCBTIO       R1 -> TIOT                       00018800
         DROP  R1                                                       00018900
         ST    R1,TIOTPTR              SAVE FOR LATER USE               00019000
         USING TIOT1,R1                                                 00019100
         SR    R15,R15                                                  00019200
GETTIOTL CLC   TIOEDDNM,KSERERDS       DDNAME SERERDS ?                 00019300
         BE    GETTIOTF                YES, BRANCH                      00019400
         IC    R15,TIOELNGH            NO, R15 = L'CURRENT TIOT ENTRY   00019500
         AR    R1,R15                  INCR TO NEXT TIOT ENTRY          00019600
         CLI   TIOELNGH,0              L'NEXT TIOT ENTRY = 0 ?          00019700
         BNE   GETTIOTL                NO, PROCESS NEXT TIOT ENTRY      00019800
         B     @RF00090                SOMETHING HAS GONE VERY WRONG    00019900
*                                                                       00020000
GETTIOTF SR    R3,R3                                                    00020100
         ICM   R3,B'0111',TIOEFSRT     GET UCB ADDR AND ADDR IEFUCBOB   00020200
         DROP  R1                                                       00020300
         USING UCB,R3                                                   00020400
*                                                                       00020500
*        LOCATE DEVICE ENTRY IN IECZDTAB FOR THIS DASD DEVICE TYPE      00020600
*                                                                       00020700
         LA    R2,DVCTYPMK             R1 = MASK TO EXTACT UNIT TYPE    00020800
         N     R2,UCBTYP               DASD UNIT TYPE NOW IN R2         00020900
         DROP  R3                                                       00021000
         SR    R3,R3                   ZERO WORK REG                    00021100
         L     R1,CVTPTR               R1 -> CVT                        00021200
         L     R1,CVTZDTAB-CVTMAP(,R1) R1 -> IECZDTAB                   00021300
         USING DVCTI,R1                R1 -> INDEX AT START OF IECZDTAB 00021400
         IC    R3,DVCTIOFF(R2)         R3  = DEVICE OFFSET IN IECZDTAB  00021500
         DROP  R1                                                       00021600
         AR    R3,R1                   R3 -> DEVICE ENTRY IN IECZDTAB   00021700
         ST    R3,ADVCT                SAVE IECZDTAB DEVICE ENTRY       00021800
*                                                                       00021900
*    CALL OLDDIP; /*CREATE HEADER RECORD AND NULL RECORD             */ 00022000
         BAL   R14,OLDDIP                                               00022100
*    IF RETCODE = SUCCESS & /*IF SUCCESSFULLY WRITE HDR              */ 00022200
*       FRAMEYES = ON THEN /*IF USER SPECIFIES WRITE FRAMES          */ 00022300
         ICM   R15,B'1111',RETCODE                                      00022400
         BNZ   @RF00094                                                 00022500
         TM    SWITCH,FRAMEYES                                          00022600
         BNO   @RF00094                                                 00022700
*     DO;                                                               00022800
*      CALL NEWDIP; /*WRITE FRAMES TO SYS1.LOGREC                    */ 00022900
         BAL   R14,NEWDIP                                               00023000
         TM    SWITCH,ONEFRAME                                          00023100
         BNO   IFCD00F0                                                 00023200
         BAL   R14,WRTHDR                                               00023300
*      IF RETCODE = FOUR THEN                                           00023400
IFCD00F0 CLC   RETCODE,KF4                                              00023500
         BNE   @RF00094                                                 00023600
*        RETCODE = SUCCESS;                                             00023700
         XC    RETCODE,RETCODE                                          00023800
*     END;                                                              00023900
*                                       /*OTHER USERS ALLOWED LOGREC*/  00024000
@RF00094 DEQ   (SYSZLOGR,LOGRECA,8,SYSTEM)                              00024100
*    IF RETCODE > TWELVE | FRAMEYES = NO THEN                           00024200
*      DO;                                                              00024300
         L     R15,RETCODE                                              00024400
         C     R15,KF12                                                 00024500
         BH    @RT00101                                                 00024600
         TM    SWITCH,FRAMEYES                                          00024700
         BNZ   @RF00101                                                 00024800
*      END;                                                             00024900
@RT00101 EQU   *                                                        00025000
*                                       /*OTHER USERS ALLOWED LOGREC*/  00025100
@RF00101 LA    R1,SERERDS               R1 - > DCB                      00025200
         USING IHADCB,R1                                                00025300
         MVC   DCBFDAD+3(4),SAVENDCC    SET DCBFDAD TO LAST TRACK       00025400
*                                       IN DATA SET                     00025500
         MVC   DCBTRBAL,SAVTRKLN        SET TRACK BALANCE TO EMPTY      00025600
*                                       TRACK                           00025700
         DROP  R1                                                       00025800
*                                                                       00025900
         CLOSE (SERERDS,DISP)                                           00026000
*                                                                       00026100
*    IF RETCODE < SIXTEEN THEN /*IF LOGREC HAS BEEN INITIALIZED*/       00026200
         L     R15,RETCODE                                              00026300
         C     R15,KF16                                                 00026400
         BNL   @RF00087                                                 00026500
*      CALL MSGWTR(1); /*ISSUE END OF JOB MESSAGE                    */ 00026600
         LA    R1,@AL00109                                              00026700
         BAL   R14,MSGWTR                                               00026800
         B     @RF00087                                                 00026900
*                                                                       00027000
*  END;                                                                 00027100
*ELSE  /*UNSUCCESSFULL OPEN OF SYS1.LOGREC                           */ 00027200
*  DO;                                                                  00027300
*    CALL MSGWTR(2);                                                    00027400
@RF00090 LA    R1,@AL00112                                              00027500
         BAL   R14,MSGWTR                                               00027600
*    RETCODE = SIXTEEN;                                                 00027700
         MVC   RETCODE,KF16                                             00027800
*  END;                                                                 00027900
*END;                                                                   00028000
*R15 = RETCODE;                                                         00028100
@RF00087 L     R15,RETCODE             SET RETURN COED IN R15 PRIOR     00028200
         B     @EL00001                TO EXIT                          00028300
*                                                                       00028400
*********************************************************************** 00028500
*                                                                       00028600
*        BUILD AND WRITE THE HEADER RECORD TO LOGREC                    00028700
*                                                                       00028800
*********************************************************************** 00028900
*                                                                       00029000
*OLDDIP: PROC;                                                          00029100
OLDDIP   STM   R14,R12,@SA00002                                         00029200
*CALL BUILDHDR; /*FORMAT SYS1.LOGREC HEADER FROM CONTROL BLOCK INFO  */ 00029300
         BAL   R14,BUILDHDR                                             00029400
*IF RETCODE = SUCCESS THEN /*IF VALID SYSTEM RESIDENCE DEVICE        */ 00029500
         ICM   R15,B'1111',RETCODE                                      00029600
         BNZ   @RF00119                                                 00029700
*  CALL WRITELOG(40,WRITEPTR); /*WRITE HEADER RECORD TO LOGREC       */ 00029800
         LA    R1,WRITEPRM                                              00029900
         BAL   R14,WRITELOG                                             00030000
*IF RETCODE = SUCCESS THEN                                              00030100
@RF00119 ICM   R15,B'1111',RETCODE                                      00030200
         BNZ   @RF00121                                                 00030300
*                                                                       00030400
*        WRITE NULL RECORD TO SYS1.LOGREC                               00030500
*        THIS WILL BE UPDATED BY SVC 76 TO BE THE IPL TIMESTAMP         00030600
*        RECORD                                                         00030700
*                                                                       00030800
*  DO;                                                                  00030900
*    BUF240 = (BUF240 && BUF240); /*CONSTRUCT NULL RECORD OF        */  00031000
*                                 /*HEX ZEROS VIA EXCLUSIVE OR      */  00031100
         XC    BUF240(HDRRECL),BUF240                                   00031200
*    CALL WRITELOG(40,WRITEPTR); /*WRITE NULL RECORD TO SYS1.LOGREC */  00031300
         LA    R1,WRITEPRM                                              00031400
         BAL   R14,WRITELOG                                             00031500
*    IF RETCODE = SUCCESS THEN RETURN TO CALLER                         00031600
         ICM   R15,B'1111',RETCODE                                      00031700
         BZ    @ER00002                                                 00031800
*  END;                                                                 00031900
*ELSE /*ERROR WRITING HEADER RECORD                                  */ 00032000
*  CALL MSGWTR(3);                                                      00032100
         LA    R1,DATA0FD4                                              00032200
         BAL   R14,MSGWTR                                               00032300
         B     @ER00002                                                 00032400
*                                                                       00032500
*  CALL MSGWTR(3);                                                      00032600
@RF00121 LA    R1,DATA0FD4                                              00032700
         BAL   R14,MSGWTR                                               00032800
*END; /*END OF PROC OLDDIP                                           */ 00032900
@ER00002 LM    R14,R12,@SA00002                                         00033000
         BR    R14                                                      00033100
*                                                                       00033200
*********************************************************************** 00033300
*                                                                       00033400
*        NEWDIP                                                         00033500
*        SCAN THROUGH THE TIOT AND PROCESS EACH FRAMESXX DD STMT        00033600
*                                                                       00033700
*********************************************************************** 00033800
*                                                                       00033900
*NEWDIP: PROC; /*READ THE SRF TO GET FRAME TEXT, CONSTRUCT FRAME     */ 00034000
NEWDIP   STM   R14,R12,@SA00003                                         00034100
*                                                                       00034200
         TIME  DEC                                                      00034300
*                                                                       00034400
         STCM  R0,B'1111',RECTIME                                       00034500
         STCM  R1,B'1111',RECDAT                                        00034600
         OI    FRFLAG,FRFLAGY                                           00034700
         B     IFCD028E                                                 00034800
*                                                                       00034900
IFCD0202 L     R15,TIOTPTR             R15 -> TIOT DD ENTRY             00035000
         USING TIOT1,R15                                                00035100
         CLC   TIOEDDNM(L'KFRAMES),KFRAMES  FRAMESXX DDNAME IN TIOT ?   00035200
         BNE   IFCD027E                NO, BRANCH                       00035300
         LH    R14,DDCOUNT             YES, GET CNT OF FRAMES DD STMTS  00035400
         C     R14,KF16                MORE THAN 16 ?                   00035500
         BNL   IFCD0272                YES, BRANCH                      00035600
         LA    R14,1(,R14)             INCR COUNT OF FRAMES DD STMTS    00035700
         STH   R14,DDCOUNT             UPDATE COUNT                     00035800
         MVC   FRAMEDD,TIOEDDNM        GET SPECIFIC FRAMESXX DDNAME     00035900
         MVC   SRFDCB+DCBDDNAM-IHADCB(8),TIOEDDNM  MOVE INTO DCB        00036000
         MVI   RECTYPE,X'A0'                                            00036100
         MVI   FRAMTYPS,X'02'                                           00036200
         MVI   SEQCNT,X'00'                                             00036300
         MVI   RECCNT,X'00'                                             00036400
         BAL   R14,PROCSRF             PROCESS A SRF DEVICE             00036500
         L     R15,RETCODE                                              00036600
         C     R15,HIGHRETC                                             00036700
         BNH   IFCD0254                                                 00036800
         ST    R15,HIGHRETC                                             00036900
*          IF RETCODE = SUCCESS THEN  /*IF SUCCESSFUL READ          */  00037000
IFCD0254 L     R15,RETCODE                                              00037100
         C     R15,KF16                RETURN CODE GT 16 ?              00037200
         BNL   IFCD026A                EQUAL OR GREATER, BRANCH         00037300
         SLR   R15,R15                                                  00037400
         ST    R15,RETCODE             ZERO RETCODE                     00037500
         B     IFCD027E                                                 00037600
*                                                                       00037700
IFCD026A NI    FRFLAG,255-FRFLAGY      NO FRAMESXX DD                   00037800
         B     IFCD027E                                                 00037900
*                                                                       00038000
IFCD0272 LA    R1,DATA0FD8                                              00038100
         BAL   R14,MSGWTR                                               00038200
         NI    FRFLAG,255-FRFLAGY      NO FRAMESXX DD                   00038300
IFCD027E L     R15,TIOTPTR             GET CURRENT TIOTPTR              00038400
         SLR   R14,R14                                                  00038500
         IC    R14,TIOELNGH            ADD L'CURRENT ENTRY              00038600
         AR    R15,R14                                                  00038700
         CLI   TIOELNGH,0              L'NEXT TIOT ENTRY = 0 ?          00038800
         BE    IFCD02A2                YES, BRANCH, END OF TIOT         00038900
         ST    R15,TIOTPTR             UPDATE TIOTPTR                   00039000
*                                                                       00039100
*        INITIAL ENTRY INTO ROUTINE                                     00039200
*                                                                       00039300
IFCD028E L     R15,TIOTPTR             R15 -> CURRENT TIOT ENTRY        00039400
         CLI   TIOELNGH,0              L'NEXT TIOT ENTRY = 0 ?          00039500
         BE    IFCD02A2                YES, BRANCH                      00039600
         TM    FRFLAG,FRFLAGY          FRAMESXX DD ?                    00039700
         BO    IFCD0202                RE-ENTER LOOP                    00039800
         DROP  R15                                                      00039900
IFCD02A2 CLC   HIGHRETC,KF12                                            00040000
         BNE   IFCD02BE                                                 00040100
         TM    SWITCH,ONEFRAME                                          00040200
         BNO   IFCD02BE                                                 00040300
         MVC   RETCODE,KF8                                              00040400
         B     IFCD02C6                                                 00040500
*                                                                       00040600
IFCD02BE L     R15,HIGHRETC                                             00040700
         ST    R15,RETCODE                                              00040800
IFCD02C6 LH    R15,DDCOUNT             ANY FRAMESXX DD STMTS ?          00040900
         LTR   R15,R15                                                  00041000
         BNZ   IFCD02D8                YES, BRANCH                      00041100
         LA    R1,DATA0FDC             MO, GENERATE ERROR MSG           00041200
         BAL   R14,MSGWTR                                               00041300
IFCD02D8 LM    R14,R12,@SA00003                                         00041400
         BR    R14                     RETURN                           00041500
*                                                                       00041600
*********************************************************************** 00041700
*                                                                       00041800
*        PROCESS SRF                                                    00041900
*                                                                       00042000
*********************************************************************** 00042100
*                                                                       00042200
*        FOR A MERIDIAN LIKE CPU (303X) WITH AN ACCESSIBLE SRF          00042300
*                                                                       00042400
PROCSRF  STM   R14,R12,@SA011C8                                         00042500
*            CALL OPENSRF;         /*OPEN SYSTEM REFERENCE FILE      */ 00042600
         BAL   R14,OPENSRF                                              00042700
*            IF RETCODE = SUCCESS THEN                                  00042800
         ICM   R15,B'1111',RETCODE                                      00042900
         BNZ   IFCD0364                                                 00043000
*              DO;                                                      00043100
*                CALL GETSER;       /*RETRIEVE CPU SERIAL NUMBER     */ 00043200
         BAL   R14,GETSER                                               00043300
*            IF RETCODE = SUCCESS THEN                                  00043400
         ICM   R15,B'1111',RETCODE                                      00043500
         BNZ   IFCD036C                                                 00043600
*                CALL BUILDFRH; /*BUILD CONSTANT PART OF FRAME HEADER*/ 00043700
         BAL   R14,BUILDFRH                                             00043800
*                CALL VERBPROC;            /* PERFORM VERBAGE        */ 00043900
         BAL   R14,VERBPROC                                             00044000
*                CALL READSRF;  /*READ FRAME TEXT ACROSS SYSTEM      */ 00044100
*                               /*REFERENCE FILE INTERFACE SET       */ 00044200
         BAL   R14,READSRF                                              00044300
*                               /*RETURN CODE IF EOF OR FAILURE      */ 00044400
*                IF RETCODE = SUCCESS THEN /*IF NOT RETRIEVE FRAME*/    00044500
         ICM   R15,B'1111',RETCODE                                      00044600
         BZ    IFCD033A                                                 00044700
*                  DO;                                                  00044800
*                    IF RETCODE = FOUR THEN /*IF EOF ON 1ST READ     */ 00044900
         C     R15,KF4                                                  00045000
         BNE   IFCD0328                                                 00045100
*                      CALL MSGWTR(10);                                 00045200
         LA    R1,@AL00193                                              00045300
         BAL   R14,MSGWTR                                               00045400
         B     IFCD0330                                                 00045500
*                    ELSE  /*LEGITIMATE READ ERROR                   */ 00045600
*                      CALL MSGWTR(7);                                  00045700
IFCD0328 LA    R1,@AL00282                                              00045800
         BAL   R14,MSGWTR                                               00045900
*                    RETCODE = TWELVE; /*INDICATE NO FRAMES */          00046000
IFCD0330 MVC   RETCODE,KF12                                             00046100
         B     IFCD035C                                                 00046200
*                                                                       00046300
*                  END;                                                 00046400
*                ELSE /*SUCCESSFUL READ OF A FRAME                      00046500
*                  DO;                                                  00046600
*                    READPTR = (READPTR && WRITEPTR);/*SHIFT READ,WRTE* 00046700
IFCD033A L     R15,WRITEPTR                                             00046800
         L     R14,READPTR                                              00046900
         XR    R14,R15                                                  00047000
         ST    R14,READPTR                                              00047100
*                    WRITEPTR = (WRITEPTR && READPTR);/*BUFFERS VIA 3*/ 00047200
*                    READPTR = (READPTR && WRITEPTR); /*EXCLUSIVE ORS*/ 00047300
         XR    R15,R14                                                  00047400
         ST    R15,WRITEPTR                                             00047500
         XR    R14,R15                                                  00047600
         ST    R14,READPTR                                              00047700
*                    RECCNT = 1; /*FIRST FRAME OF THE SET            */ 00047800
         MVI   RECCNT,X'01'                                             00047900
*                    SEQCNT = 1; /*FIRST FRAME FOR 50 FRAME LIMIT CK*/  00048000
         MVI   SEQCNT,X'01'                                             00048100
IFCD035C BAL   R14,PROCREC                                              00048200
         B     IFCD036C                                                 00048300
*                                                                       00048400
IFCD0364 LA    R1,@AL00151                                              00048500
         BAL   R14,MSGWTR                                               00048600
IFCD036C LM    R14,R12,@SA011C8                                         00048700
         BR    R14                                                      00048800
*                                                                       00048900
*                  END;                                                 00049000
*              END;                                                     00049100
*                                                                       00049200
*********************************************************************** 00049300
*                                                                       00049400
*        PROCESS RECORD READ FROM SRF                                   00049500
*                                                                       00049600
*********************************************************************** 00049700
*                                                                       00049800
PROCREC  STM   R14,R12,@SA012F4                                         00049900
         B     IFCD0488                                                 00050000
*                                                                       00050100
IFCD037A BAL   R14,READSRF                                              00050200
         ICM   R15,B'1111',RETCODE     GET RETURN CODE                  00050300
         BZ    IFCD03CE                ZERO, BRANCH                     00050400
*                                      NON ZERO RETURN CODE             00050500
*              RECNLST = OFF;/*INDICATE RECORD IN BUFFER IS LAST OF ST* 00050600
         NI    RECNLST,B'01111111'                                      00050700
*              IF RETCODE = FOUR & /*IF END OF FILE                  */ 00050800
*                 FRAMTYPS = MCF THEN /*HAVE BEEN READING MCH FRMS*/    00050900
         C     R15,KF4                                                  00051000
         BNE   IFCD042C                                                 00051100
         CLI   FRAMTYPS,X'02'                                           00051200
         BNE   IFCD042C                                                 00051300
*                  DO;                                                  00051400
*                    FRAMTYPS = CCF; /*SET UP TO READ CCF FRAMES     */ 00051500
         MVI   FRAMTYPS,X'01'                                           00051600
*                    RETCODE = SUCCESS; /*IND SUCCESS, NOT COMPLETE  */ 00051700
         XC    RETCODE,RETCODE                                          00051800
*                    SEQCNT = ZERO; /*RESET PHYSICAL SEQUENCE NUMBER */ 00051900
         MVI   SEQCNT,X'00'                                             00052000
*                                   /*FOR NEXT SET OF FRAMES         */ 00052100
*                    CALL VERBPROC;        /* PERFORM VERBAGE        */ 00052200
         BAL   R14,VERBPROC                                             00052300
*                    CALL READSRF; /*READ SRF TO RESET SEQUENCING    */ 00052400
         BAL   R14,READSRF                                              00052500
*                    IF RETCODE = FOUR THEN /*IF EOF, NO CCF FRAMES  */ 00052600
         CLC   RETCODE,KF4                                              00052700
         BNE   IFCD042C                                                 00052800
*                      DO;   /*RETCODE ALREADY SET TO 8 IF HARD ERROR*/ 00052900
*                        CALL MSGWTR(10); /*MISSING FRAME SET MESSAGE*/ 00053000
         LA    R1,@AL00193                                              00053100
         BAL   R14,MSGWTR                                               00053200
*                        RETCODE = 12   ; /*INDICATE MISSING FRAMES  */ 00053300
         MVC   RETCODE,KF12                                             00053400
         B     IFCD042C                                                 00053500
*                                                                       00053600
*                  END;                                                 00053700
*            END;                                                       00053800
*          ELSE  /*IF SUCCESSFUL READ                                */ 00053900
*            IF RECCNT = MAXFRM THEN /*IF FRAME JUST READ IS ONE TOO */ 00054000
*              DO;                   /*MANY FOR EREP1 THEN           */ 00054100
IFCD03CE CLI   RECCNT,50               READ 50 RECORDS ?                00054200
         BNE   IFCD042C                NO, BRANCH                       00054300
*                CALL MSGWTR(8); /*MAXIMUM FRAME NUMBER EXCEEDED MESS*/ 00054400
         LA    R1,@AL00180                                              00054500
         BAL   R14,MSGWTR                                               00054600
*                RETCODE = 12   ; /*INDICATE LOST FRAMES             */ 00054700
         MVC   RETCODE,KF12                                             00054800
*                RECNLST = OFF; /*SET LAST FRAME SWITCH ON.          */ 00054900
         NI    RECNLST,B'01111111'                                      00055000
*                IF FRAMTYPS = MCF THEN /*IF CHANNEL FRAMES LEFT     */ 00055100
         CLI   FRAMTYPS,X'02'                                           00055200
         BNE   IFCD0426                                                 00055300
*                  DO;                                                  00055400
*                    RETCODE = ZERO; /*CONTINUE IN LOOP              */ 00055500
         XC    RETCODE,RETCODE                                          00055600
*                    FRAMLOST = ON; /*INDICATE FRAMES LOST WITHOUT   */ 00055700
*                     /*USING RETURN CODE AND EXITING LOOP           */ 00055800
         OI    SWITCH,FRAMLOST                                          00055900
*                    SEQCNT = ZERO; /*RESET FOR CHANNEL FRAMES       */ 00056000
         MVI   SEQCNT,X'00'                                             00056100
*                    FRAMTYPS = CCF; /*PREPARE TO READ CHANNEL FRMS  */ 00056200
         MVI   FRAMTYPS,X'01'                                           00056300
*                    CALL VERBPROC;        /* PERFORM VERBAGE        */ 00056400
         BAL   R14,VERBPROC                                             00056500
*                    CALL READSRF; /*READ FRAME OVERLAY TOO MANY FRAME* 00056600
         BAL   R14,READSRF                                              00056700
*                    IF RETCODE = FOUR THEN /*IF END OF FILE ON 1ST  */ 00056800
         CLC   RETCODE,KF4                                              00056900
         BNE   IFCD042C                                                 00057000
*                      DO;                /*CCH FRAME READ THEN*/       00057100
*                        CALL MSGWTR(10); /*ISSUE MISSING SET MESSAGE*/ 00057200
         LA    R1,@AL00193                                              00057300
         BAL   R14,MSGWTR                                               00057400
*                        RETCODE = 12;  /*IND MISSING FRAMES AND     */ 00057500
*                                       /*EXIT THE LOOP              */ 00057600
         MVC   RETCODE,KF12                                             00057700
         B     IFCD042C                                                 00057800
*                                                                       00057900
*                      END;                                             00058000
*                  END;                                                 00058100
*                ELSE /*NO MORE MACHINE FRAMES MUST BE READ          */ 00058200
*                  RETCODE = 12   ; /*DONT ALLOW FRAME JUST READ     */ 00058300
IFCD0426 MVC   RETCODE,KF12                                             00058400
*              END;               /*OR ANY FURTHER FRAMES TO BE READ */ 00058500
*          RECORD = RECBUILD;     /*MOVE BUILT HEADER TO BUFFER      */ 00058600
IFCD042C L     R1,WRITEPTR                                              00058700
         MVC   0(24,R1),RECBUILD                                        00058800
*          RECCNT = SEQCNT + 1; /*UP INDEX FOR NEXT RECORD           */ 00058900
         SR    R1,R1                                                    00059000
         IC    R1,SEQCNT                                                00059100
         LA    R1,1(,R1)                                                00059200
         STC   R1,RECCNT                                                00059300
*          SEQCNT = SEQCNT + 1; /*UP RUNNING SEQUENCE NUMBER OF RECRD*/ 00059400
         STC   R1,SEQCNT                                                00059500
*          RECNLST = ON;   /*RESET LAST RECORD SWITCH FOR NEXT RECORD*/ 00059600
         OI    RECNLST,B'10000000'                                      00059700
*          IF FRAMTYPS = CCF THEN /*IF NOW READING CHANNEL FRAMES    */ 00059800
         CLI   FRAMTYPS,X'01'                                           00059900
         BNE   IFCD0458                                                 00060000
*            RECTYPE = 'B0'X;     /*THEN SET RECORD TYPE TO CCF FOR  */ 00060100
         MVI   RECTYPE,X'B0'                                            00060200
*                                 /*FRAME WRITTEN NEXT LOOP ITERATION*/ 00060300
*          CALL WRITELOG(1944,WRITEPTR); /*WRITE PREVIOUS FRAME TO LG*/ 00060400
IFCD0458 LA    R1,@AL00205                                              00060500
         BAL   R14,WRITELOG                                             00060600
*                               /*NOTE ITS LOCATION AND CHECK FOR    */ 00060700
*                               /*OVERFLOW OF THE EXTENT             */ 00060800
*                               /*COUNT SUCCESSFUL WRITES OF FRAMES  */ 00060900
*          IF RETCODE = SUCCESS THEN                                    00061000
         ICM   R15,B'1111',RETCODE                                      00061100
         BNZ   IFCD046E                                                 00061200
*            ONEFRAME = ON;                                             00061300
         OI    SWITCH,ONEFRAME                                          00061400
*          READPTR = (READPTR && WRITEPTR); /*SWITCH READ,WRITE      */ 00061500
IFCD046E L     R15,WRITEPTR                                             00061600
         L     R14,READPTR                                              00061700
         XR    R14,R15                                                  00061800
         ST    R14,READPTR                                              00061900
*          WRITEPTR = (WRITEPTR && READPTR); /*BUFFERS VIA THREE     */ 00062000
         XR    R15,R14                                                  00062100
         ST    R15,WRITEPTR                                             00062200
*          READPTR = (READPTR && WRITEPTR); /*EXCLUSIVE OR           */ 00062300
         XR    R14,R15                                                  00062400
         ST    R14,READPTR                                              00062500
*         END; /*END OF READ, WRITE LOOP                             */ 00062600
IFCD0488 ICM   R15,B'1111',RETCODE                                      00062700
         BZ    IFCD037A                                                 00062800
*  CALL CLOSESRF; /*DISABLE AND CLOSE SRF                            */ 00062900
         BAL   R14,CLOSESRF                                             00063000
*  IF FRAMLOST = ON THEN /*IF FRAMES LOST BUT RETCODE NOT SET        */ 00063100
         TM    SWITCH,FRAMLOST                                          00063200
         BNO   IFCD04A4                                                 00063300
*    RETCODE = 12   ; /*USE RETCODE TO PASS RETURN CODE TO USER      */ 00063400
         MVC   RETCODE,KF12                                             00063500
IFCD04A4 LM    R14,R12,@SA012F4                                         00063600
         BR    R14                                                      00063700
*                                                                       00063800
*********************************************************************** 00063900
*                                                                       00064000
*        REWRITE HEADER RECORD                                          00064100
*                                                                       00064200
*********************************************************************** 00064300
*                                                                       00064400
WRTHDR   STM   R14,R12,@SA00004                                         00064500
         L     R6,SERERDS+DCBDEBAD-IHADCB  R6 ->SERERDS DEB             00064600
         USING DEBBASIC,R6                                              00064700
         LA    R6,DEBBASND                                              00064800
         USING DEBDASD,R6              R6 -> DEB DASD EXTENTION         00064900
         MVC   SERERDS+DCBFDAD+3-IHADCB(4),DEBENDCC   SET HIGH EXTENT   00065000
*                                                     FOR EOF           00065100
         DROP  R6                                                       00065200
*                                                                       00065300
         CLOSE (SERERDS,REREAD)    CLOSE LOGREC FOR NORMAL WRITING      00065400
*                                                                       00065500
         OPEN  (SERERDS,UPDAT)   OPEN LOGREC TO REWRITE THE HEADER      00065600
*                                                                       00065700
         READ  LISTDECB,SF,SERERDS,HDRREC,HDRRECL,,,,MF=E               00065800
*                                                                       00065900
         CHECK LISTDECB                                                 00066000
*                                                                       00066100
*     CALL REDOHDR;        /*REWRITE LOGREC HEADER TO REFLECT FRAMES*/  00066200
         BAL   R14,REDOHDR                                              00066300
*                                                                       00066400
         WRITE LISTDECB,SF,SERERDS,HDRREC,HDRRECL,,,,MF=E               00066500
*                                                                       00066600
         CHECK LISTDECB                                                 00066700
*    END;                                                               00066800
*END;                                                                   00066900
         LM    R14,R12,@SA00004                                         00067000
         BR    R14                                                      00067100
*                                                                       00067200
*********************************************************************** 00067300
*                                                                       00067400
*        FORMAT SYS1.LOGREC HEADER RECORD FROM INFO IN                  00067500
*        SYSTEM CONTROL BLOCKS                                          00067600
*                                                                       00067700
*********************************************************************** 00067800
*                                                                       00067900
*BUILDHDR: PROC;                                                        00068000
BUILDHDR STM   R14,R12,@SA0000X                                         00068100
*  CALL GETDEV; /*FORMAT HEADER RECORD FIELDS FROM DEVICE TABLE      */ 00068200
*               /*AND THE DCB,DEB                                    */ 00068300
         BAL   R14,GETDEV                                               00068400
         MVC   BUF240(HDRRECL),HDRREC   MOVE HEADER TO INITIAL WRITE BF 00068500
*END;                                                                   00068600
@ER00004 LM    R14,R12,@SA0000X                                         00068700
         BR    R14                                                      00068800
*                                                                       00068900
*********************************************************************** 00069000
*                                                                       00069100
*        WRITELOG                                                       00069200
*                                                                       00069300
*        WRITE A RECORD TO SYS1.LOGREC USING BSAM                       00069400
*        RETRIEVE THE ADDR OF THE RECORD USING NOTE CHECK TO SEE        00069500
*        IF SYS1.LOGREC EXTENT HAS OVERFLOWED                           00069600
*        SET RETURN CODE IF WRITE ERROR OR OVERFLOW EXTENT              00069700
*                                                                       00069800
*********************************************************************** 00069900
*                                                                       00070000
*        ON ENTRY -                                                     00070100
*        R1 -> A(L'RECORD)                                              00070200
*              A(RECORD TO BE WRITTEN)                                  00070300
*                                                                       00070400
*WRITELOG: PROC;                                                        00070500
WRITELOG STM   R14,R12,@SA00005                                         00070600
         L     R2,4(,R1)                R2 -> RECORD ADDR               00070700
         L     R2,0(,R2)                R2 -> RECORD                    00070800
*R3 = INPARM; /*LENGTH OF RECORD TO BE WRITTEN                       */ 00070900
         L     R3,0(,R1)                R3 -> L'RECORD ADDR             00071000
         L     R3,0(,R3)                R3 = L'RECORD                   00071100
*                                                                       00071200
DWRITE   WRITE SERDECB,SF,SERERDS,(2),(3)                               00071300
*                                                                       00071400
         CHECK SERDECB                                                  00071500
*                                                                       00071600
         NOTE  SERERDS                  RETRIEVE TTR OF RECORD WRITTEN  00071700
*                                                                       00071800
*TTRSAVE = R1;     /*STORE RELATIVE DIRECT ACCESS LOCATION FROM NOTE*/  00071900
         ST    R1,TTRSAVE                                               00072000
*IF FRAMEYES = ON &   /*IF FRAMES, CHECK TO SEE IF ENOUGH ROOM ON   */  00072100
*   TTRSAVE = MAXTTR THEN /*ON LOGREC FOR NEXT WRITE. ASSUME AT     */  00072200
         TM    SWITCH,FRAMEYES                                          00072300
         BNO   @RF00240                                                 00072400
         L     R15,TTRSAVE                                              00072500
         C     R15,MAXTTR                                               00072600
         BNE   @RF00240                                                 00072700
*                         /*LEAST ONE TRACK FOR HEADER AND TIME STMP */ 00072800
*  DO;                                                                  00072900
*    CALL MSGWTR(4); /*NOT ENOUGH ROOM FOR FRAMES                    */ 00073000
         LA    R1,@AL00242                                              00073100
         BAL   R14,MSGWTR                                               00073200
*    RETCODE = SIXTEEN; /*NO ROOM FOR ERROR RECORDS SO BETTER ABORT  */ 00073300
         MVC   RETCODE,KF16                                             00073400
         B     ENDOPROC                                                 00073500
*                                                                       00073600
*  END;                                                                 00073700
*ELSE IF FRAMEYES = ON THEN /*IF SUCCESSFUL WRITE OF A FRAME THEN    */ 00073800
@RF00240 TM    SWITCH,FRAMEYES                                          00073900
         BNO   ENDOPROC                                                 00074000
*  ONEFRAME = ON; /*AT LEAST ONE FRAME ON LOGREC SUCCESSFULLY        */ 00074100
         OI    SWITCH,ONEFRAME                                          00074200
*GOTO ENDOPROC; /*BOUNCE AROUND THE NON=SYNCHRONOUS ERROR EXIT*/        00074300
         B     ENDOPROC                                                 00074400
*                                                                       00074500
*LOGERR:; /*NON-STRUCTURED ASYNCHRONOUS ERROR EXIT                   */ 00074600
LOGERR   ST    R14,LOGSAVE                                              00074700
*                                                                       00074800
         SYNADAF ACSMETH=BSAM                                           00074900
*                                                                       00075000
*IF TTRSAVE < THIRD THEN /*IF WRITING HEADER                         */ 00075100
         L     R15,TTRSAVE                                              00075200
         C     R15,THIRD                                                00075300
         BNL   @RF00249                                                 00075400
*  DO;                                                                  00075500
*    CALL MSGWTR(3);                                                    00075600
         LA    R1,DATA0FD4                                              00075700
         BAL   R14,MSGWTR                                               00075800
*    RETCODE = 16;                                                      00075900
         MVC   RETCODE,KF16                                             00076000
         B     IFCD064E                                                 00076100
*                                                                       00076200
*  END;                                                                 00076300
*ELSE /*ATTEMPTING TO WRITE FRAME                                    */ 00076400
*  DO;                                                                  00076500
*    CALL MSGWTR(5);                                                    00076600
@RF00249 LA    R1,@AL00255                                              00076700
         BAL   R14,MSGWTR                                               00076800
*    RETCODE = 8;                                                       00076900
         MVC   RETCODE,KF16                                             00077000
*                                                                       00077100
IFCD064E SYNADRLS                                                       00077200
*                                                                       00077300
         L     R14,LOGSAVE                                              00077400
         BR    R14                                                      00077500
*  END;                                                                 00077600
*ENDOPROC:;                                                             00077700
*END; /*END OF WRITE TO LOGREC PROC                                     00077800
ENDOPROC LM    R14,12,@SA00005                                          00077900
         BR    R14                                                      00078000
*                                                                       00078100
*********************************************************************** 00078200
*                                                                       00078300
*        READ SRF, IDENTIFY END OF FILE AND ERROR CONDITIONS            00078400
*                                                                       00078500
*********************************************************************** 00078600
*                                                                       00078700
*READSRF: PROC;                                                         00078800
READSRF  STM   R14,R12,@SA00006                                         00078900
*IF RETCODE = SUCCESS THEN DO;                                          00079000
         SLR   R15,R15                                                  00079100
         C     R15,RETCODE             RETCODE = ZERO ?                 00079200
         BNE   @ER00006                NO, BRANCH                       00079300
* IOBSTART = ADDR(RDVERB);     /*SET ADDR OF FUNCTION SELECT CCW     */ 00079400
         LA    R14,RDVERB                                               00079500
         ST    R14,IOBSTART                                             00079600
* SRFECB = ZERO;               /*CLEAR THE EVENT CONTROL BLOCK       */ 00079700
         ST    R15,SRFECB                                               00079800
* RDADR  = READPTR + 24; /*  ADDR OF READ BUFF INTO CHANNEL PROGRAM  */ 00079900
         L     R1,READPTR                                               00080000
         LA    R1,24(,R1)                                               00080100
         STCM  R1,B'0111',RDVERB+1                                      00080200
* DO UNTIL ECB = INTERCPT;/*DO UNTIL REQUEST NOT INTERCEPTED-44     */  00080300
*                                                                       00080400
@DL00266 EXCP  SRFIOB                                                   00080500
*                                                                       00080600
         WAIT  ECB=SRFECB              WAIT UNTIL FRAME HAS BEEN READ   00080700
* END;                                                                  00080800
@DE00266 CLI   SRFECB,ECBINCPT                                          00080900
         BE    @DL00266                                                 00081000
* IF ECB = OKAY THEN  /*EITHER ERROR OR END OF FILE                 */  00081100
         CLI   SRFECB,ECBNORM                                           00081200
         BE    @ER00006                                                 00081300
*   DO;                                                                 00081400
*     IF ECB  = EOFMAYBE THEN /*IF ECB NON HARD ERROR                */ 00081500
         CLI   SRFECB,ECBPERR                                           00081600
         BNE   @RF00271                                                 00081700
*       DO;                                                             00081800
*         IF IOBUSTAT = ON & /*IF CHANNEL END                        */ 00081900
*            DEVEND  = ON & /*IF DEVICE END                          */ 00082000
*            UNITEXCP = ON & /*IF UNIT EXCEPTION                     */ 00082100
*            IOBUSTAT = OFF THEN /*IF NOT A UNIT CHECK               */ 00082200
         TM    IOBUSTAT,IOBUSB4+IOBUSB5+IOBUSB7                         00082300
         BNO   @RF00273                                                 00082400
         TM    IOBUSTAT,IOBUSB6                                         00082500
         BNZ   @RF00273                                                 00082600
*            RETCODE = FOUR; /*LEGITIMATE END OF FILE                */ 00082700
         MVC   RETCODE,KF4                                              00082800
         B     @ER00006                                                 00082900
*                                                                       00083000
*         ELSE /*ILLEGITIMATE                                        */ 00083100
*           DO;                                                         00083200
*             RETCODE = EIGHT; /*UNSUCCESSFUL FRAME READ             */ 00083300
@RF00273 MVC   RETCODE,KF12                                             00083400
*             CALL MSGWTR(7);                                           00083500
         LA    R1,@AL00282                                              00083600
         BAL   R14,MSGWTR                                               00083700
         B     @ER00006                                                 00083800
*                                                                       00083900
*           END;                                                        00084000
*       END;                                                            00084100
*     ELSE /*HARD ERROR BY DEFAULT                                   */ 00084200
*       DO;                                                             00084300
*         RETCODE = EIGHT;                                              00084400
@RF00271 MVC   RETCODE,KF12                                             00084500
*         CALL MSGWTR(7);                                               00084600
         LA    R1,@AL00282                                              00084700
         BAL   R14,MSGWTR                                               00084800
*       END;                                                            00084900
*   END;                                                                00085000
*END;                                      /* END OF READ            */ 00085100
*END; /*END OF PROC READSRF                                          */ 00085200
@ER00006 LM    R14,R12,@SA00006                                         00085300
         BR    R14                                                      00085400
*                                                                       00085500
*********************************************************************** 00085600
*                                                                       00085700
*        PERFORM VERBAGE CCW AND TEST FOR ERROR                         00085800
*                                                                       00085900
*********************************************************************** 00086000
*                                                                       00086100
*VERBPROC:  PROC;                                                       00086200
VERBPROC STM   R14,R12,@SA00007                                         00086300
*IF FRAMTYPS = CCF THEN                                                 00086400
         CLI   FRAMTYPS,X'01'                                           00086500
         BNE   @RF00288                                                 00086600
*  IOBFLAG1 = IOBUNREL                                                  00086700
         MVI   IOBFLAG1,IOBUNREL                                        00086800
*IOBSTART = ADDR(VERBAGE);         /* SET ADDR OF VERBAGE CCW        */ 00086900
@RF00288 LA    R15,VERBAGE                                              00087000
         ST    R15,IOBSTART                                             00087100
*SRFECB = ZERO;                    /* CLEAR ECB                      */ 00087200
         SLR   R15,R15                                                  00087300
         ST    R15,SRFECB                                               00087400
*DO UNTIL ECB = INTERCPT;         /* LOOP UNTIL REQUEST NOT INTRCPTD*/  00087500
*                                                                       00087600
@DL00292 EXCP  SRFIOB              ISSUE EXCP                           00087700
*                                                                       00087800
         WAIT  ECB=SRFECB          WAIT UNTIL ECB IS POSTED             00087900
*END;                                                                   00088000
@DE00292 CLI   SRFECB,ECBINCPT                                          00088100
         BE    @DL00292                                                 00088200
*IF ECB = OKAY THEN               /* IF FRAME READ FAILED           */  00088300
         CLI   SRFECB,ECBNORM                                           00088400
         BE    @ER00007                                                 00088500
*  DO;                                                                  00088600
*    CALL MSGWTR(12);                                                   00088700
         LA    R1,@AL00297                                              00088800
         BAL   R14,MSGWTR                                               00088900
*    RETCODE = EIGHT;                                                   00089000
         MVC   RETCODE,KF12                                             00089100
*  END;                                                                 00089200
*END;                              /* END OF VERBPROC PROC           */ 00089300
@ER00007 LM    R14,R12,@SA00007                                         00089400
         BR    R14                                                      00089500
*                                                                       00089600
*********************************************************************** 00089700
*                                                                       00089800
*        WRITE ERROR MESSAGES AND END OF JOB TEXT VIA WTO               00089900
*                                                                       00090000
*********************************************************************** 00090100
*                                                                       00090200
*MSGWTR: PROC;                                                          00090300
MSGWTR   STM   R14,R12,@SA00008                                         00090400
         MVC   @PC00008(4),0(R1)                                        00090500
*IF MSGNO = 1 THEN                                                      00090600
         L     R15,@PC00008                                             00090700
         CLC   0(4,R15),KF1                                             00090800
         BNE   @RF00303                                                 00090900
*  DO;                                                                  00091000
*    CALL BUILDTXT; /*MODIFY WTO MESSAGE TEXT                        */ 00091100
         BAL   R14,BUILDTXT                                             00091200
* MSG ALIGNMENT=         *+19   *+26 *+31       *+42       *+53         00091300
         CNOP  0,4                      ALIGN MODIFIED TEXT             00091400
IFC001I  WTO   'IFC001I  D=XXXX N=0Y F=CCCCHHHH L=CCCCHHHH S=CCCCHHHH02X00091500
                DIP COMPLETE',ROUTCDE=(1),DESC=(6)                      00091600
*                                                                       00091700
*        ISSUE WTO TO IDENTIFY LOGREC DATASET NAME AND VOLSER           00091800
*                                                                       00091900
         MVI   IFC001I+15,C' '         BLANK OUT TEXT                   00092000
         MVC   IFC001I+16(60),IFC001I+15                                00092100
         MVC   IFC001I+17(4),=C'DSN='   MOVE IN DATASET NAME            00092200
         MVC   IFC001I+21(L'JFCBDSNM),JFCBDSNM   MOVE IN DATASET NAME   00092300
         LA    R14,IFC001I+21                                           00092400
BLKLOOP  CLI   0(R14),C' '             LOOK FOR END OF DATASET NAME     00092500
         BNH   LOOPEND                                                  00092600
         LA    R14,1(,R14)              INCR LOOP PTR                   00092700
         B     BLKLOOP                                                  00092800
*                                                                       00092900
LOOPEND  MVC   1(4,R14),=C'VOL='                                        00093000
         MVC   5(6,R14),JFCBVOLS                                        00093100
         LA    R1,IFC001I+4                                             00093200
*                                                                       00093300
         WTO   MF=(E,(1))               ISSUE WTO                       00093400
*                                                                       00093500
         B     @RC00303                                                 00093600
*                                                                       00093700
*  END; /*END OF GENERATING MESSAGE 1                                */ 00093800
*ELSE /*NOT MESSAGE ONE                                              */ 00093900
*  DO;                                                                  00094000
*    IF MSGNO = 2 THEN                                                  00094100
@RF00303 L     R15,@PC00008                                             00094200
         CLC   0(4,R15),KF2                                             00094300
         BNE   @RF00309                                                 00094400
*      DO;                                                              00094500
*                                                                       00094600
         WTO   'IFC002I SYS1.LOGREC CANNOT BE OPENED',ROUTCDE=(1),     X00094700
               DESC=(6)                                                 00094800
*                                                                       00094900
         B     @RC00303                                                 00095000
*                                                                       00095100
*      END;                                                             00095200
*    ELSE /*NOT 1 NOR 2                                              */ 00095300
*      DO;                                                              00095400
*        IF MSGNO = 3 THEN                                              00095500
@RF00309 L     R15,@PC00008                                             00095600
         CLC   0(4,R15),KF3                                             00095700
         BNE   @RF00314                                                 00095800
*          DO;                                                          00095900
*            RETCODE = SIXTEEN; /*PREVENT FURTHER PROCESSING         */ 00096000
         MVC   RETCODE,KF16                                             00096100
*                                                                       00096200
         WTO   'IFC003I SYS1.LOGREC HEADER WRITE ERROR',ROUTCDE=(1),   X00096300
               DESC=(6)                                                 00096400
*                                                                       00096500
         B     @RC00303                                                 00096600
*                                                                       00096700
*          END;                                                         00096800
*        ELSE /*NOT 1,2,3                                            */ 00096900
*          DO;                                                          00097000
*            IF MSGNO = 5 THEN                                          00097100
@RF00314 L     R15,@PC00008                                             00097200
         CLC   0(4,R15),KF5                                             00097300
         BNE   @RF00320                                                 00097400
         MVC   IFC005I+49(8),FRAMEDD                                    00097500
*              DO;                                                      00097600
*                                                                       00097700
IFC005I  WTO   'IFC005I SYS1.LOGREC FRAME WRITE ERROR,DD=FRAMESXX',    X00097800
               ROUTCDE=(1),DESC=(6)                                     00097900
*                                                                       00098000
         B     @RC00303                                                 00098100
*                                                                       00098200
*              END;                                                     00098300
*            ELSE /*NOT 1,2,3,5                                      */ 00098400
*              DO;                                                      00098500
*                IF MSGNO = 4 THEN                                      00098600
@RF00320 L     R15,@PC00008                                             00098700
         CLC   0(4,R15),KF4                                             00098800
         BNE   @RF00325                                                 00098900
*                  DO;                                                  00099000
*                                                                       00099100
         WTO   'IFC004I SYS1.LOGREC ALLOCATION TOO SMALL FOR FRAMES',  X00099200
               ROUTCDE=(1),DESC=(6)                                     00099300
*                                                                       00099400
         B     @RC00303                                                 00099500
*                                                                       00099600
*                  END;                                                 00099700
*                ELSE /*NOT 1,2,3,4,5                                */ 00099800
*                  DO;                                                  00099900
*                    IF MSGNO = 6 THEN                                  00100000
@RF00325 L     R15,@PC00008                                             00100100
         CLC   0(4,R15),KF6                                             00100200
         BNE   @RF00330                                                 00100300
*                      DO;                                              00100400
         MVC   IFC006I+40(8),FRAMEDD                                    00100500
*                                                                       00100600
IFC006I  WTO   'IFC006I SRF CANNOT BE OPENED,DD=FRAMESXX',             X00100700
               ROUTCDE=(1),DESC=(6)                                     00100800
*                                                                       00100900
         B     @RC00303                                                 00101000
*                                                                       00101100
*                      END;                                             00101200
*                    ELSE /*NOT 1,2,3,4,5, OR 6                      */ 00101300
*                      DO;                                              00101400
*                        IF MSGNO = 7 THEN                              00101500
@RF00330 L     R15,@PC00008                                             00101600
         CLC   0(4,R15),KF7                                             00101700
         BNE   @RF00335                                                 00101800
*                          DO;                                          00101900
*                            CALL FILLMESS; /*FILL IN SPECIFICS OF MES* 00102000
         BAL   R14,FILLMESS                                             00102100
         MVC   IFC007I+64(8),FRAMEDD                                    00102200
*                                                                       00102300
IFC007I  WTO   'IFC007I SRF READ ERROR SENSE=SSSS,CSW=CCCCCCCCCCCCCC,DDX00102400
               =FRAMESXX',ROUTCDE=(1),DESC=(6)                          00102500
*                                                                       00102600
         B     @RC00303                                                 00102700
*                                                                       00102800
*                          END;                                         00102900
*                        ELSE /*NOT 1,2,3,4,5,6,7                    */ 00103000
*                          DO;                                          00103100
*                            IF MSGNO = 8 THEN                          00103200
@RF00335 L     R15,@PC00008                                             00103300
         CLC   0(4,R15),KF8                                             00103400
         BNE   @RF00341                                                 00103500
*                              DO;                                      00103600
         MVC   IFC008I+46(8),FRAMEDD                                    00103700
*                                                                       00103800
IFC008I  WTO   'IFC008I MORE THAN 50 FRAMES IN SET,DD=FRAMESXX',       *00103900
               ROUTCDE=(1),DESC=(6)                                     00104000
*                              END;                                     00104100
*                            ELSE /*NOT 1,2,3,4,5,6,7,8              */ 00104200
*                              DO;                                      00104300
         B     @RC00303                                                 00104400
*                                                                       00104500
*                                IF MSGNO = 9 THEN                      00104600
@RF00341 L     R15,@PC00008                                             00104700
         CLC   0(4,R15),KF9                                             00104800
         BNE   @RF00346                                                 00104900
*                                  DO;                                  00105000
*                                  RETCODE = SIXTEEN; /*ABORT        */ 00105100
         MVC   RETCODE,KF16                                             00105200
*                                                                       00105300
         WTO   'IFC009I INVALID SYSTEM RESIDENCE DEVICE',ROUTCDE=(1),  X00105400
               DESC=(6)                                                 00105500
*                                                                       00105600
         B     @RC00303                                                 00105700
*                                  END;                                 00105800
*                                ELSE /*NOT 1,2,3,4,5,6,7,8,9*/         00105900
*                                  DO;                                  00106000
*                                    IF MSGNO = 10 THEN                 00106100
@RF00346 L     R15,@PC00008                                             00106200
         CLC   0(4,R15),KF10                                            00106300
         BNE   @RF00352                                                 00106400
*                                      DO;                              00106500
*                                                                       00106600
IFC155I  WTO   'IFC155I FRAME SET MISSING,DD=FRAMESXX',                X00106700
               ROUTCDE=(1),DESC=(6)                                     00106800
*                                                                       00106900
         B     @RC00303                                                 00107000
*                                      END;                             00107100
*                                    ELSE IF MSGNO = 11 THEN            00107200
@RF00352 L     R15,@PC00008                                             00107300
         CLC   0(4,R15),KF11                                            00107400
         BNE   @RC00303                                                 00107500
*                                      DO;                              00107600
*                                                                       00107700
         WTO   'IFC156I INVALID PARM FIELD',ROUTCDE=(1),DESC=(6)        00107800
*                                                                       00107900
*                                      END;                             00108000
* IF MSGNO = TWELVE THEN                                                00108100
@RC00303 L     R15,@PC00008                                             00108200
         CLC   0(4,R15),KF12                                            00108300
         BNE   TESTE13                                                  00108400
*   DO;                                                                 00108500
*                                                                       00108600
         WTO   'IFC157I VERBAGE FAILURE',ROUTCDE=(1),DESC=(6)           00108700
*                                                                       00108800
         B     @ER00008                                                 00108900
*   END;                                                                00109000
TESTE13  L     R15,@PC00008                                             00109100
         CLC   0(4,R15),KF13                                            00109200
         BNE   TESTE14                                                  00109300
*                                                                       00109400
         WTO   'IFC158I MORE THAN 16 FRAMES DD STATEMENTS',            C00109500
               ROUTCDE=(1),DESC=(6)                                     00109600
*                                                                       00109700
         MVC   RETCODE,KF12                                             00109800
         B     @ER00008                                                 00109900
*                                                                       00110000
TESTE14  L     R15,@PC00008                                             00110100
         CLC   0(4,R15),KF14                                            00110200
         BNE   TESTE15                                                  00110300
         MVC   TESTE14A+41(8),FRAMEDD                                   00110400
*                                                                       00110500
TESTE14A WTO   'IFC159I UNABLE TO READ CPU ID,DD=FRAMESXX',            C00110600
               ROUTCDE=(1),DESC=(6)                                     00110700
*                                                                       00110800
         MVC   RETCODE,KF12                                             00110900
         B     @ER00008                                                 00111000
*                                                                       00111100
TESTE15  L     R15,@PC00008                                             00111200
         CLC   0(4,R15),KF15                                            00111300
         BNE   @ER00008                                                 00111400
*                                                                       00111500
TESTE16  WTO   'IFC160I NO FRAMES DD STATEMENTS, PARM=FRAMES IGNORED', C00111600
               ROUTCDE=(1),DESC=(6)                                     00111700
*                                                                       00111800
         MVC   RETCODE,KF12                                             00111900
*                                  END;                                 00112000
*                              END;                                     00112100
*                          END;                                         00112200
*                      END;                                             00112300
*                  END;                                                 00112400
*              END;                                                     00112500
*          END;                                                         00112600
*      END;                                                             00112700
*   END;                                                                00112800
*END; /*END OF PROC MSGWTR                                           */ 00112900
@ER00008 LM    R14,R12,@SA00008                                         00113000
         BR    R14                                                      00113100
*                                                                       00113200
*********************************************************************** 00113300
*                                                                       00113400
*        INSERT SENSE BYTES, CSW, AND DDNAME INTO ERROR MESSAGE         00113500
*        NUMBER SEVEN                                                   00113600
*                                                                       00113700
*********************************************************************** 00113800
*                                                                       00113900
*FILLMESS: PROC;                                                        00114000
FILLMESS STM   R14,R12,@SA00009                                         00114100
*R5 = ADDR(IOBSENS0); /*ADDRESS OF SENSE BYTE ONE FROM IOB           */ 00114200
         LA    R5,IOBSENS0                                              00114300
*R2 = ADDR(IFC007I+37);      /*LOCATION TO BE PLACED IN GENERATED WTO*/ 00114400
         LA    R2,IFC007I+37                                            00114500
*R3 = FOUR;                  /*NUMBER OF PRINTABLE CHARACTERS        */ 00114600
         LA    R3,4                                                     00114700
*CALL UNPACK;                /*RTN TO CONVERT TO PRINTABLE CHARACTERS*/ 00114800
         BAL   R14,UNPACK                                               00114900
*R5 = ADDR(IOBCSW); /*ADDRESS OF LOW ORDER 7 BYTES OF CSW            */ 00115000
         LA    R5,IOBCSW                                                00115100
*R2 = ADDR(IFC007I+46);      /*LOCATION TO BE PLACED IN GENERATED WTO*/ 00115200
         LA    R2,IFC007I+46                                            00115300
*R3 = EIGHT;                 /*NUMBER OF PRINTABLE CHARACTERS        */ 00115400
         LA    R3,8                                                     00115500
*CALL UNPACK;                /*CONVERT                               */ 00115600
         BAL   R14,UNPACK                                               00115700
*R5 = ADDR(IOBCSW) + 4;      /* ADDR OF LAST THREE BYTES OF CSW      */ 00115800
         LA    R5,IOBCSW+4                                              00115900
*R2 = ADDR(IFC007I+54);      /*WHERE PLACED IN WTO MESSAGE           */ 00116000
         LA    R2,IFC007I+54                                            00116100
*R3 = SIX;                   /*3 BYTES LEFT TO BE CONVERTED          */ 00116200
         LA    R3,6                                                     00116300
*CALL UNPACK;                /*CONVERT                               */ 00116400
         BAL   R14,UNPACK                                               00116500
*END;  /*END OF PROC FILLMESS                                        */ 00116600
@ER00009 LM    R14,R12,@SA00009                                         00116700
         BR    R14                                                      00116800
*                                                                       00116900
*********************************************************************** 00117000
*                                                                       00117100
*        RETRIEVE AND FORMAT CPU SERIAL NUMBER AND CPU VERSION          00117200
*        FROM SRF                                                       00117300
*                                                                       00117400
*********************************************************************** 00117500
*                                                                       00117600
*GETSER: PROC;                                                          00117700
GETSER   STM   R14,R12,@SA00010                                         00117800
         LA    R15,RDCPUID                                              00117900
         ST    R15,IOBSTART                                             00118000
         SLR   R15,R15                                                  00118100
         ST    R15,SRFECB                                               00118200
*                                                                       00118300
GETSERA  EXCP  SRFIOB                                                   00118400
*                                                                       00118500
         WAIT  1,ECB=SRFECB                                             00118600
*                                                                       00118700
         CLI   SRFECB,ECBINCPT                                          00118800
         BE    GETSERA                                                  00118900
         CLI   SRFECB,ECBNORM                                           00119000
         BE    GETSERB                                                  00119100
         LA    R1,DATA1004                                              00119200
         BAL   R14,MSGWTR                                               00119300
         B     @ER00010                EXIT                             00119400
*                                                                       00119500
GETSERB  MVZ   STCPUID(11),ALLZERO                                      00119600
         PACK  STCPUSER(6),STCPUID(11)                                  00119700
         MVI   STCPUMCL,X'00'                                           00119800
*END; /*END OF PROC GETSER                                           */ 00119900
@ER00010 LM    R14,R12,@SA00010                                         00120000
         BR    R14                                                      00120100
*                                                                       00120200
*********************************************************************** 00120300
*                                                                       00120400
*        ISSUE OPEN AND ENABLE FOR SRF                                  00120500
*                                                                       00120600
*********************************************************************** 00120700
*                                                                       00120800
*OPENSRF: PROC;                                                         00120900
OPENSRF  STM   R14,R12,@SA00011                                         00121000
         OI    SRFDCB+DCBIFLG-IHADCB,DCBIFNE3    NEVER USE IOS ERROR    00121100
*                                                RECOVERY               00121200
         OI    IOBFLAG1,IOBUNREL                                        00121300
         LA    R15,ENABLE              R15 -> ENABLE CCW                00121400
         ST    R15,IOBSTART                                             00121500
*                                                                       00121600
         OPEN  (SRFDCB,INPUT)                                           00121700
*                                                                       00121800
*IF (SRFDCB) = OPENOK THEN          /*IF OPEN WAS NOT SUCCESSFUL*/      00121900
         TM    SRFDCB+DCBOFLGS-IHADCB,DCBOFOPN                          00122000
         BO    @DL00399               BRANCH, OPEN SUCCESSFUL           00122100
*  RETCODE = TWELVE; /*INDICATE NO FRAMES CAN BE RETRIEVED           */ 00122200
         MVC   RETCODE,KF12                                             00122300
         B     @ER00011                                                 00122400
*                                                                       00122500
*ELSE /*SUCCESSFUL OPEN                                              */ 00122600
* DO;                                                                   00122700
*   /*ECB IS INITIALIZED TO 00                                       */ 00122800
*   /*IOBFLAG1 INDICATING NEITHER COMMAND NOR DATA CHAINING IS ZERO  */ 00122900
*   /*IOBSTART POINTS TO THE ENABLE CHANNEL PROGRAM                  */ 00123000
*   DO UNTIL ECB = INTERCPT;    /*LOOP UNTIL IO REQUEST HAS NOT BEEN*/  00123100
*                                /*INTERCEPTED                       */ 00123200
*                                                                       00123300
@DL00399 EXCP  SRFIOB                                                   00123400
*                                                                       00123500
         WAIT  ECB=SRFECB     WAIT UNTIL ENABLE HAS BEEN SUCCESSFUL     00123600
*   END;                                                                00123700
@DE00399 CLI   SRFECB,ECBINCPT                                          00123800
         BE    @DL00399                                                 00123900
*   IF ECB = OKAY THEN           /*IF ECB NOT 7F THEN ENABLE FAILED*/   00124000
         CLI   SRFECB,ECBNORM                                           00124100
         BE    @ER00011                                                 00124200
*     RETCODE = TWELVE;                                                 00124300
         MVC   RETCODE,KF12                                             00124400
* END;                                                                  00124500
*END; /*END OF PROC OPENSRF                                          */ 00124600
@ER00011 LM    R14,R12,@SA00011                                         00124700
         BR    R14                                                      00124800
*                                                                       00124900
*********************************************************************** 00125000
*                                                                       00125100
*        DISABLE AND CLOSE THE SRF                                      00125200
*                                                                       00125300
*********************************************************************** 00125400
*                                                                       00125500
*CLOSESRF: PROC;                                                        00125600
CLOSESRF STM   R14,R12,@SA00012                                         00125700
*    IOBFLAG1 = 0;     /*SPECIFY TYPE OF CHANNEL PROGRAM IN IOB*/       00125800
         MVI   IOBFLAG1,0                                               00125900
*    IOBSTART = ADDR(DISABLE); /*LOCATE CHANNEL PROGRAM FOR IOB*/       00126000
         LA    R15,DISABLE                                              00126100
         ST    R15,IOBSTART                                             00126200
*    SRFECB = ZERO; /*ZERO EVENT CONTROL BLOCK                       */ 00126300
         SLR   R15,R15                                                  00126400
         ST    R15,SRFECB                                               00126500
*    DO UNTIL ECB = INTERCPT;   /*LOOP UNTIL IO REQUEST HAS NOT BEEN*/  00126600
*                                /*INTERCEPTED                       */ 00126700
*                                                                       00126800
@DL00410 EXCP  SRFIOB                                                   00126900
*                                                                       00127000
         WAIT  ECB=SRFECB  WAIT UNTIL DISABLE IS SUCCESSFULL. DONT      00127100
*                          CHECK IF FAILS WHO CARES                     00127200
*    END;                                                               00127300
@DE00410 CLI   SRFECB,ECBINCPT                                          00127400
         BE    @DL00410                                                 00127500
*                               /*CLOSE SRF DEVICE                   */ 00127600
         CLOSE (SRFDCB,LEAVE)                                           00127700
*                               /*CLOSE SRF DEVICE                   */ 00127800
*   END; /*END OF PROC CLOSESRF                                      */ 00127900
@ER00012 LM    R14,R12,@SA00012                                         00128000
         BR    R14                                                      00128100
*                                                                       00128200
*********************************************************************** 00128300
*                                                                       00128400
*        BUILD CONSTANT PORTION OF FRAME HEADER                         00128500
*                                                                       00128600
*********************************************************************** 00128700
*                                                                       00128800
*BUILDFRH: PROC;                                                        00128900
BUILDFRH STM   R14,R12,12(R13)                                          00129000
*PACK(HOLD,CVTRELNO); /*PACK EBCDIC RELEASE NUMBER                   */ 00129100
         L     R15,CVTPTR                                               00129200
         LR    R14,R15                                                  00129300
         SL    R14,KF4                 R14 -> CVTRELNO                  00129400
         PACK  HOLD(8),0(2,R14)                                         00129500
*CVB(RECSYS,HOLD); /*PLACE BINARY RELEASE IN THE HEADING             */ 00129600
         CVB   R14,HOLD                                                 00129700
         STC   R14,RECSYS                                               00129800
*                                                                       00129900
*        IF (CVTDCB & '22'X) = '22'X THEN /*IF VS1 AND DYN.ADR.TRANS*/  00130000
         TM    CVTDCB-CVTMAP(R15),CVT2SPS+CVT6DAT                       00130100
         BNO   @RF00423                                                 00130200
*          RECSYS = (RECSYS | '40'X);  /*SET VS1 BITS ON             */ 00130300
         OI    RECSYS,X'40'                                             00130400
*                                                                       00130500
*        IF (CVTDCB & '12'X) = '12'X |    /*IF MVS OR SVS AND DY.ADR*/  00130600
*           (CVTDCB & '11'X) = '11'X THEN /*IF MVS                   */ 00130700
@RF00423 TM    CVTDCB-CVTMAP(R15),CVT4MS1+CVT6DAT                       00130800
         BO    @RT00425                                                 00130900
         TM    CVTDCB-CVTMAP(R15),CVT4MS1+CVTMVS2                       00131000
         BNO   @RF00425                                                 00131100
*          RECSYS = (RECSYS | '80'X);  /*SET VS2 BITS ON             */ 00131200
@RT00425 OI    RECSYS,X'80'                                             00131300
*        RECCPU = STCPUVER;/*PUT CPU VERSION, CPU MODEL ,CPU SERIAL, */ 00131400
@RF00425 MVC   RECCPU(8),STCPUVER                                       00131500
*                         /*AND MAXIMUM EXTENDED LOGOUT LENGTH IN HDR*/ 00131600
*END; /*END OF PROC BUILDFRH                                         */ 00131700
@ER00013 LM    R14,R12,12(R13)                                          00131800
         BR    R14                                                      00131900
*                                                                       00132000
*********************************************************************** 00132100
*                                                                       00132200
*        RECREATE HEADER TO REFLECT FRAMES                              00132300
*                                                                       00132400
*********************************************************************** 00132500
*                                                                       00132600
*REDOHDR:PROC;                                                          00132700
REDOHDR  STM   R14,R12,@SA00014                                         00132800
*R0 = TTRSAVE; /*R0 = TTR OF LAST WRITTEN RECORD                     */ 00132900
         L     R0,TTRSAVE                                               00133000
*R2 = ADDR(MBBCCHHR);               /*PASS LOCATION FOR ABSOLUTE ADDR*/ 00133100
         LA    R2,MBBCCHHR                                              00133200
*R15 = CVTPCNVT; /*LOAD ADDR OF CONVERSION ROUTINE                   */ 00133300
         L     R9,CVTPTR                                                00133400
         L     R15,CVTPCNVT-CVTMAP(,R9)                                 00133500
         LA    R1,SERERDS              R1 -> SERERDS DCB                00133600
         L     R1,DCBDEBAD-IHADCB(R1)  R1 -> DEB                        00133700
         LA    R3,MYSAVE               R3 -> SAVEAREA ACROSS CVTPCNVT   00133800
         STM   R9,R13,0(R3)            SAVE REGS ACROSS CALL            00133900
         BALR  R14,R15                 CALL CVTPCNVT TTR -> CCHHR RTN   00134000
         LM    R9,R13,0(R3)            RESTORE REGISTERS                00134100
         MVC   HDRREA,BBCCHHR          RECORD ENTRY AREA                00134200
         MVC   HDRLASTR,HDRREA         LAST RECORD WRITTEN LOCATION     00134300
         MVC   HDRTBAL,ARBSMALL        SET TRACK BALANCE SO SMALL       00134400
*                                      THAT NO RECORD WILL FIT          00134500
         OI    HDREWSW,HDRFRSW         SET FRAMES EXIST SWITCH          00134600
*END; /*END OF PROC REDOHDR                                          */ 00134700
@ER00014 LM    R14,R12,@SA00014                                         00134800
         BR    R14                                                      00134900
*                                                                       00135000
*****************************************************************       00135100
*                                                                       00135200
*        GETDEV                                                         00135300
*                                                                       00135400
*****************************************************************       00135500
*                                                                       00135600
*GETDEV: PROC;                                                          00135700
*        RETRIEVE EXTENT INFORMATION FROM THE DEB                       00135800
*        FORMAT HDR RECORD FIELDS                                       00135900
*        CALCULATE THE TRACK BALANCE                                    00136000
*        NOTE - ONLY THE FIRST EXTENT IS PROCESSED                      00136100
*                                                                       00136200
GETDEV   STM   R14,R12,@SA00015                                         00136300
         LA    R2,SERERDS               R2 -> SERERDS DCB               00136400
         L     R6,DCBDEBAD-IHADCB(,R2)  R6 -> DEB                       00136500
         USING DEBBASIC,R6                                              00136600
         LA    R6,DEBBASND                                              00136700
         USING DEBDASD,R6                                               00136800
*                                                                       00136900
*        INITIALIZE START AND END OF EXTENT FIELDS                      00137000
*                                                                       00137100
         MVC   HDRSTRCC,DEBSTRCC       START EXTENT OF SYS1.LOGREC      00137200
         MVC   HDRENDCC,DEBENDCC       END EXTENT OF SYS1.LOGREC        00137300
         MVC   SAVENDCC,DEBENDCC       SAVE FOR DCBFDAD                 00137400
*                                                                       00137500
         L     R7,ADVCT                R7 -> IECZDTAB DEVICE ENTRY      00137600
         USING DVCT,R7                                                  00137700
*                                                                       00137800
*        PREVENT ANY ATTEMPT TO WRITE PAST SYS1.LOGREC EXTENT           00137900
*                                                                       00138000
*        WHEN WRITING FRAMES TO SYS1.LOGREC BY CREATING                 00138100
*        ARTIFICIAL TTR0 FOR LATER COMPARISON TO NOTE AFTER BSAM        00138200
*        WRITE                                                          00138300
*                                                                       00138400
         SR    R4,R4                                                    00138500
         SR    R5,R5                                                    00138600
         MVC   SAVTRKLN,DVCTRKLN       SAVE FOR DCBTRBAL                00138700
         ICM   R5,B'0011',DVCTRKLN     R5 = BYTES PER TRACK ON LOGREC   00138800
*                                           DEVICE                      00138900
         D     R4,BYTPFRM              DIVIDE BY BYTES PER FRAME PLUS   00139000
*                                      GUESSTIMATED TRACK OVERHEAD      00139100
*                                      R5 = NUMBER OF FRAMES PER TRACK  00139200
         SR    R4,R4                                                    00139300
         ICM   R4,B'0011',DEBNMTRK     R4 = TOTAL TRACKS IN LOGREC      00139400
         BCTR  R4,0                    CHANGE TO RELATIVE TRACK NUMBER  00139500
         BCTR  R4,0                    MAKE ROOM FOR DUMMY END OF FILE  00139600
         SLL   R4,16                   MOVE TO HIGH ORDER HALFWRD (TT)  00139700
         SLL   R5,8                    FRAMES PER TRACK TO REL REC (R)  00139800
         OR    R4,R5                   MAKE MAXIMUM TTR0 IN R4          00139900
         ST    R4,MAXTTR               SAVE FOR COMPARE WHILE WRITING   00140000
*                                                                       00140100
*        COMPLETE DEVICE GEOMETRY FIELDS                                00140200
*                                                                       00140300
*        CALCULATE HDREWMT AS 90% OF TOTAL TRACKS IN EXTENT             00140400
*                                                                       00140500
         STM   R0,13,@SA00016          SAVE REGS                        00140600
         SR    R5,R5                                                    00140700
         ICM   R5,B'0011',DEBNMTRK     R5 = TRACKS IN EXTENT            00140800
         MH    R5,KH90                 CALC 90%                         00140900
         SR    R4,R4                                                    00141000
         D     R4,KF100                R5 = 90% OF TRACKS ALLOCATED     00141100
         LR    R0,R5                                                    00141200
         BCTR  R0,0                    CONVERT FROM NO OF TRACKS TO     00141300
*                                      RELATIVE TRACK                   00141400
         SLL   R0,16                   SHIFT TO TTRN FORMAT FOR         00141500
*                                      CVTPCNVT CALL                    00141600
         L     R1,SERERDS+DCBDEBAD-IHADCB  R1 -> DEB                    00141700
         LA    R2,WORKDW               R2 -> MBBCCHHR RESULTS FIELD     00141800
         LA    R4,@SA00016             GET SAVEAREA ADDR ACROSS CALL    00141900
         L     R15,CVTPTR                                               00142000
         L     R15,CVTPCNVT-CVTMAP(R15)                                 00142100
         BASR  R14,R15                 CONVERT TTRN TO MBBCCHHR         00142200
         LM    R0,R13,0(R4)            RESTORE REGS FROM @SA00016       00142300
         LTR   R15,R15                 GOOD RETURN CODE                 00142400
         BNZ   BADRES                  NO, BRANCH                       00142500
         MVC   HDREWMT,WORKDW+3        MOVE IN CCHH FOR EARLY WARNING   00142600
*                                      TEST                             00142700
*                                                                       00142800
*        CALCULATE HDREWTC AS 90% OF TRACK CAPACITY                     00142900
*                                                                       00143000
         SR    R5,R5                                                    00143100
         ICM   R5,B'0011',DVCTRKLN     R5 = BYTES PER TRACK ON LOGREC   00143200
*                                      DEVICE                           00143300
         MH    R5,KH90                                                  00143400
         SR    R4,R4                                                    00143500
         D     R4,KF100                                                 00143600
         STCM  R5,B'0011',HDREWTC                                       00143700
*                                                                       00143800
*        INITIALIZE TRACKS PER CYL FROM DEVICE TABLE ENTRY              00143900
*                                                                       00144000
         LH    R4,DVCTRK                                                00144100
         BCTR  R4,0                    DECR FOR COMPATIBLITY WITH       00144200
*                                      OLD VERSION OF IFCDIP00          00144300
         STCM  R4,B'0011',HDRTRK                                        00144400
*                                                                       00144500
*        INITIALIZE LAST RECORD WRITTEN AND NEXT RECORD TO WRITE        00144600
*                                                                       00144700
         MVC   HDRREA+2(4),HDRSTRCC    LOW EXT TO REC ENTRY AREA        00144800
         MVI   HDRREA+6,2              SET RECORD TO TWO                00144900
         MVC   HDRLASTR,HDRREA         MAKE LAST USED THE SAME          00145000
*                                                                       00145100
*        CALCULATE THE TRACK BALANCE THAT WILL RESULT AFTER             00145200
*        HEADER RECORD AND OUTAGE RECORD HAVE BEEN WRITTEN TO           00145300
*        LOGREC DATASET SO THAT THE HEADER RECORD WILL CONTAIN          00145400
*        THE CORRECT TRACK BALANCE VALUE                                00145500
*                                                                       00145600
         SR    R4,R4                                                    00145700
         ICM   R4,B'0011',SERERDS+DCBTRBAL-IHADCB   CURRENT TRK BALANCE 00145800
         LA    R5,HDRRECL              L'RECORD                         00145900
         ICM   R5,B'1000',=X'01'       SET RECORD NO = 1 FOR HDRREC     00146000
*                                                                       00146100
         TRKCALC FUNCTN=TRKBAL,REMOVE=NO,DEVTAB=(7),                   X00146200
               BALANCE=(4),RKDD=(5),REGSAVE=YES                         00146300
*                                                                       00146400
         LTR   R15,R15                 TRKCALC OK ?                     00146500
         BNZ   BADRES                  NO, BRANCH                       00146600
         LR    R4,R0                   YES, SET TRACK BALANCE           00146700
         ICM   R5,B'1000',=X'02'       SET RECORD NO = 2 FOR OUTAGE REC 00146800
*                                                                       00146900
         TRKCALC FUNCTN=TRKBAL,REMOVE=NO,DEVTAB=(7),                   X00147000
               BALANCE=(4),RKDD=(5),REGSAVE=YES                         00147100
*                                                                       00147200
         LTR   R15,R15                 TRKCALC OK ?                     00147300
         BNZ   BADRES                  NO, BRANCH                       00147400
         STCM  R0,B'0011',HDRTBAL      UPDATE HDRREC TRACK BALANCE      00147500
*                                                                       00147600
*        INITIALIZE TOTAL BYTES ON TRACK FROM DEVICE TABLE ENTRY        00147700
*                                                                       00147800
         MVC   HDRTRKLN,DVCTRKLN       TOTAL BYTES ON TRACK             00147900
*                                                                       00148000
*        INITIALIZE DEVICE TYPE FROM DCB                                00148100
*                                                                       00148200
         MVC   HDRDEVT,SERERDS+DCBDEVT-IHADCB   DEVICE TYPE CODE        00148300
         NI    HDRDEVT,DVCTYPMK        LOW ORDER NIBBLE ONLY            00148400
*                                                                       00148500
*        INITIALIZE FLAGS AND INDICATORS                                00148600
*                                                                       00148700
         MVI   HDRMSGCT,0              LOGREC FULL MSG COUNT            00148800
         MVI   HDREWSW,0               CLEAR SWITCH AREA                00148900
         MVI   HDRCHK,X'FF'            CHECK BYTE                       00149000
         B     ENDGETDV                GOTO EXIT                        00149100
*                                                                       00149200
*BADRES:;                                                               00149300
*        CALL MSGWTR(9);   /* IDENTIFY ILLEGAL SYSRES DEVICE         */ 00149400
BADRES   LA    R1,@AL00442                                              00149500
         BAL   R14,MSGWTR                                               00149600
*        RETCODE = SIXTEEN; /*DONT FORMAT SYS1.LOGREC                */ 00149700
         MVC   RETCODE,KF16                                             00149800
*ENDGETDV:;                                                             00149900
         DROP  R6,R7                                                    00150000
*END; /*END OF PROC GETDEV                                           */ 00150100
ENDGETDV LM    R14,R12,@SA00015                                         00150200
         BR    R14                                                      00150300
*                                                                       00150400
*********************************************************************** 00150500
*                                                                       00150600
*        BUILDTXT                                                       00150700
*        FORMAT HEADER RECORD INFORMATION FOR MSG IFC001I               00150800
*        MOVE INTO MSG                                                  00150900
*                                                                       00151000
*********************************************************************** 00151100
*                                                                       00151200
*BUILDTXT: PROC;                                                        00151300
BUILDTXT STM   R14,R12,@SA00017                                         00151400
         L     R5,HDRSTRCC              LOW EXTENT                      00151500
         LA    R2,F                     IMAGE AREA                      00151600
         LA    R3,8                     NUMBER OF CHAR TO CONVERT       00151700
*        CALL  UNPACK;                  /*CONVERT TO PRINTABLE*/        00151800
         BAL   R14,UNPACK                                               00151900
         LA    R3,8                     NUMBER OF CHAR TO CONVERT       00152000
         L     R5,HDRENDCC              HIGH EXTENT                     00152100
         LA    R2,L                     IMAGE AREA                      00152200
*        CALL  UNPACK;                  /*CONVERT TO PRINTABLE*/        00152300
         BAL   R14,UNPACK                                               00152400
         ICM   R5,B'1111',HDRREA+2      RESTART AREA IN HEADER (CCHH)   00152500
         LA    R3,8                     MAKE CCHH PRINTABLE             00152600
         LA    R2,S                     LOCATION IN MESSAGE             00152700
*        CALL  UNPACK;                  /*CONVERT TO PRINTABLE*/        00152800
         BAL   R14,UNPACK                                               00152900
         ICM   R5,B'1111',HDRREA+6      RESTART RECORD ADDR IN HDR      00153000
         LA    R3,2                     RECORD ADDRESS IS 2 PRINT DIG   00153100
         LA    R2,S+8                   WHERE TO PUT IN MESSAGE         00153200
*        CALL  UNPACK;                  /*CONVERT TO PRINTABLE*/        00153300
         BAL   R14,UNPACK                                               00153400
         SR    R7,R7                                                    00153500
         IC    R7,HDRDEVT               GET DEVICE CODE                 00153600
         LA    R2,HEXTAB(R7)            R2 -> PRINTABLE CHARACTER       00153700
         MVC   N(1),0(R2)               DEVICE CODE                     00153800
         BCTR  R7,0                     CONVERT TO 0 - 14               00153900
         SLL   R7,2                     MULTIPLY BY 4                   00154000
         LA    R7,DASDTAB(R7)           LOAD ENTRY ADDR OF SYSRES       00154100
         MVC   D(4),0(R7)               MOVE IN DEVICE NAME             00154200
*END; /*END OF PROC BUILDPROC                                        */ 00154300
@ER00017 LM    R14,R12,@SA00017                                         00154400
         BR    R14                                                      00154500
*                                                                       00154600
D        EQU   IFC001I+19               DEVICE NAME                     00154700
N        EQU   IFC001I+27               DEVICE TYPE CODE                00154800
F        EQU   IFC001I+31               FIRST TRACK ADDR                00154900
L        EQU   IFC001I+42               LAST TRACK ADDR                 00155000
S        EQU   IFC001I+53               RECORD START ADDRESS+ID         00155100
*                                                                       00155200
*********************************************************************** 00155300
*                                                                       00155400
*        UNPACK CHARACTERS FOR PRINT                                    00155500
*                                                                       00155600
*        R2 -> CONVERTED CHARACTERS                                     00155700
*        R5  = CHARACTERS TO BE CONVERTED                               00155800
*        R3  = NUMBER OF PRINTABLE CHARACTERS TO BE CREATED             00155900
*                                                                       00156000
*********************************************************************** 00156100
*                                                                       00156200
*UNPACK: PROC;                                                          00156300
UNPACK   STM   R14,R12,@SA00018                                         00156400
UNPACK1  SR    R4,R4                    CLEAR REG                       00156500
         SLDL  R4,4                     POSITION                        00156600
         IC    R4,HEXTAB(R4)            PRINTABLE CHARACTER             00156700
         STC   R4,0(R2)                 STORE IT                        00156800
         LA    R2,1(R2)                 UPDATE                          00156900
         BCT   R3,UNPACK1               NEXT                            00157000
*END; /*END OF UNPACK PROC                                              00157100
@ER00018 LM    R14,R12,@SA00018                                         00157200
         BR    R14                                                      00157300
*                                                                       00157400
*END; /*END OF EXTERNAL PROC IFCDIP00                                */ 00157500
@EL00001 L     R13,4(,R13)                                              00157600
         L     R14,12(,R13)                                             00157700
         LM    R0,R12,20(R13)                                           00157800
         BR    R14                     RETURN WITH RETURN CODE IN R15   00157900
*                                                                       00158000
         DC    0F'0'                                                    00158100
MSG11    DC    A(KF11)                 LIST WITH   1 ARGUMENT(S)        00158200
@AL00109 DC    A(KF1)                  LIST WITH   1 ARGUMENT(S)        00158300
@AL00112 DC    A(KF2)                  LIST WITH   1 ARGUMENT(S)        00158400
*                                                                       00158500
*        PARAMETER LIST FOR WRITELOG PROC                               00158600
*                                                                       00158700
WRITEPRM DC    A(KF40)                 LIST WITH   2 ARGUMENT(S)        00158800
         DC    A(WRITEPTR)                                              00158900
*                                                                       00159000
DATA0FD4 DC    A(KF3)                                                   00159100
DATA0FD8 DC    A(KF13)                                                  00159200
DATA0FDC DC    A(KF15)                                                  00159300
@AL00193 DC    A(KF10)                 LIST WITH   1 ARGUMENT(S)        00159400
@AL00282 DC    A(KF7)                  LIST WITH   1 ARGUMENT(S)        00159500
@AL00151 DC    A(KF6)                  LIST WITH   1 ARGUMENT(S)        00159600
@AL00180 DC    A(KF8)                  LIST WITH   1 ARGUMENT(S)        00159700
@AL00205 DC    A(KF1944)               LIST WITH   2 ARGUMENT(S)        00159800
         DC    A(WRITEPTR)                                              00159900
@AL00242 DC    A(KF4)                  LIST WITH   1 ARGUMENT(S)        00160000
@AL00255 DC    A(KF5)                  LIST WITH   1 ARGUMENT(S)        00160100
@AL00297 DC    A(KF12)                 LIST WITH   1 ARGUMENT(S)        00160200
DATA1004 DC    A(KF14)                                                  00160300
@AL00442 DC    A(KF9)                  LIST WITH   1 ARGUMENT(S)        00160400
*                                                                       00160500
*        SAVE AREAS                                                     00160600
*                                                                       00160700
@SA00001 DC    18F'0'                                                   00160800
@SA00008 DC    15F'0'                                                   00160900
@PC00008 DC    F'0'                                                     00161000
@SA00002 DC    15F'0'                                                   00161100
@SA00003 DC    15F'0'                                                   00161200
@SA00004 DC    15F'0'                                                   00161300
@SA0000X DC    15F'0'                                                   00161400
@SA00005 DC    15F'0'                                                   00161500
@SA011C8 DC    15F'0'                                                   00161600
@SA00011 DC    15F'0'                                                   00161700
@SA00010 DC    15F'0'                                                   00161800
@SA00007 DC    15F'0'                                                   00161900
@SA00006 DC    15F'0'                                                   00162000
@SA012F4 DC    15F'0'                                                   00162100
@SA00012 DC    15F'0'                                                   00162200
@SA00014 DC    15F'0'                                                   00162300
@SA00015 DC    15F'0'                                                   00162400
@SA00016 DC    15F'0'                                                   00162500
@SA00017 DC    15F'0'                                                   00162600
@SA00009 DC    15F'0'                                                   00162700
@SA00018 DC    15F'0'                                                   00162800
*                                                                       00162900
*        CONSTANTS                                                      00163000
*                                                                       00163100
KF1      DC    F'1'                                                     00163200
KF2      DC    F'2'                                                     00163300
KF3      DC    F'3'                                                     00163400
KF4      DC    F'4'                                                     00163500
KF5      DC    F'5'                                                     00163600
KF6      DC    F'6'                                                     00163700
KF7      DC    F'7'                                                     00163800
KF8      DC    F'8'                                                     00163900
KF9      DC    F'9'                                                     00164000
KF10     DC    F'10'                                                    00164100
KF11     DC    F'11'                                                    00164200
KF12     DC    F'12'                                                    00164300
KF13     DC    F'13'                                                    00164400
KF14     DC    F'14'                                                    00164500
KF15     DC    F'15'                                                    00164600
KF16     DC    F'16'                                                    00164700
KF40     DC    F'40'                                                    00164800
KF90     DC    F'90'                                                    00164900
KH90     EQU   KF90+2                                                   00165000
KF100    DC    F'100'                                                   00165100
KF1944   DC    F'1944'                                                  00165200
*                                                                       00165300
ADVCT    DC    A(0)                    -> IECZDTAB DEVICE ENTRY         00165400
RETCODE  DC    F'0'                                                     00165500
LOGSAVE  DC    F'0'                                                     00165600
READPTR  DC    AL4(BUF1)                                                00165700
WRITEPTR DC    AL4(BUF2)                                                00165800
HIGHRETC DC    F'0'                                                     00165900
TIOTPTR  DC    A(0)                                                     00166000
DDCOUNT  DC    H'0'                                                     00166100
SEQCNT   DC    AL1(0)                                                   00166200
*                                                                       00166300
SRFECB   DC    F'0'                                                     00166400
*                                                                       00166500
KFRAMES  DC    C'FRAMES'                                                00166600
KSERERDS DC    CL8'SERERDS'            SYS1.LOGREC DDNAME               00166700
*                                                                       00166800
*        ENQ MAJOR AND MINOR NAMES                                      00166900
*                                                                       00167000
*        NOTE THAT AN ENQ ON THE MAJOR NAME OF SYSZLOGR WILL RESULT     00167100
*        IN AN 338 ABEND UNLESS ISSUED BY AN AUTHORIZED PROGRAM         00167200
*                                                                       00167300
SYSZLOGR DC    CL8'SYSZLOGR'                                            00167400
LOGRECA  DC    CL8'RECORDER'                                            00167500
*                                                                       00167600
*        SRF CCW COMMNDS                                                00167700
*                                                                       00167800
ENABLE   CCW   SRFENAB,0,SLI,1                                          00167900
*                                                                       00168000
DISABLE  CCW   SRFDIS,0,SLI,1                                           00168100
*                                                                       00168200
RDCPUID  CCW   SRFRDCPU,STCPUID,SLI,10                                  00168300
*                                                                       00168400
STCPUID  DC    XL11'00000000000000000000F0'                             00168500
         DC    0D'0'                                                    00168600
STCPUVER DC    X'00'                                                    00168700
STCPUSER DC    X'000000'                                                00168800
STCPUMOD DC    X'0000'                                                  00168900
STCPUMCL DC    X'0000'                                                  00169000
*                                                                       00169100
ALLZERO  DC    CL11'00000000000'                                        00169200
*                                                                       00169300
VERBAGE  CCW   SRFVERB,FRAMTYPS,SLI,1                                   00169400
*                                                                       00169500
FRAMTYPS DC    X'02'                                                    00169600
*                                                                       00169700
RDVERB   CCW   SRFREAD,0,SLI,1920                                       00169800
*                                                                       00169900
SWITCH   DC    X'0000'                                                  00170000
FRAMEYES EQU   X'80'                                                    00170100
FRAMLOST EQU   X'40'                                                    00170200
ONEFRAME EQU   X'20'                                                    00170300
*                                                                       00170400
*        SYSTEM REFERENCE FILE IOB                                      00170500
*                                                                       00170600
SRFIOB   DC    4D'0'                   STANDARD NON DASD IOB            00170700
*                                                                       00170800
         ORG   SRFIOB-16               ALLOW FOR IOB PREFIX             00170900
         PRINT NOGEN                                                    00171000
         IEZIOB DSECT=NO                                                00171100
         PRINT GEN                                                      00171200
*        INITIALIZE SRF IOB FIELDS                                      00171300
         ORG   IOBECBPB                                                 00171400
         DC    AL3(SRFECB)             INIT IOB ECB ADDR                00171500
         ORG   IOBSTART                                                 00171600
         DC    AL4(ENABLE)             INIT CHANNEL PROG ADDR           00171700
         ORG   IOBDCBPB                                                 00171800
         DC    AL3(SRFDCB)             INIT IOB DCB ADDR                00171900
         ORG   SRFIOB+32               RESET COUNTER AFTER IOB MAP      00172000
*                                                                       00172100
BUF1     DS    CL1944                                                   00172200
BUF2     DS    CL1944                                                   00172300
         ORG   BUF2                                                     00172400
BUF240   DS    CL40                                                     00172500
         ORG   BUF2+1944                                                00172600
RECBUILD DS    CL24                                                     00172700
         ORG   RECBUILD                                                 00172800
RECTYPE  DC    X'A0'                                                    00172900
RECSYS   DC    X'00'                                                    00173000
RECSW0   DC    X'8800'                                                  00173100
         ORG   RECSW0                                                   00173200
RECNLST  DS    BL1                                                      00173300
         ORG   RECBUILD+4                                               00173400
@NM00005 DC    X'0000'                                                  00173500
RECCNT   DC    AL1(0)                                                   00173600
@NM00006 DC    X'00'                                                    00173700
RECDAT   DC    X'00000000'                                              00173800
RECTIME  DC    X'00000000'                                              00173900
RECCPU   DC    X'0000000000000000'                                      00174000
         ORG   RECBUILD+24                                              00174100
HOLD     DC    XL8'00'                                                  00174200
MBBCCHHR DS    CL8                                                      00174300
         ORG   MBBCCHHR                                                 00174400
M        DS    CL1                                                      00174500
BBCCHHR  DS    CL7                                                      00174600
         ORG   BBCCHHR                                                  00174700
BB       DS    CL2                                                      00174800
CCHHR    DS    CL5                                                      00174900
         ORG   MBBCCHHR+8                                               00175000
*                                                                       00175100
FRAMEDD  DC    XL8'0000000000000000'                                    00175200
FRFLAG   DC    X'00'                                                    00175300
FRFLAGY  EQU   X'80'                   GOT FRAMEXX DD                   00175400
*                                                                       00175500
*********************************************************************** 00175600
*                                                                       00175700
*        DEVICE TABLE                                                   00175800
*                                                                       00175900
*        DEVICE NAMES CURRENTLY SUPPORTED AS SYSTEM RESIDENCE           00176000
*        DEVICES                                                        00176100
*                                                                       00176200
*********************************************************************** 00176300
*                                                                       00176400
*        THIS TABLE IS ONLY USED FOR PURPOSES OF IDENTIFING THE         00176500
*        UNIT DEVICE NAME IN MSG IFC001I                                00176600
*                                                                       00176700
*              DEVICE NAME         DEVTYP                               00176800
DASDTAB  DC    CL4'2311'           X'01'                                00176900
         DC    CL4'2301'           X'02'                                00177000
         DC    CL4'2303'           X'03'                                00177100
         DC    CL4'9345'           X'04'    REPURPOSED FROM 2302        00177200
         DC    CL4'2321'           X'05'                                00177300
         DC    CL4'2351'           X'06'                                00177400
         DC    CL4'2352'           X'07'                                00177500
         DC    CL4'2314'           X'08'                                00177600
         DC    CL4'3330'           X'09'                                00177700
         DC    CL4'3340'           X'0A'                                00177800
         DC    CL4'3350'           X'0B'                                00177900
         DC    CL4'3375'           X'0C'                                00178000
         DC    CL4'3331'           X'0D'                                00178100
         DC    CL4'3380'           X'0E'                                00178200
         DC    CL4'3390'           X'0F'                                00178300
*                                                                       00178400
*********************************************************************** 00178500
*                                                                       00178600
*        LOGREC HEADER RECORD                                           00178700
*                                                                       00178800
*********************************************************************** 00178900
*                                                                       00179000
         CNOP  2,4                                                      00179100
HDRREC   DC    X'FFFF'             +00 RECORD ID                        00179200
HDRSTRCC DC    XL4'00000000'       +02 START OF LOGREC EXTENT CCHH      00179300
HDRENDCC DC    XL4'00000000'       +06 END OF LOGREC EXTENT CCHH        00179400
HDRMSGCT DC    XL1'00'             +10 COUNT OF LOGREC FULL MSG         00179500
HDRREA   DC    XL7'00000000000000' +11 RECORD ENTRY AREA (BBCCHHR)      00179600
*                                      START OF LOGREC RECORDS          00179700
HDRTBAL  DC    AL2(0)              +18 TRACK BALANCE                    00179800
HDRTRKLN DC    AL2(0)              +20 NO OF BYTES PER TRACK            00179900
HDRLASTR DC    XL7'00000000000000' +22 LAST RECORD WRITTEN (BBCCHHR)    00180000
HDRTRK   DC    AL2(0)              +29 TRKS PER CYL(HIGHEST TRK ON CYL) 00180100
HDREWTC  DC    AL2(0)              +31 EARLY WARNING TRACK CAPACITY     00180200
*                                      SET AT 90% OF LAST TRACK         00180300
HDRDEVT  DC    X'00'               +33 UNIT DEVICE TYPE (LOW NIBBLE)    00180400
HDREWMT  DC    XL4'00000000'       +34 EARLY WARNING TRK (CCHH)         00180500
*                                      SET AT 90% OF ALL TRACKS         00180600
HDREWSW  DC    X'00'               +38 EARLY WARNING SWITCH             00180700
HDREWMI  EQU   X'80'                   SET WHEN EARLY WARNING MSG       00180800
*                                      IFB060E HAS BEEN ISSUED TO OPER  00180900
HDRFRSW  EQU   X'20'                   FRAMES PRESENT IN LOGREC         00181000
*                                                                       00181100
HDRCHK   DC    X'FF'               +39 CHECK BYTE                       00181200
HDRRECL  EQU   *-HDRREC                L'HDRREC                         00181300
*                                                                       00181400
*********************************************************************** 00181500
*                                                                       00181600
*        SYS1.LOGREC DATASET DCB                                        00181700
*                                                                       00181800
*********************************************************************** 00181900
*                                                                       00182000
SERERDS  DCB   DDNAME=SERERDS,                                         X00182100
               DSORG=PS,                                               X00182200
               MACRF=(RP,WP),                                          X00182300
               RECFM=U,                                                X00182400
               BLKSIZE=1944,                                           X00182500
               DEVD=DA,                                                X00182600
               EXLST=EXITJFCB,                                         X00182700
               SYNAD=LOGERR                                             00182800
*                                                                       00182900
*********************************************************************** 00183000
*                                                                       00183100
*        SYSTEM REFERENCE FILE DCB                                      00183200
*                                                                       00183300
*********************************************************************** 00183400
*                                                                       00183500
SRFDCB   DCB   MACRF=(E),                                              X00183600
               IOBAD=SRFIOB,                                           X00183700
               DSORG=PS,                                               X00183800
               DEVD=DA,                                                X00183900
               RECFM=U,                                                X00184000
               DDNAME=FRAMES                                            00184100
*                                                                       00184200
*********************************************************************** 00184300
*                                                                       00184400
*        LOGREC DATASET DECB                                            00184500
*                                                                       00184600
*********************************************************************** 00184700
*                                                                       00184800
         WRITE LISTDECB,SF,SERERDS,HDRREC,HDRRECL,,,,MF=L               00184900
*                                                                       00185000
*        SERERDS DCB EXLST                                              00185100
*                                                                       00185200
         DC    0F'0'                                                    00185300
EXITJFCB DC    X'87'                   REQUEST RDJFCB                   00185400
         DC    AL3(JFCB)               ADDR OF JFCB                     00185500
*                                                                       00185600
WORKDW   DC    D'0'                    TEMP WORK AREA                   00185700
SAVENDCC DC    XL4'00'                 SAVED ENDCC FOR DCBFDAD          00185800
SAVTRKLN DC    H'0'                    SAVE TRACK LEN FOR DCBTRBAL      00185900
DEVTYPE  DC    X'00'                   DEVICE TYPE STORAGE              00186000
HEXTAB   DC    C'0123456789ABCDEF'     CONVERSION TABLE                 00186100
BYTPFRM  DC    F'2500'                 BYTES IN FRAME PLUS GUESSTIMATED 00186200
*                                      TRACK OVERHEAD (VERY APROXIMATE) 00186300
ARBSMALL DC    X'0014'                 SET TRKBAL SO NO RECORD WILL FIT 00186400
TTRSAVE  DC    F'0'                                                     00186500
TTRLST   DC    F'0'                    TTR OF LAST FRAME                00186600
MYSAVE   DC    5F'0'                   TRACK CONVERSION RTN SAVEAREA    00186700
MAXTTR   DC    X'00000000'  PLACE MAXIMUM RELATIVE LOCATION OF LOGREC   00186800
THIRD    DC    X'00000200'  TTR BEFORE 1ST FRAME. USED TO DETERMINE     00186900
*                           WHICH ERROR MESSAGE TO ISSUE. IF WRITE      00187000
*                           FAILS, NOTE WILL NOT BE EXECUTED AND TTR    00187100
*                           IN TTRSAVE WILL BE TTR FOR LAST RECORD OR   00187200
*                           ZERO IF HEADER WRITE FAIL.                  00187300
         DC    0D'0'                                                    00187400
JFCB     DC    XL176'00'                                                00187500
*                                                                       00187600
         ORG   JFCB                                                     00187700
*                                                                       00187800
*        PRINT NOGEN                                                    00187900
*                                                                       00188000
         IEFJFCBN                                                       00188100
*                                                                       00188200
         ORG   ,                                                        00188300
*                                                                       00188400
*        UCB DEVICE CHARACTERISTICS                                     00188500
*                                                                       00188600
DVADATA  DC    5F'0'                                                    00188700
         ORG   DVADATA                                                  00188800
*                                                                       00188900
         IHADVA DSECT=NO           MAP DEVTYPE DATA                     00189000
*                                                                       00189100
         ORG   ,                                                        00189200
*                                                                       00189300
         LTORG                                                          00189400
*                                                                       00189500
         PRINT GEN                                                      00189600
*                                                                       00189700
*        CCW COMMAND CODE EQUATES FOR DISK I/O                          00189800
*                                                                       00189900
CCWNOOP  EQU   X'03'               NO OPERATION                         00190000
CCWTIC   EQU   X'08'               TRANSFER IN CHANNEL                  00190100
SEEK     EQU   X'07'               SEEK                                 00190200
SEEKC    EQU   X'0B'               SEEK CYL                             00190300
SEEKH    EQU   X'1B'               SEEK HEAD                            00190400
SEARKEQ  EQU   X'29'               SEARCH KEY EQUAL                     00190500
SEARKHE  EQU   X'69'               SEARCH KEY HIGH OR EQUAL             00190600
READHA   EQU   X'1A'               READ HOME ADDRESS                    00190700
READR0   EQU   X'16'               READ RECORD 0                        00190800
READC    EQU   X'12'               READ COUNT                           00190900
READDATA EQU   X'06'               READ DATA                            00191000
READKD   EQU   X'0E'               READ KEY AND DATA                    00191100
READCKD  EQU   X'1E'               READ COUNT, KEY AND DATA             00191200
SETSECT  EQU   X'23'               SET SECTOR                           00191300
SETFM    EQU   X'1F'               SET FILE MASK                        00191400
READSECT EQU   X'22'               READ SECTOR                          00191500
SEARIDEQ EQU   X'31'               SEARCH IDENTIFIER EQUAL              00191600
*                                                                       00191700
SENSE    EQU   X'04'               SENSE                                00191800
WRITEDAT EQU   X'05'               WRITE DATA                           00191900
WRITEKD  EQU   X'0D'               WRITE KEY AND DATA                   00192000
WRITECKD EQU   X'1D'               WRITE COUNT, KEY, AND DATA           00192100
*                                                                       00192200
READEVCH EQU   X'64'               READ DEVICE CHARACTERISTICS          00192300
*                                  (64 BYTES)                           00192400
READMCKD EQU   X'5E'               READ MULTIPLE COUNT, KEY, AND DATA   00192500
MT       EQU   X'80'               'MULTI-TRACK' USE WITH APPROPRIATE   00192600
*                                  OPCODES AS -  CCW   SEARKHE+MT,B,C,D 00192700
*                                                                       00192800
*        CCW COMMAND CODE EQUATES FOR                                   00192900
*        IBM 7443 SERVICE RECORD FILE (SRF)                             00193000
*                                                                       00193100
SRFENAB  EQU   X'A3'                                                    00193200
SRFDIS   EQU   X'C3'                                                    00193300
SRFRDCPU EQU   X'FE'                                                    00193400
SRFVERB  EQU   X'69'                                                    00193500
SRFREAD  EQU   X'32'                                                    00193600
*                                                                       00193700
*        CCW FLAG EQUATES                                               00193800
*                                                                       00193900
CD       EQU   X'80'               CHAIN DATA                           00194000
CC       EQU   X'40'               CHAIN COMMAND                        00194100
SLI      EQU   X'20'               SUPPRESS INCORRECT LENGTH INDICATION 00194200
SKIP     EQU   X'10'               SKIP TRANSFER OF DATA                00194300
*                                                                       00194400
*        CSW FLAG EQUATES                                               00194500
*                                                                       00194600
*        UNIT STATUS BYTE                                               00194700
*                                                                       00194800
CUE      EQU   X'20'               CONTROL UNIT END                     00194900
CUB      EQU   X'10'               CONTROL UNIT BUSY                    00195000
CE       EQU   X'08'               CHANNEL END                          00195100
DE       EQU   X'04'               DEVICE END                           00195200
UC       EQU   X'02'               UNIT CHECK                           00195300
UE       EQU   X'01'               UNIT EXCEPTION                       00195400
*                                                                       00195500
*        CHANNEL STATUS BYTE                                            00195600
*                                                                       00195700
PCI      EQU   X'80'               PROGRAM CONTROLLED INTERRUPT         00195800
IL       EQU   X'40'               INCORRECT LENGTH                     00195900
PC       EQU   X'20'               PROGRAM CHECK                        00196000
PROTC    EQU   X'10'               PROTECTION CHECK                     00196100
CDC      EQU   X'08'               CHANNEL DATA CHECK                   00196200
CCC      EQU   X'04'               CHANNEL CONTROL CHECK                00196300
ICC      EQU   X'02'               INTERFACE CONTROL CHECK              00196400
CHK      EQU   X'01'               CHAINING CHECK                       00196500
*                                                                       00196600
*        REGISTER EQUATES                                               00196700
*                                                                       00196800
R0       EQU   0                                                        00196900
R1       EQU   1                                                        00197000
R2       EQU   2                                                        00197100
R3       EQU   3                                                        00197200
R4       EQU   4                                                        00197300
R5       EQU   5                                                        00197400
R6       EQU   6                                                        00197500
R7       EQU   7                                                        00197600
R8       EQU   8                                                        00197700
R9       EQU   9                                                        00197800
R10      EQU   10                                                       00197900
R11      EQU   11                                                       00198000
R12      EQU   12                                                       00198100
R13      EQU   13                                                       00198200
R14      EQU   14                                                       00198300
R15      EQU   15                                                       00198400
*                                                                       00198500
         PRINT NOGEN                                                    00198600
*                                                                       00198700
*        IEZDEB                                                         00198800
*                                                                       00198900
         IEZDEB                                                         00199000
*                                                                       00199100
*        IHADVCT - DEVICE CHARACTERISTICS TABLE                         00199200
*                                                                       00199300
         IHADVCT                                                        00199400
*                                                                       00199500
*        UCB                                                            00199600
*                                                                       00199700
UCB      DSECT                                                          00199800
*                                                                       00199900
         IEFUCBOB                                                       00200000
*                                                                       00200100
*        CVT                                                            00200200
*                                                                       00200300
         CVT   DSECT=YES,PREFIX=YES                                     00200400
*                                                                       00200500
*        TCB                                                            00200600
*                                                                       00200700
         IKJTCB                                                         00200800
*                                                                       00200900
*        TIOT                                                           00201000
*                                                                       00201100
TIOT     DSECT                                                          00201200
*                                                                       00201300
         IEFTIOT1                                                       00201400
*                                                                       00201500
*        DCB                                                            00201600
*                                                                       00201700
         DCBD  DSORG=PS,DEVD=(DA)                                       00201800
*                                                                       00201900
*        ECB                                                            00202000
*                                                                       00202100
         IHAECB                                                         00202200
*                                                                       00202300
         PRINT GEN                                                      00202400
*                                                                       00202500
         END   IFCDIP00                                                 00202600
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IFCDIP00('ZP60036')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60036)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60036)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60036)
        DIS(WRITE)
        .
/*
//
