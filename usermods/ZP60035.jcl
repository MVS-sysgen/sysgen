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
