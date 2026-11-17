//RP00003  JOB (SYSGEN),'RP00003',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* INSTALL USERMOD RP00003 - DEVTYPE SVC RETURNS THE ACTUAL NUMBER   *
//* OF CYLINDERS OF 3375/3380/3390 (ROB PRINS)                        *
//*                                                                   *
//* FROM JAY MOSELEY'S INSTALLMVS V8 TAPE/USERMODS.HET                *
//*********************************************************************
//*
//SOURCE   EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  DATA,DLM='??'
./ ADD NAME=IGC0002D
SVC24    TITLE 'IGC0002D - DEVTYPE SVC ROUTINE'                         00000100
*                                                                       00000200
IGC0002D CSECT                                                          00000300
*                                                                       00000400
*        STATUS -                                                       00000500
*        LASTUPD         = JCLIN    TYPE=UPD                            00000600
*        LIBRARIES       = DISTLIB=AOSD0                                00000700
*        FMID            = EDM1102                                      00000800
*        RMID            = UZ73568                                      00000900
*        LMOD            = IGC0002D                                     00001000
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS ---STATUS---  00001100
*                          EDM1102  FUNCTION  00.000  MOD      ACC RGN  00001200
*                          UZ73568  PTF       74.164  MOD APP  ACC      00001300
*                                                                       00001400
*        SOURCE WAS CREATED BY DISASSEMBLY TO MATCH THE                 00001500
*        LOAD MODULE AT THE UZ73568 LEVEL                               00001600
*                                                                       00001700
*        CHANGE ACTIVITY -                                              00001800
*               RP00003 - 1. RETURN THE CORRECT NUMBER OF CYLINDERS     00001900
*                            IN THE DVACYL RESULT FIELD FOR 3380        00002000
*                            AND 3390 EXTENDED DASD MODELS WHERE THE    00002100
*                            NUMBER OF CYLINDERS IS GREATER THAN THE    00002200
*                            BASE MODEL OF THE 3380/3390 DEVICE         00002300
*                         2. NUMEROUS CHANGES TO IMPROVE SOURCE         00002400
*                            READABILITY AND USE OF SYSTEM MAPPING      00002500
*                            MACROS                                     00002600
*                                                                       00002700
*        NOTE -              ANY CHANGE ACTIVITY THAT IMPACTS THE       00002800
*                            LENGTH OF THE GETMAINED AREA MUST BE       00002900
*                            REFLECTED IN THE RECOVERY MODULE           00003000
*                            IGCT002D.                                  00003100
*        FUNCTION -                                                     00003200
*        THIS SVC ROUTINE LOCATES AND PASSES TO THE REQUESTOR           00003300
*        THE DEVICE CHARACTERISTICS OF THE DEVICE SPECIFIED BY          00003400
*        THE DD NAME PROVIDED.                                          00003500
*                                                                       00003600
*        OUTPUT -                                                       00003700
*        FOR ALL DEVICES -                                              00003800
*        4 BYTE UCB TYPE FIELD AND A 4 BYTE MAXIMUM BLOCK SIZE FIELD    00003900
*        FOR DASD DEVICES -                                             00004000
*        A 12 BYTE CHARACTERISTICS TABLE ENTRY                          00004100
*                                                                       00004200
*        SVC REGISTER CONVENTIONS -                                     00004300
*                                                                       00004400
*        ON ENTRY -                                                     00004500
*        R0 - IF POSITIVE ON ENTRY (DEVTAB NOT SPECIFIED),              00004600
*             -> 8 BYTE FIELD FOR OUTPUT.                               00004700
*             IF NEGATIVE ON ENTRY (DASD DEVICE AND DEVTAB SPECIFIED),  00004800
*             -> 20 BYTE FIELD FOR OUTPUT                               00004900
*        R1 - -> FIELD CONTAINING DD NAME.                              00005000
*             IF IN COMPLEMENT FORM THE RPS WORD WILL BE INCLUDED       00005100
*             THE REGISTER WILL BE IN NORMAL FORM ON EXIT               00005200
*        R2     UNDEFINED                                               00005300
*        R3  -> CVT                                                     00005400
*        R4  -> TCB                                                     00005500
*        R5  -> SVRB                                                    00005600
*        R6  -> SVC ENTRY POINT                                         00005700
*        R7  -> ASCB                                                    00005800
*        R8-R12 UNDEFINED                                               00005900
*        R13    CALLERS VALUE                                           00006000
*        R14    RETURN ADDR TO SVC FLIH EXIT CODE                       00006100
*        R15    CALLERS VALUE                                           00006200
*                                                                       00006300
*        OPERATION -                                                    00006400
*        THE TIOT FOR THE TCB -> R4 UPON ENTRY IS LOCATED AND           00006500
*        THE DD ENTRIES IN IT ARE COMPARED TO THE INPUT DD NAME.        00006600
*        WHEN A MATCH OCCURS, THE UCB FOR THE DD IS LOCATED AND         00006700
*        ITS TYPE FIELD IS COPIED INTO THE OUTPUT AREA.                 00006800
*        IF IT IS A DIRECT ACCESS DEVICE, AND DEVTAB IS                 00006900
*        SPECIFIED, THE DEVICE CHARACTERISTICS TABLE IS LOCATED         00007000
*        USING CVT AND THE APPROPRIATE 12 BYTE ENTRY IS COPIED          00007100
*        INTO THE USERS 20 BYTE OUTPUT AREA.                            00007200
*                                                                       00007300
*        3340, 3380 AND 3390 DASD MODELS CAN HAVE AN INCREASED          00007400
*        NUMBER OF CYLINDERS THAT IS NOT REFLECTED IN THE UCB           00007500
*        UCBTBYT4 FIELD WHERE THE SAME DEVICE CODE IS ALLOCATED         00007600
*        TO BASE AND EXTENDED MODELS. TO DETERMINE THE ACTUAL           00007700
*        NUMBER OF CYLINDERS FOR SUCH DEVICES ADDITIONAL                00007800
*        INFORMATION MUST BE OBTAINED FROM THE ACTUAL DEVICE. FOR       00007900
*        3340 DASD A CCW SENSE COMMAND TO THE DEVICE PROVIDES           00008000
*        A FLAG TO IDENTIFY A MODEL 35 OR MODEL 70 DEVICE. THE          00008100
*        NUMBER OF CYLINDERS IS THEN SET ACCORDINGLY.                   00008200
*        FOR 3380 AND 3390 A READ DEVICE CHARACTERISTICS CCW            00008300
*        COMMAND RETURNS THE ACTUAL NUMBER OF PRIMARY CYLINDERS         00008400
*        AVAILABLE ON THE DEVICE.                                       00008500
*                                                                       00008600
*        IF THE I/O REQUEST TO THE DEVICE DOES NOT COMPLETE             00008700
*        SUCESSSFULLY THEN THE NUMBER OF CYLINDERS IS SET TO THE        00008800
*        NUMBER PROVIDED WITH THE BASE MODEL AND THE DVABDCYL           00008900
*        FLAG IN THE DVAFLAGS BYTE IS SET ON                            00009000
*                                                                       00009100
*        IF THE APPROPRIATE DD ENTRY IS NOT FOUND IN THE TIOT,          00009200
*        AN ERROR CODE, 04, IS PLACED IN R15 AND RETURN IS MADE         00009300
*        TO THE USER.                                                   00009400
*                                                                       00009500
R0       EQU   0                        PASSING PARAMETER REGISTER      00009600
R1       EQU   1                        PASSING PARAMETER REGISTER      00009700
R2       EQU   2                        -> CALLERS RESULT AREA          00009800
R3       EQU   3                        -> CVT                          00009900
R4       EQU   4                        -> TCB                          00010000
R5       EQU   5                        -> RB, WORK REGISTER            00010100
R6       EQU   6                        BASE REGISTER                   00010200
R7       EQU   7                        -> TIOT                         00010300
R8       EQU   8                        -> UCB                          00010400
R9       EQU   9                        WORK REGISTER                   00010500
R10      EQU   10                       WORK REGISTER                   00010600
R11      EQU   11                       INTERNAL LINK REGISTER          00010700
R12      EQU   12                       WORK REGISTER                   00010800
R13      EQU   13                                                       00010900
R14      EQU   14                                                       00011000
R15      EQU   15                       RETURN CODE REGISTER            00011100
*                                                                       00011200
         USING *,R6                     SET ON ENTRY FROM SVC FLIH      00011300
         USING CVT,R3                                                   00011400
         USING TCB,R4                                                   00011500
         USING RBBASIC,R5                                               00011600
         USING DVAREA,R2                R2 -> CALLERS RESULT AREA       00011700
         B     BEGIN                    BRANCH AROUND MODULE ID         00011800
*                                                                       00011900
         DC    C'IGC0002D @RP00003 &SYSDATE &SYSTIME'                   00012000
*                                                                       00012100
BEGIN    MVC   RBEXSAVE+4(ESTAELX-ESTAEL),ESTAEL                        00012200
         XC    RBEXSAVE(4),RBEXSAVE     USED FOR -> GETMAINED STORAGE   00012300
         LR    R10,R14                  SAVE RETURN ADDR                00012400
*                                                                       00012500
         ESTAE ,CT,MF=(E,RBEXSAVE+4),PARAM=(R5)                         00012600
*                                                                       00012700
*        CHANGE TO CALLERS KEY                                          00012800
*                                                                       00012900
         MODESET EXTKEY=RBT234,WORKREG=9                                00013000
*                                                                       00013100
         LR    R14,R10                  RESTORE EXIT ADDR               00013200
         LM    R0,R1,XRBREG0            PICKUP CALLERS R0,R1            00013300
         LPR   R2,R0                    MAKE PARAMETER REGS +VE         00013400
         LPR   R1,R1                                                    00013500
         CLI   0(R1),X'00'              FIRST BYTE OF DDNAME ZERO ?     00013600
         BE    ABNRTN                   YES, ERROR EXIT                 00013700
         L     R7,TCBTIO                R7 -> TIOT                      00013800
         LTR   R7,R7                    VALID TIOT ADDR ?               00013900
         BZ    ABNRTN                   NO, ABNORMAL RETURN             00014000
         USING TIOT,R7                  ADDRESS TIOT                    00014100
         LA    R7,TIOENTRY              R7 -> FIRST DD ENTRY            00014200
         USING TIOENTRY,R7              MAP TIOT ENTRY                  00014300
*                                                                       00014400
*        SCAN TIOT LOOP                                                 00014500
*                                                                       00014600
DV002    TM    0(R7),X'FF'              END OF DD ENTRIES ?             00014700
         BZ    ABNRTN                   YES, DDNAME NOT FOUND           00014800
         CLC   0(L'TIOEDDNM,R1),TIOEDDNM   DDNAME MATCH ?               00014900
         BE    DV0025                   YES, BRANCH                     00015000
         SR    R9,R9                    NO, CLEAR WORK REG              00015100
         IC    R9,TIOELNGH              GET L'TIOT ENTRY                00015200
         LA    R7,0(R9,R7)              INCR TO NEXT DDNAME ENTRY       00015300
         B     DV002                    GOTO BEGIN LOOP                 00015400
*                                                                       00015500
*        FOUND TIOT DD NAME ENTRY                                       00015600
*                                                                       00015700
DV0025   TM    TIOELINK,TIOTTERM        TSO TERMINAL ?                  00015800
         BO    DVTSODM                  YES, BRANCH                     00015900
         TM    TIOELINK,TIOESYIN+TIOESYOT   JES2 DATA SET ?             00016000
         BZ    DV003                    NO, BRANCH                      00016100
         MVI   DVAUNIT,X'02'            YES, SYSIN, SYSOUT, SUBSYS      00016200
         B     DV0028                   BRANCH TO SET UP REST OF        00016300
*                                       OUTPUT                          00016400
DVTSODM  MVI   DVAUNIT,X'01'            UNIT TYPE = TSO                 00016500
*                                                                       00016600
DV0028   MVI   DVACLASS,X'01'           SIMULATED DEVICE, NO UCB        00016700
         XC    DVAOPTS,DVAOPTS          OPTION BYTES                    00016800
         MVC   DVAMAXRC,KF32760         MAXIMUM RECORD SIZE             00016900
         B     DVNMEXT                  BRANCH TO EXIT                  00017000
*                                                                       00017100
*        STANDARD NON TSO, NON JES DATA SET                             00017200
*                                                                       00017300
DV003    L     R8,TIOESTTB              R8 -> UCB                       00017400
         LA    R8,0(,R8)                CLEAR HI-ORDER BYTE OF UCB ADDR 00017500
         LTR   R8,R8                    UCB ADDRESS ZERO ?              00017600
         BZ    DVDUMMY                  YES, BRANCH NO UCB              00017700
         SR    R9,R9                                                    00017800
         IC    R9,TIOELNGH              R9 = L'THIS ENTRY               00017900
         AR    R9,R7                                                    00018000
         TM    0(R9),X'FF'                                              00018100
         BZ    DV003A                   YES - BRANCH                    00018200
         CLI   4(R9),X'40'                                              00018300
         BNE   DV003A                                                   00018400
         SR    R9,R9                                                    00018500
         L     R9,TCBDEB                R9 -> DEB QUEUE                 00018600
         USING DEBBASIC,R9                                              00018700
IGC000E8 LTR   R9,R9                    DEB ADDR ZERO ?                 00018800
         BZ    DV003A                   YES, BRANCH                     00018900
         L     R10,DEBDCBAD             R10 -> DCB FROM DEB             00019000
         USING IHADCB,R10                                               00019100
         TM    DCBOFLGS,DCBOFOPN+DCBOFPPC  DCB OPEN ?                   00019200
         BNO   IGC0013C                 NO, BRANCH                      00019300
         LH    R10,DCBTIOT              R10 = TIOT OFFSET               00019400
         A     R10,TCBTIO               ADD -> TIOT                     00019500
         CR    R10,R7                   THIS DCB -> TIOT ENTRY ?        00019600
         BL    IGC0013C                 NO LOW, BRANCH                  00019700
         BE    IGC00130                 YES, MATCH, BRANCH              00019800
         LR    R12,R7                   HIGH                            00019900
IGC0010E SR    R15,R15                                                  00020000
         IC    R15,0(,R12)              R15 = TIOELNGH                  00020100
         LA    R12,0(R15,R12)                                           00020200
         TM    0(R12),X'FF'                                             00020300
         BZ    IGC0013C                                                 00020400
         CLI   4(R12),X'40'                                             00020500
         BNE   IGC0013C                                                 00020600
         CR    R10,R12                                                  00020700
         BNE   IGC0010E                                                 00020800
         LR    R7,R12                                                   00020900
IGC00130 L     R8,TIOESTTB              R8 -> UCB                       00021000
         LA    R8,0(,R8)                CLEAR HI-ORDER BYTE OF UCB ADDR 00021100
         B     DV003A                                                   00021200
*                                                                       00021300
         DROP  R7                       TIOT NO LONGER NEEDED           00021400
*                                                                       00021500
*        TIOT ENTRY VIA DEB/DCB NOT MATCH DDNAME                        00021600
*                                                                       00021700
IGC0013C L     R9,4(,R9)                INCR DEB QUEUE ADDR             00021800
         LA    R9,0(,R9)                CLEAR HI-ORDER BYTE             00021900
         B     IGC000E8                 TEST NEXT DEB TABLE ENTRY       00022000
*                                                                       00022100
         DROP  R9                                                       00022200
*                                                                       00022300
*        FOUND UCB FOR PROVIDED DDNAME                                  00022400
*                                                                       00022500
         USING UCB,R8                                                   00022600
DV003A   MVC   DVAUCBTY,UCBTYP          MOVE TYPE FIELD TO OUTPUT AREA  00022700
         TM    UCBTBYT3,UCB3DACC        DASD DEVICE ?                   00022800
         BO    DVDA                     YES, BRANCH TO DASD PROCESSING  00022900
         L     R12,KF32760              NO, SET RECORD SIZE FOR TAPE    00023000
         TM    UCBTBYT3,UCB3TAPE        TAPE DEVICE ?                   00023100
         BO    DV004                    YES, GO PROCESS                 00023200
         CLI   UCBTBYT3,UCB3DISP        GRAPHICS ?                      00023300
         BE    DVNMEXT                  YES - BRANCH TO RETURN          00023400
         CLI   UCBTBYT3,UCB3COMM        COMMUNICATIONS ?                00023500
         BE    DVNMEXT                  YES - BRANCH TO RETURN          00023600
         CLI   UCBTBYT3,UCB3CTC         CTC ?                           00023700
         BE    DVNMEXT                  YES - BRANCH TO RETURN          00023800
         TM    UCBTBYT3,UCB3UREC        UNIT RECORD ?                   00023900
         BZ    ABNRTN                   NO, DEVICE TYPE IS NOT          00024000
*                                       SUPPORTED BY THIS MACRO         00024100
*                                                                       00024200
*        UNIT RECORD PROCESSING                                         00024300
*                                                                       00024400
         LA    R12,80                   RECORD SIZE FOR CARD DEVICES    00024500
*                                                                       00024600
*        CARD READER/PUNCH UNITS ARE ASSIGNED IN THE                    00024700
*        RANGE X'01'-X'06' EXCEPT FOR THE 3525                          00024800
*                                                                       00024900
         CLI   UCBTBYT4,X'06'           CARD RDR/PCH DEVICES ?          00025000
         BNH   DV004                    YES, GO UPDATE                  00025100
         CLI   UCBTBYT4,X'0C'           SPECIFIC TEST FOR 3525          00025200
         BE    DV004                    YES, GO UPDATE                  00025300
         LA    R12,136                  RECORD SIZE FOR 3800            00025400
         CLI   UCBTBYT4,X'0E'           3800 LASER PRINTER              00025500
         BE    DV004                                                    00025600
         LA    R12,132                  RECORD SIZE FOR 132 PRINTERS    00025700
         CLI   UCBTBYT4,X'09'           3211 ?                          00025800
         BE    DV004                    YES, GO UPDATE                  00025900
         CLI   UCBTBYT4,X'0B'           3203 ?                          00026000
         BE    DV004                    YES, GO UPDATE                  00026100
         LA    R12,120                  RECORD SIZE FOR 120 PRINTERS    00026200
         CLI   UCBTBYT4,X'0A'           1442/3 1403 ?                   00026300
         BNH   DV004                    YES, GO UPDATE                  00026400
         LA    R12,74                   SET RECORD SIZE                 00026500
         CLI   UCBTBYT4,X'19'           3895 ?                          00026600
         BE    DV004                    YES, GO UPDATE                  00026700
         L     R12,KF32760              SET RECORD SIZE TO 32760        00026800
         CLI   UCBTBYT4,X'10'           2671 TAPE TAPE ?                00026900
         BE    DV004                    YES, GO UPDATE                  00027000
         LA    R12,120                  SET RECORD SIZE TO 120          00027100
         TM    UCBTBYT4,X'20'           1052/3210/3213/3215             00027200
         BO    DV004                    YES, GO UPDATE                  00027300
         SR    R12,R12                  UNKNOWN UNIT RECORD DEVICE      00027400
         B     DV004                                                    00027500
*                                                                       00027600
*        DASD PROCESSING                                                00027700
*                                                                       00027800
DVDA     L     R9,CVTZDTAB              R9 -> DVCT                      00027900
         IC    R10,UCBTBYT4             INDEX BYTE FROM UCB TYPE FIELD  00028000
         N     R10,DVMASK2              AND OUT ALL BUT LOW ORDER 4 BIT 00028100
         IC    R10,0(R9,R10)            INDEX BYTE FROM HEAD            00028200
*                                       OF TABLE.                       00028300
         AR    R9,R10                   ADD INDEX TO ADDRESS            00028400
         LTR   R0,R0                    DEVTAB OUTPUT REQUESTED ?       00028500
         BP    DVDA1                    NO, SKIP                        00028600
         MVC   DVATAB,0(R9)             MOVE DEVICE CHARACTERISTICS     00028700
         L     R1,XRBREG1               R1 = CALLERS R1                 00028800
         LTR   R1,R1                    RPS INFO REQUESTED ?            00028900
         BP    DVDA1                    NO, BRANCH                      00029000
         TM    UCBTBYT2,UCBRPS          DEVICE SUPPORT RPS ?            00029100
         BNO   DVDA1                    NO, BRANCH                      00029200
         MVC   DVARPS,14(R9)            YES, MOVE IN RPS DATA           00029300
DVDA1    LR    R12,R9                   SAVE DVCT ADDR IN R12           00029400
*                                                                       00029500
*        RETURN TO KEY 0                                                00029600
*                                                                       00029700
         MODESET EXTKEY=ZERO                                            00029800
*                                                                       00029900
         ST    R14,RBEXSAVE+32          SAVE RETURN ADDR                00030000
*                                                                       00030100
*        USE TRKCALC TO DETERMINE MAXIMUM RECORD SIZE.                  00030200
*        THIS WILL BE LESS THAN THE DEVICE TRACK CAPACITY               00030300
*                                                                       00030400
*        USE RBEXSAVE AS PARAMETER AREA FOR TRKCALC                     00030500
*                                                                       00030600
         TRKCALC FUNCTN=TRKBAL,DEVTAB=(R9),R=01,K=0,                   C00030700
               DD=65535,MAXSIZE=YES,                                   C00030800
               MF=(E,RBEXSAVE+20)                                       00030900
*                                                                       00031000
         MODESET EXTKEY=RBT234,WORKREG=9                                00031100
*                                                                       00031200
*        STORE MAX RECORD SIZE IN RESULT AREA                           00031300
*                                                                       00031400
         ST    R0,DVAMAXRC                                              00031500
         C     R0,KF32760               > 32760 ?                       00031600
         BNH   DEVSIZE                  NO, BRANCH                      00031700
         MVC   DVAMAXRC,KF32760         YES, REDUCE TO 32760            00031800
DEVSIZE  LR    R9,R12                   RESTORE DVCT ADDR               00031900
         LM    R0,R1,XRBREG0            PICKUP CALLERS PARM REGS        00032000
         L     R14,RBEXSAVE+32          RESTORE RETURN ADDR             00032100
         LTR   R0,R0                    DEVTAB REQUESTED ?              00032200
         BP    DVNMEXT                  NO, BRANCH                      00032300
*                                                                       00032400
*        TEST FOR DASD MODELS WITH VARIABLE NUMBER OF CYLINDERS         00032500
*                                                                       00032600
         CLI   UCBTBYT4,X'0A'           3340 DASD ?                     00032700
         BE    DVDVARC                                                  00032800
         CLI   UCBTBYT4,X'0E'           3380 DASD ?                     00032900
         BE    DVDVARC                                                  00033000
         CLI   UCBTBYT4,X'0F'           3390 DASD ?                     00033100
         BE    DVDVARC                                                  00033200
         B     DVDA5                    NO, DVCT DATA VALID             00033300
*                                                                       00033400
*        PROCESS 3340/3380/3390 DASD TO DETERMINE MODEL                 00033500
*                                                                       00033600
DVDVARC  TM    UCBJBNR,UCBVRDEV         VIO DEVICE ?                    00033700
         BO    DVDA5                    YES, BRANCH                     00033800
         TM    UCBFL1,UCBNOTRD          DEVICE NOT READY?               00033900
         BO    CYLINVAL                 YES, BRANCH                     00034000
         TM    UCBDMCT,UCBMOUNT         DEVICE MOUNTED ?                00034100
         BZ    DVDA3                    YES, BRANCH                     00034200
CYLINVAL OI    DVAFLAGS,DVABDCYL        NO, INDICATE CYL NO INVALID     00034300
         B     DVDA5                    BYPASS READ OF SENSE/RDC DATA   00034400
*                                                                       00034500
*        SETUP TO ISSUE SENSE OR READ DEVICE CHARACTERISTICS CCW        00034600
*                                                                       00034700
DVDA3    MODESET EXTKEY=DATAMGT                                         00034800
*                                                                       00034900
         GETMAIN  RC,LV=CTLBLKSL,SP=230   STORAGE FOR EXCP BLOCKS       00035000
*                                                                       00035100
         LTR   R15,R15                  SUCESSFUL?                      00035200
         BNZ   CYLINVAL                 NO, BRANCH                      00035300
*                                                                       00035400
         MODESET EXTKEY=ZERO                                            00035500
*                                                                       00035600
         ST    R1,RBEXSAVE              SAVE ADDR OF GETMAINED STORAGE  00035700
*                                                                       00035800
         MODESET EXTKEY=DATAMGT                                         00035900
*                                                                       00036000
         LR    R12,R1                   R12 -> GETMAINED STORAGE        00036100
         USING CTLBLKS,R12              BASE FOR EXCP CNTL BLOCKS       00036200
         LR    R0,R1                                                    00036300
         LA    R1,CTLBLKSL              SET L'GETMAIN                   00036400
         SR    R15,R15                  SET SOURCE PAD AND LEN TO ZERO  00036500
         MVCL  R0,R14                   ENSURE STORAGE IS ZEROED        00036600
         MVC   CTLEYE,EYEP1            1ST PART OF EYE CATCHER  @RP0301 00036700
         OC    CTLEYE,EYEP2            2ND PART OF EYE CATCHER  @RP0301 00036800
*                                                                       00036900
*********************************************************************** 00037000
*                                                                       00037100
*        BUILD CONTROL BLOCKS FOR I/O                                   00037200
*        DCB                                                            00037300
*        DEB                                                            00037400
*        IOB                                                            00037500
*        ECB                                                            00037600
*        CCW                                                            00037700
*        DATA AREA                                                      00037800
*                                                                       00037900
*********************************************************************** 00038000
*                                                                       00038100
         LA    R1,CTLDEB               R1 -> DEB                        00038200
         USING DEB,R1                                                   00038300
         ST    R8,DEBSUCBA             STORE -> UCB IN DEB              00038400
         MVC   DEBAPPB,CVTXAPG+1       APPENDAGE VECTOR TABLE           00038500
         OI    DEBDEBID,X'0F'          DEB ID                           00038600
         LA    R10,CTLDCB                                               00038700
         LA    R0,DEBBASIC             CHAIN DCB AND DEB                00038800
         STCM  R0,B'0111',DCBDEBA      STORE -> DEB INTO DCB            00038900
         STCM  R10,B'0111',DEBDCBB     STORE -> DCB INTO DEB            00039000
         DROP  R1                                                       00039100
         LA    R1,CTLIOB                                                00039200
         USING IOBSTDRD,R1                                              00039300
         STCM  R10,B'0111',IOBDCBPB    STORE -> DCB INTO IOB            00039400
         LA    R0,CTLECB                                                00039500
         ST    R0,IOBECBPT             STORE -> ECB INTO IOB            00039600
         MVC   CTLCCW,MODRDC           MOVE IN MODEL RDC CCW            00039700
         CLI   UCBTBYT4,X'0A'          3340 DASD ?                      00039800
         BNE   DVDVARA                 NO, BRANCH                       00039900
         MVC   CTLCCW,MODSENSE         YES, MOVE IN MODEL SENSE CCW     00040000
DVDVARA  LA    R0,CTLDCHAR                                              00040100
         STCM  R0,B'0111',CTLCCW+1     STORE -> I/O AREA INTO CCW       00040200
         LA    R0,CTLCCW                                                00040300
         STCM  R0,B'0111',IOBSTRTB     STORE -> CCW INTO IOB            00040400
*                                                                       00040500
*        REQUEST SENSE DATA OR DEVICE CHARACTERISTICS DATA              00040600
*                                                                       00040700
         EXCP  CTLIOB                  ISSUE SENSE COMMAND              00040800
*                                                                       00040900
*        WAIT ON COMPLETION OF SENSE OR RDC COMMAND                     00041000
*                                                                       00041100
         WAIT  ECB=CTLECB               WAIT FOR I/O TO COMPLETE        00041200
*                                                                       00041300
         MODESET EXTKEY=RBT234,WORKREG=15     CHANGE TO CALLER'S KEY    00041400
*                                                                       00041500
         CLI   CTLECB,X'7F'             EXCP SUCCESSFUL?                00041600
         BE    DVDA3A                   YES, BRANCH                     00041700
         OI    DVAFLAGS,DVABDCYL        NO, INDICATE CYL NO INVALID     00041800
         B     DVDA4A                   BYPASS CYLINDER SETTING         00041900
*                                                                       00042000
*        TEST DATA RETURNED FROM COMMAND                                00042100
*                                                                       00042200
DVDA3A   CLI   UCBTBYT4,X'0A'           3340 DASD ?                     00042300
         BNE   DVDA3AA                  NO, BRANCH, MUST BE 3380/3390   00042400
         TM    CTLSDATA+2,X'02'         YES, TEST FOR 72MB 3340 MODEL   00042500
         BO    DVDA4A                   YES, CYL NO IS CORRECT          00042600
         LA    R0,349                   NO, SET CYL NO FOR 36MB MODEL   00042700
         STH   R0,DVACYL                STORE NO OF CYLINDERS           00042800
         B     DVDA4A                                                   00042900
*                                                                       00043000
*        3380/3390 DASD DEVICE                                          00043100
*        MOVE NUMBER OF PRIME CYLS FROM DEVICE CHARACTERISTICS DATA     00043200
*                                                                       00043300
DVDA3AA  MVC   DVACYL,CTLDCHAR+12       NO OF PRIMARY CYLS (UNSIGNED)   00043400
*                                                                       00043500
*        CLEANUP AFTER I/O OPERATION                                    00043600
*                                                                       00043700
DVDA4A   MODESET EXTKEY=ZERO                                            00043800
*                                                                       00043900
         XC    RBEXSAVE(4),RBEXSAVE     ZERO -> GETMAINED STORAGE       00044000
*                                                                       00044100
         MODESET EXTKEY=DATAMGT                                         00044200
*                                                                       00044300
         FREEMAIN  R,LV=CTLBLKSL,A=(R12),SP=230   RELEASE DYNAMIC AREA  00044400
*                                                                       00044500
DVDA5    B     DVNMEXT                                                  00044600
*                                                                       00044700
*        STORE MAXIMUM BLKSIZE IN RESULT AREA                           00044800
*                                                                       00044900
DV004    ST    R12,DVAMAXRC             STORE BLOCK SIZE IN 2ND WORD    00045000
*                                                                       00045100
DVNMEXT  SR    R15,R15                  ZERO RETURN CODE                00045200
*                                                                       00045300
DVRTRN   MODESET EXTKEY=ZERO                                            00045400
*                                                                       00045500
         BR    R14                      RETURN TO CALLER                00045600
*                                                                       00045700
DVDUMMY  XC    DVAPREFX,DVAPREFX        CLEAR RESULT AREA               00045800
         B     DVNMEXT                  EXIT                            00045900
*                                                                       00046000
ABNRTN   LA    R15,4                    SET UP ABNORMAL RETURN CODE     00046100
         B     DVRTRN                                                   00046200
*                                                                       00046300
*        RECOVERY ROUTINE                                               00046400
*                                                                       00046500
STAEEXIT LA    R15,12                                                   00046600
         CR    R15,R0                  STORAGE AVAILBLE FOR SDWA ?      00046700
         BER   R14                     NO, RETURN                       00046800
         STM   R14,R12,12(R13)         YES, SAVE REGS                   00046900
         BALR  R12,R0                  ESTABLISH ADDDRESSABILITY        00047000
         USING *,R12                                                    00047100
         USING SDWA,R1                                                  00047200
         MVC   SDWACSCT,KIGC002D       MODULE NAME IN ERROR             00047300
         MVC   SDWAREXN,IGCT002D       NAME OF RECOVERY ROUTINE         00047400
         DROP  R3                                                       00047500
         L     R1,CVTPTR                                                00047600
         USING CVT,R1                                                   00047700
         L     R2,CVTLINK              R2 -> SYS1.LINKLIB DCB           00047800
*                                                                       00047900
*        LOAD  IGCT002D FROM LINKLIB BUT IT IS ACTUALLY IN LPALIB       00048000
*                                                                       00048100
         LOAD  EPLOC=IGCT002D,DCB=(R2)                                  00048200
         LR    R5,R0                   R5 -> IGCT002D EP                00048300
*                                                                       00048400
*        DELETE IGCT002D                                                00048500
*                                                                       00048600
         DELETE EPLOC=IGCT002D                                          00048700
*                                                                       00048800
         LR    R15,R5                  R15 -> EP OF IGCT002D            00048900
         L     R14,12(,R13)            RESTORE REGS EXCEPT R15          00049000
         LM    R0,R12,20(R13)                                           00049100
         BR    R15                     PASS CONTROL TO IGCT002D         00049200
*                                                                       00049300
         DROP  R12                                                      00049400
*                                                                       00049500
KF32760  DC    F'32760'                MAX RECORD SIZE                  00049600
DVMASK1  DC    X'00FFFFFF'                                              00049700
DVMASK2  DC    X'0000000F'                                              00049800
EYEP1    DC    XL8'0323130213160312'      EYE CATCHER PART 1    @RP0301 00049900
EYEP2    DC    XL8'C0C0C0C0C0C0C0C0'      EYE CATCHER PART 2    @RP0301 00050000
*                                                                       00050100
*        MODEL SENSE CCW                                                00050200
*                                                                       00050300
MODSENSE CCW   CCWSENSE,0,CCWSLI,6     SENSE COMMAND                    00050400
*                                                                       00050500
*        MODEL READ DEVICE CHARACTERISTICS CCW                          00050600
*                                                                       00050700
MODRDC   CCW   READDCHR,0,CCWSLI,64    READ DEVICE CHAR COMMAND         00050800
*                                                                       00050900
IGCT002D DC    C'IGCT002D'             NAME OF RECOVERY MODULE          00051000
         DC    0D'0'                                                    00051100
ESTAEL   ESTAE STAEEXIT,RECORD=YES,MF=L                                 00051200
ESTAELX  EQU   *                      L'ESTAE PARAMETER LIST            00051300
*                                                                       00051400
KIGC002D DC    C'IGC0002D'                                              00051500
*                                                                       00051600
*        IOS CCW AND CSW EQUATES                                        00051700
*                                                                       00051800
*        CCW COMMAND CODE EQUATES FOR DISK I/O                          00051900
*                                                                       00052000
CCWNOOP  EQU   X'03'              NO OPERATION                          00052100
CCWTIC   EQU   X'08'              TRANSFER IN CHANNEL                   00052200
CCWSENSE EQU   X'04'              SENSE                                 00052300
READDCHR EQU   X'64'              READ DEVICE CHARACTERISTICS           00052400
*                                 FOR 3380/3390 DASD                    00052500
*                                                                       00052600
SEEK     EQU   X'07'              SEEK                                  00052700
SEEKC    EQU   X'0B'              SEEK CYL                              00052800
SEEKH    EQU   X'1B'              SEEK HEAD                             00052900
SEARKEQ  EQU   X'29'              SEARCH KEY EQUAL                      00053000
SEARKHE  EQU   X'69'              SEARCH KEY HIGH OR EQUAL              00053100
READHA   EQU   X'1A'              READ HOME ADDRESS                     00053200
READR0   EQU   X'16'              READ RECORD 0                         00053300
READCNT  EQU   X'12'              READ COUNT                            00053400
READDATA EQU   X'06'              READ DATA                             00053500
READKD   EQU   X'0E'              READ KEY AND DATA                     00053600
READCKD  EQU   X'1E'              READ COUNT, KEY AND DATA              00053700
READMCKD EQU   X'5E'              READ MULTIPLE COUNT, KEY, AND DATA    00053800
SETSECT  EQU   X'23'              SET SECTOR                            00053900
SETFM    EQU   X'1F'              SET FILE MASK                         00054000
READSECT EQU   X'22'              READ SECTOR                           00054100
SEARIDEQ EQU   X'31'              SEARCH IDENTIFIER EQUAL               00054200
*                                                                       00054300
WRITEDAT EQU   X'05'              WRITE DATA                            00054400
WRITEKD  EQU   X'0D'              WRITE KEY AND DATA                    00054500
WRITECKD EQU   X'1D'              WRITE COUNT,KEY AND DATA              00054600
WRITSCKD EQU   X'01'              WRITE SPECIAL COUNT,KEY AND DATA      00054700
*                                 FOR OVERFLOW RECORDS                  00054800
WRITERSE EQU   X'11'              ERASE                                 00054900
*                                                                       00055000
CCWMT    EQU   X'80'              MULTI-TRACK USE WITH APPROPRIATE      00055100
*                                 OPCODES - CCW SEARKHE+CCWMT,B,C,D     00055200
*                                                                       00055300
*        CCW FLAG EQUATES                                               00055400
*                                                                       00055500
CCWCD    EQU   X'80'              CHAIN DATA                            00055600
CCWCC    EQU   X'40'              CHAIN COMMAND                         00055700
CCWSLI   EQU   X'20'              SUPPRESS INCORRECT LENGTH INDICATION  00055800
CCWSKIP  EQU   X'10'              SKIP TRANSFER OF DATA                 00055900
CCWPCI   EQU   X'08'              PROGRAM CONTROLLED INTERRUPT          00056000
CCWIDA   EQU   X'04'              INDIRECT DATA ADDR (IDAW)             00056100
*                                                                       00056200
*        CSW FLAG EQUATES                                               00056300
*                                                                       00056400
*        UNIT STATUS BYTE                                               00056500
*                                                                       00056600
CSWCUE   EQU   X'20'              CONTROL UNIT END                      00056700
CSWCUB   EQU   X'10'              CONTROL UNIT BUSY                     00056800
CSWCE    EQU   X'08'              CHANNEL END                           00056900
CSWDE    EQU   X'04'              DEVICE END                            00057000
CSWUC    EQU   X'02'              UNIT CHECK                            00057100
CSWUE    EQU   X'01'              UNIT EXCEPTION                        00057200
*                                                                       00057300
*        CHANNEL STATUS BYTE                                            00057400
*                                                                       00057500
CSWPCI   EQU   X'80'              PROGRAM CONTROLLED INTERRUPT          00057600
CSWIL    EQU   X'40'              INCORRECT LENGTH                      00057700
CSWPC    EQU   X'20'              PROGRAM CHECK                         00057800
CSWPROTC EQU   X'10'              PROTECTION CHECK                      00057900
CSWCDC   EQU   X'08'              CHANNEL DATA CHECK                    00058000
CSWCCC   EQU   X'04'              CHANNEL CONTROL CHECK                 00058100
CSWICC   EQU   X'02'              INTERFACE CONTROL CHECK               00058200
CSWCHK   EQU   X'01'              CHAINING CHECK                        00058300
*                                                                       00058400
         PRINT NOGEN                                                    00058500
*                                                                       00058600
         IEZIOB DSECT=YES                                               00058700
*                                                                       00058800
         IEZDEB                                                         00058900
*                                                                       00059000
         DCBD  DSORG=DA                                                 00059100
*                                                                       00059200
         IKJRB                                                          00059300
*                                                                       00059400
*        IHADVA - MAP OUTPUT RESULTS RETURNED TO CALLER                 00059500
*                                                                       00059600
         IHADVA                                                         00059700
*                                                                       00059800
         IKJTCB                                                         00059900
*                                                                       00060000
*        TIOT                                                           00060100
*                                                                       00060200
TIOT     DSECT                                                          00060300
         IEFTIOT1                                                       00060400
*                                                                       00060500
*        CVT                                                            00060600
*                                                                       00060700
         CVT   DSECT=YES                                                00060800
*                                                                       00060900
*        UCB                                                            00061000
*                                                                       00061100
UCB      DSECT                                                          00061200
         IEFUCBOB                                                       00061300
*                                                                       00061400
         IHASDWA                                                        00061500
*                                                                       00061600
*********************************************************************** 00061700
*                                                                       00061800
*        DYNAMIC AREA                                                   00061900
*                                                                       00062000
*        GETMAINED IN SUBPOOL 230                                       00062100
*                                                                       00062200
*********************************************************************** 00062300
*                                                                       00062400
CTLBLKS  DSECT                                                          00062500
CTLEYE   DS    CL8                     EYE CATCHER                      00062600
*                                                                       00062700
*        DCB                                                            00062800
*                                                                       00062900
CTLDCB   DCB   MACRF=(E),DSORG=PS                                       00063000
*                                                                       00063100
*        IOB                                                            00063200
*                                                                       00063300
CTLIOB   DS    10F                                                      00063400
*                                                                       00063500
*        DEB                                                            00063600
*                                                                       00063700
         DS    0D                                                       00063800
CTLDEB   DS    XL(DEBBASND-DEBAVT+16)  DEB ORIGIN                       00063900
*                                                                       00064000
*        ECB                                                            00064100
*                                                                       00064200
CTLECB   DS    F                       ECB                              00064300
*                                                                       00064400
*        CCW                                                            00064500
*                                                                       00064600
CTLCCW   DS    D                                                        00064700
*                                                                       00064800
*        I/O AREA                                                       00064900
*                                                                       00065000
CTLDCHAR DS    XL64                    DEVICE CHARACTERISTICS DATA      00065100
         ORG   CTLDCHAR                                                 00065200
CTLSDATA DS    XL8                     SENSE DATA                       00065300
         ORG                                                            00065400
         DS    D                       ALIGN TO DOUBLE WORD             00065500
CTLBLKSL EQU   *-CTLBLKS               L'CTLBLKS                        00065600
*                                                                       00065700
         END                                                            00065800
./ ADD NAME=IGCT002D
SVCT24   TITLE 'IGCT0024 - DEVTYPE SVC 24 ESTAE RECOVERY ROUTINE'       00000100
*                                                                       00000200
*        STATUS -                                                       00000300
*        LASTUPD         = JCLIN    TYPE=UPD                            00000400
*        LIBRARIES       = DISTLIB=AOSD0                                00000500
*        FMID            = EDM1102                                      00000600
*        RMID            = UZ28801                                      00000700
*        LMOD            = IGCT002D                                     00000800
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS ---STATUS---  00000900
*                          EDM1102  FUNCTION  00.000  MOD     ACC RGN   00001000
*                                                                       00001100
*        SOURCE WAS CREATED BY DISASSEMBLY TO MATCH THE                 00001200
*        LOAD MODULE AT THE UZ28801 LEVEL                               00001300
*                                                                       00001400
*        CHANGE ACTIVITY -                                              00001501
*               RP00003 - 1. FREEMAIN THE CORRECT LENGTH OF             00001601
*                            DYNAMIC STORAGE GETMAINED BY IGC0002D.     00001703
*                            ANY CHANGES TO THE LENGTH OF THE STORAGE   00001803
*                            GETMAINED IN IGC0002D MUST BE REFLECTED    00001903
*                            IN THIS RECOVERY MODULE                    00002003
*                         2. NUMEROUS CHANGES TO IMPROVE SOURCE         00002101
*                            READABILITY AND USE OF SYSTEM MAPPING      00002201
*                            MACROS                                     00002301
*                                                                       00002401
*        FUNCTION -                                                     00002500
*        ESTAE RECOVERY MODULE FOR DEVTYPE SVC 24                       00002600
*                                                                       00002700
*        REGISTERS ON ENTRY -                                           00002800
*        R1  -> SDWA FOR IGC0002D                                       00002900
*        R2  -  R12 REGISTERS AT TIME OF ERROR                          00003000
*        R13 -                                                          00003100
*        R14 -  REGISTERS AT TIME OF ERROR                              00003200
*        R15 -> ENTRY POINT OF IGCT0024                                 00003300
*                                                                       00003400
IGCT002D CSECT                                                          00003500
*                                                                       00003600
CTLBLKSL EQU   272                <--- L'STORAGE GETMAINED BY IGC0002D  00003701
*                                      SEE DEFINITION IN IGC0002D       00003800
         USING IGCT002D,R15                                             00003900
         USING TCB,R12                                                  00004000
         USING SDWA,R5                                                  00004100
         B     BEGIN                                                    00004200
*                                                                       00004301
         DC    AL1(MODIDX-MODID)                                        00004400
MODID    DC    C'IGCT002D @RP00003 &SYSDATE &SYSTIME'                   00004504
MODIDX   EQU   *                                                        00004600
*                                                                       00004701
         DROP  R15                                                      00004800
BEGIN    LA    R9,0(,R15)              BASE                             00004900
         USING IGCT002D,R9                                              00005000
         LR    R5,R1                   R5 -> SDWA                       00005100
*                                                                       00005200
*        IGC0002D STORED -> RB INTO ESTAE PARAM AREA                    00005300
*                                                                       00005400
         L     R15,SDWAPARM            R15 -> RB                        00005500
         USING RBBASIC,R15                                              00005600
         ST    R14,RBIQEWRK            SAVE RETURN ADDR                 00005700
         L     R8,XRBREG1              PICKUP SVC 24 CALLERS R1         00005800
         LPR   R8,R8                   MAKE POSITIVE                    00005900
         L     R6,XRBREG0              PICKUP SVC 24 CALLERS R0         00006000
         LPR   R6,R6                   MAKE POSITIVE                    00006100
         DROP  R15                                                      00006200
         TM    SDWAERRC,SDWAEAS        RECOGNIZED ERROR ?               00006300
         BO    IGC000A6                YES, BRANCH                      00006400
         TM    SDWAERRD,SDWACLUP       JUST CLEANUP ?                   00006500
         BO    IGC000A6                YES, BRANCH                      00006600
         USING PSA,R0                                                   00006700
         L     R12,PSATOLD             R12 -> TCB                       00006800
         USING TCB,R12                                                  00006900
         TM    TCBCMPF,TCBCREQ         DUMP REQUESTED ?                 00007000
         BO    IGC0006C                YES, BRANCH                      00007100
         LA    R15,4095                SET MASK FOR 3 LOW ORDER NIBBLES 00007200
         N     R15,SDWAABCC                                             00007300
         LTR   R15,R15                 ANY USER COMPLETION CODE SET ?   00007400
         BNZ   IGC000A6                YES, BRANCH                      00007500
         TM    SDWAERRC,SDWASTAF       RECURSIVE ERROR ?                00007600
         BO    IGC0006C                YES, BRANCH                      00007700
         TM    SDWAERRA,SDWAMCHK+SDWATEXC+SDWAPGIO  UNRECOVERABLE ERRS? 00007800
         BNZ   IGC000A6                YES, BRANCH                      00007900
         B     IGC000F8                NO, RECOVERABLE ERROR            00008000
*                                                                       00008100
*        FORMAT PARAMETER LIST FOR SDUMP                                00008200
*                                                                       00008300
IGC0006C MVC   SDWAVRA+24(L'DUMPHDR+L'DUMPHDRT),DUMPHDR                 00008400
         L     R12,PSATOLD             R12 -> TCB                       00008500
         L     R12,TCBTIO              R12 -> TIOT                      00008600
         DROP  R12                                                      00008700
         USING TIOT,R12                                                 00008800
         MVC   SDWAVRA+34(L'TIOCNJOB),TIOCNJOB                          00008900
         MVC   SDWAVRA+43(8),TIOCSTEP                                   00009000
         DROP  R12                                                      00009100
         MVC   SDWAVRA(24),SDUMPPRM                                     00009200
         LA    R1,SDWAVRA                                               00009300
*                                                                       00009400
*        ISSUE SDUMP                                                    00009500
*                                                                       00009600
         SDUMP MF=(E,(R1)),HDRAD=SDWAVRA+24                             00009700
*                                                                       00009800
         OI    SDWAERRC,SDWAEAS        ERROR RECOGNIZED, DATA PROVIDED  00009900
IGC000A6 L     R12,SDWAPARM            RB ADDR STORED BY IGC0002D       00010000
         USING RBBASIC,R12                                              00010100
         L     R4,XRBREG0              PICKUP SVC 24 CALLERS R0         00010200
         ST    R4,SDWAVRA                                               00010300
         L     R4,XRBREG1              PICKUP SVC 24 CALLERS R1         00010400
         ST    R4,SDWAVRA+4                                             00010500
         L     R12,RBEXSAVE            IF IGC0002D GETMAINED STORAGE    00010600
*                                      THEN ADDR STORED HERE            00010700
         ST    R12,SDWAVRA+8                                            00010800
         LTR   R12,R12                 DID IGC0002D GETMAIN STORAGE ?   00010900
         BZ    IGC000E0                NO, BRANCH                       00011000
         DROP  R12                                                      00011100
*                                                                       00011200
*        FREE STORAGE GETMAINED BY IGC0002D                             00011301
*                                                                       00011400
         FREEMAIN  R,LV=CTLBLKSL,A=(R12),SP=230   RELEASE DYNAMIC AREA  00011501
*                                                                       00011600
IGC000E0 MVI   SDWAURAL,X'0F'          L' SUPPLIED RECORDING DATA       00011700
*                                                                       00011800
         SETRP  WKAREA=(5),DUMP=IGNORE,RC=0,RECORD=YES                  00011900
*                                                                       00012000
         L     R12,SDWAPARM            R12 -> RB                        00012100
         USING RBBASIC,R12                                              00012200
         L     R14,RBIQEWRK            PICKUP R14 SAVED AT ENTRY        00012300
IGC000F6 BR    R14                     EXIT                             00012400
*                                                                       00012500
         DROP  R12                                                      00012600
*                                                                       00012700
IGC000F8 SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,RELATED=GETLOCAL         00012800
*                                                                       00012900
         LA    R7,1                    SET ERROR CODE                   00013000
         LR    R1,R8                   RESTORE CALLERS R1 -> DDNAME     00013100
         LR    R2,R1                   COPY TO R2                       00013200
         SL    R2,KFM7                 ADD 7, R2 -> LAST BYTE OF        00013300
*                                      DDNAME                           00013400
*                                                                       00013500
*        PROVIDE ADDR OF TCB FOR IEAVEVAL                               00013600
*                                                                       00013700
         L     R12,SDWAPARM            R12 -> RB                        00013800
         USING RBBASIC,R12                                              00013900
         L     R4,RBLINK               R4 -> PREVIOUS RB (CALLER)       00014000
         DROP  R12                                                      00014100
         USING RBBASIC,R4                                               00014200
         LA    R4,RBOPSWB2                                              00014300
         SL    R4,KF28                                                  00014400
         DROP  R4                                                       00014500
*                                                                       00014600
*        CALL IEAVEVAL TO VALIDATE ACCESS TO CALLER PROVIDED            00014700
*        DDNAME FIELD                                                   00014800
*                                                                       00014900
         L     R12,CVTPTR                                               00015000
         USING CVT,R12                                                  00015100
         L     R15,CVT0VL00            R15 -> IEAVEVAL VALIDITY CK RTN  00015200
         DROP  R12                                                      00015300
         BALR  R14,R15                 CALL VALIDITY CHECK RTN          00015400
         BNZ   IGC00230                FAILED VALIDITY CHECK, BRANCH    00015500
*                                                                       00015600
*        DETERMINE LENGTH OF CALLER PROVIDED RESULT FIELD               00015700
*        BASED ON CALL REGISTER VALUES                                  00015800
*                                                                       00015900
         LA    R7,2                    SET ERROR                        00016000
         LR    R1,R6                   RESTORE CALLERS R0 -> RESULT     00016100
         L     R12,SDWAPARM                                             00016200
         USING RBBASIC,R12                                              00016300
         L     R12,XRBREG1             R12 = CALLERS R1                 00016400
         LTR   R12,R12                 NEGATIVE ?                       00016500
         BNM   IGC0014A                POSITIVE, BRANCH                 00016600
         LR    R2,R1                   NEGATIVE, RESTORE VALUE          00016700
         SL    R2,KFM23                ADD 23, R2 -> LAST BYTE OF DASD  00016800
*                                      RESULT AREA ALLOWING FOR RPS     00016900
         B     IGC00168                                                 00017000
*                                                                       00017100
*        NON RPS RESULT FIELD                                           00017200
*                                                                       00017300
IGC0014A L     R12,SDWAPARM                                             00017400
         L     R12,RBGRS0              R12 = CALLERS R0                 00017500
         LTR   R12,R12                 NEGATIVE ?                       00017600
         BNM   IGC00162                POSITIVE, BRANCH                 00017700
         LR    R2,R1                   NEGATIVE, RESTORE VALUE          00017800
         SL    R2,KFM19                ADD 19, R2 -> LAST BYTE OF DASD  00017900
*                                      NON RPS RESULT AREA              00018000
         B     IGC00168                                                 00018100
*                                                                       00018200
*        8 BYTE RESULT AREA                                             00018300
*                                                                       00018400
IGC00162 LR    R2,R1                                                    00018500
         SL    R2,KFM7                 ADD 7, R2 -> LAST BYTE OF        00018600
*                                      NON DASD RESULT AREA             00018700
*                                                                       00018800
*        CALL IEAVEVAL TO VALIDATE ACCESS TO CALLER PROVIDED            00018900
*        DDNAME FIELD                                                   00019000
*                                                                       00019100
*        PROVIDE ADDR OF TCB FOR IEAVEVAL                               00019200
*                                                                       00019300
IGC00168 L     R12,SDWAPARM                                             00019400
         L     R4,RBLINK                                                00019500
         DROP  R12                                                      00019600
         LA    R4,RBOPSWB2-RBBASIC(,R4) R4 -> PSW SECOND BYTE (KEY)     00019700
         SL    R4,KF28                 SUBTRACT X'1C' ?                 00019800
*                                                                       00019900
*        IEAVEVAL R4 PARAMETER -                                        00020000
*                                                                       00020100
*        R4  -> TCB                                                     00020200
*               - OR -                                                  00020300
*               ZERO IF CURRENT TCB TO BE USED                          00020400
*               - OR -                                                  00020500
*               NEGATIVE IF KEY PROVIDED IN BITS 24-27 OF R4            00020600
*                                                                       00020700
*        CALL IEAVEVAL TO VALIDATE ACCESS TO CALLER PROVIDED            00020800
*        DDNAME FIELD                                                   00020900
*                                                                       00021000
         L     R12,CVTPTR                                               00021100
         USING CVT,R12                                                  00021200
         L     R15,CVT0VL00            R15 -> IEAVEVAL VALIDITY CK RTN  00021300
         DROP  R12                                                      00021400
         BALR  R14,R15                 CALL VALIDITY CHECK RTN          00021500
         BNZ   IGC00230                FAILED VALIDITY CHECK, BRANCH    00021600
*                                                                       00021700
*        FINISHED USING IEAVEVAL VALIDATION ROUTINE                     00021800
*                                                                       00021900
*        RELEASE LOCAL LOCK                                             00022000
*                                                                       00022100
*        NOTE PREVIOUS COMMENT ON CHANGED SETLOCK MACRO CODE            00022200
*        GENERATION                                                     00022300
*                                                                       00022400
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=GETLOCK                     00022500
*                                                                       00022600
*        FORMAT PARAMETER LIST FOR SDUMP                                00022700
*                                                                       00022800
         MVC   SDWAVRA+24(27),DUMPHDR                                   00022900
         L     R12,PSATOLD             R12 -> TCB                       00023000
         USING TCB,R12                                                  00023100
         L     R12,TCBTIO              R12 -> TIOT                      00023200
         DROP  R12                                                      00023300
         USING TIOT,R12                                                 00023400
         MVC   SDWAVRA+34(L'TIOCNJOB),TIOCNJOB                          00023500
         MVC   SDWAVRA+43(8),TIOCSTEP                                   00023600
         DROP  R12                                                      00023700
         MVC   SDWAVRA(24),SDUMPPRM                                     00023800
         LA    R1,SDWAVRA                                               00023900
*                                                                       00024000
*        ISSUE SDUMP                                                    00024100
*                                                                       00024200
         SDUMP MF=(E,(R1)),HDRAD=SDWAVRA+24                             00024300
*                                                                       00024400
         LR    R7,R15                  SAVE SDUMP RETURN CODE           00024500
         OI    SDWAERRC,SDWAEAS        ERROR RECOGNIZED                 00024600
         MVC   SDWAVRA(49),IEC913I     MOVE IN WTO SKELETON             00024700
         LR    R12,R7                  R12 = SDUMP RETURN CODE          00024800
         SRL   R12,2                   SHIFT TO LOW ORDER               00024900
         ALR   R12,R12                 MULT BY 2                        00025000
         LA    R4,BINTCHAR+2(R12)      R4 -> CHAR VALUE                 00025100
         MVC   SDWAVRA+16(2),0(R4)     MOVE ERROR CODE IN CHAR INTO WTO 00025200
         L     R12,PSATOLD             R12 -> TCB                       00025300
         USING TCB,R12                                                  00025400
         L     R4,TCBTIO               R4 -> TIOT                       00025500
         USING TIOT,R4                                                  00025600
         MVC   SDWAVRA+19(L'TIOCNJOB),TIOCNJOB                          00025700
         MVC   SDWAVRA+28(8),TIOCSTEP                                   00025800
*                                                                       00025903
         MODESET EXTKEY=TCB,WORKREG=14                                  00026003
*                                      R8 = CALLERS R1 -> DDNAME        00026100
         LM    R14,R15,0(R8)           R14-R15 = CALLERS DDNAME         00026200
*                                      IN CALLERS KEY                   00026302
*                                                                       00026402
         MODESET EXTKEY=ZERO           CHANGE TO KEY ZERO               00026502
*                                                                       00026602
         STCM  R14,B'1111',SDWAVRA+37  STORE DDNAME INTO WTO            00026702
         STCM  R15,B'1111',SDWAVRA+41                                   00026802
*                                                                       00026900
         WTO   MF=(E,SDWAVRA)          ISSUE IEC913I ERROR MESSAGE      00027000
*                                                                       00027100
         MVC   SDWAVRA+12(3),SDWACMPC                                   00027200
*                                                                       00027300
         SETRP WKAREA=(5),DUMP=IGNORE,RC=0                              00027400
*                                                                       00027500
         SLR   R14,R14                                                  00027600
         ICM   R14,B'0111',ABEND218          SET ABEND CODE             00027702
         STCM  R14,B'0111',SDWACMPC-SDWA(R1)                            00027802
         B     IGC000A6                                                 00027900
*                                                                       00028000
*        PARAMETER VALIDATION HAS FAILED                                00028100
*                                                                       00028200
*        NOTE PREVIOUS COMMENT ON CHANGED SETLOCK MACRO CODE            00028300
*        GENERATION                                                     00028400
*                                                                       00028500
IGC00230 SETLOCK RELEASE,TYPE=LOCAL,RELATED=GETLOCK                     00028600
*                                                                       00028700
         MVC   SDWAVRA(49),IEC912I     MOVE SKELETON WTO TO WORKAREA    00028800
         L     R12,PSATOLD             R12 -> TCB                       00028900
         L     R4,TCBTIO               R4 -> TIOT                       00029000
         MVC   SDWAVRA+19(L'TIOCNJOB),TIOCNJOB                          00029100
         MVC   SDWAVRA+28(8),TIOCSTEP                                   00029200
         DROP  R4                                                       00029300
         LR    R4,R7                   R4 = ERROR CODE                  00029400
         ALR   R4,R4                   MULT BY 2                        00029500
         LA    R1,BINTCHAR(R4)                                          00029600
         MVC   SDWAVRA+16(2),0(R1)     MOVE ERROR CODE INTO WTO         00029700
         C     R7,KF2                  ERROR CODE OF 2 ?                00029800
         BNE   IGC00286                NO, BRANCH                       00029900
*                                                                       00030003
         MODESET EXTKEY=TCB,WORKREG=14   CHANGE TO CALLERS KEY          00030103
*                                                                       00030203
         DROP  R12                                                      00030300
         LM    R14,R15,0(R8)           R14-R15 = CALLERS DDNAME         00030400
*                                      IN CALLERS KEY                   00030502
*                                                                       00030602
         MODESET EXTKEY=ZERO           CHANGE TO KEY ZERO               00030702
*                                                                       00030802
         STCM  R14,B'1111',SDWAVRA+37  STORE DDNAME INTO WTO            00030902
         STCM  R15,B'1111',SDWAVRA+41                                   00031002
         B     IGC0028C                                                 00031100
*                                                                       00031200
IGC00286 MVC   SDWAVRA+37(L'KCASTER),KCASTER                            00031300
*                                                                       00031400
IGC0028C WTO   MF=(E,SDWAVRA)                                           00031500
*                                                                       00031600
         L     R12,SDWAPARM            R12 -> RB                        00031700
         USING RBBASIC,R12                                              00031800
         L     R12,RBEXSAVE            IGC0002D GETMAIN ANY STORAGE ?   00031900
         LTR   R12,R12                                                  00032000
         BZ    IGC002B8                NO, BRANCH AROUND FREEMAIN       00032100
         DROP  R12                                                      00032200
*                                                                       00032300
*        FREE STORAGE GETMAINED BY IGC0002D                             00032401
*                                                                       00032500
         FREEMAIN  R,LV=CTLBLKSL,A=(R12),SP=230   RELEASE DYNAMIC AREA  00032601
*                                                                       00032701
IGC002B8 L     R12,SDWAPARM            R12 -> RB                        00032800
         USING RBBASIC,R12                                              00032900
         ST    R12,SDWASR05            STORE -> RB INTO SDWA R5         00033000
         ST    R7,SDWASR06             STORE ERROR CODE INTO SDWA R6    00033100
*                                                                       00033200
*        RYO SETRP                                                      00033300
*                                                                       00033400
*        SETRP WKAREA=(5),RC=4,RETADDR=IGC002EA,FRESDWA=YES,            00033500
*              RECORD=IGNORE                                            00033600
*                                                                       00033700
         LR    R1,R5                                                    00033800
         MVI   SDWARCDE-SDWA(1),4      SET RC FIELD                     00033900
         LA    R4,IGC002EA             R4 -> RETRY ROUTINE              00034000
         ST    R4,SDWARTYA-SDWA(,1)    INITIALIZE RETRY ADDRESS FIELD   00034100
         OI    SDWAACF2-SDWA(1),SDWAFREE        TURN ON FREE SDWA       00034200
         NI    SDWAACF2-SDWA(1),255-SDWARCRD    TURN OFF RECORDING      00034300
         NI    SDWAABCC-SDWA(1),255-SDWAREQ     TURN OFF DUMP           00034400
         OI    SDWAACF2-SDWA(1),SDWAUPRG        USE UPDATED REGS        00034500
         L     R14,RBIQEWRK            RESTORE RETURN ADDR              00034600
         B     IGC000F6                                                 00034700
*                                                                       00034800
*        CANCEL ESTAE                                                   00034900
*                                                                       00035000
IGC002EA ESTAE 0                                                        00035100
*                                                                       00035200
         LR    R15,R6                                                   00035300
         LM    R2,R14,SDWAGR04         RESTORE REGS                     00035400
*                                                                       00035500
*        ISSUE ABEND                                                    00035600
*                                                                       00035700
         ABEND 280,DUMP,,SYSTEM                                         00035800
*                                                                       00035900
KF2      DC    F'2'                                                     00036000
KF28     DC    F'28'                                                    00036100
KFM23    DC    F'-23'                                                   00036200
KFM19    DC    F'-19'                                                   00036300
KFM7     DC    F'-7'                                                    00036400
         DC    D'0'                                                     00036500
*                                                                       00036600
DUMPHDR  DC    AL1(L'DUMPHDRT)                                          00036700
DUMPHDRT DC    CL26'IGCT002D,        ,'                                 00036800
*                                                                       00036900
KCASTER  DC    C'********'                                              00037000
*                                                                       00037100
IEC912I  WTO   'IEC912I 118-  ,        ,        ,        ',            C00037200
               ROUTCDE=(11),DESC=(6),MF=L                               00037300
*                                                                       00037400
IEC913I  WTO   'IEC913I 218-  ,        ,        ,        ',            C00037500
               ROUTCDE=(11),DESC=(6),MF=L                               00037600
*                                                                       00037700
ABEND218 DC    X'218000'               SYSTEM ABEND CODE 218            00037800
*                                                                       00037900
*        BINARY TO CHAR CONVERT TABLE                                   00038000
*                                                                       00038100
BINTCHAR DC    C'  '                                                    00038200
         DC    C'01020304'                                              00038300
*                                                                       00038400
         DC    0F'0'                                                    00038500
*                                                                       00038600
*        SDUMP MF=L WAS GENERATED WITH A PREVIOUS VERSION OF THE        00038700
*        SDUMP MACRO THAT DID NOT GENERATE THE NEW ENHANCED             00038800
*        FORMAT OF THE PARAMETER LIST                                   00038900
*                                                                       00039000
*        SDUMP MF=L,QUIESCE=YES,SDATA=(RGN,LPA,TRT,SWA)                 00039100
*                                                                       00039200
SDUMPPRM DC    X'0280740000000000'     *                                00039300
         DC    XL16'0'                 V                                00039400
*                                                                       00039500
         PRINT NOGEN                                                    00039600
*                                                                       00039700
         CVT   DSECT=YES,LIST=YES                                       00039800
*                                                                       00039900
         IKJRB  DSECT=YES                                               00040000
*                                                                       00040100
         IKJTCB  LIST=YES                                               00040200
*                                                                       00040300
         IHAPSA DSECT=YES                                               00040400
*                                                                       00040500
         IHASDWA                                                        00040600
*                                                                       00040700
*        TIOT                                                           00040800
*                                                                       00040900
TIOT     DSECT                                                          00041000
         IEFTIOT1                                                       00041100
*                                                                       00041200
*        REGISTER EQUATES                                               00041301
*                                                                       00041400
R0       EQU   0                                                        00041501
R1       EQU   1                                                        00041601
R2       EQU   2                                                        00041701
R3       EQU   3                                                        00041801
R4       EQU   4                                                        00041901
R5       EQU   5                                                        00042001
R6       EQU   6                                                        00042101
R7       EQU   7                                                        00042201
R8       EQU   8                                                        00042301
R9       EQU   9                                                        00042401
R10      EQU   10                                                       00042501
R11      EQU   11                                                       00042601
R12      EQU   12                                                       00042701
R13      EQU   13                                                       00042801
R14      EQU   14                                                       00042901
R15      EQU   15                                                       00043001
*                                                                       00043100
*        IEZBITS                                                        00043200
*                                                                       00043300
         IEZBITS                                                        00043400
*                                                                       00043500
         END   IGCT002D                                                 00043600
./ ENDUP
??
//ASM1     EXEC SMPASM,M=IGC0002D,COND=(0,NE)
//ASM2     EXEC SMPASM,M=IGCT002D,COND=(0,NE)
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA',COND=(0,NE)
//SMPPTFIN DD  DATA,DLM='??'
++USERMOD(RP00003).
++VER(Z038) FMID(EDM1102)
  PRE(UZ28801,UZ73568)
  /* PROBLEM DESCRIPTION:
     THE NUMBER OF CYLINDERS FOR 3380 AND 3390 DASD VOLUMES IS
     DEPENDENT ON THE CHARACTERISTICS OF THE VARIOUS MODELS OF
     3380 AND 3390 DASD.

     THERE IS NO IDENTIFICATION OF THE MODEL NUMBER AND HENCE NUMBER
     OF CYLINDERS PROVIDED IN ANY OF THE DEVICE CONTROL BLOCKS.
     THE NUMBER OF CYLINDERS ON A 3380 OR 3390 DASD VOLUME RETURNED BY
     THE DEVTYPE SVC REFLECTS ONLY THE NUMBER OF CYLINDERS AVAILABLE
     ON THE BASE MODEL AND NOT THE EXTENDED MODELS.

     CHANGES MADE BY THIS USERMOD INCLUDE:
       - THE DVACYL FIELD IN THE DEVTAB RESULT AREA, RETURNED BY
         DEVTYPE SVC, NOW CONTAINS THE ACTUAL NUMBER OF PHYSICAL
         CYLINDERS APPROPRIATE FOR THE DASD VOLUME IDENTIFIED BY
         THE DD NAME. THIS INFORMATION IS RETRIEVED FROM THE
         DASD DEVICE BY USE OF THE READ DEVICE CHARACTERISTICS
         CCW COMMAND.
       - THE DVAMODL FLAG IS NOW AVAILABLE IN DVAFLAGS
         TO IDENTIFY MODULO DASD DEVICES WHERE THE MODULO DASD
         TRACK CAPACITY ALGORITHM MUST BE USED.
       - DATA VARIABLES REQUIRED FOR MODULO DASD TRACK CAPACITY
         CALCULATIONS ARE CORRECTLY NAMED.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     SKYBIRD SYSTEMS: PUBLIC DOMAIN USERMOD FOR MVS 3.8

   THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
   MODULES:
       IGC00002D
       IGCT0002D
   MACROS:
       IHADVA
 */.
++MOD(IGC0002D) DISTLIB(AOSD0)   TXLIB(UMODOBJ).
++MOD(IGCT002D) DISTLIB(AOSD0)   TXLIB(UMODOBJ).
++MAC(IHADVA)   DISTLIB(AMACLIB).
* % GOTO DVAL1;                          /* BILINGUAL MAPPING        */ 00000102
         MACRO                                                          00000202
         IHADVA &DSECT=YES                                              00000302
.*                                                                      00000401
.*       STATUS -                                                       00000501
.*       LASTUPD         = EDM1102  TYPE=ADD                            00000601
.*       LIBRARIES       = DISTLIB=AMACLIB                              00000701
.*       FMID            = EDM1102                                      00000801
.*       RMID            = EDM1102                                      00000901
.*       SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS   STATUS      00001001
.*                         EDM1102  FUNCTION  00.000  MAC   ACC RGN     00001101
.*                                                                      00001201
.*       CHANGE ACTIVITY -                                              00001301
.*              RP00003 - 1. ADD FLAG FOR MODULO DASD AND REDEFINE      00001402
.*                           AREAS USED FOR MODULO DASD CALCULATION     00001502
.*                        2. ADD NOTE THAT DVABDCYL FLAG CAN NOW        00001602
.*                           ADDITIONALLY BE SET ON FOR 3380/3390 DASD  00001702
.*                                                                      00001802
.* /* START OF SPECIFICATIONS ****                                      00001902
.*                                                                      00002002
.*01  MODULE-NAME = IHADVA                                              00002102
.*                                                                      00002202
.*01  COPYRIGHT = NONE                                                  00002302
.*                                                                      00002402
.*01  STATUS = VS2 RELEASE 2 LEVEL 0                                    00002502
.*                                                                      00002602
.*01  DESCRIPTIVE-NAME = DEVTYPE PARAMETER AREA                         00002702
.*                                                                      00002802
.*01  FUNCTION = TO MAP THE PARAMETER AREA RETURNED TO THE CALLER       00002902
.*               BY THE DEVTYPE SVC.                                    00003002
.*                                                                      00003102
.*01  MODULE-TYPE = MACRO                                               00003202
.*                                                                      00003302
.*02    MACRO-SIZE = NA FOR MAPPING MACRO                               00003402
.*                                                                      00003502
.*02    PARAMETER-RECEIVED = DSECT                                      00003602
.*                                                                      00003702
.*03     TYPE = CHARACTER STRING                                        00003802
.*                                                                      00003902
.*04       VALUE = YES OR NO                                            00004002
.*                                                                      00004102
.*01  TABLES = NONE                                                     00004202
.*                                                                      00004302
.*01  INNER-MACROS = NONE                                               00004402
.*                                                                      00004502
.*01  SYSTEM-MACROS = NONE                                              00004602
.*                                                                      00004702
.*01  CHANGE-ACTIVITY = SEE BELOW                                       00004802
.*    VS2 RELEASE 2                                                     00004902
.*       MACRO CREATED                                                  00005002
.*                                                                      00005102
.*                                                                      00005202
.**** END OF SPECIFICATIONS ***/                                        00005302
* %DVAL1: ;                                                             00005402
* /* MACCOMP Y-2 SC1D0/SJD48                                         */ 00005502
* /* MACSTAT Y-2 73138/021160                                        */ 00005602
*/********************************************************************/ 00005702
*/*                 DEVTYPE PARAMETER AREA                           */ 00005802
*/*     THIS MACRO MAPS THE PARAMETER AREA RETURNED TO THE CALLER BY */ 00005902
*/*     THE DEVTYPE SVC.                                             */ 00006002
*/*     IN AN ASSEMBLER CALL DSECT=NO MAY BE CODED.                  */ 00006102
*/*                                                                  */ 00006202
*/*     IN A PLS INCLUDE, DVABASE MAY BE DECLARED AS DEF(LABEL)      */ 00006302
*/*     TO CAUSE IT TO BE USED IN A STRUCTURE. IF DVABASE IS         */ 00006402
*/*     OMITTED THE STRUCTURE IS DECLARED AS BASED.                  */ 00006502
*/*                                                                  */ 00006602
*/********************************************************************/ 00006702
* % GOTO DVAL2;                                                         00006802
         AIF   ('&DSECT' EQ 'NO').DVANOD                                00006902
DVAREA   DSECT                          OUTPUT AREA FROM DEVTYPE MACRO  00007002
         AGO   .DVACONT                                                 00007102
.DVANOD  ANOP                                                           00007202
DVAREA   EQU   *                                                        00007302
.DVACONT ANOP                                                           00007402
*                                                                       00007502
*********************************************************************** 00007602
*  THE FIRST TWO FULL WORDS OF THE OUTPUT AREA CONTAIN THE            * 00007702
*  UCBTYPE FIELD AND MAXIMUM RECORD SIZE FOR ALL DEVICES.  THEY ARE   * 00007802
*  FOLLOWED BY THE DEVICE TABLE ENTRY IF THE DEVICE IS DIRECT ACCESS  * 00007902
*  AND IF KEYWORD DEVTAB WAS SPECIFIED.                               * 00008002
*********************************************************************** 00008102
DVAPREFX DS    0CL8                     PREFIX WORDS OF DEVTYPE OUTPUT  00008202
DVAUCBTY DS    0F                       UCBTYPE FIELD                   00008302
DVAOPTS  DS    XL2                      UCB OPTIONS                     00008402
DVACLASS DS    XL1                      DEVICE CLASS                    00008502
DVAUNIT  DS    XL1                      UNIT TYPE                       00008602
DVAMAXRC DS    F                        MAXIMUM RECORD SIZE             00008702
DVADEVLG EQU   *-DVAREA                 LENGTH OF THE DEVTYPE OUTPUT    00008802
*                                       PREFIX                          00008902
*                                                                       00009002
*********************************************************************** 00009102
*                  DEVTAB SECTION FOR DASD DEVICES                    * 00009202
*********************************************************************** 00009302
DVATAB   DS    0CL12                                                    00009402
DVACYL   DS    H                        PHYS NO CYL PER VOLUME          00009502
DVATRK   DS    H                        NO TRACKS PER CYLINDER          00009602
DVATRKLN DS    H                        NO. OF BYTES PER TRACK          00009702
*                                                                       00009802
DVAOVHD  DS    0H                       BLOCK OVERHEAD IF DVA2BOV=1     00009902
*                                       ****************************    00010002
*                                       * IF DVA2BOV=0 USE INSTEAD *    00010102
*                                       * THE FOLLOWING TWO VALUES *    00010202
*                                       ****************************    00010302
         ORG   DVAOVHD                                       @RP00003@  00010402
DVAOVH1  DS    H                        MODULO OVERHEAD 1    @RP00003@  00010502
         ORG   DVAOVHD                                       @RP00003@  00010603
DVAOVNLB DS    XL1                      OVERHEAD NOT LAST BLOCK         00010702
DVAOVLB  DS    XL1                      OVERHEAD LAST BLOCK             00010802
*                                                                       00010902
DVAOVNK  DS    XL1                      OVERHEAD DECREMENT NOT KEYED    00011002
         ORG   DVAOVNK                                       @RP00003@  00011102
DVAMOD1  DS    XL1                      MODULO FACTOR        @RP00003@  00011202
*                                                                       00011302
DVAFLAGS DS    BL1                                                      00011402
DVABDCYL EQU   X'80'                    IF 1, DVACYL IS INVALID YL02130 00011502
*                                       NOTE -                          00011602
*                                       THIS MAY BE SET ON   @RP00003@  00011702
*                                       FOR 3340/3380/3390   @RP00003@  00011802
*                                       DASD IF SENSE OR     @RP00003@  00011902
*                                       READ DEV CHAR IS NOT @RP00003@  00012002
*                                       SUCCESSFUL           @RP00003@  00012102
DVAMODL  EQU   X'10'                    MODULO DASD          @RP00003@  00012202
*                                       USE DVAMOD1, DVAOVH1,@RP00003@  00012302
*                                       DVAOVH2              @RP00003@  00012402
DVA2BOV  EQU   X'08'                    IF 1, USE DVAOVHD               00012502
*                                       IF 0, USE DVAOVNLB,DVAOVLB      00012602
DVAFTOL  EQU   X'01'                    IF 1, APPLY TOLERANCE FACTOR    00012702
*                                                                       00012802
DVATOL   DS    H                        TOLERANCE FACTOR                00012902
*                                 ************************************* 00013002
*                                 * APPLY TOLERANCE FACTOR AS FOLLOWS:* 00013102
*                                 * 1. ADD BLOCKSIZE AND KEYLENGTH    * 00013202
*                                 * 2. MULTIPLY BY DVATOL             * 00013302
*                                 * 3. SHIFT RIGHT DVASHFT BITS       * 00013402
*                                 * 4. ADD APPROPRIATE OVERHEADS      * 00013502
*                                 ************************************* 00013602
         ORG   DVATOL                                        @RP00003@  00013702
DVAOVH2  DS    H                        MODULO OVERHEAD 2    @RP00003@  00013802
DVASHFT  EQU   9                        SHIFT AMT TO DIVIDE BY 512      00013902
*                                                                       00014002
DVAENTLG EQU   *-DVATAB                 BASIC SIZE OF DEVICE TABLE      00014102
*                                       ENTRY, NOT INCLUDING ADDITIONAL 00014202
*                                       CHARACTERISTICS FOR RPS         00014302
*                                                                       00014402
**********************************************************************  00014502
*              THE FOLLOWING SECTION OF THE TABLE IS PRESENT         *  00014602
*              ONLY FOR RPS DEVICES--TEST UCBTBYT2 FOR UCB2OPT3      *  00014702
**********************************************************************  00014802
DVARPS   DS    0CL4                     RPS SECTION                     00014902
DVAOVR0  DS    H                        OVERHEAD BYTES FOR RECORD 0     00015002
DVASECT  DS    XL1                      NR. SECTORS IN FULL TRACK       00015102
DVASECTD DS    XL1                      NR. DATA SECTORS                00015202
         ORG   DVASECTD                                      @RP00003@  00015302
DVABPSEC DS    XL1                      NO BYTES PER SECTOR  @RP00003@  00015402
DVARPSLG EQU   *-DVARPS                 SIZE OF RPS SECTION             00015502
*                                                                       00015602
*              END OF DVA                                               00015702
         MEND                                                           00015802
*% DVAL2: ;                                                             00015902
*        % IF DVABASE = ''                                              00016002
*        %    THEN                                                      00016102
*        %      DVABASE = 'BASED';                                      00016202
*DCL   1 DVAREA DVABASE,                                                00016302
*        2 DVAPREFX,                    /* WORDS PREFIXED BY DEVTYPE */ 00016402
*          3 DVAUCBTY FIXED(32),  /* UCB TYPE FIELD                  */ 00016502
*            4 DVAOPTS BIT(16),   /* UCB OPTIONS                     */ 00016602
*            4 DVACLASS BIT(8),   /* DEVICE CLASS                    */ 00016702
*            4 DVAUNIT BIT(8),    /* UNIT TYPE                       */ 00016802
*          3 DVAMAXRC FIXED(31),  /* MAXIMUM RECORD SIZE             */ 00016902
*                                                                       00017002
*        2 DVATAB,                /* SECTION INCLUDED BY DEVTAB      */ 00017102
*          3 DVACYL FIXED(15),    /* PHYS NUMBER CYL PER VOLUME      */ 00017202
*          3 DVATRK FIXED(15),    /* NR OF TRACKS PER CYL            */ 00017302
*          3 DVATRKLN FIXED(15),  /* TRACK LENGTH ( BYTES)           */ 00017402
*          3 DVAOVHD FIXED(15),   /* BLOCK OVERHEAD IF DVA2BOV IS ON */ 00017502
*                                 /***********************************/ 00017602
*                                 /* IF DVA2BOV IS OFF USE INSTEAD   */ 00017702
*                                 /* THE FOLLOWING TWO VALUES        */ 00017802
*            4 DVAOVNLB PTR(8),   /* OVERHEAD NOT LAST BLOCK         */ 00017902
*            4 DVAOVLB PTR(8),    /* OVERHEAD LAST BLOCK             */ 00018002
*                                 /***********************************/ 00018102
*          3 DVAOVNK PTR(8),      /* OVERHEAD DECR IF NOT KEYED      */ 00018202
*          3 DVAFLAGS BIT(8),     /* FLAG BYTE                       */ 00018302
*            4 DVABDCYL BIT(1),   /* IF 1, DVACYL IS INVALID YL02130 */ 00018402
*            4 * BIT(3),          /* RESERVED                        */ 00018502
*            4 DVA2BOV BIT(1),    /* IF ON, USE DVAOVHD              */ 00018602
*                                 /* ELSE USE DVAOVNLB & DVAOVLB     */ 00018702
*            4 * BIT(2),          /* RESERVED                        */ 00018802
*            4 DVAFTOL BIT(1),    /* IF ON, APPLY TOLERANCE FACTOR   */ 00018902
*                                                                       00019002
*          3 DVATOL FIXED(15),    /* TOLERANCE FACTOR                */ 00019102
*                                 /***********************************/ 00019202
*                                 /* (BLKSI+KEYLE)*DVATOL/DVADVSR    */ 00019302
*                                 /* GIVES THE ADJUSTED BLOCK SIZE   */ 00019402
*                                 /* TO WHICH APPROPRIATE OVERHEADS  */ 00019502
*                                 /* ARE THEN ADDED.                 */ 00019602
*                                 /***********************************/ 00019702
*            % DVADVSR = '512';   /* FRACTION DENOMINATOR FOR DVATOL */ 00019802
*                                                                       00019902
*        2 DVARPS,                /* RPS SECTION                     */ 00020002
*          3 DVAOVR0 FIXED(15),   /* OVERHEAD BYTES FOR RECORD 0     */ 00020102
*          3 DVASECT PTR(8),      /* NUMBER OF SECTORS IN FULL TRACK */ 00020202
*          3 DVASECTD PTR(8);     /* NUMBER OF DATA SECTORS          */ 00020302
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(RP00003)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(RP00003 UZ73568)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(RP00003 UZ73568)
        DIS(WRITE)
        .
/*
//
