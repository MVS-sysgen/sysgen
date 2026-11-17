//RP00001  JOB (SYSGEN),'RP00001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* INSTALL USERMOD RP00001 - ADD 3375/3380/3390 SUPPORT TO AMDPRDMP  *
//* AND DUMP DATA SET CREATION (RP00001+RP00002, ROB PRINS)           *
//*                                                                   *
//* FROM JAY MOSELEY'S INSTALLMVS V8 TAPE/USERMODS.HET                *
//*********************************************************************
//*
//SOURCE   EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  DATA,DLM='??'
./ ADD NAME=IEAVTSDH
         TITLE 'IEAVTSDH - SVC DUMP DATA SET INITIALIZATION'            00000100
*                                                                       00000200
*        STATUS -                                                       00000300
*        LASTUPD         = JCLIN    TYPE=UPD                            00000400
*        LIBRARIES       = DISTLIB=AOSC5                                00000500
*        FMID            = FBB1221                                      00000600
*        RMID            = UZ54567                                      00000700
*        LMOD            = IGC0005A                                     00000800
*        SYSMOD HISTORY  = SYSMOD   TYPE       DATE   MCS   STATUS      00000900
*                          EBB1102  FUNCTION  00.000  MOD       ACC RGN 00001000
*                          FBB1221  FUNCTION  00.000  MOD       ACC RGN 00001100
*                          UZ54567  PTF       74.164  MOD   APP ACC     00001200
*                                                                       00001300
*        SOURCE WAS UPDATED BY DISASSEMBLY TO MATCH THE                 00001400
*        LOAD MODULE AT THE UZ54567 LEVEL.                              00001500
*                                                                       00001600
*        CHANGE ACTIVITY - USERMOD RP00002                              00001700
*        1. REPLACE THE RYO RECORDS PER TRACK CALCULATION WITH THE      00001800
*           TRKCALC SYSTEM SERVICE TO PROVIDE SUPPORT FOR ALL DASD      00001900
*           DEVICE TYPES INCLUDING 3375/3380/3390 MODULO DASD           00002000
*        2. USE OF SYSTEM MAPPING MACROS                                00002100
*        3. NUMEROUS SMALL CHANGES TO ENHANCE SOURCE CODE               00002200
*           READABILITY                                                 00002300
*                                                                       00002400
*        ON ENTRY - INVOKED BY IEAVAD00                                 00002500
*        R3  -> FRR PARAMETER AREA                                      00002600
*        R7  -> SVC DUMP WORK AREA MAPPED BY IHASDWRK                   00002700
*        R13 -> SDWRGSV1 IN SDWORK                                      00002800
*        R15 -> IEAVTSDH                                                00002900
*                                                                       00003000
IEAVTSDH CSECT                                                          00003100
*                                                                       00003200
         SAVE  (14,12),,'IEAVTSDH RP00002 &SYSDATE &SYSTIME'            00003300
*                                                                       00003400
         LA    R9,0(,R15)                                               00003500
         USING IEAVTSDH,R9                                              00003600
         USING PSA,R0                                                   00003700
*   ERRBASE1=R9;                    /* SAVE BASE REGISTER FOR           00003800
*                                      RECOVERY                      */ 00003900
         ST    R9,ERRBASE1(,R3)                                         00004000
*   ERRADDR=ADDR(RTYENDMP);         /* ESTABLISH RETRY ADDRESS       */ 00004100
         LA    R12,RTYENDMP                                             00004200
         ST    R12,ERRADDR(,R3)                                         00004300
*   SVCDMPNM=CSECTNAM;              /* SAVE MODULE NAME FOR RECOVERY */ 00004400
         MVC   SVCDMPNM(8,R7),KEAVTSDH                                  00004500
*   /*****************************************************************/ 00004600
*   /*                                                               */ 00004700
*   /* IF SYSMDUMP IS REQUESTED, BSAM I/O IS USED. NO I/O INIT NEEDS */ 00004800
*   /* TO BE PERFORMED FOR SYSMDUMP REQUESTS                         */ 00004900
*   /*                                                               */ 00005000
*   /*****************************************************************/ 00005100
*                                                                       00005200
*   IF SDUABEND='0'B THEN           /* IF THIS IS NOT A SYSMDUMP        00005300
*                                      REQUEST                       */ 00005400
         L     R12,SDWSDPL(,R7)                                         00005500
         TM    SDUFLAG1-SDUMP(R12),SDUABEND                             00005600
         BNZ   @RF00193                                                 00005700
*     DO;                           /* SELECT A DUMP DATA SET AND       00005800
*                                      INITIALIZE I/O CONTROL BLOCKS */ 00005900
*       SDSMBUFF=ADDR(SDBUFFER);    /* GET ADDRESS OF 4104 BYTE         00006000
*                                      OUTPUT BUFFER FOR PRDMP RECORD   00006100
*                                      ADDRESSING                    */ 00006200
         LA    R8,SDBUFFER(,R7)                                         00006300
         ST    R8,SDSMBUFF(,R7)                                         00006400
*/********************************************************************/ 00006500
*/*   THIS SEGMENT INITIALIZES FOR I/O.  IF THE CALLER DID NOT       */ 00006600
*/*   SUPPLY A DATASET, ONE IS SELECTED FROM THE SYS1.DUMP           */ 00006700
*/*   DATASETS.  THE IOB AND CCW ARE INITIALIED TO REFLECT THE       */ 00006800
*/*   DEVICE (DASD OR TAPE).  INPUT: SDUMP PARM LIST, SYS1.DUMP      */ 00006900
*/*   DATASET TABLE IN RTMCT. OUTPUT: INITIALIZED I/O CONTROL        */ 00007000
*/*   BLOCKS INCLUDING A DUMP DATASET FOR THIS DUMP                  */ 00007100
*/*   INCLUDED BY: TOP SEGMENT                                       */ 00007200
*/********************************************************************/ 00007300
*                                                                       00007400
*       CCWCHAIN=CCWCHAIN&&CCWCHAIN;/* CLEAR CCW CHAIN TO ZERO       */ 00007500
         XC    CCWCHAIN(24,R7),CCWCHAIN(R7)                             00007600
*       SDECB=NULL;                 /* CLEAR ECB TO ZEROES           */ 00007700
         SLR   R8,R8                                                    00007800
         ST    R8,SDECB(,R7)                                            00007900
*       SDIOB=SDIOB&&SDIOB;         /* CLEAR IOB TO ZEROES           */ 00008000
         XC    SDIOB(40,R7),SDIOB(R7)                                   00008100
*       IOBECBPB=ADDR(SDECB);       /* PUT ECB ADDRESS INTO IOB      */ 00008200
         LA    R8,4                                                     00008300
         ALR   R8,R7                   R8 -> IOB                        00008400
         STCM  R7,7,IOBECBPB-IOBSTDRD(R8)                               00008500
*       IOBSTRTB=ADDR(CCWCHAIN);    /* START ADDRESS OF CCW          */ 00008600
         LA    R6,CCWCHAIN(,R7)                                         00008700
         STCM  R6,7,IOBSTRTB-IOBSTDRD(R8)                               00008800
*       IOBUNREL='1'B;              /* I/O REQUEST IS UNRELATED TO      00008900
*                                      ANY OTHER I/O REQUESTS        */ 00009000
         OI    IOBFLAG1-IOBSTDRD(R8),IOBUNREL                           00009100
*       /*************************************************************/ 00009200
*       /*                                                           */ 00009300
*       /* SYSMDUMP REQUESTS WILL ALWAYS HAVE THE DCB OPERAND        */ 00009400
*       /* SPECIFIED AND WILL NOT MODIFY THE RTCT                    */ 00009500
*       /*                                                           */ 00009600
*       /*************************************************************/ 00009700
*                                                                       00009800
*       IF SDUDCB='1'B THEN         /* IF CALLER SUPPLIED A DATASET  */ 00009900
         TM    SDUFLAG0-SDUMP(R12),SDUDCB                               00010000
         BNO   @RF00208                                                 00010100
*         DO;                       /* USE IT                        */ 00010200
*           IOBDCBPB=SDUDCBAD;      /* CHAIN I/O CNTRL BLOCKS        */ 00010300
         MVC   IOBDCBPB-IOBSTDRD(,R8),SDUDCBAD+1-SDUMP(R12)             00010400
         B     @RC00208                                                 00010500
*                                                                       00010600
*         END;                                                          00010700
*       ELSE                                                            00010800
*         DO;                                                           00010900
*           /*********************************************************/ 00011000
*           /*                                                       */ 00011100
*           /* SEARCH FOR UNUSED DUMP DATA SET TO USE FOR THIS DUMP  */ 00011200
*           /*                                                       */ 00011300
*           /*********************************************************/ 00011400
*           DSTINDEX=0;                                                 00011500
@RF00208 MVI   M994REQ(R7),X'00'       INIT MSG FLAG                    00011600
         SLR   R12,R12                                                  00011700
         STH   R12,DSTINDEX(,R7)                                        00011800
*           DSINDEX=0;                                                  00011900
         STH   R12,DSINDEX(,R7)                                         00012000
*       DO R10=1 TO DIM(RTCTSDDS) WHILE(RTCTDSNM(R10)^=0&DSTINDEX<2);   00012100
         LA    R10,1                                                    00012200
@DL00217 LR    R12,R10                                                  00012300
         MH    R12,KH12                                                 00012400
         L     R11,CVTPTR                                               00012500
         L     R11,CVTRTMCT-CVTMAP(,R11)                                00012600
         L     R8,RTCTDSNM-13(R12,R11)                                  00012700
         LA    R8,0(,R8)                                                00012800
         LTR   R8,R8                                                    00012900
         BZ    @DC00217                                                 00013000
         LH    R8,DSTINDEX(,R7)                                         00013100
         N     R8,@CF05273                                              00013200
         C     R8,KF2                                                   00013300
         BNL   @DC00217                                                 00013400
*                                   /* LOOK FOR 2 UNUSED DUMP DATA      00013500
*                                      SETS                          */ 00013600
*             IF RTCTDSUS(R10)='0'B /* IS THIS DS UNUSED             */ 00013700
*               THEN                                                    00013800
         ALR   R12,R11                                                  00013900
         TM    RTCTDSUS-12(R12),B'01000000'                             00014000
         BNZ   @RF00218                                                 00014100
*               DO;                 /* YES                           */ 00014200
*                 IF RTCTDETP(R10)='0'B/* TAPE OR DA?                */ 00014300
*                   THEN            /* TAPE                          */ 00014400
         LR    R12,R10                                                  00014500
         MH    R12,KH12                                                 00014600
         ALR   R11,R12                                                  00014700
         TM    RTCTDETP-12(R11),B'00100000'                             00014800
         BNZ   @RF00220                                                 00014900
*              DSTINDEX=DSTINDEX+2; /* COUNT TAPES AS ENOUGH FOR ALL    00015000
*                                      NEEDS                         */ 00015100
         AL    R8,KF2                                                   00015200
         STH   R8,DSTINDEX(,R7)                                         00015300
         B     @RC00220                                                 00015400
*                                                                       00015500
*                 ELSE              /* DASD                          */ 00015600
*                   DSTINDEX=DSTINDEX+1; /* COUNT DASD AS ONLY GOOD FOR 00015700
*                                      ONE DUMP                      */ 00015800
@RF00220 LA    R12,1                                                    00015900
         LH    R11,DSTINDEX(,R7)                                        00016000
         N     R11,@CF05273                                             00016100
         ALR   R12,R11                                                  00016200
         STH   R12,DSTINDEX(,R7)                                        00016300
*                 IF DSINDEX=0      /* HAS A DS BEEN CHOSEN TO BE       00016400
*                                      USED                          */ 00016500
*                   THEN            /* NO, NONE CHOSEN SO FAR        */ 00016600
@RC00220 LH    R12,DSINDEX(,R7)                                         00016700
         N     R12,@CF05273                                             00016800
         LTR   R12,R12                                                  00016900
         BNZ   @RF00218                                                 00017000
*                   DSINDEX=R10;    /* CHOOSE THIS DS AS THE ONE TO     00017100
*                                      BE USED FOR THIS DUMP         */ 00017200
         STH   R10,DSINDEX(,R7)                                         00017300
*               END;                                                    00017400
*           END;                                                        00017500
@RF00218 AL    R10,KF1                                                  00017600
         C     R10,KF10                                                 00017700
         BNH   @DL00217                                                 00017800
*           IF DSTINDEX<2           /* WILL THERE BE AN UNUSED DUMP     00017900
*                                      DS LEFT AFTER THIS DUMP?         00018000
*             THEN                  /* NO EITHER THERE ARE NO UNSED     00018100
*                                      DS OR THIS DUMP WILL USE THE     00018200
*                                      LAST ONE                      */ 00018300
@DC00217 CLC   DSTINDEX(2,R7),KH2                                       00018400
         BNL   @RF00227                                                 00018500
*             DO;                   /* SO LOOK THROUGH THE USED DS      00018600
*                                      FOR ONES THAT HAVE BECOME        00018700
*                                      AVAILABLE                     */ 00018800
*/* ******************************************************************/ 00018900
*/*    THIS SEGMENT WILL CHECK EACH SYS1.DUMP DATASET AFTER ALL OF   */ 00019000
*/*    THE ALLOCATED DATA SETS HAVE BEEN USED. IF THE DATA SET IS ON */ 00019100
*/*    TAPE IT IS ALWAYS CONSIDERED EMPTY AND CAN BE USED. IF THE    */ 00019200
*/*    DATA SET IS ON DASD, THE FIRST RECORD IS READ. IF THE READ IS */ 00019300
*/*    UNSUCCESSFUL (ECBCC NOT '7F') AND THE UNIT EXCEPTION FLAG     */ 00019400
*/*    IS ON IN THE CSW, THE DATASET IS CONSIDERED EMPTY AND CAN     */ 00019500
*/*    BE USED.  IF ALL ALLOCATED DUMP DATA SETS ARE FULL THE DUMP   */ 00019600
*/*    TERMINATES WITH RETURN CODE 8.  ALSO MESSAGE IEA994A IS       */ 00019700
*/*    WRITTEN TO THE OPERATOR IF A SIMILAR MESSAGE IS NOT ALREADY   */ 00019800
*/*    OUTSTANDING.  IF A DUMP DATA SET IS FOUND AND AN IEA994A      */ 00019900
*/*    MESSAGE IS OUTSTANDING IT IS DELETED                          */ 00020000
*/*                                                                  */ 00020100
*/*    INPUT: DUMP DATA SET TABLE IN RTCT,I/O CONTROL BLOCKS,WTO LIST*/ 00020200
*/*                                                                  */ 00020300
*/*    OUTPUT: EMPTY USABLE DATASET, RETURN CODE IS NULL OR RETURN   */ 00020400
*/*            CODE IS NODUMP IF ALL DEFINED DATASETS ARE FULL.      */ 00020500
*/*                                                                  */ 00020600
*/*    INCLUDED BY: SDFILE                                           */ 00020700
*/* ******************************************************************/ 00020800
*                                                                       00020900
*               IOBCMDCH='1'B;      /* COMMAND CHAINNING             */ 00021000
         LA    R12,4                                                    00021100
         ALR   R12,R7                  R12 -> IOB                       00021200
         USING IOBSTDRD,R12                                             00021300
         OI    IOBFLAG1,IOBCMDCH                                        00021400
*               IOBM=NULLCHAR;      /* RELATIVE EXTENT NUMBER IS 0   */ 00021500
         MVI   IOBM,X'00'                                               00021600
*               IOBR=ONE;           /* FIRST RECORD                  */ 00021700
         MVI   IOBR,X'01'                                               00021800
*               CMDCODE1=SEARCHID;  /* SEARCH IDENTIFIER EQUAL       */ 00021900
         MVI   CMDCODE1(R7),X'31'                                       00022000
*               DATAADR1=ADDR(IOBCC);/* ADDRESS OF CCHHR IN IOB      */ 00022100
         LA    R12,IOBCC                                                00022200
         DROP  R12                                                      00022300
         STCM  R12,7,DATAADR1(R7)                                       00022400
*               CMDC1='1'B;         /* COMMAND CHAINNING             */ 00022500
         OI    CMDC1(R7),B'01000000'                                    00022600
*               BYTECNT1=LENCCHHR;  /* BYTE COUNT IS LEN OF CCHHR    */ 00022700
         MVC   BYTECNT1(2,R7),KH5                                       00022800
*               CMDCODE2=TIC;       /* TRANSFER IN CHANNEL           */ 00022900
         MVI   CMDCODE2(R7),X'08'                                       00023000
*               DATAADR2=ADDR(CCW1);/* ADDRESS OF SEARCH ID EQ CCW   */ 00023100
         LA    R12,CCW1(,R7)                                            00023200
         STCM  R12,7,DATAADR2(R7)                                       00023300
*               CMDCODE3=DRDATA;    /* READ DASD DATA RECORD         */ 00023400
         MVI   CMDCODE3(R7),X'06'                                       00023500
*               DATAADR3=NULL;      /* ZERO DATA ADDR, NO DATA XFER  */ 00023600
         SLR   R12,R12                                                  00023700
         STCM  R12,7,DATAADR3(R7)                                       00023800
*               SLI3='1'B;          /* SUPPRESS INCORRECT LEN IND    */ 00023900
*               SKIP3='1'B;         /* SUPPRESS DATA TRANSFER        */ 00024000
         OI    SLI3(R7),B'00110000'                                     00024100
*               BYTECNT3=DDRCDLEN;  /* LENGTH OF DASD DATA RECORD    */ 00024200
         MVC   BYTECNT3(2,R7),KH4112                                    00024300
*               DO R10=1 TO DIM(RTCTSDDS) WHILE(RTCTDSNM(R10)^=0);      00024400
*                                                                       00024500
*        CYCLE THROUGH ALL THE DEFINED DUMP DATA SETS                   00024600
*                                                                       00024700
         LA    R10,1                                                    00024800
@DL00243 LR    R12,R10                                                  00024900
         MH    R12,KH12                                                 00025000
         L     R11,CVTPTR                                               00025100
         L     R11,CVTRTMCT-CVTMAP(,R11)                                00025200
         L     R8,RTCTDSNM-13(R12,R11)                                  00025300
         LA    R8,0(,R8)                                                00025400
         LTR   R8,R8                                                    00025500
         BZ    @DC00243                                                 00025600
*                 IF RTCTDSUS(R10)='1'B/* IS THIS DS MARKED AS USED     00025700
*                   THEN            /* YES                           */ 00025800
         ALR   R12,R11                                                  00025900
         TM    RTCTDSUS-12(R12),B'01000000'                             00026000
         BNO   @RF00244                                                 00026100
*                   IF RTCTDETP(R10)='0'B/* IS IT A TAPE OR DA DS    */ 00026200
*                     THEN                                              00026300
         LR    R12,R10                                                  00026400
         MH    R12,KH12                                                 00026500
         ALR   R12,R11                                                  00026600
         TM    RTCTDETP-12(R12),B'00100000'                             00026700
         BNZ   @RF00245                                                 00026800
*                     DO;           /* TAPE                          */ 00026900
*                       IF RTCTDSST(R10)='0'B THEN/* IF DATA SET IS     00027000
*                                      NOT FULL                      */ 00027100
         LR    R12,R10                                                  00027200
         MH    R12,KH12                                                 00027300
         ALR   R12,R11                                                  00027400
         TM    RTCTDSST-12(R12),B'10000000'                             00027500
         BNZ   @RF00247                                                 00027600
*                         DO;       /* MARK IT UNUSED AND USE THIS      00027700
*                                      DATA SET                      */ 00027800
*                           RTCTDSUS(R10)='0'B;/* MARK IT UNUSED        00027900
*                                      SINCE TAPES THAT ARE NOT FULL    00028000
*                                      ARE ALWAYS AVAILABLE FOR USE  */ 00028100
         LR    R12,R10                                                  00028200
         MH    R12,KH12                                                 00028300
         ALR   R11,R12                                                  00028400
         NI    RTCTDSUS-12(R11),B'10111111'                             00028500
*                           DSCNT=DSCNT+2;/* INCR THE COUNT OF USABLE   00028600
*                                      DUMP DS BY 2 BECAUSE A TAPE      00028700
*                                      BEING ALWAYS AVAILABLE CAN       00028800
*                                      HANDLE MORE THAN ONE DUMP EVEN   00028900
*                                      IF IT IS THE LAST DS LEFT     */ 00029000
         LA    R12,2                                                    00029100
         LH    R11,DSTINDEX(,R7)                                        00029200
         N     R11,@CF05273                                             00029300
         ALR   R12,R11                                                  00029400
         STH   R12,DSTINDEX(,R7)                                        00029500
*                           IF DSINDEX=0/* HAS A DS BEEN CHOSEN TO BE   00029600
*                                      USED                          */ 00029700
*                             THEN  /* NO, NONE CHOSEN SO FAR        */ 00029800
         LH    R12,DSINDEX(,R7)                                         00029900
         N     R12,@CF05273                                             00030000
         LTR   R12,R12                                                  00030100
         BNZ   @RF00244                                                 00030200
*                             DSINDEX=R10;/* CHOOSE THIS DS AS THE      00030300
*                                      ONE TO BE USED FOR THIS DUMP  */ 00030400
         STH   R10,DSINDEX(,R7)                                         00030500
@RF00247 B     @RF00244                                                 00030600
*                                                                       00030700
*                         END;      /* END OF TAPE DUMP DATA SET NOT    00030800
*                                      FULL                          */ 00030900
*                       ELSE                                            00031000
*                         ;         /* THIS IS A FULL TAPE DUMP DATA    00031100
*                                      SET IGNORE IT                 */ 00031200
*                     END;                                              00031300
*                   ELSE                                                00031400
*                     DO;           /* THIS IS A DASD DATA SET       */ 00031500
*                       SDECB=NULL; /* CLEAR ECB TO ZEROES           */ 00031600
@RF00245 SLR   R12,R12                                                  00031700
         ST    R12,SDECB(,R7)                                           00031800
*                       IOBCSW=IOBCSW&&IOBCSW;/* CLEAR OLD CSW          00031900
*                                      INFORMATION                   */ 00032000
         XC    IOBCSW+4-IOBSTDRD(7,R7),IOBCSW+4-IOBSTDRD(R7)            00032100
*FUL06:                 IOBDCBPB=ADDR(RTCTDCB(R10))-DADCBLEN;/* DCB     00032200
*                                      ADDRESS                       */ 00032300
FUL06    LA    R12,4                                                    00032400
         ALR   R12,R7                                                   00032500
         LR    R11,R10                                                  00032600
         MH    R11,KH12                                                 00032700
         L     R8,CVTPTR                                                00032800
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00032900
         LA    R11,RTCTDCB-12(R11,R8)                                   00033000
         SL    R11,KF44                                                 00033100
         STCM  R11,7,IOBDCBPB-IOBSTDRD(R12)                             00033200
*                       IOBBB=DEBBINUM;/* BIN NUMBER FROM DEB        */ 00033300
         LA    R8,DEBBASND-DEBBASIC                                     00033400
         L     R11,DCBDEBAD-IHADCB(,R11)                                00033500
         LA    R11,0(,R11)                                              00033600
         ALR   R8,R11                     R8 -> DEBDASD                 00033700
         USING DEBDASD,R8                                               00033800
         USING IOBSTDRD,R12                                             00033900
         MVC   IOBBB,DEBBINUM                                           00034000
*                       IOBCC=DEBSTRCC;/* START CLY NUMBER FROM DEB  */ 00034100
         MVC   IOBCC,DEBSTRCC                                           00034200
*                       IOBHH=DEBSTRHH;/* START HEAD NUMBER FROM DEB */ 00034300
         MVC   IOBHH,DEBSTRHH                                           00034400
*                       IOBR=ONE;       /* READ FIRST RECORD         */ 00034500
         MVI   IOBR,X'01'                                               00034600
         DROP  R8,R12                                                   00034700
*                       R1=ADDR(SDIOB);/* ADDRESS OF IOB FOR EXCP    */ 00034800
         LA    R1,SDIOB(,R7)                                            00034900
*                                       /* READ FIRST RCD IN DASD DS */ 00035000
         EXCP  (R1)                                                     00035100
*                       R1=ADDR(SDECB);/* ADDRESS OF ECB FOR WAIT    */ 00035200
         LR    R1,R7                                                    00035300
*                                      /* WAIT FOR READ TO COMPLETE  */ 00035400
         WAIT  1,ECB=(R1)                                               00035500
*FUL08:                                                                 00035600
*                       IF ECBCC^='7F'X&/* IF I/O UNSUCCESSFUL AND   */ 00035700
*                           CHANCCHK='0'B&/* NOT ATTENTION AND       */ 00035800
*                           INTFCCHK='0'B&/* NOT INTERACE CONTROL CHK   00035900
*                                      AND                           */ 00036000
*                           CHANDCHK='0'B&/* NOT CHANNEL DATA CHECK     00036100
*                                      AND                           */ 00036200
*                           UNITCHK='0'B&/* NOT UNIT CHECK AND       */ 00036300
*                           CHAINCHK='0'B&/* NOT CHAINNING CHECK AND */ 00036400
*                           PROGMCHK='0'B&/* NOT PROGRAM CHECK AND   */ 00036500
*                           PROTCHK='0'B&/* NOT PROTECTION CHECK AND */ 00036600
*                           UNTEXCEP='1'B/* IS UNIT EXCEPTION THEN   */ 00036700
*                         THEN                                          00036800
FUL08    CLI   0(R7),X'7F'                ECBCC = 0 ?                   00036900
         BE    @RF00244                                                 00037000
         LA    R12,4                                                    00037100
         ALR   R12,R7                                                   00037200
         USING IOBSTDRD,R12                                             00037300
         TM    IOBCSTAT,IOBCSB4+IOBCSB5+IOBCSB6                         00037400
         BNZ   @RF00244                                                 00037500
         TM    IOBUSTAT,IOBUSB6                                         00037600
         BNZ   @RF00244                                                 00037700
         TM    IOBCSTAT,IOBCSB2+IOBCSB3+IOBCSB7                         00037800
         BNZ   @RF00244                                                 00037900
         TM    IOBUSTAT,IOBUSB7                                         00038000
         BNO   @RF00244                                                 00038100
         DROP  R12                                                      00038200
*                         DO;       /* DASD DATASET IS AVAILABLE        00038300
*                                      (EMPTY)                       */ 00038400
*                           RTCTDSUS(R10)='0'B;/* SO MARK IT AS         00038500
*                                      UNUSED AGAIN                  */ 00038600
         LR    R12,R10                                                  00038700
         MH    R12,KH12                                                 00038800
         L     R11,CVTPTR                                               00038900
         L     R11,CVTRTMCT-CVTMAP(,R11)                                00039000
         ALR   R11,R12                                                  00039100
         NI    RTCTDSUS-12(R11),B'10111111'                             00039200
*                           DSCNT=DSCNT+1;/* INCR THE COUNT OF USABLE   00039300
*                                      DUMP DS                       */ 00039400
         LA    R12,1                                                    00039500
         LH    R11,DSTINDEX(,R7)                                        00039600
         N     R11,@CF05273                                             00039700
         ALR   R12,R11                                                  00039800
         STH   R12,DSTINDEX(,R7)                                        00039900
*                           IF DSINDEX=0/* HAS A DS BEEN CHOSEN TO BE   00040000
*                                      USED                          */ 00040100
*                             THEN  /* NO, NONE CHOSEN SO FAR        */ 00040200
         LH    R12,DSINDEX(,R7)                                         00040300
         N     R12,@CF05273                                             00040400
         LTR   R12,R12                                                  00040500
         BNZ   @RF00244                                                 00040600
*                             DSINDEX=R10;/* CHOOSE THIS DS AS THE      00040700
*                                      ONE TO BE USED FOR THIS DUMP  */ 00040800
         STH   R10,DSINDEX(,R7)                                         00040900
*                         END;                                          00041000
*                     END;                                              00041100
*               END;                /* OF THE LOOP TO CHECK             00041200
*                                      AVAILABLITY OF THE DS         */ 00041300
@RF00244 AL    R10,KF1                                                  00041400
         C     R10,KF10                                                 00041500
         BNH   @DL00243                                                 00041600
*               SDECB=NULL;         /* CLEAR ECB TO ZEROES           */ 00041700
@DC00243 SLR   R12,R12                                                  00041800
         ST    R12,SDECB(,R7)                                           00041900
*               IOBCSW=IOBCSW&&IOBCSW;/* CLEAR OLD CSW INFORMATION   */ 00042000
         LA    R11,4                                                    00042100
         ALR   R11,R7                                                   00042200
         XC    IOBCSW-IOBSTDRD(7,R11),IOBCSW-IOBSTDRD(R11)              00042300
*               CCWCHAIN=CCWCHAIN&&CCWCHAIN;/* CLEAR CCW CHAIN TO       00042400
*                                      ZEROES                        */ 00042500
         XC    CCWCHAIN(24,R7),CCWCHAIN(R7)                             00042600
*               IOBDCBPB=NULL;      /* SET DCB ADDR TO 0             */ 00042700
         STCM  R12,7,IOBDCBPB-IOBSTDRD(R11)                             00042800
*             END;                  /* END OF LOOKING FOR DS THAT       00042900
*                                      HAVE BECOME AVAILABLE         */ 00043000
*           IF DSTINDEX=0 THEN      /* ARE ALL ALLOCATED SYS1.DUMP DS   00043100
*                                      ARE FULL                      */ 00043200
@RF00227 MVI   M994REQ(R7),C'N'        IEAVTSDC NOT TO ISSUE MSG        00043300
         LH    R12,DSTINDEX(,R7)                                        00043400
         N     R12,@CF05273                                             00043500
         LTR   R12,R12                                                  00043600
         BNZ   @RF00282                                                 00043700
*             DO;                                                       00043800
*               ERRENDMP='1'B;      /* INDICATE TERMINAL ERROR       */ 00043900
         OI    ERRENDMP(R3),B'01000000'                                 00044000
*               SDRETCOD=NODUMP;    /* RETURN CODE INDICATES NODUMP  */ 00044100
         MVC   SDRETCOD(4,R7),KF8                                       00044200
*               /*****************************************************/ 00044300
*               /*                                                   */ 00044400
*               /* ISSUE MESSAGE IEA994A TO OPERATOR IF ALL DUMP DATA*/ 00044500
*               /* SETS ARE FULL                                     */ 00044600
*               /*                                                   */ 00044700
*               /*****************************************************/ 00044800
*                                                                       00044900
*               IF SVC994ID=NULL THEN/* IS AN IEA994A MESSAGE           00045000
*                                      OUTSTANDING                   */ 00045100
         L     R12,SVC994ID(,R7)                                        00045200
         LTR   R12,R12                                                  00045300
         BNZ   @RC00282                                                 00045400
         MVI   M994REQ(R7),C'Y'        IEAVTSDC TO ISSUE MSG IEA994A    00045500
         B     @RC00282                                                 00045600
@RF00282 L     R12,SVC994ID(,R7)                                        00045700
         LTR   R12,R12                                                  00045800
         BZ    @RF00310                                                 00045900
         OI    171(R7),X'10'                                            00046000
@RF00310 LA    R12,12                                                   00046100
         LH    R5,DSINDEX(,R7)                                          00046200
         N     R5,@CF05273                                              00046300
         MR    R4,R12                                                   00046400
         L     R11,CVTPTR                                               00046500
         L     R11,CVTRTMCT-CVTMAP(,R11)                                00046600
         ALR   R5,R11                                                   00046700
         OI    RTCTDSUS-12(R5),B'01000000'                              00046800
*               IF RTCTDETP(DSINDEX)='0'B THEN                          00046900
         LH    R8,DSINDEX(,R7)                                          00047000
         N     R8,@CF05273                                              00047100
         MH    R8,KH12                                                  00047200
         ALR   R8,R11                                                   00047300
         TM    RTCTDETP-12(R8),B'00100000'                              00047400
         BNZ   @RF00317                                                 00047500
*                 DO;                                                   00047600
*                   IOBDCBPB=ADDR(RTCTTDCB)-TADCBLEN;                   00047700
         LA    R8,RTCTTDCB(,R11)                                        00047800
         SLR   R8,R12                                                   00047900
         STCM  R8,7,IOBDCBPB+4-IOBSTDRD(R7)                             00048000
*                   DCBDEBA=RTCTDCB(DSINDEX);                           00048100
         LH    R12,DSINDEX(,R7)                                         00048200
         N     R12,@CF05273                                             00048300
         MH    R12,KH12                                                 00048400
         L     R12,RTCTDCB-12(R12,R11)                                  00048500
         STCM  R12,7,DCBDEBA-IHADCB(R8)                                 00048600
*                   RTCTTDCB(1)=0;                                      00048700
         SLR   R12,R12                                                  00048800
         ST    R12,RTCTTDCB(,R11)                                       00048900
         B     @RC00317                                                 00049000
*                                                                       00049100
*                 END;                                                  00049200
*               ELSE                                                    00049300
*                 IOBDCBPB=ADDR(RTCTDCB(DSINDEX))-DADCBLEN;             00049400
@RF00317 LH    R12,DSINDEX(,R7)                                         00049500
         N     R12,@CF05273                                             00049600
         MH    R12,KH12                                                 00049700
         L     R11,CVTPTR                                               00049800
         L     R11,CVTRTMCT-CVTMAP(,R11)                                00049900
         LA    R12,RTCTDCB-12(R12,R11)                                  00050000
         SL    R12,KF44                                                 00050100
         STCM  R12,7,IOBDCBPB+4-IOBSTDRD(R7)                            00050200
*               IF DSTINDEX=1       /* IS THIS THE LAST EMPTY DATASET*/ 00050300
*                 THEN              /* YES IT IS THE LAST EMPTY      */ 00050400
@RC00317 CLC   DSTINDEX(2,R7),KH1                                       00050500
         BNE   @RC00282                                                 00050600
*                 FLGFLAST='1'B;    /* REQUEST WTO MSG IEA994E BE       00050700
*                                      ISSUED                        */ 00050800
         OI    FLGFLAST(R7),B'00100000'                                 00050900
*             END;                  /* END NOT ALL ALLOCATED DUMP       00051000
*                                      DATA SETS FULL                */ 00051100
*           DSTINDEX=0;                                                 00051200
@RC00282 SLR   R12,R12                                                  00051300
         STH   R12,DSTINDEX(,R7)                                        00051400
*         END;                                                          00051500
*                                                                       00051600
*       /*************************************************************/ 00051700
*       /*                                                           */ 00051800
*       /* IF A DUMP DATA SET WAS FOUND INITIALIZE IT                */ 00051900
*       /*                                                           */ 00052000
*       /*************************************************************/ 00052100
*                                                                       00052200
*       IF ERRENDMP='0'B THEN       /* IF A DATASET WAS FOUND,       */ 00052300
@RC00208 TM    ERRENDMP(R3),B'01000000'                                 00052400
         BNZ   @RF00193                                                 00052500
*         DO;                       /* INITIALIZE I/O CNTRL BLOCKS   */ 00052600
*           IF UCB3TAPE='1'B THEN   /* IF DATASET ON TAPE,           */ 00052700
         LA    R12,4                                                    00052800
         ALR   R12,R7                                                   00052900
         L     R8,IOBDCBPT-IOBSTDRD(,R12)                               00053000
         L     R8,DCBDEBAD-IHADCB(,R8)                                  00053100
         L     R8,DEBSUCBA-DEBBASIC(,R8)                                00053200
         TM    UCB3TAPE(R8),B'10000000'  TAPE RESIDENT DATA SET ?       00053300
         BNO   @RF00332                  NO, MUST BE DASD               00053400
*             DO;                                                       00053500
*                                                                       00053600
*        TAPE DATA SET                                                  00053700
*                                                                       00053800
*               IOBINCAM=BLKCNT1;   /* INCREMENT BLK COUNT BY 1      */ 00053900
         MVC   IOBINCAM-IOBSTDRD(,R12),KH1                              00054000
*               CMDCODE1=TWDATA;    /* CMD CODE-WRITE TAPE DATA RCD  */ 00054100
         MVI   CMDCODE1(R7),X'01'                                       00054200
*               SLI1='1'B;          /* SUPPRESS INCORRECT LEN IND    */ 00054300
         OI    SLI1(R7),B'00100000'                                     00054400
*               DATAADR1=ADDR(PRDKEY1);/* ADDR OF START OF TAPE RCD  */ 00054500
         MVC   DATAADR1(3,R7),SDSMBUFF+1(R7)                            00054600
*               BYTECNT1=TDRCDLEN;  /* LENGTH OF TAPE DATA RECORD    */ 00054700
         MVC   BYTECNT1(2,R7),KH4104                                    00054800
         B     @RF00193                                                 00054900
*                                                                       00055000
*             END;                                                      00055100
*           ELSE                    /* DATASET ON DASD               */ 00055200
*             DO;                                                       00055300
*                                                                       00055400
*        DASD DATA SET                                                  00055500
*                                                                       00055600
*               IOBCMDCH='1'B;      /* COMMAND CHAINNING             */ 00055700
@RF00332 LA    R12,4                   INCR -> SDWORK -> SDIOB          00055800
         ALR   R12,R7                                                   00055900
         USING IOBSTDRD,R12                                             00056000
         OI    IOBFLAG1,IOBCMDCH                                        00056100
*               IOBM=NULLCHAR;      /* RELATIVE EXTENT NUMBER IS 0   */ 00056200
         MVI   IOBM,X'00'                                               00056300
*               IOBBB=DEBBINUM;     /* BIN NUMBER FROM DEB           */ 00056400
         L     R8,IOBDCBPT              R8 -> DCB                       00056500
         L     R8,DCBDEBAD-IHADCB(,R8)  R8 -> DEBBASIC                  00056600
         LA    R8,0(,R8)                CLEAR HIGH ORDER BYTE           00056700
         LA    R6,DEBBASND-DEBBASIC                                     00056800
         ALR   R6,R8                    R6 -> DEB                       00056900
         USING DEBDASD,R6                                               00057000
         MVC   IOBBB,DEBBINUM                                           00057100
*               IOBCC=DEBSTRCC;     /* START CYL NUMBER FROM DEB     */ 00057200
         MVC   IOBCC,DEBSTRCC                                           00057300
*               IOBHH=DEBSTRHH;     /* START HEAD NUMBER FROM DEB    */ 00057400
         MVC   IOBHH,DEBSTRHH                                           00057500
         DROP  R6                                                       00057600
*               IOBR=NULLCHAR;      /* FIRST RECORD TO BE WRITTEN    */ 00057700
         MVI   IOBR,X'00'                                               00057800
*               RCDCC=IOBCC;        /* COUNT FIELD CYL - CYL         */ 00057900
         MVC   RCDCC(2,R7),IOBCC                                        00058000
*               RCDHH=IOBHH;        /* COUNT FIELD HEAD - HEAD       */ 00058100
         MVC   RCDHH(2,R7),IOBHH                                        00058200
*               RECORDNO=NULLCHAR;  /* COUNT FIELD - RECORD ZERO     */ 00058300
         MVI   RECORDNO(R7),X'00'                                       00058400
*               CMDCODE1=SEARCHID;  /* CMD CODE-SEARCH ID EQUAL      */ 00058500
         MVI   CMDCODE1(R7),X'31'                                       00058600
*               DATAADR1=ADDR(IOBCC);/* ADDR OF CCHHR IN IOB         */ 00058700
         LA    R12,IOBCC                                                00058800
         DROP  R12                                                      00058900
         STCM  R12,7,DATAADR1(R7)                                       00059000
*               CMDC1='1'B;         /* FLAGS IS COMMAND CHAIN        */ 00059100
         OI    CMDC1(R7),B'01000000'                                    00059200
*               BYTECNT1=LENCCHHR;  /* BYTE COUNT IS LEN OF CCHHR    */ 00059300
         MVC   BYTECNT1(2,R7),KH5                                       00059400
*               CMDCODE2=TIC;       /* CMD CODE-TRANSFER IN CHANNEL  */ 00059500
         MVI   CMDCODE2(R7),X'08'                                       00059600
*               DATAADR2=ADDR(CCW1);/* DATA ADDR-ADDR OF SEARCH CCW  */ 00059700
         LA    R12,CCW1(,R7)                                            00059800
         STCM  R12,7,DATAADR2(R7)                                       00059900
*               CMDCODE3=DWDATA;    /* CMD CODE-WRITE DASD DATA RCD  */ 00060000
         MVI   CMDCODE3(R7),X'1D'                                       00060100
*               DATAADR3=ADDR(SDOUTBUF);/* ADDR OF START OF DASD RCD */ 00060200
         LA    R12,SDOUTBUF(,R7)                                        00060300
         STCM  R12,7,DATAADR3(R7)                                       00060400
*               BYTECNT3=DDRCDLEN;  /* LENGTH OF DASD DATA RECORD    */ 00060500
         MVC   BYTECNT3(2,R7),KH4112                                    00060600
*               KEYLEN=NULLCHAR;    /* KEY LENGTH 0, NO KEYS         */ 00060700
         MVI   KEYLEN(R7),X'00'                                         00060800
*               DATALEN=DADATALN;   /* DATA LENTH IN DATA RECORD     */ 00060900
         L     R12,KF4104                                               00061000
         STH   R12,DATALEN(,R7)                                         00061100
*                                                                       00061200
*        /***************************************************/ @RP00002 00061300
*        /*                                                 */ @RP00002 00061400
*        /* USE THE TRKCALC SYSTEM SERVICE TO DETERMINE THE */ @RP00002 00061500
*        /* NUMBER OF DUMP RECORDS TO BE WRITTEN PER TRACK  */ @RP00002 00061600
*        /*                                                 */ @RP00002 00061700
*        /* SET THE NUMBER OF TRACKS PER CYLINDER FROM      */ @RP00002 00061800
*        /* THE DEVICE CHARACTERISTICS TABLE ENTRY FOR      */ @RP00002 00061900
*        /* THE DASD TYPE                                   */ @RP00002 00062000
*        /*                                                 */ @RP00002 00062100
*        /***************************************************/ @RP00002 00062200
*                                                              @RP00002 00062300
*        LOCATE DEVICE CHARACTERISTICS TABLE ENTRY             @RP00002 00062400
*        FOR THIS DASD TYPE                                    @RP00002 00062500
*                                                              @RP00002 00062600
         L     R6,CVTPTR                                       @RP00002 00062700
         L     R6,CVTZDTAB-CVTMAP(,R6)                         @RP00002 00062800
         L     R8,DEBSUCBA-DEBBASIC(,R8)    R8 -> UCB FROM DEB @RP00002 00062900
         L     R8,UCBTYP(,R8)                                  @RP00002 00063000
         N     R8,KX00000F                  ISOLATE DASD MODEL @RP00002 00063100
         SLR   R2,R2                                           @RP00002 00063200
         IC    R2,DVCTIOFF-DVCTI(R8,R6)                        @RP00002 00063300
         ALR   R2,R6                        R2 -> DVCT ENTRY   @RP00002 00063400
*                                                              @RP00002 00063500
*        OBTAIN TRACKS PER CYLINDER                            @RP00002 00063600
*                                                              @RP00002 00063700
         MVC   SDTRCMAX(2,R7),DVCTRK-DVCT(R2)                  @RP00002 00063800
*                                                              @RP00002 00063900
*        TRKCALC USES REGS R0, R1, R9, R10, R11, R14, R15      @RP00002 00064000
*        AND REQUIRES A 12 BYTE PARAMETER LIST                 @RP00002 00064100
*        IEAVTSDH DOES NOT PROVIDE A SAVE AREA TO USE          @RP00002 00064200
*        12 BYTES OF THE IOB ARE BORROWED FOR USE AS THE       @RP00002 00064300
*        TRKCALC PARAMETER AREA                                @RP00002 00064400
*                                                              @RP00002 00064500
*        SAVE IOBSEEK                                          @RP00002 00064600
*                                                              @RP00002 00064700
         LA    R1,IOBINCAM-IOBSTDRD+SDIOB(,R7)                 @RP00002 00064800
         LM    R4,R5,4(R1)                   SAVE IOBSEEK      @RP00002 00064900
         XC    0(12,R1),0(R1)                ZERO PARAM AREA   @RP00002 00065000
         ICM   R12,B'1000',KF1+3             SET R TO X'01'    @RP00002 00065100
         LR    R8,R9                         SAVE BASE REG     @RP00002 00065200
*                                                              @RP00002 00065300
         TRKCALC FUNCTN=TRKCAP,DEVTAB=(R2),RKDD=(R12),         @RP00002C00065400
               REGSAVE=NO,MF=(E,(1))                           @RP00002 00065500
*                                                              @RP00002 00065600
         LR    R9,R8                        RESTORE BASE REG   @RP00002 00065700
         STH   R0,SDRCTMAX(,R7)             SAVE BLKS PER TRACK@RP00002 00065800
         LA    R1,IOBINCAM-IOBSTDRD+SDIOB(,R7)                 @RP00002 00065900
         XC    0(4,R1),0(R1)                ZERO IOBINCAM/ERRCT@RP00002 00066000
         STM   R4,R5,4(R1)                  RESTORE IOBSEEK    @RP00002 00066100
*             END;                                                      00066200
*         END;                                                          00066300
*     END;                          /* END OF SELECT A DUMP DATA SET */ 00066400
*                                                                       00066500
*   /*****************************************************************/ 00066600
*   /*                                                               */ 00066700
*   /* IF DUMP DATA SET FOUND AND THE 4K BUFFER IS REQUESTED PUT THE */ 00066800
*   /* 4K BUFFER ON THE DUMP DATA SET FIRST                          */ 00066900
*   /*                                                               */ 00067000
*   /*****************************************************************/ 00067100
*                                                                       00067200
*   ERRADDR=ADDR(RTYTOHDR);         /* RETRY ADDR FOR 4K BUF         */ 00067300
@RF00193 LA    R12,RTYTOHDR                                             00067400
         ST    R12,ERRADDR(,R3)                                         00067500
*   IF ERRENDMP='0'B&               /* IF A DUMP DATA SET IS            00067600
*                                      AVAILABLE                     */ 00067700
*       SDUBUF='1'B THEN            /* AND THE 4K BUFFER REQUESTED   */ 00067800
         TM    ERRENDMP(R3),B'01000000'                                 00067900
         BNZ   RTYTOHDR                                                 00068000
         L     R12,SDWSDPL(,R7)                                         00068100
         TM    SDUFLAG0-SDUMP(R12),SDUBUF                               00068200
         BNO   RTYTOHDR                                                 00068300
*     DO;                           /* DUMP SDUMP 4K BUFFER          */ 00068400
*       R10=ADDR(PRDDATA);          /* GET OUTPUT BUFFER ADDR FOR       00068500
*                                      MOVE                          */ 00068600
         L     R12,SDSMBUFF(,R7)                                        00068700
         LA    R10,PRDDATA(,R12)                                        00068800
*       R11=FOURK;                  /* LENGTH OF DATA                */ 00068900
         L     R8,KF4096                                                00069000
         LR    R11,R8                                                   00069100
*       R4=CVTSDBF&HIGHBIT0;        /* 4K SQA BUFFER ADDR FOR MOVE      00069200
*                                      FROM                          */ 00069300
         L     R6,CVTPTR                                                00069400
         L     R4,CVTSDBF-CVTMAP(,R6)                                   00069500
         N     R4,KX7FFFFF                                              00069600
*       PRDASID=COMNASID;           /* PUT COMMON AREA FFFF ASID IN     00069700
*                                      HEADER OF RECORD              */ 00069800
         MVC   PRDASID(2,R12),KXFFFF                                    00069900
*       PRDADDR=R4;                 /* DATA ADDRESS INTO DUMP RECORD */ 00070000
         ST    R4,PRDADDR(,R12)                                         00070100
*       R5=FOURK;                   /* LENGTH OF DATA                */ 00070200
         LR    R5,R8                                                    00070300
*                                   /* MOVE SQA BUFFER TO OUTPUT        00070400
*                                      BUFFER                        */ 00070500
         MVCL   R10,R4                                                  00070600
*       R10=ADDR(PRDKEY1);          /* ADDRESS OF STORAGE KEY SLOT   */ 00070700
         L     R12,SDSMBUFF(,R7)                                        00070800
         LR    R10,R12                                                  00070900
*       R11=ADDR(PRDKEY2);          /* ADDRESS OF SECOND STORAGE KEY */ 00071000
         LA    R11,PRDKEY2(,R12)                                        00071100
*       R4=CVTSDBF&HIGHBIT0;        /* ADDRESS OF FIRST 2K OF DATA   */ 00071200
         L     R12,CVTPTR                                               00071300
         L     R4,CVTSDBF-CVTMAP(,R12)                                  00071400
         N     R4,KX7FFFFF                                              00071500
*       R5=R4+TWOK;                 /* ADDRESS OF SECOND 2K OF DATA  */ 00071600
         LA    R5,2048                                                  00071700
         ALR   R5,R4                                                    00071800
         LRA   R4,0(R4)                REAL ADDRESS OF 1SR 2K OF DATA   00071900
         ISK   R4,R4                   STORAGE KEY OF FIRST 2K OF DATA  00072000
         STC   R4,0(R10)               STORAGE KEY INTO DATA RECORD     00072100
         LRA   R5,0(R5)                REAL ADDR OF SECOND 2K OF DATA   00072200
         ISK   R5,R5                   STORAGE KEY OF SECOND 2K OF DATA 00072300
         STC   R5,0(R11)               STORAGE KEY INTO DATA RECORD     00072400
*       R13=ADDR(SDWRGSV2);         /* GET SAVEAREA ADDRESS          */ 00072500
         LA    R13,SDWRGSV2(,R7)                                        00072600
*       CALL IEAVTSDP;              /* WRITE OUT DATA RECORD         */ 00072700
         L     R15,@CV05110                                             00072800
         BALR  R14,R15                                                  00072900
*       ERRBASE1=R9;                /* RESTORE BASE REGISTER FOR        00073000
*                                      RECOVERY                      */ 00073100
         ST    R9,ERRBASE1(,R3)                                         00073200
*       SVCDMPNM=CSECTNAM;          /* RESTORE CSECT NAME FOR           00073300
*                                      RECOVERY                      */ 00073400
         MVC   SVCDMPNM(8,R7),KEAVTSDH                                  00073500
*       IF ERRENDMP='1'B THEN       /* IF ERR OR WRITING BUFFER      */ 00073600
         TM    ERRENDMP(R3),B'01000000'                                 00073700
         BNO   @RF00387                                                 00073800
*         SDRETCOD=NODUMP;          /* INDICATE NO DUMP TAKEN        */ 00073900
         MVC   SDRETCOD(4,R7),KF8                                       00074000
         B     RTYTOHDR                                                 00074100
*                                                                       00074200
*       ELSE                                                            00074300
*         ERRCDOUT='1'B;            /* INDICATE AT LEAST 1 RECORD       00074400
*                                      DUMPED                        */ 00074500
@RF00387 OI    ERRCDOUT(R3),B'00000001'                                 00074600
*     END;                                                              00074700
*RTYTOHDR:                                                              00074800
*   ERRADDR=ADDR(RTYEND10);         /* IF ERROR HERE, GET OUT        */ 00074900
RTYTOHDR LA    R12,RTYEND10                                             00075000
         ST    R12,ERRADDR(,R3)                                         00075100
*   IF ERRENDMP='0'B THEN           /* IF NO ERRORS FOUND SO FAR     */ 00075200
         TM    ERRENDMP(R3),B'01000000'                                 00075300
         BNZ   @RF00392                                                 00075400
*     DO;                                                               00075500
*                                                                       00075600
*/********************************************************************/ 00075700
*/*   THIS SEQMENT BUILDS THE HEADER RECORD (FOR AMDPRDMP) IN        */ 00075800
*/*   THE OUTPUT BUFFER AND CALLS IEAVTSDP TO WRITE IT OUT.          */ 00075900
*/*                                                                  */ 00076000
*/*   INPUT: USER DATA, POINTED TO FROM SDUMP PARAMETER LIST         */ 00076100
*/*                                                                  */ 00076200
*/*   OUTPUT: HEADER RECORD WRITTEN TO DUMP DATASET OR NODUMP        */ 00076300
*/*           RETURN CODE.                                           */ 00076400
*/*                                                                  */ 00076500
*/*   INCLUDED BY: TOP SEGMENT                                       */ 00076600
*/********************************************************************/ 00076700
*                                                                       00076800
*       PRDHDRID=PRDHDR;            /* IDENTIFY HEADER RECORD        */ 00076900
         L     R12,SDSMBUFF(,R7)                                        00077000
         MVI   PRDHDRID(R12),X'FF'                                      00077100
*       PRDRECID=PRDHDR;            /* RECORD IDENTIFIR IS HEADERID  */ 00077200
         MVI   PRDRECID(R12),X'FF'                                      00077300
*       PRDASID=ASCBASID;           /* ASID OF ADDR SPACE DUMPING    */ 00077400
         L     R8,PSAAOLD                                               00077500
         MVC   PRDASID(2,R12),ASCBASID(R8)                              00077600
*       IF ERRCSDT='1'B THEN        /* IF MODULE IEAVTSDT WAS CALLER */ 00077700
         TM    ERRCSDT(R3),B'00010000'                                  00077800
         BNO   @RF00402                                                 00077900
*         PRDMODNM=SDTNAME;         /* PUT IEAVTSDT NAME IN HEADER   */ 00078000
         MVC   PRDMODNM(8,R12),@CC05046                                 00078100
         B     @RC00402                                                 00078200
*                                                                       00078300
*       ELSE                                                            00078400
*         IF SDUABEND='1'B THEN     /* SYSMDUMP CALLER               */ 00078500
@RF00402 L     R12,SDWSDPL(,R7)                                         00078600
         TM    SDUFLAG1-SDUMP(R12),SDUABEND                             00078700
         BNO   @RF00404                                                 00078800
*           PRDMODNM=SYSMNAME;      /* YES... PUT SYSMDUMP NAME IN      00078900
*                                      HEADER                        */ 00079000
         L     R12,SDSMBUFF(,R7)                                        00079100
         MVC   PRDMODNM(8,R12),@CC05094                                 00079200
         B     @RC00402                                                 00079300
*                                                                       00079400
*         ELSE                                                          00079500
*           IF RBLINKB->RBCDE1=NULL THEN/* IF CALLER DOES NOT HAVE A    00079600
*                                      CDE OR LPDE                   */ 00079700
@RF00404 L     R12,PSATOLD                                              00079800
         L     R12,TCBRBP(,R12)                                         00079900
         L     R12,RBLINKB-1(,R12)                                      00080000
         L     R12,RBCDE1-1(,R12)                                       00080100
         LA    R12,0(,R12)                                              00080200
         LTR   R12,R12                                                  00080300
         BNZ   @RF00406                                                 00080400
*             PRDMODNM=SVCDNAME;    /* CALLERS NAME IN HEADER        */ 00080500
         L     R12,SDSMBUFF(,R7)                                        00080600
         MVC   PRDMODNM(8,R12),@CC05092                                 00080700
         B     @RC00402                                                 00080800
*                                                                       00080900
*           ELSE                                                        00081000
*             IF RBLINKB->RBTRSVRB='0'B THEN/* IF CALLER HAS A CDE      00081100
*                                      GET HIS NAME FROM THE CDE     */ 00081200
@RF00406 L     R12,PSATOLD                                              00081300
         L     R12,TCBRBP(,R12)                                         00081400
         L     R12,RBLINKB-1(,R12)                                      00081500
         LA    R12,0(,R12)                                              00081600
         TM    RBTRSVRB(R12),B'00010000'                                00081700
         BNZ   @RF00408                                                 00081800
*               PRDMODNM=RBLINKB->RBCDE1->CDNAME;/* GET NAME OF         00081900
*                                      CALLER FROM CDE               */ 00082000
         L     R8,SDSMBUFF(,R7)                                         00082100
         L     R12,RBCDE1-1(,R12)                                       00082200
         MVC   PRDMODNM(8,R8),CDNAME(R12)                               00082300
         B     @RC00402                                                 00082400
*                                                                       00082500
*             ELSE                                                      00082600
*               PRDMODNM=RBLINKB->RBCDE1->LPDENAME;/* OBTAIN CALLER     00082700
*                                      NAME FROM LPDE                */ 00082800
@RF00408 L     R12,SDSMBUFF(,R7)                                        00082900
         L     R8,PSATOLD                                               00083000
         L     R8,TCBRBP(,R8)                                           00083100
         L     R8,RBLINKB-1(,R8)                                        00083200
         L     R8,RBCDE1-1(,R8)                                         00083300
         MVC   PRDMODNM(8,R12),LPDENAME(R8)                             00083400
*       PRDPSW=RBLINKB->RBOPSW;     /* CALLERS PSW                   */ 00083500
@RC00402 L     R12,SDSMBUFF(,R7)                                        00083600
         L     R8,PSATOLD                                               00083700
         L     R8,TCBRBP(,R8)                                           00083800
         L     R6,RBLINKB-1(,R8)                                        00083900
         MVC   PRDPSW(8,R12),RBOPSW(R6)                                 00084000
*       PRDGPR=RBGRSAVE;            /* CALLERS REGISTERS INTO HDR    */ 00084100
         MVC   PRDGPR(64,R12),RBGRSAVE(R8)                              00084200
*       R10=ADDR(PRDFPR);           /* FLT PT REG FIELD IN HDR RCD   */ 00084300
         LA    R10,PRDFPR(,R12)                                         00084400
*       R11=ADDR(PRDCR);            /* CNTRL REG FIELD IN HDR RCD    */ 00084500
         LA    R11,PRDCR(,R12)                                          00084600
*                                   /* FLT PT, CNTRL REGS INTO HDR   */ 00084700
         STD   0,0(R10)                FLOAT POINT REG 0 INTO HEADER    00084800
         STD   2,8(R10)                FLOAT POINT REG 2 INTO HEADER    00084900
         STD   4,16(R10)               FLOAT POINT REG 4 INTO HEADER    00085000
         STD   6,24(R10)               FLOAT POINT REG 6 INTO HEADER    00085100
         STCTL 0,15,0(R11)             CONTROL REGISTERS INTO HEADER    00085200
*       R10=ADDR(PRDTODVL);         /* ADDR TO STORE CLOCK INTO      */ 00085300
         L     R10,SDSMBUFF(,R7)                                        00085400
         LA    R10,PRDTODVL(,R10)                                       00085500
*                                   /* TIME OF DAY CLOCK VALUE       */ 00085600
         STCK  0(R10)                                                   00085700
*       LCLTODVL=LCLTODVL+CVTTZ;    /* ADD ADJUSTMENT FOR LOCAL TIME */ 00085800
         L     R12,SDSMBUFF(,R7)                                        00085900
         L     R8,LCLTODVL(,R12)                                        00086000
         L     R6,CVTPTR                                                00086100
         AL    R8,CVTTZ-CVTMAP(,R6)                                     00086200
         ST    R8,LCLTODVL(,R12)                                        00086300
*       PRDTITLE=' ';               /* CLEAR TITLE TO BLANKS         */ 00086400
         MVI   PRDTITLE+1(R12),C' '                                     00086500
         MVC   PRDTITLE+2(98,R12),PRDTITLE+1(R12)                       00086600
         MVI   PRDTITLE(R12),C' '                                       00086700
*       ERRADDR=ADDR(RTYHDR50);     /* NEXT RETRY LOCATION ADDRESS   */ 00086800
         LA    R8,RTYHDR50                                              00086900
         ST    R8,ERRADDR(,R3)                                          00087000
*       IF SDUHDR='1'B THEN         /* IF USER DATA SUPPLIED         */ 00087100
         L     R8,SDWSDPL(,R7)                                          00087200
         TM    SDUFLAG0-SDUMP(R8),SDUHDR                                00087300
         BNO   RTYHDR50                                                 00087400
*         PRDTITLE(1:SDUHDRLN)=     /* TITLE IS CALLER SUPPLIED      */ 00087500
*             SDUSERDA(1:SDUHDRLN); /* USER DATA                     */ 00087600
         L     R8,SDUHDRAD-SDUMP(,R8)                                   00087700
         SLR   R6,R6                                                    00087800
         IC    R6,0(,R8)               R6 = L'TITLE                     00087900
         BCTR  R6,0                                                     00088000
         EX    R6,@SM05278             MVC PRDTITLE(0,R12),1(R8)        00088100
*RTYHDR50:                                                              00088200
*       ERRADDR=ADDR(RTYEND10);     /* NEXT RETRY LOCATION ADDRESS   */ 00088300
RTYHDR50 LA    R12,RTYEND10                                             00088400
         ST    R12,ERRADDR(,R3)                                         00088500
*       PRDCVT=CVTPTR;              /* ADDRESS OF CVT                */ 00088600
         L     R12,SDSMBUFF(,R7)                                        00088700
         L     R8,CVTPTR                                                00088800
         ST    R8,PRDCVT(,R12)                                          00088900
*       IF SDUNEW='0'B THEN         /* IF OLD PARAMETER LIST         */ 00089000
         L     R8,SDWSDPL(,R7)                                          00089100
         TM    SDUFLAG1-SDUMP(R8),SDUNEW                                00089200
         BNZ   @RF00428                                                 00089300
*         SDPRDLST(1:24)=SDUMP(1:24);/* MOVE OLD PARM LIST TO HEADER */ 00089400
         MVC   SDPRDLST(24,R12),0(R8)                                   00089500
         B     @RC00428                                                 00089600
*                                                                       00089700
*       ELSE                                                            00089800
*         SDPRDLST=SDUMP;           /* MOVE NEW PARM LIST TO HEADER  */ 00089900
@RF00428 L     R12,SDSMBUFF(,R7)                                        00090000
         L     R8,SDWSDPL(,R7)                                          00090100
         MVC   SDPRDLST(40,R12),0(R8)                                   00090200
*       SDPRDOPT=SVCDOPTS;          /* PUT SDATA OPTIONS USED INTO      00090300
*                                      AMDPRDMP HEADER RECORD        */ 00090400
@RC00428 L     R12,SDSMBUFF(,R7)                                        00090500
         MVC   SDPRDOPT(2,R12),SVCDOPTS(R7)                             00090600
*                                                                       00090700
*/********************************************************************/ 00090800
*/*  THIS SEGMENT MOVES THE ERRORID FIELD IN THE RTCT INTO THE       */ 00090900
*/*  PRINT DUMP HEADER RECORD.  IF THIS IS A SCHEDULED DUMP          */ 00091000
*/*  REQUEST, THE THE ERROR ID HAS ALREADY BEEN PLACED IN THE        */ 00091100
*/*  RTCT BY IEAVTSDX. OTHERWISE,                                    */ 00091200
*/*  IF TCBRTM2 ='1' B AND THE CURRENT TCB CONTAINS A POINTER        */ 00091300
*/*  TO RTM2WA, THEN THE ERRORID IS MOVED FROM RTM2WA TO             */ 00091400
*/*  THE RTCT.  IF NEITHER OF THE ABOVE IS TRUE, THEN                */ 00091500
*/*  ERRORID IN THE RTCT  ZEROED. THE CONTENTS OF ERRORID IS         */ 00091600
*/*  MOVED TO THE HEADER RECORD.                                     */ 00091700
*/*                                                                  */ 00091800
*/*  INPUT: SDWORK, DUMP OPTIONS                                     */ 00091900
*/*         SVC 51 SVRB                                              */ 00092000
*/*                                                                  */ 00092100
*/*  OUTPUT: PRDERRID CONTAINS ERRORID OR ZEROES.                    */ 00092200
*/*                                                                  */ 00092300
*/*  INCLUDED BY: SDHDR                                              */ 00092400
*/*                                                                  */ 00092500
*/********************************************************************/ 00092600
*                                                                       00092700
*       IF RTCTSDSH=ON THEN         /* IF THIS WAS A SCHEDULED DUMP     00092800
*                                      REQUEST THEN IEAVTSDX HAS        00092900
*                                      ALREADY PUT THE ERRORID IN THE   00093000
*                                      RTCT                          */ 00093100
         L     R8,CVTPTR                                                00093200
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00093300
         TM    RTCTSDSH(R8),B'00100000'                                 00093400
         BNO   @RF00432                                                 00093500
*         PRDERRID=ERRORID;         /* MOVE ERRORID OR ZEROS TO THE     00093600
*                                      HEADER RECORD                 */ 00093700
         MVC   PRDERRID(10,R12),ERRORID(R8)                             00093800
         B     @RC00432                                                 00093900
*                                                                       00094000
*       ELSE                        /* OTHERWISE, SEE IF AN          */ 00094100
*         DO;                       /* ERRORID EXISTS                */ 00094200
*           IF PSATOLD^=NULL THEN   /* IF CURRENT TCB PTR VALID      */ 00094300
@RF00432 L     R12,PSATOLD                                              00094400
         SLR   R8,R8                                                    00094500
         CR    R12,R8                                                   00094600
         BE    @RC00432                                                 00094700
*             DO;                   /* CHECK FOR SYSMDUMP AND ERRORID*/ 00094800
*               IF SDUABEND=ON THEN /* IF THIS A SYSMDUMP ENTRY      */ 00094900
         L     R6,SDWSDPL(,R7)                                          00095000
         TM    SDUFLAG1-SDUMP(R6),SDUABEND                              00095100
         BNO   @RF00439                                                 00095200
*                 DO;               /* GET ERRORID BUT DONT TOUCH       00095300
*                                      RTCT                          */ 00095400
*                   IF(TCBRTM2=ON)& /* IF RTM2 WAS ENTERED AND       */ 00095500
*                       (TCBRTWA^=NULL)&/* RTM2WA POINTER VALID      */ 00095600
*                       (RTM2ERFL=ON) THEN/* AND ERRORID GENERATED   */ 00095700
         TM    TCBRTM2(R12),B'00001000'                                 00095800
         BNO   @RF00441                                                 00095900
         L     R12,TCBRTWA(,R12)                                        00096000
         CR    R12,R8                                                   00096100
         BE    @RF00441                                                 00096200
         TM    RTM2ERFL(R12),B'00000001'                                00096300
         BNO   @RF00441                                                 00096400
*                     PRDERRID=RTM2ERID;/* MOVE ERRORID TO HEADER    */ 00096500
         L     R8,SDSMBUFF(,R7)                                         00096600
         MVC   PRDERRID(10,R8),RTM2ERID(R12)                            00096700
         B     @RC00432                                                 00096800
*                                                                       00096900
*                   ELSE                                                00097000
*                     PRDERRID=''B; /* NO ERRORID ZERO HEADER        */ 00097100
@RF00441 L     R12,SDSMBUFF(,R7)                                        00097200
         XC    PRDERRID(10,R12),PRDERRID(R12)                           00097300
         B     @RC00432                                                 00097400
*                                                                       00097500
*                 END;              /* END OF SYSMDUMP REQUEST       */ 00097600
*               ELSE                                                    00097700
*                 DO;               /* NOT A SYSMDUMP REQUEST        */ 00097800
*                   IF(TCBRTM2=ON)& /* IF RTM2 WAS ENTERED           */ 00097900
*                       (TCBRTWA^=NULL)&/* RTM2WA PTR VALID          */ 00098000
*                       (RTM2ERFL=ON) THEN/* AND ERRORID EXISTS      */ 00098100
@RF00439 L     R12,PSATOLD                                              00098200
         TM    TCBRTM2(R12),B'00001000'                                 00098300
         BNO   @RF00446                                                 00098400
         L     R12,TCBRTWA(,R12)                                        00098500
         LTR   R12,R12                                                  00098600
         BZ    @RF00446                                                 00098700
         TM    RTM2ERFL(R12),B'00000001'                                00098800
         BNO   @RF00446                                                 00098900
*                     ERRORID=RTM2ERID;/* MOVE ERRORID TO RTCT       */ 00099000
         L     R8,CVTPTR                                                00099100
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00099200
         MVI   234(R8),X'40'                                            00099300
         MVI   235(R8),X'40'                                            00099400
         MVC   ERRORID(10,R8),RTM2ERID(R12)                             00099500
         B     @RC00446                                                 00099600
*                                                                       00099700
*                   ELSE                                                00099800
*                     ERRORID=''B;  /* ZERO ERRORID IN RTCT          */ 00099900
@RF00446 L     R12,CVTPTR                                               00100000
         L     R12,CVTRTMCT-CVTMAP(,R12)                                00100100
         XC    ERRORID(12,R12),ERRORID(R12)                             00100200
*                   PRDERRID=ERRORID;/* MOVE ERRORID TO HEADER       */ 00100300
@RC00446 L     R12,SDSMBUFF(,R7)                                        00100400
         L     R8,CVTPTR                                                00100500
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00100600
         MVC   PRDERRID(10,R12),ERRORID(R8)                             00100700
*                 END;              /* END OF NOT A SYSMDUMP REQUEST */ 00100800
*             END;                  /* END OF CURRENT TCB PTR VALID  */ 00100900
*         END;                      /* END CHECK FOR ERRORID         */ 00101000
*       R13=ADDR(SDWRGSV2);         /* GET ADDRESS OF SAVE AREA      */ 00101100
@RC00432 LA    R13,SDWRGSV2(,R7)                                        00101200
*       CALL IEAVTSDP;              /* WRITE OUT HEADER RECORD       */ 00101300
         L     R15,@CV05110                                             00101400
         BALR  R14,R15                                                  00101500
*       ERRBASE1=R9;                /* RESTORE BASE REGISTER FOR        00101600
*                                      RECOVERY                      */ 00101700
         ST    R9,ERRBASE1(,R3)                                         00101800
*       SVCDMPNM=CSECTNAM;          /* RESTORE CSECT NAME FOR           00101900
*                                      RECOVERY                      */ 00102000
         MVC   SVCDMPNM(8,R7),KEAVTSDH                                  00102100
*RTYEND10:                                                              00102200
*       ERRADDR=ADDR(RTYENDMP);     /* GET NEXT RETRY LOCATION ADDR  */ 00102300
RTYEND10 LA    R12,RTYENDMP                                             00102400
         ST    R12,ERRADDR(,R3)                                         00102500
*     END;                          /* END OF BUILD HEADER RECORD    */ 00102600
*   IF ERRENDMP='0'B THEN           /* IF STILL NO ERRORS            */ 00102700
@RF00392 TM    ERRENDMP(R3),B'01000000'                                 00102800
         BNZ   @RF00460                                                 00102900
*     DO;                           /* INITIALIZE ADDRESS RANGE TABLE   00103000
*                                      AND INCLUDE AREAS FOR AMDPRDMP*/ 00103100
*                                                                       00103200
*/* ******************************************************************/ 00103300
*/*   FUNCTION:                                                      */ 00103400
*/*      THIS SEGMENT INITIALIZES THE ADDRESS RANGE TABLE IN THE     */ 00103500
*/*      SDUMP 4K BUFFER OR THE SQA AREA PASSED AS A PARAMETER IF    */ 00103600
*/*      THIS IS A SYSMDUMP REQUEST. THIS SEGMENT ALSO MOVES THE     */ 00103700
*/*      ADDRESSES OF AREAS CRUCIAL TO AMDPRDMP TO THE ADDRESS RANGE */ 00103800
*/*      TABLE: CVT, SCVT, ASVT, ASCBS, UCBS, PCCA, PCCAVT           */ 00103900
*/*                                                                  */ 00104000
*/*   INPUT: SDUMP PARAMETER LIST, CVT, ASCB, ASVT, UCBS             */ 00104100
*/*                                                                  */ 00104200
*/*   OUTPUT: ADDRESS RANGE TABLE, POINTERS IN SDUMP WORK AREA       */ 00104300
*/*                                                                  */ 00104400
*/* ******************************************************************/ 00104500
*                                                                       00104600
*       IF SDUABEND='1'B THEN       /* IF THIS IS A SYSMDUMP REQUEST */ 00104700
         L     R12,SDWSDPL(,R7)                                         00104800
         TM    SDUFLAG1-SDUMP(R12),SDUABEND                             00104900
         BNO   @RF00462                                                 00105000
*         RANGEPTR=SDUSYSMS;        /* RANGE TABLE IS BUILD IN SQA      00105100
*                                      BUFFER PASSED IN PARAMETER       00105200
*                                      LIST BY SYSMDUMP              */ 00105300
         L     R12,SDUSYSMS-SDUMP(,R12)                                 00105400
         ST    R12,RANGEPTR(,R7)                                        00105500
         B     @RC00462                                                 00105600
*                                                                       00105700
*       ELSE                                                            00105800
*         RANGEPTR=CVTSDBF&HIGHBIT0;/* IF NOT A SYSMDUMP REQUEST        00105900
*                                      RANGE TABLE IS BUILT IN THE      00106000
*                                      SDUMP 4K SQA BUFFER           */ 00106100
@RF00462 L     R12,CVTPTR                                               00106200
         L     R12,CVTSDBF-CVTMAP(,R12)                                 00106300
         N     R12,KX7FFFFF                                             00106400
         ST    R12,RANGEPTR(,R7)                                        00106500
*       STARTADR=NULL;              /* FIRST ADDRESS RANGE IS EMPTY  */ 00106600
@RC00462 SLR   R12,R12                                                  00106700
         L     R8,RANGEPTR(,R7)                                         00106800
         ST    R12,STARTADR(,R8)                                        00106900
*       ENDADDR=NULL;               /* START ADDRESS 1ST RANGE IS       00107000
*                                      ZERO                          */ 00107100
         ST    R12,ENDADDR(,R8)                                         00107200
*       LASTRNGE='1'B;              /* END ADDRESS IS NULL FIRST        00107300
*                                      RANGE                         */ 00107400
         OI    LASTRNGE(R8),B'10000000'                                 00107500
*       ENDADRNT=RANGEPTR+FOURK-LEN1ADRN;/* CALCULATE END OF ADDRESS    00107600
*                                      RANGE TABLE                   */ 00107700
         LR    R12,R8                                                   00107800
         SL    R12,KFM4088                                              00107900
         ST    R12,ENDADRNT(,R7)                                        00108000
*       RANGEPTR=RANGEPTR+LEN1ADRN; /* MOVE POINTER TO FIRST ADDRESS    00108100
*                                      RANGE                         */ 00108200
         AL    R8,KF8                                                   00108300
         ST    R8,RANGEPTR(,R7)                                         00108400
*       GLOBADDR=RANGEPTR;          /* INDICATE BEGINNING OF GLOBAL     00108500
*                                      PART OF TABLE                 */ 00108600
         ST    R8,GLOBADDR(,R7)                                         00108700
*       STARTADR=CVTPTR&HIGHBIT0;   /* GET BEGINNING ADDRESS OF CVT  */ 00108800
         L     R12,CVTPTR                                               00108900
         N     R12,KX7FFFFF                                             00109000
         ST    R12,STARTADR(,R8)                                        00109100
*       ENDADDR=STARTADR+FOURK;     /* ENDING ADDRESS OF CVT TO         00109200
*                                      INCLUDE ALL OF THE CVT        */ 00109300
         AL    R12,KF4096                                               00109400
         ST    R12,ENDADDR(,R8)                                         00109500
*       CALL SDADDADR;              /* ADD RANGE TO THE ADDRESS RANGE   00109600
*                                      TABLE                         */ 00109700
         BAL   R14,SDADDADR                                             00109800
*       STARTADR=CVTABEND&HIGHBIT0; /* GET SCVT BEGINNING ADDR       */ 00109900
         L     R12,RANGEPTR(,R7)                                        00110000
         L     R8,CVTPTR                                                00110100
         L     R8,CVTABEND-CVTMAP(,R8)                                  00110200
         N     R8,KX7FFFFF                                              00110300
         ST    R8,STARTADR(,R12)                                        00110400
*       ENDADDR=STARTADR+FOURK;     /* GET SCVT ENDING ADDRESS       */ 00110500
         AL    R8,KF4096                                                00110600
         ST    R8,ENDADDR(,R12)                                         00110700
*       CALL SDADDADR;              /* ADD SCVT TO RANGE TABLE       */ 00110800
         BAL   R14,SDADDADR                                             00110900
*       STARTADR=ADDR(ASVTBEGN)&HIGHBIT0;/* BEGINNING ADDRESS OF ASVT*/ 00111000
         L     R12,RANGEPTR(,R7)                                        00111100
         L     R8,CVTPTR                                                00111200
         L     R8,CVTASVT-CVTMAP(,R8)                                   00111300
         LA    R6,ASVTBEGN(,R8)                                         00111400
         N     R6,KX7FFFFF                                              00111500
         ST    R6,STARTADR(,R12)                                        00111600
*       ENDADDR=STARTADR+(ADDR(ASVTENTY)-ADDR(ASVTBEGN))+(4*ASVTMAXU);  00111700
         AL    R6,KF16                                                  00111800
         L     R8,ASVTMAXU(,R8)                                         00111900
         SLA   R8,2                                                     00112000
         ALR   R6,R8                                                    00112100
         ST    R6,ENDADDR(,R12)                                         00112200
*                                   /* CALCULATE ASVT ENDING ADDRESS */ 00112300
*       CALL SDADDADR;              /* ADD ASVT ADDRESSES TO RANGE      00112400
*                                      TABLE                         */ 00112500
         BAL   R14,SDADDADR                                             00112600
*       IF SDUABEND='0'B THEN       /* IF THIS IS NOT A SYSMDUMP        00112700
*                                      REQUEST                       */ 00112800
         L     R12,SDWSDPL(,R7)                                         00112900
         TM    SDUFLAG1-SDUMP(R12),SDUABEND                             00113000
         BNZ   @RF00481                                                 00113100
*         DO RTCTINDX=ONE TO RTCTSDNA;/* DUMP ASCB FOR ALL REQUESTED    00113200
*                                      ADDRESS SPACES                */ 00113300
         LA    R12,1                                                    00113400
         B     @DE00482                                                 00113500
*                                                                       00113600
*           STARTADR=ASVTENTY(RTCTSDAS(RTCTINDX))&HIGHBIT0;/* GET       00113700
*                                      START ADDRESS OF ASCB THIS       00113800
*                                      ADDRESS SPACE                 */ 00113900
@DL00482 L     R12,RANGEPTR(,R7)                                        00114000
         L     R8,CVTPTR                                                00114100
         L     R6,CVTRTMCT-CVTMAP(,R8)                                  00114200
         SLR   R2,R2                                                    00114300
         IC    R2,RTCTINDX(,R6)                                         00114400
         SLA   R2,2                                                     00114500
         ALR   R6,R2                                                    00114600
         SLR   R2,R2                                                    00114700
         ICM   R2,3,RTCTSDAS-4(R6)                                      00114800
         SLA   R2,2                                                     00114900
         L     R8,CVTASVT-CVTMAP(,R8)                                   00115000
         L     R8,ASVTENTY-4(R2,R8)                                     00115100
         N     R8,KX7FFFFF                                              00115200
         ST    R8,STARTADR(,R12)                                        00115300
*           ENDADDR=STARTADR+LENGTH(ASCB);/* GET ENDING ASCB ADDRESS */ 00115400
         AL    R8,KF208                                                 00115500
         ST    R8,ENDADDR(,R12)                                         00115600
*           CALL SDADDADR;          /* ADD THIS ASCB ADDRESS TO RANGE   00115700
*                                      TABLE                         */ 00115800
         BAL   R14,SDADDADR                                             00115900
*         END;                      /* END DUMP ALL REQUESTED ASCB'S */ 00116000
         L     R8,CVTPTR                                                00116100
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00116200
         LA    R12,1                                                    00116300
         SLR   R6,R6                                                    00116400
         IC    R6,RTCTINDX(,R8)                                         00116500
         ALR   R12,R6                                                   00116600
@DE00482 L     R8,CVTPTR                                                00116700
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00116800
         STC   R12,RTCTINDX(,R8)                                        00116900
         L     R8,CVTPTR                                                00117000
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00117100
         SLR   R6,R6                                                    00117200
         IC    R6,RTCTSDNA(,R8)                                         00117300
         CR    R12,R6                                                   00117400
         BNH   @DL00482                                                 00117500
         B     @RC00481                                                 00117600
*                                                                       00117700
*       ELSE                                                            00117800
*         DO;                       /* THIS IS A SYSMDUMP REQUEST    */ 00117900
*           STARTADR=PSAAOLD&HIGHBIT0;/* ASCB ADDRESS IS IN PSA      */ 00118000
@RF00481 L     R12,RANGEPTR(,R7)                                        00118100
         L     R8,PSAAOLD                                               00118200
         N     R8,KX7FFFFF                                              00118300
         ST    R8,STARTADR(,R12)                                        00118400
*           ENDADDR=STARTADR+LENGTH(ASCB);/* GET ENDING ASCB ADDRESS */ 00118500
         AL    R8,KF208                                                 00118600
         ST    R8,ENDADDR(,R12)                                         00118700
*           CALL SDADDADR;          /* ADD ASCB RANGE TO RANGE TABLE */ 00118800
         BAL   R14,SDADDADR                                             00118900
*         END;                      /* END OF THIS IS A SYSMDUMP        00119000
*                                      REQUEST                       */ 00119100
*                                                                       00119200
*       /*************************************************************/ 00119300
*       /*                                                           */ 00119400
*       /* MAKE SURE THE PCCAVT AND ALL PCCAS ARE IN THE DUMP        */ 00119500
*       /*                                                           */ 00119600
*       /* SO PRDMP CAN ALWAYS FIND THE CPUID                        */ 00119700
*       /*                                                           */ 00119800
*       /*************************************************************/ 00119900
*                                                                       00120000
*       STARTADR=CVTPCCAT;          /* GET ADDRESS OF PCCAVT         */ 00120100
@RC00481 L     R12,RANGEPTR(,R7)                                        00120200
         L     R8,CVTPTR                                                00120300
         L     R8,CVTPCCAT-CVTMAP(,R8)                                  00120400
         ST    R8,STARTADR(,R12)                                        00120500
*       ENDADDR=STARTADR+LENGTH(PCCAVT);/* GET END ADDR PCCAVT       */ 00120600
         AL    R8,KF64                                                  00120700
         ST    R8,ENDADDR(,R12)                                         00120800
*       CALL SDADDADR;              /* ADD PCCAVT TO RANGE TABLE     */ 00120900
         BAL   R14,SDADDADR                                             00121000
*       DO RTCTINDX=1 TO CPUMAX;    /* LOOP MOVING ALL PCCAS @G64GPHD*/ 00121100
         LA    R12,1                                                    00121200
         L     R8,CVTPTR                                                00121300
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00121400
         STC   R12,RTCTINDX(,R8)                                        00121500
*         IF PCCAT00P(RTCTINDX)^=NULL THEN/* IF THIS PCCA IS DEFINED */ 00121600
@DL00495 L     R12,CVTPTR                                               00121700
         L     R8,CVTRTMCT-CVTMAP(,R12)                                 00121800
         SLR   R6,R6                                                    00121900
         IC    R6,RTCTINDX(,R8)                                         00122000
         SLA   R6,2                                                     00122100
         L     R12,CVTPCCAT-CVTMAP(,R12)                                00122200
         AL    R12,KFM4                                                 00122300
         L     R12,PCCAT00P(R6,R12)                                     00122400
         LTR   R12,R12                                                  00122500
         BZ    @RF00496                                                 00122600
*           DO;                     /* ADD THIS PCCA RANGE TO TABLE  */ 00122700
*             STARTADR=PCCAT00P(RTCTINDX);/* GET STARTING ADDR       */ 00122800
         L     R8,RANGEPTR(,R7)                                         00122900
         ST    R12,STARTADR(,R8)                                        00123000
*             ENDADDR=STARTADR+LENGTH(PCCA);/* GET END PCCA ADDR     */ 00123100
         AL    R12,KF584                                                00123200
         ST    R12,ENDADDR(,R8)                                         00123300
*             CALL SDADDADR;        /* ADD THIS PCCA TO TABLE        */ 00123400
         BAL   R14,SDADDADR                                             00123500
*           END;                    /* END OF ADD THIS PCCA RANGE TO    00123600
*                                      TABLE                         */ 00123700
*       END;                        /* END OF MOVING ALL PCCA RANGES */ 00123800
@RF00496 L     R8,CVTPTR                                                00123900
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00124000
         LA    R12,1                                                    00124100
         SLR   R6,R6                                                    00124200
         IC    R6,RTCTINDX(,R8)                                         00124300
         ALR   R12,R6                                                   00124400
         L     R8,CVTPTR                                                00124500
         L     R8,CVTRTMCT-CVTMAP(,R8)                                  00124600
         STC   R12,RTCTINDX(,R8)                                        00124700
         C     R12,KF15                                                 00124800
         BNH   @DL00495                                                 00124900
*       STARTADR=CVTSDBF&HIGHBIT0;  /* GET BEGINNING ADDR RANGE TABLE*/ 00125000
         L     R12,RANGEPTR(,R7)                                        00125100
         L     R8,CVTPTR                                                00125200
         L     R8,CVTSDBF-CVTMAP(,R8)                                   00125300
         N     R8,KX7FFFFF                                              00125400
         ST    R8,STARTADR(,R12)                                        00125500
*       ENDADDR=STARTADR+FOURK;     /* GET ENDING ADDR RANGE TABLE   */ 00125600
         AL    R8,KF4096                                                00125700
         ST    R8,ENDADDR(,R12)                                         00125800
*       CALL SDADDADR;              /* ADD RANGE TABLE ADDRESS TO       00125900
*                                      RANGE TABLE                   */ 00126000
         BAL   R14,SDADDADR                                             00126100
*       STARTADR=CVTGDA&HIGHBIT0;   /* GET BEGINNING GDA ADDRESS     */ 00126200
         L     R12,RANGEPTR(,R7)                                        00126300
         L     R8,CVTPTR                                                00126400
         L     R8,CVTGDA-CVTMAP(,R8)                                    00126500
         N     R8,KX7FFFFF                                              00126600
         ST    R8,STARTADR(,R12)                                        00126700
*       ENDADDR=STARTADR+LENGTH(GDA);/* GET GDA END ADDRESS          */ 00126800
         AL    R8,KF64                                                  00126900
         ST    R8,ENDADDR(,R12)                                         00127000
*       CALL SDADDADR;              /* ADDR GDA AREA TO RANGE TABLE  */ 00127100
         BAL   R14,SDADDADR                                             00127200
*       STARTADR=FOURK;             /* GET BEGINNING ADDRESS UCB        00127300
*                                      DEFAULT AREA                  */ 00127400
         L     R12,RANGEPTR(,R7)                                        00127500
         MVC   STARTADR(4,R12),KF4096                                   00127600
*       ENDADDR=CVTIXAVL->IOCATTBL; /* END ADDRESS UCB DEFAULT AREA  */ 00127700
         L     R8,CVTPTR                                                00127800
         L     R8,CVTIXAVL-CVTMAP(,R8)                                  00127900
         L     R8,IOCATTBL(,R8)                                         00128000
         ST    R8,ENDADDR(,R12)                                         00128100
*       CALL SDADDADR;              /* ADD UCB DEFAULT AREA TO RANGE    00128200
*                                      TABLE                         */ 00128300
         BAL   R14,SDADDADR                                             00128400
*RTYENDMP:                                                              00128500
*       IF ERRENDMP='1'B THEN       /* IF ERROR WRITING HDR RCD      */ 00128600
RTYENDMP TM    ERRENDMP(R3),B'01000000'                                 00128700
         BNO   @RF00460                                                 00128800
*         SDRETCOD=NODUMP;          /* INDICATE NO DUMP TAKEN        */ 00128900
         MVC   SDRETCOD(4,R7),KF8                                       00129000
*       ELSE                                                            00129100
*         ;                                                             00129200
*     END;                                                              00129300
*   R13=ADDR(SDWRGSV1);             /* RESTORE SAVE AREA ADDRESS     */ 00129400
@RF00460 LA    R13,SDWRGSV1(,R7)                                        00129500
*   RETURN;                                                             00129600
@ER00001 LM    R14,R12,12(R13)                                          00129700
         BR    R14                                                      00129800
*                                                                       00129900
*/********************************************************************/ 00130000
*/*   THIS SEGMENT WILL ADD AN ADDRESS RANGE TO THE ADDRESS RANGE    */ 00130100
*/*   TABLE.  THE ADDRESSES TO BE ADDED ARE IN STARTADR AND          */ 00130200
*/*   ENDADDR (IN THE ADDRESS RANGE TABLE).  IF THEIR IS ROOM IN     */ 00130300
*/*   THE ADDRESS RANGE TABLE, THE START ADDRESS WILL BE ROUNDED     */ 00130400
*/*   DOWN TO A 4K BOUNDARY AND THE END ADDRESS WILL BE ROUNDED UP   */ 00130500
*/*   TO A 4K BOUNDARY.  IF THEIR IS NO MORE ROOM IN THE ADDRESS     */ 00130600
*/*   RANGE TABLE, A RETURN CODE OF PARTDUMP WILL BE STORED IN       */ 00130700
*/*   SDRETCOD.  IF THE START ADDRESS IS GREATER THAN THE END        */ 00130800
*/*   ADDRESS A C0D ABEND WILL BE ISSUED.                            */ 00130900
*/*                                                                  */ 00131000
*/*   INPUT: RANGEPTR CONTAINS ADDRESS OF NEXT AVAILABLE SLOT IN     */ 00131100
*/*      ADDRESS RANGE TABLE, ENDADRNT CONTAINS ADDRESS OF LAST SLOT */ 00131200
*/*       IN ADDRESS RANGE TABLE, STARTADR CONTAINS BEG ADDRESS OF   */ 00131300
*/*      RANGE, ENDADDR CONTAINS END ADDRESS OF RANGE                */ 00131400
*/*                                                                  */ 00131500
*/*   OUTPUT: RANGE ADDED TO ADDRESS RANGE TABLE, RANGEPTR           */ 00131600
*/*      CONTAINS ADDRESS OF NEXT AVAILABLE SLOT OR RETURN CODE OF   */ 00131700
*/*       PARTDUMP OR C0D ABEND                                      */ 00131800
*/*                                                                  */ 00131900
*/*   CALLED BY: SDALLPSA,USERNGES,SDSPQE,SDGLOBAL,LPACDE,SDNUC,     */ 00132000
*/*   LPALPDE,SDLOCAL                                                */ 00132100
*/********************************************************************/ 00132200
*                                                                       00132300
*SDADDADR:                                                              00132400
*   PROC OPTIONS(NOSAVE,NOSAVEAREA);                                    00132500
*   ADDADRWA=R10;                   /* SAVE WORK REGISTER            */ 00132600
SDADDADR ST    R10,ADDADRWA(,R7)                                        00132700
*   ADDADRWB=R11;                   /* SAVE WORK REGISTER            */ 00132800
         ST    R11,ADDADRWB(,R7)                                        00132900
*   ADDADRWC=R4;                    /* SAVE WORK REGISTER            */ 00133000
         ST    R4,ADDADRWC(,R7)                                         00133100
*   ADDADRWD=R5;                    /* SAVE WORK REGISTER            */ 00133200
         ST    R5,ADDADRWD(,R7)                                         00133300
*   IF RANGEPTR<(ENDADRNT-LEN1ADRN) THEN/* IF RNGE TABLE NOT FULL    */ 00133400
         L     R12,RANGEPTR(,R7)                                        00133500
         L     R8,ENDADRNT(,R7)                                         00133600
         SL    R8,KF8                                                   00133700
         CR    R12,R8                                                   00133800
         BNL   @RF00525                                                 00133900
*     DO;                                                               00134000
*       STARTADR=STARTADR&FOURKBDY; /* ROUND START ADDR DOWN-4K BDY  */ 00134100
         L     R8,@CF05056                                              00134200
         L     R6,STARTADR(,R12)                                        00134300
         NR    R6,R8                                                    00134400
         ST    R6,STARTADR(,R12)                                        00134500
*       ENDADDR=ENDADDR&HIGHBIT0;   /* END ADDRESS IS AN ADDRESS     */ 00134600
         L     R6,ENDADDR(,R12)                                         00134700
         N     R6,KX7FFFFF                                              00134800
         ST    R6,ENDADDR(,R12)                                         00134900
*       IF(ENDADDR//FOURK)^=NULL THEN/* IF END ADDR NOT ON 4K BDY,   */ 00135000
         LR    R4,R6                                                    00135100
         SRDA  R4,32                                                    00135200
         D     R4,KF4096                                                00135300
         LTR   R4,R4                                                    00135400
         BZ    @RF00529                                                 00135500
*         ENDADDR=(ENDADDR+FOURK)&FOURKBDY;/* ROUND UP TO 4K BDY     */ 00135600
         AL    R6,KF4096                                                00135700
         NR    R6,R8                                                    00135800
         ST    R6,ENDADDR(,R12)                                         00135900
*       ELSE                                                            00136000
*         ;                                                             00136100
*       IF STARTADR>ENDADDR THEN    /* ERROR IF START > END          */ 00136200
@RF00529 L     R12,RANGEPTR(,R7)                                        00136300
         CLC   STARTADR(4,R12),ENDADDR(R12)                             00136400
         BNH   @RF00532                                                 00136500
*                                                                       00136600
         ABEND X'C0D',DUMP,,SYSTEM                                      00136700
*                                                                       00136800
         B     @RC00525                                                 00136900
*                                                                       00137000
*       ELSE                                                            00137100
*         RANGEPTR=RANGEPTR+LEN1ADRN;/* ADDR OF NEXT SLOT IN TAB     */ 00137200
@RF00532 LA    R12,8                                                    00137300
         AL    R12,RANGEPTR(,R7)                                        00137400
         ST    R12,RANGEPTR(,R7)                                        00137500
         B     @RC00525                                                 00137600
*                                                                       00137700
*     END;                                                              00137800
*   ELSE                            /* IF ADDRESS RANGE TABLE FULL,  */ 00137900
*     DO;                                                               00138000
*       FLGRTFUL='1'B;              /* IND ADDRESS RANGE TABLE FULL  */ 00138100
@RF00525 OI    FLGRTFUL(R7),B'10000000'                                 00138200
*       RTCTSDPR=PARTDUMP;          /* GIVE PERMANENT PARTIAL DUMP   */ 00138300
         L     R12,CVTPTR                                               00138400
         L     R12,CVTRTMCT-CVTMAP(,R12)                                00138500
         MVI   RTCTSDPR(R12),X'04'                                      00138600
*     END;                                                              00138700
*   R10=ADDADRWA;                   /* RESTORE WORK REGISTER         */ 00138800
@RC00525 L     R10,ADDADRWA(,R7)                                        00138900
*   R11=ADDADRWB;                   /* RESTORE WORK REGISTER         */ 00139000
         L     R11,ADDADRWB(,R7)                                        00139100
*   R4=ADDADRWC;                    /* RESTORE WORK REGISTER         */ 00139200
         L     R4,ADDADRWC(,R7)                                         00139300
*   R5=ADDADRWD;                    /* RESTORE WORK REGISTER         */ 00139400
         L     R5,ADDADRWD(,R7)                                         00139500
*   END SDADDADR;                                                       00139600
@ER00002 BR    R14                                                      00139700
*   END IEAVTSDH                                                        00139800
*                                                                       00139900
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */ 00140000
*/*%INCLUDE SYSLIB  (SDHSPEC )                                       */ 00140100
*/*%INCLUDE SYSLIB  (SDHDCL  )                                       */ 00140200
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 00140300
*/*%INCLUDE SYSLIB  (IEZIOB  )                                       */ 00140400
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */ 00140500
*/*%INCLUDE SYSLIB  (IHARB   )                                       */ 00140600
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */ 00140700
*/*%INCLUDE SYSLIB  (IEZDEB  )                                       */ 00140800
*/*%INCLUDE SYSLIB  (IHAECB  )                                       */ 00140900
*/*%INCLUDE SYSLIB  (IHASCVT )                                       */ 00141000
*/*%INCLUDE SYSLIB  (IHARTCT )                                       */ 00141100
*/*%INCLUDE SYSLIB  (IEFUCBOB)                                       */ 00141200
*/*%INCLUDE SYSLIB  (IHADCBDF)                                       */ 00141300
*/*%INCLUDE SYSLIB  (IHADCB  )                                       */ 00141400
*/*%INCLUDE SYSLIB  (IHAWSAVT)                                       */ 00141500
*/*%INCLUDE SYSLIB  (IHASDUMP)                                       */ 00141600
*/*%INCLUDE SYSLIB  (IHARTM2A)                                       */ 00141700
*/*%INCLUDE SYSLIB  (IHASNP  )                                       */ 00141800
*/*%INCLUDE SYSLIB  (IHARMPL )                                       */ 00141900
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 00142000
*/*%INCLUDE SYSLIB  (IHALPDE )                                       */ 00142100
*/*%INCLUDE SYSLIB  (IHACDE  )                                       */ 00142200
*/*%INCLUDE SYSLIB  (IHALCCA )                                       */ 00142300
*/*%INCLUDE SYSLIB  (IHALCCAT)                                       */ 00142400
*/*%INCLUDE SYSLIB  (IHAPCCA )                                       */ 00142500
*/*%INCLUDE SYSLIB  (IHAPCCAT)                                       */ 00142600
*/*%INCLUDE SYSLIB  (IHAASVT )                                       */ 00142700
*/*%INCLUDE SYSLIB  (IHAGDA  )                                       */ 00142800
*/*%INCLUDE SYSLIB  (IECDIOCM)                                       */ 00142900
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 00143000
*/*%INCLUDE SYSLIB  (IHAASXB )                                       */ 00143100
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 00143200
*/*%INCLUDE SYSLIB  (IHASDWRK)                                       */ 00143300
*/*%INCLUDE SYSLIB  (IHADVCT )                                       */ 00143400
*/*%INCLUDE SYSLIB  (AMDDATA )                                       */ 00143500
*/*%INCLUDE SYSLIB  (SDFILE  )                                       */ 00143600
*/*%INCLUDE SYSLIB  (SDFULL  )                                       */ 00143700
*/*%INCLUDE SYSLIB  (SDHDR   )                                       */ 00143800
*/*%INCLUDE SYSLIB  (ERRORID )                                       */ 00143900
*/*%INCLUDE SYSLIB  (SDHGLOBL)                                       */ 00144000
*/*%INCLUDE SYSLIB  (SDADDADR)                                       */ 00144100
*                                                                       00144200
*/********************************************************************/ 00144300
*/*                                                                  */ 00144400
*/* MACRO NAME = SDWORK                                              */ 00144500
*/*                                                                  */ 00144600
*/* MICROFICHE NAME = IHASDWRK                                       */ 00144700
*/*                                                                  */ 00144800
*/* DESCRIPTIVE NAME = SVC DUMP WORK AREA                            */ 00144900
*/*                                                                  */ 00145000
*/* STATUS = OS/VS2 RELEASE 3.7 PLUS SU 64        11/02/78   @ZM49564*/ 00145100
*/*                                                          @G64MPHD*/ 00145200
*/*                                                                  */ 00145300
*/* FUNCTION = THE SVC DUMP WORK AREA IS USED BY SVC DUMP DURING     */ 00145400
*/*    ITS PROCESSING TO CONTAIN POINTERS, COUNTERS AND AN OUTPUT    */ 00145500
*/*    BUFFER.  IT CONTAINS THE FRR/ESTAE WORK AREA USED BY SVC      */ 00145600
*/*    DUMP.  IT IS OBTAINED BY MODULE IEAVTSDI (SVC DUMP NIP RIM)   */ 00145700
*/*    FROM CSA SUBPOOL 231. THIS MAPPING IS ALSO USED BY    @G64LPHD*/ 00145800
*/*    IEAVTABD, TO OBTAIN THE LENGTH OF THE INTERFACE WORK  @G64LPHD*/ 00145900
*/*    AREAS FOR SYSMDUMP REQUESTS.                          @G64LPHD*/ 00146000
*/*                                                                  */ 00146100
*/* SERIALIZATION - RTCTSDPL FIELD (ONE ACCESS AT A TIME)            */ 00146200
*/*                                                                  */ 00146300
*/* METHOD OF ACCESS = RTCTSDWK POINTS TO THIS AREA AFTER IPL.       */ 00146400
*/*        THE CVT POINTS TO THE RTCT (CVTRTMCT).            @G64MPHD*/ 00146500
*/*                                                                  */ 00146600
*/* TOTAL SIZE = DECIMAL 4656, X'1230'                       @G64MPHD*/ 00146700
*/*                                                                  */ 00146800
*/* CHANGE ACTIVITY =                                        @G64LPHD*/ 00146900
*/*                   - SU64 DEVICE INDEPENDENCE RESTRUCTURE @G64LPHD*/ 00147000
*/*                   - SU64 MASTER TRACE DUMP EXIT SUPPORT  @G64MPHD*/ 00147100
*/*                   - OZ43713                              @ZA43713*/ 00147200
*/*      OZ43713 CHANGED A RESERVED FIELD FIXED(31)          @ZA43713*/ 00147300
*/*              TO M994REQ CHAR(1),RESERVED CHAR(3)         @ZA43713*/ 00147400
*/*      OZ53292 - USE RESERVED FIELD TO PROVIDE A BIT       @ZA53292*/ 00147500
*/*              TO INDICATE THAT DOM IS REQUIRED.           @ZA53292*/ 00147600
*/*              USED BY IEAVTSDC AND IEAVTSDH.              @ZA53292*/ 00147700
*/********************************************************************/ 00147800
*  DCL 1 SDWORK BASED BDY(DWORD),      /*SVC DUMP WORK AREA          */ 00147900
*        2 SDCNTRL,                    /*SVC DUMP CONTROL SECTION OF    00148000
*                                              THE WORK AREA @G33VPHD*/ 00148100
*          3 SDECB    FIXED(32),       /*ECB WAITED ON - EXCP, GTF      00148200
*                                      READ, PAGE FIX/FREE         */   00148300
*          3 SDIOB    CHAR(40),        /*IOB, I/O BY EXCP            */ 00148400
*          3 LSTESTAE CHAR(16),        /*LIST FORM OF ESTAE MACRO    */ 00148500
*          3 SDDSCHAR CHAR(4) BDY(WORD), /*DUMP DATASET CHARACTERISTICS 00148600
*                                                            @G64LPHD*/ 00148700
*            4 SDRCTMAX FIXED(15) UNSIGNED,  /*MAX RECORDS PER TRACK    00148800
*                                                            @G64LPHD*/ 00148900
*            4 SDTRCMAX FIXED(15) UNSIGNED,  /*MAX TRACKS PER CYLINDER  00149000
*                                                            @G64LPHD*/ 00149100
*          3 NUCADDR  PTR(31),         /*ADDR OF NUCLEUS SEC OF TABLE*/ 00149200
*          3 LOCALADR PTR(31),         /*ADDR OF LOCAL SEC OF TABLE  */ 00149300
*          3 GLOBADDR PTR(31),         /*ADDR OF GLOBAL SEC OF TABLE */ 00149400
*          3 RANGEPTR PTR(31),         /*ADDR OF CURRENT ADDR RANGE  */ 00149500
*          3 ENDADRNT PTR(31),         /*END OF ADDRESS RANGE TABLE  */ 00149600
*          3 SVCDSRBA PTR(31),       /*SRB BLOCK ADDR IN SDX @G33VPHD*/ 00149700
*          3 SDSRBADR PTR(31),   /*SRB ROUTINE ADDRESS FROM MODULE      00149800
*                                                   IEAVTSDX @G33VPHD*/ 00149900
*          3 SDSRBBAS PTR(31),  /*SRB ROUTINE BASE REGISTER FROM        00150000
*                                            MODULE IEAVTSDX @G33VPHD*/ 00150100
*          3 SDWSDPL PTR(31),         /*ADDR SDUMP PARM LIST @G33SPHD*/ 00150200
*          3 SVCDOPTS BIT(16),         /*DUMP OPTIONS FOR THIS DUMP  */ 00150300
*            4 SVCDOPT1 CHAR(1),       /*FIRST BYTE OF OPTIONS       */ 00150400
*              5 SVCDAPSA BIT(1),      /*DUMP ALL PSA IN SYSTEM      */ 00150500
*              5 SVCDPSA  BIT(1),      /*DUMP PSA FOR CURRENT CPU    */ 00150600
*              5 SVCDNUC  BIT(1),      /*DUMP THE NUCLEUS            */ 00150700
*              5 SVCDSQA  BIT(1),      /*DUMP SQA                    */ 00150800
*              5 SVCDLSQA BIT(1),      /*DUMP LSQA                   */ 00150900
*              5 SVCDRGN  BIT(1),      /*DUMP RGN(PRIVATE AREA)      */ 00151000
*              5 SVCDLPA  BIT(1),      /*DUMP MODS, SVCS IN LPA      */ 00151100
*              5 SVCDTRT  BIT(1),      /*DUMP SUP TRT OR GTF TRT     */ 00151200
*            4 SVCDOPT2 CHAR(1),       /*SECOND BYTE OF OPTIONS      */ 00151300
*              5 SVCDCSA  BIT(1),      /*DUMP CSA                    */ 00151400
*              5 SVCDSWA  BIT(1),      /*DUMP SWA                    */ 00151500
*              5 SVCDSMDP BIT(1),      /*DUMP SUMMARY DUMP   @G33VPHD*/ 00151600
*              5 SVCDNSMD BIT(1),      /*NO SUMMARY DUMP     @G33VPHD*/ 00151700
*              5 SVCDNAPA BIT(1),      /*NO ALL PSA DUMP     @G33VPHD*/ 00151800
*              5 SVCDNSQA BIT(1),      /*NO SQA DUMP         @G33VPHD*/ 00151900
*          3 FLAGSONE BIT(8),          /*FLAG BYTE                   */ 00152000
*            4 FLGRTFUL BIT(1),        /*IND ADDR RANGE TABLE FULL   */ 00152100
*            4 FLGTAPEE BIT(1),        /*IND END OF VOL ON TAPE      */ 00152200
*            4 FLGFLAST BIT(1),        /*IND LAST FREE DUMP DS FOUND    00152300
*                                                            @G33VPHD*/ 00152400
*            4 SDWPAGOT BIT(1),    /*INDICATE PAGE FAULT POSSIBLE       00152500
*                                                            @ZM45207*/ 00152600
*            4 SVCDMTRC BIT(1),    /*INDICATE MASTER TRACE TO BE DUMPED 00152700
*                                                            @ZM49564*/ 00152800
*          3 FLAGSTWO BIT(8),          /*FLAG BYTE                   */ 00152900
*          3 ERRORWK  CHAR(24),        /*ERR WKAREA WHEN ESTAE ACTIVE*/ 00153000
*          3 SVCDMPNM CHAR(8),         /*NAME SVC DUMP CSECT IN CONTROL 00153100
*                                                            @G33VPHD*/ 00153200
*          3 SVCDLPST CHAR(12),        /*LIST FORM OF POST   @G33VPHD*/ 00153300
*          3 SDIORETA PTR(31),         /*RET ADDR FROM SDIO          */ 00153400
*          3 DISPRETA PTR(31),         /*RET ADDR FROM SETSDISP      */ 00153500
*          3 CDERETA  PTR(31),         /*RET ADDR FROM LPACDE        */ 00153600
*          3 LPDERETA PTR(31),         /*RET ADDR FROM LPALPDE       */ 00153700
*          3 FREERETA PTR(31),         /*RETURN ADDRESS FROM SDFREE  */ 00153800
*          3 AFFNSAV  BIT(16),    /*SAVE CPU AFFINITY VALUE  @G33VPHD*/ 00153900
*          3 SDWKEY   BIT(8),      /*SAVE PROTECT KEY FOR SYSMDUMP      00154000
*                                                            @G33VPHD*/ 00154100
*          3 SDFLAGS  BIT(8),          /*USED TO DOM MSG     @ZA53292*/ 00154200
*            4 *      BIT(3),          /*RESERVED            @ZA53292*/ 00154300
*            4 SDDOMMSG BIT(1),        /*IF ONE THEN DOM MSG @ZA53292*/ 00154400
*            4 *      BIT(4),          /*RESERVED            @ZA53292*/ 00154500
*          3 SPQERETA PTR(31),         /*RETURN ADDRESS FROM SDSPQE  */ 00154600
*          3 PGFRRETA PTR(31),         /*RETURN ADDR FROM SDPGFREE   */ 00154700
*          3 USERRETA PTR(31),         /*RETURN ADDR FROM USERNGES   */ 00154800
*          3 SPQEADDR PTR(31),         /*ADDRESS OF SPQE FOR SDSPQE  */ 00154900
*          3 USESTART PTR(31),         /*START ADDRESS FOR INCLUDING    00155000
*                                        USER STORAGE ADDRESSES IN A    00155100
*                                        PART OF THE ADDR RANGE TABLE*/ 00155200
*          3 USEEND PTR(31),           /*END ADDR FOR USER STOR ADDR */ 00155300
*          3 ADDADRWA PTR(31),         /*SAVE WORKA IN SDADDADR RTN  */ 00155400
*          3 ADDADRWB PTR(31),         /*SAVE WORKB IN SDADDADR RTN  */ 00155500
*          3 ADDADRWC PTR(31),         /*SAVE WORKC IN SDADDADR RTN  */ 00155600
*          3 ADDADRWD PTR(31),         /*SAVE WORKD IN SDADDADR RTN  */ 00155700
*          3 SDRETCOD PTR(31),         /*RETURN CODE FROM DUMP RTN   */ 00155800
*          3 SDWRGSV1 CHAR(72),        /*REGISTER SAVE AREA  @G33VPHD*/ 00155900
*          3 SDWRGSV2 CHAR(72),        /*REGISTER SAVE AREA  @G33VPHD*/ 00156000
*          3 DSTINDEX FIXED(31),       /*INDEX TO CURR DS IN DUMP DS    00156100
*                                       TABLE(RTMCT), FOR MSG DS NAM*/  00156200
*          3 TIMCNT FIXED(15),         /*LOOP COUNTER        @G17EP5W*/ 00156300
*          3 CHCNT FIXED(15),          /*LOOP COUNTER        @G17EP5W*/ 00156400
*          3 CVRT11 CHAR(8) BDY(DWORD), /*AREA FOR CVD       @G17EP5W*/ 00156500
*            4 * CHAR(4),              /*NOT USED FOR UNPK   @G17EP5W*/ 00156600
*            4 CVRT1 CHAR(4),          /*USED FOR UNPACK     @G17EP5W*/ 00156700
*              5 *  CHAR(2),           /*UNUSED FOR UNPACK   @G33VPHD*/ 00156800
*              5 SDCVRT1 CHAR(2),    /*USED FOR ASID UNPACK  @G33VPHD*/ 00156900
*          3 CVRT2 CHAR(8) BDY(DWORD), /*AREA FOR UNPACK     @G17EP5W*/ 00157000
*            4 * CHAR(2),              /*UNPACK CHARACTERS   @G33VPHD*/ 00157100
*            4 SDCVRT4 CHAR(6),     /*UNPACK CHARACTERS ASID @G33VPHD*/ 00157200
*              5 * CHAR(5),            /*UNUSED FOR UNPACK   @G33VPHD*/ 00157300
*              5 CVRT2SGN CHAR(1),   /*SIGN POSITION         @G33VPHD*/ 00157400
*          3 SVC994ID FIXED(31),     /*MESG ID OF OUTSTANDING IEA994A   00157500
*                                     MESSAGE OR ZERO IF NO MESSAGE     00157600
*                                          IS OUTSTANDING    @G33VPHD*/ 00157700
*          3 M994REQ CHAR(1),          /*'00'X PRIOR TO      @ZA43713*/ 00157800
*                                      /*      IEAVTSDH OR   @ZA43713*/ 00157900
*                                      /*     AFTER IEAVTSDC @ZA43713*/ 00158000
*                                      /* 'Y'C IF IEAVTSDH   @ZA43713*/ 00158100
*                                      /*      REQUESTS      @ZA43713*/ 00158200
*                                      /*      IEAVTSDC TO   @ZA43713*/ 00158300
*                                      /*      PUT OUT MSG.  @ZA43713*/ 00158400
*                                      /* 'N'C IF IEAVTSDH   @ZA43713*/ 00158500
*                                      /*      INDICATES     @ZA43713*/ 00158600
*                                      /*      IEAVTSDC NOT  @ZA43713*/ 00158700
*                                      /*      TO ISSUE MSG. @ZA43713*/ 00158800
*          3 *    CHAR(3),             /*RESERVED            @ZA43713*/ 00158900
*          3 SVCNAME CHAR(8) BDY(DWORD),    /*NAME FOR PROGRAM MANAGER  00159000
*                                                 SUBROUTINE @G33VPHD*/ 00159100
*            4 SVCNM1T5    CHAR(5),    /*FIRST 5 CHAR NAME(STD=IGC00)*/ 00159200
*              5 SVCNM1T4  CHAR(4),    /*FIRST 4 CHAR NAME(STD=IGC0) */ 00159300
*            4 SVCNM6T8    CHAR(3),    /*LAST 3 CHAR NAME            */ 00159400
*              5 SVCNM6    CHAR(1),    /*6TH CHAR IN NAME(0)         */ 00159500
*              5 SVCNM7T8  CHAR(2),    /*LAST 3 CHAR NAME(-RBINTCOD) */ 00159600
*                6 SVCNM7  CHAR(1),    /*7TH CHAR IN NAME            */ 00159700
*                6 SVCNM8  CHAR(1),    /*8TH(LAST) CHAR IN NAME      */ 00159800
*          3 CCWCHAIN BDY(DWORD),      /*START OF CCW CHAIN FOR EXCP */ 00159900
*            4 CCW1,                   /*FIRST CCW                   */ 00160000
*              5 CMDCODE1 CHAR(1),     /*COMMAND CODE                */ 00160100
*              5 DATAADR1 PTR(24),     /*DATA ADDRESS                */ 00160200
*              5 FLAGS1 CHAR(1),       /*FLAGS                       */ 00160300
*                6 CD1  BIT(1),        /*DATA CHAINNING              */ 00160400
*                6 CMDC1 BIT(1),       /*COMMAND CHAINNING           */ 00160500
*                6 SLI1 BIT(1),        /*SUPPRESS INCORRECT LEN IND  */ 00160600
*                6 SKIP1 BIT(1),       /*SUPPRESS DATA TRANSFER      */ 00160700
*                6 PCI1 BIT(1),        /*PROGRAM CONTROLLED INTERRUPT*/ 00160800
*              5 *      CHAR(1),       /*NOT USED                    */ 00160900
*              5 BYTECNT1 FIXED(16),   /*COUNT OF BYTES TO TRANSFER  */ 00161000
*            4 CCW2,                   /*SECOND CCW                  */ 00161100
*              5 CMDCODE2 CHAR(1),     /*COMMAND CODE                */ 00161200
*              5 DATAADR2 PTR(24),     /*DATA ADDRESS                */ 00161300
*              5 FLAGS2 CHAR(1),       /*FLAGS                       */ 00161400
*                6 CD2  BIT(1),        /*DATA CHAINNING              */ 00161500
*                6 CC2  BIT(1),        /*COMMAND CHAINNING           */ 00161600
*                6 SLI2 BIT(1),        /*SUPPRESS INCORRECT LEN IND  */ 00161700
*                6 SKIP2 BIT(1),       /*SUPPRESS DATA TRANSFER      */ 00161800
*                6 PCI2 BIT(1),        /*PROGRAM CONTROLLED INTERRUPT*/ 00161900
*              5 *      CHAR(1),       /*NOT USED                    */ 00162000
*              5 BYTECNT2 FIXED(16),   /*COUNT OF BYTES TO TRANSFER  */ 00162100
*            4 CCW3,                   /*THIRD CCW                   */ 00162200
*              5 CMDCODE3 CHAR(1),     /*COMMAND CODE                */ 00162300
*              5 DATAADR3 PTR(24),     /*DATA ADDRESS                */ 00162400
*              5 FLAGS3 CHAR(1),       /*FLAGS                       */ 00162500
*                6 CD3  BIT(1),        /*DATA CHAINNING              */ 00162600
*                6 CC3  BIT(1),        /*COMMAND CHAINNING           */ 00162700
*                6 SLI3 BIT(1),        /*SUPPRESS INCORRECT LEN IND  */ 00162800
*                6 SKIP3 BIT(1),       /*SUPPRESS DATA TRANSFER      */ 00162900
*                6 PCI3 BIT(1),        /*PROGRAM CONTROLLED INTERRUPT*/ 00163000
*              5 *      CHAR(1),       /*NOT USED                    */ 00163100
*              5 BYTECNT3 FIXED(16),   /*COUNT OF BYTES TO TRANSFER  */ 00163200
*            3 SDSMBUFF PTR(31),   /*ADDRESS OF THE 4104 BYTE OUTPUT    00163300
*                                                     BUFFER @G33VPHD*/ 00163400
*            3 SDSMDECB PTR(31),   /*ADDRESS OF DECB FOR SYSMDUMP       00163500
*                                                            @G33VPHD*/ 00163600
*          2 SDOUTBUF BDY(DWORD),      /*SDUMP OUTPUT BUFFER STARTS     00163700
*                                                      HERE  @G64MPHD*/ 00163800
*            3 RCDCCHH CHAR(4),        /*CYL CYL HEAD HEAD-COUNT FLD */ 00163900
*              4 RCDCC CHAR(2),        /*CYL CYL - COUNT FIELD       */ 00164000
*              4 RCDHH CHAR(2),        /*HEAD HEAD - COUNT FIELD     */ 00164100
*            3 RECORDNO CHAR(1),       /*RECORD NUMBER - COUNT FIELD */ 00164200
*            3 KEYLEN FIXED(8),        /*KEY LENGTH - COUNT FIELD    */ 00164300
*            3 DATALEN FIXED(16),      /*DATA LENGTH - COUNT FIELD   */ 00164400
*          2 SDBUFFER CHAR(4104),  /*REST OF OUTPUT BUFFER   @G33VPHD*/ 00164500
*          2 SDCOMWRK CHAR(120);   /*SDUMP COMMON WORK AREA USED BY     00164600
*                                                   IEAVTSDU @G64MPHD*/ 00164700
KH5      DC    H'5'                                                     00164800
KH12     DC    H'12'                                                    00164900
KH4112   DC    H'4112'                                                  00165000
@SM05278 MVC   PRDTITLE(0,R12),1(R8)       MOVE DUMP TITLE              00165100
KF1      DC    F'1'                                                     00165200
KH1      EQU   KF1+2                                                    00165300
KF2      DC    F'2'                                                     00165400
KH2      EQU   KF2+2                                                    00165500
KF8      DC    F'8'                                                     00165600
KF10     DC    F'10'                                                    00165700
KF15     DC    F'15'                                                    00165800
KF16     DC    F'16'                                                    00165900
KF44     DC    F'44'                                                    00166000
KF64     DC    F'64'                                                    00166100
KF208    DC    F'208'                                                   00166200
KF584    DC    F'584'                                                   00166300
KF4096   DC    F'4096'                                                  00166400
KF4104   DC    F'4104'                                                  00166500
KH4104   EQU   KF4104+2                                                 00166600
KFM4088  DC    F'-4088'                                                 00166700
KFM4     DC    F'-4'                                                    00166800
KX00000F DC    XL4'0000000F'                                            00166900
@CF05056 DC    XL4'7FFFF000'                                            00167000
KX7FFFFF DC    XL4'7FFFFFFF'                                            00167100
@CF05273 DC    XL4'0000FFFF'                                            00167200
@CV05110 DC    V(IEAVTSDP)                                              00167300
@CC05046 DC    C'IEAVTSDT'                                              00167400
KEAVTSDH DC    C'IEAVTSDH'                                              00167500
@CC05092 DC    C'SVCDUMP '                                              00167600
@CC05094 DC    C'SYSMDUMP'                                              00167700
KXFFFF   DC    X'FFFF'                                                  00167800
*                                                                       00167900
         PRINT NOGEN                                                    00168000
*                                                                       00168100
         CVT   DSECT=YES                                                00168200
R0       EQU   00                      EQUATES FOR REGISTERS 0-15       00168300
R1       EQU   01                                                       00168400
R2       EQU   02                                                       00168500
R3       EQU   03                                                       00168600
R4       EQU   04                                                       00168700
R5       EQU   05                                                       00168800
R6       EQU   06                                                       00168900
R7       EQU   07                                                       00169000
R8       EQU   08                                                       00169100
R9       EQU   09                                                       00169200
R10      EQU   10                                                       00169300
R11      EQU   11                                                       00169400
R12      EQU   12                                                       00169500
R13      EQU   13                                                       00169600
R14      EQU   14                                                       00169700
R15      EQU   15                                                       00169800
TCBFIX   EQU   0                                                        00169900
TCBFRS   EQU   TCBFIX                                                   00170000
TCBPTR   EQU   0                                                        00170100
TCB      EQU   0                                                        00170200
TCBRBP   EQU   TCB                                                      00170300
TCBPIE   EQU   TCB+4                                                    00170400
TCBPMASK EQU   TCBPIE                                                   00170500
TCBCMP   EQU   TCB+16                                                   00170600
TCBCMPF  EQU   TCBCMP                                                   00170700
TCBCPP   EQU   TCBCMPF                                                  00170800
TCBSTCC  EQU   TCBCMPF                                                  00170900
TCBCDBL  EQU   TCBCMPF                                                  00171000
TCBCWTO  EQU   TCBCMPF                                                  00171100
TCBCIND  EQU   TCBCMPF                                                  00171200
TCBCMSG  EQU   TCBCMPF                                                  00171300
TCBTRN   EQU   TCB+20                                                   00171400
TCBABF   EQU   TCBTRN                                                   00171500
TCBMSS   EQU   TCB+24                                                   00171600
TCBPKF   EQU   TCB+28                                                   00171700
TCBFLGS  EQU   TCB+29                                                   00171800
TCBFLGS1 EQU   TCBFLGS                                                  00171900
TCBFLGS2 EQU   TCBFLGS+1                                                00172000
TCBFLGS3 EQU   TCBFLGS+2                                                00172100
TCBFLGS4 EQU   TCBFLGS+3                                                00172200
TCBFLGS5 EQU   TCBFLGS+4                                                00172300
TCBABWF  EQU   TCBFLGS5                                                 00172400
TCBJPQ   EQU   TCB+44                                                   00172500
TCBPURGE EQU   TCBJPQ                                                   00172600
TCBGRS   EQU   TCB+48                                                   00172700
TCBFSA   EQU   TCB+112                                                  00172800
TCBTME   EQU   TCB+120                                                  00172900
TCBJSTCB EQU   TCB+124                                                  00173000
TCBTSFLG EQU   TCB+148                                                  00173100
TCBTSTSK EQU   TCBTSFLG                                                 00173200
TCBSTAB  EQU   TCB+160                                                  00173300
TCBNSTAE EQU   TCBSTAB                                                  00173400
TCBTCT   EQU   TCB+164                                                  00173500
TCBTCTGF EQU   TCBTCT                                                   00173600
TCBSCNDY EQU   TCB+172                                                  00173700
TCBNDSP  EQU   TCBSCNDY                                                 00173800
TCBNDSP1 EQU   TCBNDSP+1                                                00173900
TCBNDSP2 EQU   TCBNDSP+2                                                00174000
TCBNDSP3 EQU   TCBNDSP+3                                                00174100
TCBJSCB  EQU   TCB+180                                                  00174200
TCBRECDE EQU   TCBJSCB                                                  00174300
TCBEXT1  EQU   TCB+196                                                  00174400
TCBBITS  EQU   TCB+200                                                  00174500
TCBNDSP4 EQU   TCBBITS                                                  00174600
TCBNDSP5 EQU   TCBBITS+1                                                00174700
TCBFLGS6 EQU   TCBBITS+2                                                00174800
TCBFLGS7 EQU   TCBBITS+3                                                00174900
TCBDAR   EQU   TCB+204                                                  00175000
TCBDARC  EQU   TCBDAR                                                   00175100
TCBEXT2  EQU   TCB+208                                                  00175200
TCBRTWA  EQU   TCB+224                                                  00175300
TCBXSCT  EQU   TCB+240                                                  00175400
TCBXSCT1 EQU   TCBXSCT                                                  00175500
TCBFOE   EQU   TCB+244                                                  00175600
TCBESTAE EQU   TCB+264                                                  00175700
TCBESTRM EQU   TCBESTAE+1                                               00175800
TCBFBYT1 EQU   TCB+276                                                  00175900
TCBRTM2  EQU   TCBFBYT1                                                 00176000
TCBFBYT2 EQU   TCB+277                                                  00176100
TCBRV124 EQU   TCB+278                                                  00176200
TCBRV133 EQU   TCB+279                                                  00176300
TCBXTNT2 EQU   0                                                        00176400
TCBGTF   EQU   TCBXTNT2                                                 00176500
TCBTFLG  EQU   TCBGTF                                                   00176600
TCBRV146 EQU   TCBXTNT2+28                                              00176700
TCBRV155 EQU   TCBXTNT2+29                                              00176800
TCBRV164 EQU   TCBXTNT2+30                                              00176900
TCBRV173 EQU   TCBXTNT2+31                                              00177000
RBPRFX   EQU   0                                                        00177100
RBSECPTR EQU   0                                                        00177200
RBBASIC  EQU   0                                                        00177300
RBEXRTNM EQU   RBBASIC                                                  00177400
RBTMFLD  EQU   RBEXRTNM                                                 00177500
RBSTAB   EQU   RBBASIC+10                                               00177600
XSTAB    EQU   RBSTAB                                                   00177700
RBSTAB1  EQU   XSTAB                                                    00177800
XSTAB1   EQU   RBSTAB1                                                  00177900
RBFTCKPT EQU   XSTAB1                                                   00178000
RBSTAB2  EQU   XSTAB+1                                                  00178100
XSTAB2   EQU   RBSTAB2                                                  00178200
RBTCBNXT EQU   XSTAB2                                                   00178300
RBFACTV  EQU   XSTAB2                                                   00178400
RBFDYN   EQU   XSTAB2                                                   00178500
RBECBWT  EQU   XSTAB2                                                   00178600
RBOPSW   EQU   RBBASIC+16                                               00178700
XRBPSW   EQU   RBOPSW                                                   00178800
RBOPSWB1 EQU   XRBPSW                                                   00178900
RBOPSWB2 EQU   XRBPSW+1                                                 00179000
RBLINK   EQU   RBBASIC+28                                               00179100
XRBLNK   EQU   RBLINK                                                   00179200
RBWCF    EQU   XRBLNK                                                   00179300
RBLINKB  EQU   XRBLNK+1                                                 00179400
RBGRSAVE EQU   RBBASIC+32                                               00179500
XRBREG   EQU   RBGRSAVE                                                 00179600
RBGRS0   EQU   XRBREG                                                   00179700
RBGRS1   EQU   XRBREG+4                                                 00179800
RBGRS2   EQU   XRBREG+8                                                 00179900
RBGRS3   EQU   XRBREG+12                                                00180000
RBGRS4   EQU   XRBREG+16                                                00180100
RBGRS5   EQU   XRBREG+20                                                00180200
RBGRS6   EQU   XRBREG+24                                                00180300
RBGRS7   EQU   XRBREG+28                                                00180400
RBGRS8   EQU   XRBREG+32                                                00180500
RBGRS9   EQU   XRBREG+36                                                00180600
RBGRS10  EQU   XRBREG+40                                                00180700
RBGRS11  EQU   XRBREG+44                                                00180800
RBGRS12  EQU   XRBREG+48                                                00180900
RBGRS13  EQU   XRBREG+52                                                00181000
RBGRS14  EQU   XRBREG+56                                                00181100
RBGRS15  EQU   XRBREG+60                                                00181200
XRBESA   EQU   RBBASIC+96                                               00181300
RBPREFIX EQU   0                                                        00181400
RBRSV019 EQU   RBPREFIX+14                                              00181500
RBRSV028 EQU   RBPREFIX+15                                              00181600
RBRSV041 EQU   RBPREFIX+27                                              00181700
RBRSV054 EQU   RBPREFIX+39                                              00181800
RBRTOPSW EQU   RBPREFIX+40                                              00181900
RBRTPSW2 EQU   RBRTOPSW+8                                               00182000
RBRTICIL EQU   RBRTPSW2                                                 00182100
RBFLAGS1 EQU   RBPREFIX+56                                              00182200
RBINTCDA EQU   RBPREFIX+61                                              00182300
RBSECT   EQU   0                                                        00182400
RBPPSAV  EQU   RBSECT                                                   00182500
@NM00018 EQU   RBSECT+10                                                00182600
@NM00019 EQU   @NM00018                                                 00182700
RBTRSVRB EQU   @NM00019                                                 00182800
@NM00021 EQU   @NM00018+1                                               00182900
RBETXR   EQU   @NM00021                                                 00183000
RBEP     EQU   RBSECT+12                                                00183100
RBPGMQ   EQU   RBSECT+24                                                00183200
@NM00028 EQU   RBSECT+28                                                00183300
@NM00029 EQU   @NM00028                                                 00183400
IRBEND   EQU   RBSECT+96                                                00183500
@NM00032 EQU   RBSECT+96                                                00183600
RBRSV138 EQU   @NM00032+7                                               00183700
RBSCBB   EQU   RBSECT+144                                               00183800
RBSPARM  EQU   RBSCBB+8                                                 00183900
RBSFLGS1 EQU   RBSPARM                                                  00184000
RBSIOPRC EQU   RBSFLGS1                                                 00184100
RBSOWNR  EQU   RBSCBB+12                                                00184200
RBSFLGS2 EQU   RBSOWNR                                                  00184300
RBSDATA  EQU   RBSCBB+16                                                00184400
RBSFLG3  EQU   RBSDATA                                                  00184500
RBRSV150 EQU   RBSECT+167                                               00184600
SCVTSECT EQU   0                                                        00184700
RTCT     EQU   0                                                        00184800
RTCTPLIB EQU   RTCT+4                                                   00184900
RTCTSAP  EQU   RTCTPLIB                                                 00185000
RTCTSAP1 EQU   RTCTSAP                                                  00185100
RTCTSAP2 EQU   RTCTSAP+1                                                00185200
RTCTSAP3 EQU   RTCTSAP+2                                                00185300
RTCTSUP  EQU   RTCTPLIB+4                                               00185400
RTCTSUP1 EQU   RTCTSUP                                                  00185500
RTCTSUP2 EQU   RTCTSUP+1                                                00185600
RTCTSUP3 EQU   RTCTSUP+2                                                00185700
RTCTSYD  EQU   RTCTPLIB+8                                               00185800
RTCTSY01 EQU   RTCTSYD                                                  00185900
RTCTSDDS EQU   RTCT+36                                                  00186000
RTCTDSNM EQU   RTCTSDDS                                                 00186100
RTCTFLG  EQU   RTCTSDDS+3                                               00186200
RTCTDSST EQU   RTCTFLG                                                  00186300
RTCTDSUS EQU   RTCTFLG                                                  00186400
RTCTDETP EQU   RTCTFLG                                                  00186500
RTCTDCB  EQU   RTCTSDDS+4                                               00186600
RTCTSDPL EQU   RTCT+156                                                 00186700
RTCTRFLG EQU   RTCT+176                                                 00186800
RTCTTDCB EQU   RTCT+184                                                 00186900
RTCTERID EQU   RTCT+224                                                 00187000
RTCTOPT  EQU   RTCT+236                                                 00187100
RTCTASO  EQU   RTCT+252                                                 00187200
RTCTASO1 EQU   RTCTASO                                                  00187300
RTCTASO2 EQU   RTCTASO+1                                                00187400
RTCTSDI  EQU   RTCT+254                                                 00187500
RTCTSDNA EQU   RTCTSDI                                                  00187600
RTCTINDX EQU   RTCTSDI+1                                                00187700
RTCTSDPR EQU   RTCTSDI+2                                                00187800
RTCTSDF  EQU   RTCT+264                                                 00187900
RTCTSDF1 EQU   RTCTSDF                                                  00188000
RTCTSDSH EQU   RTCTSDF1                                                 00188100
RTCTSDF2 EQU   RTCTSDF+1                                                00188200
RTCTZZZ3 EQU   RTCT+266                                                 00188300
RTCTSDF3 EQU   RTCT+268                                                 00188400
RTCTSDAS EQU   RTCTSDF3                                                 00188500
RTCTSDF4 EQU   RTCTSDF3+2                                               00188600
RTCTMOPT EQU   0                                                        00188700
RTCTMSAO EQU   RTCTMOPT                                                 00188800
RTCTSASD EQU   RTCTMSAO                                                 00188900
RTCTSAO1 EQU   RTCTSASD                                                 00189000
RTCTSAO2 EQU   RTCTSASD+1                                               00189100
RTCTSAPD EQU   RTCTMSAO+2                                               00189200
RTCTSAO3 EQU   RTCTSAPD                                                 00189300
RTCTSAO4 EQU   RTCTSAPD+1                                               00189400
RTCTSAMG EQU   RTCTSAO4                                                 00189500
RTCTMSUO EQU   RTCTMOPT+4                                               00189600
RTCTSUSD EQU   RTCTMSUO                                                 00189700
RTCTSUO1 EQU   RTCTSUSD                                                 00189800
RTCTSUO2 EQU   RTCTSUSD+1                                               00189900
RTCTSUPD EQU   RTCTMSUO+2                                               00190000
RTCTSUO3 EQU   RTCTSUPD                                                 00190100
RTCTSUO4 EQU   RTCTSUPD+1                                               00190200
RTCTSUMG EQU   RTCTSUO4                                                 00190300
RTCTMSYO EQU   RTCTMOPT+8                                               00190400
RTCTSD01 EQU   RTCTMSYO                                                 00190500
RTCTSD04 EQU   RTCTMSYO+3                                               00190600
RTCTSMMG EQU   RTCTSD04                                                 00190700
RTCTMSDO EQU   RTCTMOPT+12                                              00190800
RTCTSDOD EQU   RTCTMSDO                                                 00190900
RTCTSDO1 EQU   RTCTSDOD                                                 00191000
RTCTSDO2 EQU   RTCTSDOD+1                                               00191100
RTCTSDO3 EQU   RTCTMSDO+2                                               00191200
RTCTSDO4 EQU   RTCTMSDO+3                                               00191300
RTCTSDMG EQU   RTCTSDO4                                                 00191400
UCB      EQU   0                                                        00191500
UCBPTR   EQU   0                                                        00191600
UCBOB    EQU   0                                                        00191700
UCBJBNR  EQU   UCBOB                                                    00191800
UCBFL5   EQU   UCBOB+1                                                  00191900
UCBAF    EQU   UCBFL5                                                   00192000
UCBSTAT  EQU   UCBOB+3                                                  00192100
UCBCHAN  EQU   UCBOB+4                                                  00192200
UCBSFLS  EQU   UCBOB+6                                                  00192300
UCBFLA   EQU   UCBSFLS                                                  00192400
UCBFL1   EQU   UCBFLA                                                   00192500
UCBBSY   EQU   UCBFL1                                                   00192600
UCBNRY   EQU   UCBFL1                                                   00192700
UCBPST   EQU   UCBFL1                                                   00192800
UCBCUB   EQU   UCBFL1                                                   00192900
UCBFLB   EQU   UCBSFLS+1                                                00193000
UCBCHM   EQU   UCBOB+8                                                  00193100
UCBCHM1  EQU   UCBCHM                                                   00193200
UCBPTH0  EQU   UCBCHM1                                                  00193300
UCBPTH1  EQU   UCBCHM1                                                  00193400
UCBWGT   EQU   UCBOB+12                                                 00193500
UCBTYP   EQU   UCBOB+16                                                 00193600
UCBTBYT1 EQU   UCBTYP                                                   00193700
UCB1FEA5 EQU   UCBTBYT1                                                 00193800
UCB1FEA6 EQU   UCBTBYT1                                                 00193900
UCBTBYT2 EQU   UCBTYP+1                                                 00194000
UCB2OPT2 EQU   UCBTBYT2                                                 00194100
UCBDUDN1 EQU   UCB2OPT2                                                 00194200
UCB2OPT3 EQU   UCBTBYT2                                                 00194300
UCBDUDN2 EQU   UCB2OPT3                                                 00194400
UCB2OPT4 EQU   UCBTBYT2                                                 00194500
UCBRWTAU EQU   UCB2OPT4                                                 00194600
UCB2OPT6 EQU   UCBTBYT2                                                 00194700
UCB2OPT7 EQU   UCBTBYT2                                                 00194800
UCBTBYT3 EQU   UCBTYP+2                                                 00194900
UCBDVCLS EQU   UCBTBYT3                                                 00195000
UCB3TAPE EQU   UCBDVCLS                                                 00195100
UCBTBYT4 EQU   UCBTYP+3                                                 00195200
UCBEXTPT EQU   UCBOB+20                                                 00195300
UCBFLC   EQU   UCBEXTPT                                                 00195400
UCBDEV   EQU   UCBOB+24                                                 00195500
UCBCMEXT EQU   0                                                        00195600
UCBATI   EQU   UCBCMEXT+3                                               00195700
UCBFLP1  EQU   UCBCMEXT+5                                               00195800
UCBMIHTI EQU   UCBCMEXT+16                                              00195900
UCBMT    EQU   0                                                        00196000
UCBOCR   EQU   0                                                        00196100
WSAL     EQU   0                                                        00196200
WSAG     EQU   0                                                        00196300
WSAC     EQU   0                                                        00196400
SNPPARMS EQU   0                                                        00196500
SNPFLAG  EQU   SNPPARMS+1                                               00196600
SNPSDATA EQU   SNPPARMS+4                                               00196700
SNPSDAT0 EQU   SNPSDATA                                                 00196800
SNPSDAT1 EQU   SNPSDATA+1                                               00196900
SNPPDATA EQU   SNPPARMS+6                                               00197000
RMPL     EQU   0                                                        00197100
RMPLFLG1 EQU   RMPL                                                     00197200
RTM2WA   EQU   0                                                        00197300
RTM2DESC EQU   RTM2WA                                                   00197400
RTM2RT2D EQU   RTM2DESC+8                                               00197500
RTM2CODE EQU   RTM2WA+28                                                00197600
RTM2CCF  EQU   RTM2CODE                                                 00197700
RTM2PGCY EQU   RTM2WA+60                                                00197800
RTM2TRRY EQU   RTM2PGCY                                                 00197900
RTM2EEDR EQU   RTM2TRRY                                                 00198000
RTM2EREG EQU   RTM2EEDR                                                 00198100
RTM2APSW EQU   RTM2EEDR+64                                              00198200
RTM2EPSW EQU   RTM2APSW                                                 00198300
RTM2EMK1 EQU   RTM2EPSW                                                 00198400
RTM2MWP1 EQU   RTM2EPSW+1                                               00198500
RTM2INT1 EQU   RTM2EPSW+2                                               00198600
RTM2NXT1 EQU   RTM2EPSW+4                                               00198700
RTM2AEC1 EQU   RTM2APSW+8                                               00198800
RTM2ILC1 EQU   RTM2AEC1+1                                               00198900
RTM2INC1 EQU   RTM2AEC1+2                                               00199000
RTM2ICD1 EQU   RTM2INC1+1                                               00199100
RTM2EEDH EQU   RTM2TRRY+92                                              00199200
RTM2STCK EQU   RTM2EEDH                                                 00199300
RTM2MCHI EQU   RTM2EEDH+8                                               00199400
RTM2MCHS EQU   RTM2MCHI                                                 00199500
RTM2MCHD EQU   RTM2MCHI+1                                               00199600
RTM2RSR1 EQU   RTM2EEDH+12                                              00199700
RTM2RSR2 EQU   RTM2EEDH+13                                              00199800
RTM2FLGS EQU   RTM2TRRY+120                                             00199900
RTM2ERRA EQU   RTM2FLGS                                                 00200000
RTM2ERRB EQU   RTM2FLGS+1                                               00200100
RTM2ERRC EQU   RTM2FLGS+2                                               00200200
RTM2ERRD EQU   RTM2FLGS+3                                               00200300
RTM2ERFL EQU   RTM2ERRD                                                 00200400
RTM2CVER EQU   RTM2WA+186                                               00200500
RTM2TRRC EQU   RTM2CVER                                                 00200600
RTM2IOFS EQU   RTM2TRRC                                                 00200700
RTM2IOBP EQU   RTM2TRRC+2                                               00200800
RTM2SCBS EQU   RTM2TRRC+14                                              00200900
RTM2RTCD EQU   RTM2TRRC+26                                              00201000
RTM2SPLL EQU   RTM2RTCD+4                                               00201100
RTM2PARQ EQU   RTM2TRRC+46                                              00201200
RTM2CTL1 EQU   RTM2CVER+50                                              00201300
RTM2CMKA EQU   RTM2CTL1                                                 00201400
RTM2MWPA EQU   RTM2CTL1+1                                               00201500
RTM2PMKA EQU   RTM2CTL1+4                                               00201600
RTM2CTL2 EQU   RTM2CVER+58                                              00201700
RTM2CMKP EQU   RTM2CTL2                                                 00201800
RTM2MWPP EQU   RTM2CTL2+1                                               00201900
RTM2PMKP EQU   RTM2CTL2+4                                               00202000
RTM2SNAP EQU   RTM2CVER+66                                              00202100
RTM2SPRM EQU   RTM2SNAP+4                                               00202200
RTM2DPSL EQU   RTM2SNAP+24                                              00202300
RTM2FRM1 EQU   RTM2DPSL                                                 00202400
RTM2TO1  EQU   RTM2DPSL+4                                               00202500
RTM2FRM2 EQU   RTM2DPSL+8                                               00202600
RTM2TO2  EQU   RTM2DPSL+12                                              00202700
RTM2FRM3 EQU   RTM2DPSL+16                                              00202800
RTM2TO3  EQU   RTM2DPSL+20                                              00202900
RTM2FRM4 EQU   RTM2DPSL+24                                              00203000
RTM2TO4  EQU   RTM2DPSL+28                                              00203100
RTM2SECB EQU   RTM2CVER+138                                             00203200
RTM2ECBA EQU   RTM2SECB                                                 00203300
RTM2SFRG EQU   RTM2CVER+186                                             00203400
RTM2SCTL EQU   RTM2CVER+259                                             00203500
RTM2CCTL EQU   RTM2SCTL                                                 00203600
RTM2ABDR EQU   RTM2SCTL+4                                               00203700
RTM2ABID EQU   RTM2ABDR                                                 00203800
RTM2ABND EQU   RTM2ABDR+1                                               00203900
RTM2RCTL EQU   RTM2SCTL+6                                               00204000
RTM2INTF EQU   RTM2CVER+266                                             00204100
RTM2CTLR EQU   RTM2INTF                                                 00204200
RTM2TSKT EQU   RTM2INTF+2                                               00204300
RTM2ABDP EQU   RTM2INTF+4                                               00204400
RTM2ASIR EQU   RTM2INTF+5                                               00204500
RTM2FLX  EQU   RTM2INTF+6                                               00204600
RTM2FLX1 EQU   RTM2FLX                                                  00204700
RTM2FLX2 EQU   RTM2FLX+1                                                00204800
RTM2RECL EQU   RTM2CVER+274                                             00204900
RTM2DCTL EQU   RTM2RECL+12                                              00205000
RTM2ECTL EQU   RTM2RECL+13                                              00205100
RTM2TRYR EQU   RTM2RECL+16                                              00205200
RTM2TRF1 EQU   RTM2TRYR                                                 00205300
RTM2TRF2 EQU   RTM2TRYR+2                                               00205400
RTM2TRF3 EQU   RTM2TRYR+3                                               00205500
RTM2RECH EQU   RTM2CVER+294                                             00205600
RTM2RRG  EQU   RTM2CVER+314                                             00205700
RTM2CRG  EQU   RTM2CVER+378                                             00205800
RTM2RMIN EQU   RTM2WA+708                                               00205900
RTM2RMPL EQU   RTM2RMIN+4                                               00206000
RTM2RMWA EQU   RTM2RMIN+28                                              00206100
RTM2ERID EQU   RTM2WA+872                                               00206200
RTM2ENSN EQU   RTM2WA+884                                               00206300
RTM2SNLN EQU   RTM2ENSN                                                 00206400
RTM2RYRG EQU   RTM2WA+904                                               00206500
RTM2PTR  EQU   0                                                        00206600
STORAGE  EQU   0                                                        00206700
STORFRM  EQU   STORAGE                                                  00206800
STORTO   EQU   STORAGE+4                                                00206900
LPDE     EQU   0                                                        00207000
LPDENAME EQU   LPDE+8                                                   00207100
LPDEATTR EQU   LPDE+28                                                  00207200
LPDEATT2 EQU   LPDE+29                                                  00207300
LPDEMJNM EQU   LPDE+32                                                  00207400
LPDEPTR  EQU   0                                                        00207500
CDENTRY  EQU   0                                                        00207600
CDNAME   EQU   CDENTRY+8                                                00207700
CDATTR   EQU   CDENTRY+28                                               00207800
CDATTR2  EQU   CDENTRY+29                                               00207900
CDENTPTR EQU   0                                                        00208000
LCCA     EQU   0                                                        00208100
LCCAIHRC EQU   LCCA+520                                                 00208200
LCCAIHR1 EQU   LCCAIHRC                                                 00208300
LCCAIHR2 EQU   LCCAIHRC+1                                               00208400
LCCAIHR3 EQU   LCCAIHRC+2                                               00208500
LCCAIHR4 EQU   LCCAIHRC+3                                               00208600
LCCASPIN EQU   LCCA+524                                                 00208700
LCCASPN1 EQU   LCCASPIN                                                 00208800
LCCASPN2 EQU   LCCASPIN+1                                               00208900
LCCASPN3 EQU   LCCASPIN+2                                               00209000
LCCASPN4 EQU   LCCASPIN+3                                               00209100
LCCACPUS EQU   LCCA+536                                                 00209200
LCCADSF1 EQU   LCCA+540                                                 00209300
LCCADSF2 EQU   LCCA+541                                                 00209400
LCCACRFL EQU   LCCA+692                                                 00209500
LCCACREX EQU   LCCA+693                                                 00209600
LCCALKFG EQU   LCCA+694                                                 00209700
LCCASRBF EQU   LCCA+720                                                 00209800
LCCAVT   EQU   0                                                        00209900
PCCA     EQU   0                                                        00210000
PCCATMST EQU   PCCA+128                                                 00210100
PCCATMFL EQU   PCCATMST                                                 00210200
PCCATODE EQU   PCCATMST+1                                               00210300
PCCACCE  EQU   PCCATMST+2                                               00210400
PCCAINTE EQU   PCCATMST+3                                               00210500
PCCAEMSB EQU   PCCA+136                                                 00210600
PCCAEMSI EQU   PCCAEMSB                                                 00210700
PCCARISP EQU   PCCAEMSI                                                 00210800
PCCAEMS2 EQU   PCCAEMSI+1                                               00210900
PCCAEMS3 EQU   PCCAEMSI+2                                               00211000
PCCARMSB EQU   PCCAEMSI+3                                               00211100
PCCACHAN EQU   PCCA+224                                                 00211200
PCCAWERP EQU   PCCA+280                                                 00211300
PCCACHPF EQU   PCCAWERP+4                                               00211400
PCCACHBL EQU   PCCAWERP+5                                               00211500
PCCACHVA EQU   PCCAWERP+6                                               00211600
PCCACHTS EQU   PCCAWERP+7                                               00211700
PCCACHS1 EQU   PCCA+288                                                 00211800
PCCACHS2 EQU   PCCA+289                                                 00211900
PCCACHRB EQU   PCCA+290                                                 00212000
PCCACHF1 EQU   PCCA+308                                                 00212100
PCCACHF2 EQU   PCCA+309                                                 00212200
PCCACHF3 EQU   PCCA+310                                                 00212300
PCCACHF4 EQU   PCCA+311                                                 00212400
PCCAATTR EQU   PCCA+376                                                 00212500
PCCAVT   EQU   0                                                        00212600
PCCAT00P EQU   PCCAVT                                                   00212700
ASVT     EQU   0                                                        00212800
ASVTBEGN EQU   ASVT+512                                                 00212900
ASVTMAXU EQU   ASVT+516                                                 00213000
ASVTFRST EQU   ASVT+524                                                 00213100
ASVTENTY EQU   ASVT+528                                                 00213200
GDA      EQU   0                                                        00213300
GVSMFLAG EQU   GDA                                                      00213400
GDAFLAGS EQU   GVSMFLAG                                                 00213500
VRPOSTQ  EQU   GDA+32                                                   00213600
VRWAITQ  EQU   GDA+40                                                   00213700
IOCOM    EQU   0                                                        00213800
IOCATTBL EQU   IOCOM+68                                                 00213900
IOCOMPTR EQU   0                                                        00214000
ASCB     EQU   0                                                        00214100
ASCBSUPC EQU   ASCB+16                                                  00214200
ASCBASID EQU   ASCB+36                                                  00214300
ASCBRSM  EQU   ASCB+52                                                  00214400
ASCBRSMF EQU   ASCBRSM                                                  00214500
ASCBFW1  EQU   ASCB+100                                                 00214600
ASCBRCTF EQU   ASCBFW1+2                                                00214700
ASCBFLG1 EQU   ASCBFW1+3                                                00214800
ASCBASXB EQU   ASCB+108                                                 00214900
ASCBDSP1 EQU   ASCB+114                                                 00215000
ASCBFLG2 EQU   ASCB+115                                                 00215100
ASCBLKGP EQU   ASCB+128                                                 00215200
ASCBSRQ  EQU   ASCB+180                                                 00215300
ASCBSRQ1 EQU   ASCBSRQ                                                  00215400
ASCBSRQ2 EQU   ASCBSRQ+1                                                00215500
ASCBSRQ3 EQU   ASCBSRQ+2                                                00215600
ASCBSRQ4 EQU   ASCBSRQ+3                                                00215700
ASCBSRBM EQU   ASCB+195                                                 00215800
ASXB     EQU   0                                                        00215900
ASXBSPSA EQU   ASXB+112                                                 00216000
ASXBAEQ  EQU   ASXB+168                                                 00216100
ASXBSECR EQU   ASXB+199                                                 00216200
ASCBSFLG EQU   ASXBSECR                                                 00216300
ASXBCASW EQU   ASXB+216                                                 00216400
ASXBCRB1 EQU   ASXBCASW                                                 00216500
SDWORK   EQU   0                                                        00216600
SDCNTRL  EQU   SDWORK                                                   00216700
SDECB    EQU   SDCNTRL                                                  00216800
SDIOB    EQU   SDCNTRL+4                                                00216900
SDDSCHAR EQU   SDCNTRL+60                                               00217000
SDRCTMAX EQU   SDDSCHAR                                                 00217100
SDTRCMAX EQU   SDDSCHAR+2                                               00217200
GLOBADDR EQU   SDCNTRL+72                                               00217300
RANGEPTR EQU   SDCNTRL+76                                               00217400
ENDADRNT EQU   SDCNTRL+80                                               00217500
SDWSDPL  EQU   SDCNTRL+96                                               00217600
SVCDOPTS EQU   SDCNTRL+100                                              00217700
SVCDOPT1 EQU   SVCDOPTS                                                 00217800
SVCDOPT2 EQU   SVCDOPTS+1                                               00217900
FLAGSONE EQU   SDCNTRL+102                                              00218000
FLGRTFUL EQU   FLAGSONE                                                 00218100
FLGFLAST EQU   FLAGSONE                                                 00218200
SVCDMPNM EQU   SDCNTRL+128                                              00218300
ADDADRWA EQU   SDCNTRL+196                                              00218400
ADDADRWB EQU   SDCNTRL+200                                              00218500
ADDADRWC EQU   SDCNTRL+204                                              00218600
ADDADRWD EQU   SDCNTRL+208                                              00218700
SDRETCOD EQU   SDCNTRL+212                                              00218800
SDWRGSV1 EQU   SDCNTRL+216                                              00218900
SDWRGSV2 EQU   SDCNTRL+288                                              00219000
DSTINDEX EQU   SDCNTRL+360                                              00219100
DSINDEX  EQU   SDCNTRL+362                                              00219200
CVRT11   EQU   SDCNTRL+368                                              00219300
CVRT1    EQU   CVRT11+4                                                 00219400
CVRT2    EQU   SDCNTRL+376                                              00219500
SDCVRT4  EQU   CVRT2+2                                                  00219600
SVC994ID EQU   SDCNTRL+384                                              00219700
M994REQ  EQU   SDCNTRL+388         '00'X PRIOR TO                       00219800
*                                        IEAVTSDH OR                    00219900
*                                       AFTER IEAVTSDC                  00220000
*                                   'Y'C IF IEAVTSDH                    00220100
*                                        REQUESTS                       00220200
*                                        IEAVTSDC TO                    00220300
*                                        PUT OUT MSG.                   00220400
*                                   'N'C IF IEAVTSDH                    00220500
*                                        INDICATES                      00220600
*                                        IEAVTSDC NOT                   00220700
*                                        TO ISSUE MSG.                  00220800
SVCNAME  EQU   SDCNTRL+392                                              00220900
SVCNM1T5 EQU   SVCNAME                                                  00221000
SVCNM6T8 EQU   SVCNAME+5                                                00221100
SVCNM7T8 EQU   SVCNM6T8+1                                               00221200
CCWCHAIN EQU   SDCNTRL+400                                              00221300
CCW1     EQU   CCWCHAIN                                                 00221400
CMDCODE1 EQU   CCW1                                                     00221500
DATAADR1 EQU   CCW1+1                                                   00221600
FLAGS1   EQU   CCW1+4                                                   00221700
CMDC1    EQU   FLAGS1                                                   00221800
SLI1     EQU   FLAGS1                                                   00221900
BYTECNT1 EQU   CCW1+6                                                   00222000
CCW2     EQU   CCWCHAIN+8                                               00222100
CMDCODE2 EQU   CCW2                                                     00222200
DATAADR2 EQU   CCW2+1                                                   00222300
FLAGS2   EQU   CCW2+4                                                   00222400
CCW3     EQU   CCWCHAIN+16                                              00222500
CMDCODE3 EQU   CCW3                                                     00222600
DATAADR3 EQU   CCW3+1                                                   00222700
FLAGS3   EQU   CCW3+4                                                   00222800
SLI3     EQU   FLAGS3                                                   00222900
SKIP3    EQU   FLAGS3                                                   00223000
BYTECNT3 EQU   CCW3+6                                                   00223100
SDSMBUFF EQU   SDCNTRL+424                                              00223200
SDOUTBUF EQU   SDWORK+432                                               00223300
RCDCCHH  EQU   SDOUTBUF                                                 00223400
RCDCC    EQU   RCDCCHH                                                  00223500
RCDHH    EQU   RCDCCHH+2                                                00223600
RECORDNO EQU   SDOUTBUF+4                                               00223700
KEYLEN   EQU   SDOUTBUF+5                                               00223800
DATALEN  EQU   SDOUTBUF+6                                               00223900
SDBUFFER EQU   SDWORK+440                                               00224000
PRDINPUT EQU   0                                                        00224100
PRDHDRID EQU   PRDINPUT                                                 00224200
PRDRECID EQU   PRDINPUT+1                                               00224300
PRDASID  EQU   PRDINPUT+2                                               00224400
PRDMODNM EQU   PRDINPUT+4                                               00224500
PRDTODVL EQU   PRDINPUT+12                                              00224600
PRDTITLE EQU   PRDINPUT+20                                              00224700
PRDREGS  EQU   PRDINPUT+120                                             00224800
PRDFPR   EQU   PRDREGS                                                  00224900
PRDGPR   EQU   PRDREGS+32                                               00225000
PRDCR    EQU   PRDREGS+96                                               00225100
PRDPSW   EQU   PRDREGS+160                                              00225200
PRDCVT   EQU   PRDINPUT+288                                             00225300
PRDERRID EQU   PRDINPUT+324                                             00225400
PRDINPTR EQU   0                                                        00225500
ERRWORK  EQU   0                                                        00225600
ERRFLGS1 EQU   ERRWORK                                                  00225700
ERRFLGS2 EQU   ERRWORK+1                                                00225800
ERRENDMP EQU   ERRFLGS2                                                 00225900
ERRCDOUT EQU   ERRFLGS2                                                 00226000
ERRADDR  EQU   ERRWORK+4                                                00226100
ERRBASE1 EQU   ERRWORK+12                                               00226200
ERRFLAG3 EQU   ERRWORK+16                                               00226300
ERRCSDT  EQU   ERRFLAG3                                                 00226400
STORANGE EQU   0                                                        00226500
STARTADR EQU   STORANGE                                                 00226600
ENDADDR  EQU   STORANGE+4                                               00226700
LASTRNGE EQU   ENDADDR                                                  00226800
TRTPTRS  EQU   0                                                        00226900
TRTCURR  EQU   TRTPTRS                                                  00227000
TRTENTY  EQU   0                                                        00227100
UCBBGN   EQU   0                                                        00227200
UCBDCPTR EQU   0                                                        00227300
RBSECS03 EQU   RBEP                                                     00227400
RBCDE    EQU   RBSECS03                                                 00227500
RBCDFLGS EQU   RBCDE                                                    00227600
RBCDE1   EQU   RBCDE+1                                                  00227700
RBSECS05 EQU   RBPGMQ                                                   00227800
RBSQE    EQU   RBSECS05                                                 00227900
RBSECS06 EQU   RBPGMQ                                                   00228000
RBIQE    EQU   RBSECS06                                                 00228100
RBSECS07 EQU   RBPGMQ                                                   00228200
RBIQE2   EQU   RBSECS07                                                 00228300
RBSECS08 EQU   IRBEND                                                   00228400
RBSECS09 EQU   IRBEND                                                   00228500
SCVTSPTR EQU   CVTABEND                                                 00228600
UCBOBS01 EQU   UCBDEV                                                   00228700
UCBSTAB  EQU   UCBOBS01+10                                              00228800
UCBBSVL  EQU   UCBSTAB                                                  00228900
UCBPRSRS EQU   UCBSTAB                                                  00229000
UCBDMCT  EQU   UCBOBS01+11                                              00229100
UCBFL4   EQU   UCBOBS01+13                                              00229200
UCBOBS02 EQU   UCBDEV                                                   00229300
UCBTFL1  EQU   UCBOBS02+19                                              00229400
UCBXTN   EQU   UCBOBS02+20                                              00229500
UCBVOPT  EQU   UCBXTN                                                   00229600
UCBOBS03 EQU   UCBDEV                                                   00229700
UCBXTADR EQU   UCBOBS03                                                 00229800
UCBOBS04 EQU   UCBDEV                                                   00229900
UCBBTA   EQU   UCBOBS04+12                                              00230000
UCBOBS06 EQU   UCBDEV                                                   00230100
UCBAOF   EQU   UCBOBS06                                                 00230200
UCBAOF1  EQU   UCBAOF                                                   00230300
UCBAOF2  EQU   UCBAOF+1                                                 00230400
@NM00085 EQU   UCBOBS06+3                                               00230500
UCBIRB   EQU   UCBOBS06+4                                               00230600
UCBGRAF  EQU   UCBIRB                                                   00230700
UCBLDNCA EQU   UCBOBS06+8                                               00230800
UCBRDYQ  EQU   UCBLDNCA                                                 00230900
UCBIRLN  EQU   UCBRDYQ                                                  00231000
UCBLDNCB EQU   UCBRDYQ+1                                                00231100
UCBCTLNK EQU   UCBOBS06+12                                              00231200
UCBOBS07 EQU   UCBDEV                                                   00231300
UCBOBS08 EQU   UCBDEV                                                   00231400
UCBCTCAD EQU   UCBOBS08                                                 00231500
UCBCTCF1 EQU   UCBOBS08+4                                               00231600
UCBOBS09 EQU   UCBDEV                                                   00231700
ASXBPTR  EQU   ASCBASXB                                                 00231800
PRDFLC   EQU   PRDFPR                                                   00231900
@NM00199 EQU   PRDHDRID                                                 00232000
PRDKEY1  EQU   @NM00199                                                 00232100
PRDKEY2  EQU   @NM00199+1                                               00232200
PRDADDR  EQU   @NM00199+4                                               00232300
PRDDATA  EQU   @NM00199+8                                               00232400
@NM00201 EQU   PRDHDRID                                                 00232500
PRDFLAGS EQU   @NM00201+2                                               00232600
ERRORID  EQU   RTCTERID                                                 00232700
SDPRDHDR EQU   PRDCVT                                                   00232800
SDPRDLST EQU   SDPRDHDR+4                                               00232900
SDPRDOPT EQU   SDPRDHDR+44                                              00233000
LCLTODVL EQU   PRDTODVL                                                 00233100
@ENDDATA EQU   *                                                        00233200
*                                                                       00233300
         IHAPSA DSECT=YES                                               00233400
*                                                                       00233500
         IEZIOB DSECT=YES                                               00233600
*                                                                       00233700
         IEZDEB                                                         00233800
*                                                                       00233900
         DCBD  DSORG=DA                                                 00234000
*                                                                       00234100
         IHASDUMP DSECT=YES                                             00234200
*                                                                       00234300
*        IHADVCT - DEVICE CHARACTERISTICS TABLE                         00234400
*                                                                       00234500
         IHADVCT                                                        00234600
*                                                                       00234700
         END   IEAVTSDH,(C'PLS2018',0802,78348)                         00234800
./ ENDUP
??
//ASM1     EXEC SMPASM,M=IEAVTSDH,COND=(0,NE)
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA',COND=(0,NE)
//SMPPTFIN DD  DATA,DLM='??'
++USERMOD(RP00001) .
++VER(Z038) FMID(EBB1102)
  PRE(UZ30889).
++IF FMID(FBB1221) REQ(RP00002)
 /*
   PROBLEM DESCRIPTION:
     AMDPRDMP DID NOT PRODUCE A VALID DUMP ON DDNAME=PRINTER,
       IF THE WORK DATASET WITH DDNAME=SYSUT1 IS ALLOCATED ON A
       D/3375, D/3380 OR D/3390, BECAUSE THE NUMBER OF BLOCKS
       ON A TRACK ARE NOT CORRECTLY CALCULATED.

   TEMPORARY FIX:
       UPDATE MODULE AMDPREAD TO ADD THE RIGHT NUMBER OF BLOCKS
       PER TRACK FOR D/3375, D/3380 AND D/3390.

   SPECIAL CONDITIONS:
     ACTION:
       NONE.

   COMMENTS:
     SKYBIRD SYSTEMS: PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 1.
     REWORK HISTORY:
       2023-11-06: INITIAL AVAILABILITY.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       AMDPREAD
 */.
++ZAP (AMDPREAD).
NAME AMDPREAD
VER 0B88 9101,BE8B                 TM    DEVTOLRB,B'00000001'
VER 0B8C 47E0,BB9A                 BNO   @RF00604
VER 0FA0 0000,0000,0000,0000       << PATCH AREA >>
VER 0FA8 0000,0000,0000,0000
VER 0FB0 0000,0000,0000,0000
VER 0FB8 0000,0000,0000,0000
VER 0FC0 0000,0000,0000,0000
VER 0FC8 0000,0000,0000,0000
VER 0FD0 0000,0000,0000,0000
VER 0FE0 0000
REP 0B88 47F0,BF9A                 B     PATCH
REP 0FA0 950F,BE7D        PATCH    CLI   @NM00005+3,X'0F' 3390?
REP 0FA4 4780,BFBE                 BE    #3390
REP 0FA8 950E,BE7D                 CLI   @NM00005+3,X'0E' 3380?
REP 0FAC 4780,BFC6                 BE    #3380
REP 0FB0 950C,BE7D                 CLI   @NM00005+3,X'0C' 3375?
REP 0FB4 4780,BFCE                 BE    #3375
REP 0FB8 9101,BE8B                 TM    DEVTOLRB,B'00000001'
REP 0FBC 47E0,BB9A                 BNO   @RF00604
REP 0FC0 47F0,BB90                 B     <<AFTER BNO   @RF00604)
REP 0FC4 D201,BE40,BFD6   #3390    MVC   RECTRK,=AL2(12)  12 RECS/TRK
REP 0FCA 07FE                      BR    14
REP 0FCC D201,BE40,BFD8   #3380    MVC   RECTRK,=AL2(10)  10 RECS/TRK
REP 0FD2 07FE                      BR    14
REP 0FD4 D201,BE40,BFDA   #3375    MVC   RECTRK,=AL2(7)   07 RECS/TRK
REP 0FDA 07FE                      BR    14
REP 0FDC 000C                      DC    AL2(12)
REP 0FDE 000A                      DC    AL2(10)
REP 0FE0 0007                      DC    AL2(7)
++USERMOD(RP00002).
++VER(Z038) FMID(FBB1221)
  PRE(UZ54567)
  REQ(RP00001)
 /*
   PROBLEM DESCRIPTION:
     MODULE IEAVTSDH INCORRECTLY CALCULATES THE NUMBER OF DUMP
     RECORDS THAT CAN BE WRITTEN PER TRACK TO A SYS1.DUMPXX DATA
     SET IF THE SYS1.DUMPXX DATA SET IS ALLOCATED ON A 3390 DEVICE

     CHANGES MADE BY THIS SYSMOD INCLUDE -
     1. REPLACE THE RYO RECORDS PER TRACK CALCULATION WITH THE
        TRKCALC SYSTEM SERVICE TO PROVIDE SUPPORT FOR ALL DASD
        DEVICE TYPES INCLUDING 3375/3380/3390 MODULO DASD
     2. USE OF SYSTEM MAPPING MACROS
     3. NUMEROUS SMALL CHANGES TO ENHANCE SOURCE CODE
        READABILITY

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVTSDH IN LPA LOAD MODULE IGC0005A
 */.
++MOD(IEAVTSDH) DISTLIB(AOSC5) TXLIB(UMODOBJ).
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(RP00001 RP00002)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(RP00001 RP00002)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(RP00001 RP00002)
        DIS(WRITE)
        .
/*
//
