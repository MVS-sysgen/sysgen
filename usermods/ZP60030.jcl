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
