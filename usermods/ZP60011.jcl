//ZP60011  JOB (SYSGEN),'J06 M25: ZP60011',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  CAPTURE CHANNEL PROGRAM CCWS IN GTF SIO TRACE RECORD.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60011)            /* TRACE SIO CHANNEL PROGRAM */  .
++VER(Z038) FMID(FBB1221)
 /*
   PROBLEM DESCRIPTION:
     GTF CANNOT TRACE ANY CCWS USED IN A CHANNEL PROGRAM.
       GTF DOES NOT PROVIDE A FACILITY FOR PERFORMING A CCW TRACE.
       CCW TRACES ARE A SIGNIFICANT TOOL FOR DIAGNOSING I/O PROBLEMS.

       THIS USERMOD CHANGES THE SIO RECORD BUILD ROUTINE TO CAPTURE
       UP TO 25 CCWS IN AN I/O PROGRAM PROCESSED BY SIO, AND PLACE
       THE CONTENTS INTO THE GTF SIO TRACE RECORD.  STRICTLY
       SPEAKING, THIS IS NOT A CCW TRACE AS THERE IS NO GUARANTEE
       THAT ALL OF THE CAPTURED CCWS WERE EXECUTED BY THE CHANNEL.
       UP TO 8 BYTES OF WRITE DATA MAY BE CAPTURED FOR NON-DASD
       DEVICES WHICH IS STORED IN THE SEEK ADDRESS TRACE FIELD.

       ADDITIONALLY THE SIO TRACE RECORD FORMAT ROUTINE IS ALTERED
       TO FORMAT CAPTURED CCWS, EACH ONE ON ITS OWN PRINT LINE.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 11.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       AHLTSIO
       AMDSYS00
 */.
++MOD(AHLTSIO) DISTLIB(AOS11).
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
//         DD  DSN=SYS1.APVTMACS,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IO,UIO,SIO COMPREHENSIVE BUILD ROUTINE                 *
                        '
*
*  MODIFIED BY GREG PRICE 27TH FEBRUARY 2003 FOR USERMOD ZP60011
*           TO GTF TRACE A CHANNEL PROGRAM PROCESSED BY SIO
*
AHLTSIO  CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(24)                                             0001
         DC    C'AHLTSIO  78.217 '                                 0001
         DC    C'ZP60011 '                                      ZP60011
         DROP  R15
@PROLOG  BALR  R11,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R11                                         0001
         EJECT
         B     @PB00002                                            0184
         USING PSA,0
AHLPRECM DS    0H                                                  0185
               USING AHLTSIO,R11           NEW ADDRESSABILITY
               SETFRR A,FRRAD=(RTCAREG),WRKREGS=(7,9),PARMAD=(R9)
*   SVR14NRM=R10;                   /* SAVE RETURN ADDR TO MCIH      */
         ST    R10,SVR14NRM(,R9)                                   0186
*   GENERATE REFS(FLCPOPSW,AHLPSCM2);                              0187
               TM  FLCPOPSW+1,X'81'           PK 0-7 + SUPER STATE
               BNZ  AHLPSCM2                 EXIT
*   MCATRAIL=ZERO;                  /* ZERO FLAG FIELD               */
         MVC   MCATRAIL(8,AWSAPTR),ZERO                            0188
*   PRECPUID=PSACPUSA;              /* GATHER CPUID          @YM03489*/
         LH    R12,PSACPUSA                                        0189
         STH   R12,PRECPUID(,AWSAPTR)                              0189
*   RETURN;                                                        0190
@EL00002 DS    0H                                                  0190
@EF00002 DS    0H                                                  0190
@ER00002 BR    R14                                                 0190
*   END;                                                           0191
@PB00002 DS    0H                                                  0191
*   GEN(EJECT);                                                    0192
         EJECT
*AHLPSTCM:                                                         0193
*   MCATRL4='00'X;                  /* CLEAR ANY DEBUG BITS          */
AHLPSTCM MVI   MCATRL4(AWSAPTR),X'00'                              0193
*AHLPSCM1:                                                         0194
*                                                                  0194
*   /*****************************************************************/
*   /*                                                               */
*   /* ISSUE STACK MACRO TO CAUSE TRC ENTRY TO BE MOVED TO GTF       */
*   /* BUFFERS                                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0194
*   DO;                             /* AHLSTACK                    0194
*                                      DLENGTH(RECLEN)AID(255)FID(00)*/
*                                                                  0194
AHLPSCM1 DS    0H                                                  0195
*     /***************************************************************/
*     /*                                                             */
*     /* THE FOLLOWING CODE IS GENERATED BY THE AHLSTACK MACRO. THIS */
*     /* CODE BUILDS THE GTF RECORD PREFIX AND THEN CALLS AHLSBLOK TO*/
*     /* STACK THE RECORD.                                           */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0195
*     DCL                                                          0195
*       1 AHLGREC INT BASED(AHLRECPR) BDY(WORD),/* MAPPING FOR RECORD
*                                      PREFIX                        */
*        2 AHLGLGTH PTR(16),        /* LENGTH OF RECORD PLUS PREFIX  */
*        2 AHLGRES PTR(16),         /* FOR COMPATIBILITY TO QSAM     */
*        2 AHLGAID PTR(8),          /* APPLICATION ID                */
*        2 AHLGFID PTR(8),          /* FORMAT APPENDAGE ID           */
*        2 AHLGTST CHAR(8),         /* TIMESTAMP IF SPECIFIED        */
*         3 AHLGEIDS PTR(16) BDY(BYTE),/* RECORD EVENT IDENTIFIER IF
*                                      NO TIMESTAMP                  */
*        2 AHLGEIDL PTR(16);        /* RECORD EVENT IDENTIFIER WITH
*                                      TIMESTAMP                     */
*     DCL                                                          0196
*       AHLRECPR PTR(31) REG(1),    /* USED AS POINTER TO RECORD   0196
*                                      PREFIX AREA                   */
*       AHLSBREG PTR(31) REG(14),   /* USED TO HOLD A STABLE COPY OF
*                                      THE SBLOKRTN FIELD IN THE   0196
*                                      GTFPCT                        */
*       AHLRETCD PTR(31) REG(15);   /* USED TO PASS RETURN CODE TO 0196
*                                      INVOKER                       */
*     DCL                                                          0197
*       AHLSBLOK ENTRY OPTIONS(NOEXIT,SEQFLOW,REFS(MCAWSA),SETS(   0197
*           AHLRETCD),NOFLOWS) BASED(AHLSBREG) VALRG(0);/* ENTRY FOR
*                                      AHLSBLOK                      */
*     RFY                                                          0198
*      (AHLRECPR,                                                  0198
*       AHLSBREG,                                                  0198
*       AHLRETCD) RSTD;                                            0198
*     IF OPTTIME='1'B THEN          /* CHECK FOR TIMESTAMP REQUESTED */
         TM    OPTTIME(PCTPTR),B'00000001'                         0199
         BNO   @RF00199                                            0199
*       DO;                         /* YES, TIMESTAMP                */
*         AHLRECPR=ADDR(MCARECPR);  /* SETUP ADDRESSIBILITY TO PREFIX
*                                      FOR PREFIX WITH TIMESTAMP     */
         LA    AHLRECPR,MCARECPR(,AWSAPTR)                         0201
*         AHLGLGTH=RECLEN+16;       /* SET LENGTH TO INCLUDE ***   0202
*                                      DLENGTH *** PLUS TIMESTAMP  0202
*                                      PREFIX                        */
         LA    R12,16                                              0202
         ALR   R12,RECLEN                                          0202
         STH   R12,AHLGLGTH(,AHLRECPR)                             0202
*         GEN(STCK AHLGTST(AHLRECPR) PUT TOD CLOCK VALUE INTO PREFIX )
*         SETS(AHLGTST) NOEXIT SEQFLOW NOFLOWS;                    0203
         STCK AHLGTST(AHLRECPR) PUT TOD CLOCK VALUE INTO PREFIX
*         AHLGEIDL=MCAMCR->MCREID;  /* *** EID *** NOT SPECIFIED PUT
*                                      DEFAULT INTO PREFIX           */
         L     R12,MCAMCR(,AWSAPTR)                                0204
         LH    R12,MCREID(,R12)                                    0204
         N     R12,@CF03270                                        0204
         STH   R12,AHLGEIDL(,AHLRECPR)                             0204
*       END;                                                       0205
*     ELSE                          /* NO TIMESTAMP TO BE DONE, BUILD
*                                      SHORT PREFIX                  */
*       DO;                                                        0206
         B     @RC00199                                            0206
@RF00199 DS    0H                                                  0207
*         AHLRECPR=ADDR(MCARECPR)+8;/* SETUP ADDRESSIBILITY TO FOR 0207
*                                      SHORT PREFIX WITHOUT TIMESTAMP*/
         LA    R12,8                                               0207
         LA    AHLRECPR,MCARECPR(,AWSAPTR)                         0207
         ALR   AHLRECPR,R12                                        0207
*         AHLGLGTH=RECLEN+8;        /* PUT TOTAL RECORD LENGTH INTO
*                                      PREFIX *** DLENGTH *** PLUS 0208
*                                      PREFIX LENGTH                 */
         ALR   R12,RECLEN                                          0208
         STH   R12,AHLGLGTH(,AHLRECPR)                             0208
*         AHLGEIDS=MCAMCR->MCREID;  /* *** EID *** NOT SPECIFIED PUT
*                                      DEFAULT INTO PREFIX           */
         L     R12,MCAMCR(,AWSAPTR)                                0209
         LH    R12,MCREID(,R12)                                    0209
         N     R12,@CF03270                                        0209
         STH   R12,AHLGEIDS(,AHLRECPR)                             0209
*       END;                                                       0210
*     AHLGRES=0;                    /* ZERO OUT FOR QSAM           0211
*                                      COMPATIBILITY                 */
@RC00199 SLR   R12,R12                                             0211
         STH   R12,AHLGRES(,AHLRECPR)                              0211
*     AHLGAID=255;                  /* PUT *** AID *** INTO PREFIX   */
         MVI   AHLGAID(AHLRECPR),X'FF'                             0212
*     AHLGFID=00;                   /* PUT *** FID *** INTO PREFIX   */
         MVI   AHLGFID(AHLRECPR),X'00'                             0213
*     AHLRETCD=4;                   /* SET DEFAULT RETURN CODE       */
         LA    AHLRETCD,4                                          0214
*     AHLSBREG=SBLOKRTN;            /* GET A STABLE COPY OF AHLSBLOK
*                                      ADDR                          */
         L     AHLSBREG,SBLOKRTN(,PCTPTR)                          0215
*     IF AHLSBREG^=0 THEN           /* CHECK TO SEE IF AHLSBLOK IS 0216
*                                      AVAILABLE                     */
         CR    AHLSBREG,R12                                        0216
         BE    @RF00216                                            0216
*       CALL AHLSBLOK;              /* YES, SO CALL AHLSBLOK TO BLOCK
*                                      RECORD                        */
         LR    R15,AHLSBREG                                        0217
         BALR  R14,R15                                             0217
*     RFY                                                          0218
*      (AHLRECPR,                                                  0218
*       AHLSBREG,                                                  0218
*       AHLRETCD) UNRSTD;                                          0218
@RF00216 DS    0H                                                  0219
*   END;                                                           0219
*AHLPSCM2:                                                         0220
*   ;                                                              0220
AHLPSCM2 DS    0H                                                  0221
*   GEN(SETFRR D,WRKREGS=(7,9));                                   0221
         SETFRR D,WRKREGS=(7,9)
*   GO TO R10;                      /* RESTORE RETURN ADDR AND RETURN
*                                      TO MCIH                       */
         BR    R10                                                 0222
*   GEN(EJECT);                                                    0223
*                                                                  0223
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* GATHER DATA AND BUILD IO TRACE RECORD                         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0224
*AHLTIO:                                                           0224
*   ;                                                              0224
AHLTIO   DS    0H                                                  0225
*   GENERATE;                                                      0225
         USING *,R15
         L     R11,BASEADDR
         DROP  R15
*   R10=R14;                        /* SAVE RETURN ADDRESS           */
         LR    R10,R14                                             0226
*   RTCAREG=ADDR(FRRIO);            /* ERROR ROUTINE ADDR            */
         LA    RTCAREG,FRRIO                                       0227
*   CALL AHLPRECM;                                                 0228
         BAL   R14,AHLPRECM                                        0228
*   IOCSID=PSACSID;                 /* CURRENT CHANNEL SET ID        */
         LH    R12,PSACSID
         STC   R12,IOCSID(,AWSAPTR)
*   MCAIO=ON;                       /* INDICATE IO RTN               */
         OI    MCAIO(AWSAPTR),B'00100000'                          0229
*   IOPSW=FLCIOPSW;                 /* GATHER PSW                    */
         MVC   IOPSW(8,AWSAPTR),FLCIOPSW                           0230
*   IODEV=EVREG6(3:4);              /* GATHER IO DEVICE              */
         L     R12,LCCAPTR                                         0231
         MVC   IODEV(2,AWSAPTR),EVREG6+2(R12)                      0231
*   IOCSW=FLCCSW;                   /* GATHER CSW                    */
         MVC   IOCSW(8,AWSAPTR),FLCCSW                             0232
*   RECLEN=LENGTH(IOREC);                                          0233
         LA    RECLEN,60                                           0233
*   IF EVREG2=0                     /* IOSB VALID ?                  */
*     THEN                                                         0234
         L     R12,EVREG2(,R12)                                    0234
         LTR   R12,R12                                             0234
         BNZ   @RF00234                                            0234
*     DO;                           /* PUT U/A IN IOSB DEP FLDS      */
*       IOASCB=UNAVAL;                                             0236
         MVC   IOASCB(4,AWSAPTR),UNAVAL                            0236
*       IOTCB=UNAVAL;                                              0237
         MVC   IOTCB(4,AWSAPTR),UNAVAL                             0237
*       IOJOBN=UNAVAL;                                             0238
         MVC   IOJOBN(8,AWSAPTR),UNAVAL                            0238
*       IOUA1=UNAVAL;                                              0239
         MVC   IOUA1(10,AWSAPTR),UNAVAL                            0239
*       IOUA2=UNAVAL(9:16);                                        0240
         MVC   IOUA2(8,AWSAPTR),UNAVAL+8                           0240
*       GOTO RCOV2;                                                0241
         B     RCOV2                                               0241
*     END;                                                         0242
*   MCAMASCB=ON;                    /* TURN ON DBUG BIT              */
@RF00234 OI    MCAMASCB(AWSAPTR),B'00100000'                       0243
*   RFY                                                            0244
*     R8 RSTD;                                                     0244
*   R8=ASVTENTY(IOSASID);           /* GET ASCB PTR                  */
         L     R12,LCCAPTR                                         0245
         L     R12,IOSBPTR(,R12)                                   0245
         LH    R12,IOSASID(,R12)                                   0245
         N     R12,@CF03270                                        0245
         SLA   R12,2                                               0245
         L     R15,CVTPTR                                          0245
         L     R15,ASVTPTR(,R15)                                   0245
         L     R8,ASVTENTY-4(R12,R15)                              0245
*   IF OPTTRC=ON                    /* TRC?                          */
*     THEN                                                         0246
         TM    OPTTRC(PCTPTR),B'00001000'                          0246
         BO    @RT00246                                            0246
*     GOTO AHLASCB1;                /* YES, TRACE GTF                */
*   IF R8=GTFASCB                   /* NO; IS THIS GTF?              */
*     THEN                                                         0248
         C     R8,GTFASCB(,PCTPTR)                                 0248
         BE    @RT00248                                            0248
*     GO TO AHLPSCM2;               /* YES, DONT TRACE               */
*AHLASCB1:                                                         0250
*   IOASCB=R8;                      /* GATHER ASCB ADDR              */
*                                                                  0250
AHLASCB1 ST    R8,IOASCB(,AWSAPTR)                                 0250
*   /*****************************************************************/
*   /*                                                               */
*   /* GET JOBNAME, IF AVAILABLE                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0251
*   RFY                                                            0251
*     R6 RSTD;                                                     0251
*   RFY                                                            0252
*     ASCB BASED(R8);                                              0252
*   MCAMJOBN=ON;                    /* DEBUG FLAG                    */
         OI    MCAMJOBN(AWSAPTR),B'00000001'                       0253
*   R6=ASCBJBNI;                    /* 1ST JOBNAME PTR               */
         L     R6,ASCBJBNI-ASCB(,R8)                               0254
*   IF R6=0 THEN                                                   0255
         SLR   R12,R12                                             0255
         CR    R6,R12                                              0255
         BNE   @RF00255                                            0255
*     DO;                           /* TRY NEXT PTR IF 1ST IS 0      */
*       R6=ASCBJBNS;                /* 2ND JOBNAME PTR               */
         L     R6,ASCBJBNS-ASCB(,R8)                               0257
*       IF R6=0 THEN                                               0258
         CR    R6,R12                                              0258
         BE    @RT00258                                            0258
*         GO TO NOJBNM1;            /* NO PTR AVAILABLE              */
*     END;                                                         0260
*   IF JBNMTEST^=0 THEN             /* THERE IS A JOBNAME            */
@RF00255 L     R12,JBNMTEST(,R6)                                   0261
         LTR   R12,R12                                             0261
         BZ    @RF00261                                            0261
*     IOJOBN=JOBNAME;               /* TRACE JOBNAME                 */
         MVC   IOJOBN(8,AWSAPTR),JOBNAME(R6)                       0262
*   ELSE                                                           0263
*NOJBNM1:                                                          0263
*     IOJOBN=NOTAPP;                /* N/A IN JOBNAME                */
         B     @RC00261                                            0263
@RF00261 DS    0H                                                  0263
NOJBNM1  MVC   IOJOBN(8,AWSAPTR),@CC03000                          0263
*   RFY                                                            0264
*    (R8,                                                          0264
*     R6) UNRSTD;                                                  0264
@RC00261 DS    0H                                                  0265
*   RFY                                                            0265
*     ASCB BASED(ASCBPTR);                                         0265
*RCOV1:                                                            0266
*   MCAMIOSB=ON;                    /* TURN ON IOSB BIT              */
RCOV1    OI    MCAMIOSB(AWSAPTR),B'00001000'                       0266
*   IOCPA=IOSCPA;                   /* REAL AND VIRT CCW ADDRS       */
         L     R12,LCCAPTR                                         0267
         L     R12,IOSBPTR(,R12)                                   0267
         MVC   IOCPA(8,AWSAPTR),IOSCPA(R12)                        0267
*   IOFLA=IOSFLA;                   /* FLAGS                         */
         MVC   IOFLA(1,AWSAPTR),IOSFLA(R12)                        0268
*   IOOPT=IOSOPT;                   /* FLAGS                         */
         MVC   IOOPT(1,AWSAPTR),IOSOPT(R12)                        0269
*   IOFMSK=IOSFMSK;                 /* FLAGS                         */
         MVC   IOFMSK(1,AWSAPTR),IOSFMSK(R12)                      0270
*   IODVRID=IOSDVRID;               /* DRIVER ID                     */
         MVC   IODVRID(1,AWSAPTR),IOSDVRID(R12)                    0271
*   IODSID=IOSDSID;                 /* DATA SET ID                   */
         MVC   IODSID(4,AWSAPTR),IOSDSID(R12)                      0272
*   IF IOSERP=0 THEN                /* NO SENSE AVAILABLE            */
         L     R12,IOSERP(,R12)                                    0273
         LTR   R12,R12                                             0273
         BNZ   @RF00273                                            0273
*     IOSENSE=NOTAPP;               /* N/A IN SENSE                  */
         MVC   IOSENSE(2,AWSAPTR),@CC03000                         0274
*   ELSE                                                           0275
*     IOSENSE=EWASNS;               /* GATHER SENSE                  */
         B     @RC00273                                            0275
@RF00273 L     R12,LCCAPTR                                         0275
         L     R12,IOSBPTR(,R12)                                   0275
         L     R12,EWAPTR(,R12)                                    0275
         MVC   IOSENSE(2,AWSAPTR),EWASNS(R12)                      0275
*   MCAMTCB=ON;                     /* TURN ON DBUG BIT              */
@RC00273 OI    MCAMTCB(AWSAPTR),B'00010000'                        0276
*   IF SRBPTCB=0 THEN               /* NO TCB AVAILABLE              */
         L     R12,LCCAPTR                                         0277
         L     R12,IOSBPTR(,R12)                                   0277
         L     R12,SRBPTR(,R12)                                    0277
         L     R12,SRBPTCB-SRBSECT(,R12)                           0277
         LTR   R12,R12                                             0277
         BNZ   @RF00277                                            0277
*     IOTCB=NOTAPP;                 /* N/A IN TCB                    */
         MVC   IOTCB(4,AWSAPTR),@CC03000                           0278
*   ELSE                                                           0279
*     IOTCB=SRBPTCB;                /* TCB FOR IO INT                */
         B     @RC00277                                            0279
@RF00277 L     R12,LCCAPTR                                         0279
         L     R12,IOSBPTR(,R12)                                   0279
         L     R12,SRBPTR(,R12)                                    0279
         MVC   IOTCB(4,AWSAPTR),SRBPTCB-SRBSECT(R12)               0279
*RCOV2:                             /* GATHER UCB RELATED FIELDS     */
*   MCAMUCB=ON;                     /* DEBUG FLAG                    */
@RC00277 DS    0H                                                  0280
RCOV2    OI    MCAMUCB(AWSAPTR),B'01000000'                        0280
*   GENERATE;                                                      0281
*                                                              @ZM33343
         L     R12,LCCAPTR                                     @ZM33343
         LA    R8,512                                          @ZM33343
         AL    R8,EVREG7(,R12)                                 @ZM33343
         USING UCBOB,R8
         MVC   IOSFLS(2,AWSAPTR),UCBSFLS                       @ZM33343
         MVC   IOCPU(1,AWSAPTR),UCBCPU                         @ZM33343
         MVC   IOCHAN(2,AWSAPTR),UCBCHAN                       @ZM33343
         DROP  R8
*   GOTO AHLPSTCM;                  /* EXIT                          */
         B     AHLPSTCM                                            0282
*   GEN(EJECT);                                                    0283
*                                                                  0283
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* GATHER DATA AND BUILD UIO TRACE RECORD                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0284
*AHLTUIO:                                                          0284
*   ;                               /* UIO MIN GATHER RTN            */
AHLTUIO  DS    0H                                                  0285
*   GENERATE;                                                      0285
         USING *,R15
         L     R11,BASEADDR
         DROP  R15
*   R10=R14;                        /* SAVE RETURN ADDRESS           */
         LR    R10,R14                                             0286
*   RTCAREG=ADDR(FRRUIO);           /* ERROR ROUTINE ADDR            */
         LA    RTCAREG,FRRUIO                                      0287
*   CALL AHLPRECM;                  /* COMMON INITIALIZER RTN        */
         BAL   R14,AHLPRECM                                        0288
*   UIOCSID=PSACSID;                /* CURRENT CHANNEL SET ID        */
         LH    R12,PSACSID
         STC   R12,UIOCSID(,AWSAPTR)
*   MCAUIO=ON;                      /* INDICATE UIO RTN IN CONTROL   */
         OI    MCAUIO(AWSAPTR),B'00001000'                         0289
*   UIOASCB=NOTAPP;                 /* N/A FOR ASCB                  */
         MVC   UIOASCB(4,AWSAPTR),@CC03000                         0290
*   UIOJOBN=NOTAPP;                 /* N/A FOR JOBNAME               */
         MVC   UIOJOBN(8,AWSAPTR),@CC03000                         0291
*   UIOPSW=FLCIOPSW;                /* GATHER PSW                    */
         MVC   UIOPSW(8,AWSAPTR),FLCIOPSW                          0292
*   UIOCSW=FLCCSW;                  /* GATHER CSW                    */
         MVC   UIOCSW(8,AWSAPTR),FLCCSW                            0293
*   UIODEV=EVREG6(3:4);             /* GATHER DEV. ADDR              */
         L     R12,LCCAPTR                                         0294
         MVC   UIODEV(2,AWSAPTR),EVREG6+2(R12)                     0294
*   RECLEN=LENGTH(UIOREC);                                         0295
         LA    RECLEN,33                                           0295
*   GOTO AHLPSTCM;                  /* GOTO EXIT RTN                 */
         B     AHLPSTCM                                            0296
*   GEN(EJECT);                                                    0297
*                                                                  0297
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* GATHER DATA AND BUILD SIO TRACE RECORD                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0298
*AHLTSIO1:                                                         0298
*   ;                                                              0298
AHLTSIO1 DS    0H                                                  0299
*   GENERATE;                                                      0299
         USING *,R15
         L     R11,BASEADDR
         DROP  R15
*   R10=R14;                        /* SAVE RTN ADDR                 */
         LR    R10,R14                                             0300
*   RTCAREG=ADDR(FRRSIO);           /* ERROR ROUTINE ADDR            */
         LA    RTCAREG,FRRSIO                                      0301
*   CALL AHLPRECM;                  /* COMMON INITIALIZER            */
         BAL   R14,AHLPRECM                                        0302
*   SIOCSID=PSACSID;                /* CURRENT CHANNEL SET ID        */
         LH    R12,PSACSID
         STC   R12,SIOCSID(,AWSAPTR)
*   MCASIO=ON;                      /* SIO RTN IN CNTR.              */
         OI    MCASIO(AWSAPTR),B'00010000'                         0303
*   SIOCAW=FLCCAW;                  /* GATHER CAW                    */
         MVC   SIOCAW(4,AWSAPTR),FLCCAW                            0304
*   SIOSTS=FLCCSW(5:6);             /* GATHER CSW                    */
         MVC   SIOSTS(2,AWSAPTR),FLCCSW+4                          0305
*   SIODEV=EVREG6(3:4);             /* GATHER DEV ADDR               */
         L     R12,LCCAPTR                                         0306
         MVC   SIODEV(2,AWSAPTR),EVREG6+2(R12)                     0306
*   RECLEN=LENGTH(SIOREC);                                         0307
         LA    RECLEN,50                                           0307
*   IF EVREG2=0                     /* IOSB BAD ?                    */
*     THEN                                                         0308
         L     R12,EVREG2(,R12)                                    0308
         LTR   R12,R12                                             0308
         BNZ   @RF00308                                            0308
*     DO;                           /* FLAG ALL FIELDS U/A           */
*       SIOASCB=UNAVAL;                                            0310
         MVC   SIOASCB(4,AWSAPTR),UNAVAL                           0310
*       SIOJOBN=UNAVAL;                                            0311
         MVC   SIOJOBN(8,AWSAPTR),UNAVAL                           0311
*       SIOUA1=UNAVAL(13:25);                                      0312
         MVC   SIOUA1(13,AWSAPTR),UNAVAL+12                        0312
*       SIOUA2=UNAVAL(13:26);                                      0313
         MVC   SIOUA2(14,AWSAPTR),UNAVAL+12                        0313
*       SIOCC='U';                  /* NO CC , FLAG WITH U           */
         MVI   SIOCC(AWSAPTR),C'U'                                 0314
*       GOTO AHLPSTCM;              /* GOTO EXIT RTN                 */
         B     AHLPSTCM                                            0315
*     END;                                                         0316
*   MCAMASCB=ON;                    /* TURN ON DBUG BIT              */
@RF00308 OI    MCAMASCB(AWSAPTR),B'00100000'                       0317
*   RFY                                                            0318
*     R8 RSTD;                                                     0318
*   R8=ASVTENTY(IOSASID);           /* GET ASCB PTR                  */
         L     R12,LCCAPTR                                         0319
         L     R12,IOSBPTR(,R12)                                   0319
         LH    R12,IOSASID(,R12)                                   0319
         N     R12,@CF03270                                        0319
         SLA   R12,2                                               0319
         L     R15,CVTPTR                                          0319
         L     R15,ASVTPTR(,R15)                                   0319
         L     R8,ASVTENTY-4(R12,R15)                              0319
*   IF OPTTRC=ON                    /* TRC?                          */
*     THEN                                                         0320
         TM    OPTTRC(PCTPTR),B'00001000'                          0320
         BO    @RT00320                                            0320
*     GOTO AHLASCB2;                /* YES, TRACE GTF                */
*   IF R8=GTFASCB                   /* NO; IS THIS GTF?              */
*     THEN                                                         0322
         C     R8,GTFASCB(,PCTPTR)                                 0322
         BE    @RT00322                                            0322
*     GO TO AHLPSCM2;               /* YES, DONT TRACE               */
*AHLASCB2:                                                         0324
*   SIOASCB=R8;                     /* GATHER ASCB ADDR              */
*                                                                  0324
AHLASCB2 ST    R8,SIOASCB(,AWSAPTR)                                0324
*   /*****************************************************************/
*   /*                                                               */
*   /* GET JOBNAME, IF AVAILABLE                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0325
*   RFY                                                            0325
*     R6 RSTD;                                                     0325
*   RFY                                                            0326
*     ASCB BASED(R8);                                              0326
*   MCAMJOBN=ON;                    /* DEBUG FLAG                    */
         OI    MCAMJOBN(AWSAPTR),B'00000001'                       0327
*   R6=ASCBJBNI;                    /* 1ST JOBNAME PTR               */
         L     R6,ASCBJBNI-ASCB(,R8)                               0328
*   IF R6=0 THEN                                                   0329
         SLR   R12,R12                                             0329
         CR    R6,R12                                              0329
         BNE   @RF00329                                            0329
*     DO;                           /* TRY NEXT PTR IF 1ST IS 0      */
*       R6=ASCBJBNS;                /* 2ND JOBNAME PTR               */
         L     R6,ASCBJBNS-ASCB(,R8)                               0331
*       IF R6=0 THEN                                               0332
         CR    R6,R12                                              0332
         BE    @RT00332                                            0332
*         GO TO NOJBNM2;            /* NO PTR AVAILABLE              */
*     END;                                                         0334
*   IF JBNMTEST^=0 THEN             /* THERE IS A JOBNAME            */
@RF00329 L     R12,JBNMTEST(,R6)                                   0335
         LTR   R12,R12                                             0335
         BZ    @RF00335                                            0335
*     SIOJOBN=JOBNAME;              /* TRACE JOBNAME                 */
         MVC   SIOJOBN(8,AWSAPTR),JOBNAME(R6)                      0336
*   ELSE                                                           0337
*NOJBNM2:                                                          0337
*     SIOJOBN=NOTAPP;               /* N/A IN JOBNAME                */
         B     @RC00335                                            0337
@RF00335 DS    0H                                                  0337
NOJBNM2  MVC   SIOJOBN(8,AWSAPTR),@CC03000                         0337
*   RFY                                                            0338
*    (R8,                                                          0338
*     R6) UNRSTD;                                                  0338
@RC00335 DS    0H                                                  0339
*   RFY                                                            0339
*     ASCB BASED(ASCBPTR);                                         0339
*AHLIOSB:                                                          0340
*   MCAMIOSB=ON;                    /* IOSB BASED PROCESSING         */
AHLIOSB  OI    MCAMIOSB(AWSAPTR),B'00001000'                       0340
*   SIOPATH=IOSPATH;                /* FLAGS                         */
         L     R12,LCCAPTR                                         0341
         L     R12,IOSBPTR(,R12)                                   0341
         MVC   SIOPATH(2,AWSAPTR),IOSPATH(R12)                     0341
*   SIOOPT=IOSOPT;                  /* FLAGS                         */
         MVC   SIOOPT(1,AWSAPTR),IOSOPT(R12)                       0342
*   SIOFMSK=IOSFMSK;                /* FLAGS                         */
         MVC   SIOFMSK(1,AWSAPTR),IOSFMSK(R12)                     0343
*   SIODVRID=IOSDVRID;              /* DRIVER ID                     */
         MVC   SIODVRID(1,AWSAPTR),IOSDVRID(R12)                   0344
*   SIODSID=IOSDSID;                /* DATA SET ID                   */
         MVC   SIODSID(4,AWSAPTR),IOSDSID(R12)                     0345
*   SIOSEEK=IOSEEKA;                /* GATHER SEEK ADDR              */
         MVC   SIOSEEK(8,AWSAPTR),IOSEEKA(R12)                     0346
*   SIOAFF=IOSAFF;                  /* GATHER AFF                    */
         MVC   SIOAFF(1,AWSAPTR),IOSAFF(R12)                       0347
*   SIOCC=IOSCC;                    /* GATHER CC                     */
         MVC   SIOCC(1,AWSAPTR),IOSCC(R12)                         0348
*   SIOCPA=IOSCPA;                  /* REAL AND VIRT CCW ADDR VST    */
         MVC   SIOCPA(8,AWSAPTR),IOSCPA(R12)                       0349
************   START OF CODE BLOCK ADDED BY ZP60011   *****************
         SLR   R6,R6                     INITIALISE CCW COUNT         *
         LA    R15,SIOCCWS(,AWSAPTR)     POINT TO CCW COLLECTION AREA *
         L     R0,IOSCPA(,R12)           GET CHANNEL PROGRAM REAL ADDR*
         SRL   R0,12                     GET REAL PAGE NUMBER         *
         L     R8,IOSVST(,R12)           POINT TO CHANNEL PROGRAM     *
CCWLOOP  LA    R6,1(,R6)                 INCREMENT CCW COUNT          *
         STH   R6,SIOCCW#(,AWSAPTR)      SAVE CCW COUNT               *
         MVC   0(8,R15),0(R8)            COPY CCW                     *
         CLI   SIOCCW#+1(AWSAPTR),25     REACHED MAXIMUM CCW COUNT?   *
         BNL   DONECCWS                  YES, STOP CCW COLLECTION     *
         TM    4(R15),X'C0'              ANY CCW CHAINING?            *
         BZ    TICCHECK                  NO, GO CHECK FOR T-I-C       *
         TM    0(R15),X'07'              TRANSFER IN CHANNEL?         *
         BZ    TICCHECK                  YES, NOT NEXT DOUBLEWORD     *
         LA    R8,8(,R8)                 POINT TO NEXT POSSIBLE CCW   *
CCWNEXT  LA    R15,8(,R15)               POINT TO NEXT COLLECTION SLOT*
         B     CCWLOOP                   GO COLLECT NEXT CCW          *
TICCHECK CLI   0(R15),X'08'              TRANSFER IN CHANNEL?         *
         BNE   DONECCWS                  NO, NO MORE CCWS             *
         SLR   R8,R8                     CLEAR FOR INSERT             *
         ICM   R8,7,1(R15)               GET NEXT CCW REAL ADDRESS    *
         LR    R0,R8                     COPY IT                      *
         SRL   R0,12                     GET REAL PAGE NUMBER         *
         SLL   R0,4                      GET REAL PAGE NUMBER TIMES 16*
         L     R14,CVTPTR                POINT TO THE CVT             *
         L     R14,CVTPVTP-CVT(,R14)     POINT TO THE PVT             *
         CLM   R0,3,PVTFPFN-PVT(R14)     CHANNEL PROGRAM IN NUCLEUS?  *
         BL    CCWNEXT                   YES, V=R, BUT NOT LIKELY     *
         L     R14,PVTPFTP-PVT(,R14)     NO, POINT TO PFT "ORIGIN"    *
         AR    R14,R0                    POINT TO PFTE OF CCW         *
         SLR   R0,R0                     CLEAR FOR INSERT             *
         ICM   R0,6,PFTVBN-PFTE(R14)     POINT TO VIRTUAL PAGE        *
         SLL   R8,20                     SHIFT OUT REAL PAGE NUMBER   *
         SRL   R8,20                     GET OFFSET INTO PAGE         *
         OR    R8,R0                     GET CCW VIRTUAL ADDRESS      *
         B     CCWNEXT                   GO PROCESS NEXT CCW          *
DONECCWS LR    R0,R6                     COPY CCW COUNT               *
         SLL   R6,3                      GET COLLECTED CCW BYTE COUNT *
         LA    RECLEN,2(R6,RECLEN)       GET FINAL RECORD LENGTH      *
         L     R15,IOSUCB(,R12)          POINT TO THE UCB             *
         USING UCBOB,R15                                              *
         CLI   UCBTBYT3,UCB3DACC         DASD DEVICE CLASS?           *
         BE    AHLPSTCM                  YES, DO NOT OVERLAY SEEK ADDR*
         DROP  R15                       UCBOB                        *
         LA    R15,SIOCCWS(,AWSAPTR)     POINT TO FIRST CAPTURED CCW  *
         L     R6,IOSCPA(,R12)           GET ITS REAL ADDRESS         *
         SRL   R6,12                     GET THE REAL PAGE NUMBER     *
WRTCCWLP TM    0(R15),X'03'              CONTROL/SENSE/TIC/BAD CCW?   *
         BM    ISWRTCCW                  NO, GO SEE IF IT IS WRITE    *
         LA    R15,8(,R15)               YES, POINT TO NEXT CCW       *
         BCT   R0,WRTCCWLP               GO CHECK IT                  *
         B     AHLPSTCM                  ALL CCWS WERE CONTROL        *
WRTDATLD MVC   SIOSEEK(0,AWSAPTR),0(R14) <<< EXECUTED >>>             *
ISWRTCCW TM    0(R15),X'01'              WRITE CCW?                   *
         BNO   AHLPSTCM                  NO, CAN'T HAVE WRITE TEXT    *
         L     R14,0(,R15)               YES, GET REAL TEXT ADDRESS   *
         LA    R14,0(,R14)               CLEAR COMMAND CODE           *
         LR    R0,R14                    COPY THE REAL ADDRESS        *
         SRL   R0,12                     GET REAL PAGE NUMBER         *
         SLL   R0,4                      GET REAL PAGE NUMBER TIMES 16*
         L     R6,CVTPTR                 POINT TO THE CVT             *
         L     R6,CVTPVTP-CVT(,R6)       POINT TO THE PVT             *
         CLM   R0,3,PVTFPFN-PVT(R6)      CHANNEL PROGRAM IN NUCLEUS?  *
         BL    SHOWTEXT                  YES, V=R, BUT NOT LIKELY     *
         L     R6,PVTPFTP-PVT(,R6)       NO, POINT TO PFT "ORIGIN"    *
         AR    R6,R0                     POINT TO PFTE OF WRITE TEXT  *
         SLR   R0,R0                     CLEAR FOR INSERT             *
         ICM   R0,6,PFTVBN-PFTE(R6)      POINT TO VIRTUAL PAGE        *
         SLL   R14,20                    SHIFT OUT REAL PAGE NUMBER   *
         SRL   R14,20                    GET OFFSET INTO PAGE         *
         OR    R14,R0                    GET TEXT VIRTUAL ADDRESS     *
SHOWTEXT LA    R0,8                      GET MAXIMUM COUNT TO COPY    *
         SLR   R6,R6                     CLEAR FOR INSERT             *
         ICM   R6,3,6(R15)               GET BYTE COUNT OF WRITE CCW  *
         CR    R6,R0                     DATA TOO LONG TO CAPTURE?    *
         BNH   *+6                       NO                           *
         LR    R6,R0                     YES, REDUCE TO MAXIMUM       *
         BCTR  R6,0                      DECREMENT FOR EXECUTE        *
         EX    R6,WRTDATLD               LOAD SOME WRITE DATA         *
         OI    SIOCCW#(AWSAPTR),X'80'    FLAG WRITE DATA CAPTURED     *
************    END OF CODE BLOCK ADDED BY ZP60011    *****************
*   GOTO AHLPSTCM;                  /* EXIT                          */
         B     AHLPSTCM                                            0350
*   GEN(EJECT);                                                    0351
*                                                                  0351
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* AHLTSIO ERROR ROUTINE                                         */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0352
*AHLERPRE:                                                         0352
*   PROC OPTIONS(DONTSAVE,NOSAVEAREA);                             0352
         B     @PB00003                                            0352
AHLERPRE DS    0H                                                  0353
*   RFY                                                            0353
*     R0 RSTD;                                                     0353
*   RFY                                                            0354
*     MCQE BASED(R0);                                              0354
*   RFY                                                            0355
*     MCRWSA BASED(WSACGTF);                                       0355
*   AWSAPTR=MCRMCA;                 /* GET ADDR OF MCAWSA AT ERROR 0356
*                                      FROM MCRWSA                   */
         L     R12,LCCAPTR                                         0356
         L     R12,LCCACPUS(,R12)                                  0356
         L     R12,WSACGTF(,R12)                                   0356
         L     AWSAPTR,MCRMCA(,R12)                                0356
*   RFY                                                            0357
*     MCRWSA BASED(MCAMCR);                                        0357
*   MCAERROR=ON;                    /* FLAG ERROR RECOVERY IN CONTROL*/
         OI    MCAERROR(AWSAPTR),B'00000100'                       0358
*   R0=MCRREG0;                     /* MCQE AT TIME OF ERROR         */
         L     R12,MCAMCR(,AWSAPTR)                                0359
         L     R0,MCRREG0(,R12)                                    0359
*   PCTPTR=MCQEAT;                  /* PCT AT TIME OF ERROR          */
         LR    R12,R0                                              0360
         L     PCTPTR,MCQEAT(,R12)                                 0360
*   DO;                             /* SETRP RECPARM(RECID)          */
*     RESPECIFY                                                    0362
*      (R0,                                                        0362
*       R1,                                                        0362
*       R14,                                                       0362
*       R15) RSTD;                                                 0362
*     R1->SDWARCDE=0;               /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0363
*     R15=ADDR(RECID);              /* ACCESS RECORD PARAMETER LIST  */
         LA    R15,RECID                                           0364
*     R1->SDWARECP=R15->I256C(1:24);/* COPY RECORD PARAMETERS        */
         MVC   SDWARECP(24,R1),I256C(R15)                          0365
*     RESPECIFY                                                    0366
*      (R0,                                                        0366
*       R1,                                                        0366
*       R14,                                                       0366
*       R15) UNRSTD;                                               0366
*   END;                                                           0367
*   WTOLIST=WTONAMES;               /* INITIALIZE WTO LIST           */
         MVC   WTOLIST(16,AWSAPTR),WTONAMES                        0368
*   WTOFUNC(1)=TRAILSAV(1);         /* S OR BLANK                    */
         MVC   WTOFUNC(1,AWSAPTR),TRAILSAV(R9)                     0369
*   IF SDWAMCHK=ON THEN             /* A MACHINE CHECK OCCURRED      */
         TM    SDWAMCHK(R1),B'10000000'                            0370
         BNO   @RF00370                                            0370
*     IF SDWAINSF=ON&               /* INSTRUCTION FAILURE           */
*         SDWAREGU=OFF&             /* REGISTERS IN SDWA ARE VALID   */
*         SDWAPSWU=OFF THEN         /* PSW IS VALID TOO              */
         TM    SDWAINSF(R1),B'00000100'                            0371
         BNO   @RF00371                                            0371
         TM    SDWAREGU(R1),B'01100000'                            0371
         BNZ   @RF00371                                            0371
*       DO;                         /* RETRY MAY BE ATTEMPTED        */
*         DO;                       /* SETRP                       0373
*                                      RETADDR(MCHKRTY)RC(SDWARETY)  */
*           RESPECIFY                                              0374
*            (R0,                                                  0374
*             R1,                                                  0374
*             R14,                                                 0374
*             R15) RSTD;                                           0374
*           R1->SDWARCDE=SDWARETY;     /* STORE RC INTO SDWA         */
         MVI   SDWARCDE(R1),X'04'                                  0375
*           R1->SDWARTYA=ADDR(MCHKRTY);/* SAVE RETRY ADDRESS         */
         L     R12,SDWANXT1(,R1)                                   0376
         ST    R12,SDWARTYA(,R1)                                   0376
*           RESPECIFY                                              0377
*            (R0,                                                  0377
*             R1,                                                  0377
*             R14,                                                 0377
*             R15) UNRSTD;                                         0377
*         END;                                                     0378
*       END;                                                       0379
*     ELSE                                                         0380
*       DO;                         /* RETRY NOT POSSIBLE            */
         B     @RC00371                                            0380
@RF00371 DS    0H                                                  0381
*         SDWASAV=R1;               /* SAVE R1 ACR0SS CALLS          */
         ST    R1,SDWASAV(,AWSAPTR)                                0381
*         MCQESAV=R0;               /* SAVE R0 ACR0SS CALLS          */
         ST    R0,MCQESAV(,AWSAPTR)                                0382
*         R0=ADDR(DMPLIST);         /* PARM FOR DUMP                 */
         LA    R0,DMPLIST                                          0383
*         CALL DMPMOD;              /* TAKE A DUMP                   */
         L     R15,PCTDMPMD(,PCTPTR)                               0384
         BALR  R14,R15                                             0384
*         WTOUN=R15;                /* MOVE IN BLANKS OR UN          */
         STH   R15,WTOUN(,AWSAPTR)                                 0385
*         R1=ADDR(WTOLIST);         /* SET UP PARM ADDR              */
         LA    R1,WTOLIST(,AWSAPTR)                                0386
*         CALL AHLWTO;              /* ISSUE AHLWTO                  */
         L     R15,PCTWTOMD(,PCTPTR)                               0387
         BALR  R14,R15                                             0387
*         R0=MCQESAV;               /* RESTORE R0                    */
         L     R0,MCQESAV(,AWSAPTR)                                0388
*         R1=SDWASAV;               /* RESTORE SDWA ADDR             */
         L     R1,SDWASAV(,AWSAPTR)                                0389
*         DO;                       /* SETRP RC(SDWACWT)             */
*           RESPECIFY                                              0391
*            (R0,                                                  0391
*             R1,                                                  0391
*             R14,                                                 0391
*             R15) RSTD;                                           0391
*           R1->SDWARCDE=SDWACWT;   /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0392
*           RESPECIFY                                              0393
*            (R0,                                                  0393
*             R1,                                                  0393
*             R14,                                                 0393
*             R15) UNRSTD;                                         0393
*         END;                                                     0394
*       END;                                                       0395
*   ELSE                            /* CHECK IF THIS ROUTINE WAS   0396
*                                      PERCOLATED TO                 */
*     IF SDWAPERC=ON THEN           /* YES, PERCOLATED TO            */
         B     @RC00370                                            0396
@RF00370 TM    SDWAPERC(R1),B'00010000'                            0396
         BNO   @RF00396                                            0396
*       IF SDWAEAS=ON THEN          /* PRESYSALLDAUS FRR WAS SUCCESSFUL   */
         TM    SDWAEAS(R1),B'00001000'                             0397
         BNO   @RF00397                                            0397
*         DO;                       /* SO CONTINUE PERCOLATION       */
*           DO;                     /* SETRP RC(SDWACWT)             */
*             RESPECIFY                                            0400
*              (R0,                                                0400
*               R1,                                                0400
*               R14,                                               0400
*               R15) RSTD;                                         0400
*             R1->SDWARCDE=SDWACWT; /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0401
*             RESPECIFY                                            0402
*              (R0,                                                0402
*               R1,                                                0402
*               R14,                                               0402
*               R15) UNRSTD;                                       0402
*           END;                                                   0403
*         END;                                                     0404
*       ELSE                                                       0405
*         DO;                       /* PRESYSALLDAUS FRR HAD A PROBLEM    */
         B     @RC00397                                            0405
@RF00397 DS    0H                                                  0406
*           DO;                     /* SETRP RC(SDWACWT)             */
*             RESPECIFY                                            0407
*              (R0,                                                0407
*               R1,                                                0407
*               R14,                                               0407
*               R15) RSTD;                                         0407
*             R1->SDWARCDE=SDWACWT; /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0408
*             RESPECIFY                                            0409
*              (R0,                                                0409
*               R1,                                                0409
*               R14,                                               0409
*               R15) UNRSTD;                                       0409
*           END;                                                   0410
*           MCQESAV=R0;             /* SAVE R0                       */
         ST    R0,MCQESAV(,AWSAPTR)                                0411
*           R0=ADDR(DMPLIST);       /* PARM FOR DUMP                 */
         LA    R0,DMPLIST                                          0412
*           CALL DMPMOD;            /* TAKE A DUMP                   */
         L     R15,PCTDMPMD(,PCTPTR)                               0413
         BALR  R14,R15                                             0413
*           R0=MCQESAV;             /* RESTORE R0                    */
         L     R0,MCQESAV(,AWSAPTR)                                0414
*         END;                                                     0415
*     ELSE                                                         0416
*                                                                  0416
*       /*************************************************************/
*       /*                                                           */
*       /* CHECK FOR DEBUG MODE AND PROHIBIT RETRY FLAGS             */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0416
*       IF GTFDEBUG=ON              /* IMMEDIATE DUMP REQUESTED      */
*           SDWACLUP=ON THEN        /* NO RETRY ALLOWED              */
         B     @RC00396                                            0416
@RF00396 TM    GTFDEBUG(PCTPTR),B'01000000'                        0416
         BO    @RT00416                                            0416
         TM    SDWACLUP(R1),B'10000000'                            0416
         BNO   @RF00416                                            0416
@RT00416 DS    0H                                                  0417
*         DO;                                                      0417
*           SDWASAV=R1;             /* SAVE R1 ACR0SS CALLS          */
         ST    R1,SDWASAV(,AWSAPTR)                                0418
*           MCQESAV=R0;             /* SAVE R0 ACR0SS CALLS          */
         ST    R0,MCQESAV(,AWSAPTR)                                0419
*           R0=ADDR(DMPLIST);       /* PARM FOR DUMP                 */
         LA    R0,DMPLIST                                          0420
*           CALL DMPMOD;            /* TAKE A DUMP                   */
         L     R15,PCTDMPMD(,PCTPTR)                               0421
         BALR  R14,R15                                             0421
*           WTOUN=R15;              /* MOVE IN BLANKS OR UN          */
         STH   R15,WTOUN(,AWSAPTR)                                 0422
*           R1=ADDR(WTOLIST);       /* SET UP PARM ADDR              */
         LA    R1,WTOLIST(,AWSAPTR)                                0423
*           CALL AHLWTO;            /* ISSUE AHLWTO                  */
         L     R15,PCTWTOMD(,PCTPTR)                               0424
         BALR  R14,R15                                             0424
*           R0=MCQESAV;             /* RESTORE R0                    */
         L     R0,MCQESAV(,AWSAPTR)                                0425
*           R1=SDWASAV;             /* RESTORE SDWA ADDR             */
         L     R1,SDWASAV(,AWSAPTR)                                0426
*           DO;                     /* SETRP RC(SDWACWT)             */
*             RESPECIFY                                            0428
*              (R0,                                                0428
*               R1,                                                0428
*               R14,                                               0428
*               R15) RSTD;                                         0428
*             R1->SDWARCDE=SDWACWT; /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0429
*             RESPECIFY                                            0430
*              (R0,                                                0430
*               R1,                                                0430
*               R14,                                               0430
*               R15) UNRSTD;                                       0430
*           END;                    /* CONTINUE TERMINATION          */
*         END;                                                     0432
*       ELSE                                                       0433
*         RETURN;                   /* RETURN TO CALLER              */
*/*   RETURN TO RTM. SDWA IS SET UP FOR RETURN                       */
*                                                                  0434
*   R14=SVR14FRR;                   /* SET UP RETURN TO RTM          */
@RC00396 DS    0H                                                  0434
@RC00370 L     R14,SVR14FRR(,R9)                                   0434
*   BC(15,R14);                     /* RETURN                        */
         BCR   15,R14                                              0435
*   END AHLERPRE;                                                  0436
@EL00003 DS    0H                                                  0436
@EF00003 DS    0H                                                  0436
@ER00003 BR    R14                                                 0436
@PB00003 DS    0H                                                  0436
*                                                                  0437
*   /*****************************************************************/
*   /*                                                               */
*   /* COMMON FRR EXIT ROUTINE FOR NONRECOVERABLR FRRS               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0437
*FRRGEN:                                                           0437
*   ERRSLOT=CATERIND;               /* PLACE 'EEEE' IN RECORD        */
FRRGEN   MVC   ERRSLOT(2,AWSAPTR),@CB03013                         0437
*   SDWASAV=R1;                     /* SAVE SDWA PTR                 */
         ST    R1,SDWASAV(,AWSAPTR)                                0438
*   DO;                             /* AHLSTACK                    0439
*                                      DLENGTH(RECLEN)AID(255)FID(00)*/
*                                                                  0439
*     /***************************************************************/
*     /*                                                             */
*     /* THE FOLLOWING CODE IS GENERATED BY THE AHLSTACK MACRO. THIS */
*     /* CODE BUILDS THE GTF RECORD PREFIX AND THEN CALLS AHLSBLOK TO*/
*     /* STACK THE RECORD.                                           */
*     /*                                                             */
*     /***************************************************************/
*                                                                  0440
*     RFY                                                          0440
*      (AHLRECPR,                                                  0440
*       AHLSBREG,                                                  0440
*       AHLRETCD) RSTD;                                            0440
*     IF OPTTIME='1'B THEN          /* CHECK FOR TIMESTAMP REQUESTED */
         TM    OPTTIME(PCTPTR),B'00000001'                         0441
         BNO   @RF00441                                            0441
*       DO;                         /* YES, TIMESTAMP                */
*         AHLRECPR=ADDR(MCARECPR);  /* SETUP ADDRESSIBILITY TO PREFIX
*                                      FOR PREFIX WITH TIMESTAMP     */
         LA    AHLRECPR,MCARECPR(,AWSAPTR)                         0443
*         AHLGLGTH=RECLEN+16;       /* SET LENGTH TO INCLUDE ***   0444
*                                      DLENGTH *** PLUS TIMESTAMP  0444
*                                      PREFIX                        */
         LA    R12,16                                              0444
         ALR   R12,RECLEN                                          0444
         STH   R12,AHLGLGTH(,AHLRECPR)                             0444
*         GEN(STCK AHLGTST(AHLRECPR) PUT TOD CLOCK VALUE INTO PREFIX )
*         SETS(AHLGTST) NOEXIT SEQFLOW NOFLOWS;                    0445
         STCK AHLGTST(AHLRECPR) PUT TOD CLOCK VALUE INTO PREFIX
*         AHLGEIDL=MCAMCR->MCREID;  /* *** EID *** NOT SPECIFIED PUT
*                                      DEFAULT INTO PREFIX           */
         L     R12,MCAMCR(,AWSAPTR)                                0446
         LH    R12,MCREID(,R12)                                    0446
         N     R12,@CF03270                                        0446
         STH   R12,AHLGEIDL(,AHLRECPR)                             0446
*       END;                                                       0447
*     ELSE                          /* NO TIMESTAMP TO BE DONE, BUILD
*                                      SHORT PREFIX                  */
*       DO;                                                        0448
         B     @RC00441                                            0448
@RF00441 DS    0H                                                  0449
*         AHLRECPR=ADDR(MCARECPR)+8;/* SETUP ADDRESSIBILITY TO FOR 0449
*                                      SHORT PREFIX WITHOUT TIMESTAMP*/
         LA    R12,8                                               0449
         LA    AHLRECPR,MCARECPR(,AWSAPTR)                         0449
         ALR   AHLRECPR,R12                                        0449
*         AHLGLGTH=RECLEN+8;        /* PUT TOTAL RECORD LENGTH INTO
*                                      PREFIX *** DLENGTH *** PLUS 0450
*                                      PREFIX LENGTH                 */
         ALR   R12,RECLEN                                          0450
         STH   R12,AHLGLGTH(,AHLRECPR)                             0450
*         AHLGEIDS=MCAMCR->MCREID;  /* *** EID *** NOT SPECIFIED PUT
*                                      DEFAULT INTO PREFIX           */
         L     R12,MCAMCR(,AWSAPTR)                                0451
         LH    R12,MCREID(,R12)                                    0451
         N     R12,@CF03270                                        0451
         STH   R12,AHLGEIDS(,AHLRECPR)                             0451
*       END;                                                       0452
*     AHLGRES=0;                    /* ZERO OUT FOR QSAM           0453
*                                      COMPATIBILITY                 */
@RC00441 SLR   R12,R12                                             0453
         STH   R12,AHLGRES(,AHLRECPR)                              0453
*     AHLGAID=255;                  /* PUT *** AID *** INTO PREFIX   */
         MVI   AHLGAID(AHLRECPR),X'FF'                             0454
*     AHLGFID=00;                   /* PUT *** FID *** INTO PREFIX   */
         MVI   AHLGFID(AHLRECPR),X'00'                             0455
*     AHLRETCD=4;                   /* SET DEFAULT RETURN CODE       */
         LA    AHLRETCD,4                                          0456
*     AHLSBREG=SBLOKRTN;            /* GET A STABLE COPY OF AHLSBLOK
*                                      ADDR                          */
         L     AHLSBREG,SBLOKRTN(,PCTPTR)                          0457
*     IF AHLSBREG^=0 THEN           /* CHECK TO SEE IF AHLSBLOK IS 0458
*                                      AVAILABLE                     */
         CR    AHLSBREG,R12                                        0458
         BE    @RF00458                                            0458
*       CALL AHLSBLOK;              /* YES, SO CALL AHLSBLOK TO BLOCK
*                                      RECORD                        */
         LR    R15,AHLSBREG                                        0459
         BALR  R14,R15                                             0459
*     RFY                                                          0460
*      (AHLRECPR,                                                  0460
*       AHLSBREG,                                                  0460
*       AHLRETCD) UNRSTD;                                          0460
@RF00458 DS    0H                                                  0461
*   END;                                                           0461
*   R1=SDWASAV;                     /* RESTORE PTR                   */
         L     R1,SDWASAV(,AWSAPTR)                                0462
*   IF PCTCATF=0 THEN               /* ALL FUNCTION ARE DISABLED     */
         L     R12,PCTCATF(,PCTPTR)                                0463
         LTR   R12,R12                                             0463
         BNZ   @RF00463                                            0463
*     DO;                           /* SETRP RC(SDWACWT)             */
*       RESPECIFY                                                  0465
*        (R0,                                                      0465
*         R1,                                                      0465
*         R14,                                                     0465
*         R15) RSTD;                                               0465
*       R1->SDWARCDE=SDWACWT;       /* SAVE RC VALUE                 */
         MVI   SDWARCDE(R1),X'00'                                  0466
*       RESPECIFY                                                  0467
*        (R0,                                                      0467
*         R1,                                                      0467
*         R14,                                                     0467
*         R15) UNRSTD;                                             0467
*     END;                          /* TERMINATE                     */
*   ELSE                                                           0469
*     DO;                           /* STILL SOME FUNCTIONS RUNNING  */
         B     @RC00463                                            0469
@RF00463 DS    0H                                                  0470
*       DO;                         /* SETRP                       0470
*                                      RETADDR(SVR14RTN)RC(SDWARETY) */
*         RESPECIFY                                                0471
*          (R0,                                                    0471
*           R1,                                                    0471
*           R14,                                                   0471
*           R15) RSTD;                                             0471
*         R1->SDWARCDE=SDWARETY;    /* STORE RC INTO SDWA            */
         MVI   SDWARCDE(R1),X'04'                                  0472
*         R1->SDWARTYA=ADDR(SVR14RTN);/* SAVE RETRY ADDRESS          */
         L     R12,SVR14NRM(,R9)                                   0473
         ST    R12,SDWARTYA(,R1)                                   0473
*         RESPECIFY                                                0474
*          (R0,                                                    0474
*           R1,                                                    0474
*           R14,                                                   0474
*           R15) UNRSTD;                                           0474
*       END;                        /* RETRY TO MCIH                 */
*       R14=SVR14FRR;               /* GET RTM RETURN ADDR           */
         L     R14,SVR14FRR(,R9)                                   0476
*       GEN(SETFRR D,WRKREGS=(R9,R6));                             0477
         SETFRR D,WRKREGS=(R9,R6)
*       BC(15,R14);                 /* RETURN                        */
         BCR   15,R14                                              0478
*     END;                                                         0479
*   R14=SVR14FRR;                   /* GET RTM RETURN ADDR           */
@RC00463 L     R14,SVR14FRR(,R9)                                   0480
*   BC(15,R14);                     /* RETURN                        */
         BCR   15,R14                                              0481
*   GEN(EJECT);                                                    0482
*                                                                  0482
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* FRR FOR UIO ERRORS                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0483
*FRRUIO:                                                           0483
*   ;                                                              0483
FRRUIO   DS    0H                                                  0484
*   GENERATE;                                                      0484
         USING FRRUIO,R15
         L     R11,BASEADDR          PICK UP OLD BASE ADDR
         DROP  R15
*   RFY                                                            0485
*    (R1,                                                          0485
*     R14) RSTD;                                                   0485
*   RFY                                                            0486
*     SDWA BASED(R1);                                              0486
*   R9=SDWAPARM;                    /* PTR TO USER PARM AREA         */
         L     R9,SDWAPARM(,R1)                                    0487
*   SVR14FRR=R14;                   /* SAVE RETURN ADDR TO RTM       */
         ST    R14,SVR14FRR(,R9)                                   0488
*   RFY                                                            0489
*     R14 UNRSTD;                                                  0489
*   TRAILSAV(1)=' ';                /* INDICATE IO FUNCTION          */
         MVI   TRAILSAV(R9),C' '                                   0490
*   CALL AHLERPRE;                  /* CALL COMMON INITIALIZER       */
*                                                                  0491
         BAL   R14,AHLERPRE                                        0491
*   /*****************************************************************/
*   /*                                                               */
*   /* DISABLE FUNCTION AND TAKE DUMP IF NECESSARY                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0492
*UIOCHK:                                                           0492
*   IF PCTIO=ON PCTIOF=ON THEN                                     0492
UIOCHK   TM    PCTIO(PCTPTR),B'00100000'                           0492
         BO    @RT00492                                            0492
         TM    PCTIOF(PCTPTR),B'00001000'                          0492
         BNO   @RF00492                                            0492
@RT00492 DS    0H                                                  0493
*     DO;                           /* FUNCTION NOT YET DISABLED     */
*       RFY                                                        0494
*        (R6,                                                      0494
*         R8FRRWRK) RSTD;                                          0494
*       R6=PCTCATF;                 /* GET CURRENT PICTURE OF FLAGS  */
         L     R6,PCTCATF(,PCTPTR)                                 0495
*       R8FRRWRK=R6&IOOFF;          /* FLAGS WITH THIS FUNCTION OFF  */
         LR    R8FRRWRK,R6                                         0496
         N     R8FRRWRK,@CF02994                                   0496
*       CS(R6,R8FRRWRK,PCTCATF);    /* SET FLAGS TO THIS FUNCTION OFF*/
         CS    R6,R8,PCTCATF(PCTPTR)                               0497
*       BC(4,UIOCHK);               /* SOMEONE ELSE BEAT US TO FLAGS.
*                                      CHECK AGAIN                   */
         BC    4,UIOCHK                                            0498
*       SDWASAV=R1;                 /* SAVE R1 ACR0SS CALLS          */
         ST    R1,SDWASAV(,AWSAPTR)                                0499
*       MCQESAV=R0;                 /* SAVE R0 ACR0SS CALLS          */
         ST    R0,MCQESAV(,AWSAPTR)                                0500
*       CALL DMPMOD;                /* TAKE A DUMP                   */
         L     R15,PCTDMPMD(,PCTPTR)                               0501
         BALR  R14,R15                                             0501
*       WTOUN=R15;                  /* MOVE IN BLANKS OR UN          */
         STH   R15,WTOUN(,AWSAPTR)                                 0502
*       R1=ADDR(WTOLIST);           /* SET UP PARM ADDR              */
         LA    R1,WTOLIST(,AWSAPTR)                                0503
*       CALL AHLWTO;                /* ISSUE AHLWTO                  */
         L     R15,PCTWTOMD(,PCTPTR)                               0504
         BALR  R14,R15                                             0504
*       R0=MCQESAV;                 /* RESTORE MCQE ADDR             */
*                                                                  0505
         L     R0,MCQESAV(,AWSAPTR)                                0505
*       /*************************************************************/
*       /*                                                           */
*       /* ISSUE SETEVENT TO DISABLE THIS FUNCTION                   */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0506
*       DO;                         /* SETEVENT NAME('GTF ')MCQE(MCQE
*                                      )ACTION(DISABLE)EIDAD(IOEIDS)M
*                                      F(E,MCAWTO,COMPLETE)EIDNO(4)  */
*                                                                  0506
*         /***********************************************************/
*         /*                                                         */
*         /* THE FOLLOWING SECTION OF CODE IS GENERATED BY THE       */
*         /* SETEVENT MACRO. THIS CODE WILL BE EITHER AN INITIALIZED */
*         /* PARAMETER LIST, OR IF IT IS THE RESULT OF STANDARD OR   */
*         /* EXECUTE FORM IT WILL FILL IN THE LIST, GENERATE ALL     */
*         /* LINKAGES AND CALL THE PROPER SERVICE, EITHER AHLSETEV OR*/
*         /* AHLSETD                                                 */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0507
*         DCL                                                      0507
*           AHLPPTR REG(1) PTR(31) RSTD;/* PARAMETER LIST ADDRESS    */
*         DCL                                                      0508
*           AHLSETEV ENTRY OPTIONS(EXIT,SEQFLOW) BASED(MCHSETE) VALRG(0
*               );                  /* ENTRY FOR ADD ACTIVAT,FREE    */
*         DCL                                                      0509
*           AHLSETD ENTRY OPTIONS(EXIT,SEQFLOW) BASED(MCHSETD) VALRG(0)
*             ;                     /* ENTRY FOR DISABLE AND CHANGE  */
*         DCL                                                      0510
*           1 SEPL BASED(AHLPPTR) BDY(WORD),/* DSECT PARAMETER LIST  */
*            2 SEFG PTR(8),         /* ACTION GLAGS                  */
*            2 * PTR(8),                                           0510
*            2 SEEN PTR(8),         /* EID COUNT                     */
*            2 SECN PTR(8),         /* CLASS COUNT                   */
*            2 SENM CHAR(8),        /* APPLICATION NMAE              */
*            2 SEEL PTR(31),        /* ADDRESS OF LIST OF EIDS       */
*            2 SECL PTR(31),        /* ADDRESS OF LIST OF CLASSES    */
*            2 SEMC PTR(31);        /* MCQE ADDRESS                  */
*         AHLPPTR=ADDR(MCAWTO);     /* SET UP PLIST PTR              */
         LA    AHLPPTR,MCAWTO(,AWSAPTR)                            0511
*         SEPL=SEPL&&SEPL;          /* INIT PLIST TO ZERO            */
         XC    SEPL(24,AHLPPTR),SEPL(AHLPPTR)                      0512
*         SEMC=ADDR(MCQE);          /* SET UP MCQE ADDRESS           */
         ST    R0,SEMC(,AHLPPTR)                                   0513
*         SENM='GTF     ';          /* MOVE NAME INTO PLIST          */
         MVC   SENM(8,AHLPPTR),@CC03243                            0514
*         SEFG=32;                  /* MOVE FLAGS INTO PLIST         */
         MVI   SEFG(AHLPPTR),X'20'                                 0515
*         SEEN=4;                   /* SET TO VALUE SPECIFIED        */
         MVI   SEEN(AHLPPTR),X'04'                                 0516
*         SEEL=ADDR(IOEIDS);        /* SET TO VALUE SPECIFIED        */
         LA    R12,IOEIDS                                          0517
         ST    R12,SEEL(,AHLPPTR)                                  0517
*         IF SEFG=32 SEFG=8 THEN    /* CHECK FOR DISABLE OR * CHANGE */
         CLI   SEFG(AHLPPTR),32                                    0518
         BE    @RT00518                                            0518
         CLI   SEFG(AHLPPTR),8                                     0518
         BNE   @RF00518                                            0518
@RT00518 DS    0H                                                  0519
*           CALL AHLSETD;           /* YES                           */
         L     R12,CVTPTR                                          0519
         L     R12,CVTGTF-CVT(,R12)                                0519
         L     R15,MCHSETD(,R12)                                   0519
         BALR  R14,R15                                             0519
*         ELSE                      /* NO                            */
*           CALL AHLSETEV;                                         0520
         B     @RC00518                                            0520
@RF00518 L     R12,CVTPTR                                          0520
         L     R12,CVTGTF-CVT(,R12)                                0520
         L     R15,MCHSETE(,R12)                                   0520
         BALR  R14,R15                                             0520
*       END;                                                       0521
@RC00518 DS    0H                                                  0522
*       R1=SDWASAV;                 /* RESTORE SDWA ADDR             */
         L     R1,SDWASAV(,AWSAPTR)                                0522
*     END;                                                         0523
*   IF MCAUIO=ON THEN               /* SET UP UIO RECORD LEN         */
@RF00492 TM    MCAUIO(AWSAPTR),B'00001000'                         0524
         BNO   @RF00524                                            0524
*     RECLEN=LENGTH(UIOREC);        /* LENGTH OF RECORD FOR STACK    */
         LA    RECLEN,33                                           0525
*   ELSE                            /* SET UP IO RECORD LEN          */
*     RECLEN=LENGTH(IOREC);         /* LENGTH OF RECORD FOR STACK    */
         B     @RC00524                                            0526
@RF00524 LA    RECLEN,60                                           0526
*   GO TO FRRGEN;                   /* COMMON EXIT ROUTINE           */
         B     FRRGEN                                              0527
*   GEN(EJECT);                                                    0528
*                                                                  0528
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* FRR FOR IO ERRORS                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0529
*FRRIO:                                                            0529
*   ;                                                              0529
FRRIO    DS    0H                                                  0530
*   GENERATE;                                                      0530
         USING FRRIO,R15
         L     R11,BASEADDR          PICK UP OLD BASE ADDR
         DROP  R15
*   RFY                                                            0531
*    (R1,                                                          0531
*     R14) RSTD;                                                   0531
*   RFY                                                            0532
*     SDWA BASED(R1);                                              0532
*   R9=SDWAPARM;                    /* PTR TO USER PARM AREA         */
         L     R9,SDWAPARM(,R1)                                    0533
*   SVR14FRR=R14;                   /* SAVE RETURN ADDR TO RTM       */
         ST    R14,SVR14FRR(,R9)                                   0534
*   RFY                                                            0535
*     R14 UNRSTD;                                                  0535
*   TRAILSAV=' ';                   /* FLAG IO FUNCTION              */
         MVI   TRAILSAV+1(R9),C' '                                 0536
         MVC   TRAILSAV+2(2,R9),TRAILSAV+1(R9)                     0536
         MVI   TRAILSAV(R9),C' '                                   0536
*   CALL AHLERPRE;                  /* CALL COMMON INITIALIZER       */
         BAL   R14,AHLERPRE                                        0537
*   IF MCAMUCB=ON THEN              /* ERROR GATHERING UCB           */
         TM    MCAMUCB(AWSAPTR),B'01000000'                        0538
         BNO   @RF00538                                            0538
*     DO;                                                          0539
*       IOUCBER=ERRIND;             /* **** IN UCB FIELD             */
         MVC   IOUCBER(5,AWSAPTR),@CC03010                         0540
*RETRYP:                                                           0541
*       DO;                         /* SETRP                       0541
*                                      RETADDR(AHLPSTCM)RC(SDWARETY) */
RETRYP   DS    0H                                                  0542
*         RESPECIFY                                                0542
*          (R0,                                                    0542
*           R1,                                                    0542
*           R14,                                                   0542
*           R15) RSTD;                                             0542
*         R1->SDWARCDE=SDWARETY;    /* STORE RC INTO SDWA            */
         MVI   SDWARCDE(R1),X'04'                                  0543
*         R1->SDWARTYA=ADDR(AHLPSTCM);/* SAVE RETRY ADDRESS          */
         LA    R12,AHLPSTCM                                        0544
         ST    R12,SDWARTYA(,R1)                                   0544
*         RESPECIFY                                                0545
*          (R0,                                                    0545
*           R1,                                                    0545
*           R14,                                                   0545
*           R15) UNRSTD;                                           0545
*       END;                        /* RETRY TO PSTCOM               */
*                                                                  0546
*       /*************************************************************/
*       /*                                                           */
*       /* RETURN TO RTM. SDWA IS SET UP FOR RETURN                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0547
*       R14=SVR14FRR;               /* SET UP RETURN TO RTM          */
         L     R14,SVR14FRR(,R9)                                   0547
*       BC(15,R14);                 /* RETURN                        */
         BCR   15,R14                                              0548
*     END;                                                         0549
*   IF MCAMTCB=ON THEN              /* ERROR GATHERING TCB           */
@RF00538 TM    MCAMTCB(AWSAPTR),B'00010000'                        0550
         BNO   @RF00550                                            0550
*     DO;                                                          0551
*       IOTCB=ERRIND;               /* **** IN TCB FIELD             */
         MVC   IOTCB(4,AWSAPTR),@CC03010                           0552
*RETRY2:                                                           0553
*       DO;                         /* SETRP                       0553
*                                      RETADDR(RCOV2)RC(SDWARETY)    */
RETRY2   DS    0H                                                  0554
*         RESPECIFY                                                0554
*          (R0,                                                    0554
*           R1,                                                    0554
*           R14,                                                   0554
*           R15) RSTD;                                             0554
*         R1->SDWARCDE=SDWARETY;    /* STORE RC INTO SDWA            */
         MVI   SDWARCDE(R1),X'04'                                  0555
*         R1->SDWARTYA=ADDR(RCOV2); /* SAVE RETRY ADDRESS            */
         LA    R12,RCOV2                                           0556
         ST    R12,SDWARTYA(,R1)                                   0556
*         RESPECIFY                                                0557
*          (R0,                                                    0557
*           R1,                                                    0557
*           R14,                                                   0557
*           R15) UNRSTD;                                           0557
*       END;                        /* RETRY TO RCOV2                */
*                                                                  0558
*       /*************************************************************/
*       /*                                                           */
*       /* RETURN TO RTM. SDWA IS SET UP FOR RETURN                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0559
*       R14=SVR14FRR;               /* SET UP RETURN TO RTM          */
         L     R14,SVR14FRR(,R9)                                   0559
*       BC(15,R14);                 /* RETURN                        */
         BCR   15,R14                                              0560
*     END;                                                         0561
*   IF MCAMIOSB=ON THEN             /* ERROR GATHERING IOSB          */
@RF00550 TM    MCAMIOSB(AWSAPTR),B'00001000'                       0562
         BNO   @RF00562                                            0562
*     DO;                                                          0563
*       IOTCB=ERRIND;               /* **** IN RECORD                */
         MVC   IOTCB(4,AWSAPTR),@CC03010                           0564
*       IOUA1=ERRIND;               /* **** IN RECORD                */
         MVC   IOUA1(10,AWSAPTR),@CC03010                          0565
*       IOUA2=ERRIND;               /* **** IN RECORD                */
         MVC   IOUA2(8,AWSAPTR),@CC03010                           0566
*       GO TO RETRY2;               /* RETRY TO RCOV2                */
         B     RETRY2                                              0567
*     END;                                                         0568
*   IF MCAMJOBN=ON THEN                                            0569
@RF00562 TM    MCAMJOBN(AWSAPTR),B'00000001'                       0569
         BO    @RT00569                                            0569
*     GO TO JOBERR1;                /* GO PUT *** IN JOBNAME         */
*   IF MCAMASCB=ON THEN             /* ERROR GATHERING ASCB          */
         TM    MCAMASCB(AWSAPTR),B'00100000'                       0571
         BNO   @RF00571                                            0571
*     DO;                                                          0572
*       IOASCB=ERRIND;              /* **** IN ASCB FIELD            */
         MVC   IOASCB(4,AWSAPTR),@CC03010                          0573
*JOBERR1:                                                          0574
*       IOJOBN=ERRIND;              /* **** IN JOBNAME               */
JOBERR1  MVC   IOJOBN(8,AWSAPTR),@CC03010                          0574
*       DO;                         /* SETRP                       0575
*                                      RETADDR(RCOV1)RC(SDWARETY)    */
*         RESPECIFY                                                0576
*          (R0,                                                    0576
*           R1,                                                    0576
*           R14,                                                   0576
*           R15) RSTD;                                             0576
*         R1->SDWARCDE=SDWARETY;    /* STORE RC INTO SDWA            */
         MVI   SDWARCDE(R1),X'04'                                  0577
*         R1->SDWARTYA=ADDR(RCOV1); /* SAVE RETRY ADDRESS            */
         LA    R12,RCOV1                                           0578
         ST    R12,SDWARTYA(,R1)                                   0578
*         RESPECIFY                                                0579
*          (R0,                                                    0579
*           R1,                                                    0579
*           R14,                                                   0579
*           R15) UNRSTD;                                           0579
*       END;                        /* RETRY TO RCOV1                */
*                                                                  0580
*       /*************************************************************/
*       /*                                                           */
*       /* RETURN TO RTM. SDWA IS SET UP FOR RETURN                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0581
*       R14=SVR14FRR;               /* SET UP RETURN TO RTM          */
         L     R14,SVR14FRR(,R9)                                   0581
*       BC(15,R14);                 /* RETURN                        */
         BCR   15,R14                                              0582
*     END;                                                         0583
*   GO TO UIOCHK;                   /* DISABLE FUNCTION AND TAKE DUMP
*                                      IF NECESSARY                  */
         B     UIOCHK                                              0584
*   GEN(EJECT);                                                    0585
*                                                                  0585
         EJECT
*   /*****************************************************************/
*   /*                                                               */
*   /* FRR FOR SIO ERRORS                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0586
*FRRSIO:                                                           0586
*   ;                                                              0586
FRRSIO   DS    0H                                                  0587
*   GENERATE;                                                      0587
         USING FRRSIO,R15
         L     R11,BASEADDR          PICK UP OLD BASE ADDR
         DROP  R15
*   RFY                                                            0588
*    (R1,                                                          0588
*     R14) RSTD;                                                   0588
*   RFY                                                            0589
*     SDWA BASED(R1);                                              0589
*   R9=SDWAPARM;                    /* PTR TO USER PARM AREA         */
         L     R9,SDWAPARM(,R1)                                    0590
*   SVR14FRR=R14;                   /* SAVE RETURN ADDR TO RTM       */
         ST    R14,SVR14FRR(,R9)                                   0591
*   RFY                                                            0592
*     R14 UNRSTD;                                                  0592
*   TRAILSAV(1)='S';                /* SET FUNCTION TO SIO           */
         MVI   TRAILSAV(R9),C'S'                                   0593
*   CALL AHLERPRE;                  /* CALL COMMON INITIALIZER       */
         BAL   R14,AHLERPRE                                        0594
*   IF MCAMIOSB=ON THEN             /* ERROR GATHERING IOSB          */
         TM    MCAMIOSB(AWSAPTR),B'00001000'                       0595
         BNO   @RF00595                                            0595
*     DO;                                                          0596
*       SIOUA1=ERRIND;              /* **** IN RECORD                */
         MVC   SIOUA1(13,AWSAPTR),@CC03010                         0597
*       SIOUA2=ERRIND;              /* **** IN RECORD                */
         MVC   SIOUA2(14,AWSAPTR),@CC03010                         0598
*       GO TO RETRYP;               /* RETRY TO PSTCOM               */
         B     RETRYP                                              0599
*     END;                                                         0600
*   IF MCAMJOBN=ON THEN                                            0601
@RF00595 TM    MCAMJOBN(AWSAPTR),B'00000001'                       0601
         BNO   @RF00601                                            0601
*     DO;                           /* GO PUT *** IN JOBNAME         */
*RETRYJ:                                                           0603
*       SIOJOBN=ERRIND;             /* **** IN JOBNAME               */
RETRYJ   MVC   SIOJOBN(8,AWSAPTR),@CC03010                         0603
*       DO;                         /* SETRP                       0604
*                                      RETADDR(AHLIOSB)RC(SDWARETY)  */
*         RESPECIFY                                                0605
*          (R0,                                                    0605
*           R1,                                                    0605
*           R14,                                                   0605
*           R15) RSTD;                                             0605
*         R1->SDWARCDE=SDWARETY;    /* STORE RC INTO SDWA            */
         MVI   SDWARCDE(R1),X'04'                                  0606
*         R1->SDWARTYA=ADDR(AHLIOSB);/* SAVE RETRY ADDRESS           */
         LA    R12,AHLIOSB                                         0607
         ST    R12,SDWARTYA(,R1)                                   0607
*         RESPECIFY                                                0608
*          (R0,                                                    0608
*           R1,                                                    0608
*           R14,                                                   0608
*           R15) UNRSTD;                                           0608
*       END;                        /* RETRY TO IOSB                 */
*                                                                  0609
*       /*************************************************************/
*       /*                                                           */
*       /* RETURN TO RTM. SDWA IS SET UP FOR RETURN                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0610
*       R14=SVR14FRR;               /* SET UP RETURN TO RTM          */
         L     R14,SVR14FRR(,R9)                                   0610
*       BC(15,R14);                 /* RETURN                        */
         BCR   15,R14                                              0611
*     END;                                                         0612
*   IF MCAMASCB=ON THEN             /* ERROR GATHERING ASCB          */
@RF00601 TM    MCAMASCB(AWSAPTR),B'00100000'                       0613
         BNO   @RF00613                                            0613
*     DO;                                                          0614
*       SIOASCB=ERRIND;             /* **** IN ASCB FIELD            */
         MVC   SIOASCB(4,AWSAPTR),@CC03010                         0615
*       GO TO RETRYJ;               /* RETRY                         */
         B     RETRYJ                                              0616
*     END;                                                         0617
*                                                                  0617
*   /*****************************************************************/
*   /*                                                               */
*   /* DISABLE FUNCTION AND TAKE DUMP IF NECESSARY                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0618
*SIOCHK:                                                           0618
*   IF PCTSIO=ON PCTSIOF=ON THEN                                   0618
@RF00613 DS    0H                                                  0618
SIOCHK   TM    PCTSIO(PCTPTR),B'00010000'                          0618
         BO    @RT00618                                            0618
         TM    PCTSIOF(PCTPTR),B'00000100'                         0618
         BNO   @RF00618                                            0618
@RT00618 DS    0H                                                  0619
*     DO;                           /* FUNCTION NOT DISABLED         */
*       RFY                                                        0620
*        (R6,                                                      0620
*         R8FRRWRK) RSTD;                                          0620
*       R6=PCTCATF;                 /* GET CURRENT PICTURE OF FLAGS  */
         L     R6,PCTCATF(,PCTPTR)                                 0621
*       R8FRRWRK=R6&SIOOFF;         /* FLAGS WITH THIS FUNCTION OFF  */
         LR    R8FRRWRK,R6                                         0622
         N     R8FRRWRK,@CF02996                                   0622
*       CS(R6,R8FRRWRK,PCTCATF);    /* SET FLAGS TO THIS FUNCTION OFF*/
         CS    R6,R8,PCTCATF(PCTPTR)                               0623
*       BC(4,SIOCHK);               /* SOMEONE ELSE BEAT US TO FLAGS.
*                                      CHECK AGAIN                   */
         BC    4,SIOCHK                                            0624
*       SDWASAV=R1;                 /* SAVE R1 ACR0SS CALLS          */
         ST    R1,SDWASAV(,AWSAPTR)                                0625
*       MCQESAV=R0;                 /* SAVE R0 ACR0SS CALLS          */
         ST    R0,MCQESAV(,AWSAPTR)                                0626
*       CALL DMPMOD;                /* TAKE A DUMP                   */
         L     R15,PCTDMPMD(,PCTPTR)                               0627
         BALR  R14,R15                                             0627
*       WTOUN=R15;                  /* MOVE IN BLANKS OR UN          */
         STH   R15,WTOUN(,AWSAPTR)                                 0628
*       R1=ADDR(WTOLIST);           /* SET UP PARM ADDR              */
         LA    R1,WTOLIST(,AWSAPTR)                                0629
*       CALL AHLWTO;                /* ISSUE AHLWTO                  */
         L     R15,PCTWTOMD(,PCTPTR)                               0630
         BALR  R14,R15                                             0630
*       R0=MCQESAV;                 /* RESTORE MCQE ADDR             */
*                                                                  0631
         L     R0,MCQESAV(,AWSAPTR)                                0631
*       /*************************************************************/
*       /*                                                           */
*       /* ISSUE SETEVENT TO DISABLE THIS FUNCTION                   */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0632
*       DO;                         /* SETEVENT NAME('GTF ')MCQE(MCQE
*                                      )ACTION(DISABLE)EIDAD(SIOEID)M
*                                      F(E,MCAWTO,COMPLETE)EIDNO(1)  */
*                                                                  0632
*         /***********************************************************/
*         /*                                                         */
*         /* THE FOLLOWING SECTION OF CODE IS GENERATED BY THE       */
*         /* SETEVENT MACRO. THIS CODE WILL BE EITHER AN INITIALIZED */
*         /* PARAMETER LIST, OR IF IT IS THE RESULT OF STANDARD OR   */
*         /* EXECUTE FORM IT WILL FILL IN THE LIST, GENERATE ALL     */
*         /* LINKAGES AND CALL THE PROPER SERVICE, EITHER AHLSETEV OR*/
*         /* AHLSETD                                                 */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0633
*         AHLPPTR=ADDR(MCAWTO);     /* SET UP PLIST PTR              */
         LA    AHLPPTR,MCAWTO(,AWSAPTR)                            0633
*         SEPL=SEPL&&SEPL;          /* INIT PLIST TO ZERO            */
         XC    SEPL(24,AHLPPTR),SEPL(AHLPPTR)                      0634
*         SEMC=ADDR(MCQE);          /* SET UP MCQE ADDRESS           */
         ST    R0,SEMC(,AHLPPTR)                                   0635
*         SENM='GTF     ';          /* MOVE NAME INTO PLIST          */
         MVC   SENM(8,AHLPPTR),@CC03243                            0636
*         SEFG=32;                  /* MOVE FLAGS INTO PLIST         */
         MVI   SEFG(AHLPPTR),X'20'                                 0637
*         SEEN=1;                   /* SET TO VALUE SPECIFIED        */
         MVI   SEEN(AHLPPTR),X'01'                                 0638
*         SEEL=ADDR(SIOEID);        /* SET TO VALUE SPECIFIED        */
         LA    R12,SIOEID                                          0639
         ST    R12,SEEL(,AHLPPTR)                                  0639
*         IF SEFG=32 SEFG=8 THEN    /* CHECK FOR DISABLE OR * CHANGE */
         CLI   SEFG(AHLPPTR),32                                    0640
         BE    @RT00640                                            0640
         CLI   SEFG(AHLPPTR),8                                     0640
         BNE   @RF00640                                            0640
@RT00640 DS    0H                                                  0641
*           CALL AHLSETD;           /* YES                           */
         L     R12,CVTPTR                                          0641
         L     R12,CVTGTF-CVT(,R12)                                0641
         L     R15,MCHSETD(,R12)                                   0641
         BALR  R14,R15                                             0641
*         ELSE                      /* NO                            */
*           CALL AHLSETEV;                                         0642
         B     @RC00640                                            0642
@RF00640 L     R12,CVTPTR                                          0642
         L     R12,CVTGTF-CVT(,R12)                                0642
         L     R15,MCHSETE(,R12)                                   0642
         BALR  R14,R15                                             0642
*       END;                                                       0643
@RC00640 DS    0H                                                  0644
*       R1=SDWASAV;                 /* RESTORE SDWA ADDR             */
         L     R1,SDWASAV(,AWSAPTR)                                0644
*     END;                                                         0645
*   RECLEN=LENGTH(SIOREC);          /* LENGTH OF RECORD FOR STACK    */
@RF00618 LA    RECLEN,50                                           0646
*   GO TO FRRGEN;                   /* COMMON EXIT ROUTINE           */
         B     FRRGEN                                              0647
*   DECLARE                         /* GENERAL PURPOSE REGISTERS     */
*     R0 PTR(31) REG(0),                                           0648
*     R1 PTR(31) REG(1),                                           0648
*     R14 PTR(31) REG(14),                                         0648
*     R15 PTR(31) REG(15);                                         0648
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       0649
*     I031F FIXED(31) BASED,                                       0649
*     I031P PTR(31) BASED,                                         0649
*     I015F FIXED(15) BASED,                                       0649
*     I015P PTR(15) BASED,                                         0649
*     I008P PTR(8) BASED,                                          0649
*     I001C CHAR(1) BASED;                                         0649
*AHLTEND:                                                          0650
*   END AHLTSIO                                                    0650
*                                                                  0650
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IHASRB  )                                       */
*/*%INCLUDE SYSLIB  (IHAASVT )                                       */
*/*%INCLUDE SYSLIB  (IECDIOSB)                                       */
*/*%INCLUDE SYSLIB  (IHALCCA )                                       */
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */
*/*%INCLUDE SYSLIB  (MCHEAD  )                                       */
*/*%INCLUDE SYSLIB  (IHAPCCA )                                       */
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */
*/*%INCLUDE SYSLIB  (IHAWSAVT)                                       */
*/*%INCLUDE SYSLIB  (IEFUCBOB)                                       */
*/*%INCLUDE SYSLIB  (MCQE    )                                       */
*/*%INCLUDE SYSLIB  (IMDMEDIT)                                       */
*/*%INCLUDE SYSLIB  (IHAFRRS )                                       */
*/*%INCLUDE SYSLIB  (IHASDWA )                                       */
*/*%INCLUDE SYSLIB  (EWAMAP  )                                       */
*/*%INCLUDE SYSLIB  (GTFPCT  )                                       */
*/*%INCLUDE SYSLIB  (MCAWSA  )                                       */
*/*%INCLUDE SYSLIB  (MCRWSA  )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*                                                                  0650
*       ;                                                          0650
AHLTEND  DS    0H                                                  0650
@DATA    DS    0H
@DATD    DSECT
         DS    0F
AHLTSIO  CSECT
         DS    0F
@CF02994 DC    XL4'DFF7FFFF'
@CF02996 DC    XL4'EFFBFFFF'
@CF03270 DC    XL4'0000FFFF'
@DATD    DSECT
         DS    0D
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
AHLTSIO  CSECT
         NOPR  ((@ENDDATD-@DATD)/61*16)
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
BASEADDR DC    AL4(AHLTSIO)
@CC03010 DC    C'***********************'
@CC03000 DC    C'N/A     '
@CC03243 DC    C'GTF     '
@CB03013 DC    X'EEEE'
ZERO     DC    X'0000000000000000'
UNAVAL   DC    CL32'U/A     U/A U/A     U/A     U/A '
RECID    DC    CL24'ahltsIO ahltsIO ahltsIO '
DMPHDR   DS    CL27
         ORG   DMPHDR
@NM00118 DC    AL1(26)
@NM00119 DC    CL26'DUMP OF GTF MODULE AHLTSIO'
         ORG   DMPHDR+27
WTONAMES DC    CL16'SIO  IO   TRAC  '
DMPLIST  DS    CL12
         ORG   DMPLIST
HDRPTR   DC    AL4(DMPHDR)
SIOSTRT  DC    AL4(AHLTEND)
SIOEND   DC    AL4(AHLTSIO)
         ORG   SIOEND
EOL      DC    X'80'
         ORG   DMPLIST+12
EIDLIST  DS    CL30
         ORG   EIDLIST
IOEIDS   DS    CL6
         ORG   IOEIDS+0
@NM00124 DC    X'5200'
@NM00125 DS    CL4
         ORG   IOEIDS+6
         ORG   IOEIDS+6
         DC    X'5101'
         ORG   IOEIDS+12
         ORG   IOEIDS+12
         DC    X'2100'
         ORG   IOEIDS+18
         ORG   IOEIDS+18
         DC    X'5201'
         ORG   EIDLIST+24
SIOEID   DC    X'5100'
@NM00126 DS    CL4
         ORG   EIDLIST+30
TSIOPTCH DC    100X'00'
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
PCTPTR   EQU   R2
AWSAPTR  EQU   R3
R8FRRWRK EQU   R8
RTCAREG  EQU   R4
RECLEN   EQU   R5
AHLRECPR EQU   R1
AHLSBREG EQU   R14
AHLRETCD EQU   R15
AHLPPTR  EQU   R1
         ENTRY TSIOPTCH
         ENTRY AHLTIO
         ENTRY AHLTUIO
         ENTRY AHLTSIO1
         SPACE
         PRINT NOGEN                                            ZP60011
         IHAPSA                                                 ZP60011
         CVT   DSECT=YES                                        ZP60011
         IHAPVT                                                 ZP60011
         IHAPFTE                                                ZP60011
         IHAASCB                                                ZP60011
         IHASRB                                                 ZP60011
         IEFUCBOB LIST=NO,PREFIX=YES                            ZP60011
         PRINT GEN                                              ZP60011
         SPACE
ASVT     EQU   0
ASVTFRST EQU   ASVT+524
ASVTENTY EQU   ASVT+528
IOSB     EQU   0
IOSFLA   EQU   IOSB
IOSACHN  EQU   IOSFLA
IOSFLB   EQU   IOSB+1
IOSFLC   EQU   IOSB+2
IOSDVALT EQU   IOSFLC
IOSDVRID EQU   IOSB+4
IOSASID  EQU   IOSB+6
IOSPKEY  EQU   IOSB+12
IOSOPT   EQU   IOSB+14
IOSOPT2  EQU   IOSB+15
@NM00005 EQU   IOSB+20
IOSCC    EQU   @NM00005
IOSCSW   EQU   @NM00005+1
IOSTATUS EQU   IOSCSW+3
IOSSRB   EQU   IOSB+28
IOSIPIB  EQU   IOSB+44
IOSERP   EQU   IOSB+52
IOSRST   EQU   IOSB+72
IOSDSID  EQU   IOSB+80
IOSAFF   EQU   IOSB+85
IOSPATH  EQU   IOSB+86
IOSCHN   EQU   IOSPATH
IOSFMSK  EQU   IOSB+88
IOSCKEY  EQU   IOSB+89
IOSEEKA  EQU   IOSB+100
IOSCCHH  EQU   IOSEEKA+3
IOSSKHH  EQU   IOSCCHH+2
LCCA     EQU   0
LCCAPGR2 EQU   LCCA+72
LCCAIHRC EQU   LCCA+520
LCCAIHR1 EQU   LCCAIHRC
LCCAIHR2 EQU   LCCAIHRC+1
LCCAIHR3 EQU   LCCAIHRC+2
LCCAIHR4 EQU   LCCAIHRC+3
LCCASPIN EQU   LCCA+524
LCCASPN1 EQU   LCCASPIN
LCCASPN2 EQU   LCCASPIN+1
LCCASPN3 EQU   LCCASPIN+2
LCCASPN4 EQU   LCCASPIN+3
LCCACPUS EQU   LCCA+536
LCCADSF1 EQU   LCCA+540
LCCADSF2 EQU   LCCA+541
LCCASPSA EQU   LCCA+544
LCCACRFL EQU   LCCA+692
LCCACREX EQU   LCCA+693
LCCALKFG EQU   LCCA+694
LCCASRBF EQU   LCCA+720
MCHEAD   EQU   0
MCHFLGS  EQU   MCHEAD+24
MCHFLG1  EQU   MCHFLGS
MCHSETE  EQU   MCHEAD+28
MCHSETD  EQU   MCHEAD+32
WSAG     EQU   0
WSAC     EQU   0
WSACGTF  EQU   WSAC+4
MCQE     EQU   0
MCQEASCB EQU   MCQE+16
MCQEAT   EQU   MCQE+32
MCQEFLGS EQU   MCQE+41
FRRS     EQU   0
FRRSHEAD EQU   FRRS
FRRSCURR EQU   FRRSHEAD+12
FRRSENTR EQU   0
FRRSFLGS EQU   FRRSENTR+4
FRRSFLG1 EQU   FRRSFLGS
SDWA     EQU   0
SDWAPARM EQU   SDWA
SDWAFIOB EQU   SDWA+4
SDWAABCC EQU   SDWAFIOB
SDWACMPF EQU   SDWAABCC
SDWACTL1 EQU   SDWA+8
SDWACMKA EQU   SDWACTL1
SDWAMWPA EQU   SDWACTL1+1
SDWAPMKA EQU   SDWACTL1+4
SDWACTL2 EQU   SDWA+16
SDWACMKP EQU   SDWACTL2
SDWAMWPP EQU   SDWACTL2+1
SDWAPMKP EQU   SDWACTL2+4
SDWAGRSV EQU   SDWA+24
SDWANAME EQU   SDWA+88
SDWAEC1  EQU   SDWA+104
SDWAEMK1 EQU   SDWAEC1
SDWAMWP1 EQU   SDWAEC1+1
SDWAINT1 EQU   SDWAEC1+2
SDWANXT1 EQU   SDWAEC1+4
SDWAAEC1 EQU   SDWA+112
SDWAILC1 EQU   SDWAAEC1+1
SDWAINC1 EQU   SDWAAEC1+2
SDWAICD1 EQU   SDWAINC1+1
SDWAEC2  EQU   SDWA+120
SDWAEMK2 EQU   SDWAEC2
SDWAMWP2 EQU   SDWAEC2+1
SDWAINT2 EQU   SDWAEC2+2
SDWANXT2 EQU   SDWAEC2+4
SDWAAEC2 EQU   SDWA+128
SDWAILC2 EQU   SDWAAEC2+1
SDWAINC2 EQU   SDWAAEC2+2
SDWAICD2 EQU   SDWAINC2+1
SDWASRSV EQU   SDWA+136
SDWAIDNT EQU   SDWA+200
SDWAMCH  EQU   SDWA+204
SDWASTCK EQU   SDWAMCH
SDWAMCHI EQU   SDWAMCH+8
SDWAMCHS EQU   SDWAMCHI
SDWAMCHD EQU   SDWAMCHI+1
SDWAREGU EQU   SDWAMCHD
SDWAPSWU EQU   SDWAMCHD
SDWAINSF EQU   SDWAMCHD
SDWARSR1 EQU   SDWAMCH+12
SDWARSR2 EQU   SDWAMCH+13
SDWAFLGS EQU   SDWA+232
SDWAERRA EQU   SDWAFLGS
SDWAMCHK EQU   SDWAERRA
SDWAERRB EQU   SDWAFLGS+1
SDWAERRC EQU   SDWAFLGS+2
SDWAPERC EQU   SDWAERRC
SDWAEAS  EQU   SDWAERRC
SDWAERRD EQU   SDWAFLGS+3
SDWACLUP EQU   SDWAERRD
SDWAIOFS EQU   SDWA+238
SDWARTYA EQU   SDWA+240
SDWACPUA EQU   SDWA+248
SDWAPARQ EQU   SDWA+252
SDWARCDE EQU   SDWAPARQ
SDWAACF2 EQU   SDWAPARQ+1
SDWAACF3 EQU   SDWAPARQ+2
SDWAACF4 EQU   SDWAPARQ+3
SDWALKWA EQU   SDWA+256
SDWALKWS EQU   SDWALKWA
SDWARECP EQU   SDWA+292
SDWASNPA EQU   SDWA+320
SDWADUMP EQU   SDWASNPA
SDWADPFS EQU   SDWADUMP+1
SDWADDAT EQU   SDWASNPA+4
SDWASDAT EQU   SDWADDAT
SDWAPDAT EQU   SDWADDAT+2
SDWADPSA EQU   SDWA+328
SDWADPSL EQU   SDWADPSA
SDWARA   EQU   SDWA+400
SDWADPVA EQU   SDWARA+2
EWA      EQU   0
EWAHDR   EQU   EWA
EWAFLAGS EQU   EWA+4
EWAFLG1  EQU   EWAFLAGS
EWASCCD  EQU   EWAFLG1
EWASCC3  EQU   EWASCCD
EWAFLG2  EQU   EWAFLAGS+1
EWASNSCT EQU   EWAFLAGS+3
EWACNTRS EQU   EWA+10
EWAERPIB EQU   EWA+16
EWARGFG1 EQU   EWAERPIB+4
EWARGFG2 EQU   EWAERPIB+5
EWAXCSW1 EQU   EWAERPIB+6
EWAXCSW2 EQU   EWAERPIB+7
EWAIERP  EQU   EWA+32
GTFPCT   EQU   0
GTFSTAT  EQU   GTFPCT+8
GTFDEBUG EQU   GTFSTAT
GTFOPTS  EQU   GTFPCT+12
GTFOPTS1 EQU   GTFOPTS
OPTTRC   EQU   GTFOPTS1
GTFOPTS2 EQU   GTFOPTS+1
GTFOPTS3 EQU   GTFOPTS+2
GTFOPTS4 EQU   GTFOPTS+3
OPTTIME  EQU   GTFOPTS4
PCTCATF  EQU   GTFPCT+16
PCTIO    EQU   PCTCATF
PCTSIO   EQU   PCTCATF
PCTIOF   EQU   PCTCATF+1
PCTSIOF  EQU   PCTCATF+1
NEXTBLOK EQU   GTFPCT+180
SBLOKRTN EQU   GTFPCT+188
WRTRCOM  EQU   GTFPCT+204
TRMWRIT  EQU   GTFPCT+212
WRTRERR  EQU   GTFPCT+216
GTFSSRB  EQU   GTFPCT+220
QLOCKWRD EQU   GTFPCT+236
QLOCKECB EQU   GTFPCT+240
AUDITFLG EQU   GTFPCT+296
GTFASCB  EQU   GTFPCT+300
PCTDMPMD EQU   GTFPCT+308
PCTWTOMD EQU   GTFPCT+312
MCAWSA   EQU   0
MCATRAIL EQU   MCAWSA+256
MCATRL1  EQU   MCATRAIL
MCAIO    EQU   MCATRL1
MCASIO   EQU   MCATRL1
MCATRL2  EQU   MCATRAIL+1
MCAUIO   EQU   MCATRL2
MCAERROR EQU   MCATRL2
MCATRL3  EQU   MCATRAIL+2
MCATRL4  EQU   MCATRAIL+3
MCAMUCB  EQU   MCATRL4
MCAMASCB EQU   MCATRL4
MCAMTCB  EQU   MCATRL4
MCAMIOSB EQU   MCATRL4
MCAMJOBN EQU   MCATRL4
MCATRL5  EQU   MCATRAIL+4
MCATRL6  EQU   MCATRAIL+5
MCATRL7  EQU   MCATRAIL+6
MCATRL8  EQU   MCATRAIL+7
MCAMCR   EQU   MCAWSA+264
MCARECPR EQU   MCAWSA+268
MCAREC   EQU   MCAWSA+284
MCAWORK  EQU   MCAWSA+584
MCRWSA   EQU   0
MCROUTSA EQU   MCRWSA+64
MCRMCA   EQU   MCRWSA+128
MCRWSAF  EQU   MCRWSA+132
MCREID   EQU   MCRWSA+134
JOBNAME  EQU   0
JBNMTEST EQU   JOBNAME
MCHKRTY  EQU   0
FRRPARM  EQU   0
TRAILSAV EQU   FRRPARM+4
SVR14NRM EQU   FRRPARM+8
SVR14FRR EQU   FRRPARM+12
SVR14RTN EQU   0
DMPMOD   EQU   0
AHLWTO   EQU   0
AHLGREC  EQU   0
AHLGLGTH EQU   AHLGREC
AHLGRES  EQU   AHLGREC+2
AHLGAID  EQU   AHLGREC+4
AHLGFID  EQU   AHLGREC+5
AHLGTST  EQU   AHLGREC+6
AHLGEIDS EQU   AHLGTST
AHLGEIDL EQU   AHLGREC+14
AHLSBLOK EQU   0
I256C    EQU   0
AHLSETEV EQU   0
AHLSETD  EQU   0
SEPL     EQU   0
SEFG     EQU   SEPL
SEEN     EQU   SEPL+2
SENM     EQU   SEPL+4
SEEL     EQU   SEPL+12
SEMC     EQU   SEPL+20
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
SRBPTR   EQU   IOSSRB
ASVTPTR  EQU   CVTASVT-CVT
EVREGS   EQU   LCCAPGR2
EVREG01  EQU   EVREGS
EVREG0   EQU   EVREG01
EVREG2   EQU   EVREGS+8
EVREG6   EQU   EVREGS+24
EVREG7   EQU   EVREGS+28
IOSBPTR  EQU   EVREG2
IOSATTSC EQU   IOSIPIB
IOSWTOWA EQU   IOSIPIB
IOSPCISC EQU   IOSIPIB
LCCAPTR  EQU   PSALCCAV
ASCBPTR  EQU   MCQEASCB
FRRSPTR  EQU   PSACSTK
FRREPTR  EQU   FRRSCURR
EWAPTR   EQU   IOSERP
EWADDIOS EQU   EWAIERP
EWASNS   EQU   EWADDIOS
WORKAREA EQU   MCAWORK
MCQESAV  EQU   WORKAREA
SDWASAV  EQU   WORKAREA+4
MCAWTO   EQU   WORKAREA+8
WTOLIST  EQU   MCAWTO
WTOFUNC  EQU   WTOLIST+4
WTOUN    EQU   WTOLIST+14
ERRSLOT  EQU   MCAREC
IOSCPA   EQU   IOSRST
MCRREGS  EQU   MCROUTSA
MCRREG0  EQU   MCRREGS
IOREC    EQU   MCAREC
IOASCB   EQU   IOREC
IOJOBN   EQU   IOREC+6
IOPSW    EQU   IOREC+14
IOTCB    EQU   IOREC+22
IOCSW    EQU   IOREC+26
IOCPA    EQU   IOREC+34
IOSENSE  EQU   IOREC+42
IODEV    EQU   IOREC+44
IODSID   EQU   IOREC+46
IOFLGS   EQU   IOREC+50
IOFLA    EQU   IOFLGS
IOOPT    EQU   IOFLGS+1
IOFMSK   EQU   IOFLGS+2
IODVRID  EQU   IOFLGS+3
IOUCBER  EQU   IOFLGS+4
IOCSID   EQU   IOREC+59
SIOREC   EQU   MCAREC
SIOASCB  EQU   SIOREC
SIOJOBN  EQU   SIOREC+6
SIOCPA   EQU   SIOREC+14
SIODSID  EQU   SIOREC+22
SIOCC    EQU   SIOREC+26
SIODEV   EQU   SIOREC+27
SIOCAW   EQU   SIOREC+29
SIOSTS   EQU   SIOREC+33
SIOSEEK  EQU   SIOREC+35
SIOFLGS  EQU   SIOREC+43
SIOAFF   EQU   SIOFLGS
SIOPATH  EQU   SIOFLGS+1
SIOOPT   EQU   SIOFLGS+3
SIOFMSK  EQU   SIOFLGS+4
SIODVRID EQU   SIOFLGS+5
SIOCSID  EQU   SIOREC+49
SIOCCW#  EQU   SIOREC+50                                        ZP60011
SIOCCWS  EQU   SIOREC+52                                        ZP60011
UIOREC   EQU   MCAREC
UIOASCB  EQU   UIOREC
UIOJOBN  EQU   UIOREC+6
UIOPSW   EQU   UIOREC+14
UIOCSW   EQU   UIOREC+22
UIODEV   EQU   UIOREC+30
UIOCSID  EQU   UIOREC+32
PRECM    EQU   MCAREC
PRECPUID EQU   PRECM+4
IOUA1    EQU   IOCPA
IOUA2    EQU   IODSID
SIOUA1   EQU   SIOCPA
SIOUA2   EQU   SIOSEEK
*                                      START UNREFERENCED COMPONENTS
PREASCB  EQU   PRECM
UIOCPUID EQU   UIOREC+4
SIOCPUID EQU   SIOREC+4
IOCHAN   EQU   IOUCBER+3
IOCPU    EQU   IOUCBER+2
IOSFLS   EQU   IOUCBER
IOCPUID  EQU   IOREC+4
MCRREG1  EQU   MCRREGS+4
WTOMODE  EQU   WTOLIST+8
WTOMOD   EQU   WTOLIST
EWAEND   EQU   EWADDIOS+71
EWASCSW  EQU   EWADDIOS+64
IOSPCIWA EQU   IOSPCISC+44
IOSPCIRS EQU   IOSPCISC+8
@NM00010 EQU   IOSPCISC+4
@NM00009 EQU   IOSPCISC
IOSWRNDR EQU   IOSWTOWA+4
IOSWTOPT EQU   IOSWTOWA+3
IOSWTOCP EQU   IOSWTOWA+2
IOSWTOCH EQU   IOSWTOWA
IOSATTWA EQU   IOSATTSC+40
@NM00008 EQU   IOSATTSC+24
IOSATTSN EQU   IOSATTSC
EVREG15  EQU   EVREGS+60
@NM00122 EQU   EVREGS+32
@NM00121 EQU   EVREGS+12
EVREG1   EQU   EVREG01+4
@NM00120 EQU   EVREG0+2
EVREG0ID EQU   EVREG0
SECL     EQU   SEPL+16
SECN     EQU   SEPL+3
@NM00127 EQU   SEPL+1
SVR14CAL EQU   FRRPARM+16
@NM00123 EQU   FRRPARM+2
RECURCTR EQU   FRRPARM
MCRRESV  EQU   MCRWSAF
MCRBRENT EQU   MCRWSAF
MCERROR  EQU   MCRWSAF
MCINFGBR EQU   MCRWSAF
MCINCTL  EQU   MCRWSAF
MCRMCIH  EQU   MCRWSA
MCASWORK EQU   MCAWSA+644
@NM00115 EQU   MCATRL8
@NM00114 EQU   MCATRL8
@NM00113 EQU   MCATRL8
@NM00112 EQU   MCATRL8
MCAFRECT EQU   MCATRL7
MCAFRECC EQU   MCATRL7
MCAFRESP EQU   MCATRL7
MCAFREXJ EQU   MCATRL7
MCAFRSBR EQU   MCATRL6
MCAFRDMJ EQU   MCATRL6
MCAFRDPC EQU   MCATRL6
MCAFRPIC EQU   MCATRL6
MCAFRPIJ EQU   MCATRL6
@NM00111 EQU   MCATRL5
MCADREGS EQU   MCATRL5
MCAFRRB  EQU   MCATRL5
MCAFRRJ  EQU   MCATRL5
MCAFRRA  EQU   MCATRL5
MCAFRERT EQU   MCATRL5
MCAFREPC EQU   MCATRL5
MCAMSDWA EQU   MCATRL4
MCAMBTU  EQU   MCATRL4
MCAMEXT  EQU   MCATRL4
MCAFRFRT EQU   MCATRL3
MCAFRFA  EQU   MCATRL3
MCAFRSJ  EQU   MCATRL3
MCAFRSA  EQU   MCATRL3
MCAPIF   EQU   MCATRL3
MCASIOF  EQU   MCATRL3
MCAIOF   EQU   MCATRL3
MCASVCF  EQU   MCATRL3
MCATDIR  EQU   MCATRL2
MCALSR   EQU   MCATRL2
MCASTA   EQU   MCATRL2
MCAFRR   EQU   MCATRL2
MCASRM   EQU   MCATRL2
MCARNIO  EQU   MCATRL2
MCAUSR   EQU   MCATRL1
MCAPI    EQU   MCATRL1
MCASRB   EQU   MCATRL1
MCADSP   EQU   MCATRL1
MCAEXT   EQU   MCATRL1
MCASVC   EQU   MCATRL1
MCAMSG   EQU   MCAWSA+192
MCASETEV EQU   MCAWSA+128
MCAFWORK EQU   MCAWSA+112
MCASFEOB EQU   MCAWSA+56
MCASBLOK EQU   MCAWSA
TRANSMAX EQU   GTFPCT+304
@NM00110 EQU   AUDITFLG+3
AFSLEBLD EQU   AUDITFLG+3
WSERFG06 EQU   AUDITFLG+3
WSERFG05 EQU   AUDITFLG+3
WSERFG04 EQU   AUDITFLG+3
WSERFG03 EQU   AUDITFLG+3
WSERFG02 EQU   AUDITFLG+2
WSERFLAG EQU   AUDITFLG+2
TERMFLAG EQU   AUDITFLG+2
AFXWRIT  EQU   AUDITFLG+2
AFQLWAIT EQU   AUDITFLG+2
AFSLEGET EQU   AUDITFLG+2
AFBUFGET EQU   AUDITFLG+2
AFFEOB   EQU   AUDITFLG+2
AFBLKFIX EQU   AUDITFLG+1
AFEQFIX  EQU   AUDITFLG+1
AFPGRLSE EQU   AUDITFLG+1
AFWRAP   EQU   AUDITFLG+1
AFBUFOFF EQU   AUDITFLG+1
AFLDFX   EQU   AUDITFLG+1
AFCLOSE  EQU   AUDITFLG+1
AFOPEN   EQU   AUDITFLG+1
AFGMBLOK EQU   AUDITFLG
AFGMSRB  EQU   AUDITFLG
AFWAIT   EQU   AUDITFLG
AFTERM   EQU   AUDITFLG
AFINIT   EQU   AUDITFLG
AFWWRIT  EQU   AUDITFLG
AFCWRIT  EQU   AUDITFLG
AFIWRIT  EQU   AUDITFLG
EQSHORT  EQU   GTFPCT+292
SLEMAX   EQU   GTFPCT+288
SLECNT   EQU   GTFPCT+284
RQPURGE  EQU   GTFPCT+280
RQTOTAL  EQU   GTFPCT+276
WRTDLST  EQU   GTFPCT+272
NUMOFBUF EQU   GTFPCT+268
SBUFDISA EQU   GTFPCT+264
SLESUSED EQU   GTFPCT+260
EQMIN    EQU   GTFPCT+256
EQLENGTH EQU   GTFPCT+252
WQMAX    EQU   GTFPCT+248
WQLENGTH EQU   GTFPCT+244
QLECBP   EQU   QLOCKECB
@NM00109 EQU   QLOCKECB
@NM00108 EQU   QLOCKWRD+3
SCHEDBUF EQU   QLOCKWRD+2
POSTWRIT EQU   QLOCKWRD+1
QLOCK    EQU   QLOCKWRD
WRITERDD EQU   GTFPCT+228
GTFLOSTR EQU   GTFPCT+224
SRBAVAIL EQU   GTFSSRB
WRTRERRP EQU   WRTRERR
@NM00107 EQU   WRTRERR
TRMWRITP EQU   TRMWRIT
@NM00106 EQU   TRMWRIT
GTFCMECB EQU   GTFPCT+208
WRTRCOMP EQU   WRTRCOM
@NM00105 EQU   WRTRCOM
TRANSCNT EQU   GTFPCT+200
AHLRDPLQ EQU   GTFPCT+196
SFEOBRTN EQU   GTFPCT+192
CURRBLOK EQU   GTFPCT+184
GBCDS    EQU   NEXTBLOK
WSMODEND EQU   GTFPCT+176
WSMODBEG EQU   GTFPCT+172
BCBPAGE  EQU   GTFPCT+156
SLEPAGE  EQU   GTFPCT+152
SAVECNT  EQU   GTFPCT+148
SLQHEAD  EQU   GTFPCT+144
HQLENGTH EQU   GTFPCT+140
QTAIL    EQU   GTFPCT+136
HQHEAD   EQU   GTFPCT+132
WQHEAD   EQU   GTFPCT+128
RQHEAD   EQU   GTFPCT+124
EQHEAD   EQU   GTFPCT+120
WPOSTSRB EQU   GTFPCT+116
WTSKTCBA EQU   GTFPCT+112
WRTRTCBA EQU   GTFPCT+108
TRMWTASK EQU   GTFPCT+104
WTSKATTH EQU   GTFPCT+100
WTSKINIT EQU   GTFPCT+96
WTASKECB EQU   GTFPCT+92
WMTABPTR EQU   GTFPCT+88
ATTCHECB EQU   GTFPCT+84
AHLECB   EQU   GTFPCT+80
STOPECBA EQU   GTFPCT+76
ECBADRLT EQU   GTFPCT+72
FIXLEN   EQU   GTFPCT+68
FIXPTR   EQU   GTFPCT+64
INITPTR  EQU   GTFPCT+60
MCQEPTR  EQU   GTFPCT+56
GTFEIDL  EQU   GTFPCT+52
GTFPIBL  EQU   GTFPCT+48
GTFPIFT  EQU   GTFPCT+44
GTFSVCBL EQU   GTFPCT+40
GTFSVCFT EQU   GTFPCT+36
GTFIOBL  EQU   GTFPCT+32
GTFIOFT  EQU   GTFPCT+28
GTFSIOBL EQU   GTFPCT+24
GTFSIOFT EQU   GTFPCT+20
@NM00104 EQU   PCTCATF+1
PCTPIF   EQU   PCTCATF+1
PCTSVCF  EQU   PCTCATF+1
PCTRR    EQU   PCTCATF+1
PCTSRM   EQU   PCTCATF+1
PCTRNIO  EQU   PCTCATF+1
PCTUSR   EQU   PCTCATF
PCTPI    EQU   PCTCATF
@NM00103 EQU   PCTCATF
PCTDSP   EQU   PCTCATF
PCTEXT   EQU   PCTCATF
PCTSVC   EQU   PCTCATF
OPTRES3  EQU   GTFOPTS4
OPTIOSIO EQU   GTFOPTS3
OPTRES2  EQU   GTFOPTS3
OPTRR    EQU   GTFOPTS3
OPTSRM   EQU   GTFOPTS3
OPTRNIO  EQU   GTFOPTS3
OPTEXT   EQU   GTFOPTS3
OPTIOP   EQU   GTFOPTS2
OPTIO    EQU   GTFOPTS2
OPTPIP   EQU   GTFOPTS2
OPTPI    EQU   GTFOPTS2
OPTSIOP  EQU   GTFOPTS2
OPTSIO   EQU   GTFOPTS2
OPTSVCP  EQU   GTFOPTS2
OPTSVC   EQU   GTFOPTS2
OPTPCI   EQU   GTFOPTS1
OPTRES1  EQU   GTFOPTS1
OPTDSP   EQU   GTFOPTS1
OPTUSR   EQU   GTFOPTS1
OPTSYS   EQU   GTFOPTS1
OPTSYSP  EQU   GTFOPTS1
OPTSYSM  EQU   GTFOPTS1
GTFRESV  EQU   GTFSTAT
GTFMODE  EQU   GTFSTAT
GTFPCTID EQU   GTFPCT
EWADDISP EQU   EWA+29
EWADCNT  EQU   EWA+28
EWACPU   EQU   EWA+27
@NM00102 EQU   EWA+26
EWACHA   EQU   EWA+24
EWACSEQ  EQU   EWAXCSW2
EWACDIN  EQU   EWAXCSW2
@NM00101 EQU   EWAXCSW2
EWACTEC  EQU   EWAXCSW2
EWACDAV  EQU   EWAXCSW1
EWACCHV  EQU   EWAXCSW1
EWACCMD  EQU   EWAXCSW1
EWACUNS  EQU   EWAXCSW1
EWACSQV  EQU   EWAXCSW1
@NM00100 EQU   EWAXCSW1
EWACITF  EQU   EWAXCSW1
@NM00099 EQU   EWARGFG2
EWACCUE  EQU   EWARGFG2
EWACSTG  EQU   EWARGFG2
EWACSCU  EQU   EWARGFG2
EWACCHA  EQU   EWARGFG2
EWACCPU  EQU   EWARGFG2
EWANORTY EQU   EWARGFG1
EWACCNT  EQU   EWARGFG1
EWACSNS  EQU   EWARGFG1
@NM00098 EQU   EWARGFG1
EWACHIO  EQU   EWARGFG1
EWACTIO  EQU   EWARGFG1
EWACINT  EQU   EWARGFG1
EWACSIO  EQU   EWARGFG1
EWAUCB   EQU   EWAERPIB+1
@NM00097 EQU   EWAERPIB
EWASTUP  EQU   EWA+14
EWACNTR4 EQU   EWACNTRS+3
EWACNTR3 EQU   EWACNTRS+2
EWACNTR2 EQU   EWACNTRS+1
EWACNTR1 EQU   EWACNTRS
EWASSTAT EQU   EWA+8
EWASCTMX EQU   EWASNSCT
@NM00096 EQU   EWASNSCT
EWAFLG3  EQU   EWAFLAGS+2
@NM00095 EQU   EWAFLG2
EWADIR   EQU   EWAFLG2
EWACOVF  EQU   EWAFLG2
EWAWTEMP EQU   EWAFLG2
EWAMDR   EQU   EWAFLG2
EWABDSNS EQU   EWAFLG1
EWADDMSG EQU   EWAFLG1
EWASCC1  EQU   EWASCC3
EWASCC2  EQU   EWASCC3
@NM00094 EQU   EWAFLG1
EWASLIS  EQU   EWAFLG1
EWAEXT   EQU   EWAHDR
@NM00093 EQU   SDWA+512
SDWAVRA  EQU   SDWARA+4
SDWAURAL EQU   SDWARA+3
@NM00092 EQU   SDWADPVA
SDWAEBC  EQU   SDWADPVA
SDWAHEX  EQU   SDWADPVA
SDWAVRAL EQU   SDWARA
@NM00091 EQU   SDWA+396
SDWACOMP EQU   SDWA+392
SDWARCPL EQU   SDWA+364
@NM00090 EQU   SDWADPSA+32
SDWATO4  EQU   SDWADPSL+28
SDWAFRM4 EQU   SDWADPSL+24
SDWATO3  EQU   SDWADPSL+20
SDWAFRM3 EQU   SDWADPSL+16
SDWATO2  EQU   SDWADPSL+12
SDWAFRM2 EQU   SDWADPSL+8
SDWATO1  EQU   SDWADPSL+4
SDWAFRM1 EQU   SDWADPSL
@NM00089 EQU   SDWADDAT+3
@NM00088 EQU   SDWAPDAT
SDWAUSPL EQU   SDWAPDAT
SDWADPSW EQU   SDWAPDAT
SDWATJPA EQU   SDWAPDAT
SDWATLPA EQU   SDWAPDAT
SDWADREG EQU   SDWAPDAT
SDWADSAH EQU   SDWAPDAT
SDWADSAS EQU   SDWAPDAT
@NM00087 EQU   SDWADDAT+1
@NM00086 EQU   SDWASDAT
SDWAQQS  EQU   SDWASDAT
SDWACBS  EQU   SDWASDAT
SDWAGTF  EQU   SDWASDAT
SDWASWA  EQU   SDWASDAT
SDWALSQA EQU   SDWASDAT
SDWASQA  EQU   SDWASDAT
SDWANUC  EQU   SDWASDAT
@NM00085 EQU   SDWADUMP+2
@NM00084 EQU   SDWADPFS
SDWASLST EQU   SDWADPFS
@NM00083 EQU   SDWADPFS
SDWADLST EQU   SDWADPFS
SDWADPT  EQU   SDWADPFS
SDWADPID EQU   SDWADUMP
SDWADPLA EQU   SDWA+316
SDWAREXN EQU   SDWARECP+16
SDWACSCT EQU   SDWARECP+8
SDWAMODN EQU   SDWARECP
@NM00082 EQU   SDWALKWA+32
SDWATALW EQU   SDWALKWS+28
SDWATDLW EQU   SDWALKWS+24
SDWATNLW EQU   SDWALKWS+20
SDWAAPLW EQU   SDWALKWS+16
SDWAIPLW EQU   SDWALKWS+12
SDWAILLW EQU   SDWALKWS+8
SDWAIULW EQU   SDWALKWS+4
SDWAICLW EQU   SDWALKWS
SDWAFLLK EQU   SDWAACF4
SDWACMS  EQU   SDWAACF4
SDWAOPTM EQU   SDWAACF4
SDWATADB EQU   SDWAACF4
SDWATDNB EQU   SDWAACF4
SDWATNCB EQU   SDWAACF4
SDWAILCH EQU   SDWAACF4
SDWAIUCB EQU   SDWAACF4
SDWAICAT EQU   SDWAACF3
SDWAIPRG EQU   SDWAACF3
SDWASALL EQU   SDWAACF3
SDWAASMP EQU   SDWAACF3
SDWADISP EQU   SDWAACF3
@NM00081 EQU   SDWAACF3
@NM00080 EQU   SDWAACF2
SDWAFREE EQU   SDWAACF2
SDWAUPRG EQU   SDWAACF2
@NM00079 EQU   SDWAACF2
SDWASPIN EQU   SDWAACF2
@NM00078 EQU   SDWAACF2
SDWARCRD EQU   SDWAACF2
SDWALCPU EQU   SDWACPUA+2
@NM00077 EQU   SDWACPUA
SDWARECA EQU   SDWA+244
@NM00076 EQU   SDWA+239
@NM00075 EQU   SDWAIOFS
SDWANIOP EQU   SDWAIOFS
SDWANOIO EQU   SDWAIOFS
SDWAIOHT EQU   SDWAIOFS
SDWAIOQR EQU   SDWAIOFS
SDWAFMID EQU   SDWA+236
@NM00074 EQU   SDWAERRD
SDWAMCIV EQU   SDWAERRD
SDWARPIV EQU   SDWAERRD
SDWAMABD EQU   SDWAERRD
SDWACTS  EQU   SDWAERRD
SDWASTAE EQU   SDWAERRD
SDWANRBE EQU   SDWAERRD
@NM00073 EQU   SDWAERRC
SDWAIRB  EQU   SDWAERRC
SDWASTAI EQU   SDWAERRC
SDWASTAF EQU   SDWAERRC
SDWASRBM EQU   SDWAERRB
SDWALDIS EQU   SDWAERRB
SDWAENRB EQU   SDWAERRB
SDWATYP1 EQU   SDWAERRB
@NM00072 EQU   SDWAERRB
SDWAPGIO EQU   SDWAERRA
SDWATEXC EQU   SDWAERRA
SDWASVCE EQU   SDWAERRA
SDWAABTM EQU   SDWAERRA
SDWASVCD EQU   SDWAERRA
SDWARKEY EQU   SDWAERRA
SDWAPCHK EQU   SDWAERRA
SDWATIME EQU   SDWAMCH+20
SDWARFSA EQU   SDWAMCH+16
@NM00071 EQU   SDWAMCH+14
SDWAVEQR EQU   SDWARSR2
SDWAPGFX EQU   SDWARSR2
SDWAFLSQ EQU   SDWARSR2
SDWAFSQA EQU   SDWARSR2
SDWANUCL EQU   SDWARSR2
SDWASPER EQU   SDWARSR2
SDWAINTC EQU   SDWARSR2
SDWAOFLN EQU   SDWARSR2
SDWACHNG EQU   SDWARSR1
SDWAMSER EQU   SDWARSR1
@NM00070 EQU   SDWARSR1
SDWACPID EQU   SDWAMCH+10
SDWATERR EQU   SDWAMCHD
SDWAFPRX EQU   SDWAMCHD
SDWAACR  EQU   SDWAMCHD
SDWASCK  EQU   SDWAMCHD
SDWASKYF EQU   SDWAMCHD
@NM00069 EQU   SDWAMCHS
SDWARSRF EQU   SDWAMCHS
SDWARSRC EQU   SDWAMCHS
SDWAINVP EQU   SDWAMCHS
SDWATSVL EQU   SDWAMCHS
SDWARCDF EQU   SDWAMCHS
SDWASRVL EQU   SDWAMCHS
SDWASCKE EQU   SDWASTCK+4
SDWASCKB EQU   SDWASTCK
SDWALNTH EQU   SDWAIDNT+1
SDWASPID EQU   SDWAIDNT
SDWASR15 EQU   SDWASRSV+60
SDWASR14 EQU   SDWASRSV+56
SDWASR13 EQU   SDWASRSV+52
SDWASR12 EQU   SDWASRSV+48
SDWASR11 EQU   SDWASRSV+44
SDWASR10 EQU   SDWASRSV+40
SDWASR09 EQU   SDWASRSV+36
SDWASR08 EQU   SDWASRSV+32
SDWASR07 EQU   SDWASRSV+28
SDWASR06 EQU   SDWASRSV+24
SDWASR05 EQU   SDWASRSV+20
SDWASR04 EQU   SDWASRSV+16
SDWASR03 EQU   SDWASRSV+12
SDWASR02 EQU   SDWASRSV+8
SDWASR01 EQU   SDWASRSV+4
SDWASR00 EQU   SDWASRSV
SDWATRN2 EQU   SDWAAEC2+4
SDWAIPC2 EQU   SDWAICD2
SDWAIMC2 EQU   SDWAICD2
SDWAIPR2 EQU   SDWAICD2
@NM00068 EQU   SDWAINC2
@NM00067 EQU   SDWAILC2
SDWAIL2  EQU   SDWAILC2
@NM00066 EQU   SDWAILC2
@NM00065 EQU   SDWAAEC2
SDWAADD2 EQU   SDWANXT2+1
@NM00064 EQU   SDWANXT2
@NM00063 EQU   SDWAEC2+3
SDWASGN2 EQU   SDWAINT2
SDWAEXP2 EQU   SDWAINT2
SDWADEC2 EQU   SDWAINT2
SDWAFPO2 EQU   SDWAINT2
SDWACC2  EQU   SDWAINT2
@NM00062 EQU   SDWAINT2
SDWAPGM2 EQU   SDWAMWP2
SDWAWAT2 EQU   SDWAMWP2
SDWAMCK2 EQU   SDWAMWP2
SDWAECT2 EQU   SDWAMWP2
SDWAKEY2 EQU   SDWAMWP2
SDWAEXT2 EQU   SDWAEMK2
SDWAIO2  EQU   SDWAEMK2
SDWATRM2 EQU   SDWAEMK2
@NM00061 EQU   SDWAEMK2
SDWAPER2 EQU   SDWAEMK2
@NM00060 EQU   SDWAEMK2
SDWATRAN EQU   SDWAAEC1+4
SDWAIPC1 EQU   SDWAICD1
SDWAIMC1 EQU   SDWAICD1
SDWAIPR1 EQU   SDWAICD1
@NM00059 EQU   SDWAINC1
@NM00058 EQU   SDWAILC1
SDWAIL1  EQU   SDWAILC1
@NM00057 EQU   SDWAILC1
@NM00056 EQU   SDWAAEC1
SDWAADD1 EQU   SDWANXT1+1
@NM00055 EQU   SDWANXT1
@NM00054 EQU   SDWAEC1+3
SDWASGN1 EQU   SDWAINT1
SDWAEXP1 EQU   SDWAINT1
SDWADEC1 EQU   SDWAINT1
SDWAFPO1 EQU   SDWAINT1
SDWACC1  EQU   SDWAINT1
@NM00053 EQU   SDWAINT1
SDWAPGM1 EQU   SDWAMWP1
SDWAWAT1 EQU   SDWAMWP1
SDWAMCK1 EQU   SDWAMWP1
SDWAECT1 EQU   SDWAMWP1
SDWAKEY1 EQU   SDWAMWP1
SDWAEXT1 EQU   SDWAEMK1
SDWAIO1  EQU   SDWAEMK1
SDWATRM1 EQU   SDWAEMK1
@NM00052 EQU   SDWAEMK1
SDWAPER1 EQU   SDWAEMK1
@NM00051 EQU   SDWAEMK1
SDWAIOBR EQU   SDWA+100
SDWAEPA  EQU   SDWA+96
@NM00050 EQU   SDWANAME+4
SDWARBAD EQU   SDWANAME
SDWAGR15 EQU   SDWAGRSV+60
SDWAGR14 EQU   SDWAGRSV+56
SDWAGR13 EQU   SDWAGRSV+52
SDWAGR12 EQU   SDWAGRSV+48
SDWAGR11 EQU   SDWAGRSV+44
SDWAGR10 EQU   SDWAGRSV+40
SDWAGR09 EQU   SDWAGRSV+36
SDWAGR08 EQU   SDWAGRSV+32
SDWAGR07 EQU   SDWAGRSV+28
SDWAGR06 EQU   SDWAGRSV+24
SDWAGR05 EQU   SDWAGRSV+20
SDWAGR04 EQU   SDWAGRSV+16
SDWAGR03 EQU   SDWAGRSV+12
SDWAGR02 EQU   SDWAGRSV+8
SDWAGR01 EQU   SDWAGRSV+4
SDWAGR00 EQU   SDWAGRSV
SDWANXTP EQU   SDWACTL2+5
SDWASGP  EQU   SDWAPMKP
SDWAEUP  EQU   SDWAPMKP
SDWADOP  EQU   SDWAPMKP
SDWAFPP  EQU   SDWAPMKP
SDWACCP  EQU   SDWAPMKP
SDWAILP  EQU   SDWAPMKP
SDWAINTP EQU   SDWACTL2+2
SDWASPVP EQU   SDWAMWPP
SDWAWATP EQU   SDWAMWPP
SDWAMCKP EQU   SDWAMWPP
@NM00049 EQU   SDWAMWPP
SDWAKEYP EQU   SDWAMWPP
SDWAEXTP EQU   SDWACMKP
SDWAIOP  EQU   SDWACMKP
SDWANXTA EQU   SDWACTL1+5
SDWASGA  EQU   SDWAPMKA
SDWAEUA  EQU   SDWAPMKA
SDWADOA  EQU   SDWAPMKA
SDWAFPA  EQU   SDWAPMKA
SDWACCA  EQU   SDWAPMKA
SDWAILA  EQU   SDWAPMKA
SDWAINTA EQU   SDWACTL1+2
SDWASPVA EQU   SDWAMWPA
SDWAWATA EQU   SDWAMWPA
SDWAMCKA EQU   SDWAMWPA
@NM00048 EQU   SDWAMWPA
SDWAKEYA EQU   SDWAMWPA
SDWAEXTA EQU   SDWACMKA
SDWAIOA  EQU   SDWACMKA
SDWACMPC EQU   SDWAABCC+1
@NM00047 EQU   SDWACMPF
SDWASTCC EQU   SDWACMPF
@NM00046 EQU   SDWACMPF
SDWASTEP EQU   SDWACMPF
SDWAREQ  EQU   SDWACMPF
FRRSPARM EQU   FRRSENTR+8
@NM00045 EQU   FRRSFLGS+1
@NM00044 EQU   FRRSFLG1
FRRSNEST EQU   FRRSFLG1
FRRSRCUR EQU   FRRSFLG1
FRRSFRRA EQU   FRRSENTR
FRRSENTS EQU   FRRS+84
FRRSRTMW EQU   FRRS+16
FRRSELEN EQU   FRRSHEAD+8
FRRSLAST EQU   FRRSHEAD+4
FRRSEMP  EQU   FRRSHEAD
MCQESRB  EQU   MCQE+44
@NM00043 EQU   MCQE+42
@NM00042 EQU   MCQEFLGS
MCQEACT  EQU   MCQEFLGS
MCQESCHE EQU   MCQE+40
MCQETECB EQU   MCQE+36
MCQEMCCL EQU   MCQE+28
MCQEMCEE EQU   MCQE+24
MCQEMCQE EQU   MCQE+20
MCQENAME EQU   MCQE+8
MCQEIDEN EQU   MCQE
@NM00038 EQU   UCB
WSACCCH  EQU   WSAC+52
WSACRRSA EQU   WSAC+48
WSACREST EQU   WSAC+44
WSACRSTI EQU   WSAC+40
WSACABTM EQU   WSAC+36
WSACMF1  EQU   WSAC+32
WSACEDS0 EQU   WSAC+28
WSACIOS  EQU   WSAC+24
WSACRTMK EQU   WSAC+20
WSACACR  EQU   WSAC+16
WSACTIME EQU   WSAC+12
WSACOPTM EQU   WSAC+8
WSACCWSA EQU   WSAC
WSAGSCHE EQU   WSAG+40
WSAGREST EQU   WSAG+36
WSAGNQDQ EQU   WSAG+32
WSAGMEMT EQU   WSAG+28
WSAGOPTM EQU   WSAG+24
WSAGSTAT EQU   WSAG+20
WSAGEMS0 EQU   WSAG+16
WSAGSSRS EQU   WSAG+12
WSAGRSM  EQU   WSAG+8
WSAGGMFM EQU   WSAG+4
WSAGPGIO EQU   WSAG
MCHEADTR EQU   MCHEAD+48
MCHEADRD EQU   MCHEAD+44
MCHFRRAD EQU   MCHEAD+40
MCHMCER  EQU   MCHEAD+36
@NM00012 EQU   MCHFLGS+1
@NM00011 EQU   MCHFLG1
MCHTERM  EQU   MCHFLG1
MCHACT   EQU   MCHFLG1
MCHDIS   EQU   MCHEAD+20
MCHCTL   EQU   MCHEAD+16
MCHCNT   EQU   MCHEAD+12
MCHCUR   EQU   MCHEAD+8
MCHIDEN  EQU   MCHEAD
LCCAR125 EQU   LCCA+964
LCCAR124 EQU   LCCA+960
LCCASGPR EQU   LCCA+896
LCCADRT2 EQU   LCCA+888
LCCADRT1 EQU   LCCA+880
LCCAR103 EQU   LCCA+876
LCCAESS2 EQU   LCCA+872
LCCASPLJ EQU   LCCA+868
LCCASMQJ EQU   LCCA+864
LCCAIRT  EQU   LCCA+736
LCCARV90 EQU   LCCA+732
LCCARV89 EQU   LCCA+728
LCCAPGTA EQU   LCCASRBF+2
LCCASAFN EQU   LCCASRBF
LCCAECSA EQU   LCCA+716
LCCAICR0 EQU   LCCA+712
LCCALWTM EQU   LCCA+704
LCCARV78 EQU   LCCA+700
LCCAPINV EQU   LCCA+696
LCCARV88 EQU   LCCA+695
LCCARV87 EQU   LCCALKFG
LCCARV86 EQU   LCCALKFG
LCCARV85 EQU   LCCALKFG
LCCARV84 EQU   LCCALKFG
LCCALKRD EQU   LCCALKFG
LCCALKAQ EQU   LCCALKFG
LCCALKSA EQU   LCCALKFG
LCCALKDP EQU   LCCALKFG
LCCACRST EQU   LCCACREX
LCCACRDP EQU   LCCACREX
LCCACRLM EQU   LCCACREX
LCCACRIN EQU   LCCACREX
LCCACRRT EQU   LCCACREX
LCCACRLE EQU   LCCACREX
LCCACRRM EQU   LCCACREX
LCCACREF EQU   LCCACREX
LCCAVARY EQU   LCCACRFL
LCCARV73 EQU   LCCACRFL
LCCARV72 EQU   LCCACRFL
LCCARV71 EQU   LCCACRFL
LCCARV70 EQU   LCCACRFL
LCCARV69 EQU   LCCACRFL
LCCACLMS EQU   LCCACRFL
LCCACRTM EQU   LCCACRFL
LCCALCR0 EQU   LCCA+688
LCCACRLC EQU   LCCA+684
LCCARCPU EQU   LCCA+680
LCCADCPU EQU   LCCA+676
LCCASRBJ EQU   LCCA+672
LCCADSSR EQU   LCCA+668
LCCADSSC EQU   LCCA+660
LCCADSS3 EQU   LCCA+648
LCCADSS2 EQU   LCCA+636
LCCADSS1 EQU   LCCA+624
LCCAWTIM EQU   LCCA+616
LCCAITOD EQU   LCCA+608
LCCADTOD EQU   LCCA+600
LCCASTOD EQU   LCCA+592
LCCARIR5 EQU   LCCASPSA+44
LCCARIR4 EQU   LCCASPSA+40
LCCARIR3 EQU   LCCASPSA+36
LCCARIR2 EQU   LCCASPSA+32
LCCARPR5 EQU   LCCASPSA+28
LCCARPR4 EQU   LCCASPSA+24
LCCARPR3 EQU   LCCASPSA+20
LCCARPR2 EQU   LCCASPSA+16
LCCADSR5 EQU   LCCASPSA+12
LCCADSR4 EQU   LCCASPSA+8
LCCADSR3 EQU   LCCASPSA+4
LCCADSR2 EQU   LCCASPSA
LCCARV68 EQU   LCCA+543
LCCAPSMK EQU   LCCA+542
LCCARV67 EQU   LCCADSF2
LCCARV66 EQU   LCCADSF2
LCCARV65 EQU   LCCADSF2
LCCARV64 EQU   LCCADSF2
LCCADSRW EQU   LCCADSF2
LCCADSPL EQU   LCCADSF2
LCCAGSRB EQU   LCCADSF2
LCCASRBM EQU   LCCADSF2
LCCARV61 EQU   LCCADSF1
LCCARV60 EQU   LCCADSF1
LCCARV59 EQU   LCCADSF1
LCCARV58 EQU   LCCADSF1
LCCATIMR EQU   LCCADSF1
LCCADSS  EQU   LCCADSF1
LCCAVCPU EQU   LCCADSF1
LCCAACR  EQU   LCCADSF1
LCCAASCP EQU   LCCA+532
LCCAESSA EQU   LCCA+528
LCCARV55 EQU   LCCASPN4
LCCARV54 EQU   LCCASPN4
LCCARV53 EQU   LCCASPN4
LCCARV52 EQU   LCCASPN4
LCCARV51 EQU   LCCASPN4
LCCARV50 EQU   LCCASPN4
LCCARV49 EQU   LCCASPN4
LCCARV48 EQU   LCCASPN4
LCCARV47 EQU   LCCASPN3
LCCARV46 EQU   LCCASPN3
LCCARV45 EQU   LCCASPN3
LCCARV44 EQU   LCCASPN3
LCCARV43 EQU   LCCASPN3
LCCARV42 EQU   LCCASPN3
LCCARV41 EQU   LCCASPN3
LCCARV40 EQU   LCCASPN3
LCCARV39 EQU   LCCASPN2
LCCARV38 EQU   LCCASPN2
LCCARV37 EQU   LCCASPN2
LCCARV36 EQU   LCCASPN2
LCCARV35 EQU   LCCASPN2
LCCARV34 EQU   LCCASPN2
LCCARV33 EQU   LCCASPN2
LCCARV32 EQU   LCCASPN2
LCCARV31 EQU   LCCASPN1
LCCARV30 EQU   LCCASPN1
LCCAMFIO EQU   LCCASPN1
LCCARSTR EQU   LCCASPN1
LCCATSPN EQU   LCCASPN1
LCCALOCK EQU   LCCASPN1
LCCASIGP EQU   LCCASPN1
LCCAPTLB EQU   LCCASPN1
LCCARV27 EQU   LCCAIHR4
LCCARV26 EQU   LCCAIHR4
LCCARV25 EQU   LCCAIHR4
LCCARV24 EQU   LCCAIHR4
LCCARV23 EQU   LCCAIHR4
LCCARV22 EQU   LCCAIHR4
LCCARV21 EQU   LCCAIHR4
LCCARV20 EQU   LCCAIHR4
LCCARV19 EQU   LCCAIHR3
LCCARV18 EQU   LCCAIHR3
LCCARV17 EQU   LCCAIHR3
LCCARV16 EQU   LCCAIHR3
LCCARV15 EQU   LCCAIHR3
LCCARV14 EQU   LCCAIHR3
LCCARV13 EQU   LCCAIHR3
LCCARV12 EQU   LCCAIHR3
LCCARV11 EQU   LCCAIHR2
LCCARV10 EQU   LCCAIHR2
LCCARV09 EQU   LCCAIHR2
LCCARV08 EQU   LCCAIHR2
LCCARV07 EQU   LCCAIHR2
LCCARV06 EQU   LCCAIHR2
LCCARV05 EQU   LCCAIHR2
LCCARV04 EQU   LCCAIHR2
LCCARV03 EQU   LCCAIHR1
LCCARV02 EQU   LCCAIHR1
LCCARV01 EQU   LCCAIHR1
LCCAPPIE EQU   LCCAIHR1
LCCAPSG1 EQU   LCCAIHR1
LCCAPDAT EQU   LCCAIHR1
LCCAXRC2 EQU   LCCAIHR1
LCCAXRC1 EQU   LCCAIHR1
LCCAIOPS EQU   LCCA+512
LCCAGPGR EQU   LCCA+448
LCCAR133 EQU   LCCA+444
LCCAR132 EQU   LCCA+440
LCCAR131 EQU   LCCA+436
LCCAR130 EQU   LCCA+432
LCCAR129 EQU   LCCA+428
LCCAR128 EQU   LCCA+424
LCCAR127 EQU   LCCA+420
LCCAR126 EQU   LCCA+416
LCCARSGR EQU   LCCA+352
LCCAXGR3 EQU   LCCA+288
LCCAXGR2 EQU   LCCA+224
LCCAXGR1 EQU   LCCA+160
LCCACR0  EQU   LCCA+156
LCCAMCR1 EQU   LCCA+152
LCCAPVAD EQU   LCCA+148
LCCAPINT EQU   LCCA+144
LCCAPPSW EQU   LCCA+136
LCCAPGR1 EQU   LCCA+8
LCCARV77 EQU   LCCA+6
LCCACPUA EQU   LCCA+4
LCCALCCA EQU   LCCA
IOSEND   EQU   IOSB+108
IOSSKR   EQU   IOSEEKA+7
IOSSKH2  EQU   IOSSKHH+1
IOSSKH1  EQU   IOSSKHH
IOSSKCC  EQU   IOSCCHH
IOSSKBB  EQU   IOSEEKA+1
IOSSKM   EQU   IOSEEKA
IOSEEK   EQU   IOSB+92
IOSMDM   EQU   IOSB+91
IOSMDB   EQU   IOSB+90
IOSCKEY7 EQU   IOSCKEY
IOSCKEY6 EQU   IOSCKEY
IOSCKEY5 EQU   IOSCKEY
IOSCKEY4 EQU   IOSCKEY
@NM00007 EQU   IOSCKEY
IOSCUDEV EQU   IOSPATH+1
@NM00006 EQU   IOSCHN
IOSPATH3 EQU   IOSCHN
IOSPATH2 EQU   IOSCHN
IOSEXP   EQU   IOSCHN
IOSGDP   EQU   IOSCHN
IOSRSS1B EQU   IOSB+84
IOSVST   EQU   IOSB+76
IOSDIE   EQU   IOSB+68
IOSABN   EQU   IOSB+64
IOSNRM   EQU   IOSB+60
IOSPCI   EQU   IOSB+56
IOSPCHN  EQU   IOSB+48
IOSSNS   EQU   IOSB+42
IOSAPMSK EQU   IOSB+40
IOSRES4A EQU   IOSB+36
IOSUSE   EQU   IOSB+32
IOSCSWRC EQU   IOSCSW+5
IOSTSB   EQU   IOSTATUS+1
IOSTSA   EQU   IOSTATUS
IOSCSWCA EQU   IOSCSW
IOSUCB   EQU   IOSB+16
IOSOPT2X EQU   IOSOPT2
IOSHTP   EQU   IOSOPT2
IOSRELSE EQU   IOSOPT
IOSAPR   EQU   IOSOPT
IOSTSLL  EQU   IOSOPT
IOSNERP  EQU   IOSOPT
IOSPSLL  EQU   IOSOPT
IOSQISCE EQU   IOSOPT
IOSDEP   EQU   IOSOPT
IOSBYP   EQU   IOSOPT
IOSCOD   EQU   IOSB+13
IOSPKY7  EQU   IOSPKEY
IOSPGDPX EQU   IOSPKEY
IOSIDR   EQU   IOSPKEY
IOSLCL   EQU   IOSPKEY
@NM00004 EQU   IOSPKEY
IOSPGAD  EQU   IOSB+8
IOSPRLVL EQU   IOSB+5
IOSPROC  EQU   IOSB+3
IOSFLC7  EQU   IOSFLC
IOSFLC6  EQU   IOSFLC
IOSGLC5  EQU   IOSFLC
IOSFLC4  EQU   IOSFLC
IOSTP    EQU   IOSFLC
IOSCC3WE EQU   IOSFLC
IOSVERIF EQU   IOSDVALT
IOSDVMNT EQU   IOSFLC
IOSLOG   EQU   IOSFLB
IOSBDCST EQU   IOSFLB
IOSMSG   EQU   IOSFLB
IOSFLB4  EQU   IOSFLB
IOSFLB3  EQU   IOSFLB
IOSFLB2  EQU   IOSFLB
IOSFLB1  EQU   IOSFLB
IOSDIESE EQU   IOSFLB
IOSIOSB  EQU   IOSFLA
IOSDOM   EQU   IOSFLA
IOSEX    EQU   IOSFLA
IOSSMDB  EQU   IOSFLA
IOSSMDA  EQU   IOSFLA
IOSERR   EQU   IOSFLA
IOSCCHN  EQU   IOSACHN
IOSDCHN  EQU   IOSACHN
ASVTEND  EQU   ASVT+528
ASVTAVAL EQU   ASVTENTY
ASVTAVAI EQU   ASVTFRST
ASVTRS00 EQU   ASVT+520
ASVTMAXU EQU   ASVT+516
ASVTASVT EQU   ASVT+512
ASVTBEGN EQU   ASVT+512
@NM00003 EQU   ASVT
@NM00002 EQU   SRB+40
@NM00001 EQU   SRB+38
*                                      END UNREFERENCED COMPONENTS
@RT00246 EQU   AHLASCB1
@RT00248 EQU   AHLPSCM2
@RT00258 EQU   NOJBNM1
@RT00320 EQU   AHLASCB2
@RT00322 EQU   AHLPSCM2
@RT00332 EQU   NOJBNM2
@RC00371 EQU   @RC00370
@RC00397 EQU   @RC00396
@RF00416 EQU   @EL00003
@RC00524 EQU   FRRGEN
@RT00569 EQU   JOBERR1
@RF00571 EQU   UIOCHK
@ENDDATA EQU   *
         END   AHLTSIO,(C'PLS1241',0800,78217)
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY AHLTSIO('ZP60011')
++MOD(AMDSYS00) DISTLIB(AOS12).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'AMDSYS00 - EDIT FORMATTING ROUTINE, SIO-IO RECORDS     *
                        '
*
*  MODIFIED BY GREG PRICE 3RD MARCH 2003 FOR USERMOD ZP60011
*           TO DISPLAY CCW CONTENTS PRESENT IN SIO RECORDS
*
AMDSYS00 CSECT ,                                                01S0001
@MAINENT DS    0H                                               01S0001
         USING *,R15                                            01S0001
         B     @PROLOG                                          01S0001
         DC    AL1(32)                                          ZP60011
         DC    C'AMDSYS00 78219  '                              01S0001
         DC    C'UZ86400 '
         DC    C'ZP60011 '                                      ZP60011
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)                                  01S0001
         BALR  R10,0                                            01S0001
@PSTART  DS    0H                                               01S0001
         USING @PSTART,R10                                      01S0001
*/********************************************************************/
*/*                                                                  */
*/*                             AMDSYS00                             */
*/*                                                                  */
*/********************************************************************/
* R11=R1;                          /* MOVE PARM ADDRESS TO R11       */
         LR    R11,R1                                           01S0039
* R12=WKAREADD;                    /* R12 IS BASE FOR 200 BYTE WORK  */
         L     R12,WKAREADD(,R11)                               01S0040
*                                  /* AREA USED FOR DSECT            */
* GEN SETS(R13,R15);               /* SET UP USING, SAVE AREA REG    */
         USING @DATD,R12
         ST    R13,@SA00001+4
         LA    R15,@SA00001
         ST    R15,8(R13)
         LR    R13,R15
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT CHECKS THE INPUT RECORD TO SEE IF IT SHOULD BE     */
*/*  PRINTED OR NOT, AND IF SO, WHETHER THE RECORD IS TO BE DUMPED   */
*/*  IN HEX, FORMATTED IN 2 LINES, OR FORMATTED IN 1 LINE. THE       */
*/*  APPROPRIATE RETURN CODE IS SET UP FOR EACH CASE.                */
*/*                                                                  */
*/********************************************************************/
* IF RECERR='EEEE'X THEN           /* IF RECORD WAS IN ERROR,        */
         L     R14,DATAPTR(,R11)                                02S0042
         CLC   RECERR(2,R14),@CB00622                           02S0042
         BNE   @RF00042                                         02S0042
*   RC=HEXDUMP;                    /* DUMP IT IN HEX                 */
         LA    RC,12                                            02S0043
* ELSE                                                          02S0044
*   DO;                                                         02S0044
         B     @RC00042                                         02S0044
@RF00042 DS    0H                                               02S0045
*     IF TIME2SW=ON   RECEID=IMDMIO1 THEN /* IF 2ND TIME THRU, OR    */
         TM    SWITCHES,TIME2SW                                 02S0045
         BO    @RT00045                                         02S0045
         L     R14,EIDPTR(,R11)                                 02S0045
         CLC   RECEID(2,R14),@CB00208                           02S0045
         BNE   @RF00045                                         02S0045
@RT00045 DS    0H                                               02S0046
*                                  /* IF UIO RECORD, THEN ONLY       */
*       RC=PRNTLAST;               /* 1 LINE NEEDED                  */
         LA    RC,4                                             02S0046
*     ELSE                                                      02S0047
*       DO;                                                     02S0047
         B     @RC00045                                         02S0047
@RF00045 DS    0H                                               02S0048
*         DO CTR=1 TO DIM(MYEIDTAB) WHILE LOOPDONE=OFF;         02S0048
         LA    CTR,1                                            02S0048
@DL00048 TM    SWITCHES,LOOPDONE                                02S0048
         BNZ   @DC00048                                         02S0048
*           IF RECEID=MYEIDTAB(CTR) THEN /* IF 2 LINE EID FOUND,     */
         L     R14,EIDPTR(,R11)                                 02S0049
         LR    R8,CTR                                           02S0049
         ALR   R8,R8                                            02S0049
         LA    R7,MYEIDTAB-2(R8)                                02S0049
         CLC   RECEID(2,R14),0(R7)                              02S0049
         BNE   @RF00049                                         02S0049
*             LOOPDONE=ON;         /* FORCE LOOP EXIT                */
         OI    SWITCHES,LOOPDONE                                02S0050
*         END;                                                  02S0051
@RF00049 AL    CTR,@CF00044                                     02S0051
         C     CTR,@CF00098                                     02S0051
         BNH   @DL00048                                         02S0051
@DC00048 DS    0H                                               02S0052
*         IF LOOPDONE=ON THEN      /* IF EXIT WAS FORCED, THEN       */
         TM    SWITCHES,LOOPDONE                                02S0052
         BNO   @RF00052                                         02S0052
*           DO;                    /* EID WAS FOUND                  */
*             LOOPDONE=OFF;        /* RESET SWITCH, AND INDICATE     */
         NI    SWITCHES,255-LOOPDONE                            02S0054
*             RC=PRNTFRST;         /* LINE 1 OF 2 TO BE PROCESSED    */
         SLR   RC,RC                                            02S0055
*           END;                                                02S0056
*         ELSE                     /* NO EID FOUND,                  */
*           RC=RECSKIP;            /* SKIP RECORD                    */
         B     @RC00052                                         02S0057
@RF00052 LA    RC,8                                             02S0057
*       END;                                                    02S0058
@RC00052 DS    0H                                               02S0059
*   END;                                                        02S0059
@RC00045 DS    0H                                               02S0060
* IF RC=PRNTFRST   RC=PRNTLAST THEN  /* IF RECORD IS FORMATTED BY    */
@RC00042 LTR   RC,RC                                            01S0060
         BZ    @RT00060                                         01S0060
         C     RC,@CF00098                                      01S0060
         BNE   @RF00060                                         01S0060
@RT00060 DS    0H                                               01S0061
*                                  /* THIS ROUTINE,                  */
*   DO;                            /* THEN FORMAT IT                 */
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT FORMATS THE INPUT RECORD AFTER DECIDING WHICH      */
*/*  FORMAT TO USE.                                                  */
*/*                                                                  */
*/********************************************************************/
* R4=SNAPPARM->ADPLBUF;            /* LOAD ADDR OF LOCAL BUFFER      */
         L     R14,SNAPPARM(,R11)                               02S0062
         L     R4,ADPLBUF(,R14)                                 02S0062
* IF RECEID=IMDMIO1 THEN           /* IF UIO RECORD, FORMAT IT       */
         L     R14,EIDPTR(,R11)                                 02S0063
         CLC   RECEID(2,R14),@CB00208                           02S0063
         BNE   @RF00063                                         02S0063
*   DO;                                                         02S0064
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT FORMATS THE UIO RECORD INTO 1 OUTPUT LINE.         */
*/*                                                                  */
*/********************************************************************/
* CALL CHECKDEV (UIODEV,IO);       /* SEE IF DEVICE IS BEING TRACED  */
         L     R14,DATAPTR(,R11)                                03S0065
         LA    R14,UIODEV(,R14)                                 03S0065
         ST    R14,@AL00001                                     03S0065
         LA    R14,IO(,AEDITCBR)                                03S0065
         ST    R14,@AL00001+4                                   03S0065
         LA    R1,@AL00001                                      03S0065
         BAL   R14,CHECKDEV                                     03S0065
* IF R15=0 THEN                    /* IF DEVICE IS BEING TRACED,     */
         LTR   R15,R15                                          03S0066
         BNZ   @RF00066                                         03S0066
*   DO;                            /* FORMAT THE RECORD              */
*     R0=ADDR(UIOPTRN);            /* SET UP PATTERN ADDRESS         */
         LA    R0,UIOPTRN                                       03S0068
*     R1=SNAPPARM;                 /* POINT TO PARM FOR FORMAT       */
         L     R1,SNAPPARM(,R11)                                03S0069
*     ADPLFMT1=ADDR(UIOLBLS);      /* INITIALIZE LABEL POINTER       */
         LA    R14,UIOLBLS                                      03S0070
         ST    R14,ADPLFMT1(,R1)                                03S0070
*     ADPLFMT2=DATAPTR;            /* INITIALIZE DATA POINTER        */
         L     R14,DATAPTR(,R11)                                03S0071
         ST    R14,ADPLFMT2(,R1)                                03S0071
*     CALL FRMT;                   /* FORMAT THE LINE                */
         L     R15,FRMADDR(,R11)                                03S0072
         BALR  R14,R15                                          03S0072
*   END;                                                        03S0073
* ELSE                             /* DEVICE IS NOT BEING TRACED     */
*   RC=RECSKIP;                    /* SKIP RECORD                    */
         B     @RC00066                                         03S0074
@RF00066 LA    RC,8                                             03S0074
*   END;                                                        02S0075
* ELSE                                                          02S0076
*   DO;                                                         02S0076
         B     @RC00063                                         02S0076
@RF00063 DS    0H                                               02S0077
*     IF RECEID=IMDMSIO THEN       /* IF SIO RECORD, FORMAT 1 LINE   */
         L     R14,EIDPTR(,R11)                                 02S0077
         CLC   RECEID(2,R14),@CB00212                           02S0077
         BNE   @RF00077                                         02S0077
*       DO;                                                     02S0078
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT FORMATS A LINE OF THE SIO RECORD.                  */
*/*                                                                  */
*/********************************************************************/
* IF TIME2SW=OFF THEN              /* IF THIS IS THE FIRST TIME FOR  */
         TM    SWITCHES,TIME2SW                                 03S0079
         BNZ   @RF00079                                         03S0079
*                                  /* THIS RECORD,                   */
*   DO;                            /* FORMAT THE FIRST LINE          */
*     CALL CHECKDEV (SIODEV,SIF);  /* ARE WE FORMATTING THIS DEVICE  */
         L     R14,DATAPTR(,R11)                                03S0081
         LA    R14,SIODEV(,R14)                                 03S0081
         ST    R14,@AL00001                                     03S0081
         LA    R14,SIF(,AEDITCBR)                               03S0081
         ST    R14,@AL00001+4                                   03S0081
         LA    R1,@AL00001                                      03S0081
         BAL   R14,CHECKDEV                                     03S0081
*     IF R15=0 THEN                                             03S0082
         LTR   R15,R15                                          03S0082
         BNZ   @RF00082                                         03S0082
*       DO;                        /* YES, FORMAT LINE ONE           */
*         R1=SNAPPARM;             /* SET UP PARM FOR FORMAT         */
         L     R1,SNAPPARM(,R11)                                03S0084
*         R0=ADDR(SIOPTRN1);       /* POINT TO FIRST PATTERN         */
         LA    R0,SIOPTRN1                                      03S0085
*         ADPLFMT1=ADDR(SIOLBLS);  /* INITIALIZE LABEL POINTER       */
         LA    R14,SIOLBLS                                      03S0086
         ST    R14,ADPLFMT1(,R1)                                03S0086
*         ADPLFMT2=DATAPTR;        /* INITIALIZE DATA POINTER        */
         L     R14,DATAPTR(,R11)                                03S0087
         ST    R14,ADPLFMT2(,R1)                                03S0087
*         CALL FRMT;               /* FORMAT PART OF LINE            */
         L     R15,FRMADDR(,R11)                                03S0088
         BALR  R14,R15                                          03S0088
*         R0=ADDR(SIOPTRN2);       /* SET UP 2ND PATTERN             */
         LA    R0,SIOPTRN2                                      03S0089
*         ADPLFMT2=ADDR(SIODEV);   /* POINT TO REST OF DATA          */
         L     R14,DATAPTR(,R11)                                03S0090
         LA    R14,SIODEV(,R14)                                 03S0090
         ST    R14,ADPLFMT2(,R1)                                03S0090
*         CALL FRMT;               /* FORMAT REST OF LINE            */
         L     R15,FRMADDR(,R11)                                03S0091
         BALR  R14,R15                                          03S0091
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT CHECKS CERTAIN FIELDS IN THE SIO RECORD FOR        */
*/*  'U/A' OR ASTERISKS. IF THE FIELD CONTAINS EITHER INDICATOR, THE */
*/*  FORMATTED LINE IN THE OUTPUT BUFFER IS MODIFIED TO REFLECT THE  */
*/*  INDICATION.                                                     */
*/*                                                                  */
*/********************************************************************/
* IF SIOASCB=UA(1:LENGTH(SIOASCB)) THEN  /* CHECK ASCB FIELD         */
         L     R14,DATAPTR(,R11)                                04S0092
         CLC   SIOASCB(4,R14),UA                                04S0092
         BNE   @RF00092                                         04S0092
*   BUFFER(16:23)=UA;                                           04S0093
         MVC   BUFFER+15(8,R4),UA                               04S0093
* ELSE                                                          04S0094
*   IF SIOASCB=ASTER(1:LENGTH(SIOASCB)) THEN                    04S0094
         B     @RC00092                                         04S0094
@RF00092 L     R14,DATAPTR(,R11)                                04S0094
         CLC   SIOASCB(4,R14),ASTER                             04S0094
         BNE   @RF00094                                         04S0094
*     BUFFER(16:23)=ASTER;                                      04S0095
         MVC   BUFFER+15(8,R4),ASTER                            04S0095
* IF SIOCPA=UA THEN                /* CHECK CPA FIELD                */
@RF00094 DS    0H                                               04S0096
@RC00092 L     R14,DATAPTR(,R11)                                04S0096
         CLC   SIOCPA(8,R14),UA                                 04S0096
         BNE   @RF00096                                         04S0096
*   DO;                                                         04S0097
*     BUFFER(59:66)=UA;                                         04S0098
         MVC   BUFFER+58(8,R4),UA                               04S0098
*     BUFFER(68:75)='';                                         04S0099
         MVI   BUFFER+67(R4),C' '                               04S0099
         MVC   BUFFER+68(7,R4),BUFFER+67(R4)                    04S0099
*   END;                                                        04S0100
* ELSE                                                          04S0101
*   IF SIOCPA=ASTER THEN                                        04S0101
         B     @RC00096                                         04S0101
@RF00096 L     R14,DATAPTR(,R11)                                04S0101
         CLC   SIOCPA(8,R14),ASTER                              04S0101
         BNE   @RF00101                                         04S0101
*     DO;                                                       04S0102
*       BUFFER(59:66)=ASTER;                                    04S0103
         MVC   BUFFER+58(8,R4),ASTER                            04S0103
*       BUFFER(68:75)=ASTER;                                    04S0104
         MVC   BUFFER+67(8,R4),ASTER                            04S0104
*     END;                                                      04S0105
* IF SIODSID=UA(1:LENGTH(SIODSID)) THEN  /* CHECK DSID FIELD         */
@RF00101 DS    0H                                               04S0106
@RC00096 L     R14,DATAPTR(,R11)                                04S0106
         CLC   SIODSID(4,R14),UA                                04S0106
         BNE   @RF00106                                         04S0106
*   BUFFER(97:104)=UA;                                          04S0107
         MVC   BUFFER+96(8,R4),UA                               04S0107
* ELSE                                                          04S0108
*   IF SIODSID=ASTER(1:LENGTH(SIODSID)) THEN                    04S0108
         B     @RC00106                                         04S0108
@RF00106 L     R14,DATAPTR(,R11)                                04S0108
         CLC   SIODSID(4,R14),ASTER                             04S0108
         BNE   @RF00108                                         04S0108
*     BUFFER(97:104)=ASTER;                                     04S0109
         MVC   BUFFER+96(8,R4),ASTER                            04S0109
*         TIME2SW=ON;              /* SET SWITCH TO FORMAT LINE 2    */
@RF00108 DS    0H                                               03S0110
@RC00106 OI    SWITCHES,TIME2SW                                 03S0110
*       END;                                                    03S0111
*     ELSE                         /* IF DEVICE IS NOT BEING DONE,   */
*       RC=RECSKIP;                /* SKIP THE RECORD                */
         B     @RC00082                                         03S0112
@RF00082 LA    RC,8                                             03S0112
*   END;                                                        03S0113
* ELSE                             /* IF THIS IS THE 2ND TIME FOR    */
*                                  /* THIS RECORD,                   */
*   DO;                            /* FORMAT THE SECOND LINE         */
         B     @RC00079                                         03S0114
@RF00079 DS    0H                                               03S0115
         TM    SWITCHES,TIME2SW2      FORMATTING CCWS NOW?      ZP60011
         BO    FMTCCWS                YES, GO TO IT             ZP60011
*     R1=SNAPPARM;                 /* SET UP PARM FOR FORMAT         */
         L     R1,SNAPPARM(,R11)                                03S0115
*     R0=ADDR(SIOPTRN3);           /* SET UP 3RD PATTERN     @ZA09664*/
         LA    R0,SIOPTRN3                                      03S0116
*     SIOCC=SIOCC&'30'X;           /*  CONDITION CODE        @ZA09664*/
         L     R14,DATAPTR(,R11)                                03S0117
         NI    SIOCC(R14),X'30'                                 03S0117
*     ADPLFMT2=ADDR(SIOCC);        /* POINT TO DATA                  */
         LA    R14,SIOCC(,R14)                                  03S0118
         ST    R14,ADPLFMT2(,R1)                                03S0118
*     CALL FRMT;                   /* FORMAT PART OF LINE 2          */
         L     R15,FRMADDR(,R11)                                03S0119
         BALR  R14,R15                                          03S0119
*     BLNKCC=' ';               /* BLANK OUT SECOND BYTE CC  @YM09151*/
         MVI   BLNKCC(R4),C' '                                  03S0120
*     R0=ADDR(SIOPTRN4);           /* SET UP 4TH PATTERN             */
         LA    R0,SIOPTRN4                                      03S0121
*     ADPLFMT2=ADDR(SIOSTS);       /* DATA FOR LAST PATTERN          */
         L     R14,DATAPTR(,R11)                                03S0122
         LA    R14,SIOSTS(,R14)                                 03S0122
         ST    R14,ADPLFMT2(,R1)                                03S0122
*     CALL FRMT;                   /* FORMAT REST OF RECORD          */
         L     R15,FRMADDR(,R11)                                03S0123
         BALR  R14,R15                                          03S0123
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT CHECKS CERTAIN FIELDS IN THE SIO RECORD FOR        */
*/*  'U/A' OR ASTERISKS. IF THE FIELD CONTAINS EITHER INDICATOR, THE */
*/*  FORMATTED LINE IN THE OUTPUT BUFFER IS MODIFIED TO REFLECT THE  */
*/*  INDICATION.                                                     */
*/*                                                                  */
*/********************************************************************/
* IF SIOFLAGS=UA(1:LENGTH(SIOFLAGS)) THEN  /* CHECK FLAGS FIELD      */
         L     R14,DATAPTR(,R11)                                04S0124
         CLC   SIOFLAGS(6,R14),UA                               04S0124
         BNE   @RF00124                                         04S0124
*   DO;                                                         04S0125
*     BUFFER(16:23)=UA;                                         04S0126
         MVC   BUFFER+15(8,R4),UA                               04S0126
*     BUFFER(25:28)='';                                         04S0127
         MVI   BUFFER+24(R4),C' '                               04S0127
         MVC   BUFFER+25(3,R4),BUFFER+24(R4)                    04S0127
*   END;                                                        04S0128
* ELSE                                                          04S0129
*   IF SIOFLAGS=ASTER(1:LENGTH(SIOFLAGS)) THEN                  04S0129
         B     @RC00124                                         04S0129
@RF00124 L     R14,DATAPTR(,R11)                                04S0129
         CLC   SIOFLAGS(6,R14),ASTER                            04S0129
         BNE   @RF00129                                         04S0129
*     DO;                                                       04S0130
*       BUFFER(16:23)=ASTER;                                    04S0131
         MVC   BUFFER+15(8,R4),ASTER                            04S0131
*       BUFFER(25:28)=ASTER(1:4);                               04S0132
         MVC   BUFFER+24(4,R4),ASTER                            04S0132
*     END;                                                      04S0133
* IF SIOSEEK=UA THEN               /* CHECK FLAGS FIELD              */
@RF00129 DS    0H                                               04S0134
@RC00124 L     R14,DATAPTR(,R11)                                04S0134
         CLC   SIOSEEK(8,R14),UA                                04S0134
         BNE   @RF00134                                         04S0134
*   DO;                                                         04S0135
*     BUFFER(59:66)=UA;                                         04S0136
         MVC   BUFFER+58(8,R4),UA                               04S0136
*     BUFFER(68:75)='';                                         04S0137
         MVI   BUFFER+67(R4),C' '                               04S0137
         MVC   BUFFER+68(7,R4),BUFFER+67(R4)                    04S0137
*   END;                                                        04S0138
* ELSE                                                          04S0139
*   IF SIOSEEK=ASTER THEN                                       04S0139
         B     @RC00134                                         04S0139
@RF00134 L     R14,DATAPTR(,R11)                                04S0139
         CLC   SIOSEEK(8,R14),ASTER                             04S0139
         BNE   @RF00139                                         04S0139
*     DO;                                                       04S0140
*       BUFFER(59:66)=ASTER;                                    04S0141
         MVC   BUFFER+58(8,R4),ASTER                            04S0141
*       BUFFER(68:75)=ASTER;                                    04S0142
         MVC   BUFFER+67(8,R4),ASTER                            04S0142
*     END;                                                      04S0143
* IF SIOCC=UA(1) THEN              /* CHECK CC FIELD                 */
@RF00139 DS    0H                                               04S0144
@RC00134 L     R14,DATAPTR(,R11)                                04S0144
         CLC   SIOCC(1,R14),UA                                  04S0144
         BNE   @RF00144                                         04S0144
*   BUFFER(81:83)=UA(1:3);    /*                             @YM09151*/
         MVC   BUFFER+80(3,R4),UA                               04S0145
* ELSE                                                          04S0146
*   IF SIOCC=ASTER(1) THEN                                      04S0146
         B     @RC00144                                         04S0146
@RF00144 L     R14,DATAPTR(,R11)                                04S0146
         CLC   SIOCC(1,R14),ASTER                               04S0146
         BNE   @RF00146                                         04S0146
*     BUFFER(81:82)=ASTER(1:2);       /*                     @YM09151*/
         MVC   BUFFER+80(2,R4),ASTER                            04S0147
*     TIME2SW=OFF;                 /* RESET SECOND TIME SWITCH       */
@RF00146 DS    0H                                               03S0148
@RC00144 NI    SWITCHES,255-TIME2SW                             03S0148
*   END;                                                        03S0149
*       END;                                                    02S0150
         SLR   R14,R14                CLEAR FOR INSERT          ZP60011
         ICM   R14,7,RECDLL(AEDITCBR) GET THE RECORD LENGTH     ZP60011
         S     R14,DATAPTR(,R11)                                ZP60011
         A     R14,RECPTR(,R11)       GET SIOREC LENGTH         ZP60011
         LA    R0,SIOCCWS-SIOREC      GET NEW DATA OFFSET       ZP60011
         CR    R14,R0                 CCW DATA PRESENT?         ZP60011
         BNH   @RC00077               NO, NOT LONG ENOUGH       ZP60011
         L     R14,DATAPTR(,R11)      POINT TO DATA             ZP60011
         OI    SWITCHES,TIME2SW+TIME2SW2                        ZP60011
         XC    ZEROCCW,ZEROCCW        SETUP CCW WORK AREA       ZP60011
         MVC   CCWMAX+3(1),SIOCCW#+1(R14)                       ZP60011
         CLI   CCWMAX+3,28            ENFORCE MAXIMUM           ZP60011
         BNH   *+8                    (MORE WOULD LIKELY EXCEED ZP60011
         MVI   CCWMAX+3,28             GTF RECORD SIZE LIMIT)   ZP60011
         TM    SIOCCW#(R14),X'80'     ANY WRITE DATA CAPTURED?  ZP60011
         BNO   *+10                   NO                        ZP60011
         MVC   BUFFER+50(7,R4),WRTTXT YES, LABEL THIS           ZP60011
         SLR   RC,RC                  NEED TO RETURN BACK HERE  ZP60011
*     ELSE                                                      02S0151
*       DO;                                                     02S0151
         B     @RC00077                                         02S0151
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT WAS ADDED BY USERMOD ZP60011 TO FORMAT ANY CCWS    */
*/*  CAPTURED IN THE SIO TRACE RECORD.  ONE CCW IS FORMATTED PER     */
*/*  PRINT LINE.                                                     */
*/*                                                                  */
*/********************************************************************/
FMTCCWS  L     R14,DATAPTR(,R11)      POINT TO RECORD'S DATA
         L     R15,CCWNUM
         SLL   R15,3
         LA    R14,SIOCCWS(R15,R14)   POINT TO CURRENT CCW
         MVC   WORKCCW,0(R14)         COPY IT FOR PROCESSING
         LA    R0,CCWPTRN             SET UP PATTERN ADDRESS
         L     R1,SNAPPARM(,R11)      POINT TO PARM FOR FORMAT
         LA    R14,CCWLBLS            INITIALIZE LABEL POINTER
         ST    R14,ADPLFMT1(,R1)
         LA    R14,WORKCCW            INITIALIZE DATA POINTER
         ST    R14,ADPLFMT2(,R1)
         L     R15,FRMADDR(,R11)      FORMAT THE LINE
         BALR  R14,R15
         MVC   BUFFER+13(1,R4),CCWNUM+3
         TR    BUFFER+13(1,R4),CCW#TB SHOW RELATIVE CCW NUMBER
         IC    R14,WORKCCW            GET COMMAND CODE
         LA    R15,CCTABB             POINT TO LITERAL TABLE
         TM    WORKCCW,X'03'          LOW TWO BITS ZERO?
         BNZ   CCTABOK                NO, HAVE RIGHT TABLE
         LA    R15,CCTABA             YES, POINT TO LITERAL TABLE
         SRL   R14,2                  DISCARD ZERO BITS
CCTABOK  LA    R0,3                   GET MASK
         NR    R14,R0                 JUST KEEP LOW TWO BITS
         SLL   R14,3                  GET TABLE INDEX
         AR    R15,R14                POINT TO LITERAL
         MVC   BUFFER+27(8,R4),0(R15) SHOW CCW OPERATION TYPE
         TM    WORKCCW+4,X'80'        CHAIN DATA?
         BNO   *+10                   NO
         MVC   BUFFER+59(2,R4),LITCD  YES
         TM    WORKCCW+4,X'40'        CHAIN COMMAND?
         BNO   *+10                   NO
         MVC   BUFFER+62(2,R4),LITCC  YES
         TM    WORKCCW+4,X'20'        SUPPRESS LENGTH INDICATOR?
         BNO   *+10                   NO
         MVC   BUFFER+65(3,R4),LITSLI YES
         TM    WORKCCW+4,X'10'        SUPPRESS DATA TRANSFER?
         BNO   *+10                   NO
         MVC   BUFFER+69(4,R4),LITSKP YES
         TM    WORKCCW+4,X'08'        PROGRAM CONTROLLED INTERRUPTION?
         BNO   *+10                   NO
         MVC   BUFFER+74(3,R4),LITPCI YES
         TM    WORKCCW+4,X'04'        INDIRECT ADDRESSING?
         BNO   *+10                   NO
         MVC   BUFFER+78(3,R4),LITIDA YES
         SLR   R14,R14
         ICM   R14,3,WORKCCW+6        GET BYTE COUNT
         CVD   R14,WORK
         MVC   BUFFER+108(7,R4),ED5MSK
         LA    R1,BUFFER+113(,R4)     IN CASE COUNT IS UNDER 10
         EDMK  BUFFER+108(6,R4),WORK+5
         BCTR  R1,0
         MVI   0(R1),C'('
         LA    R0,1
         A     R0,CCWNUM              INCREMENT CCW NUMBER
         ST    R0,CCWNUM
         SLR   RC,RC                  MAY NEED TO COME HERE AGAIN
         CLC   CCWNUM,CCWMAX          PROCESSED LAST CCW?
         BL    @RC00077               NO, NEED TO RETURN
         LA    RC,4                   YES, NO NEED TO RETURN
         NI    SWITCHES,255-TIME2SW-TIME2SW2
         XC    ZEROCCW,ZEROCCW        CLEAN UP WORK AREA
         B     @RC00077               RETURN TO CALLER
*/************ END OF ZP60011 CCW FORMATTING ROUTINE *****************/
@RF00077 DS    0H                                               02S0152
*         IF RECEID=IMDMIO2        /* IF IO RECORD OR                */
*            RECEID=IMDMPCI        /* PCI RECORD OR                  */
*            RECEID=IMDMEOS THEN   /* EOS RECORD, FORMAT 1 LINE      */
         L     R14,EIDPTR(,R11)                                 02S0152
         CLC   RECEID(2,R14),@CB00210                           02S0152
         BE    @RT00152                                         02S0152
         CLC   RECEID(2,R14),@CB00202                           02S0152
         BE    @RT00152                                         02S0152
         CLC   RECEID(2,R14),@CB00224                           02S0152
         BNE   @RF00152                                         02S0152
@RT00152 DS    0H                                               02S0153
*           DO;                                                 02S0153
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT FORMATS A LINE OF THE IO, PCI, AND EOS RECORDS.    */
*/*                                                                  */
*/********************************************************************/
* IF TIME2SW=OFF THEN              /* IF THIS IS THE FIRST TIME FOR  */
         TM    SWITCHES,TIME2SW                                 03S0154
         BNZ   @RF00154                                         03S0154
*                                  /* THIS RECORD,                   */
*   DO;                            /* FORMAT THE FIRST LINE          */
*     CALL CHECKDEV (IODEV,IO);    /* ARE WE FORMATTING THIS DEVICE  */
         L     R14,DATAPTR(,R11)                                03S0156
         LA    R14,IODEV(,R14)                                  03S0156
         ST    R14,@AL00001                                     03S0156
         LA    R14,IO(,AEDITCBR)                                03S0156
         ST    R14,@AL00001+4                                   03S0156
         LA    R1,@AL00001                                      03S0156
         BAL   R14,CHECKDEV                                     03S0156
*     IF R15=0 THEN                                             03S0157
         LTR   R15,R15                                          03S0157
         BNZ   @RF00157                                         03S0157
*       DO;                        /* YES, FORMAT LINE ONE           */
*         IF RECEID=IMDMIO2 THEN   /* CHECK FOR IO RECORD            */
         L     R14,EIDPTR(,R11)                                 03S0159
         CLC   RECEID(2,R14),@CB00210                           03S0159
         BNE   @RF00159                                         03S0159
*           BUFFER(1:3)='IO ';     /* PUT RECORD TYPE IN LINE        */
         MVC   BUFFER(3,R4),@CC00637                            03S0160
*         IF RECEID=IMDMPCI THEN   /* CHECK FOR PCI RECORD           */
@RF00159 L     R14,EIDPTR(,R11)                                 03S0161
         CLC   RECEID(2,R14),@CB00202                           03S0161
         BNE   @RF00161                                         03S0161
*           BUFFER(1:3)='PCI';     /* PUT RECORD TYPE IN LINE        */
         MVC   BUFFER(3,R4),@CC00638                            03S0162
*         IF RECEID=IMDMEOS THEN   /* CHECK FOR EOS RECORD           */
@RF00161 L     R14,EIDPTR(,R11)                                 03S0163
         CLC   RECEID(2,R14),@CB00224                           03S0163
         BNE   @RF00163                                         03S0163
*           BUFFER(1:3)='EOS';     /* PUT RECORD TYPE IN LINE        */
         MVC   BUFFER(3,R4),@CC00639                            03S0164
*         R1=SNAPPARM;             /* SET UP PARM FOR FORMAT         */
@RF00163 L     R1,SNAPPARM(,R11)                                03S0165
*         R0=ADDR(IOPTRN1);        /* POINT TO FIRST PATTERN         */
         LA    R0,IOPTRN1                                       03S0166
*         ADPLFMT1=ADDR(IOLBLS);   /* INITIALIZE LABEL POINTER       */
         LA    R14,IOLBLS                                       03S0167
         ST    R14,ADPLFMT1(,R1)                                03S0167
*         ADPLFMT2=DATAPTR;        /* INITIALIZE DATA POINTER        */
         L     R14,DATAPTR(,R11)                                03S0168
         ST    R14,ADPLFMT2(,R1)                                03S0168
*         CALL FRMT;               /* FORMAT PART OF LINE            */
         L     R15,FRMADDR(,R11)                                03S0169
         BALR  R14,R15                                          03S0169
*         R0=ADDR(IOPTRN2);        /* SET UP 2ND PATTERN             */
         LA    R0,IOPTRN2                                       03S0170
*         ADPLFMT2=ADDR(IODEV);    /* POINT TO REST OF DATA          */
         L     R14,DATAPTR(,R11)                                03S0171
         LA    R14,IODEV(,R14)                                  03S0171
         ST    R14,ADPLFMT2(,R1)                                03S0171
*         CALL FRMT;               /* FORMAT REST OF LINE            */
         L     R15,FRMADDR(,R11)                                03S0172
         BALR  R14,R15                                          03S0172
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT CHECKS CERTAIN FIELDS IN THE IO RECORD FOR 'N/A',  */
*/*  'U/A' OR ASTERISKS. IF THE FIELD CONTAINS ANY INDICATOR, THE    */
*/*  FORMATTED LINE IN THE OUTPUT BUFFER IS MODIFIED TO REFLECT THE  */
*/*  INDICATION.                                                     */
*/*                                                                  */
*/********************************************************************/
* IF IOASCB=UA(1:LENGTH(IOASCB)) THEN  /* CHECK ASCB FIELD           */
         L     R14,DATAPTR(,R11)                                04S0173
         CLC   IOASCB(4,R14),UA                                 04S0173
         BNE   @RF00173                                         04S0173
*   BUFFER(16:23)=UA;                                           04S0174
         MVC   BUFFER+15(8,R4),UA                               04S0174
* ELSE                                                          04S0175
*   IF IOASCB=ASTER(1:LENGTH(IOASCB)) THEN                      04S0175
         B     @RC00173                                         04S0175
@RF00173 L     R14,DATAPTR(,R11)                                04S0175
         CLC   IOASCB(4,R14),ASTER                              04S0175
         BNE   @RF00175                                         04S0175
*     BUFFER(16:23)=ASTER;                                      04S0176
         MVC   BUFFER+15(8,R4),ASTER                            04S0176
* IF IOTCB=UA(1:LENGTH(IOTCB)) THEN  /* CHECK TCB FIELD              */
@RF00175 DS    0H                                               04S0177
@RC00173 L     R14,DATAPTR(,R11)                                04S0177
         CLC   IOTCB(4,R14),UA                                  04S0177
         BNE   @RF00177                                         04S0177
*   BUFFER(82:89)=UA;                                           04S0178
         MVC   BUFFER+81(8,R4),UA                               04S0178
* ELSE                                                          04S0179
*   IF IOTCB=ASTER(1:LENGTH(IOTCB)) THEN                        04S0179
         B     @RC00177                                         04S0179
@RF00177 L     R14,DATAPTR(,R11)                                04S0179
         CLC   IOTCB(4,R14),ASTER                               04S0179
         BNE   @RF00179                                         04S0179
*     BUFFER(82:89)=ASTER;                                      04S0180
         MVC   BUFFER+81(8,R4),ASTER                            04S0180
*   ELSE                                                        04S0181
*     IF IOTCB=NA(1:LENGTH(IOTCB)) THEN                         04S0181
         B     @RC00179                                         04S0181
@RF00179 L     R14,DATAPTR(,R11)                                04S0181
         CLC   IOTCB(4,R14),NA                                  04S0181
         BNE   @RF00181                                         04S0181
*       BUFFER(82:89)=NA;                                       04S0182
         MVC   BUFFER+81(8,R4),NA                               04S0182
* IF IODSID=UA(1:LENGTH(IODSID)) THEN  /* CHECK DSID FIELD           */
@RF00181 DS    0H                                               04S0183
@RC00179 DS    0H                                               04S0183
@RC00177 L     R14,DATAPTR(,R11)                                04S0183
         CLC   IODSID(4,R14),UA                                 04S0183
         BNE   @RF00183                                         04S0183
*   BUFFER(97:104)=UA;                                          04S0184
         MVC   BUFFER+96(8,R4),UA                               04S0184
* ELSE                                                          04S0185
*   IF IODSID=ASTER(1:LENGTH(IODSID)) THEN                      04S0185
         B     @RC00183                                         04S0185
@RF00183 L     R14,DATAPTR(,R11)                                04S0185
         CLC   IODSID(4,R14),ASTER                              04S0185
         BNE   @RF00185                                         04S0185
*     BUFFER(97:104)=ASTER;                                     04S0186
         MVC   BUFFER+96(8,R4),ASTER                            04S0186
*         TIME2SW=ON;              /* SET SWITCH TO FORMAT LINE 2    */
@RF00185 DS    0H                                               03S0187
@RC00183 OI    SWITCHES,TIME2SW                                 03S0187
*       END;                                                    03S0188
*     ELSE                         /* IF DEVICE IS NOT BEING DONE,   */
*       RC=RECSKIP;                /* SKIP THE RECORD                */
         B     @RC00157                                         03S0189
@RF00157 LA    RC,8                                             03S0189
*   END;                                                        03S0190
* ELSE                             /* IF THIS IS THE 2ND TIME FOR    */
*                                  /* THIS RECORD,                   */
*   DO;                            /* FORMAT THE SECOND LINE         */
         B     @RC00154                                         03S0191
@RF00154 DS    0H                                               03S0192
*     R1=SNAPPARM;                 /* SET UP PARM FOR FORMAT         */
         L     R1,SNAPPARM(,R11)                                03S0192
*     R0=ADDR(IOPTRN3);            /* SET UP 3RD PATTERN             */
         LA    R0,IOPTRN3                                       03S0193
*     ADPLFMT2=ADDR(IOCSW);        /* POINT TO DATA                  */
         L     R14,DATAPTR(,R11)                                03S0194
         LA    R14,IOCSW(,R14)                                  03S0194
         ST    R14,ADPLFMT2(,R1)                                03S0194
*     CALL FRMT;                   /* FORMAT PART OF LINE 2          */
         L     R15,FRMADDR(,R11)                                03S0195
         BALR  R14,R15                                          03S0195
*     R0=ADDR(IOPTRN4);            /* SET UP 4TH PATTERN             */
         LA    R0,IOPTRN4                                       03S0196
*     ADPLFMT2=ADDR(IOFLA);        /* DATA FOR LAST PATTERN          */
         L     R14,DATAPTR(,R11)                                03S0197
         LA    R14,IOFLA(,R14)                                  03S0197
         ST    R14,ADPLFMT2(,R1)                                03S0197
*     CALL FRMT;                   /* FORMAT REST OF RECORD          */
         L     R15,FRMADDR(,R11)                                03S0198
         BALR  R14,R15                                          03S0198
*/********************************************************************/
*/*                                                                  */
*/*  THIS SEGMENT CHECKS CERTAIN FIELDS IN THE IO RECORD FOR 'N/A',  */
*/*  'U/A' OR ASTERISKS. IF THE FIELD CONTAINS ANY INDICATOR, THE    */
*/*  FORMATTED LINE IN THE OUTPUT BUFFER IS MODIFIED TO REFLECT THE  */
*/*  INDICATION.                                                     */
*/*                                                                  */
*/********************************************************************/
* IF IOSNS=UA(1:LENGTH(IOSNS)) THEN  /* CHECK SNS FIELD              */
         L     R14,DATAPTR(,R11)                                04S0199
         CLC   IOSNS(2,R14),UA                                  04S0199
         BNE   @RF00199                                         04S0199
*   BUFFER(41:44)=UA(1:4);                                      04S0200
         MVC   BUFFER+40(4,R4),UA                               04S0200
* ELSE                                                          04S0201
*   IF IOSNS=ASTER(1:LENGTH(IOSNS)) THEN                        04S0201
         B     @RC00199                                         04S0201
@RF00199 L     R14,DATAPTR(,R11)                                04S0201
         CLC   IOSNS(2,R14),ASTER                               04S0201
         BNE   @RF00201                                         04S0201
*     BUFFER(41:44)=ASTER(1:4);                                 04S0202
         MVC   BUFFER+40(4,R4),ASTER                            04S0202
*   ELSE                                                        04S0203
*     IF IOSNS=NA(1:LENGTH(IOSNS)) THEN                         04S0203
         B     @RC00201                                         04S0203
@RF00201 L     R14,DATAPTR(,R11)                                04S0203
         CLC   IOSNS(2,R14),NA                                  04S0203
         BNE   @RF00203                                         04S0203
*       BUFFER(41:44)=NA(1:4);                                  04S0204
         MVC   BUFFER+40(4,R4),NA                               04S0204
* IF IOCPA=UA THEN                 /* CHECK CPA FIELD                */
@RF00203 DS    0H                                               04S0205
@RC00201 DS    0H                                               04S0205
@RC00199 L     R14,DATAPTR(,R11)                                04S0205
         CLC   IOCPA(8,R14),UA                                  04S0205
         BNE   @RF00205                                         04S0205
*   DO;                                                         04S0206
*     BUFFER(59:66)=UA;                                         04S0207
         MVC   BUFFER+58(8,R4),UA                               04S0207
*     BUFFER(68:75)='';                                         04S0208
         MVI   BUFFER+67(R4),C' '                               04S0208
         MVC   BUFFER+68(7,R4),BUFFER+67(R4)                    04S0208
*   END;                                                        04S0209
* ELSE                                                          04S0210
*   IF IOCPA=ASTER THEN                                         04S0210
         B     @RC00205                                         04S0210
@RF00205 L     R14,DATAPTR(,R11)                                04S0210
         CLC   IOCPA(8,R14),ASTER                               04S0210
         BNE   @RF00210                                         04S0210
*     DO;                                                       04S0211
*       BUFFER(59:66)=ASTER;                                    04S0212
         MVC   BUFFER+58(8,R4),ASTER                            04S0212
*       BUFFER(68:75)=ASTER;                                    04S0213
         MVC   BUFFER+67(8,R4),ASTER                            04S0213
*     END;                                                      04S0214
* IF IOFLAGS(1:4)=UA(1:4) THEN     /* CHECK 1ST PART OF FLAGS FIELD  */
@RF00210 DS    0H                                               04S0215
@RC00205 L     R14,DATAPTR(,R11)                                04S0215
         CLC   IOFLAGS(4,R14),UA                                04S0215
         BNE   @RF00215                                         04S0215
*   BUFFER(82:89)=UA;                                           04S0216
         MVC   BUFFER+81(8,R4),UA                               04S0216
* ELSE                                                          04S0217
*   IF IOFLAGS(1:4)=ASTER(1:4) THEN                             04S0217
         B     @RC00215                                         04S0217
@RF00215 L     R14,DATAPTR(,R11)                                04S0217
         CLC   IOFLAGS(4,R14),ASTER                             04S0217
         BNE   @RF00217                                         04S0217
*     BUFFER(82:89)=ASTER;                                      04S0218
         MVC   BUFFER+81(8,R4),ASTER                            04S0218
* IF IOFLAGS(5:9)=ASTER(1:5) THEN  /* CHECK 2ND PART OF FLAGS FIELD  */
@RF00217 DS    0H                                               04S0219
@RC00215 L     R14,DATAPTR(,R11)                                04S0219
         CLC   IOFLAGS+4(5,R14),ASTER                           04S0219
         BNE   @RF00219                                         04S0219
*   DO;                                                         04S0220
*     BUFFER(92:99)=ASTER;                                      04S0221
         MVC   BUFFER+91(8,R4),ASTER                            04S0221
*     BUFFER(101:102)=ASTER(1:2);                               04S0222
         MVC   BUFFER+100(2,R4),ASTER                           04S0222
*   END;                                                        04S0223
*     TIME2SW=OFF;                 /* RESET SECOND TIME SWITCH       */
@RF00219 NI    SWITCHES,255-TIME2SW                             03S0224
*   END;                                                        03S0225
*           END;                                                02S0226
@RC00154 DS    0H                                               02S0227
*       END;                                                    02S0227
@RF00152 DS    0H                                               02S0228
*   END;                                                        02S0228
@RC00077 DS    0H                                               02S0229
*   END;                                                        01S0229
@RC00063 DS    0H                                               01S0230
* RETURN CODE(RC);                 /* RETURN CODE HAS BEEN SET UP    */
@RF00060 LR    R15,RC                                           01S0230
         L     R13,4(,R13)                                      01S0230
         L     R14,12(,R13)                                     01S0230
         LM    R0,R12,20(R13)                                   01S0230
         BR    R14                                              01S0230
*/********************************************************************/
*/*                                                                  */
*/*  THIS SUBROUTINE DETERMINES WHETHER THE DEVICE IN THE TRACE      */
*/*  RECORD IS TO BE TRACED OR NOT, AND PASSES A RETURN CODE BACK    */
*/*  IN REGISTER 15.                                                 */
*/*                                                                  */
*/********************************************************************/
*CHECKDEV:                                                      02S0231
* PROC (DEV,TABL);                                              02S0231
CHECKDEV STM   R14,R12,12(R13)                                  02S0231
         MVC   @PC00002(8),0(R1)                                02S0231
* DCL DEV CHAR(2);                 /* DEVICE TO BE CHECKED           */
* DCL 1 TABL,                      /* TABLE TO CHECK WITH            */
*       3 * BIT(32),                                            02S0233
*         5 ALL BIT(1),            /* ALL DEVICES ACCEPTABLE         */
*         5 SEL BIT(1),            /* DEVICES IN LIST ACCEPTABLE     */
*       3 DVCS(50) CHAR(2);        /* LIST OF ACCEPTABLE DEVICES     */
* DCL DELIM CHAR(2) CONSTANT('  '); /* LIST DELIMITER                */
* IF ALL=ON THEN                   /* IF ALL DEVICES ARE OK,         */
         L     R14,@PC00002+4                                   02S0235
         TM    ALL(R14),B'10000000'                             02S0235
         BNO   @RF00235                                         02S0235
*   R15=0;                         /* SET GOOD RETURN CODE           */
         SLR   R15,R15                                          02S0236
* ELSE                             /* IF ALL ARE NOT OK,             */
*   DO;                            /* CHECK FOR SOME OK              */
         B     @RC00235                                         02S0237
@RF00235 DS    0H                                               02S0238
*     IF SEL=ON THEN               /* IF SOME DEVICES ARE OK,        */
         L     R14,@PC00002+4                                   02S0238
         TM    SEL(R14),B'01000000'                             02S0238
         BNO   @RF00238                                         02S0238
*       DO;                        /* CHECK FOR SPECIFIC DEVICE      */
*         DO CTR=1 TO DIM(DVCS) WHILE DEVFOUND=OFF;             02S0240
         LA    CTR,1                                            02S0240
@DL00240 TM    SWITCHES,DEVFOUND                                02S0240
         BNZ   @DC00240                                         02S0240
*           IF DVCS(CTR)=DEV THEN  /* IF DEVICE MATCH FOUND,         */
         LR    R14,CTR                                          02S0241
         ALR   R14,R14                                          02S0241
         L     R8,@PC00002+4                                    02S0241
         L     R7,@PC00002                                      02S0241
         ALR   R8,R14                                           02S0241
         CLC   DVCS-2(2,R8),DEV(R7)                             02S0241
         BNE   @RF00241                                         02S0241
*             DEVFOUND=ON;         /* INDICATE SO                    */
         OI    SWITCHES,DEVFOUND                                02S0242
*           ELSE                                                02S0243
*             IF DVCS(CTR)=DELIM THEN /* IF DELIMITER REACHED,       */
         B     @RC00241                                         02S0243
@RF00241 LR    R14,CTR                                          02S0243
         ALR   R14,R14                                          02S0243
         L     R8,@PC00002+4                                    02S0243
         ALR   R8,R14                                           02S0243
         CLC   DVCS-2(2,R8),@CC00658                            02S0243
         BNE   @RF00243                                         02S0243
*               CTR=DIM(DVCS);     /* SET CTR TO LEAVE LOOP          */
         LA    CTR,50                                           02S0244
*         END;                                                  02S0245
@RF00243 DS    0H                                               02S0245
@RC00241 AL    CTR,@CF00044                                     02S0245
         C     CTR,@CF00103                                     02S0245
         BNH   @DL00240                                         02S0245
@DC00240 DS    0H                                               02S0246
*         IF DEVFOUND=ON THEN      /* IF DEVICE WAS OK TO TRACE,     */
         TM    SWITCHES,DEVFOUND                                02S0246
         BNO   @RF00246                                         02S0246
*           DO;                    /* RESET SWITCH, SET RETURN CODE  */
*             DEVFOUND=OFF;                                     02S0248
         NI    SWITCHES,255-DEVFOUND                            02S0248
*             R15=0;                                            02S0249
         SLR   R15,R15                                          02S0249
*           END;                                                02S0250
*         ELSE                     /* IF DEVICE MATCH NOT FOUND,     */
*           R15=4;                 /* SET BAD RETURN CODE            */
         B     @RC00246                                         02S0251
@RF00246 LA    R15,4                                            02S0251
*       END;                                                    02S0252
*     ELSE                         /* IF NO DEVICES ARE BEING TRACED, *
*       R15=4;                     /* SET BAD RETURN CODE            */
         B     @RC00238                                         02S0253
@RF00238 LA    R15,4                                            02S0253
*   END;                                                        02S0254
@RC00238 DS    0H                                               02S0255
* RETURN CODE(R15);                                             02S0255
@RC00235 L     R14,12(,R13)                                     02S0255
         LM    R0,R12,20(R13)                                   02S0255
         BR    R14                                              02S0255
* END CHECKDEV;                                                 02S0256
* END AMDSYS00                                                  01S0257
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSUT5  (SYSPROLG)                                        *
*/*%INCLUDE SYSUT5  (MACROS  )                                        *
*/*%INCLUDE SYSUT5  (DECLARES)                                        *
*/*%INCLUDE SYSLIB  (AMDPRTAB)                                        *
*/*%INCLUDE SYSLIB  (IHAABDPL)                                        *
*/*%INCLUDE SYSLIB  (IMDMEDIT)                                        *
*/*%INCLUDE SYSUT5  (CHECKREC)                                        *
*/*%INCLUDE SYSUT5  (FMTREC  )                                        *
*/*%INCLUDE SYSUT5  (FMTUIO  )                                        *
*/*%INCLUDE SYSUT5  (FMTSIO  )                                        *
*/*%INCLUDE SYSUT5  (CKSIOL1 )                                        *
*/*%INCLUDE SYSUT5  (CKSIOL2 )                                        *
*/*%INCLUDE SYSUT5  (FMTIO   )                                        *
*/*%INCLUDE SYSUT5  (CKIOL1  )                                        *
*/*%INCLUDE SYSUT5  (CKIOL2  )                                        *
*/*%INCLUDE SYSUT5  (CHECKDEV)                                        *
*;                                                              01S0257
@DATA    DS    0H
@DATD    DSECT
         DS    0F
@SA00001 DS    18F
@PC00002 DS    2F
@AL00001 DS    2A
AMDSYS00 CSECT
         DS    0F
@CF00044 DC    F'1'
@CF00098 DC    F'4'
@CF00103 DC    F'50'
@DATD    DSECT
         DS    0D
SWITCHES DS    BL1
TIME2SW  EQU   B'10000000'
LOOPDONE EQU   B'01000000'
DEVFOUND EQU   B'00100000'
TIME2SW2 EQU   B'00010000'                                      ZP60011
         DS    0D                                               ZP60011
ZEROCCW  DS    0CL24                       AREA TO CLEAR        ZP60011
WORKCCW  DS    D                                                ZP60011
WORK     DS    D                                                ZP60011
CCWMAX   DS    F                                                ZP60011
CCWNUM   DS    F                                                ZP60011
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
AMDSYS00 CSECT
         NOPR  ((@ENDDATD-@DATD)/201*16)
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
@CC00637 DC    C'IO '
@CC00638 DC    C'PCI'
@CC00639 DC    C'EOS'
@CC00658 DC    C'  '
@CB00202 DC    X'2100'
@CB00208 DC    X'5201'
@CB00210 DC    X'5200'
@CB00212 DC    X'5100'
@CB00224 DC    X'5101'
@CB00622 DC    X'EEEE'
UA       DC    CL8'U/A'
NA       DC    CL8'N/A'
ASTER    DC    CL8'********'
         DS    CL1
UIOPTRN  DS    CL41
         ORG   UIOPTRN
@NM00004 DC    AL1(55)
@NM00005 DC    AL1(51)
@NM00006 DC    AL1(10)
@NM00007 DC    AL1(15)
@NM00008 DC    AL4(UIOLBLS)
@NM00009 DC    AL1(21)
@NM00010 DC    AL1(33)
@NM00011 DC    AL1(25)
@NM00012 DC    AL1(29)
@NM00013 DC    AL1(53)
@NM00014 DC    AL1(55)
@NM00015 DC    AL1(35)
@NM00016 DC    AL1(40)
@NM00017 DC    AL1(21)
@NM00018 DC    AL1(99)
@NM00019 DC    AL1(50)
@NM00020 DC    AL1(58)
@NM00021 DC    AL1(20)
@NM00022 DC    AL1(3)
@NM00023 DC    AL1(0)
@NM00024 DC    AL1(67)
@NM00025 DC    AL1(21)
@NM00026 DC    AL1(35)
@NM00027 DC    AL1(77)
@NM00028 DC    AL1(81)
@NM00029 DC    AL1(20)
@NM00030 DC    AL1(3)
@NM00031 DC    AL1(0)
@NM00032 DC    AL1(90)
@NM00033 DC    AL1(21)
@NM00034 DC    AL1(33)
@NM00035 DC    AL1(0)
@NM00036 DC    AL1(5)
         DC    AL1(21,16,100,103)
@NM00037 DC    AL1(0)
         ORG   UIOPTRN+41
UIOLBLS  DC    CL26'ASCBCPUJOBNOLD PSWCSWUIOCS'
         DS    CL1
SIOPTRN1 DS    CL29
         ORG   SIOPTRN1
@NM00038 DC    AL1(23)
@NM00039 DC    XL1'33'
@NM00040 DC    AL1(10)
@NM00041 DC    AL1(15)
@NM00042 DC    AL4(SIOLBLS)
@NM00043 DC    AL1(21)
@NM00044 DC    XL1'21'
@NM00045 DC    AL1(25)
@NM00046 DC    AL1(29)
@NM00047 DC    AL1(53)
@NM00048 DC    XL1'37'
@NM00049 DC    AL1(35)
@NM00050 DC    AL1(40)
@NM00051 DC    AL1(21)
@NM00052 DC    XL1'63'
@NM00053 DC    AL1(50)
@NM00054 DC    AL1(58)
@NM00055 DC    AL1(20)
@NM00056 DC    AL1(3)
@NM00057 DC    AL1(0)
@NM00058 DC    AL1(67)
@NM00059 DC    AL1(21)
@NM00060 DC    AL1(51)
@NM00061 DC    AL1(91)
@NM00062 DC    AL1(96)
@NM00063 DC    AL1(0)
         ORG   SIOPTRN1+29
SIOLBLS  DS    CL47
         ORG   SIOLBLS
@NM00099 DC    CL22'ASCBCPUJOBNR/V CPADSID'
@NM00100 DC    CL6'SIOCAW'
@NM00101 DC    CL2'CC'
@NM00102 DC    CL17'STATSK ADDRFLGSCS'
         ORG   SIOLBLS+47
SIOPTRN2 DS    CL9
         ORG   SIOPTRN2
@NM00064 DC    AL1(21)
@NM00065 DC    AL1(33)
@NM00066 DC    AL1(0)
@NM00067 DC    AL1(5)
@NM00068 DC    AL1(21)
@NM00069 DC    AL1(35)
@NM00070 DC    AL1(77)
@NM00071 DC    AL1(81)
@NM00072 DC    AL1(0)
         ORG   SIOPTRN2+9
         DS    CL3
SIOPTRN3 DS    CL5
         ORG   SIOPTRN3
@NM00073 DC    AL1(21)
@NM00074 DC    AL1(16)
@NM00075 DC    AL1(77)
@NM00076 DC    AL1(80)
@NM00077 DC    AL1(0)
         ORG   SIOPTRN3+5
         DS    CL3
SIOPTRN4 DS    CL25
         ORG   SIOPTRN4
@NM00078 DC    AL1(21)
@NM00079 DC    AL1(49)
@NM00080 DC    AL1(35)
@NM00081 DC    AL1(40)
@NM00082 DC    AL1(21)
@NM00083 DC    AL1(99)
@NM00084 DC    AL1(50)
@NM00085 DC    AL1(58)
@NM00086 DC    AL1(20)
@NM00087 DC    AL1(3)
@NM00088 DC    AL1(0)
@NM00089 DC    AL1(67)
@NM00090 DC    AL1(21)
@NM00091 DC    AL1(51)
@NM00092 DC    AL1(10)
@NM00093 DC    AL1(15)
@NM00094 DC    AL1(20)
@NM00095 DC    AL1(1)
@NM00096 DC    AL1(0)
@NM00097 DC    AL1(24)
         DC    AL1(21,16,84,87)
@NM00098 DC    AL1(0)
         ORG   SIOPTRN4+25
         DS    CL3
IOPTRN1  DS    CL29
         ORG   IOPTRN1
@NM00103 DC    AL1(23)
@NM00104 DC    AL1(51)
@NM00105 DC    AL1(10)
@NM00106 DC    AL1(15)
@NM00107 DC    AL4(IOLBLS)
@NM00108 DC    AL1(21)
@NM00109 DC    AL1(33)
@NM00110 DC    AL1(25)
@NM00111 DC    AL1(29)
@NM00112 DC    AL1(53)
@NM00113 DC    AL1(55)
@NM00114 DC    AL1(35)
@NM00115 DC    AL1(40)
@NM00116 DC    AL1(21)
@NM00117 DC    AL1(99)
@NM00118 DC    AL1(50)
@NM00119 DC    AL1(58)
@NM00120 DC    AL1(20)
@NM00121 DC    AL1(3)
@NM00122 DC    AL1(0)
@NM00123 DC    AL1(67)
@NM00124 DC    AL1(21)
@NM00125 DC    AL1(35)
@NM00126 DC    AL1(77)
@NM00127 DC    AL1(81)
@NM00128 DC    AL1(0)
         ORG   IOPTRN1+29
IOLBLS   DS    CL43
         ORG   IOLBLS
@NM00176 DC    CL21'ASCBCPUJOBNOLD PSWTCB'
@NM00177 DC    CL4'DSID'
@NM00178 DC    CL13'CSWR/V CPASNS'
@NM00179 DC    CL5'FLGCS'
         ORG   IOLBLS+43
IOPTRN2  DS    CL9
         ORG   IOPTRN2
@NM00129 DC    AL1(20)
@NM00130 DC    AL1(1)
@NM00131 DC    AL1(0)
@NM00132 DC    AL1(5)
@NM00133 DC    AL1(21)
@NM00134 DC    AL1(51)
@NM00135 DC    AL1(91)
@NM00136 DC    AL1(96)
@NM00137 DC    AL1(0)
         ORG   IOPTRN2+9
         DS    CL3
IOPTRN3  DS    CL21
         ORG   IOPTRN3
@NM00138 DC    AL1(21)
@NM00139 DC    AL1(35)
@NM00140 DC    AL1(10)
@NM00141 DC    AL1(15)
@NM00142 DC    AL1(20)
@NM00143 DC    AL1(3)
@NM00144 DC    AL1(0)
@NM00145 DC    AL1(24)
@NM00146 DC    AL1(21)
@NM00147 DC    AL1(99)
@NM00148 DC    AL1(50)
@NM00149 DC    AL1(58)
@NM00150 DC    AL1(20)
@NM00151 DC    AL1(3)
@NM00152 DC    AL1(0)
@NM00153 DC    AL1(67)
@NM00154 DC    AL1(21)
@NM00155 DC    AL1(33)
@NM00156 DC    AL1(35)
@NM00157 DC    AL1(40)
@NM00158 DC    AL1(0)
         ORG   IOPTRN3+21
         DS    CL3
IOPTRN4  DS    CL21
         ORG   IOPTRN4
@NM00159 DC    AL1(21)
@NM00160 DC    AL1(35)
@NM00161 DC    AL1(77)
@NM00162 DC    AL1(81)
@NM00163 DC    AL1(20)
@NM00164 DC    AL1(1)
@NM00165 DC    AL1(0)
@NM00166 DC    AL1(91)
@NM00167 DC    AL1(20)
@NM00168 DC    AL1(0)
@NM00169 DC    AL1(0)
@NM00170 DC    AL1(100)
@NM00171 DC    AL1(20)
@NM00172 DC    AL1(1)
@NM00173 DC    AL1(0)
@NM00174 DC    AL1(95)
         DC    AL1(21,16,103,106)
@NM00175 DC    AL1(0)
         ORG   IOPTRN4+21
         DS    CL3
CCWPTRN  DS    0F                                               ZP60011
         DC    X'17'                   FLAGS                    ZP60011
         DC    X'C0'                   LENGTH CODES             ZP60011
         DC    AL1(10)                 LABEL OFFSET             ZP60011
         DC    AL1(24)                 DATA OFFSET              ZP60011
         DC    AL4(CCWLBLS)            LABELS ADDRESS           ZP60011
         DC    X'15'                   FLAGS                    ZP60011
         DC    X'32'                   LENGTH CODES             ZP60011
         DC    AL1(35)                 LABEL OFFSET             ZP60011
         DC    AL1(40)                 DATA OFFSET              ZP60011
         DC    X'15'                   FLAGS                    ZP60011
         DC    X'40'                   LENGTH CODES             ZP60011
         DC    AL1(50)                 LABEL OFFSET             ZP60011
         DC    AL1(56)                 DATA OFFSET              ZP60011
         DC    X'15'                   FLAGS                    ZP60011
         DC    X'30'                   LENGTH CODES             ZP60011
         DC    AL1(83)                 LABEL OFFSET             ZP60011
         DC    AL1(88)                 DATA OFFSET              ZP60011
         DC    X'15'                   FLAGS                    ZP60011
         DC    X'91'                   LENGTH CODES             ZP60011
         DC    AL1(92)                 LABEL OFFSET             ZP60011
         DC    AL1(103)                DATA OFFSET              ZP60011
         DC    4AL1(0)                 END OF LIST MARKER       ZP60011
WRTTXT   DC    C'WRT TXT'                                       ZP60011
CCWLBLS  DC    C'CCW   COMMANDADDRFLAGSFILLBYTE COUNT'          ZP60011
ED5MSK   DC    X'402020202120',C')'                             ZP60011
CCW#TB   DC    C'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'          ZP60011
LITCD    DC    C'CD'                                            ZP60011
LITCC    DC    C'CC'                                            ZP60011
LITSLI   DC    C'SLI'                                           ZP60011
LITSKP   DC    C'SKIP'                                          ZP60011
LITPCI   DC    C'PCI'                                           ZP60011
LITIDA   DC    C'IDA'                                           ZP60011
         DS    0F                                               ZP60011
CCTABA   DC    CL8'INVALID '                                    ZP60011
         DC    CL8'SENSE   '                                    ZP60011
         DC    CL8'T-I-C   '                                    ZP60011
         DC    CL8'RDBKWD  '                                    ZP60011
CCTABB   EQU   *-8                                              ZP60011
         DC    CL8'WRITE   '                                    ZP60011
         DC    CL8'READ    '                                    ZP60011
         DC    CL8'CONTROL '                                    ZP60011
MYEIDTAB DC    X'2100'
         DC    X'5200'
         DC    X'5100'
         DC    X'5101'
SYS00PCH DC    25F'0'
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
CTR      EQU   R6
RC       EQU   R6
AEDITCBR EQU   R9
AMDPRTAB EQU   0
DEBGFLGS EQU   AMDPRTAB+8
PTHFLGS1 EQU   DEBGFLGS
PTHFLGS2 EQU   DEBGFLGS+1
INRFCFGS EQU   DEBGFLGS+2
IOFLGS   EQU   DEBGFLGS+3
STARTIME EQU   AMDPRTAB+40
STOPTIME EQU   AMDPRTAB+52
NAMES    EQU   AMDPRTAB+64
ADS      EQU   AMDPRTAB+104
AADS     EQU   ADS
SIF      EQU   AMDPRTAB+124
SIOFLGS  EQU   SIF
DVADS1   EQU   AMDPRTAB+128
IO       EQU   AMDPRTAB+228
IOFLGS2  EQU   IO
DVADS2   EQU   AMDPRTAB+232
VCF      EQU   AMDPRTAB+332
SVCFLGS  EQU   VCF
USR      EQU   AMDPRTAB+368
USRFLGS  EQU   USR
USRTAB   EQU   AMDPRTAB+372
PI       EQU   AMDPRTAB+452
PIFLGS   EQU   PI
PGMNGEN  EQU   AMDPRTAB+456
GENFLAGS EQU   PGMNGEN+3
GENFLGS1 EQU   AMDPRTAB+460
RECDLL   EQU   AMDPRTAB+461
ESTARTME EQU   AMDPRTAB+500
ESTOPTME EQU   AMDPRTAB+509
ABDPL    EQU   0
ADPLFLAG EQU   ABDPL+7
ADPLBUF  EQU   ABDPL+8
ADPLFMT1 EQU   ABDPL+44
ADPLFMT2 EQU   ABDPL+48
PARMLIST EQU   0
RECPTR   EQU   PARMLIST
EIDPTR   EQU   PARMLIST+12
DATAPTR  EQU   PARMLIST+16
FRMADDR  EQU   PARMLIST+20
WKAREADD EQU   PARMLIST+24
SNAPPARM EQU   PARMLIST+28
RECEID   EQU   0
BUFFER   EQU   0
FRMT     EQU   0
DATAREC  EQU   0
ASCBPTR  EQU   DATAREC
RECERR   EQU   ASCBPTR
SIOREC   EQU   0
SIOASCB  EQU   SIOREC
SIOCPUID EQU   SIOREC+4
SIOJOBN  EQU   SIOREC+6
SIOCPA   EQU   SIOREC+14
SIODSID  EQU   SIOREC+22
SIOCC    EQU   SIOREC+26
SIODEV   EQU   SIOREC+27
SIOCAW   EQU   SIOREC+29
SIOSTS   EQU   SIOREC+33
SIOSEEK  EQU   SIOREC+35
SIOFLAGS EQU   SIOREC+43
SIOAFF   EQU   SIOFLAGS
SIOPATH  EQU   SIOFLAGS+1
SIOOPT   EQU   SIOFLAGS+3
SIOFMSK  EQU   SIOFLAGS+4
SIODVRID EQU   SIOFLAGS+5
SIOCSID  EQU   SIOREC+49
SIOCCW#  EQU   SIOREC+50                                        ZP60011
SIOCCWS  EQU   SIOREC+52                                        ZP60011
IOREC    EQU   0
IOASCB   EQU   IOREC
IOTCB    EQU   IOREC+22
IOCSW    EQU   IOREC+26
IOCPA    EQU   IOREC+34
IOSNS    EQU   IOREC+42
IODEV    EQU   IOREC+44
IODSID   EQU   IOREC+46
IOFLAGS  EQU   IOREC+50
IOFLA    EQU   IOFLAGS
UIOREC   EQU   0
UIODEV   EQU   UIOREC+30
DEV      EQU   0
TABL     EQU   0
@NM00180 EQU   TABL
ALL      EQU   @NM00180
SEL      EQU   @NM00180
DVCS     EQU   TABL+4
ABDPLPTR EQU   SNAPPARM
BLNKCC   EQU   BUFFER+81
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
UIOCSW   EQU   UIOREC+22
UIOPSW   EQU   UIOREC+14
UIOJOBN  EQU   UIOREC+6
UIOCPUID EQU   UIOREC+4
UIOASCB  EQU   UIOREC
IOCHAN   EQU   IOFLAGS+7
IOCPU    EQU   IOFLAGS+6
IOSFLS   EQU   IOFLAGS+4
IODVRID  EQU   IOFLAGS+3
IOFMSK   EQU   IOFLAGS+2
IOOPT    EQU   IOFLAGS+1
IOPSW    EQU   IOREC+14
IOJOBN   EQU   IOREC+6
IOCPUID  EQU   IOREC+4
JOBNAME  EQU   DATAREC+6
CPUID    EQU   DATAREC+4
GTFOPTR  EQU   PARMLIST+8
BUFPTR   EQU   PARMLIST+4
ADPLEXT  EQU   ABDPL+52
ADPLCOM4 EQU   ABDPL+40
ADPLCOM3 EQU   ABDPL+36
ADPLCOM2 EQU   ABDPL+32
ADPLCOM1 EQU   ABDPL+28
ADPLFRMT EQU   ABDPL+24
ADPLMEMA EQU   ABDPL+20
ADPLCVT  EQU   ABDPL+16
ADPLPRNT EQU   ABDPL+12
@NM00002 EQU   ADPLFLAG
ADPLSYTM EQU   ADPLFLAG
ADPLSNPR EQU   ADPLFLAG
ADPLSBPL EQU   ABDPL+6
ADPLASID EQU   ABDPL+4
ADPLTCB  EQU   ABDPL
RESERVED EQU   AMDPRTAB+522
CVTTZONE EQU   AMDPRTAB+518
ESPSEC   EQU   ESTOPTME+7
ESPMIN   EQU   ESTOPTME+5
ESPHR    EQU   ESTOPTME+3
ESPDAY   EQU   ESTOPTME
ESSEC    EQU   ESTARTME+7
ESMIN    EQU   ESTARTME+5
ESHR     EQU   ESTARTME+3
ESDAY    EQU   ESTARTME
ADTSBUF  EQU   AMDPRTAB+496
OFSTDATA EQU   AMDPRTAB+494
OFSTEID  EQU   AMDPRTAB+492
AFRMAD   EQU   AMDPRTAB+488
REENTWKA EQU   AMDPRTAB+484
PRFMTADD EQU   AMDPRTAB+480
AEIOCT   EQU   AMDPRTAB+476
EXITADDR EQU   AMDPRTAB+472
EXITNM   EQU   AMDPRTAB+464
@NM00001 EQU   GENFLGS1
FIRSTHSW EQU   GENFLGS1
TSFOUND  EQU   GENFLGS1
EOFINPRO EQU   GENFLGS1
TS       EQU   GENFLGS1
EOF      EQU   GENFLAGS
RR       EQU   GENFLAGS
SRM      EQU   GENFLAGS
RNIO     EQU   GENFLAGS
SYSM     EQU   GENFLAGS
SYS      EQU   GENFLAGS
DSP      EQU   GENFLAGS
EXT      EQU   GENFLAGS
PICODEA  EQU   PGMNGEN+2
PICODES  EQU   PGMNGEN
Q        EQU   PI+1
P01R     EQU   PIFLGS
SELP     EQU   PIFLGS
ALLP     EQU   PIFLGS
USRNGTAB EQU   USRTAB
O        EQU   USR+1
N01R     EQU   USRFLGS
SELU     EQU   USRFLGS
ALLU     EQU   USRFLGS
SVCNUMS  EQU   AMDPRTAB+336
M        EQU   VCF+1
L        EQU   SVCFLGS
SELV     EQU   SVCFLGS
ALLV     EQU   SVCFLGS
IODVADS  EQU   DVADS2
K01RES   EQU   IO+1
JJ       EQU   IOFLGS2
SELI     EQU   IOFLGS2
ALLI     EQU   IOFLGS2
SIODVADS EQU   DVADS1
II       EQU   SIF+1
H        EQU   SIOFLGS
NOEQU    EQU   SIOFLGS
EQUIV    EQU   SIOFLGS
SELS     EQU   SIOFLGS
ALLS     EQU   SIOFLGS
ASCBADDR EQU   AADS
JOBNAMES EQU   NAMES
DAY2     EQU   STOPTIME+10
F        EQU   STOPTIME+8
TIME2    EQU   STOPTIME
DAY      EQU   STARTIME+10
D        EQU   STARTIME+8
TIME     EQU   STARTIME
DDNAME   EQU   AMDPRTAB+32
USEREXIT EQU   AMDPRTAB+24
GTFWDPTR EQU   AMDPRTAB+20
C        EQU   DEBGFLGS+4
B        EQU   IOFLGS
GETEOF   EQU   IOFLGS
DMDFMT   EQU   INRFCFGS
EDITSTOP EQU   INRFCFGS
EXTTRC   EQU   INRFCFGS
RET      EQU   INRFCFGS
FMT      EQU   INRFCFGS
SPIE     EQU   INRFCFGS
TERM     EQU   INRFCFGS
FLMODE   EQU   INRFCFGS
REMAIN   EQU   PTHFLGS2
APP      EQU   PTHFLGS2
REXT     EQU   PTHFLGS1
FRM      EQU   PTHFLGS1
FLT      EQU   PTHFLGS1
HEX      EQU   PTHFLGS1
CON      EQU   PTHFLGS1
GET      EQU   PTHFLGS1
SCN      EQU   PTHFLGS1
ROOT     EQU   PTHFLGS1
CURREC   EQU   AMDPRTAB+4
AFMG     EQU   AMDPRTAB
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RC00066 EQU   @RC00063
@RC00079 EQU   @RC00077
@RC00157 EQU   @RC00154
@RC00246 EQU   @RC00238
@RC00082 EQU   @RC00079
@ENDDATA EQU   *
         END   AMDSYS00,(C'PLS1723',0801,78219)
/*
//*
//STEP5   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY AMDSYS00('ZP60011')
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP6   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60011)
          .
/*
//*
//STEP7CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60011)
        CHECK
        .
/*
//*
//STEP7   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60011)
        DIS(WRITE)
        .
/*
//
