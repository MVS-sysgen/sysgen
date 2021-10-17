//SYZJ2001 JOB (M096,0616,99,9999),'SYSTEMS*BRIAN',                     00010001
//             CLASS=A,                                                 00020001
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//* THIS JOB WILL INSTALL THE MAXIMUM CONDITION CODE SYSMODS
//*     NO CHANGES TO THIS JOB BEYOND THIS POINT ARE NECESSARY
//*     TO INSTALL THESE SYSMODS.
//*
//* THERE ARE 5 STEPS TO THIS JOB:
//*  STEP 1 = IEBGENER NEW MACRO TO SYS1.AMODGEN
//*  STEP 2 = IEBGENER NEW COPY MEMBER SYZYGY1A TO SYS1.HASPSRC
//*  STEP 3 = IEBGENER NEW COPY MEMBER SYZYGY1B TO SYS1.HASPSRC
//*  STEP 4 = SMP RECEIVE THE SYSMODS (SYZJ201,SYZJ202)
//*  STEP 5 = SMP APPLY THE SYSMODS (SYZJ201,SYZJ202)
//*    IF YOU HAVE ANY QUESTIONS OR PROBLEMS CONTACT:
//*
//*       BRIAN WESTERMAN
//*       SYZYGY INCORPORATED
//*       EMAIL: BRIAN_WESTERMAN@SYZYGYINC.COM
//*       PHONE: (800) 767-2244
//*
//IEBGENR1 EXEC  PGM=IEBGENER
//* THIS STEP INSERTS IEFAJCTZ INTO SYS1.AMODGEN, IT'S NEEDED BY
//*   THE HASPSSSM MOD SYZJ201
//SYSPRINT  DD SYSOUT=*
//SYSIN   DD  DUMMY
//SYSUT2  DD  DSN=SYS1.AMODGEN(IEFAJCTZ),DISP=SHR
//SYSUT1  DD  *
* %      GOTO  JCTBSL; /*                                               00010000
         MACRO                                                          00020000
 IEFAJCTZ                                                               00030000
.* 7/6/2003 THIS IS A COPY OF IEFAJCTB WITH 3 CHANGES.                  00040000
.*          THIS MACRO IS FOR THE JES2 MAX CC MOD CHANGES TO HAPSSSSM   00050000
.* CHANGE = M2344,M0697,Y01029,Y02028,M0752,Y02670,Y02668,Y02641,       00060000
.*          Y01012,Z40RPTH,Z40RPSM,@ZA44001,@Z52459            @ZA52459 00070000
.*                                                                      00080000
***************************************************************         00090000
*                                                             *         00100000
* ADD LABEL IEFAACTB TO ASSEMBLER VERSION FOR ADDRESSABILITY  *@ZA44001 00110000
* TO THE ACT                                                  *@ZA44001 00120000
*  SMB DELETION OF FIELDS                            YOLD001 11/17/71   00130000
* ADD LABEL ACTACCNT TO ASSEMBLER VERSION                      @ZA52459 00140000
*                                                             *         00150000
***************************************************************         00160000
* MACMEAN JCT MEANS JOB CONTROL TABLE                                   00170000
* MACCOMP Y-2 SC1B9/PZD83                                      @ZA04561 00180000
* MACCOMP Y-2 SCIB9/PZD83                                      @Z40RPTH 00190000
* MACSTAT Y-2 75239/Z40RPSM                                    @Z40RPSM 00200000
*********************************************************************** 00210000
**                                                                   ** 00220000
**                       JOB CONTROL TABLE                           ** 00230000
**                                                                   ** 00240000
*********************************************************************** 00250000
 DS 0D                                                                  00260000
INJMJCT  EQU   *                                                        00270000
JCTDSKAD DS    CL3                      DISK ADDRESS OF THIS JCT        00280000
JCTIDENT DS    CL1                      JCT ID = 0                      00290000
*JCTID EQU 0  (ORIGINAL)                                                00300000
ZJCTID EQU 0  (WAS JCTID IN ORIGINAL CHANGED FOR SYZYGY1)               00310000
JCTJSRNO DS    CL1                      INTERNAL JOB SERIAL NUMBER      00320000
JCTJBLBS DS    0C                       JOBLIB SWITCH  BITS 0-3         00330000
JCTJSTAT DS    1C                  JOB STATUS INDICATORS                00340000
JCTJSTPC EQU   32 BIT-2/JOB STEP CANCELLED BY CONDITION CODES           00350000
JCTABEND EQU   8                   BIT 4 - JCT ABEND BIT           HW16 00360000
INCMSTS  EQU   4                        BIT-5/JOB FAILED BIT            00370000
*                                        BIT 5 = 1 JOB FAILED           00380000
*                                        BIT 6 = 0 GO JOB               00390000
INDMCTLG EQU   2                         BIT 6 = 1 CATALOG JOB          00400000
INCMCAT  EQU   2                        BIT-6/CATALOG BIT               00410000
INCMNSET EQU   1                        BIT7/RESERVED          @ZA04561 00420000
JCTJMGPO DS    CL1                      MESSAGE CLASS                   00430000
JCTJMGLV DS    0C 4 BITS FOR MESSAGE LEVEL -SET BY IEFVJA               00440000
INCMMGL1 EQU   16                  JCL  MESSAGE LEVEL=1 BIT         I68 00450000
INCMMGL2 EQU   32                  JCL  MESSAGE LEVEL=2 BIT         I68 00460000
INCMALL  EQU   128           ALLOCATION MESSAGE LEVEL=1 BIT         I68 00470000
JCTJPRTY DS    1C                       4 BITS FOR JOB PRIORITY         00480000
*JCTJNAME DS    CL8 (ORIGINAL)          JOBNAME                         00490000
ZJCTJNME DS    CL8 (WAS JCTJNAME IN ORIGINAL FOR SYZYGY1)               00500000
JCTJTPTN DS    CL8                      T/P TERMINAL NAME               00510000
JCTPDIP  DS    CL4                      PDI CORE POINTER         Y02670 00520000
JCTGDGNT DS    CL3                      GDG NAME TABLE           Y02670 00530000
JCTJCSMF DS    CL1                      JOB CLASS SPECS FOR SMF  Y02668 00540000
*                                       TERMINATION ROUTINES     Y02668 00550000
JCTSDKAD DS    CL4                      DISK ADDRESS OF FIRST SCT       00560000
JCTJCTX  DS    CL4                      ADDRESS OF JCTX        @Z40RPSM 00570000
JCTACTAD DS    CL4                      DISK ADDRESS OF FIRST ACT       00580000
JCTSMRBA DS    CL8                      RBA SYSTEM MSG D.S.      Y02641 00590000
JCTSCT   DS    CL1                      STEP NO. OF FAILING STEP Y02641 00600000
         DS    CL1                      RESERVED                        00610000
JCTJDPCD DS    CL2                      DEPENDENCY CODE                 00620000
JCTJDPOP DS    CL2                      DEPENDENCY OPERATOR             00630000
         DS    CL28                     ROOM FOR 7 MORE DEPS            00640000
JCTRSW1  DS    C CHECKPOINT/RESTART SWITCHES                            00650000
JCTWARMS EQU   128 BIT0 - WARM START                                    00660000
JCTSTERM EQU   64  STEP TERMINATION HAS BEGUN (PCP WARM START ONLY)AACA 00670000
JCTCONTR EQU   32 BIT 2 - JOB IS ELIGIBLE FOR CONTINUE RESTART   Y02641 00680000
*                         PROCESSING                                    00690000
JCTCKFT  EQU   16 BIT  3 - CHECKPOINT TAKEN FOR THIS STEP               00700000
JCTCKPTR EQU   8 BIT   4 - CHECKPOINT RESTART (INTRA-STEP) TO BE DONE   00710000
JCTSTEPR EQU   4 BIT   5 - STEP RESTART TO BE DONE                      00720000
* BITS 6AND 7 MUST BE ZERO                                              00730000
JCTRSW2  DS    C CHECKPOINT/RESTART SWITCHES                            00740000
JCTSYSCK EQU   128 BIT 0 - SYSCHK DD STATEMENT PRESENT                  00750000
JCTNARST EQU   64 BIT 1 - JOB INELIGIBLE FOR AUTOMATIC RESTART   Y02641 00760000
JCTNORST EQU   32 BIT 2 - NO RESTART TO BE DONE                         00770000
JCTNOCKP EQU   16 BIT 3 - NO CHECKPOINTS TO BE TAKEN                    00780000
JCTRESTT EQU   8 BIT  4 - DO RESART IF NECESSARY                        00790000
JCTDSOCR EQU   4 BIT  5-  RESERVED                                M2344 00800000
JCTDSOJB EQU   2 BIT  6-  RESERVED                                M2344 00810000
*********************************************************************** 00820000
*                                                                    ** 00830000
*        IN ORDER TO IMPLEMENT MVT IT HAS BEEN NECESSARY TO          ** 00840000
*        ADD THE FOLLOWING FIELDS TO THE JCT.  TO AVOID CAUSING      ** 00850000
*        ERRORS IN THE CASE OF THE REASSEMBLING OF ALREADY EXIST-    ** 00860000
*        ING MODULES WHICH REFERENCE THESE FIELDS, THEY ARE GEN-     ** 00870000
*        ERATED HERE ONLY AS COMMENTS CARDS.  NOTE THAT DUE TO THE   ** 00880000
*        FACT THAT THIS MACRO GENERATES THE ACT IMMEDIATELY AFTER    ** 00890000
*        THE JCT, IT IS NOT POSSIBLE TO REFERENCE THESE FIELDS BY    ** 00900000
*        CODING THEM AFTER THE MACRO.  FOR NOW THEY MUST BE REF-     ** 00910000
*        ERENCED BY DISPLACEMENT (WHICH IS GIVEN BELOW), PREFER-     ** 00920000
*        ABLY THROUGH THE USE OF EQUATES AND THE SYMBOLS BELOW.      ** 00930000
*        NOTE ALSO THAT THIS MACRO IS NOT VALID FOR REFERENCING      ** 00940000
*        THE ACT UNTIL THESE NEW FIELDS HAVE ACTUALLY BEEN           ** 00950000
*        INCORPORATED.                                               ** 00960000
*                                                                    ** 00970000
*********************************************************************** 00980000
*                                                                     * 00990000
*JCTDETDA DS    CL4                     TTR OF DISENQ TABLE           * 01000000
*                                       (DISPLACEMENT = 88 (DECIMAL)  * 01010000
*JCTEQREG DS    CL2                     REGION PARAMETER (BINARY)     * 01020000
*                                       (DISPLACEMENT = 92 (DECIMAL)  * 01030000
*                                                                     * 01040000
*********************************************************************** 01050000
         DS    6C ROOM FOR THE ABOVE                                    01060000
JCTQIDNT DS    C IDENTITY OF Q FOR  JOB (MVT ONLY)                      01070000
JCTSNUMB DS    C NUMBER OF STEPS RUN (MVT ONLY)                         01080000
JCTSTIOT DS    F TTR OF COMPRESSED TIOT (MVT ONLY)                      01090000
*                IN PCP-C/R SAVE OF SCATALLY BY IEFRAPCP           AACA 01100000
JCTDEVT  DS    F DEVICE TYPE OF CHECKPOINT DATA SET                     01110000
JCTCKTTR DS    3C TTR OF JFCB FOR CHECKPOINT DATA SET                   01120000
JCTNTRK  DS    C  NUMBER OF TRACKS ON SYS1.JOBQE USED BY         PTM258 01130000
*                   THE JOB -SET AND USED BY THE INIT./TERM.     PTM258 01140000
JCTNRCKP DS    H NUMBER OF CHECKPOINTS TAKEN                            01150000
JCTVOLSQ DS    C VOLUME SEQUENCE NUMBER FOR CHECKPOINT DATA SET         01160000
JCTJSB   DS    C JOB STATUS SWITCHES                             Y02641 01170000
JCTJSBIN EQU 8   JOB ENTERED INTERPRETATION                      Y02641 01180000
JCTJSBAL EQU 4   JOB ENTERED ALLOCATION                          Y02641 01190000
JCTJSBEX EQU 2   JOB ENTERED EXECUTION                           Y02641 01200000
JCTJSBTM EQU 1   JOB ENTERED TERMINATION                         Y02641 01210000
JCTSSTR  DS    F TTR OF SCT FOR FIRST STEP TO BE RUN                    01220000
JCTSTAT2 DS    1C                  ADDITIONAL STATUS INDICATORS    O102 01230000
JCTSPSYS EQU   128  BIT 0 - =1 INDICATES SPOOLED SYSIN FOR JOB     O102 01240000
*                             SET BY IEFVDA                        O102 01250000
*                             TESTED BY IEESD575(QUEUE ALTER)-     O102 01260000
*                             MVT AND MFT ONLY                     O102 01270000
JCTADSPC EQU   64   BIT 1 - =1 INDICATES ADDRSPC=REAL            Y01029 01280000
*                             SET BY VEA AND VJA                 Y01029 01290000
JCTENDIT EQU   32                 SET BY IEFSD41Q,IEFWEXTA       A25134 01300000
*                                 TESTED BY IEFDSOWR,IEFYNIMP    A25134 01310000
*                                 JOB TERMINATION INDICATOR      A25134 01320000
JCTSWSM  EQU   16   BIT 3 - =1 INDICATES WARM START MESSAGE       M3144 01330000
*                             'INIT=JOBNAME' IS TO BE SUPPRESSED  M3144 01340000
*                             FOR THIS JOB                        M3144 01350000
*                             SET BY IEFVHH                       M3144 01360000
*                             TESTED BY IEFSD305                  M3144 01370000
JCTPERFM EQU   8              PERFORM SPEC'D ON THE JOB CARD            01380000
JCTBLP   EQU   4              0-BLP WILL BE TREATED AS NL        Y02668 01390000
*                             1-BLP WILL BE TREATED AS BYPASS    Y02668 01400000
*                             LABEL PROCESSING                   Y02668 01410000
JCTCKIDL DS    C LENGTH OF CHECKPOINT ID                                01420000
JCTCKIDT DS    CL16 CHECKPOINT IDENT                               AACA 01430000
*******************************************************************AACA 01440000
*        THE FOLLOWING SYSTEMS MGMT FACILITIES SUBFIELDS MUST      AACA 01450000
*        BEGIN ON A HALF WORD BOUNDARY                             AACA 01460000
*******************************************************************AACA 01470000
*JCTJMR  DS    CL3  (ORIGINAL)                 *** SYSTEMS ***     AACA 01480000
ZJCTJMR  DS    CL3  (WAS JCTJMR IN ORIGINAL FOR SYZYGY1)           AACA 01490000
JCTJMRD  DS    CL1  DATE DIFFERENCE STEP START-JOB START *         AACA 01500000
JCTJMROP DS    CL1  SMF OPTION SWITCHES        * MANAGEMENT        AACA 01510000
JCTJMRCL DS    CL1  SMF CANCELLATION CONTROL STATUS      *         AACA 01520000
JCTJMRTL DS    CL3  JOB TIME LIMIT             *  FACILITIES       AACA 01530000
JCTJMRSS DS    CL3  STEP START TIME OF DAY               *         AACA 01540000
JCTJMRJT DS    CL3  JOB START TIME OF DAY      *  SUBFIELDS        AACA 01550000
JCTJMRJD DS    CL3  JOB START DATE             *****************   AACA 01560000
JCTSRBT  DS    CL4  ACCUMULATED SRB TIME FOR JOB                 Y02652 01570000
         DS    CL1  RESERVED                                            01580000
JCTSSD   DS    CL3      STEP START DATE                          Y02668 01590000
JCTUSER  DS    CL7    USER ID FIELD. SET BY C/I MODULE IEFVJA  @Z40RPTH 01600000
*                     AS A RESULT OF A USER KEYWORD ON THE JOB @Z40RPTH 01610000
*                     STATEMENT.                               @Z40RPTH 01620000
JCTPRFMF DS    CL1     PERFORMANCE GROUP NUMBER                  Y02668 01630000
JCTACODE DS    CL4     ABEND CODE FIELD                          Y02641 01640000
JCTVULDP DS    CL4     POINTER TO VOLUME UNLOAD TABLE            Y02670 01650000
JCTLNGTH EQU   *-INJMJCT   JCT LENGTH                             20001 01660000
***************************************************************** 20001 01670000
**                                                                   ** 01680000
**                       ACCOUNT CONTROL TABLE                       ** 01690000
**                                                                   ** 01700000
*********************************************************************** 01710000
IEFAACTB DS    0D                                              @ZA44001 01720000
ACTDSKAD DS    CL3                 DISK ADDRESS OF THIS ACT             01730000
ACTIDENT DS    CL1                 TABLE ID  ACT = 16                   01740000
ACTID EQU 1                                                             01750000
ACTNEXT  DS    CL4                 DISK ADDRESS OF NEXT ACT             01760000
ACTPRGNM DS    CL20                PROGRAMMERS NAME                     01770000
ACTJTIME DS    CL3                 JOB RUNNING TIME                     01780000
ACTJNFLD DS    1C                  NBR OF JOB ACCOUNTING FIELDS         01790000
ACTACCNT DS    CL144          THE REST OF THE FIELDS HAVE THE  @ZA52459 01800000
*                                  FOLLOWING FORMAT FOR JOB ACCOUNTING- 01810000
*                                  1 BYTE- LENGTH OF FIELD              01820000
*                                  VARIABLE BYTES- CONTENTS OF FIELD    01830000
*                                    (REPEATED FOR N FIELDS)            01840000
*                                  STEP ACCOUNTING HAS THE FOLLOWING    01850000
*                                  FORMAT FOR EACH STEP-                01860000
*                                  3 BYTES- MAXIMUM STEP RUNNING TIME   01870000
*                                  1 BYTE- NBR OF FIELDS IN STEP        01880000
*                                  1 BYTE- LENGTH OF FIELD              01890000
*                                  VARIABLE BYTES- CONTENTS OF FIELD    01900000
*                                    (LAST 2 REPEATED N TIMES)          01910000
* THIS SECTION FORMERLY HELD THE SMB MADE OBSOLETE BY AOS/II RELEASE 2  01920000
 MEND                                                                   01930000
  */ %   JCTBSL: ;                                                      01940000
 /* MACMEAN JCT MEANS JOB CONTROL TABLE                              */ 01950000
 /* MACCOMP Y-2 SC1B9/PZD83                                          */ 01960000
 /* MACSTAT Y-2 75239/Z40RPSM                                        */ 01970000
  DECLARE 1 INJMJCT BASED(JCTPTR),  /* NAME OF TABLE */                 01980000
     4 JCTDSKAD PTR(24) BDY (WORD),  /* DISK ADDRESS OF THIS JCT */     01990000
     4 JCTIDENT CHAR(1),  /* JCT IDENTIFICATION = 0 */                  02000000
     4 JCTJSRNO PTR(8),   /* INTERNAL JOB SERIAL NUMBER */              02010000
     4 JCTJSTAT BIT(8),   /* JOB STATUS INDICATORS */                   02020000
        6 JCTJBLBS BIT(1),  /* JOBLIB SWITCH */                         02030000
        6    *     BIT(1),  /* RESERVED                     Y02670  */  02040000
        6 JCTJSTPC BIT(1),  /* STEP CANCELLED BY CONDITION CODES */     02050000
        6    *     BIT(1),  /* RESERVED                      Y02670 */  02060000
        6 JCTABEND BIT(1),  /* ABEND BIT */                             02070000
        6 INCMSTS  BIT(1),  /* JOB FAILED BIT */                        02080000
        6 INDMCTLG BIT(1),  /* CATALOG JOB */                           02090000
         8 INCMCAT  BIT(1),  /*  CATALOG BIT */                         02100000
        6 INCMNSET BIT(1),  /* RESERVED                      @ZA04561*/ 02110000
     4 JCTJMGPO CHAR(1),  /* MESSAGE CLASS */                           02120000
     4 JCTJBYTE BIT(8),    /* MSGLEVEL & PRIORITY */                    02130000
        6 JCTJMGLV BIT(4),  /* MSGLEVEL - SET BY IEFVJA */              02140000
            8 INCMALL  BIT(1),  /* ALLOC MSGLEVEL=1 */                  02150000
            8    *     BIT(1),  /* RESERVED FOR FUTURE USE */           02160000
            8 INCMMGL2 BIT(1),  /* JCL MSGLEVEL=2 */                    02170000
            8 INCMMGL1 BIT(1),  /* JCL MSGLEVEL=1 */                    02180000
       6 JCTJPRTY BIT(4),  /* JOB PRIORITY */                           02190000
     4 JCTJNAME CHAR(8),  /* JOBNAME */                                 02200000
     4 JCTJTPTN CHAR(8),  /* T/P TERMINAL NAME */                       02210000
     4 JCTPDIP  PTR(24) BDY(WORD), /* PDI CORE POINTER     Y02670 */    02220000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02230000
     4 JCTGDGNT PTR(24) BDY(WORD), /* GDG NAME TABLE           Y02670*/ 02240000
     4 JCTJCSMF CHAR(1),  /* JOB CLASS SPECS FOR SMF TERMINATION        02250000
                             ROUTINES                         Y02668 */ 02260000
     4 JCTSDKAD PTR(24) BDY(WORD), /* DISK ADDR. OF FIRST SCT */        02270000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02280000
     4 JCTJCTX  PTR(24) BDY(WORD),     /* ADDRESS OF JCTX    @Z40RPSM*/ 02290000
     4    *     CHAR(1),    /* RESERVED FOR FURTURE USE      @Z40RPSM*/ 02300000
     4 JCTACTAD PTR(24) BDY(WORD), /* DISK ADDR. OF FIRST ACT */        02310000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02320000
     4 JCTSMRBA CHAR(8) BDY(WORD), /* RBA OF SYSTEM MSG DS     Y02641*/ 02330000
     4 JCTSCT   CHAR(1),  /* STEP # OF FAILING STEP            Y02641*/ 02340000
     4   *      CHAR(1),  /* RESERVED */                                02350000
     4 JCTCCODE (8) CHAR(4),  /* CONDITION CODES AND OPERATORS */       02360000
        6 JCTJDPCD CHAR(2),  /* JOB CONDITION CODE */                   02370000
        6 JCTJDPOP CHAR(1),  /* JOB CONDITION OPERATOR */               02380000
        6    *     CHAR(1),  /* RESERVED FOR FUTURE USE */              02390000
     4 JCTRSW1 BIT(8),   /* CHECKPOINT/RESTART SWITCHES */              02400000
        6 JCTWARMS BIT(1),  /* WARM START */                            02410000
        6 JCTSTERM BIT(1),  /* STEP TERM. HAS BEGUN */                  02420000
        6 JCTCONTR BIT(1),  /* JOB ELIGIBLE FOR CONTINUE RESTART        02430000
                                   PROCESSING                Y02641 */  02440000
        6 JCTCKFT BIT(1),  /* CHECKPOINT TAKEN FOR THIS STEP */         02450000
        6 JCTCKPTR BIT(1),  /* CHECKPOINT/RESTART TO BE DONE */         02460000
        6 JCTSTEPR BIT(1),  /* STEP RESTART TO BE DONE */               02470000
        6    *     BIT(2),  /* BITS 6,7 - MUST BE ZERO */               02480000
     4 JCTRSW2 BIT(8),   /* CHECKPOINT/RESTART SWITCHES */              02490000
        6 JCTSYSCK BIT(1),  /* SYSCHEK DD STMT PRESENT */               02500000
        6 JCTNARST BIT(1),  /* JOB INELIGIBLE FOR AUTO RESTART Y02641*/ 02510000
        6 JCTNORST BIT(1),  /* NO RESTART TO BE DONE */                 02520000
        6 JCTNOCKP BIT(1),  /* NO CHECKPOINTS TO BE TAKEN */            02530000
        6 JCTRESTT BIT(1),  /* DO RESTART IF NECESSARY */               02540000
        6 JCTDSOCR BIT(1),  /* RESERVED                         M2344*/ 02550000
        6 JCTDSOJB BIT(1),  /* RESERVED                         M2344*/ 02560000
        6 JCTDSDRA BIT(1),  /* DSDR PROCESSING HAS NOT SUCCESS. ENDED */02570000
     4 JCTDETDA PTR(24) BDY(WORD),  /* TTR OF DSENQ TABLE */            02580000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02590000
     4 JCTEQREG CHAR(2),  /* REGION PARAMETER */                        02600000
     4 JCTQIDNT CHAR(1),  /* IDENTITY OF Q FOR JOB (MVT) */             02610000
     4 JCTSNUMB PTR(8),   /* NUMBER OF STEPS RUN */                     02620000
     4 JCTSTIOT PTR(24) BDY(WORD),  /* TTR OF COMPRESSED TIOT (MVT) */  02630000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02640000
     4 JCTDEVT CHAR(4),  /* DEVICE TYPE OF CHECKPOINT DATA SET */       02650000
     4 JCTCKTTR PTR(24) BDY(WORD),  /* TTR OF JFCB FOR CKPT DATA SET */ 02660000
     4 JCTNTRK  PTR(8),  /* NUMBER OF TRK ON JOBQ USED BY THE JOB       02670000
                                   - SET & USED BY INIT/TERM */         02680000
     4 JCTNRCKP FIXED(15),  /* NUMBER OF CHECKPOINTS TAKEN */           02690000
     4 JCTVOLSQ PTR(8),   /* VOLUME SEQUENCE NUMBER FOR CHECKPOINT DS */02700000
     4 JCTJSB   PTR(8),  /* JOB STATUS SWITCHES                Y02641*/ 02710000
       6   *    BIT(4),  /* RESERVED                           Y02641*/ 02720000
       6 JCTJSBIN BIT(1), /* JOB ENTERED INTERPRETATION        Y02641*/ 02730000
       6 JCTJSBAL BIT(1), /* JOB ENTERED ALLOCATION            Y02641*/ 02740000
       6 JCTJSBEX BIT(1), /* JOB ENTERED EXECUTION             Y02641*/ 02750000
       6 JCTJSBTM BIT(1), /* JOB ENTERED TERMINATION           Y02641*/ 02760000
     4 JCTSSTR PTR(24) BDY(WORD),  /* TTR OF SCT FOR 1ST STEP */        02770000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 02780000
     4 JCTSTAT2 BIT(8),           /*                         A25134 */  02790000
       6 JCTSPSYS BIT(1),         /* SPOOLED SYSIN FOR JOB   A25134 */  02800000
       6 JCTADSPC BIT(1),         /*  ADDRSPC=REAL FOR JOB    Y01029 */ 02810000
       6 JCTENDIT BIT(1),         /* JOB TERM INDICATOR      A25134 */  02820000
       6 JCTSWSM  BIT(1),         /* INDICATES WARM START MESSAGE       02830000
                                     'INIT=JOBNAME' IS TO BE SUPPRESSED 02840000
                                     FOR THIS JOB                       02850000
                                     SET BY IEFVHH                      02860000
                                     TESTED BY IEFSD305       M3144 */  02870000
       6 JCTPERFM BIT(1),          /* PERFORM SPEC'D ON JOB CARD */     02880000
       6 JCTBLP BIT(1),            /* 0-BLP WILL BE TREATED AS NL       02890000
                                      1-BLP WILL BE TREATED AS BYPASS   02900000
                                      LABEL PROCESSING       Y02668 */  02910000
       6    *     BIT(2),         /* RESERVED                 M3144 */  02920000
     4 JCTCKIDL PTR(8),   /* LENGTH OF CHECKPOINT ID */                 02930000
     4 JCTCKIDT CHAR(16),  /* CHECKPOINT ID */                          02940000
               /* SYSTEM MANAGEMENT FACILITIES SUBFIELDS */             02950000
     4 JCTJMR PTR(24) BDY(HWORD),  /* TTR OF JMR */                     02960000
     4 JCTJMRD CHAR(1),  /* DATE DIFFERENCE - STEP/JOB STARTS */        02970000
     4 JCTJMROP BIT(8),   /* SMF OPTION SWITCHES */                     02980000
     4 JCTJMRCL CHAR(1),  /* SMF CANCELLATION CONTROL STATUS */         02990000
     4 JCTJMRTL CHAR(3),  /* JOB TIME LIMIT */                          03000000
     4 JCTJMRSS CHAR(3),  /* STEP START ( TIME OF DAY ) */              03010000
     4 JCTJMRJT CHAR(3),  /* JOB START ( TIME OF DAY ) */               03020000
     4 JCTJMRJD CHAR(3),  /* JOB START DATE */                          03030000
     4 JCTSRBT PTR(31),  /* ACCUMULATED SRB TIME FOR JOB      Y02652 */ 03040000
     4    *     CHAR(1),  /* RESERVED */                                03050000
     4 JCTSSD CHAR(3),   /* STEP START DATE                   Y02668*/  03060000
     4 JCTUSER  CHAR(7), /* USER ID - SET BY IEFVJA         @Z40RPTH*/  03070000
     4 JCTPRFMF PTR(8),  /*  PERFORMANCE GROUP NUMBER          Y02668*/ 03080000
     4 JCTACODE CHAR(4),  /* ABEND CODE FIELD                 Y02670 */ 03090000
     4 JCTVULDP PTR(24) BDY(WORD),   /* PTR TO VOL UNLOAD TAB  Y02670*/ 03100000
     4    *     CHAR(1);   /* RESERVED */                               03110000
     /*     ACCOUNT CONTROL TABLE     */                                03120000
  DECLARE 1 IEFAACTB BDY(DWORD) BASED( ADDR(INJMJCT) + 176 ),           03130000
     4 ACTDSKAD PTR(24) BDY(DWORD),  /* DISK ADDR OF THIS ACT */        03140000
     4 ACTIDENT CHAR(1),  /* ACT ID = 1    */                           03150000
     4 ACTNEXT  PTR(24) BDY(WORD), /*    TTR OF NEXT ACT          */    03160000
     4    *     CHAR(1),  /* RESERVED FOR FUTURE USE */                 03170000
     4 ACTPRGNM CHAR(20),  /*  PROGRAMMERS NAME */                      03180000
     4 ACTJTIME PTR(24) BDY(WORD),  /* JOB RUNNING TIME */              03190000
     4 ACTJNFLD PTR(8),   /* NBR OF JOB ACCT FIELDS */                  03200000
     4 ACTACCNT CHAR(144);   /*  SPACE FOR VARIABLE FIELDS */           03210000
    /*  THE REST OF THE FIELDS HAVE THE FOLLOWING FORMAT FOR JOB        03220000
      ACCOUNTING- 1 BYTE- LENGTH OF FIELD                               03230000
                  VARIABLE BYTES- CONTENTS OF FIELD                     03240000
              (LAST 2 REPEATED FOR N FIELDS  )                          03250000
         STEP ACCOUNTING HAS THE FOLLOWING FORMAT FOR EACH STEP-        03260000
          3 BYTES- MAXIMUM STEP RUNNING TIME                            03270000
          1 BYTE- NBR OF FIELDS IN STEP                                 03280000
          1 BYTE- LENGTH OF FIELD                                       03290000
          VARIABLE BYTES- CONTENTS OF FIELD                             03300000
              (LAST 2 REPEATED N TIMES )             */                 03310000
  %DECLARE JCTID    FIXED,                                              03320000
            JCTLNGTH FIXED;                                             03330000
  %JCTLNGTH=176;            /* LENGTH OF JOB CONTROL TABLE */           03340000
  %JCTID=0;                 /* JCT ID = 0 */                            03350000
/*
//IEBGENR2 EXEC  PGM=IEBGENER
//* THIS STEP INSERTS SYZYGY1A INTO SYS1.HASPSRC, IT'S NEEDED BY
//*   THE HASPSSSM MOD SYZJ201
//SYSPRINT  DD SYSOUT=*
//SYSIN   DD  DUMMY
//SYSUT2  DD  DSN=SYS1.HASPSRC(SYZYGY1A),DISP=SHR
//SYSUT1  DD  *
* THIS CODE (SYZYGY1A) IS INCLUDED IN HASPSSSM AS VIA A COPY
* STATEMENT FROM SYSMOD SYZJ201, THIS CODE SHOULD BE INCLUDED
* IN SYS1.HASPSRC
         SR     R0,R0         ZERO REGISTER                     SYZYGY1 00010000
         LR     R1,R4           ADDRESS OF JCT(OS)              SYZYGY1 00020000
         USING  INJMJCT,R1      TELL ASSEMBLER                  SYZYGY1 00030000
         TM     JCTJSTAT,JCTABEND WAS JOB ABENDED               SYZYGY1 00040000
         BNO    SYZ3A1           NO                             SYZYGY1 00050000
         L      R0,JCTACODE      GET ABEND CODE                 SYZYGY1 00060000
         B      SYZ3A3           GO TO FINISH                   SYZYGY1 00070000
SYZ3A1   L      R1,JCTSDKAD           FIRST SCT   ADDRESS       SYZYGY1 00080000
         USING  INSMSCT-16,R1    TELL ASSEMBLER                 SYZYGY1 00090000
SYZ3A4   SRL    R1,8             GET ADDRESS IN PROPER FORM     SYZYGY1 00100000
         LTR    R1,R1                  TEST FOR ZERO (END)      SYZYGY1 00110000
         BZ     SYZ3A3           GO TO FINISH                   SYZYGY1 00120000
         CH     R0,SCTSEXEC      TEST FOR NEW MAX               SYZYGY1 00130000
         BNL    SYZ3A5            NO                            SYZYGY1 00140000
         LH     R0,SCTSEXEC       YES SAVE NEW MAX              SYZYGY1 00150000
SYZ3A5   L      R1,SCTANSCT      GET NEXT SCT                   SYZYGY1 00160000
         B      SYZ3A4           LOOP                           SYZYGY1 00170000
SYZ3A3   ST     R0,JCTCNVRC      SAVE IT                        SYZYGY1 00180000
         MVI    JCTCNVRC,X'77'   MARK IT AS OURS                SYZYGY1 00190000
         B      SYZYGYX          RETURN TO NORMAL PROC          SYZYGY1 00200000
         DROP   R1                                              SYZYGY1 00210000
         TITLE   'CONTROL BLOCKS FOR      SYZYGY1'              SYZYGY1 00220000
JCTDSCT  DSECT                                                  SYZYGY1 00230000
         IEFAJCTZ                                               SYZYGY1 00240000
         EJECT                                                  SYZYGY1 00250000
SCTDSCT  DSECT                                                  SYZYGY1 00260000
         IEFASCTB                                               SYZYGY1 00270000
HASPSSSM CSECT                                                  SYZYGY1 00280000
         TITLE   'HOSTERM -SUBSYSTEM JOB TERM FUNCTION'         SYZYGY1 00290000
SYZYGYX  DS    0H                                               SYZYGY1 00300000
/*
//IEBGENR3 EXEC  PGM=IEBGENER
//* THIS STEP INSERTS SYZYGY1B INTO SYS1.HASPSRC, IT'S NEEDED BY
//*   THE HASPPRPU MOD SYZJ202
//SYSPRINT  DD SYSOUT=*
//SYSIN   DD  DUMMY
//SYSUT2  DD  DSN=SYS1.HASPSRC(SYZYGY1B),DISP=SHR
//SYSUT1  DD  *
* THIS CODE (SYZYGY1B) IS INCLUDED IN HASPPRPU AS VIA A COPY
* STATEMENT FROM SYSMOD SYZJ202, THIS CODE SHOULD BE INCLUDED
* IN SYS1.HASPSRC
         CLI    JCTCNVRC,X'77'  IS IT OURS ?                    SYZYGY1 00000320
         BNE    SYZ3B1          NO SKIP                         SYZYGY1 00000330
         CLI    JCTJTFLG,JCTJTJF      IS IT OURS ?              SYZYGY1 00000340
         BE     SYZ3B1                NO SKIP                   SYZYGY1 00000350
         TM     JCTJTFLG,JCTJTABD   ABENDED?                    SYZYGY1 00000360
         BZ     SYZ3B2         NO ABEND                         SYZYGY1 00000370
         MVC    0(22,R1),=C'- ABENDED  SYSTEM XXX '             SYZYGY1 00000380
         L      R2,JCTCNVRC     GET CODE                        SYZYGY1 00000390
         LA     R2,0(R2)        CLEAR HIGH ORDER                SYZYGY1 00000400
         SRL    R2,12           GET SYSTEM CODE IN              SYZYGY1 00000410
         LTR    R2,R2           TEST SYSTEM CODE                SYZYGY1 00000420
         BZ     SYZ3B3          USER CODE                       SYZYGY1 00000430
         SRDL   R2,12           CONVERT IT TO HEX               SYZYGY1 00000440
         SR     R2,R2                   "                       SYZYGY1 00000450
         SLDL   R2,4                    "                       SYZYGY1 00000460
         IC     R2,SYZ3BTL(R2)          "                       SYZYGY1 00000470
         STC    R2,18(R1)               "                       SYZYGY1 00000480
         SR     R2,R2                   "                       SYZYGY1 00000490
         SLDL   R2,4                    "                       SYZYGY1 00000500
         IC     R2,SYZ3BTL(R2)          "                       SYZYGY1 00000510
         STC    R2,19(R1)               "                       SYZYGY1 00000520
         SR     R2,R2                   "                       SYZYGY1 00000530
         SLDL   R2,4                    "                       SYZYGY1 00000540
         IC     R2,SYZ3BTL(R2)          "                       SYZYGY1 00000550
         STC    R2,20(R1)               "                       SYZYGY1 00000560
         B      SYZ3B4      GO INCREMENT LINE LENGTH            SYZYGY1 00000570
SYZ3B2   MVC    0(22,R1),=C'- MAX COND CODE XXXX  '             SYZYGY1 00000580
         B      SYZ3B5      GO CONVERT TO DEC/PRINTABLE         SYZYGY1 00000590
SYZ3B3   MVC    0(22,R1),=C'- ABENDED  USER XXXX  '             SYZYGY1 00000600
SYZ3B5   LH     R2,JCTCNVRC+2  GET MAX COND/USER ABEND          SYZYGY1 00000610
         CVD    R2,16(R1)      CONVERT TO DECIMAL               SYZYGY1 00000620
         UNPK   16(4,R1),16(8,R1)   UNPK FOUR LOW DIGITS        SYZYGY1 00000630
         OI     19(R1),X'F0'  SET ZONE                          SYZYGY1 00000640
         MVI    21(R1),X'40'  CLEAR LAST BYTE                   SYZYGY1 00000650
SYZ3B4   LA     R1,22(R1)    INCREMENT LENGTH                   SYZYGY1 00000660
         B      OPLOUSM2     RETURN TO NORMAL PROCESSING        SYZYGY1 00000670
SYZ3BTL  DC     CL16'0123456789ABCDEF'    CONVERSION TABLE      SYZYGY1 00000680
SYZ3B1   DS     0H       BE SURE OF ALLIGNMENT                  SYZYGY1 00000690
/*
//SMPR1   EXEC SMPREC                                                   00030000
//* THIS STEP RECEIVES THE 2 SYSMODS SYZJ201 AND SYZJ202
//* REJECT SELECT(SYZJ201,SYZJ202).
//SMPCNTL  DD  *                                                        00320000
  RESETRC.
  RECEIVE SELECT(SYZJ201,SYZJ202).                                      00340001
//SMPPTFIN  DD *                                                        00380001
++USERMOD(SYZJ201).
++VER(Z038) FMID(EJE1103) 
      PRE(UZ77164,UZ33158,UZ35334,UZ37263,
      UZ31176,UZ52543,UZ54837,UZ57911,UZ60375,UZ63374,UZ65742,
      UZ68537,UZ71437,UZ76165,TJES801).
++SRCUPD(HASPSSSM).
./ CHANGE NAME=HASPSSSM
         COPY   SYZYGY1A                                        SYZYGY1 T2269950
++USERMOD(SYZJ202).
++VER(Z038) FMID(EJE1103) PRE(UZ76165,UZ31176,UZ33158,UZ35334,
     UZ37263,UZ52543,UZ54837,UZ57911,UZ60375,UZ63374,UZ65742,
     UZ68537,UZ71437).
++SRCUPD(HASPPRPU).
./ CHANGE NAME=HASPPRPU
         COPY   SYZYGY1B                                        SYZYGY1 Q0557050
/*
//SMP     EXEC SMPAPP                                                   00030000
//*  THIS STEP IS THE APPLY STEP FOR SYZJ201 AND SYZJ202
//*    THERE ARE A COUPLE OF OVERRIDES BECAUSE THE DEFAULT
//*    SMP PROC NEEDS TO INCLUDE SYS1.HASOSRC AND THE WORK
//*    DATASETS ARE A LITTLE TOO SMALL TO ASSEMBLE HASPSSSM
//HMASMP.SYSUT1 DD UNIT=SYSDA,SPACE=(CYL,(50,50))
//HMASMP.SYSUT2 DD UNIT=SYSDA,SPACE=(CYL,(50,50))
//HMASMP.SYSLIB DD
//            DD
//            DD
//            DD
//            DD
//            DD  DSN=SYS1.HASPSRC,DISP=SHR
//SMPCNTL  DD  *                                                        00320000
  APPLY  SELECT(SYZJ201,SYZJ202).                                       00370001
