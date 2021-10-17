//ZP60009  JOB (SYSGEN),'J05 M24: ZP60009',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD NOEDIT SUPPORT FOR TPUT AND TPG TO TSO/VTAM.
//*
//STEP01  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60009)   REWORK(20190422)                      .
++VER(Z038) FMID(ETV0108)
  PRE(UZ35180,UZ26905,UZ67122,UZ22286,UZ55134,UZ28255,
      UZ71054,UZ54020,UZ68882,UZ57385,UZ28016)
 /*
   PROBLEM DESCRIPTION:
     THE NOEDIT OPERAND OF TPUT AND TPG IS NOT HONOURED BY TSO/VTAM.
       THE TIOC COMMON SVC 93 ROUTINE IGC0009C CORRECTLY DETECTS THE
       REQUEST OF NOEDIT FOR TPUT AND TPG REQUESTS, BUT THIS IS NOT
       HANDLED PROPERLY BY TSO/VTAM.
     THE GTTERM MACRO FUNCTION IS NOT SUPPORTED BY TSO/VTAM.
       WHEN A TSO APPLICATION ISSUES THE GTTERM MACRO RC=4 RESULTS.
       AN APPLICATION CANNOT DETERMINE IF THE READ PARTITION (QUERY)
       FUNCTION IS SUPPORTED.  ALSO, THE ALTERNATE SCREEN SIZE CANNOT
       BE DETERMINED.
     3270 HOUSEKEEPING IS LIMITED TO MODEL-1 AND MODEL-2 SCREEN SIZES.
       TSO WILL ONLY PERFORM LINE MODE 3270 SCREEN HOUSEKEEPING FOR
       MODEL-1 (12 BY 40) AND MODEL-2 (24 BY 80) SCREEN SIZES.  THE
       ALTERNATE SCREEN SIZE OF A TERMINAL CANNOT BE PROPERLY EXPLOITED
       BY TSO LINE MODE, NOR BY TSO FULLSCREEN APPLICATIONS.
     THE NOEDIT OPERAND OF STFSMODE IS NOT SUPPORTED BY TSO/VTAM.
       NOEDIT INPUT MODE IS SET BY SPECIFYING NOEDIT=YES ON A
       'STFSMODE ON' MACRO.  THIS IS NOT SUPPORTED BY TSO.  WITHOUT
       THIS ABILITY ANY X'1E' IN INPUT DATA (INCLUDING A QUERY
       RESPONSE, OR AN SBA ORDER FROM A SCREEN WITH MORE THAN 4096
       LOCATIONS) WILL BE INTERPRETED AS A FIELD MARK CHARACTER AND
       CAUSE THE DATA FOLLOWING IT TO BE HELD OVER UNTIL THE NEXT TGET
       MACRO IS ISSUED.

       THIS USERMOD CHANGES SEVERAL TSO/VTAM MODULES.

       THE TSO/VTAM SVC 93 ROUTER IS CHANGED TO NOT REJECT REQUESTS
       WITH NOEDIT SPECIFIED.

       THE TSO/VTAM TPUT HANDLER IS CHANGED TO NOT TRUNCATE TRAILING
       BLANKS FROM NOEDIT REQUESTS.  A PREVIOUSLY RESERVED BIT IS
       USED TO FLAG NOEDIT REQUESTS, WHICH ARE ALSO FLAGGED AS
       FULLSCREEN REQUESTS BY THE TPUT AND TPG MACROS (AND NOW ALSO
       BY THIS MODULE).

       THE TSO/VTAM TGET HANDLER IS CHANGED TO BACK OUT THE FIX FOR
       APAR OZ60978 SHIPPED IN PTF UZ57385 TO REGRESS FUNCTIONALITY
       TO THE UZ55134 LEVEL SO THAT TPUT MACROS ISSUED AFTER TGET
       MACROS WITH THE NOWAIT OPERAND ARE NOT QUEUED UNTIL THERE IS
       SOME INPUT, BUT SENT TO THE SCREEN ASYNCHRONOUSLY.  THIS HAS
       THE EFFECT OF REINTRODUCING THE PROBLEM WHERE A TGET NOWAIT
       DOES NOT FORCE THE KEYBOARD TO BECOME UNLOCKED, BUT THIS CAN
       BE CIRCUMVENTED BY THE APPLICATION SETTING THE X'02' BIT IN
       THE WCC WHEN APPROPRIATE.

       THE TSO/VTAM TGET HANDLER IS ALSO CHANGED TO ADD THE TGET MACRO
       RETURN CODES 24 AND 28 WHICH MAY BE GIVEN WHEN NOEDIT INPUT
       MODE IS IN EFFECT, THEREBY IMPROVING COMPATIBILITY WITH TSO/E.

       THE TSO/VTAM INITIALIZATION ROUTINE FOR THE TSO INPUT MANAGER
       AND TSO OUTPUT MANAGER HAS BEEN CHANGED TO NOT REGARD TERMINALS
       WITH BUFFER SIZES LARGER THAN 1920 BYTES AS MODEL-1 TERMINALS.
       THIS WILL HELP AVOID LINE MODE SCREEN HANDLING ERRORS AFTER
       LOGON RECONNECT PROCESSING.

       THE TSO/VTAM LOGON RECONNECT ROUTINE HAS BEEN CHANGED TO
       INCLUDE THE PRIMARY AND ALTERNATE SCREEN DIMENSIONS IN THE
       DATA PERTAINING TO THE NEW TERMINAL PROPAGATED TO CONTROL
       BLOCKS WHEN A LOGON RECONNECT IS PERFORMED, THUS ALLOWING TSO
       APPLICATIONS TO DETECT DYNAMIC CHANGES TO THE SCREEN SIZE.

       THE TSO/VTAM 3270 TERMINAL SCREEN MANAGER IS ALTERED TO SKIP
       ANY DATA TRANSLATION FOR NOEDIT TPUTS.  FURTHER, IN KEEPING
       WITH 3270 ARCHITECTURE DATA INTEGRITY FOR DISPLAYABLE CODE
       POINTS (WHERE ALL CODE POINTS IN THE RANGE X'40' TO X'FE'
       ARE CONSIDERED "DISPLAYABLE", AND REGARDLESS OF THE TERMINAL'S
       ABILITY TO DISPLAY THE CHARACTER, WHEN SUCH A CODE POINT IS
       WRITTEN TO THE TERMINAL, IT CAN BE READ BACK UNALTERED) ALL
       TPUTS TO 3270 EBCDIC TERMINALS WILL NO LONGER ALTER ANY DATA
       CODE POINTS IN THE X'40' TO X'FE' RANGE.  (THE X'40' CODE POINT
       IS RESERVED FOR A BLANK IN ALL SINGLE-BYTE CHARACTER SETS.)

       THE TSO/VTAM 3270 TERMINAL SCREEN MANAGER IS ALSO ALTERED TO
       SUPPORT DIFFERENT PRIMARY AND ALTERNATE SCREEN SIZES OF ANY
       SIZE SUBJECT TO THE 3270 ARCHITECTURE LIMITATIONS, AND ALSO
       THAT THE ALTERNATE SCREEN SIZE NEVER HAS FEWER COLUMNS NOR
       FEWER LINES THAN THE PRIMARY SCREEN SIZE.  TSO/VTAM LINE MODE
       HOUSEKEEPING WILL NOW USE 14-BIT ADDRESSING FOR ALL BUFFER
       LOCATIONS GREATER THAN 4095, BUT WILL STILL USE 12-BIT
       ADDRESSING FOR LOCATIONS FROM 0 TO 4095 INCLUSIVE.  THIS
       CHANGE HAS THE EFFECT OF INCREASING THE MAXIMUM TSO/VTAM 3270
       SCREEN SIZE SUPPORTED FROM 4096 LOCATIONS TO 16384 LOCATIONS.

       THE TSO/VTAM 3270 TERMINAL SCREEN MANAGER NOW HANDLES TPUT
       FULLSCR DIFFERENTLY TO THE EXTENT THAT A STANDARD WRITE (X'F1')
       WITH A NO-OP WCC (X'40') WILL NOT FORCE THE INCLUSION OF AN
       INSERT CURSOR ORDER AS THE TPUT MAY BE SENDING AN ASYNCHRONOUS
       PARTIAL SCREEN UPDATE WHICH DOES NOT OVERWRITE AN ACTIVE INPUT
       AREA WHERE SOME TEXT MAY BE BEING TYPED IN.

       THE TSO/VTAM 3270 INPUT DATA HANDLER HAS BEEN ALTERED TO NOT
       PROCESS FIELD MARK CHARACTERS INPUT WHEN NOEDIT MODE IS IN
       EFFECT.  FURTHER, THE ACTUAL NUMBER OF COLUMNS AND LINES ON
       THE SCREEN IS USED IN LINE COUNT CALCULATIONS.

       THE TSO/VTAM SVC 94 ROUTER IS ALTERED TO PASS CONTROL TO A NEW
       CSECT FOR TSO TERMINAL CONTROL FUNCTION 17 (GTTERM).  A TSO
       APPLICATION CAN NOW ISSUE A GTTERM MACRO TO DETERMINE IF THE
       QUERY BIT IS ON, AND CAN GET THE DIMENSIONS OF THE PRIMARY AND
       ALTERNATE SCREEN SIZES.  NOEDIT TPUT/TPG MACROS CAN BE USED
       TO ISSUE WRITE STRUCTURED FIELD COMMANDS TO GAIN ACCESS TO
       VARIOUS 3270 EXTENSIONS INCLUDING GRAPHICS.  QUERY SUPPORT IS
       NOT NEEDED FOR A READ BUFFER COMMAND WHICH CAN NOW ALSO BE
       ISSUED VIA A NOEDIT TPUT/TPG MACRO.  GTTERM RETURN CODES AND
       OUTPUT IS COMPATIBLE WITH TSO/E, EXCEPT THAT THE TERMID OUTPUT
       AREA IS CURRENTLY LIMITED TO 8 BYTES.

       THE TERMINAL CONTROL MACRO ROUTINE FOR THE STFSMODE MACRO HAS
       BEEN CHANGED TO SUPPORT THE NOEDIT OPERAND, AND RESHOW KEY CODES
       CAN NOW BE IN THE RANGE OF FROM 1 TO 24 INCLUSIVE (REPRESENTING
       THE VALID PFK NUMBERS).

       THE TERMINAL CONTROL MACRO ROUTINE FOR THE STLINENO MACRO HAS
       BEEN CHANGED TO SUPPORT THE SPECIFICATION OF ANY VALID LINE
       NUMBER THAT EXISTS ON THE SCREEN.  IT HAS ALSO BEEN CHANGED TO
       SUPPORT NOEDIT INPUT MODE.

       THE TERMINAL CONTROL MACRO ROUTINE FOR THE STSIZE MACRO HAS
       BEEN CHANGED TO INDICATE THAT ANY REQUESTED SCREEN SIZE WHICH
       MATCHES EITHER THE PRIMARY OR ALTERNATE SIZE OF THE SCREEN IS
       CONSIDERED A STANDARD SIZE, EVEN IF IT IS NOT THE SIZE OF A
       MODEL-1 OR MODEL-2 SCREEN.  THIS IS DONE WITH A RETURN CODE
       OF ZERO INSTEAD OF A RETURN CODE OF 12 WHICH INDICATES A
       NON-STANDARD SCREEN SIZE.  (THE FLAGGING OF NON-STANDARD
       SCREEN SIZES IS INTENDED TO WARN OF THE POSSIBILITY OF
       SCREEN CONTROL ERRORS.)

       THE MAPPING MACRO FOR THE TSO/VTAM WORK AREA IKTTVWA IS
       UPDATED TO ADD BIT TVWAALTS WHICH IS SET WHEN THE TSO/VTAM
       SCREEN MANAGER SETS THE SCREEN TO ITS ALTERNATE SIZE.
       (THE MAPPING OF THIS BIT IS INCOMPATIBLE WITH TSO/E.)

       THE STFSMODE TERMINAL CONTROL MACRO FOR TSO FULLSCREEN
       APPLICATIONS HAS BEEN UPDATED TO ADD THE NOEDIT OPERAND,
       WHICH CAN BE ASSIGNED THE VALUE OF YES OR NO.  NO IS THE
       DEFAULT.  NOEDIT=YES IS USED TO ALLOW BYTES WITH A VALUE OF
       X'1E' TO BE RETURNED TO AN APPLICATION AS INPUT DATA
       FROM THE TERMINAL WITHOUT EACH SUCH BYTE BEING PROCESSED AS
       A FIELD MARK.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 9.

     REWORK HISTORY:
       2003-02-01: INITIAL RELEASE.
       2003-03-05: KBD (UN)LOCK ASYNC MSGS DURING NOEDIT TPUTS.
       2003-03-30: DO NOT FORCE IC IF WRT WITH NO-OP WCC.
       2005-08-14: DO NOT FORCE LINE-WRAP AFTER COLUMN 80.
       2009-06-01: USE SCREEN SIZES AND QUERY BIT FROM SNA BIND.
       2009-11-07: CHANGE IKTIIOM AND ADD IKTLOGR ZAPS FOR RECONNECT.
       2012-04-28: IMPROVE THE INTEROPERABILITY OF TPUT NOEDIT (WITH
                   "UNLOCK KEYBOARD" SET IN THE WCC) AND TGET NOWAIT
                   TO MATCH THAT OF TPUT FULLSCR AND TGET NOWAIT.
       2019-04-22: CHANGE IKTVTGET SO THAT TGET NOWAIT REQUESTS DO
                   NOT INHIBIT WRITES TO THE TERMINAL UNTIL SOME
                   INBOUND DATA IS FIRST RECEIVED FROM THE TERMINAL.
                   REMOVE DEBUG CODE AND FLAGS FROM IKT3270O CSECT.
                   REMOVE FLSCRTAB EDITING OF TPUT FULLSCR DATA.

     TSO/VTAM IS A VTAM APPLICATION.  THIS SYSMOD DOES NOT IMPLY
     THAT VTAM CAN CORRECTLY PROCESS THE DATA THAT CAN NOW BE
     GENERATED BY TSO APPLICATIONS, EVEN IF THE DATA CONSISTS OF
     A PERFECTLY VALID 3270 DATA STREAM.

     EVT0108 LOCAL NON-SNA SUPPORT IS PROVIDED BY USERMOD ZP60008.

     USERMOD ZP60032 UPGRADES THE GTTERM MACRO TO BE ABLE TO EXPLOIT
     NEW FUNCTION DELIVERED IN THIS SYSMOD.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKT0009C
       IKTVTPUT
       IKTIIOM
       IKTLOGR
       IKTVTGET
       IKT3270O
       IKT0009D
       IKTXLOG
       IKT3270I
       IKT09412
       IKT09413
       IKT0940A
     MACROS:
       IKTTVWA
       STFSMODE
 */.
++ZAP(IKT0009C) DISTLIB(AOST3).
 NAME IKT0009C
 IDRDATA ZP60009
VER 00C6 4870,5060           LH    R7,XSAPRMTJ
VER 00CA 1277                LTR   R7,R7
VER 00CC 4780,C0C6           BZ
VER 00E4 4870,5060           LH    R7,XSAPRMTJ
VER 00E8 1277                LTR   R7,R7
VER 00EA 4780,C1AE           BZ
REP 00CC 47D0                BNP
REP 00EA 47D0                BNP
++ZAP(IKTVTPUT) DISTLIB(AOST3).
 NAME IKTVTPUT
 IDRDATA ZP60009
VER 0016 05C0                BALR  R12,0
VER 0258 5010,D050           ST    R1,OPFLAGS
VER 02DA 5010,D050           ST    R1,OPFLAGS
VER 043E 50E0,506C  DATAEDIT ST    R14,XSASAVEA
VER 0442 9103,5064           TM    XSAOPTNS,XSAEDITO
VER 0446 4770,C472           BNZ   @RT00307
VER 044A 9640,5068  NEXTINST OI    XSAFLAG,XSADMOVE
VER 0508 0000,0000  *        PATCH AREA
VER 050C 0000,0000  *        PATCH AREA
VER 0510 0000,0000  *        PATCH AREA
VER 0514 0000,0000  *        PATCH AREA
VER 0518 0000,0000  *        PATCH AREA
VER 051C 0000,0000  *        PATCH AREA
VER 0520 0000       *        PATCH AREA
VER 0522 0000,0000  *        PATCH AREA
VER 0526 0000       *        PATCH AREA
REP 0258 45E0,C500           BAL   R14,PATCH2
REP 02DA 45E0,C500           BAL   R14,PATCH2
REP 0446 47F0,C4F0           B     PATCH1
REP 0508 4770,C472  PATCH1   BNZ   @RT00307
REP 050C 9180,5060           TM    XSAPRMTJ,XSANOED
REP 0510 4710,C472           BO    @RT00307
REP 0514 47F0,C432           B     NEXTINST
REP 0518 5010,D050  PATCH2   ST    R1,OPFLAGS
REP 051C 9180,5060           TM    XSAPRMTJ,XSANOED
REP 0520 07EE                BNOR  R14
REP 0522 9634,D051           OI    OPOPTNS,OPEDIT+OPNOED
REP 0526 07FE                BR    R14
++ZAP(IKTIIOM) DISTLIB(AOST4).
 NAME IKTIIOM
 IDRDATA ZP60009
VER 03C4 D501,804A,64D4      CLC   TSBXTMBF,=H'1920'
VER 03CA 4770,63BE           BNE   MODEL1
VER 03CE 9250,C008           MVI   TSBLNSZ,80
VER 03D2 9218,C028           MVI   TSBLNNO,24
VER 03D6 47F0,63EA           B     SIZESET
REP 03CA 4740,63BE           BL    MODEL1
++ZAP(IKTLOGR) DISTLIB(AOST4).
 NAME IKTLOGR
 IDRDATA ZP60009
VER 0150 D207,B068,A068      MVC   TSBTRMID(8,TSBBASE),TSBTRMID(@10)
REP 0150 D20B,B064,A064      MVC   TSBPRMR(12,TSBBASE),TSBPRMR(@10)
++MOD(IKTVTGET) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP02  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '*** IKTVTGET: TSO/VTAM TGET ROUTINE ***                *
                        '
*
*   MODIFIED BY GREG PRICE APRIL 2019 FOR USERMOD ZP60009
*
IKTVTGET CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(33)                                        ZP60009-C
         DC    C'IKTVTGET  82.141'                                 0001
         DC    C' ZP60009 2019.109'                           ZP60009-A
         DROP  R15
         USING PSA,0                                          ZP60009-A
@PROLOG  BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
*                                                                  0120
*/********************************************************************/
*/*                                                                  */
*/*  START OF VTAM TGET. ENTERED FROM IGC0009C WITH LOCAL LOCK HELD  */
*/*                                                                  */
*/********************************************************************/
*                                                                  0120
*   XSATCBA=TCBPTR;                 /* SAVE TCB ADDRESS              */
         STCM  R4,7,XSATCBA-RBBASIC(R5)                            0120
*   RFY                                                            0121
*     TCBPTR UNRSTD;                /* FREE-UP REGISTER              */
*   RFY                                                            0122
*     TCB BASED(XSATCBA);                                          0122
         USING TSB,R9                  SET ON ENTRY           ZP60009-A
*        USING RBBASIC,R5              SET ON ENTRY           ZP60009-A
*   TVWAPTR=TSBXTVWA;               /* SET-UP ADDRESSABILITY TO TVWA */
         L     R4,TSBEXTNT                                         0123
         L     R8,TSBXTVWA-TSBX(,R4)                               0123
         USING TVWA,R8                                        ZP60009-A
*   SAVEREG=TVWALLWA;               /* SET R13 TO LOCALLY-LOCKED SAVE
*                                      AREA                          */
         L     R13,TVWALLWA                                        0124
*   XSADATSZ=0;                     /* INITIALIZE DATA SIZE TO 0     */
         SLR   R4,R4                                               0125
         STH   R4,XSADATSZ-RBBASIC(,R5)                            0125
         ST    R4,XSAWD12-RBBASIC(,R5)                        UZ57385-A
*   XSADATVF='0'B;                  /* INITIALIZE TO 'DATA NOT     0126
*                                      VERIFIED'                     */
         NI    XSAFLAG-RBBASIC(R5),255-XSADATVF                    0126
*                                                                  0127
*/********************************************************************/
*/*                                                                  */
*/*     FOLLOWING DO WILL NORMALLY BE EXECUTED ONCE. IT WILL ITERATE */
*/*     IF THERE IS NO DATA AVAILABLE & CALLER REQUESTED 'NOWAIT'.   */
*/*     (AFTER BEING SUSPENDED/REDISPATCHED WHEN DATA IS AVAILABLE)  */
*/*     IT WILL ALSO ITERATE IF THE QUEUE ELEMENT OBTAINED INDICATES */
*/*     'PARTIAL LINE'. I.E. REMAINDER OF INPUT LINE IS IN A FOLLOW- */
*/*     ING QUEUE ELEMENT.                                           */
*/*                                                                  */
*/********************************************************************/
*                                                                  0127
*   DO UNTIL XSADOSWT='0'B;         /* BIG DO TO PREVENT GOTO'S      */
@DL00127 DS    0H                                                  0128
*     XSADOSWT='0'B;                /* INIT. SWITCH TO OFF FOR EXIT  */
         NI    XSAFLAG-RBBASIC(R5),255-XSADOSWT                    0128
*     CALL TERMSTAT;                /* CHECK TERMINAL STATUS STATUS
*                                      RETURNED IN XSARC             */
         BAL   R14,TERMSTAT                                        0129
*     IF XSARC^=CONTINUE THEN                                      0130
         CLC   XSARC-RBBASIC(1,R5),CONTINUE                        0130
         BNE   @RT00130                                            0130
*       ;                           /* IF TERMINAL SESSION CANCELLED
*                                      OR ATTENTION HIT, RETURN      */
*     ELSE                                                         0132
*       DO;                                                        0132
*         IPREQ=OBTAIN;             /* REQUEST FOR DATA              */
         MVI   IPREQ(R13),X'02'                                    0133
*/*                                                                  */
*/*  *****************************************************************/
*/*                                                                  */
*/*       CALL INPUT QUEUE MANAGER, IKTQMIN.                         */
*/*       PARAMETER LIST INTERFACE IS BUILT DIRECTLY                 */
*/*       SO THAT PLS WILL NOT GENERATE ANY 'TEMPS'                  */
*/*       ITSELF. DIRECTLY BUILT PARAMETER LIST IS IN                */
*/*       LOCALLY LOCKED WORK AREA.                                  */
*/*                                                                  */
*/*  *****************************************************************/
*/*                                                                 **/
*                                                                  0134
*         PRMLSTAD=ADDR(IPARMS);    /* PUT ADDR INTO 'TEMP'          */
         LA    R4,IPARMS(,R13)                                     0134
         ST    R4,PRMLSTAD(,R13)                                   0134
*         RFY                                                      0135
*           PRMREG1 RSTD;                                          0135
*         PRMREG1=ADDR(PRMLSTAD);   /* REG 1 POINTS TO PLS PARM LST  */
         LA    R1,PRMLSTAD(,R13)                                   0136
*         CALL IKTQMIN;             /* CALL INPUT QUEUE MANAGER      */
         L     R4,CVTPTR                                           0137
         L     R4,CVTTCASP-CVT(,R4)                                0137
         L     R15,TCASIQM-TCAST(,R4)                              0137
         BALR  R14,R15                                             0137
*         RFY                                                      0138
*           PRMREG1 UNRSTD;                                        0138
*         IF IPRC=UNSUCCES THEN     /* ANY DATA AVAILABLE ?          */
         CLI   IPRC(R13),3                                         0139
         BNE   @RF00139                                            0139
*           DO;                     /* NO DATA                       */
*             IF TSBXTMTP^=T3767 THEN/* NOT A 3767 TERMINAL? @ZM20749*/
         L     R4,TSBEXTNT                                         0141
         CLI   TSBXTMTP-TSBX(R4),2                                 0141
         BE    @RF00141                                            0141
*               DO;                 /* NOT A 3767 TERMINAL   @ZM20749*/
*                 IF TVWAPGN='1'B|  /* IS PAGING ON?         @YM03259*/
*                     TVWAKBDL='1'B|/* KEYBOARD LOCKED?      @YM03259*/
*                     TVWAATTN>0|   /* ATTENTION?            @ZM20749*/
*                     TVWAAIGN='1'B /* ATTENTION IGNORED?    @ZM20749*/
*                   THEN            /* PAGING OR KEYBOARD LOCKED   0143
*                                                            @YM03259*/
         TM    TVWAFLG2,TVWAPGN                                    0143
         BO    @RT00143                                            0143
         TM    TVWAFLG4,TVWAKBDL                                   0143
         BO    @RT00143                                            0143
         CLI   TVWAATTN,0                                          0143
         BH    @RT00143                                            0143
         TM    TVWAFLG3,TVWAAIGN                                   0143
         BO    @RT00143                                       ZP60009-C
         TM    XSAOPTNS-RBBASIC(R5),XSANOWT                   ZP60009-A
         BO    @RF00143                                       ZP60009-A
@RT00143 DS    0H                                                  0144
*                   TVWAULK='1'B;   /* REQUEST TO UNLOCK KEYBOARD  0144
*                                                            @YM03259*/
         OI    TVWAFLG1,TVWAULK                                    0144
*                 ELSE                                             0145
*                   ;               /* NO PAGING-KEYBOARD          0145
*                                      UNLOCKED@YM03259              */
@RF00143 DS    0H                                                  0146
*                 IF TVWATIS='0'B&  /* IS TOM NOT SCHEDULED? @YM03259*/
*                     TVWATAS='0'B& /* AVAILABLE AND         @YM03259*/
*                     (TVWAKBDL='1'B|/* IS THE KEYBOARD LOCKED?    0146
*                                                            @ZM20749*/
*                     TVWAATTN>0|   /* ATTENTION?            @ZM20749*/
*                     TVWAAIGN='1'B)/* ATTENTION IGNORED?    @ZM20749*/
*                   THEN            /* YES                           */
         TM    TVWAFLG1,TVWATIS                                    0146
         BNZ   @RF00141                                            0146
         L     R4,TSBEXTNT
         L     R4,TSBXSRB-TSBX(,R4)
         LTR   R4,R4                   ANY SRB FOR TOM PRESENT?
         BZ    @RF00141                NO
         TM    XSAOPTNS-RBBASIC(R5),XSANOWT                   ZP60009-A
         BNO   NOWAITOK                NOT A TGET NOWAIT      ZP60009-A
         TM    TVWAFLG1,TVWAULK        OTHER STUFF GOING ON?  ZP60009-A
         BO    NOWAITOK                YES, IGNORE NOWAIT     ZP60009-A
         NI    TVWAFLG1,255-TVWATAS    NO, SCHEDULE A TOM SRB ZP60009-A
         OI    TVWAFLG4,TVWAKBDL       TELL TPUT K/B LOCKED   ZP60009-A
NOWAITOK DS    0H                      NOWAIT DIVERGENCE END  ZP60009-A
         TM    TVWAFLG1,TVWATAS                                    0146
         BNZ   @RF00141                                            0146
         TM    TVWAFLG4,TVWAKBDL                                   0146
         BO    @RT00146                                            0146
         CLI   TVWAATTN,0                                          0146
         BH    @RT00146                                            0146
         TM    TVWAFLG3,TVWAAIGN                                   0146
         BNO   @RF00141                                            0146
@RT00146 DS    0H                                                  0147
*                   DO;             /* SET UP SCHEDULING OF TOM      */
         BAL   R14,SCHEDTOM                                   UZ57385-C
*               END;                /*                       @ZM20749*/
*             ELSE                                                 0156
*               ;                   /* A 3767 TERMINAL       @ZM20749
*                                      IF TGET NO WAIT, THEN RETURN
*                                      TO CALLER IMMEDIATELY         */
@RF00141 DS    0H                                                  0157
*             IF XSANOWT='1'B THEN  /* DID CALLER SAY 'NOWAIT' ?     */
         TM    XSAOPTNS-RBBASIC(R5),XSANOWT                        0157
         BNO   @RF00157                                            0157
*               DO;                 /* YES,                          */
*                 XSARC=NOWAIT;     /* SET RETURN CODE TO NODATA     */
         OI    TVWAFLG8,TVWATGNO                              UZ57385-A
         MVI   XSARC-RBBASIC(R5),X'04'                             0159
*               END;                                               0160
*                                                                  0160
*             /*******************************************************/
*             /*                                                     */
*             /* NO DATA AVAILABLE AND CALLER SPECIFIED TGET WAIT.   */
*             /* SUSPEND CALLER UNTIL DATA IS AVAILABLE OR ATTENTION */
*             /* HIT                                                 */
*             /*                                                     */
*             /*******************************************************/
*                                                                  0161
*             ELSE                                                 0161
*               DO;                                                0161
         B     @RC00157                                            0161
@RF00157 DS    0H                                                  0162
         NI    TVWAFLG8,255-TVWATGNO                          UZ57385-A
*                 XSADOSWT='1'B;    /* SET SWITCH TO CONTINUE LOOP 0162
*                                      AFTER DATA BECOMES AVAILABLE  */
         OI    XSAFLAG-RBBASIC(R5),XSADOSWT                        0162
*                 RFY                                              0163
*                  (PRMREG1,                                       0163
*                   REG2) RSTD;                                    0163
*                                                                  0164
*                 /***************************************************/
*                 /*                                                 */
*                 /* SET 'TPUT' OR 'TGET' INDICATOR FOR SRM ACCORDING*/
*                 /* TO WHETHER THE TERMINAL OUTPUT MANAGER WILL BE  */
*                 /* ACTIVE BEFORE TERMINAL DATA IS ENTERED. THIS    */
*                 /* AFFECTS THE SWAP OUT/SWAP IN OF THE ADDRESS     */
*                 /* SPACE                                           */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0164
*                 IF TSBXTMTP=T3767 /* 3767?                 @YM03601*/
*                   THEN            /* 3767                  @YM03601*/
         L     R4,TSBEXTNT                                         0164
         CLI   TSBXTMTP-TSBX(R4),2                                 0164
         BNE   @RF00164                                            0164
*                   IF TSBSPIT='0'B&TSBAUTON='1'B&TSBSTAUT='1'B/*  0165
*                                      PROMPT?               @YM03601*/
*                     THEN          /* PROMPT IN EFFECT      @YM03601*/
         TM    TSBFLG3,TSBSPIT                                     0165
         BNZ   @RF00165                                            0165
         TM    TSBFLG2,TSBAUTON+TSBSTAUT                           0165
         BNO   @RF00165                                            0165
*                     TVWAPRMT='1'B;/* PROMPT IN EFFECT      @YM03601*/
         OI    TVWAFLG5,TVWAPRMT                                   0166
*                   ELSE            /* NO PROMPT             @YM03601*/
*                     DO;           /* RESET SWITCHES        @YM03601*/
         B     @RC00165                                            0167
@RF00165 DS    0H                                                  0168
*                       TVWAPRMT='0'B;/* NO PROMPT           @YM03601*/
*                       TVWAP1ST='0'B;/* RESET 1ST-PROMPT    @YM03601*/
         NI    TVWAFLG5,255-TVWAPRMT-TVWAP1ST                      0169
*                     END;          /* END OF RESET          @YM03601*/
*                 ELSE                                             0171
*                   ;               /* 3270                  @YM03601*/
@RF00164 DS    0H                                                  0172
*                 PRMREG1=1;        /* NO SYSEVENT           @YM03259*/
@RC00164 LA    R1,1                                                0172
*                 IF TVWATIS='0'B   /* IS TOM SCHEDULED?     @YM03259*/
*                   THEN            /* TOM NOT SCHEDULED     @YM03259
*                                                            @ZA12766
*                                                            @ZA12766*/
         TM    TVWAFLG1,TVWATIS                                    0173
         BNZ   @RF00173                                            0173
*                   IF TVWAKBDL='0'B/* KEYBOARD UNLOCKED?    @YM03259*/
*                     THEN          /* KEYBOARD UNLOCKED     @YM03259*/
         TM    TVWAFLG4,TVWAKBDL                                   0174
         BNZ   @RF00174                                            0174
*                     PRMREG1=INPTWAIT;/* INPUT WAIT         @YM03259*/
         SLR   R1,R1                                               0175
*                   ELSE                                           0176
*                     ;             /* KEYBOARD LOCKED       @YM03259
*                                                            @ZA12766*/
@RF00174 DS    0H                                                  0177
*                 ELSE              /* TOM SCHEDULED         @YM03259*/
*                   IF TVWAKBDL='0'B/* KEYBOARD LOCKED?      @YM03259*/
*                     THEN          /* KEYBOARD UNLOCKED     @YM03259*/
         B     @RC00173                                            0177
@RF00173 TM    TVWAFLG4,TVWAKBDL                                   0177
         BNZ   @RF00177                                            0177
*                     IF TVWATIR='1'B/* IS TOM RUNNING?      @YM03259*/
*                       THEN        /* TOM IS RUNNING        @YM03259*/
         TM    TVWAFLG5,TVWATIR                                    0178
         BNO   @RF00178                                            0178
*                       IF TSBXTMTP=IDS|TSBXTMTP=TNDS/*      @G58AK3A*/
*                         THEN      /* 3270 OR NDS           @YM03601*/
         L     R4,TSBEXTNT                                         0179
         CLI   TSBXTMTP-TSBX(R4),1                                 0179
         BE    @RT00179                                            0179
         CLI   TSBXTMTP-TSBX(R4),4                                 0179
         BNE   @RF00179                                            0179
@RT00179 DS    0H                                                  0180
*                         PRMREG1=INPTWAIT;/* INPUT WAIT     @YM03601*/
         SLR   R1,R1                                               0180
*                       ELSE        /* 3767 OR USER          @YM03601*/
*                         IF TVWADOOQ='1'B/* QUEUE EMPTY?    @YM03601*/
*                           THEN    /* QUEUE EMPTY           @YM03601*/
         B     @RC00179                                            0181
@RF00179 TM    TVWAFLG4,TVWADOOQ                                   0181
         BNO   @RF00181                                            0181
*                           PRMREG1=OTPTWAIT;/* OUTPUT WAIT  @YM03601*/
         L     R1,OTPTWAIT                                         0182
*                         ELSE      /* DATA ON QUEUE         @YM03601*/
*                           IF TVWAPRMT='1'B&TVWAP1ST='0'B/* @YM03601*/
*                             THEN  /* FIRST PROMPT          @YM03601*/
         B     @RC00181                                            0183
@RF00181 TM    TVWAFLG5,TVWAPRMT                                   0183
         BNO   @RF00183                                            0183
         TM    TVWAFLG5,TVWAP1ST                                   0183
         BNZ   @RF00183                                            0183
*                             DO;   /*                       @YM03601*/
*                               TVWAP1ST='1'B;/* NOT 1ST PROMPT    0185
*                                                            @YM03601*/
         OI    TVWAFLG5,TVWAP1ST                                   0185
*                               PRMREG1=OTPTWAIT;/* OUTPUT WAIT    0186
*                                                            @YM03601*/
         L     R1,OTPTWAIT                                         0186
*                             END;  /*                       @YM03601*/
*                           ELSE    /*                       @YM03601*/
*                             PRMREG1=INPTWAIT;/* INPUT WAIT @YM03601*/
         B     @RC00183                                            0188
@RF00183 SLR   R1,R1                                               0188
*                     ELSE                                         0189
*                       ;           /* TOM IS NOT RUNNING    @YM03259*/
@RF00178 DS    0H                                                  0190
*                   ELSE                                           0190
*                     ;             /* KEYBOARD IS LOCKED    @YM03259*/
@RF00177 DS    0H                                                  0191
*                 IF PRMREG1=INPTWAIT THEN/* IWAIT REQUESTED @ZA24418*/
@RC00177 DS    0H                                                  0191
@RC00173 LTR   R1,R1                                               0191
         BNZ   @RF00191                                            0191
*                   DO;             /*                       @ZA24418*/
*                     ;             /* DELETED TESTING FOR TVWAIOP 0193
*                                                            @ZA28881*/
*                     MPSTSOIW='1'B;/* TELL VTAM IWAIT WAS ISSUED  0194
*                                                            @ZA28881*/
         L     R4,PSAAOLD                                          0194
         L     R4,ASCBASXB-ASCB(,R4)                               0194
         L     R4,ASXBMPST-ASXB(,R4)                               0194
         OI    MPSTSOIW(R4),B'10000000'                            0194
*                     TVWAISYS='1'B;/* SET IWAIT INDICATOR   @ZA28881*/
         OI    TVWAFLG6,TVWAISYS                                   0195
*                   END;            /*                       @ZA24418*/
*                 ELSE                                             0197
*                   ;               /*                       @ZA24418*/
@RF00191 DS    0H                                                  0198
*                 IF PRMREG1^=1     /* ISSUE SYSEVENT?       @YM03259*/
*                   THEN            /* ISSUE SYSEVENT        @YM03259*/
         C     R1,FW1                                              0198
         BE    @RF00198                                            0198
*                   DO;             /* SET UP SYSEVENT               */
*                     REG2=ASCBASID;/* GET THIS ADDR. SPCS ASID      */
         L     R4,PSAAOLD                                          0200
         LH    R2,ASCBASID-ASCB(,R4)                               0200
         N     R2,HEXFFFF                                          0200
*                     GEN REFS(ASCBASID,CVTPTR,CVTOPTE,CVT) SETS(  0201
*                         PRMREG0,RTNREG,ENTRYREG);                0201
***
***         INFORM SYSTEM RESOURCE MANAGER OF WAIT
***
            SYSEVENT TERMWAIT,ASID=(R2),ENTRY=BRANCH
*                     RFY                                          0202
*                      (PRMREG1,                                   0202
*                       REG2) UNRSTD;                              0202
*                   END;            /* END OF SYSEVENT               */
*                 ELSE                                             0204
*                   ;               /* DO NOT ISSUE SYSEVENT         */
@RF00198 DS    0H                                                  0205
*                 TSBIWAIT='1'B;    /* INDICATE IWAIT CONDITION      */
         OI    TSBFLG4,TSBIWAIT                                    0205
*                                                                  0206
*                 /***************************************************/
*                 /*                                                 */
*                 /* PREPARE TO SUSPEND CALLER VIA BRANCH TO THE     */
*                 /* STATUS ROUTINE. LOCAL LOCK HELD ON ENTRY TO     */
*                 /* STATUS ROUTINE.                                 */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0206
*                 STM(PRMREG0,ENTRYREG,TCBGRS);/* SAVE REGISTERS     */
         L     R4,XSATCBA-1-RBBASIC(,R5)                           0206
         LA    R4,0(,R4)                                           0206
         STM   R0,R15,TCBGRS-TCB(R4)                               0206
*                 RBOPSW2=ADDR(REDISPCH);/* SET UP RESUME PSW TELL 0207
*                                      STATUS TO RE-DISPATCH TASK AT
*                                      LABEL 'REDISPCH'              */
         LA    R15,REDISPCH                                        0207
         ST    R15,RBOPSW2-RBBASIC(,R5)                            0207
*                 RFY                                              0208
*                  (PRMREG0,                                       0208
*                   PRMREG1) RSTD;                                 0208
*                 SAVEREG=NDISPBIT; /* SET NON-DISPATCHABLE FLAG     */
         LA    R13,2048                                            0209
*                 PRMREG0=SDTCB;    /* SET ENTRY CODE                */
         LA    R0,15                                               0210
*                 PRMREG1=XSATCBA;  /* SET TCB TO SUSPEND            */
         LR    R1,R4                                               0211
*                 CALL STATUS;      /* CALL STATUS TO SUSPEND TASK   */
         L     R4,CVTPTR                                           0212
         L     R4,CVTABEND-CVT(,R4)                                0212
         L     R15,SCVTSTAT-SCVTSECT(,R4)                          0212
         BALR  R14,R15                                             0212
*                 RFY                                              0213
*                  (PRMREG0,                                       0213
*                   PRMREG1) UNRSTD;                               0213
*                                                                  0213
*                 /***************************************************/
*                 /*                                                 */
*                 /* UPON BEING RE-DISPATCHED, LOCAL LOCK MUST BE    */
*                 /* RE-OBTAINED                                     */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0214
*REDISPCH:                                                         0214
*                 RFY                                              0214
*                  (RTNREG,                                        0214
*                   ENTRYREG,                                      0214
*                   REG0,                                          0214
*                   REG1) RSTD;                                    0214
REDISPCH DS    0H                                                  0215
*                 GEN SETS(RTNREG,ENTRYREG,REG0,REG1) REFS(PSALITA,FLC)
*                     ;                                            0215
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,REGS=USE,              --
               RELATED=(TSB,IKTVTGET(RETURN))
*                 RFY                                              0216
*                  (RTNREG,                                        0216
*                   ENTRYREG,                                      0216
*                   REG0,                                          0216
*                   REG1) UNRSTD;                                  0216
*               END;                /* END OF NO DATA AVAILABLE,   0217
*                                      SUSPEND CALLER                */
*           END;                    /* END OF NO DATA AVAILABLE      */
*/*                                                                  */
*/*  *****************************************************************/
*/*                                                                  */
*/*            DATA IS AVAILABLE. EDIT DATA AND MOVE INTO            */
*/*            CALLER'S BUFFER. EDIT ROUTINES UPDATE XSAPRMSZ,       */
*/*            XSABFRAD, IPBFSZ,IPBUFADR                             */
*/*                                                                  */
*/*  *****************************************************************/
*                                                                  0219
*         ELSE                                                     0219
*           DO;                                                    0219
         B     @RC00139                                            0219
@RF00139 DS    0H                                                  0220
*             IF XSADATVF='0'B THEN /* IF NOT DONE BEFORE, THEN GO 0220
*                                      VERIFY THAT CALLER'S ADDRESS
*                                      IS VALID                      */
         TM    XSAFLAG-RBBASIC(R5),XSADATVF                        0220
         BNZ   @RF00220                                            0220
*               CALL VERIFY;                                       0221
         BAL   R14,VERIFY                                          0221
*             RFY                                                  0222
*              (PRMREG0,                                           0222
*               PRMREG1) RSTD;                                     0222
*                                                                  0222
@RF00220 DS    0H                                                  0223
*             /*******************************************************/
*             /*                                                     */
*             /* SET UP R0 AND R1 TO POINT TO XSA AND IPARMS FOR     */
*             /* EXTERNAL EDIT ROUTINES                              */
*             /*                                                     */
*             /*******************************************************/
*                                                                  0223
*             PRMREG0=ADDR(XSA);    /* PT. TO EXTENDED SAVE AREA     */
         LA    R0,XSA-RBBASIC(,R5)                                 0223
*             PRMREG1=ADDR(IPARMS); /* PT. TO QMGR PARAM. LIST       */
         LA    R1,IPARMS(,R13)                                     0224
*             IF IPTRMTYP=TUSERDEF THEN/* CHECK FOR USER.DEFINED   0225
*                                      DEVICE TYPE                   */
         CLI   IPTRMTYP(R13),3                                     0225
         BNE   @RF00225                                            0225
*               CALL IKTGETXT;      /* CALL USER EDIT FOR SPCL DEVIC */
         L     R15,IKTGETXT                                        0226
         BALR  R14,R15                                             0226
*             ELSE                                                 0227
*               IF IPTRMTYP=IDS|IPTRMTYP=TNDS THEN/* CALL          0227
*                                      EDIT@G58AK3A                  */
         B     @RC00225                                            0227
@RF00225 CLI   IPTRMTYP(R13),1                                     0227
         BE    @RT00227                                            0227
         CLI   IPTRMTYP(R13),4                                     0227
         BNE   @RF00227                                            0227
@RT00227 DS    0H                                                  0228
*                 DO;               /* FOR 3270 OR NDS               */
*                   IF ADDR(IKTIDSX4)^=0 THEN/* IF EXIT ROUTINE, CALL
*                                      IT                            */
         L     R4,IKTIDSX4                                         0229
         LTR   R4,R4                                               0229
         BZ    @RF00229                                            0229
*                     DO;                                          0230
*                       CALL IKTIDSX4;/* INSTALLATION EXIT           */
         L     R15,IKTIDSX4                                        0231
         BALR  R14,R15                                             0231
*                       RFY                                        0232
*                         RCREG RSTD;                              0232
*                       IF RCREG=NORMALSO THEN/* CALL NORMAL EDIT IF
*                                      REQUESTED BY EXIT             */
         C     R15,FW4                                             0233
         BNE   @RF00233                                            0233
*                         DO;                                      0234
*                           RFY                                    0235
*                             RCREG UNRSTD;                        0235
*                           CALL EDIT3270;/* 3270 EDIT ROUTINE       */
         BAL   R14,EDIT3270                                        0236
*                         END;                                     0237
*                     END;          /* END 'EXIT RTN FOR 3270' DO    */
*                   ELSE                                           0239
*                     CALL EDIT3270;/* IF NO EXIT ROUTINE, CALL    0239
*                                      NORMAL 3270 EDIT ROUTINE E    */
         B     @RC00229                                            0239
@RF00229 BAL   R14,EDIT3270                                        0239
*                 END;                                             0240
*               ELSE                                               0241
*                 IF IPTRMTYP=T3767 THEN/* 3767 OR 3770 TERMINAL     */
         B     @RC00227                                            0241
@RF00227 CLI   IPTRMTYP(R13),2                                     0241
         BNE   @RF00241                                            0241
*                   IF ADDR(IKTRTX4)^=0 THEN/* IF EXIT ROUTINE, CALL
*                                      IT                            */
         L     R4,IKTRTX4                                          0242
         LTR   R4,R4                                               0242
         BZ    @RF00242                                            0242
*                     DO;                                          0243
*                       CALL IKTRTX4;/* INSTALLATION EXIT            */
         L     R15,IKTRTX4                                         0244
         BALR  R14,R15                                             0244
*                       RFY                                        0245
*                         RCREG RSTD;                              0245
*                       IF RCREG=NORMALSO THEN/* CALL NORMAL EDIT IF
*                                      REQUESTED BY EXIT             */
         C     R15,FW4                                             0246
         BNE   @RF00246                                            0246
*                         DO;                                      0247
*                           RFY                                    0248
*                             RCREG UNRSTD;                        0248
*                           CALL EDIT3767;/* 3767 AND 3770 EDIT    0249
*                                      ROUTINE E                     */
         BAL   R14,EDIT3767                                        0249
*                         END;                                     0250
*                     END;                                         0251
*                   ELSE                                           0252
*                     CALL EDIT3767;/* IF NO EXIT ROUTINE, CALL    0252
*                                      NORMAL 3767 EDIT ROUTINE      */
         B     @RC00242                                            0252
@RF00242 BAL   R14,EDIT3767                                        0252
*             RFY                                                  0253
*              (PRMREG0,                                           0253
*               PRMREG1) UNRSTD;                                   0253
@RC00242 DS    0H                                                  0253
@RF00241 DS    0H                                                  0253
@RC00227 DS    0H                                                  0253
@RC00225 DS    0H                                                  0254
*             XSADATSZ=XSADATSZ+XSACURDS;/* ACCUMULATE TOTAL INPUT 0254
*                                      DATA SIZE FOR RETURN TO     0254
*                                      CALLER. EDIT ROUTINES SET   0254
*                                      XSACURDS TO AMOUNT OF DATA  0254
*                                      MOVED FROM THIS Q ELEMENT     */
*                                                                  0254
         LH    R4,XSADATSZ-RBBASIC(,R5)                            0254
         AH    R4,XSACURDS-RBBASIC(,R5)                            0254
         STH   R4,XSADATSZ-RBBASIC(,R5)                            0254
*             /*******************************************************/
*             /*                                                     */
*             /* AS MUCH DATA AS POSSIBLE HAS BEEN EDITED. NOW CHECK */
*             /* IF ALL HAS BEEN EDITED. (IT HAS UNLESS CALLER'S     */
*             /* BUFFER IS SMALLER THAN THE INPUT LINE SIZE)         */
*             /*                                                     */
*             /*******************************************************/
*                                                                  0255
*             IF IPBFSZ=0 THEN      /* CHECK IF ALL DATA MOVED       */
         LH    R4,IPBFSZ(,R13)                                     0255
         LTR   R4,R4                                               0255
         BNZ   @RF00255                                            0255
*               DO;                                                0256
*                 IPREQ=DELETE;     /* REQUEST THAT ELEMENT BE DEL'D */
*                                                                  0257
         MVI   IPREQ(R13),X'03'                                    0257
*                 /***************************************************/
*                 /*                                                 */
*                 /* CALL INPUT QUEUE MANAGER, IKTQMIN PARAMETER LIST*/
*                 /* INTERFACE IS BUILT DIRECTLY SO THAT PLS WILL NOT*/
*                 /* GENERATE ANY TEMPS ITSELF. DIRECTLY BUILT       */
*                 /* PARAMETER LIST IS IN LOCALLY LOCKED WORK AREA.  */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0258
*                 PRMLSTAD=ADDR(IPARMS);/* PUT ADDR INTO 'TEMP'      */
         LA    R4,IPARMS(,R13)                                     0258
         ST    R4,PRMLSTAD(,R13)                                   0258
*                 RFY                                              0259
*                   PRMREG1 RSTD;                                  0259
*                 PRMREG1=ADDR(PRMLSTAD);/* REG 1 POINTS TO PLS PARM
*                                      LST                           */
         LA    R1,PRMLSTAD(,R13)                                   0260
*                 CALL IKTQMIN;     /* CALL INPUT QUEUE MANAGER      */
         L     R4,CVTPTR                                           0261
         L     R4,CVTTCASP-CVT(,R4)                                0261
         L     R15,TCASIQM-TCAST(,R4)                              0261
         BALR  R14,R15                                             0261
*                 RFY                                              0262
*                   PRMREG1 UNRSTD;                                0262
*                 IF IPPRTL='1'B THEN/* PARTIAL LINE INDICATOR MAY BE
*                                      ON, INDICATING MORE DATA      */
         TM    IPPRTL(R13),B'00100000'                             0263
         BNO   @RF00263                                            0263
*                   XSADOSWT='1'B;  /* YES, SET SWITCH TO CONTINUE 0264
*                                      CALLING QUEUE MGR             */
         OI    XSAFLAG-RBBASIC(R5),XSADOSWT                        0264
*                 ELSE                                             0265
*                   DO;                                            0265
*                                                                  0265
         B     @RC00263                                            0265
@RF00263 DS    0H                                                  0266
*                     /***********************************************/
*                     /*                                             */
*                     /* TELL SRM THAT INPUT COMPLETED -'TGETTPUT'   */
*                     /* AND ALL DATA MOVED                          */
*                     /*                                             */
*                     /***********************************************/
*                                                                  0266
*                     RFY                                          0266
*                      (PRMREG1,                                   0266
*                       REG2) RSTD;                                0266
*                     PRMREG1=TGETIND;/* INDICATE TGET(BIT 0 =0) AND
*                                      NO MORE DATA (BIT 1 =0)       */
         SLR   R1,R1                                               0267
*                     REG2=ASCBASID;/* GET ASID OF THIS ADDR. SPC.   */
         L     R4,PSAAOLD                                          0268
         LH    R2,ASCBASID-ASCB(,R4)                               0268
         N     R2,HEXFFFF                                          0268
*                     GEN REFS(ASCBASID,CVTPTR,CVTOPTE,CVT) SETS(  0269
*                         PRMREG0,RTNREG,ENTRYREG);                0269
***
***      INFORM SRM OF TGET COMPLETION
***
         SYSEVENT TGETTPUT,ASID=(R2),ENTRY=BRANCH
*                     RFY                                          0270
*                      (PRMREG1,                                   0270
*                       REG2) UNRSTD;                              0270
*                     XSARC=0;      /* ALL DATA MOVED SUCCESSFULLY   */
         MVI   XSARC-RBBASIC(R5),X'00'                             0271
         TM    TSBFLG5,TSBNEDIT        IN NOEDIT MODE?        ZP60009-A
         BNO   *+8                     NO                     ZP60009-A
         MVI   XSARC-RBBASIC(R5),X'18' YES, RC=00 -> RC=24    ZP60009-A
         NI    TVWAFLG8,255-TVWATGNO                          UZ57385-A
*                     IF TCBTCT^=0 THEN/* TEST FOR SMF RUNNING       */
         L     R4,XSATCBA-1-RBBASIC(,R5)                           0272
         L     R4,TCBTCT-TCB(,R4)                                  0272
         LTR   R4,R4                                               0272
         BZ    @RF00272                                            0272
*                       TCTLIN=TCTLIN+1;/* IF SO, INCR NO. OF TGET'S */
         LA    R15,1                                               0273
         AL    R15,TCTLIN-SMFTCT(,R4)                              0273
         ST    R15,TCTLIN-SMFTCT(,R4)                              0273
*                   END;            /* END 'NOT PARTIAL LINE' DO     */
*               END;                /* END'ALL DATA MOVED' SECTION   */
*             ELSE                  /* START 'NOT ALL DATA MOVED'    */
*               DO;                                                0276
         B     @RC00255                                            0276
@RF00255 DS    0H                                                  0277
*                 IPREQ=UPDATE;     /* REQUEST THAT ELE. BE UPDATED  */
*                                                                  0277
         MVI   IPREQ(R13),X'04'                                    0277
*                 /***************************************************/
*                 /*                                                 */
*                 /* CALL INPUT QUEUE MANAGER, IKTQMIN PARAMETER LIST*/
*                 /* INTERFACE IS BUILT DIRECTLY SO THAT PLS WILL NOT*/
*                 /* GENERATE ANY TEMPS ITSELF. DIRECTLY BUILT       */
*                 /* PARAMETER LIST IS IN LOCALLY LOCKED WORK AREA.  */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0278
*                 PRMLSTAD=ADDR(IPARMS);/* PUT ADDR INTO 'TEMP'      */
         LA    R4,IPARMS(,R13)                                     0278
         ST    R4,PRMLSTAD(,R13)                                   0278
*                 RFY                                              0279
*                   PRMREG1 RSTD;                                  0279
*                 PRMREG1=ADDR(PRMLSTAD);/* REG 1 POINTS TO PLS PARM
*                                      LST                           */
         LA    R1,PRMLSTAD(,R13)                                   0280
*                 CALL IKTQMIN;     /* CALL INPUT QUEUE MANAGER      */
         L     R4,CVTPTR                                           0281
         L     R4,CVTTCASP-CVT(,R4)                                0281
         L     R15,TCASIQM-TCAST(,R4)                              0281
         BALR  R14,R15                                             0281
*                 RFY                                              0282
*                   PRMREG1 UNRSTD;                                0282
*                                                                  0283
*                 /***************************************************/
*                 /*                                                 */
*                 /* TELL SRM THAT INPUT COMPLETED-'TGETTPUT' AND    */
*                 /* THAT ALL DATA NOT MOVED                         */
*                 /*                                                 */
*                 /***************************************************/
*                                                                  0283
*                 RFY                                              0283
*                  (PRMREG1,                                       0283
*                   REG2) RSTD;                                    0283
*                 PRMREG1=MOREDATA; /* INDICATE TGET(BIT 0=0) AND  0284
*                                      MORE DATA (BIT 1=1)           */
         L     R1,MOREDATA                                         0284
*                 REG2=ASCBASID;    /* GET THIS ADDR. SPCS. ASID     */
         L     R4,PSAAOLD                                          0285
         LH    R2,ASCBASID-ASCB(,R4)                               0285
         N     R2,HEXFFFF                                          0285
*                 GEN REFS(ASCBASID,CVTPTR,CVTOPTE,CVT) SETS(PRMREG0,
*                     RTNREG,ENTRYREG);                            0286
***
***      INFORM SRM OF TGET COMPLETION
***
         SYSEVENT TGETTPUT,ASID=(R2),ENTRY=BRANCH
*                 RFY                                              0287
*                  (PRMREG1,                                       0287
*                   REG2) UNRSTD;                                  0287
*                 XSARC=TOOSMALL;   /* TELL CALLER MORE DATA THAN CAN
*                                      FIT IN CALLER'S BUFFER        */
         MVI   XSARC-RBBASIC(R5),X'0C'                             0288
         TM    TSBFLG5,TSBNEDIT        IN NOEDIT MODE?        ZP60009-A
         BNO   *+8                     NO                     ZP60009-A
         MVI   XSARC-RBBASIC(R5),X'1C' YES, RC=12 -> RC=28    ZP60009-A
         NI    TVWAFLG8,255-TVWATGNO                          UZ57385-A
*                 IF TCBTCT^=0 THEN /* TEST FOR SMF RUNNING          */
         L     R4,XSATCBA-1-RBBASIC(,R5)                           0289
         L     R4,TCBTCT-TCB(,R4)                                  0289
         LTR   R4,R4                                               0289
         BZ    @RF00289                                            0289
*                   TCTLIN=TCTLIN+1;/* IF SO, INCR NO. OF TGET'S     */
         LA    R15,1                                               0290
         AL    R15,TCTLIN-SMFTCT(,R4)                              0290
         ST    R15,TCTLIN-SMFTCT(,R4)                              0290
*               END;                /* END 'NOT ALL DATA MOVED'      */
@RF00289 DS    0H                                                  0292
*           END;                    /* END OF DATA AVAILABLE  SECTION*/
@RC00255 DS    0H                                                  0293
*       END;                        /* END OF XSARC = SUCCESS DO     */
@RC00139 DS    0H                                                  0294
*   END;                            /* END OF DO UNTIL XSADOSW= '0'  */
@RT00130 DS    0H                                                  0294
@DE00127 TM    XSAFLAG-RBBASIC(R5),XSADOSWT                        0294
         BNZ   @DL00127                                            0294
*                                                                  0295
*/*                                                                  */
*/********************************************************************/
*/*            TGET RETURN POINT. RELEASE LOCAL LOCK,                */
*/*            RESTORE TCBFX VALUE AND RETURN TO CALLER              */
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*                                                                  0295
*   TCBTIOTG='0'B;                  /* CLEAR ATTN INDICATOR IN TCB   */
         L     R4,XSATCBA-1-RBBASIC(,R5)                           0295
         NI    TCBTSFLG-TCB(R4),255-TCBTIOTG                       0295
*   TSBATTN='0'B;                   /* CLEAR ATTN INDICATOR IN TSB   */
         NI    TSBFLG3,255-TSBATTN                                 0296
*   GEN SETS(RTNREG,ENTRYREG,REG0,REG1) REFS(PSALITA,FLC);         0297
         SETLOCK RELEASE,TYPE=LOCAL,REGS=USE,                         --
               RELATED=(TSB,IKTVTGET(REDISPCH))
*   IF XSATCBFX='1'B THEN           /* IF ASYNCHRONOUS EXITS WERE  0298
*                                      ALLOWED ON ENTRY,             */
         TM    XSAFLAG-RBBASIC(R5),XSATCBFX                        0298
         BNO   @RF00298                                            0298
*     TCBFX='0'B;                   /* RESET TCB ASYN. EXIT INDIC.   */
         L     R4,XSATCBA-1-RBBASIC(,R5)                           0299
         NI    TCBFLGS1-TCB(R4),255-TCBFX                          0299
*   RFY                                                            0300
*     PRMREG1 RSTD;                                                0300
@RF00298 DS    0H                                                  0301
*   PRMREG1=XSADATSZ;               /* RETURN INPUT DATA SIZE TO   0301
*                                      CALLER IN REG 1               */
         LH    R1,XSADATSZ-RBBASIC(,R5)                            0301
*   RFY                                                            0302
*     RTNREG RSTD;                                                 0302
*   RTNREG=XSARETG;                 /* RESTORE RETURN ADDRESS        */
         L     R14,XSARETG-RBBASIC(,R5)                            0303
*   RETURN CODE(XSARC);             /* RETURN TO ADDRESS THAT WAS IN
*                                      R14 ON ENTRY,WITH TGET CODE   */
         SLR   R15,R15                                             0304
         IC    R15,XSARC-RBBASIC(,R5)                              0304
@EL00001 DS    0H                                                  0304
@EF00001 DS    0H                                                  0304
@ER00001 BR    R14                                                 0304
*   RFY                                                            0305
*     RTNREG UNRSTD;                                               0305
*   RFY                                                            0306
*     PRMREG1 UNRSTD;                                              0306
*TERMSTAT:                                                         0307
*   PROC OPTIONS(NOSAVEAREA,NOSAVE);                               0307
TERMSTAT DS    0H                                                  0308
*                                                                  0308
*/*                                                                  */
*/********************************************************************/
*/*            TERMSTAT IS AN INTERNAL PROCEDURE CALLED TO DETERMINE */
*/*            THE STATUS OF THE TERMINAL.                           */
*/*            IT SETS XSARC ON RETURN:                              */
*/*             = 0 - NORMAL                                         */
*/*             = 14 HEX - TERMINAL SESSION HAS BEEN CANCELLED       */
*/*             =  8 - ATTENTION WAS HIT                             */
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*                                                                  0308
*   RFY                                                            0308
*     RTNREG RSTD;                                                 0308
*   XSASAVEA=RTNREG;                /* SAVE RETURN ADDRESS           */
         ST    R14,XSASAVEA-RBBASIC(,R5)                           0309
*   RFY                                                            0310
*     RTNREG UNRSTD;                                               0310
*   IF TSBCANC='1'B THEN            /* TEST TERMINAL CANCELLED       */
         TM    TSBFLG4,TSBCANC                                     0311
         BNO   @RF00311                                            0311
*     XSARC=TERMCNCL;               /* IF YES, SET RC = CANCELLED    */
         MVI   XSARC-RBBASIC(R5),X'14'                             0312
*   ELSE                                                           0313
*     IF TCBTIOTG='1'B THEN         /* TEST ATTN HIT                 */
         B     @RC00311                                            0313
@RF00311 L     R4,XSATCBA-1-RBBASIC(,R5)                           0313
         TM    TCBTSFLG-TCB(R4),TCBTIOTG                           0313
         BNO   @RF00313                                            0313
*       XSARC=ATTEN;                /* IF YES, SET RC TO ATTEN       */
         MVI   XSARC-RBBASIC(R5),X'08'                             0314
*     ELSE                                                         0315
*       XSARC=SUCCESS;              /* OTHERWISE, SET RC SUCCESSFUL  */
         B     @RC00313                                            0315
@RF00313 IC    R4,SUCCESS                                          0315
         STC   R4,XSARC-RBBASIC(,R5)                               0315
*   RFY                                                            0316
*     RTNREG RSTD;                                                 0316
@RC00313 DS    0H                                                  0316
@RC00311 DS    0H                                                  0317
*   RTNREG=XSASAVEA;                /* RESTORE RETURN ADDRESS        */
         L     R14,XSASAVEA-RBBASIC(,R5)                           0317
*   RETURN;                                                        0318
@EL00002 DS    0H                                                  0318
@EF00002 DS    0H                                                  0318
@ER00002 BR    R14                                                 0318
*   RFY                                                            0319
*     RTNREG UNRSTD;                                               0319
*   END TERMSTAT;                                                  0320
*VERIFY:                                                           0321
*   PROC OPTIONS(NOSAVEAREA,NOSAVE);                               0321
*                                                                  0321
VERIFY   DS    0H                                                  0322
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*/*      THIS TESTS IF CALLER'S DATA ADDRESS IS IN THE CALLER'S      */
*/*      KEY. IT DOES SO BY REFERENCING THE FIRST BYTE OF EACH       */
*/*      PAGE REQUIRED FOR THE TPUT.                                 */
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*                                                                  0322
*   DCL                                                            0322
*     APAGE FIXED(15) CONSTANT(4096);/* PAGE SIZE                    */
*   DCL                                                            0323
*     MODULUS FIXED(8) CONSTANT(12);/* MODULUS OF A VS2 PAGE IS    0323
*                                      2**12=4096                    */
*   RFY                                                            0324
*     RTNREG RSTD;                                                 0324
*   XSASAVEA=RTNREG;                /* SAVE RETURN ADDRESS           */
         ST    R14,XSASAVEA-RBBASIC(,R5)                           0325
*   RFY                                                            0326
*     RTNREG UNRSTD;                                               0326
*   XSADATVF='1'B;                  /* SET INDICATOR TO 'VERIFIED'   */
*   XSADMOVE='1'B;                  /* INDICATE 'DATA VERIFICATION 0328
*                                      UNDERWAY'                     */
         OI    XSAFLAG-RBBASIC(R5),XSADMOVE+XSADATVF               0328
*/*    GET INTO CALLER'S KEY TO VALIDATE DATA                        */
*                                                                  0329
*   RFY                                                            0329
*     REG2 RSTD;                                                   0329
*   REG2=RBLINKB->RBOPSWB2;         /* OBTAIN CALLERS KEY FROM RB    */
         L     R4,RBLINKB-1-RBBASIC(,R5)                           0330
         SLR   R2,R2                                               0330
         IC    R2,RBOPSWB2-RBBASIC(,R4)                            0330
*   GEN SETS(REG2);                                                0331
        MODESET KEYADDR=(2),WORKREG=2   GET IN TASK S KEY
*   RFY                                                            0332
*     REG2 UNRSTD;                                                 0332
*   RFY                                                            0333
*     WORK1REG RSTD;                                               0333
*   WORK1REG=XSABFRAD;              /* GET INPUT BUFFER ADDRESS      */
         L     R1,XSABFRAD-1-RBBASIC(,R5)                          0334
         LA    R1,0(,R1)                                           0334
*   SRL(WORK1REG,MODULUS);          /* SHIFT OFF PARTIAL PAGE        */
         SRL   R1,12                                               0335
*   SLL(WORK1REG,MODULUS);          /* SHIFT IN ZEROS                */
         SLL   R1,12                                               0336
*/*                                                                  */
*/*       LOOP THRU EACH PAGE, MOVING FIRST BYTE TO ITSELF           */
*/*                                                                  */
*                                                                  0337
*   RFY                                                            0337
*     WORK2REG RSTD;                                               0337
*   WORK2REG=XSABFRAD+XSAPRMSZ;     /* DEFINE UPPER LIMIT            */
         L     R3,XSABFRAD-1-RBBASIC(,R5)                          0338
         LA    R3,0(,R3)                                           0338
         AH    R3,XSAPRMSZ-RBBASIC(,R5)                            0338
*                                                                  0339
*/*  IF PROGRAM CHECK OCCURS DURING BELOW DO WHILE, THEN IKT93EST    */
*/*  WILL CHANGE THE ABEND CODE TO '15D'X                            */
*                                                                  0339
*   DO WHILE WORK1REG<WORK2REG;                                    0339
         B     @DE00339                                            0339
@DL00339 DS    0H                                                  0340
*     GEN REFS(WORK1REG)(MVC 0(1,WORK1REG),0(WORK1REG));/* REFERENCE
*                                      PAGE                          */
         MVC 0(1,R1),0(R1)
*     WORK1REG=WORK1REG+APAGE;      /* INCREMENT BY PAGE SIZE        */
         AL    R1,FW4096                                           0341
*   END;                                                           0342
@DE00339 CR    R1,R3                                               0342
         BL    @DL00339                                            0342
*   RFY                                                            0343
*    (WORK1REG,                                                    0343
*     WORK2REG) UNRSTD;                                            0343
*                                                                  0343
*/*                                                                  */
*/*         GET BACK INTO KEY 0                                      */
*/*                                                                  */
*                                                                  0344
*   GEN(MODESET EXTKEY=ZERO);                                      0344
         MODESET EXTKEY=ZERO
*   XSADMOVE='0'B;                  /* CLEAR INDICATOR THAT DATA   0345
*                                      VERIFICATION WAS GOING ON     */
         NI    XSAFLAG-RBBASIC(R5),255-XSADMOVE                    0345
*   RFY                                                            0346
*     RTNREG RSTD;                                                 0346
*   RTNREG=XSASAVEA;                                               0347
         L     R14,XSASAVEA-RBBASIC(,R5)                           0347
*   RETURN;                                                        0348
@EL00003 DS    0H                                                  0348
@EF00003 DS    0H                                                  0348
@ER00003 BR    R14                                                 0348
*   RFY                                                            0349
*     RTNREG UNRSTD;                                               0349
*   END VERIFY;                                                    0350
*EDIT3767:                                                         0351
*   PROC OPTIONS(NOSAVEAREA,NOSAVE);                               0351
EDIT3767 DS    0H                                                  0352
*                                                                  0352
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*/*        EDIT3767 IS THE STANDARD TSO/VTAM EDIT ROUTINE FOR        */
*/*        THE 3767 AND 3770 TERMINALS. IT CHECKS FOR A TGET EDIT    */
*/*        REQUEST AND SCANS FOR INVALID DATA IF SO. IT MOVES THE    */
*/*        DATA FROM THE INPUT QUEUE TO THE CALLER'S DATA AREA       */
*/*        AND PADS WITH BLANKS IF EDIT OPTION.                      */
*/*                                                                  */
*/*        INPUT NEEDED---XSABFRAD - CALLER' DATA ADDRESS            */
*/*                       XSAPRMSZ - CALLER'S DATA AREA SIZE         */
*/*                       IPBUFADR - QUEUE ELEMENT ADDRESS           */
*/*                       IPBFSZ   - QUEUE ELEMENT SIZE              */
*/*                       XSAEDITO - OPTION IS EDIT IF='00'          */
*/*                                                                  */
*/*        OUTPUT-----EDIT3767 UPDATES THE DATA ADDRESSES AND        */
*/*                   AREA SIZES LISTED ABOVE.                       */
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*                                                                  0352
*   RFY                                                            0352
*     RTNREG RSTD;                                                 0352
*   XSASAVEA=RTNREG;                /* SAVE RETURN ADDRESS           */
         ST    R14,XSASAVEA-RBBASIC(,R5)                           0353
*   RFY                                                            0354
*     RTNREG UNRSTD;                                               0354
*   XSACURDS=0;                     /* CLEAR DATA MOVED COUNTER      */
         SLR   R4,R4                                               0355
         STH   R4,XSACURDS-RBBASIC(,R5)                            0355
*   IF XSAEDITO^='00'B THEN         /* 00 IS EDIT OPTION             */
         TM    XSAOPTNS-RBBASIC(R5),XSAEDITO                       0356
         BZ    @RF00356                                            0356
*/*                                                                  */
*/*       TGET ASIS. IF EITHER COUNT IS ZERO ON ENTRY,               */
*/*       NOTHING TO MOVE.                                           */
*/*                                                                  */
*                                                                  0357
*     IF XSAPRMSZ^=0&IPBFSZ^=0 THEN                                0357
**       LH    R15,XSAPRMSZ-RBBASIC(,R5)                           0357
**       CR    R15,R4                                              0357
**       BE    @RF00357                                            0357
         CH    R4,XSAPRMSZ-RBBASIC(,R5)
         BNL   @RF00357
**       LH    R14,IPBFSZ(,R13)                                    0357
**       CR    R14,R4                                              0357
**       BE    @RF00357                                            0357
         CH    R4,IPBFSZ(,R13)
         BNL   @RF00357
*         CALL MOVEASIS;
         BAL   R14,MOVEASIS
*         RFY                                                      0367
*           REG1 UNRSTD;                                           0367
*       END;                        /* END 'TGET ASIS' MOVE          */
*     ELSE                                                         0369
*       ;                           /* IF EITHER=0, NO DATA MOVE     */
@RF00357 DS    0H                                                  0370
*   ELSE                            /* 'TGET EDIT' DATA MOVE         */
*     DO;                                                          0370
*                                                                  0370
         B     @RC00356                                            0370
@RF00356 DS    0H                                                  0371
*       /*************************************************************/
*       /*                                                           */
*       /* SCAN DATA VIA 'TRT' INSTRUCTION, MOVING VALID DATA AND    */
*       /* POINTING PAST INVALID CHAR. CONTINUE UNTIL THERE IS NO    */
*       /* MORE DATA TO MOVE OR NO ROOM TO MOVE IT TO.               */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0371
*       DO WHILE XSAPRMSZ^=0&IPBFSZ^=0;                            0371
         B     @DE00371                                            0371
@DL00371 DS    0H                                                  0372
*         RFY                                                      0372
*           REG3 RSTD;                                             0372
*         REG3=MIN(IPBFSZ,XSAPRMSZ,256);/* NO MORE THAN 256 CHARS CAN
*                                      BE TESTED AT ONCE.            */
         LH    R3,IPBFSZ(,R13)                                     0373
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0373
         CR    R3,R4                                               0373
         BNH   *+6
         LR    R3,R4                                               0373
         LA    R4,256                                              0373
         CR    R3,R4                                               0373
         BNH   *+6
         LR    R3,R4                                               0373
*         RFY                                                      0374
*          (REG1,                                                  0374
*           REG2) RSTD;                                            0374
*         REG1=0;                   /* CLEAR REGISTER BEFORE TRT     */
         SLR   R1,R1                                               0375
*         TRT(IPSOURCE(1:REG3),TGETEDIT(1));/* SEARCH FOR INVALID  0376
*                                      CHAR                          */
         L     R4,IPBUFADR(,R13)                                   0376
         LR    R15,R3                                              0376
         BCTR  R15,0                                               0376
         EX    R15,@SB02692                                        0376
*         BC(CC0,DATAVALD);         /* BRCH IF ALL DATA VALID        */
         BC    8,DATAVALD                                          0377
*                                                                  0378
*         /***********************************************************/
*         /*                                                         */
*         /* NOT ALL DATA IS VALID..REG1 POINTS TO FIRST INVALID CHAR*/
*         /*                                                         */
*         /***********************************************************/
*                                                                  0378
*         REG3=REG1-IPBUFADR;       /* COMPUTE NO. OF VALID CHARS  0378
*                                      MOVED.                        */
         L     R4,IPBUFADR(,R13)                                   0378
         LR    R3,R1                                               0378
         SLR   R3,R4                                               0378
*         RFY                                                      0379
*          (REG1,                                                  0379
*           REG2) UNRSTD;                                          0379
*         IF REG3^=0 THEN           /* ANY DATA TO MOVE ?            */
         LTR   R3,R3                                               0380
         BZ    @RF00380                                            0380
*           DO;                     /* YES                           */
*             XSATARGT(1:REG3)=IPSOURCE(1:REG3);/* MOVE DATA         */
         L     R2,XSABFRAD-1-RBBASIC(,R5)                          0382
         LA    R2,0(,R2)                                           0382
         LR    R15,R3                                              0382
         BCTR  R15,0                                               0382
         EX    R15,@SM02694                                        0382
*             XSAPRMSZ=XSAPRMSZ-REG3;/* UPDATE CALLER'S SIZE         */
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0383
         SLR   R4,R3                                               0383
         STH   R4,XSAPRMSZ-RBBASIC(,R5)                            0383
*             XSABFRAD=XSABFRAD+REG3;/* UPDATE CALLER'S ADDRESS      */
         ALR   R2,R3                                               0384
         STCM  R2,7,XSABFRAD-RBBASIC(R5)                           0384
*             XSACURDS=XSACURDS+REG3;/* INCREMENT AMT OF DATA MOVED  */
         LR    R4,R3                                               0385
         AH    R4,XSACURDS-RBBASIC(,R5)                            0385
         STH   R4,XSACURDS-RBBASIC(,R5)                            0385
*           END;                                                   0386
*         IPBFSZ=IPBFSZ-REG3-1;     /* UPDATE QUEUE ELE SIZE TO AM'T
*                                      OF DATA MOVED PLUS THE INVALID
*                                      CHAR THAT IS DELETED.         */
@RF00380 LH    R4,IPBFSZ(,R13)                                     0387
         SLR   R4,R3                                               0387
         BCTR  R4,0                                                0387
         STH   R4,IPBFSZ(,R13)                                     0387
*         IPBUFADR=IPBUFADR+REG3+1; /* UPDATE QUEUE ELE ADDRESS TOO  */
         LR    R4,R3                                               0388
         AL    R4,IPBUFADR(,R13)                                   0388
         AL    R4,FW1                                              0388
         ST    R4,IPBUFADR(,R13)                                   0388
*         GO TO CONTIN1;            /* GO CONTINUE SEARCHING QUEUE 0389
*                                      ELE DATA                      */
         B     CONTIN1                                             0389
*                                                                  0390
*         /***********************************************************/
*         /*                                                         */
*         /* ALL DATA SCANNED IN 'TRT' WAS VALID                     */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0390
*DATAVALD:                                                         0390
*         XSATARGT(1:REG3)=IPSOURCE(1:REG3);/* MOVE DATA             */
DATAVALD L     R4,XSABFRAD-1-RBBASIC(,R5)                          0390
         LA    R4,0(,R4)                                           0390
         LR    R2,R3                                               0390
         BCTR  R2,0                                                0390
         L     R15,IPBUFADR(,R13)                                  0390
         EX    R2,@SM02696                                         0390
*         XSAPRMSZ=XSAPRMSZ-REG3;   /* UPDATE CALLER'S DATA SIZE     */
         LH    R2,XSAPRMSZ-RBBASIC(,R5)                            0391
         SLR   R2,R3                                               0391
         STH   R2,XSAPRMSZ-RBBASIC(,R5)                            0391
*         XSABFRAD=XSABFRAD+REG3;   /* UPDATE CALLER'S ADDRESS       */
         ALR   R4,R3                                               0392
         STCM  R4,7,XSABFRAD-RBBASIC(R5)                           0392
*         IPBFSZ=IPBFSZ-REG3;       /* UPDATE QUEUE ELE SIZE         */
         LH    R4,IPBFSZ(,R13)                                     0393
         SLR   R4,R3                                               0393
         STH   R4,IPBFSZ(,R13)                                     0393
*         IPBUFADR=IPBUFADR+REG3;   /* UPDATE QUEUE ELE ADDRESS      */
         ALR   R15,R3                                              0394
         ST    R15,IPBUFADR(,R13)                                  0394
*         XSACURDS=XSACURDS+REG3;   /* INCREMENT AMT OF DATA MOVED   */
         LR    R4,R3                                               0395
         AH    R4,XSACURDS-RBBASIC(,R5)                            0395
         STH   R4,XSACURDS-RBBASIC(,R5)                            0395
*CONTIN1:                                                          0396
*                                                                  0396
*         /***********************************************************/
*         /*                                                         */
*         /* CONTINUE WITH TRT LOOP                                  */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0396
*         RFY                                                      0396
*           REG3 UNRSTD;                                           0396
CONTIN1  DS    0H                                                  0397
*       END;                        /* END EDIT'S DO WHILE           */
@DE00371 SLR   R4,R4                                               0397
         CH    R4,XSAPRMSZ-RBBASIC(,R5)                            0397
         BNL   @DC00371                                            0397
         CH    R4,IPBFSZ(,R13)                                     0397
         BL    @DL00371                                            0397
@DC00371 DS    0H                                                  0398
*                                                                  0398
*       /*************************************************************/
*       /*                                                           */
*       /* EITHER IPBFSZ OR XSAPRMSZ IS ZERO 1. IF XSAPRMSZ ^= 0 AND */
*       /* THIS IS END OF INPUT(IPPRTL=0), THEN PAD REMAINING BUFFER */
*       /* WITH BLANKS. 2. IF IPBFSZ ^= 0, THEN SCAN FOR ANOTHER     */
*       /* VALID CHAR, ADJUSTING IPBFSZ AND IPBUFADR. THIS IS DONE SO*/
*       /* THAT IF CALLER IS RETURNED A CODE '0C'HEX, INDICATING THAT*/
*       /* MORE DATA FOLLOWS, THEN THERE SHOULD BE VALID DATA        */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0398
*       IF XSAPRMSZ>0&IPPRTL='0'B THEN                             0398
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0398
         LTR   R4,R4                                               0398
         BNP   @RF00398                                            0398
         TM    IPPRTL(R13),B'00100000'                             0398
         BNZ   @RF00398                                            0398
*         DO;                                                      0399
*                                                                  0399
*           /*********************************************************/
*           /*                                                       */
*           /* PAD INPUT AREA WITH BLANKS, BUT DON'T CHANGE XSAPRMSZ */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0400
*           RFY                                                    0400
*            (REG0,                                                0400
*             REG1,                                                0400
*             REG2,                                                0400
*             REG3) RSTD;                                          0400
*           REG0=XSABFRAD;          /* TARGET ADR FOR 'MVCL'         */
         L     R0,XSABFRAD-1-RBBASIC(,R5)                          0401
         ICM   R0,8,@CB02616                                       0401
*           REG1=XSAPRMSZ;          /* AM'T OF AREA TO MOVE INTO REG2
*                                      NOT SET SINCE COUNT WILL BE 0402
*                                      ZERO, CAUSING PAD CHAR TO BE
*                                      USED                          */
         LR    R1,R4                                               0402
*           REG3=BLANK;             /* PADDING CHAR IS A BLANK       */
         LA    R3,64                                               0403
*           SLL(REG3,24);           /* PUT INTO PAD POSITION         */
         SLL   R3,24                                               0404
*           MVCL(REG0,REG2);        /* MOVE IN BLANKS & SET COUNT=0  */
         MVCL  R0,R2                                               0405
*         END;                                                     0406
*       IF IPBFSZ>0 THEN            /* MORE DATA IN QUEUE ELE ? YES,
*                                      VALIDATE IT                   */
@RF00398 LH    R4,IPBFSZ(,R13)                                     0407
         LTR   R4,R4                                               0407
         BNP   @RF00407                                            0407
*         DO UNTIL REG3^=0|IPBFSZ=0;                               0408
@DL00408 DS    0H                                                  0409
*           REG3=MIN(IPBFSZ,256);   /* 'TRT' UP TO 256 CHARS         */
         LH    R3,IPBFSZ(,R13)                                     0409
         LA    R4,256                                              0409
         CR    R3,R4                                               0409
         BNH   *+6
         LR    R3,R4                                               0409
*           REG1=0;                 /* CLEAR REGISTER BEFORE TRT     */
         SLR   R1,R1                                               0410
*           TRT(IPSOURCE(1:REG3),TGETEDIT(1));/* SEE IF ANY VALID    */
         L     R4,IPBUFADR(,R13)                                   0411
         LR    R15,R3                                              0411
         BCTR  R15,0                                               0411
         EX    R15,@SB02692                                        0411
*           BC(CC0,CONTIN2);        /* BRANCH IF THERE IS VALID DATA */
         BC    8,CONTIN2                                           0412
*           REG3=REG1-IPBUFADR;     /* NO. OF VALID CHARS            */
         L     R4,IPBUFADR(,R13)                                   0413
         LR    R3,R1                                               0413
         SLR   R3,R4                                               0413
*           IF REG3=0 THEN          /* ANY VALID AT ALL ?            */
         LTR   R3,R3                                               0414
         BNZ   @RF00414                                            0414
*             DO;                   /* NO, KEEP SEARCHING            */
*               IPBFSZ=IPBFSZ-1;    /* UPDATE QUEUE ELE SIZE         */
         LH    R15,IPBFSZ(,R13)                                    0416
         BCTR  R15,0                                               0416
         STH   R15,IPBFSZ(,R13)                                    0416
*               IPBUFADR=IPBUFADR+1;/* UPDATE QUEUE ELE ADDRESS      */
         AL    R4,FW1                                              0417
         ST    R4,IPBUFADR(,R13)                                   0417
*             END;                                                 0418
*                                                                  0418
*           /*********************************************************/
*           /*                                                       */
*           /* FALL THRU TO CONTIN2.                                 */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0419
*CONTIN2:                                                          0419
*                                                                  0419
*           /*********************************************************/
*           /*                                                       */
*           /* REG3 IS 0 IF FIRST CHAR IN TRT WAS INVALID. OTHERWISE */
*           /* IT IS THE NO. OF VALID CHARS IF BRANCH WASN'T TAKEN & */
*           /* IT IS MIN(IPBFSZ,256) IF BRANCH WAS TAKEN. IF REG3 IS */
*           /* 0 THEN THE DO UNTIL WILL CONTINUE SEARCHING FOR A     */
*           /* VALID CHAR, UPDATING QUEUE ELE POINTERS (UNLESS IPBFSZ*/
*           /* BECOMES 0, IN WHICH CASE THERE IS NO MORE DATA)       */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0419
*         END;                      /* END DO UNTIL                  */
@RF00414 DS    0H                                                  0419
CONTIN2  DS    0H                                                  0419
@DE00408 SLR   R4,R4                                               0419
         CR    R3,R4                                               0419
         BNE   @DC00408                                            0419
         CH    R4,IPBFSZ(,R13)                                     0419
         BNE   @DL00408                                            0419
@DC00408 DS    0H                                                  0420
*       RFY                                                        0420
*        (REG0,                                                    0420
*         REG1,                                                    0420
*         REG2,                                                    0420
*         REG3) UNRSTD;                                            0420
@RF00407 DS    0H                                                  0421
*     END;                          /* END 'EDIT' OPTION PROCESSING  */
*   RFY                                                            0422
*     RTNREG RSTD;                                                 0422
@RC00356 DS    0H                                                  0423
*   RTNREG=XSASAVEA;                /* RESTORE RETURN ADDRESS        */
         L     R14,XSASAVEA-RBBASIC(,R5)                           0423
*   RETURN;                                                        0424
@EL00004 DS    0H                                                  0424
@EF00004 DS    0H                                                  0424
@ER00004 BR    R14                                                 0424
*   RFY                                                            0425
*     RTNREG UNRSTD;                                               0425
*   END EDIT3767;                                                  0426
MOVEASIS ST    R14,XSAWD8-RBBASIC(,R5)
*         DO;
*           RFY
*            (REG0,
*             REG1,
*             REG2,
*             REG3) RSTD;
*           REG0=XSABFRAD;          /* TARGET ADDR FOR 'MVCL'        */
         L     R4,XSABFRAD-1-RBBASIC(,R5)
         LA    R4,0(,R4)
         LR    R0,R4
*         REG1=MIN(XSAPRMSZ,IPBFSZ);/* GET MINIMUM FOR MOVE          */
         LH    R15,XSAPRMSZ-RBBASIC(,R5)
         LH    R14,IPBFSZ(,R13)
         LR    R1,R14
         CR    R1,R15
         BNH   *+6
         LR    R1,R15
*           REG2=IPBUFADR;          /* SOURCE ADDR FOR 'MVCL'        */
         L     R11,IPBUFADR(,R13)
         LR    R2,R11
*           REG3=REG1;              /* SAME SIZE                     */
         LR    R3,R1
*         XSAPRMSZ=XSAPRMSZ-REG1;   /* UPDATE CALLER'S SIZE          */
         SLR   R15,R1
         STH   R15,XSAPRMSZ-RBBASIC(,R5)
*         XSABFRAD=XSABFRAD+REG1;   /* UPDATE CALLER'S ADDRESS       */
         ALR   R4,R1
         STCM  R4,7,XSABFRAD-RBBASIC(R5)
*         IPBFSZ=IPBFSZ-REG1;       /* UPDATE QUEUE ELE SIZE         */
         SLR   R14,R1
         STH   R14,IPBFSZ(,R13)
*         IPBUFADR=IPBUFADR+REG1;   /* UPDATE QUEUE ELE ADDRESS      */
         ALR   R11,R1
         ST    R11,IPBUFADR(,R13)
*         XSACURDS=REG1;            /* SET AMOUNT OF DATA MOVED      */
         STH   R1,XSACURDS-RBBASIC(,R5)
*           MVCL(REG0,REG2);        /* MOVE DATA FROM INPUT BUFFER TO
*                                      CALLERS DATA AREA             */
         MVCL  R0,R2
*           RFY
*            (REG0,
*             REG1,
*             REG2,
*             REG3) UNRSTD;
         L     R14,XSAWD8-RBBASIC(,R5)
         BR    R14
*EDIT3270:                                                         0427
*   PROC OPTIONS(NOSAVEAREA,NOSAVE);                               0427
EDIT3270 DS    0H                                                  0428
*                                                                  0428
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*/*        EDIT3270 IS THE STANDARD TSO/VTAM EDIT ROUTINE FOR        */
*/*        THE 3270 TERMINAL. IT CHECKS FOR A TGET EDIT              */
*/*        REQUEST AND SCANS FOR INVALID DATA AND 3270 CONTROL CHARS */
*/*        IF SO. IT MOVES THE                                       */
*/*        DATA FROM THE INPUT QUEUE TO THE CALLER'S DATA AREA       */
*/*        AND PADS WITH BLANKS IF EDIT OPTION.                      */
*/*                                                                  */
*/*        INPUT NEEDED---XSABFRAD - CALLER' DATA ADDRESS            */
*/*                       XSAPRMSZ - CALLER'S DATA AREA SIZE         */
*/*                       IPBUFADR - QUEUE ELEMENT ADDRESS           */
*/*                       IPBFSZ   - QUEUE ELEMENT SIZE              */
*/*                       XSAEDITO - OPTION IS EDIT IF='00'          */
*/*                                                                  */
*/*        OUTPUT-----EDIT3270 UPDATES THE DATA ADDRESSES AND        */
*/*                   AREA SIZES LISTED ABOVE.                       */
*/*                                                                  */
*/********************************************************************/
*/*                                                                  */
*                                                                  0428
*   DCL                                                            0428
*     SBA BIT(8) CONSTANT('11'X);   /* 3270 START BUFFER ADDRES      */
*   RFY                                                            0429
*     RTNREG RSTD;                                                 0429
*   XSASAVEA=RTNREG;                /* SAVE RETURN ADDRESS           */
         ST    R14,XSASAVEA-RBBASIC(,R5)                           0430
*   RFY                                                            0431
*     RTNREG UNRSTD;                                               0431
*   XSACURDS=0;                     /* CLEAR DATA MOVED COUNTER      */
         SLR   R4,R4                                               0432
         STH   R4,XSACURDS-RBBASIC(,R5)                            0432
*   IF XSAEDITO^='00'B THEN         /* 00 IS EDIT OPTION             */
         TM    XSAOPTNS-RBBASIC(R5),XSAEDITO                       0433
         BZ    @RF00433                                            0433
*     DO;                                                          0434
*                                                                  0434
*/*                                                                  */
*/*     DECREMENT 'CONTROL CHARACTER COUNT' TO KEEP TRACK OF NUMBER  */
*/*     OF CONTROL CHARACTERS THAT COULD NOT BE MOVED                */
*/*                                                                  */
*                                                                  0435
*       IPCCC=MAX(IPCCC-XSAPRMSZ,0);/* MOVE A MAX OF XSAPRMSZ CONTROL
*                                      CHARACTERS FROM QUEUE         */
         LH    R15,XSAPRMSZ-RBBASIC(,R5)                           0435
         SLR   R14,R14                                             0435
         IC    R14,IPCCC(,R13)                                     0435
         SLR   R14,R15                                             0435
         CR    R14,R4                                              0435
         BNL   *+6
         LR    R14,R4                                              0435
         STC   R14,IPCCC(,R13)                                     0435
*                                                                  0436
*/*     TGET ASIS DATA MOVE. IF EITHER COUNT IS ZERO ON ENTRY,       */
*/*     NOTHING TO MOVE                                              */
*/*                                                                  */
*                                                                  0436
*       IF XSAPRMSZ>0&IPBFSZ>0 THEN                                0436
         CR    R15,R4                                              0436
         BNH   @RF00436                                            0436
         CH    R4,IPBFSZ(,R13)                                     0436
         BNL   @RF00436                                            0436
*         CALL MOVEASIS;
         BAL   R14,MOVEASIS
*       ELSE                                                       0451
*         ;                         /* IF EITHER=0, NO DATA MOVE     */
@RF00436 DS    0H                                                  0452
*     END;                          /* END 'TGET ASIS'               */
*   ELSE                            /* 'TGET EDIT' DATA MOVE         */
*     DO;                                                          0453
*                                                                  0453
         B     @RC00433                                            0453
@RF00433 DS    0H                                                  0454
*       /*************************************************************/
*       /*                                                           */
*       /* POINT PAST 3270 CONTROL CHARACTERS. IPCCC IS >=0 AND <=6. */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0454
*       IPBFSZ=IPBFSZ-IPCCC;        /* ADJUST DATA COUNT             */
         SLR   R4,R4                                               0454
         IC    R4,IPCCC(,R13)                                      0454
         LH    R15,IPBFSZ(,R13)                                    0454
         SLR   R15,R4                                              0454
         STH   R15,IPBFSZ(,R13)                                    0454
*       IPBUFADR=IPBUFADR+IPCCC;    /* ADJUST DATA POINTER           */
         AL    R4,IPBUFADR(,R13)                                   0455
         ST    R4,IPBUFADR(,R13)                                   0455
*       IPCCC=0;                    /* SET CONTROL CHARACTER COUNT   */
         MVI   IPCCC(R13),X'00'                                    0456
*       IPBUFADR=IPBUFADR+XSAWD12;
         L     R14,XSAWD12-RBBASIC(,R5)
         ALR   R4,R14
         ST    R4,IPBUFADR(,R13)
*       IPBUFSZ=IPBUFSZ-XSAWD12;
         SLR   R15,R14
         STH   R15,IPBFSZ(,R13)
*                                                                  0457
*       /*************************************************************/
*       /*                                                           */
*       /* SCAN DATA VIA 'TRT' INSTRUCTION, MOVING VALID DATA AND    */
*       /* POINTING PAST INVALID CHAR. CONTINUE UNTIL THERE IS NO    */
*       /* MORE DATA TO MOVE OR NO ROOM TO MOVE IT TO.               */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0457
*       DO WHILE XSAPRMSZ^=0&IPBFSZ^=0;                            0457
         B     @DE00457                                            0457
@DL00457 DS    0H                                                  0458
*         RFY                                                      0458
*           REG3 RSTD;                                             0458
*         REG3=MIN(IPBFSZ,XSAPRMSZ,256);/* NO MORE THAN 256 CHARS CAN
*                                      BE TESTED AT ONCE.            */
         LH    R3,IPBFSZ(,R13)                                     0459
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0459
         CR    R3,R4                                               0459
         BNH   *+6
         LR    R3,R4                                               0459
         LA    R4,256                                              0459
         CR    R3,R4                                               0459
         BNH   *+6
         LR    R3,R4                                               0459
*         RFY                                                      0460
*          (REG1,                                                  0460
*           REG2) RSTD;                                            0460
*         REG1=0;                   /* CLEAR REGISTER BEFORE TRT     */
         SLR   R1,R1                                               0461
*         TRT(IPSOURCE(1:REG3),TGETEDIT(1));/* SEARCH FOR INVALID  0462
*                                      CHAR                          */
         L     R4,IPBUFADR(,R13)                                   0462
         LR    R15,R3                                              0462
         BCTR  R15,0                                               0462
         EX    R15,@SB02692                                        0462
*         BC(CC0,DATAOK);           /* BRANCH IF ALL DATA IS VALID   */
         BC    8,DATAOK                                            0463
*                                                                  0464
*         /***********************************************************/
*         /*                                                         */
*         /* NOT ALL DATA IS VALID..REG1 POINTS TO FIRST INVALID CHAR*/
*         /*                                                         */
*         /***********************************************************/
*                                                                  0464
*         REG3=REG1-IPBUFADR;       /* COMPUTE NO. OF VALID CHARS  0464
*                                      MOVED.                        */
         L     R4,IPBUFADR(,R13)                                   0464
         LR    R3,R1                                               0464
         SLR   R3,R4                                               0464
*         IF REG3^=0 THEN           /* ANY DATA TO MOVE ?            */
         LTR   R3,R3                                               0465
         BZ    @RF00465                                            0465
*           DO;                     /* YES                           */
*             XSATARGT(1:REG3)=IPSOURCE(1:REG3);/* MOVE DATA         */
         L     R15,XSABFRAD-1-RBBASIC(,R5)                         0467
         LA    R15,0(,R15)                                         0467
         LR    R14,R3                                              0467
         BCTR  R14,0                                               0467
         EX    R14,@SM02698                                        0467
*             XSAPRMSZ=XSAPRMSZ-REG3;/* UPDATE CALLER'S SIZE         */
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0468
         SLR   R4,R3                                               0468
         STH   R4,XSAPRMSZ-RBBASIC(,R5)                            0468
*             XSABFRAD=XSABFRAD+REG3;/* UPDATE CALLER'S ADDRESS      */
         ALR   R15,R3                                              0469
         STCM  R15,7,XSABFRAD-RBBASIC(R5)                          0469
*             XSACURDS=XSACURDS+REG3;/* INCREMENT AMT OF DATA MOVED  */
         LR    R4,R3                                               0470
         AH    R4,XSACURDS-RBBASIC(,R5)                            0470
         STH   R4,XSACURDS-RBBASIC(,R5)                            0470
*           END;                                                   0471
*         RFY                                                      0472
*           IPSOURCE BASED(REG1);   /* REFERENCE DATA VIA REGISTER 0472
*                                                            @YM03231*/
@RF00465 DS    0H                                                  0473
*         IF IPSOURCE(1)=SBA THEN   /* IS INVALID CHAR A START BUFFER
*                                      ADDRESS ?             @YM03231*/
         CLI   IPSOURCE(R1),X'11'                                  0473
         BNE   @RF00473                                            0473
*           REG2=3;                 /* YES...PULL OUT SBA,@,@        */
***      LA    R2,3                                                0474
         LA    R4,3
         LH    R2,IPBFSZ(,R13)
         SLR   R2,R3
         CR    R2,R4
         BNH   *+6
         LR    R2,R4
         SLR   R4,R2
         ST    R4,XSAWD12-RBBASIC(,R5)
*         ELSE                                                     0475
*           REG2=1;                 /* NO...JUST PULL OUT ONE CHAR   */
         B     @RC00473                                            0475
@RF00473 LA    R2,1                                                0475
*         RFY                                                      0476
*           IPSOURCE BASED(IPBUFADR);                              0476
@RC00473 DS    0H                                                  0477
*         IPBFSZ=IPBFSZ-REG3-REG2;  /* UPDATE QUEUE ELE SIZE TO AM'T
*                                      OF DATA MOVED PLUS THE INVALID
*                                      CHAR(S) THAT IS DELETED       */
         LH    R4,IPBFSZ(,R13)                                     0477
         SLR   R4,R3                                               0477
         SLR   R4,R2                                               0477
         STH   R4,IPBFSZ(,R13)                                     0477
*         IPBUFADR=IPBUFADR+REG3+REG2;/* UPDATE QUEUE ELE ADDRESS TOO*/
         LR    R4,R3                                               0478
         AL    R4,IPBUFADR(,R13)                                   0478
         ALR   R4,R2                                               0478
         ST    R4,IPBUFADR(,R13)                                   0478
*         RFY                                                      0479
*          (REG1,                                                  0479
*           REG2) UNRSTD;                                          0479
*         GO TO CONTIN3;            /* GO CONTINUE SEARCHING QUEUE 0480
*                                      ELE DATA                      */
         B     CONTIN3                                             0480
*                                                                  0481
*         /***********************************************************/
*         /*                                                         */
*         /* ALL DATA SCANNED IN 'TRT' WAS VALID                     */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0481
*DATAOK:                                                           0481
*         XSATARGT(1:REG3)=IPSOURCE(1:REG3);/* MOVE DATA             */
DATAOK   L     R4,XSABFRAD-1-RBBASIC(,R5)                          0481
         LA    R4,0(,R4)                                           0481
         LR    R2,R3                                               0481
         BCTR  R2,0                                                0481
         L     R15,IPBUFADR(,R13)                                  0481
         EX    R2,@SM02696                                         0481
*         XSAPRMSZ=XSAPRMSZ-REG3;   /* UPDATE CALLER'S DATA SIZE     */
         LH    R2,XSAPRMSZ-RBBASIC(,R5)                            0482
         SLR   R2,R3                                               0482
         STH   R2,XSAPRMSZ-RBBASIC(,R5)                            0482
*         XSABFRAD=XSABFRAD+REG3;   /* UPDATE CALLER'S ADDRESS       */
         ALR   R4,R3                                               0483
         STCM  R4,7,XSABFRAD-RBBASIC(R5)                           0483
*         IPBFSZ=IPBFSZ-REG3;       /* UPDATE QUEUE ELE SIZE         */
         LH    R4,IPBFSZ(,R13)                                     0484
         SLR   R4,R3                                               0484
         STH   R4,IPBFSZ(,R13)                                     0484
*         IPBUFADR=IPBUFADR+REG3;   /* UPDATE QUEUE ELE ADDRESS      */
         ALR   R15,R3                                              0485
         ST    R15,IPBUFADR(,R13)                                  0485
*         XSACURDS=XSACURDS+REG3;   /* INCREMENT AMT OF DATA MOVED   */
         LR    R4,R3                                               0486
         AH    R4,XSACURDS-RBBASIC(,R5)                            0486
         STH   R4,XSACURDS-RBBASIC(,R5)                            0486
*CONTIN3:                                                          0487
*                                                                  0487
*         /***********************************************************/
*         /*                                                         */
*         /* CONTINUE WITH TRT LOOP                                  */
*         /*                                                         */
*         /***********************************************************/
*                                                                  0487
*         RFY                                                      0487
*           REG3 UNRSTD;                                           0487
CONTIN3  DS    0H                                                  0488
*       END;                        /* END EDIT'S DO WHILE           */
@DE00457 SLR   R4,R4                                               0488
         CH    R4,XSAPRMSZ-RBBASIC(,R5)                            0488
         BNL   @DC00457                                            0488
         CH    R4,IPBFSZ(,R13)                                     0488
         BL    @DL00457                                            0488
@DC00457 DS    0H                                                  0489
*                                                                  0489
*       /*************************************************************/
*       /*                                                           */
*       /* EITHER IPBFSZ OR XSAPRMSZ IS ZERO 1. IF XSAPRMSZ > 0 AND  */
*       /* THIS IS END OF INPUT(IPPRTL=0), THEN PAD REMAINING BUFFER */
*       /* WITH BLANKS. 2. IF IPBFSZ > 0, THEN SCAN FOR ANOTHER      */
*       /* VALID CHAR, ADJUSTING IPBFSZ AND IPBUFADR. THIS IS DONE SO*/
*       /* THAT IF CALLER IS RETURNED A CODE '0C'HEX, INDICATING THAT*/
*       /* MORE DATA FOLLOWS, THEN THERE SHOULD BE VALID DATA        */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0489
*       IF XSAPRMSZ>0&IPPRTL='0'B THEN                             0489
         LH    R4,XSAPRMSZ-RBBASIC(,R5)                            0489
         LTR   R4,R4                                               0489
         BNP   @RF00489                                            0489
         TM    IPPRTL(R13),B'00100000'                             0489
         BNZ   @RF00489                                            0489
*         DO;                                                      0490
*                                                                  0490
*           /*********************************************************/
*           /*                                                       */
*           /* PAD INPUT AREA WITH BLANKS, BUT DON'T CHANGE XSAPRMSZ */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0491
*           RFY                                                    0491
*            (REG0,                                                0491
*             REG1,                                                0491
*             REG2,                                                0491
*             REG3) RSTD;                                          0491
*           REG0=XSABFRAD;          /* TARGET ADR FOR 'MVCL'         */
         L     R0,XSABFRAD-1-RBBASIC(,R5)                          0492
         ICM   R0,8,@CB02616                                       0492
*           REG1=XSAPRMSZ;          /* AM'T OF AREA TO MOVE INTO REG2
*                                      NOT SET SINCE COUNT WILL BE 0493
*                                      ZERO, CAUSING PAD CHAR TO BE
*                                      USED                          */
         LR    R1,R4                                               0493
*           REG3=BLANK;             /* PADDING CHAR IS A BLANK       */
         LA    R3,64                                               0494
*           SLL(REG3,24);           /* PUT INTO PAD POSITION         */
         SLL   R3,24                                               0495
*           MVCL(REG0,REG2);        /* MOVE IN BLANKS & SET COUNT=0  */
         MVCL  R0,R2                                               0496
*         END;                                                     0497
*       IF IPBFSZ>0 THEN            /* MORE DATA IN QUEUE ELE ? YES,
*                                      VALIDATE IT                   */
@RF00489 LH    R4,IPBFSZ(,R13)                                     0498
         LTR   R4,R4                                               0498
         BNP   @RF00498                                            0498
*         DO UNTIL REG3^=0|IPBFSZ=0;                               0499
@DL00499 DS    0H                                                  0500
*           REG3=MIN(IPBFSZ,256);   /* 'TRT' UP TO 256 CHARS         */
         LH    R3,IPBFSZ(,R13)                                     0500
         LA    R4,256                                              0500
         CR    R3,R4                                               0500
         BNH   *+6
         LR    R3,R4                                               0500
*           REG1=0;                 /* CLEAR REGISTER BEFORE TRT     */
         SLR   R1,R1                                               0501
*           TRT(IPSOURCE(1:REG3),TGETEDIT(1));/* SEE IF ANY VALID    */
         L     R4,IPBUFADR(,R13)                                   0502
         LR    R15,R3                                              0502
         BCTR  R15,0                                               0502
         EX    R15,@SB02692                                        0502
*           BC(CC0,CONTIN4);        /* BRANCH IF THERE IS VALID DATA */
         BC    8,CONTIN4                                           0503
*           REG3=REG1-IPBUFADR;     /* NO. OF VALID CHARS            */
         L     R4,IPBUFADR(,R13)                                   0504
         LR    R3,R1                                               0504
         SLR   R3,R4                                               0504
*           IF REG3=0 THEN          /* ANY VALID AT ALL ?            */
         LTR   R3,R3                                               0505
         BNZ   @RF00505                                            0505
*             DO;                   /* NO, KEEP SEARCHING            */
*               IPBFSZ=IPBFSZ-1;    /* UPDATE QUEUE ELE SIZE         */
         LH    R15,IPBFSZ(,R13)                                    0507
         BCTR  R15,0                                               0507
         STH   R15,IPBFSZ(,R13)                                    0507
*               IPBUFADR=IPBUFADR+1;/* UPDATE QUEUE ELE ADDRESS      */
         AL    R4,FW1                                              0508
         ST    R4,IPBUFADR(,R13)                                   0508
*             END;                                                 0509
*                                                                  0509
*           /*********************************************************/
*           /*                                                       */
*           /* FALL THRU TO CONTIN2.                                 */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0510
*CONTIN4:                                                          0510
*                                                                  0510
*           /*********************************************************/
*           /*                                                       */
*           /* REG3 IS 0 IF FIRST CHAR IN TRT WAS INVALID. OTHERWISE */
*           /* IT IS THE NO. OF VALID CHARS IF BRANCH WASN'T TAKEN & */
*           /* IT IS MIN(IPBFSZ,256) IF BRANCH WAS TAKEN. IF REG3 IS */
*           /* 0 THEN THE DO UNTIL WILL CONTINUE SEARCHING FOR A     */
*           /* VALID CHAR, UPDATING QUEUE ELE POINTERS (UNLESS IPBFSZ*/
*           /* BECOMES 0, IN WHICH CASE THERE IS NO MORE DATA)       */
*           /*                                                       */
*           /*********************************************************/
*                                                                  0510
*         END;                      /* END DO UNTIL                  */
@RF00505 DS    0H                                                  0510
CONTIN4  DS    0H                                                  0510
@DE00499 SLR   R4,R4                                               0510
         CR    R3,R4                                               0510
         BNE   @DC00499                                            0510
         CH    R4,IPBFSZ(,R13)                                     0510
         BNE   @DL00499                                            0510
@DC00499 DS    0H                                                  0511
*       RFY                                                        0511
*        (REG0,                                                    0511
*         REG1,                                                    0511
*         REG2,                                                    0511
*         REG3) UNRSTD;                                            0511
@RF00498 DS    0H                                                  0512
*     END;                          /* END 'EDIT' OPTION PROCESSING  */
*   RFY                                                            0513
*     RTNREG RSTD;                                                 0513
@RC00433 DS    0H                                                  0514
*   RTNREG=XSASAVEA;                /* RESTORE RETURN ADDRESS        */
         L     R14,XSASAVEA-RBBASIC(,R5)                           0514
*   RETURN;                                                        0515
@EL00005 DS    0H                                                  0515
@EF00005 DS    0H                                                  0515
@ER00005 BR    R14                                                 0515
         SPACE
SCHEDTOM DS    0H                                                  0147
*                   DO;             /* SET UP SCHEDULING OF TOM      */
*                     TVWATIS='1'B; /* INDICATE TOM SCHEDULED      0148
*                                      SCHEDULE TOM                  */
         OI    TVWAFLG1,TVWATIS                                    0148
*                     RFY                                          0149
*                       WORK1REG RSTD;                             0149
*                     WORK1REG=TSBXSRB;/* GET ADDRESS OF SRB         */
         L     R4,TSBEXTNT                                         0150
         L     R1,TSBXSRB-TSBX(,R4)                                0150
*                     GEN(SCHEDULE SRB=(WORK1REG),SCOPE=LOCAL)     0151
*                     REFS(TSBXSRB,CVTPTR,CVTLSMQ,CVTMAP,SRBFLNK,  0151
*                         SRBSECT);                                0151
         SCHEDULE SRB=(R1),SCOPE=LOCAL
*                     RFY                                          0152
*                       WORK1REG UNRSTD;                           0152
*                   END;            /* TOM HAS BEEN SCHEDULED        */
         BR    R14
*   RFY                                                            0516
*     RTNREG UNRSTD;                                               0516
*   END EDIT3270;                                                  0517
*   END IKTVTGET                                                   0518
*                                                                  0518
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IKTTCAST)                                       */
*/*%INCLUDE SYSLIB  (IKTTVWA )                                       */
*/*%INCLUDE SYSLIB  (IKJTSB  )                                       */
*/*%INCLUDE SYSLIB  (IKTTSBX )                                       */
*/*%INCLUDE SYSLIB  (IHASRB  )                                       */
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */
*/*%INCLUDE SYSLIB  (IHAASXB )                                       */
*/*%INCLUDE SYSLIB  (ISTPAB  )                                       */
*/*%INCLUDE SYSLIB  (ISTDYPAB)                                       */
*/*%INCLUDE SYSLIB  (ISTMPST )                                       */
*/*%INCLUDE SYSLIB  (IHASCVT )                                       */
*/*%INCLUDE SYSLIB  (IEFTCT  )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*/*%INCLUDE SYSLIB  (IHARB   )                                       */
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */
*/*%INCLUDE SYSLIB  (IKTIPARM)                                       */
*                                                                  0518
*       ;                                                          0518
         PRINT NOGEN
         IKTTCAST
         IKTTVWA
         IKJTSB EXT=YES
         IHASRB
         IHAASCB
         IHAASXB
         IHASCVT
         IEFTCT
         CVT   DSECT=YES
         IKJRB
         IHAPSA
         IKJTCB
         PRINT GEN
IKTVTGET CSECT
@DATA    DS    0H
@SB02692 TRT   IPSOURCE(0,R4),TGETEDIT
@SM02694 MVC   XSATARGT(0,R2),IPSOURCE(R4)
@SM02696 MVC   XSATARGT(0,R4),IPSOURCE(R15)
@SM02698 MVC   XSATARGT(0,R15),IPSOURCE(R4)
@DATD    DSECT
         DS    0F
IKTVTGET CSECT
         DS    0F
FW1      DC    F'1'
FW4      DC    F'4'
FW4096   DC    F'4096'
HEXFFFF  DC    XL4'0000FFFF'
@DATD    DSECT
         DS    0D
@ENDDATD EQU   *
IKTVTGET CSECT
         NOPR  ((@ENDDATD-@DATD)*16)
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
IKTRTX4  DC    V(IKTRTX4)
IKTIDSX4 DC    V(IKTIDSX4)
IKTGETXT DC    V(IKTGETXT)
         DS    0D
OTPTWAIT DC    XL4'80000000'
MOREDATA DC    XL4'40000000'
SUCCESS  DC    AL1(0)
@CB02616 DC    X'00'
         DS    CL2
PATCHLBL DC    20F'0'
TGETEDIT DC    5X'01'
         DC    X'00'
         DC    16X'01'
         DC    X'00'
         DC    41X'01'
         DC    192X'00'
         DS    0D                      END OF CSECT           ZP60009-A
         SPACE
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
         SPACE
ISTPAB   EQU   0
PABWQCHN EQU   ISTPAB
PABWEQA  EQU   PABWQCHN
PABCHAIN EQU   PABWQCHN+4
PABRPHFG EQU   ISTPAB+12
PABFLAGS EQU   PABRPHFG
PABERLCK EQU   PABFLAGS
PABERRLK EQU   PABERLCK
ISTDYPAB EQU   0
ISTMPST  EQU   0
MPSRSV07 EQU   ISTMPST+4
MPSTSOIW EQU   MPSRSV07
MPSRQUE  EQU   ISTMPST+8
MPSFLGA  EQU   ISTMPST+12
MPSCRA   EQU   ISTMPST+24
MPSCRACT EQU   MPSCRA+4
MPSSRB   EQU   ISTMPST+32
MPSSRBCT EQU   MPSSRB+4
MPSCRPL  EQU   ISTMPST+40
MPSRPLCT EQU   MPSCRPL+4
IPSOURCE EQU   0
XSATARGT EQU   0
DYNAMARA EQU   0
IPARMS   EQU   DYNAMARA+72
IPBUFADR EQU   IPARMS
IPREQ    EQU   IPARMS+4
IPRC     EQU   IPARMS+5
IPBFSZ   EQU   IPARMS+6
IPFLAGS  EQU   IPARMS+8
IPPRTL   EQU   IPFLAGS+1
IPCCC    EQU   IPFLAGS+2
IPTRMTYP EQU   IPFLAGS+3
IPNXFLGS EQU   IPARMS+16
PRMLSTAD EQU   DYNAMARA+92
IKTQMIN  EQU   0
STATUS   EQU   0
RBOPSW2  EQU   RBOPSW+4
XSA      EQU   RBEXSAVE
XSAPRM0  EQU   XSA
XSAPRMSZ EQU   XSAPRM0+2
XSAPRM1  EQU   XSA+4
XSAOPTNS EQU   XSAPRM1
XSANOWT  EQU   X'10'                                          ZP60009-C
XSAEDITO EQU   X'03'                                          ZP60009-C
XSABFRAD EQU   XSAPRM1+1
XSAWD3   EQU   XSA+8
XSAFLAG  EQU   XSAWD3
XSADMOVE EQU   X'40'                                          ZP60009-C
XSATCBFX EQU   X'20'                                          ZP60009-C
XSADOSWT EQU   X'10'                                          ZP60009-C
XSADATVF EQU   X'08'                                          ZP60009-C
XSATCBA  EQU   XSAWD3+1
XSAWD4   EQU   XSA+12
XSASAVEA EQU   XSAWD4
XSAENQAD EQU   XSA+16
XSAWD8   EQU   XSA+28
XSAUSERP EQU   XSA+32
XSAWD9   EQU   XSAUSERP
XSARETG  EQU   XSA+36
XSAWD11  EQU   XSA+40
XSAWD12  EQU   XSA+44
XSARC    EQU   XSAWD11
XSADATSZ EQU   XSAWD9
XSACURDS EQU   XSAWD9+2
CONTINUE EQU   SUCCESS
@RC00165 EQU   @RC00164
@RC00178 EQU   @RC00177
@RC00157 EQU   @RC00139
@RC00229 EQU   @RC00227
@RF00246 EQU   @RC00242
@RC00263 EQU   @RC00255
@RF00272 EQU   @RC00255
@RC00183 EQU   @RC00178
@RC00181 EQU   @RC00178
@RC00179 EQU   @RC00178
@RF00233 EQU   @RC00229
@ENDDATA EQU   *
         END   IKTVTGET,(C'PLS1824',0702,82141)
/*
//*
//STEP03  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKTVTGET('ZP60009')
++MOD(IKT3270O) DISTLIB(AOST3).
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP04  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '*** IKT3270O: TSO/VTAM TERMINAL SCREEN MANAGER ***     *
                        '
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*
IKT3270O CSECT
         USING IKT3270O,R15
         B     @PROLOG
         DC    AL1(33)
         DC    C'IKT3270O  83.271'
         DC    C' ZP60009 2005.226'
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)             SAVE REGS
         BALR  R5,0                        ADDRESS SET
@PSTART  LA    R6,4095(,R5)
         LA    R7,4095(,R6)
         USING PSA,0
         USING @PSTART,R5
         USING @PSTART+4095,R6
         USING @PSTART+8190,R7
         EJECT
********************************************************************
*                                                                  *
* SET UP ADDRESSING TO COMPILER WORK AREA, SET SCREEN PARMS AND    *
* PROCESS REQUEST CODE.                                            *
*                                                                  *
********************************************************************
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R12,TSBEXTNT-TSB(,R12)
         L     R9,TSBXTVWA-TSBX(,R12)      SET TVWA POINTER
         USING TVWA,R9
         L     R8,TVWATOMW                 SET BASE PTR FOR WORK AREA
         LA    R4,TOMAUTOD(,R8)
         ST    R13,4(,R4)
         ST    R4,8(,R13)                  ADDRESS OF AUTODATA AREA
         LR    R13,R4
         L     R12,160(,R4)
         LTR   R12,R12
         BZ    A000054
         BAL   R14,FREEBUFR                PERFORM
A000054  TM    TVWAFLG1,TVWAXSCD           HAS SCREEN SIZE CHANGED?
         BNO   A000060                     NO
         BAL   R14,SETSCRSZ                PERFORM
A000060  LA    R12,876
         ALR   R12,R8
         ST    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)           POINT TO START OF RU
         CLI   TOMREQCD(R8),2
         BE    A0000E2
         IC    R12,TVWALNCT
         STC   R12,168(,R4)
         L     R12,TOMCOLMN(,R8)
         ST    R12,164(,R4)
         MVI   169(R4),X'00'
         NI    170(R4),X'1F'
         TM    TVWAFLG6,TVWAFMEW
         BNO   A00009A
         OI    170(R4),X'40'
A00009A  TM    TVWAFLG4,TVWAFMSC
         BNO   A0000A6
         OI    170(R4),X'80'
A0000A6  TM    TVWAFLG3,TVWAAIGN
         BNO   A0000B2
         OI    170(R4),X'20'
A0000B2  CLI   TOMREQCD(R8),0              REQUEST FOR RU BUILD?
         BNE   A0000C2                     NO
         BAL   R14,BLDRU                   PERFORM
         B     A0000E6
A0000C2  CLI   TOMREQCD(R8),1              REQUEST FOR FORMAT WRITE?
         BNE   A0000D2                     NO
         BAL   R14,FORMTWRT                PERFORM
         B     A0000E6
A0000D2  CLI   TOMREQCD(R8),3              REQUEST FOR BREAK-IN?
         BNE   A0000DE                     NO
         BAL   R14,BKMDSEND                PERFORM
A0000DE  B     A0000E6
A0000E2  BAL   R14,A00026A                 PERFORM
A0000E6  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12                     IS RU EMPTY?
         BNP   A000110                     YES
         TM    TVWAFLG3,TVWATRAN           USER TRANSLATION NEEDED?
         BO    A00010C                     YES
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R12,TSBEXTNT-TSB(,R12)
         TM    TSBXFLG1-TSBX(R12),TSBXASCI ASCII TRANSLATION NEEDED?
         BNO   A000110                     NO
A00010C  BAL   R14,TRANPROC                PERFORM
A000110  B     A0002EE
         EJECT
A000114  ST    R14,88(,R4)
         MVI   OPREQ(R8),X'07'
         LA    R13,520(,R8)
         LA    R12,OPARMS(,R8)
         ST    R12,152(,R4)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASOQM-TCAST(,R12)
         LA    R1,152(,R4)
         BALR  R14,R15                     STD LINKAGE
         LA    R13,TOMAUTOD(,R8)
         L     R14,88(,R4)
         BR    R14                         EXIT
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, SETSCRSZ, WILL BE CALL TO INITIALIZE PARAMETERS  */
*/* USED FOR SCREEN MANAGEMENT. SETSCRSZ WILL ALSO BE CALL TO ADJUST */
*/* THESE PARAMETERS AS NEEDED DURING THE SESSION.                   */
*/*                                                                  */
*/********************************************************************/
SETSCRSZ ST    R14,12(,R13)
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         CLC   TSBALTR-TSB(2,R12),TSBPRMR-TSB(R12)              ZP60009
         BE    MOD1OR2                     PRMSZE=ALTSZE        ZP60009
         CLC   TSBLNNO-TSB(1,R12),TSBPRMR-TSB(R12)              ZP60009
         BH    USEALTSZ                    MORE LINES THAN PRIM ZP60009
         CLC   TSBLNSZ-TSB(1,R12),TSBPRMC-TSB(R12)              ZP60009
         BNH   USEPRMSZ                    NOT BIGGER THAN PRIM ZP60009
USEALTSZ LA    R1,TSBALTR-TSB(,R12)        POINT TO ALT SIZE    ZP60009
         TM    TVWAFLG6,X'01'              TVWAALTS ALREADY ON? ZP60009
         BO    GOTSCRSZ                    YES, NO NEED TO SET  ZP60009
         OI    TVWAFLG6,X'01'              NO, SET TVWAALTS     ZP60009
         B     CHGSCRSZ                    SIZE IS CHANGING     ZP60009
USEPRMSZ LA    R1,TSBPRMR-TSB(,R12)        POINT TO PRIM SIZE   ZP60009
         TM    TVWAFLG6,X'01'              TVWAALTS OFF?        ZP60009
         BZ    GOTSCRSZ                    YES, NO NEED TO CLR  ZP60009
         NI    TVWAFLG6,X'FE'              NO, RESET TVWAALTS   ZP60009
CHGSCRSZ TM    TVWAFLG5,TVWAFSM            IN FULLSCREEN MODE?  ZP60009
         BO    GOTSCRSZ                    YES, LEAVE FOR APP   ZP60009
         OI    TVWAFLG4,TVWAFMSC           NO, FORMAT SCREEN    ZP60009
         LA    R0,1                        RESTART FROM TOP     ZP60009
         STC   R0,TVWALNCT                 SET EXTERNALLY       ZP60009
         ST    R0,TOMLNCNT(,R8)            SET INTERNALLY       ZP60009
GOTSCRSZ SLR   R0,R0                       CLEAR FOR INSERT     ZP60009
         ICM   R0,3,0(R1)                  LOAD ROWS + COLS     ZP60009
         SRDL  R0,8                        GET ROWS IN R0       ZP60009
         SRL   R1,24                       GET COLS IN R1       ZP60009
         ST    R1,TOMPLSZ(,R8)             SET PHYS LINE SIZE   ZP60006
         STC   R0,TOMPLNNO(,R8)            SET PHYS LINE COUNT  ZP60009
         MR    R0,R0                       GET BUFFER SIZE      ZP60009
         L     R12,TSBEXTNT-TSB(,R12)      POINT TO TSBX        ZP60009
         STH   R1,TSBXTMBF-TSBX(,R12)      SET NEW BUFFER SIZE  ZP60009
         BCTR  R1,0                        GET LAST LOCATION    ZP60009
         STCM  R1,3,TOMFMTAD(R8)           SBA FOR LAST BYTE    ZP60009
         CH    R1,HW4095                   NEED 14-BIT ADDRESS? ZP60009
         BH    A000182                     YES                  ZP60009
         SLL   R1,2                        NO                   ZP60009
         STCM  R1,2,TOMFMTAD(R8)           USE 12-BIT ADDRESS   ZP60009
         NI    TOMFMTAD+1(R8),X'3F'                             ZP60009
         TR    TOMFMTAD(2,R8),BFADRTAB                          ZP60009
         B     A000182                                          ZP60009
MOD1OR2  EQU   *                                                ZP60009
         NI    TVWAFLG6,X'FE'              RESET TVWAALTS       ZP60009
         L     R12,TSBEXTNT-TSB(,R12)
         CLC   TSBXTMBF-TSBX(2,R12),HW1920 MODEL-2 DISPLAY?
*ZP60009 BNE   A000172
         BL    A000172                     NO, MODEK-1          ZP60009
         MVC   TOMPLSZ(4,R8),FW80          PHYSICAL LINE SIZE
         MVI   TOMPLNNO(R8),X'18'          LINES PER SCREEN
         MVC   TOMFMTAD(2,R8),A002103      LAST SCREEN POSITION
         B     A000182
A000172  MVC   TOMPLSZ(4,R8),FW40          PHYSICAL LINE SIZE
         MVI   TOMPLNNO(R8),X'0C'          LINES PER SCREEN
         MVC   TOMFMTAD(2,R8),A002105      LAST SCREEN POSITION
A000182  MVI   TOMSBACD(R8),X'11'
         MVC   TOMFMATT(2,R8),SFRDATT
         LA    R12,2
         L     R1,PSAAOLD                  PSA REFERENCE
         L     R15,ASCBTSB-ASCB(,R1)
         SLR   R14,R14
         IC    R14,TSBLNSZ-TSB(,R15)
         L     R11,TOMPLSZ(,R8)
         CR    R14,R11
         BNH   A0001AA
         LR    R14,R11
A0001AA  CR    R14,R12
         BNL   A0001B2
         LR    R14,R12
A0001B2  STC   R14,TOMLNSZ(,R8)            SET WORKING LINE SIZE
         SLR   R14,R14
         IC    R14,TSBLNNO-TSB(,R15)
         SLR   R15,R15
         IC    R15,TOMPLNNO(,R8)
         CR    R14,R15
         BNH   A0001CA
         LR    R14,R15
A0001CA  CR    R14,R12
         BNL   A0001D2
         LR    R14,R12
A0001D2  ST    R14,TOMLNNO(,R8)            SET WORKING LINES/SCREEN
         NI    TVWAFLG1,255-TVWAXSCD       RESET SCREEN SIZE REQUEST
         L     R14,12(,R13)
         BR    R14                         END OF SETSCRSZ
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, FORMTWRT, WILL BUILD AN RU CONSISTING OF A.....  */
*/*                                                                  */
*/********************************************************************/
FORMTWRT ST    R14,76(,R4)                 SAVE RETURN ADDRESS
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           TOMCOLMN=0
         LA    R12,9                       MESSAGE INDEX NUMBER
         ST    R12,TOMWRKVI(,R8)
*TOMWRKSZ=MIN(MSGSIZE,62)+18;        /* SET SIZE OF ERROR MS @G58AK2Q*/
         SLA   R12,2
         L     R1,CVTPTR                   CVT ADDRESS
         L     R1,CVTTCASP-CVT(,R1)
         L     R1,TCASMSGS-TCAST(,R1)
         AL    R1,FWMINUS4
         L     R12,0(R12,R1)
         LH    R15,0(,R12)
         LA    R14,62
         CR    R15,R14
         BNH   *+6
         LR    R15,R14
         AL    R15,FW18
         STC   R15,TOMWRKSZ(,R8)           SET SIZE OF ERROR MESSAGE
         MVI   TOMWRKBF(R8),X'F5'          SET COMMAND
         TM    TVWAFLG6,X'01'              IS TVWAALTS ON?      ZP60009
         BNO   *+8                         NO, PRIMARY SIZE     ZP60009
         MVI   TOMWRKBF(R8),X'7E'          YES, ALTERNATE SIZE  ZP60009
         MVC   TOMWRKBF+1(6,R8),A0020E9    BELL+UNLK,SBA(0),SF(UNPHI)
         LR    R14,R15
         SL    R14,FW19
         EX    R14,A00201E                 LOAD MSG TO TOMWRKBF+7
         ALR   R15,R8
         MVC   TOMWRKBF-11(11,R15),A0020D0
         L     R12,TOMPLSZ(,R8)            PHYSICAL LINE SIZE   ZP60009
         CL    R12,FW80                    WIDER THAN 80 COLS?  ZP60009
         BNH   ASTERSOK                    NO, HAVE RIGHT SBA   ZP60009
         STC   R12,TOMWRKBF-9(,R15)        SET SBA LOW ADDR     ZP60009
         NI    TOMWRKBF-9(R15),X'3F'       FIX SBA LOW ADDR     ZP60009
         SRL   R12,6                       GET SBA HIGH ADDR    ZP60009
         STC   R12,TOMWRKBF-10(,R15)       SET SBA HIGH ADDR    ZP60009
         TR    TOMWRKBF-10(2,R15),BFADRTAB FIX 12-BIT SBA ADDR  ZP60009
ASTERSOK EQU   *                           HAVE RIGHT *** ADDR  ZP60009
         OI    TVWAFLG2,TVWAPGN
         NI    TVWAFLG4,255-TVWAKBDL       KEYBOARD NOT LOCKED
         OI    TOMWFLG1(R8),TOMCURST       CURSOR ADDRESS SET
         OI    TOMWFLG2(R8),TOMERMG9       SPECIAL ERROR MESSAGE RU
         SLR   R12,R12
         IC    R12,TOMWRKSZ(,R8)
         ST    R12,TOMBFCNT(,R8)           SET RU DATA LENGTH
         TM    TVWAFLG5,TVWAFSM            IN FULLSCREEN MODE?
         BNO   *+8                         NO
         OI    TVWAFLG5,TVWAFSW            YES, FULLSCREEN TPUT WAITING
         L     R14,76(,R4)                 RESTORE RETURN ADDRESS
         BR    R14                         END OF FORMTWRT
         EJECT
A00026A  ST    R14,84(,R4)
         NI    TVWAFLG2,255-TVWAPGN
         NI    TVWAFLG4,255-TVWANOFB
         TM    170(R4),X'40'
         BNO   A000282
         OI    TVWAFLG6,TVWAFMEW
A000282  TM    170(R4),X'20'
         BNO   A00028E
         OI    TVWAFLG3,TVWAAIGN
A00028E  TM    170(R4),X'80'
         BNO   A00029A
         OI    TVWAFLG4,TVWAFMSC
A00029A  IC    R12,168(,R4)
         STC   R12,TVWALNCT
         L     R12,164(,R4)
         ST    R12,TOMCOLMN(,R8)
         SLR   R12,R12
         IC    R12,TVWAATTN
         SLR   R15,R15
         IC    R15,169(,R4)
         ALR   R12,R15
         STC   R12,TVWAATTN
         MVI   169(R4),X'00'
         BAL   R14,A000114                 PERFORM
         L     R14,84(,R4)
         BR    R14                         EXIT
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BKMDSEND, WILL BUILD AN RU....                   */
*/*                                                                  */
*/********************************************************************/
BKMDSEND ST    R14,80(,R4)
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12                     IS OUTPUT RU EMPTY?
         BNZ   A0002DC                     NO
         BAL   R14,SCRFORMT                PUT IN CMD AND WCC
A0002DC  TM    TVWAFLG5,TVWAFSM            IN FULLSCREEN MODE?
         BNZ   A0002E8                     YES
         BAL   R14,NEWCURS                 NO, FORMAT SCREEN FOR IT
A0002E8  L     R14,80(,R4)
         BR    R14                         END OF BKMDSEND
A0002EE  B     A002012
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BLDRU, WILL CONTROL THE BUILDING OF THE OUTPUT.  */
*/* APPROPRIATE ROUTINES WILL BE CALLED TO PUT DATA IN THE OUTPUT    */
*/* RU.                                                              */
*/*                                                                  */
*/********************************************************************/
BLDRU    ST    R14,72(,R4)
         NI    OPOPTNS(R8),255-OPASID      SET CROSS-MEMORY FLAG OFF
         NI    TOMWFLG2(R8),255-TOMSCHED   SEND RU POST=RESP
         NI    TOMWFLG1(R8),255-TOMCURST-TOMEXBRU
         NI    TOMWFLG2(R8),255-TOMKBULK   KEYBD UNLOCK NOT REQ'D YET
         SLR   R12,R12
         ST    R12,TOMBFCNT(,R8)           SET COUNT OF DATA IN RU TO 0
         NI    TOMWFLG2(R8),255-TOMALARM-TOMERASE-TOMTRSBA
         TM    TVWAFLG2,TVWABKMG           IS FLASHBACK DATA PENDING?
         BNO   A00031C                     NO
*  /******************************************************************/
*  /*                                                                */
*  /*      PUT LAST INPUT LINE(S) AT TOP OF 3270 SCREEN IF YES.      */
*  /*                                                                */
*  /******************************************************************/
         BAL   R14,FLSHBACK                PUT FLASHBACK IN OUTPUT RU
A00031C  TM    TVWAFLG2,TVWAERMG           TIM'S ERROR MESSAGE PENDING?
         BNO   A000330                     NO
         TM    TOMWFLG1(R8),TOMEXBRU       CAN RU BUILD CONTINUE?
         BNZ   A000330                     NO
*  /******************************************************************/
*  /*                                                                */
*  /*            PUT TIM'S ERROR MESSAGE IN OUTPUT RU                */
*  /*                                                                */
*  /******************************************************************/
         BAL   R14,BLDERR                  GO TO ERROR MESSAGE ROUTINE
A000330  CLI   TVWAATTN,0                  REQ FOR ATTENTION ACCEPTED?
         BNE   A000340                     YES
         TM    TVWAFLG3,TVWAAIGN           REQ FOR ATTENTION IGNORED?
         BNO   A00034C                     NO
A000340  TM    TOMWFLG1(R8),TOMEXBRU       CAN RU BUILD CONTINUE?
         BNZ   A00034C                     NO
*  /******************************************************************/
*  /*                                                                */
*  /*    PUT ATTENTION ACCEPTED AND/OR IGNORED MESSAGE(S) IN RU.     */
*  /*                                                                */
*  /******************************************************************/
         BAL   R14,BLDATTN                 PUT ATTENTION MESSAGES IN RU
*/********************************************************************/
*/*                                                                  */
*/*            BUILD AND EDIT USER DATA IN OUTPUT RU.                */
*/*                                                                  */
*/********************************************************************/
A00034C  TM    TOMWFLG1(R8),TOMEXBRU       CAN RU BUILD CONTINUE?
         BNZ   A000358                     NO
         BAL   R14,BLDDTA                  YES, CALL DATA BUILD ROUTINE
         TM    OPOPTNS(R8),OPNOED          NOEDIT?              ZP60009
         BNO   A000358                     NO, CONTINUE EDIT    ZP60009
         TM    OPOPTNS(R8),OPASID          CROSS-MEMORY TPUT?   ZP60009
         BO    NOEDASID                    YES, DO NOT ALLOW    ZP60009
         TM    TVWAFLG5,TVWAFSW            NOEDIT TPUT WAITING? ZP60009
         BO    A000358                     YES, CONTINUE EDIT   ZP60009
         CLC   FW1,TOMBFCNT(R8)            LONG ENOUGH FOR WCC? ZP60003
         BNL   A00055A                     NO, NO WCC TO CHECK  ZP60009
         L     R12,TOMGETMN(,R8)           POINT TO DATA STREAM ZP60009
         CLI   0(R12),X'F1'                WRITE?               ZP60009
         BE    CHECKWCC                    YES, WCC PRESENT     ZP60009
         CLI   0(R12),X'F5'                ERASE/WRITE?         ZP60009
         BE    CHECKWCC                    YES, WCC PRESENT     ZP60009
         CLI   0(R12),X'7E'                ERASE/WRITE ALT?     ZP60009
         BNE   A00055A                     NO, NO WCC TO CHECK  ZP60009
CHECKWCC TM    1(R12),X'02'                UNLOCKING KEYBOARD?  ZP60009
         BNO   A00055A                     NO, BUT NO WCC EDIT  ZP60009
         B     A00045A                     YES, SET FLAGS       ZP60009
NOEDASID NI    OPOPTNS(R8),255-OPNOED      RESET NOEDIT FLAG    ZP60009
A000358  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    TSBFLG3-TSB(R12),TSBSPIT    TCLEARQ OR STBREAK ISSUED?
         BNZ   A000394                     YES
         TM    TSBFLG2-TSB(R12),TSBAUTON+TSBSTAUT
         BNO   A000394                     NO AUTO PROMPTING
         TM    TVWAFLG3,TVWABRIN
         BNZ   A000394                     NO BREAK-IN REQUEST
         TM    TOMWFLG1(R8),TOMEXBRU       CAN RU BUILD CONTINUE?
         BNZ   A000394                     NO
         TM    TSBFLG4-TSB(R12),TSBIWAIT   INPUT WAIT IN PROGRESS?
         BO    A000390                     YES
         TM    TVWAFLG8,TVWATGNO           TGET NOWAIT ISSUED?
         BNO   A000394                     NO
*  /******************************************************************/
*  /*                                                                */
*  /*       BUILD LINE OR CHARACTER PROMPT DATA IN OUTPUT RU.        */
*  /*                                                                */
*  /******************************************************************/
A000390  BAL   R14,BLDPRMPT                GO TO PROMPT ROUTINE
A000394  L     R12,TOMBFCNT(,R8)           DATA IN RU & NOT FIRST LINE?
         LTR   R12,R12
         BNP   A0003C6                     LEAVE IF NO DATA IN RU
         L     R12,TOMLNCNT(,R8)
         C     R12,FW1
         BNH   A0003C6                     LEAVE IF FIRST LINE
         TM    TOMWFLG2(R8),TOMTRSBA       IS LINE COUNT > 1?
         BZ    A0003BA                     YES
         TM    TOMWFLG1(R8),TOMEDSAV       LAST TPUT AN EDIT?
         BNZ   A0003C2                     NO
A0003BA  OI    TVWAFLG5,TVWASCAN           YES
         B     A0003C6
A0003C2  NI    TVWAFLG5,255-TVWASCAN       RU SIZE EMPTY OR FIRST LINE
A0003C6  L     R12,TOMBFCNT(,R8)
         C     R12,FW2                     DATA IN RU (AFTER WCC)?
         BNH   A0003D6                     NO
         NI    TVWAFLG4,255-TVWADARC       RESET DATA FOUND BIT
A0003D6  TM    TOMWFLG1(R8),TOMENDBF       ENOUGH BUFFER SPACE?
         BNZ   A000432                     NO
         TM    TOMWFLG1(R8),TOMCURST       IS CURSOR ALREADY SET IN RU?
         BNZ   A000432                     YES
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12                     IS THERE DATA IN OUTPUT RU?
         BNP   A000432                     NO
*     /***************************************************************/
*     /*                                                             */
*     /* THIS DO GROUP WILL SET CURSOR FOR ALL NON-EMPTY RU'S FOR    */
*     /* WHICH CURSOR HAS NOT ALREADY BEEN SET.                      */
*     /*                                                             */
*     /***************************************************************/
*                                          BEGIN DEFAULT CURSOR PROCESS
         BAL   R14,NEWLINE                 PUT LINE ADDR IN OUTPUT RU
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)            LINE-COUNT AT LAST LINE?
         BL    A000410                     NO
         TM    TVWAFLG4,TVWANOFB           IS IT OK TO DO FLASHBACK?
         BNZ   A000410                     NO
         BAL   R14,SCRNPAGE                YES, PUT PAGE PROMPT IN RU
         B     A000432
*                                          SET CURSOR ON CURRENT LINE
A000410  LA    R12,1
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           UPDATE COUNT OF DATA IN RU
         L     R1,TOMBFPTR(,R8)
         XC    1(255,R1),1(R1)
         MVI   0(R1),X'13'                 PUT INSERT CURSOR IN RU
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
A000432  L     R2,TOMGETMN(,R8)            SET ADDR OF OUTPUT RU IN R2
         TM    TOMWFLG2(R8),TOMALARM       SOUND AUDIBLE ALARM REQUEST?
         BNO   A000442                     NO
         OI    1(R2),X'84'                 YES, MODIFY WCC FOR ALARM
A000442  TM    TVWAFLG4,TVWANOFB
         BNO   A00045A
         OI    TOMWFLG2(R8),TOMKBULK
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         NI    TSBFLG1-TSB(R12),255-TSBIFLSH
A00045A  TM    TOMWFLG1(R8),TOMEXBRU       CONTINUE BLDRU?
         BZ    A00046A                     YES
         TM    TVWAFLG4,TVWANOFB           NO FLASHBACK?
         BNO   A000472                     NO
A00046A  TM    TVWAFLG1,TVWAULK            GLOBAL UNLOCK REQUESTED?
         BO    A00047A                     YES
A000472  TM    TOMWFLG2(R8),TOMKBULK       LOCAL REQ FOR KEYBD UNLOCK?
         BNO   A00055A                     NO
*  /******************************************************************/
*  /*                                                                */
*  /* MODIFY WCC IN RU TO RESTORE KEYBOARD OPERATION IF KEYBOARD UN- */
*  /* LOCK REQUEST IS OUTSTANDING.                                   */
*  /*                                                                */
*  /******************************************************************/
A00047A  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A00048C
         TM    TOMWFLG4(R8),TOMEB
         BNZ   A00053E
*                                          BEGIN UNLOCK KEYBD PROCESS
A00048C  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12                     RU FOR UNLOCK KEYBOARD ONLY?
         BNZ   A000518                     NO
         TM    TVWAFLG5,TVWAFSM
         BNZ   A0004BE
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A0004BE
         BAL   R14,SCRFORMT                PERFORM
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         OI    TVWAFLG5,TVWASCAN
         B     A00051C
A0004BE  TM    TVWAFLG4,TVWAFMSC           FORMAT SCREEN?
         BO    A0004DA                     YES
         TM    TVWAFLG4,TVWADARC           HAS TIM RECEIVED DATA?
         BNO   A0004F2                     NO
         L     R12,TOMLNCNT(,R8)
         C     R12,FW1                     IS LINE-COUNT > 1?
         BNH   A0004F2                     NO
*                                          SET SPECIAL CURSOR FOR INPUT
A0004DA  BAL   R14,SCRFORMT                    CMD,WCC, & FORMAT
         L     R12,TOMLNCNT(,R8)
         C     R12,FW1
         BNH   A0004EE
         BAL   R14,NEWCURS                 PUT CURSOR IN RU
A0004EE  B     A000510                     END SET SPECIAL CURSOR
*                                          BLD HEADER FOR UNLOCK RU
*                                          BLD HEADER FOR UNLOCK REQ
A0004F2  L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),WRTWCC             PUT CMD AND WCC
         LA    R12,2
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           UPDATE COUNT OF DATA IN RU
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
A000510  NI    TVWAFLG4,255-TVWADARC
         B     A00051C
A000518  NI    TVWAFLG4,255-TVWADARC
A00051C  L     R2,TOMGETMN(,R8)
         OI    1(R2),X'82'                 MODIFY WCC TO RESTORE KEYBD
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    TSBFLG3-TSB(R12),TSBNOBRK
         BNO   A000538
         OI    TVWAFLG1,TVWATAS
A000538  SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
A00053E  NI    TVWAFLG4,255-TVWAKBDL       RESET KEYBOARD LOCKED FLAG
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         NI    TSBFLG1-TSB(R12),255-TSBIFLSH
         TM    TVWAFLG2,TVWAPGN
         BNZ   A00055A
         NI    TVWAFLG1,255-TVWAULK        CLEAR UNLOCK REQUEST FLAG
A00055A  L     R12,TOMLNCNT(,R8)
         STC   R12,TVWALNCT
         L     R14,72(,R4)
         BR    R14                         EXIT BLDRU
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, NEWLINE, WILL BE CALLED BY INTERNAL PROCEDURES   */
*/* TO BUILD LINE ADDRESSES WHILE BUILDING THE OUTPUT RU.            */
*/*                                                                  */
*/********************************************************************/
NEWLINE  ST    R14,120(,R4)
         LA    R2,1                        SET R2 FOR BUFFER ADDRESS
         BAL   R14,BUFFADDR                PUT LINE ADDRESS IN RU
         LA    R12,3
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           SET CNT FOR LINE ADDR IN RU
         L     R3,TOMBFPTR(,R8)
         MVI   0(R3),X'11'                 PUT SBA ORDER IN RU
         MVC   1(2,R3),TOMLNADR(R8)        PUT LINE ADDR IN OUTPUT RU
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)           POINT TO NEXT UNUSED RU BYTE
         L     R14,120(,R4)
         BR    R14                         EXIT NEWLINE
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BUFFADDR, WILL COMPUTE BUFFER ADDRESSES FOR TOM  */
*/*                                                                  */
*/********************************************************************/
BUFFADDR ST    R14,12(,R13)
*TOMLNCNT=MIN(TOMLNCNT,TOMLNNO);           NOT BEYOND LAST SCREEN LINE
         L     R12,TOMLNCNT(,R8)
         L     R15,TOMLNNO(,R8)
         CR    R12,R15
         BNH   A0005B0
         LR    R12,R15
A0005B0  ST    R12,TOMLNCNT(,R8)
*J=(TOMLNCNT-REG2)*TOMPLSZ+TOMCOLMN;       OFFSET
         LR    R1,R12
         SLR   R1,R2
         M     R0,TOMPLSZ(,R8)
         AL    R1,TOMCOLMN(,R8)
         ST    R1,TOMWRKVJ(,R8)
*J=MAX(0,J);                               J IS NONZERO
         SLR   R12,R12
         CR    R1,R12
         BNL   A0005CE
         LR    R1,R12
A0005CE  ST    R1,TOMWRKVJ(,R8)
         STCM  R1,3,TOMLNADR(R8)           SET 14-BIT ADDRESS   ZP60009
         CH    R1,HW4095                   NEED 14-BIT ADDRESS? ZP60009
         BH    BUFADROK                    YES                  ZP60009
*TOMROWAD=J/64;                            SET ROW INDEX FOR TRANSLATE
         LR    R10,R1
         SRDA  R10,32
         D     R10,FW64
         STC   R11,TOMROWAD(,R8)
*TOMCOLAD=J//64;                           SET COL INDEX FOR TRANSLATE
         LR    R10,R1
         SRDA  R10,32
         D     R10,FW64
         STC   R10,TOMCOLAD(,R8)
*TR(TOMLNADR,BFADRTAB);                    SAVE BUFFER ADDRESS
         TR    TOMLNADR(2,R8),BFADRTAB
BUFADROK EQU   *                                                ZP60009
         L     R14,12(,R13)
         BR    R14                         END OF BUFFADDR
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, NEWCURS, WILL SET CURSOR AFTER SUCCESSIVE TGETS. */
*/*                                                                  */
*/********************************************************************/
NEWCURS  ST    R14,96(,R4)
         LA    R2,2                        SET R2 FOR BUFFER ADDRESS
*IF TOMCOLMN=TOMLNSZ                       NO SPACE ON LINE?
         L     R12,TOMCOLMN(,R8)
         SLR   R3,R3
         IC    R3,TOMLNSZ(,R8)
         CR    R12,R3
         BNE   A000628
*THEN                                      NO SPACE ON LINE
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           RESET COLUMN COUNT
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)           MOVE TO NEXT LINE
         B     A000634
*ELSE                                      SPACE LEFT ON LINE
A000628  SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         BCTR  R12,0
         ST    R12,TOMCOLMN(,R8)           TOMCOLMN=TOMLNSZ-1
A000634  BAL   R14,BUFFADDR                COMPUTE BUFFER ADDRESS
         LA    R12,6
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           UPDATE RU DATA COUNT
         L     R3,TOMBFPTR(,R8)
         MVI   0(R3),X'11'                 PUT SBA ORDER IN RU
         MVC   1(2,R3),TOMLNADR(R8)        PUT BUFFER ADDRESS IN RU
         MVC   3(3,R3),SFRDATIC            READ ATTRIBUTE AND CURSOR
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           RESET COLUMN COUNT
         OI    TVWAFLG5,TVWASCAN           REQ ATTRIBUTE TRACKING
         L     R14,96(,R4)
         BR    R14                         END OF NEWCURS
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, SCRFORMT, FORMATS THE 3270 SCREEN (IF NEEDED)    */
*/* AND PUTS THE PROPER WRITE CMD IN THE OUTPUT RU. THE SCREEN IS    */
*/* FORMATTED BY PUTTING AN ATTRIBUTE CHARACTER IN THE LAST          */
*/* BUFFER LOCATION.                                                 */
*/*                                                                  */
*/********************************************************************/
SCRFORMT ST    R14,92(,R4)
         CLC   TOMLNCNT(4,R8),FW1          IS THIS THE TOP SCRN LINE?
         BE    A000686                     YES
         TM    TVWAFLG4,TVWAFMSC           IS FORMAT OF SCRN REQUESTED?
         BNO   A0006B0                     NO
*  /******************************************************************/
*  /*                                                                */
*  /* THIS DO GROUP FORMATS THE 3270 BUFFER AFTER DETERMINING THE    */
*  /* LAST BUFFER LOCATION.                                          */
*  /*                                                                */
*  /******************************************************************/
A000686  TM    TVWAFLG4,TVWAFMSC           FORMAT SCREEN BIT ON?
         BNO   A000694                     NO
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           RESET COLUMN COUNT
A000694  BAL   R14,SETSCRSZ                PERFORM
         MVC   TOMBFCNT(4,R8),FW7          COUNT FOR CMD,WCC, & FORMAT
         L     R12,TOMBFPTR(,R8)
         MVC   2(5,R12),TOMFMTDA(R8)       PUT FORMATTING DATA IN RU
         NI    TVWAFLG4,255-TVWAFMSC       TURN OFF REQUEST FOR FORMAT
         B     A0006B6
A0006B0  MVC   TOMBFCNT(4,R8),FW2          SET COUNT FOR CMD AND WCC
A0006B6  TM    TVWAFLG5,TVWAFSM            IS FULLSCR MODE IS EFFECT?
         BNO   A0006C6                     NO
         TM    TOMWFLG2(R8),TOMERASE       SCREEN ERASE?
         BO    A0006D6                     YES
A0006C6  TM    OPOPTNS(R8),OPBRK           BREAK TPUT?
         BNO   A0006E4                     NO
         TM    TVWAFLG3,TVWABRIN           BREAK-IN PROCESSING?
         BNO   A0006E4                     NO
A0006D6  L     R12,TOMBFPTR(,R8)           YES
         MVC   0(2,R12),EWRTWCCD           USE ERASE/WRITE
         TM    TVWAFLG6,X'01'              IS TVWAALTS ON?      ZP60009
         BNO   A000744                     NO, PRIMARY SIZE     ZP60009
         MVI   0(R12),X'7E'                YES, ALTERNATE SIZE  ZP60009
         B     A000744
A0006E4  CLC   TOMLNCNT(4,R8),FW1          IS THIS THE TOP SCREEN LINE?
         BNE   A00073A                     NO
         TM    TOMWFLG1(R8),TOMEDSAV       WAS LAST TPUT FULLSCRN MODE?
         BNO   A00070C                     NO
         TM    OPOPTNS(R8),OPASID          CURRENT TPUT A CROSS-MEMORY?
         BNO   A00070C                     NO
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),WRTWCCDF           USE WRT-ASID FOLLOWS FULSCRN
         B     A000744
A00070C  L     R12,TOMCOLMN(,R8)
         LTR   R12,R12                     FIRST POSITION?
         BNZ   A00072C                     NO
         TM    170(R4),X'80'
         BNO   A00072C
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),EWRTWCCD           PUT EWRT CMD AND WCC IN RU
         TM    TVWAFLG6,X'01'              IS TVWAALTS ON?      ZP60009
         BNO   A000744                     NO, PRIMARY SIZE     ZP60009
         MVI   0(R12),X'7E'                YES, ALTERNATE SIZE  ZP60009
         B     A000744
A00072C  L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),WRTWCCDF           WRITE CMD AND WCC
         B     A000744
A00073A  L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),WRTWCCDF           PUT WRITE CMD AND WCC IN RU
A000744  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         L     R14,92(,R4)
         BR    R14                         END OF SCRFORMT
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, FLSHBACK, WILL BE CALLED TO PUT THE LAST LOGICAL */
*/* INPUT SCREEN LINE(S) INTO THE OUTPUT RU FOR DISPLAY AT THE TOP   */
*/* OF THE SCREEN.                                                   */
*/*                                                                  */
*/********************************************************************/
*DCL 1 TOPQ BASED(TVWATOPQ),         /* STRUCTURE FOR FLASHBACK DATA */
*     2 TQL1 CHAR(80),               /* FIRST FLASHBACK BUFFER       */
*     2 TQL2 CHAR(80);               /* SECOND FLASHBACK BUFFER      */
FLSHBACK ST    R14,100(,R4)
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           RESET COLUMN COUNT
         BAL   R14,SCRFORMT                PUT IN CMD,WCC, AND FORMAT
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)            IS THIS THE LAST LINE?
         BL    A00077C                     NO
         BAL   R14,NEWLINE                 LINE ADDRESS IN RU
         BAL   R14,SCRNPAGE                PUT PAGE PROMPT IN RU
         B     A0008CA
*  /******************************************************************/
*  /*                                                                */
*  /* SCREEN LINE(S) IS AVAILABLE SO PUT FLASHBACK DATA IN RU.       */
*  /*                                                                */
*  /******************************************************************/
A00077C  MVC   TOMLNCNT(4,R8),FW1          TOP OF SCREEN
         BAL   R14,NEWLINE                 LINE ADDRESS IN RU
         CLI   TVWATQL1,0                  LEN OF 1ST FLSHBACK QUE > 0?
         BE    A00081A                     NO
*      /**************************************************************/
*      /*                                                            */
*      /* TAKE DATA FROM FIRST FLASHBACK QUEUE BUFFER.               */
*      /*                                                            */
*      /**************************************************************/
*        TOMBFCNT=TOMBFCNT+          /* UPDATE COUNT OF DATA IN RU BY*/
*          MIN(TVWATQL1,TOMLNSZ)+2;  /* SMALLER OR LINE-SZ & QUE LENG*/
         LA    R12,2
         SLR   R15,R15
         IC    R15,TVWATQL1
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         LR    R11,R14
         CR    R11,R15
         BNH   A0007A8
         LR    R11,R15
A0007A8  AL    R11,TOMBFCNT(,R8)
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)
         L     R10,TOMBFPTR(,R8)
         MVC   0(2,R10),SFWRTATT           PUT WRITE ATTRIBUTE INTO RU
*                                    /* MOVE FIRST FLSHBACK BUFFER   */
*        IKTOBUF(3:(MIN(TVWATQL1,TOMLNSZ-1)+2))=TQL1; /* MOVE DATA   */
         BCTR  R14,0
         CR    R15,R14
         BNH   A0007C6
         LR    R15,R14
A0007C6  BCTR  R15,0
         L     R1,TVWATOPQ
         EX    R15,A002024
*        TR(IKTOBUF(3:(MIN(TVWATQL1,TOMLNSZ-1)+2)),EDITTAB);
         SLR   R15,R15
         IC    R15,TOMLNSZ(,R8)
         BCTR  R15,0
         SLR   R14,R14
         IC    R14,TVWATQL1
         CR    R14,R15
         BNH   A0007E6
         LR    R14,R15
A0007E6  BCTR  R14,0
         EX    R14,A00202A
         L     R10,TOMGETMN(,R8)
         LR    R15,R10
         ALR   R15,R11
         ST    R15,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)           UPDATE DATA CNT FOR RD ATTRI
         MVC   0(2,R15),SFRDATT            PUT READ ATTRIBUTE INTO RU
         ALR   R10,R11
         ST    R10,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         MVI   TVWATQL1,0                  ZERO LENGTH OF FIRST QUEUE
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)           INCREMENT LINE-COUNT BY ONE
A00081A  L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)            IS THIS THE LAST SCRN LINE?
         BL    A000832                     NO
         BAL   R14,NEWLINE                 YES, PUT LINE ADDRESS IN RU
         BAL   R14,SCRNPAGE                YES, PUT PAGE PROMPT IN RU
         B     A0008CA
*        /************************************************************/
*        /*                                                          */
*        /*      TAKE DATA FROM SECOND FLASHBACK QUEUE BUFFER.       */
*        /*                                                          */
*        /************************************************************/
A000832  CLI   TVWATQL2,0                  LENG OF 2ND FLSHBACK BUFF>0?
         BE    A0008CA                     NO
         BAL   R14,NEWLINE                 PUT LINE ADDRESS IN RU
*          TOMBFCNT=TOMBFCNT +       /* UPDATA COUNT OF DATA IN RU BY*/
*            MIN(TVWATQL2,TOMLNSZ)+2;/* SMALLER OF LINE-SZ & QUE LENG*/
         LA    R12,2
         SLR   R15,R15
         IC    R15,TVWATQL2
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         LR    R11,R14
         CR    R11,R15
         BNH   A000858
         LR    R11,R15
A000858  AL    R11,TOMBFCNT(,R8)
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)
         L     R10,TOMBFPTR(,R8)
         MVC   0(2,R10),SFWRTATT           PUT WRITE ATTRIBUTE INTO RU
*          IKTOBUF(3:(MIN(TVWATQL2,TOMLNSZ-1)+2))=TQL2; /* MOVE DATA */
         BCTR  R14,0
         CR    R15,R14
         BNH   A000876
         LR    R15,R14
A000876  BCTR  R15,0
         L     R1,TVWATOPQ
         EX    R15,A002030
*          TR(IKTOBUF(3:(MIN(TVWATQL2,TOMLNSZ-1)+2)),EDITTAB);
         SLR   R15,R15
         IC    R15,TOMLNSZ(,R8)
         BCTR  R15,0
         SLR   R14,R14
         IC    R14,TVWATQL2
         CR    R14,R15
         BNH   A000896
         LR    R14,R15
A000896  BCTR  R14,0
         EX    R14,A00202A
         L     R10,TOMGETMN(,R8)
         LR    R15,R10
         ALR   R15,R11
         ST    R15,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)           UPDATE DATA CNT FOR RD ATTRI
         MVC   0(2,R15),SFRDATT            PUT READ ATTRIBUTE INTO RU
         ALR   R10,R11
         ST    R10,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         MVI   TVWATQL2,0                  ZERO LENGTH OF 2ND BUFFER
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)           INCREMENT LINE-COUNT BY ONE
A0008CA  CLI   TVWATQL1,0                  IS ALL FLASHBACK PROCESSED?
         BNE   A0008DE                     NO
         CLI   TVWATQL2,0
         BNE   A0008DE                     NO
         NI    TVWAFLG2,255-TVWABKMG       YES, TURN OFF REQUEST BIT
A0008DE  L     R14,100(,R4)
         BR    R14                         END OF FLSHBACK
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BLDATTN, BUILDS ATTENTIONS MESSAGES IN THE OUT-  */
*/* PUT RU. THE CHARACTER   IS PUT ON THE NEXT AVAILABLE SCREEN LINE */
*/* FOR EACH ACCEPTED ATTENTION. THE CHARACTER SEQUENCE  I IS PUT ON */
*/* NEXT SCREEN LINE WHEN IGNORED ATTENTION(S) ARE DETECTED          */
*/*                                                                  */
*/********************************************************************/
BLDATTN  ST    R14,108(,R4)
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)           RESET COLUMN COUNT
         C     R12,TOMBFCNT(,R8)           IS OUTPUT RU EMPTY?
         BNE   A0008FA                     NO
         BAL   R14,SCRFORMT                YES, PUT IN CMD,WCC & FORMAT
A0008FA  B     A000990
*/********************************************************************/
*/*                                                                  */
*/* THIS DO GROUP BUILDS ATTENTION MESSAGES UNTIL THE ATTENTION      */
*/* ACCEPTED COUNT GOES TO ZERO, THE ATTENTION IGNORED INDICATOR IS  */
*/* TURNED OFF, OR SCREEN PAGING IS NEEDED.                          */
*/*                                                                  */
*/********************************************************************/
A0008FE  BAL   R14,NEWLINE                 PUT LINE ADDR INTO OUTPUT RU
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)            IS THIS THE LAST SCRN LINE?
         BL    A000916                     NO
         BAL   R14,SCRNPAGE                YES, PUT PAGE PROMPT INTO RU
         B     A000990
A000916  CLC   TOMLNCNT(4,R8),FW1
         BNE   A000924
         BAL   R14,SCRFORMT                PERFORM
A000924  CLI   TVWAATTN,0                  IS ACCEPTED ATTN COUNT>0?
         BNH   A00095E                     NO
*        /************************************************************/
*        /*                                                          */
*        /* THIS DO GROUP PUTS THE MSG,  , ON THE CURRENT LINE AND   */
*        /* DECREMENTS THE ACCEPTED ATTENTION COUNT BY ONE           */
*        /*                                                          */
*        /************************************************************/
         LA    R12,5
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           SET COUNT FOR ATTENTION MSG
         L     R12,TOMBFPTR(,R8)
         MVC   0(5,R12),A0020EF            PUT ATTENTION MESSAGE IN RU
         SLR   R12,R12
         IC    R12,TVWAATTN
         BCTR  R12,0
         STC   R12,TVWAATTN                DECREMENT ACCEPTED ATTN CNT
         IC    R12,169(,R4)
         LA    R12,1(,R12)
         STC   R12,169(,R4)
         B     A000978
*        /************************************************************/
*        /*                                                          */
*        /* THIS DO GROUP PUTS THE MSG,  I, ON THE CURRENT LINE AND  */
*        /* ATTENTION IGNORED INDICATOR IS ON AND THE ATTENTION AC-  */
*        /* CEPTED COUNT IS ZERO.                                    */
*        /*                                                          */
*        /************************************************************/
A00095E  LA    R12,6
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)           SET COUNT FOR ATTN MESSAGE
         L     R12,TOMBFPTR(,R8)
         MVC   0(6,R12),A0020E3            PUT ATTN IGNORED MSG IN RU
         NI    TVWAFLG3,255-TVWAAIGN       TURN OFF ATTN INDICATOR
A000978  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)           UPDATE RU BUFFER POINTER
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)           INCREMENT LINE-COUNT BY ONE
A000990  CLI   TVWAATTN,0
         BH    A0009A0
         TM    TVWAFLG3,TVWAAIGN
         BNO   A0009A8
A0009A0  TM    TOMWFLG1(R8),TOMEXBRU
         BZ    A0008FE
A0009A8  L     R14,108(,R4)
         BR    R14                         END OF BLDATTN
         EJECT
GETBUFR  ST    R14,668(,R8)
         ST    R4,672(,R8)
         ST    R7,676(,R8)
         LA    R0,8
         AH    R0,OPBFSZ(,R8)
         ST    R0,680(,R8)
         L     R4,TVWATCB
         L     R7,PSAAOLD                  PSA REFERENCE
         B     A0009D8
         CNOP  0,4
A0009D4  EQU   *
         DC    H'0'
         DC    X'E502'                     SUBPOOL 229
A0009D8  L     R3,A0009D4
         SR    R1,R1
         L     R15,CVTPTR                  CVT ADDRESS
         L     R15,CVTCRMN-CVT(,R15)
         BALR  R14,R15                     BRANCH ENTRY GETMAIN
         L     R15,680(,R8)
         ST    R15,0(,R1)
         L     R7,676(,R8)
         L     R4,672(,R8)
         ST    R1,160(,R4)
         L     R15,160(,R4)
         AL    R15,FW4
         ST    R15,TOMGETMN(,R8)
         ST    R15,TOMBFPTR(,R8)
         L     R14,668(,R8)
         BR    R14                         END OF GETBUFR
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BLDDTA, CONTROLS BUILDING OF USER DATA IN THE    */
*/* OUTPUT RU                                                        */
*/*                                                                  */
*/********************************************************************/
BLDDTA   ST    R14,112(,R4)
         NI    TOMWFLG1(R8),255-TOMENDBF-TOMENDSC-TOMEXDTA-TOMOWAIT
A000A1A  SLR   R12,R12
         ST    R12,TOMMVCNT(,R8)
         ST    R12,TOMQDATA(,R8)
         NI    OPOPTNS(R8),255-OPBRK
         MVI   OPREQ(R8),X'02'
         LA    R13,520(,R8)
         LA    R12,OPARMS(,R8)
         ST    R12,152(,R4)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASOQM-TCAST(,R12)
         LA    R1,152(,R4)
         BALR  R14,R15                     STD LINKAGE
         LA    R13,TOMAUTOD(,R8)
         CLI   OPRC(R8),3
         BE    A000A66
         TM    TVWAFLG3,TVWABRIN
         BNO   A000AC0
         TM    OPOPTNS(R8),OPBRK
         BNZ   A000AC0
A000A66  CLI   OPRC(R8),3                  WAS OUTPUT QUEUE EMPTY?
         BNE   A000A7A
*    /****************************************************************/
*    /*                                                              */
*    /*  YES, NO MORE DATA IS AVAILABLE FROM OUTPUT MESSAGE QUEUE.   */
*    /*                                                              */
*    /****************************************************************/
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         NI    TSBFLG1-TSB(R12),255-TSBIFLSH
A000A7A  OI    TOMWFLG1(R8),TOMEXDTA
         TM    TVWAFLG3,TVWABRIN
         BNO   A000DB0
         TM    TVWAFLG2,TVWAPGN
         BNZ   A000AA4
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A000A9C
         BAL   R14,SCRFORMT                PERFORM
A000A9C  BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
A000AA4  BAL   R14,A000114                 PERFORM
         NI    TVWAFLG2,255-TVWABIR
         NI    TVWAFLG3,255-TVWABRIN
         TM    TVWAFLG5,TVWAFSM
         BNO   A000ABC
         OI    TVWAFLG7,TVWABKPG
A000ABC  B     A000DB0
A000AC0  TM    TVWAFLG4,TVWANOFB
         BNO   A000AFA
         OI    TOMWFLG1(R8),TOMEXDTA
         MVI   OPREQ(R8),X'04'
         LA    R13,520(,R8)
         LA    R12,OPARMS(,R8)
         ST    R12,152(,R4)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASOQM-TCAST(,R12)
         LA    R1,152(,R4)
         BALR  R14,R15                     STD LINKAGE
         LA    R13,TOMAUTOD(,R8)
         NI    OPOPTNS(R8),255-OPASID
         B     A000DB0
*    /****************************************************************/
*    /*                                                              */
*    /* OUTPUT QUEUE IS NOT EMPTY SO CONTINUE PROCESSING USER DATA   */
*    /*                                                              */
*    /****************************************************************/
A000AFA  TM    TOMWFLG1(R8),TOMEDSAV
         BNZ   A000B08
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
A000B08  TM    OPOPTNS(R8),OPBRK
         BNO   A000B18
         OI    TOMWFLG4(R8),TOMBRKMG
         NI    TVWAFLG2,255-TVWABIR
A000B18  TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BNO   A000B2A                     NO
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BP    A000D1A
A000B2A  TM    TOMWFLG1(R8),TOMCURST       CURSOR ALREADY SET IN RU?
         BO    A000D1A                     YES
*      /**************************************************************/
*      /*                                                            */
*      /* THIS DO GROUP IS EXECUTED EITHER IF THE CURRENT TPUT IS A  */
*      /* FULLSRCN AND THE RU IS EMPTY OR IF CURSOR IS NOT SET IN A  */
*      /* NON-EMPTY RU . ALSO ENOUGH RU SPACE EXIST FOR AT LEAST ONE */
*      /* LINE OF DATA AND PAGING MESSAGE.                           */
*      /*                                                            */
*      /**************************************************************/
         SLR   R15,R15
         L     R14,OPBUFADR(,R8)
         ST    R14,TOMQBPTR(,R8)
         L     R14,IKTIDSX1
         LTR   R14,R14
         BZ    A000BAA
         L     R14,TOMLNCNT(,R8)
         STC   R14,TVWALNCT
         LA    R14,OPARMS(,R8)
         ST    R14,668(,R8)
         LA    R14,TOMWFLG1(,R8)
         ST    R14,672(,R8)
         LA    R14,TOMBUFSZ(,R8)
         ST    R14,676(,R8)
         LA    R14,TOMBFPTR(,R8)
         ST    R14,680(,R8)
         LA    R14,TOMBFCNT(,R8)
         ST    R14,684(,R8)
         LA    R14,TOMQBPTR(,R8)
         ST    R14,688(,R8)
         LA    R14,TOMMVCNT(,R8)
         ST    R14,692(,R8)
         LA    R14,TOMQDATA(,R8)
         ST    R14,696(,R8)
         LA    R0,668(,R8)
         LA    R13,520(,R8)
         L     R15,IKTIDSX1
         BALR  R14,R15                     STD LINKAGE
         LA    R13,TOMAUTOD(,R8)
         SLR   R14,R14
         IC    R14,TVWALNCT
         ST    R14,TOMLNCNT(,R8)
A000BAA  LTR   R15,R15
         BNZ   A000D1A
*        /************************************************************/
*        /*                                                          */
*        /* USER EXIT NOT CALLED OR NORMAL PROCESSING WAS SPECIFIED  */
*        /*                                                          */
*        /************************************************************/
         TM    TVWAFLG5,TVWAFSM
         BNO   A000C2E
         TM    TOMWFLG1(R8),TOMEDSAV
         BO    A000C2E
         TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BNO   A000C2E                     NO
         TM    TVWAFLG5,TVWANFSP
         BZ    A000BD8
         TM    TVWAFLG6,TVWAIOTR
         BNZ   A000C2E
*          /**********************************************************/
*          /*                                                        */
*          /* A FULLSCRN TPUT FOLLOWS A NON-FULLSCRN TPUT WHILE IN   */
*          /* FULLSCRN MODE AND PAGING IS ALLOWED.                   */
*          /*                                                        */
*          /**********************************************************/
A000BD8  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A000BE6
         BAL   R14,SCRFORMT                PERFORM
A000BE6  OI    TOMWFLG1(R8),TOMEDSAV
         SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         SL    R12,FW7
         L     R15,TOMCOLMN(,R8)
         CR    R15,R12
         BNH   A000C00
         LR    R15,R12
A000C00  SLR   R12,R12
         CR    R15,R12
         BNL   A000C0A
         LR    R15,R12
A000C0A  ST    R15,TOMCOLMN(,R8)
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         OI    TVWAFLG5,TVWAFSW
         TM    OPOPTNS(R8),OPHOLD
         BNZ   A000C2A
         LH    R12,OPBFSZ(,R8)
         ST    R12,TOMMVCNT(,R8)
A000C2A  B     A000D1A
A000C2E  TM    TVWAFLG5,TVWAFSM
         BNO   A000C64
         NI    TVWAFLG5,255-TVWANFSP
         TM    TOMWFLG1(R8),TOMEDSAV
         BNO   A000C64
         TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BO    A000C64                     YES
*                  /**************************************************/
*                  /*                                                */
*                  /* A NON-FULLSCRN TPUT FOLLOWS A FULLSCRN TPUT    */
*                  /* WHILE IN FULLSCRN MODE.                        */
*                  /*                                                */
*                  /**************************************************/
         MVC   TOMLNCNT(4,R8),FW1
         OI    TOMWFLG2(R8),TOMALARM+TOMERASE
         OI    TVWAFLG5,TVWAWO
         BAL   R14,SCRFORMT                PERFORM
         NI    TOMWFLG1(R8),255-TOMEDSAV
         OI    TOMWFLG1(R8),TOMEXBRU
A000C64  TM    OPOPTNS(R8),OPEDIT          EDIT?
         BNZ   A000C92                     NO
         CLC   OPBFSZ(2,R8),HW1
         BNE   A000C8A
         L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'15'
         BNE   A000C8A
         BAL   R14,A001FD4                 PERFORM
         B     A000D06
A000C8A  BAL   R14,EDIT                    PERFORM
         B     A000D06
A000C92  TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BNO   A000CA2                     NO
         BAL   R14,FULSCRN                 PERFORM
         B     A000D06
A000CA2  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BZ    A000CCC
         L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'27'
         BE    A000D06
         LH    R15,OPBFSZ(,R8)
         C     R15,FW3
         BL    A000CCC
         CLI   1(R12),X'11'
         BE    A000D06
*                  /**************************************************/
*                  /*                                                */
*                  /* ASIS & CNTL TPUTS WILL BE COMBINED FOR OUTPUT  */
*                  /* WITH OTHER NON-FULLSCRN TPUTS ONLY IF NONE OF  */
*                  /* USER-SPECIFIED HEADER (CMD,WCC,SBA) IS PRESENT.*/
*                  /*                                                */
*                  /**************************************************/
A000CCC  TM    OPOPTNS(R8),X'10'
         BNO   A000D02
         TM    OPOPTNS(R8),X'20'
         BNZ   A000D02
         CLC   OPBFSZ(2,R8),HW1
         BNE   A000CFA
         L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'15'
         BNE   A000CFA
         BAL   R14,A001FD4                 PERFORM
         B     A000D06
A000CFA  BAL   R14,ASIS                    PERFORM
         B     A000D06
A000D02  BAL   R14,CNTL                    PERFORM
A000D06  TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BO    A000D1A                     YES
         TM    OPOPTNS(R8),OPASID
         BNZ   A000D1A
         NI    TVWAFLG4,255-TVWAFLSC
A000D1A  L     R12,TOMMVCNT(,R8)
         CH    R12,OPBFSZ(,R8)
         BNL   A000D5C
*        /************************************************************/
*        /*                                                          */
*        /* IF ALL USER DATA NOT PROCESS THIS ENTRY, UPDATE OUTPUT   */
*        /* QUEUE FOR NEXT ENTRY.                                    */
*        /*                                                          */
*        /************************************************************/
         AL    R12,OPBUFADR(,R8)
         ST    R12,OPBUFADR(,R8)
         MVI   OPREQ(R8),X'04'
         LA    R13,520(,R8)
         LA    R12,OPARMS(,R8)
         ST    R12,152(,R4)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASOQM-TCAST(,R12)
         LA    R1,152(,R4)
         BALR  R14,R15                     STD LINKAGE
         LA    R13,TOMAUTOD(,R8)
         OI    TOMWFLG1(R8),TOMEXDTA+TOMEXBRU
         B     A000DB0
A000D5C  TM    OPOPTNS(R8),OPHOLD
         BNO   A000D68
         OI    TOMWFLG1(R8),TOMOWAIT
A000D68  CLI   OPRC(R8),2                  IS MORE DATA ON OUTPUT QUE?
         BNE   A000D84
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         NI    TSBFLG1-TSB(R12),255-TSBIFLSH
         OI    TOMWFLG1(R8),TOMEXDTA
         B     A000DB0
A000D84  TM    OPOPTNS(R8),OPEDIT          FULLSCR?
         BO    A000DAC                     YES
         CLI   OPRC(R8),1
         BNE   A000DB0
         TM    TVWAFLG4,TVWANOFB
         BNO   A000DA4
         TM    741(R8),X'08'
         BO    A000DAC
A000DA4  TM    741(R8),X'30'
         BNO   A000DB0
A000DAC  OI    TOMWFLG1(R8),TOMEXDTA+TOMEXBRU
A000DB0  TM    TOMWFLG1(R8),TOMEXDTA
         BNO   A000A1A
         L     R14,112(,R4)
         BR    R14                         END OF BLDDTA
         EJECT
FREEBUFR ST    R14,668(,R8)
         ST    R4,672(,R8)
         ST    R7,676(,R8)
         L     R15,160(,R4)
         L     R0,0(,R15)
         LR    R1,R15
         L     R4,TVWATCB
         L     R7,PSAAOLD                  PSA REFERENCE
         B     A000DE4
         CNOP  0,4
A000DE0  EQU   *
         DC    H'0'
         DC    X'E501'                     SUBPOOL 229
A000DE4  L     R3,A000DE0
         L     R15,CVTPTR                  CVT ADDRESS
         L     R15,CVTCRMN-CVT(,R15)
         BALR  R14,R15                     BRANCH ENTRY FREEMAIN
         L     R7,676(,R8)
         L     R4,672(,R8)
         SLR   R15,R15
         ST    R15,160(,R4)
         L     R14,668(,R8)
         BR    R14                         END OF FREEBUFR
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, CNTL,  BUILDS CONTROL DATA IN THE OUTPU RU.      */
*/* VALID CONTROL CHARATERS ARE PT, IC, BYP, RES. INVALID DATA IS    */
*/* REMOVE FROM THE OUTPUT DATA                                      */
*/*                                                                  */
*/********************************************************************/
CNTL     ST    R14,140(,R4)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMMVCNT(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A000E28
         LR    R15,R14
A000E28  AL    R15,FW15
         CR    R12,R15
         BNL   A000E3A
         OI    TOMWFLG1(R8),TOMENDBF
         B     A000FE8
A000E3A  L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A000E84
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A000E54
         BAL   R14,SCRFORMT                PERFORM
A000E54  SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         SL    R12,FW7
         L     R15,TOMCOLMN(,R8)
         CR    R15,R12
         BNH   A000E6A
         LR    R15,R12
A000E6A  SLR   R12,R12
         CR    R15,R12
         BNL   A000E74
         LR    R15,R12
A000E74  ST    R15,TOMCOLMN(,R8)
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         B     A000FE8
A000E84  TM    TOMWFLG1(R8),X'40'
         BNO   A000E94
         TM    TOMWFLG1(R8),X'80'
         BZ    A000E9A
A000E94  SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
A000E9A  BAL   R14,CNTLHDRS                PERFORM
         B     A000FCC
A000EA2  LH    R12,OPBFSZ(,R8)
         SL    R12,TOMQDATA(,R8)
         LA    R15,80
         C     R15,TOMPLSZ(,R8)                                 ZP60009
         BNL   *+8                                              ZP60009
         L     R15,TOMPLSZ(,R8)                                 ZP60009
         CR    R12,R15
         BNH   A000EB6
         LR    R12,R15
A000EB6  STC   R12,TOMWRKSZ(,R8)
         BCTR  R12,0
         L     R1,TOMQBPTR(,R8)
         EX    R12,A002036
         NI    TOMWFLG1(R8),X'BF'
         OI    TOMWFLG1(R8),X'80'
         LA    R12,1
         B     A000FA2
A000ED4  L     R12,TOMBFCNT(,R8)
         LA    R15,2
         L     R14,TOMBUFSZ(,R8)
         SLR   R14,R15
         CR    R12,R14
         BNL   A000FB2
         L     R1,TOMWRKVI(,R8)
         ALR   R1,R8
         CLI   439(R1),X'14'
         BNE   A000F0E
         ALR   R12,R15
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFWRTATT
         NI    TVWAFLG4,255-TVWANOFB
         B     A000F8E
A000F0E  L     R12,TOMWRKVI(,R8)
         ALR   R12,R8
         CLI   439(R12),X'24'
         BNE   A000F42
*        /************************************************************/
*        /*                                                          */
*        /* THIS DO GROUP PUTS BYPASS ATTRIBUTE IN OUTPUT RU         */
*        /*                                                          */
*        /************************************************************/
         LA    R12,3
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFBYPATT
         MVI   2(R12),X'13'
         OI    TOMWFLG1(R8),TOMCURST
         OI    TVWAFLG4,TVWANOFB
         B     A000F8E
A000F42  L     R12,TOMWRKVI(,R8)
         ALR   R12,R8
         CLI   439(R12),X'13'
         BNE   A000F6C
*          /**********************************************************/
*          /*                                                        */
*          /* THIS DO GROUP PUTS INSERT CURSOR ORDER IN OUTPUT  RU   */
*          /*                                                        */
*          /**********************************************************/
         LA    R12,1
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVI   0(R12),X'13'
         OI    TOMWFLG1(R8),TOMCURST
         B     A000F8E
A000F6C  L     R12,TOMWRKVI(,R8)
         ALR   R12,R8
         CLI   439(R12),X'05'
         BNE   A000F8E
*            /********************************************************/
*            /*                                                      */
*            /* THIS DO GROUP PUTS PROGRAM TAB ORDER IN OUTPUT RU    */
*            /*                                                      */
*            /********************************************************/
         LA    R12,1
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVI   0(R12),X'05'
A000F8E  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
         LA    R12,1
         AL    R12,TOMWRKVI(,R8)
A000FA2  ST    R12,TOMWRKVI(,R8)
         SLR   R15,R15
         IC    R15,TOMWRKSZ(,R8)
         CR    R12,R15
         BNH   A000ED4
A000FB2  L     R12,TOMMVCNT(,R8)
         AL    R12,TOMWRKVI(,R8)
         BCTR  R12,0
         ST    R12,TOMMVCNT(,R8)
         ST    R12,TOMQDATA(,R8)
         AL    R12,OPBUFADR(,R8)
         ST    R12,TOMQBPTR(,R8)
A000FCC  L     R12,TOMMVCNT(,R8)
         CH    R12,OPBFSZ(,R8)
         BNL   A000FE8
         L     R12,TOMBUFSZ(,R8)
         BCTR  R12,0
         BCTR  R12,0
         C     R12,TOMBFCNT(,R8)
         BH    A000EA2
A000FE8  L     R14,140(,R4)
         BR    R14                         END OF CNTL
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, FULSCRN, WILL EDIT USER FULLSCREEN MODE DATA AND */
*/* MOVE THE DATA TO THE OUTPUT RU.                                  */
*/*                                                                  */
*/********************************************************************/
FULSCRN  ST    R14,132(,R4)
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         TM    TVWAFLG7,TVWASND1
         BNZ   A001012
         MVI   168(R4),X'02'
         MVC   TOMLNCNT(4,R8),FW2
         NI    170(R4),X'7F'
         NI    TVWAFLG4,255-TVWAFMSC
A001012  L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'27'
         BNE   A001032
         L     R12,TOMBUFSZ(,R8)
         CH    R12,OPBFSZ(,R8)
         BNL   A001032
         BAL   R14,GETBUFR                 PERFORM
         B     A001052
A001032  L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'27'
         BE    A001052
         LA    R12,1
         AH    R12,OPBFSZ(,R8)
         C     R12,TOMBUFSZ(,R8)
         BNH   A001052
         BAL   R14,GETBUFR                 PERFORM
A001052  L     R12,TOMQBPTR(,R8)
         TM    OPOPTNS(R8),OPNOED          NOEDIT?              ZP60009
         BO    A001094                     YES, COPY CMD + DATA ZP60009
         CLI   0(R12),X'27'
         BNE   A00107C
*  /******************************************************************/
*  /*                                                                */
*  /* INCLUDE ESCAPE CHARACTER IN COUNT OF DATA MOVED BUT DO NOT MOVE*/
*  /* ESCAPE CHARACTER TO OUTPUT RU.                                 */
*  /*                                                                */
*  /******************************************************************/
         LA    R12,1
         ST    R12,TOMMVCNT(,R8)
         AL    R12,OPBUFADR(,R8)
         ST    R12,TOMQBPTR(,R8)
         LH    R12,OPBFSZ(,R8)
         BCTR  R12,0
         STH   R12,OPBFSZ(,R8)
         B     A001094
*  /******************************************************************/
*  /*                                                                */
*  /* ESC NOT PRESENT. PUT DEFAULT WRITE COMMAND IN OUTPUT RU.       */
*  /*                                                                */
*  /******************************************************************/
A00107C  LA    R12,1
         ST    R12,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVI   0(R1),X'F1'
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)
*/********************************************************************/
*/*                                                                  */
*/*           MOVE AND EDIT FULLSCREEN MODE DATA                     */
*/*                                                                  */
*/********************************************************************/
A001094  SLR   R2,R2
         B     A001118
A00109A  L     R12,TOMQDATA(,R8)
         LA    R3,256
         ALR   R3,R12
         LH    R0,OPBFSZ(,R8)
         CR    R0,R3
         BNH   A0010B0
         LR    R0,R3
A0010B0  ST    R0,TOMMVCNT(,R8)
         SLR   R0,R12
         L     R12,TOMBFCNT(,R8)
         ALR   R12,R0
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         BCTR  R0,0
         L     R3,TOMQBPTR(,R8)
         LR    R15,R0
         EX    R15,A00203C                 MOVE DATA INTO RU
         TM    OPOPTNS(R8),OPNOED          NOEDIT?              ZP60009
         BO    A0010F6                     YES, SKIP EDIT       ZP60009
         L     R3,TOMMVCNT(,R8)
         SL    R3,TOMQDATA(,R8)
         BCTR  R3,0
*                                              22ND APRIL, 2019 ZP60009
*  IT SEEMS THAT - LIKE TPUT NOEDIT - TPUT FULLSCR DOES NOT     ZP60009
*  PERFORM ANY DATA FILTERING THESE DAYS.  ACCORDINGLY, ANY     ZP60009
*  REFERENCE TO FLSCRTAB CAN NOW BE DISPENSED WITH.             ZP60009
*  TO REINSTATE THE TRANSLATION, CHANGE X'4700' TO X'4430'.     ZP60009
****     EX    R3,A002042                  EDIT RU DATA WITH FLSCRTAB
         NOP   A002042                     ZAP PLACEHOLDER      ZP60009
         LTR   R2,R2                       CURSOR FOUND?
         BNZ   A0010F6                     YES, SKIP SEARCH
         L     R3,TOMBFPTR(,R8)
         L     R12,TOMMVCNT(,R8)
         SL    R12,TOMQDATA(,R8)
         BCTR  R12,0
         EX    R12,A002048                 SEARCH FOR CURSOR
A0010F6  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
         L     R12,TOMMVCNT(,R8)
         L     R3,TOMQBPTR(,R8)
         ALR   R3,R12
         SL    R3,TOMQDATA(,R8)
         ST    R3,TOMQBPTR(,R8)
         ST    R12,TOMQDATA(,R8)
A001118  L     R12,TOMMVCNT(,R8)
         CH    R12,OPBFSZ(,R8)
         BL    A00109A
         LTR   R2,R2                       CURSOR FOUND?
         BNZ   A001152                     YES
         TM    OPOPTNS(R8),OPNOED          NOEDIT?              ZP60009
         BO    A001152                     YES, DON'T ADD IC    ZP60009
         L     R12,TOMGETMN(,R8)
         CLI   0(R12),X'6F'                ERASE ALL UNPROTECTED?
         BE    A001152                     YES, DON'T ADD IC
         CLC   0(2,R12),WRTWCC             PARTIAL SCREEN?      ZP60009
         BE    A001152                     MAYBE, DON'T ADD IC  ZP60009
         LA    R12,1
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R3,TOMBFPTR(,R8)
         MVI   0(R3),X'13'
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)
A001152  OI    TOMWFLG1(R8),TOMCURST
         OI    TVWAFLG4,TVWAFLSC
         OI    TOMWFLG2(R8),TOMTRSBA
         OI    TOMWFLG1(R8),TOMEDSAV
         TM    TVWAFLG5,TVWAFSM
         BNZ   A001174
         MVC   TOMLNCNT(4,R8),FW1
         B     A001178
A001174  NI    TVWAFLG4,255-TVWAFMSC
A001178  L     R14,132(,R4)
         BR    R14                         END OF FULSCRN
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, ASIS, WILL EDIT ASIS MODE  USER DATA. CONTROL    */
*/* HEADER INFORMATION (CMD,WCCSBA@@,SFATTRIB) WILL BE WILL BE VALID */
*/* IN ANY COMBINATION(NO REORDERING). MISSING CONTROL INFO  WILL BE */
*/* DEFAULTED. LINE COUNTING WILL BE IN EFFECT.                      */
*/*                                                                  */
*/********************************************************************/
ASIS     ST    R14,136(,R4)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMMVCNT(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A0011A0
         LR    R15,R14
A0011A0  AL    R15,FW15
         CR    R12,R15
         BNL   A0011B2
         OI    TOMWFLG1(R8),TOMENDBF
         B     A001512
A0011B2  L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A0011FC
*  /******************************************************************/
*  /*                                                                */
*  /*    BOTTOM OF SCREEN HAS BEEN REACHED SO PAGE AND CONTINUE      */
*  /*                                                                */
*  /******************************************************************/
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A0011CC
         BAL   R14,SCRFORMT                PERFORM
A0011CC  SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         SL    R12,FW7
         L     R15,TOMCOLMN(,R8)
         CR    R15,R12
         BNH   A0011E2
         LR    R15,R12
A0011E2  SLR   R12,R12
         CR    R15,R12
         BNL   A0011EC
         LR    R15,R12
A0011EC  ST    R15,TOMCOLMN(,R8)
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         B     A001512
A0011FC  NI    TOMWFLG1(R8),X'7F'
         OI    TOMWFLG1(R8),X'40'
         BAL   R14,CNTLHDRS                PERFORM
         LA    R12,1
         L     R15,TOMCOLMN(,R8)
         ALR   R15,R12
         ST    R15,TOMCOLMN(,R8)
         ST    R12,TOMWRKVI(,R8)
         B     A0014FE
*  /******************************************************************/
*  /*                                                                */
*  /* THIS DO GROUP WILL BUILD LINES OF ASIS MODE DATA UNTIL THE DATA*/
*  /* COUNT IS ZERO,SCREEN PAGING IS NEEDED, OR RU BUFFER IS USED UP.*/
*  /*                                                                */
*  /******************************************************************/
A00121E  L     R12,TOMQDATA(,R8)
         SLR   R15,R15
         IC    R15,TOMLNSZ(,R8)
         LR    R14,R15
         SL    R14,TOMCOLMN(,R8)
         LH    R11,OPBFSZ(,R8)
         SLR   R11,R12
         CR    R14,R11
         BNH   A00123C
         LR    R14,R11
A00123C  L     R11,TOMBUFSZ(,R8)
         SL    R11,TOMBFCNT(,R8)
         CR    R14,R11
         BNH   A00124C
         LR    R14,R11
A00124C  ALR   R12,R14
         ST    R12,TOMNWLSZ(,R8)
         LA    R14,1
         C     R14,TOMWRKVI(,R8)
         BNE   A001272
         CR    R12,R15
         BNE   A001272
         BCTR  R12,0
         CR    R12,R14
         BNL   A00126E
         LR    R12,R14
A00126E  ST    R12,TOMNWLSZ(,R8)
A001272  MVC   TOMWRKVI(4,R8),FW1
         SLR   R12,R12
         ST    R12,TOMXCNT(,R8)
         B     A0013A8
A001282  L     R12,TOMWRKVI(,R8)
         L     R15,TOMQBPTR(,R8)
         ALR   R12,R15
         BCTR  R12,0
         CLI   0(R12),X'1D'
         BE    A0012A6
         L     R12,TOMWRKVI(,R8)
         ALR   R15,R12
         BCTR  R15,0
         CLI   0(R15),X'08'
         BNE   A0012F2
A0012A6  L     R12,TOMWRKVI(,R8)
         L     R15,TOMQBPTR(,R8)
         ALR   R12,R15
         BCTR  R12,0
         CLI   0(R12),X'1D'
         BNE   A0012CE
         TM    OPOPTNS(R8),OPASID
         BNO   A0012CE
         L     R12,TOMWRKVI(,R8)
         ALR   R15,R12
         BCTR  R15,0
         NI    1(R15),X'FE'
A0012CE  LA    R12,1
         L     R15,TOMWRKVI(,R8)
         ALR   R15,R12
         ST    R15,TOMWRKVI(,R8)
         L     R15,TOMXCNT(,R8)
         ALR   R15,R12
         ST    R15,TOMXCNT(,R8)
         AL    R12,TOMNWLSZ(,R8)
         ST    R12,TOMNWLSZ(,R8)
         B     A00139C
A0012F2  L     R12,TOMWRKVI(,R8)
         L     R15,TOMQBPTR(,R8)
         ALR   R12,R15
         BCTR  R12,0
         CLI   0(R12),X'11'
         BE    A001316
         L     R12,TOMWRKVI(,R8)
         ALR   R15,R12
         BCTR  R15,0
         CLI   0(R15),X'12'
         BNE   A00133C
A001316  LA    R12,3
         L     R15,TOMNWLSZ(,R8)
         ALR   R15,R12
         ST    R15,TOMNWLSZ(,R8)
         LA    R15,2
         AL    R15,TOMWRKVI(,R8)
         ST    R15,TOMWRKVI(,R8)
         AL    R12,TOMXCNT(,R8)
         ST    R12,TOMXCNT(,R8)
         B     A00139C
A00133C  L     R12,TOMWRKVI(,R8)
         L     R1,TOMQBPTR(,R8)
         ALR   R1,R12
         BCTR  R1,0
         CLI   0(R1),X'3C'
         BNE   A001372
         LA    R15,4
         L     R14,TOMXCNT(,R8)
         ALR   R14,R15
         ST    R14,TOMXCNT(,R8)
         AL    R15,TOMNWLSZ(,R8)
         ST    R15,TOMNWLSZ(,R8)
         AL    R12,FW3
         ST    R12,TOMWRKVI(,R8)
         B     A00139C
A001372  L     R12,TOMWRKVI(,R8)
         L     R1,TOMQBPTR(,R8)
         ALR   R1,R12
         BCTR  R1,0
         CLI   0(R1),X'05'
         BNE   A00139C
         LA    R12,1
         L     R15,TOMXCNT(,R8)
         ALR   R15,R12
         ST    R15,TOMXCNT(,R8)
         AL    R12,TOMNWLSZ(,R8)
         ST    R12,TOMNWLSZ(,R8)
A00139C  LA    R12,1
         AL    R12,TOMWRKVI(,R8)
         ST    R12,TOMWRKVI(,R8)
A0013A8  L     R12,TOMNWLSZ(,R8)
         LR    R15,R12
         SL    R15,TOMQDATA(,R8)
         C     R15,TOMWRKVI(,R8)
         BL    A0013D2
         CH    R12,OPBFSZ(,R8)
         BH    A0013D2
         AL    R12,TOMBFCNT(,R8)
         LCR   R12,R12
         AL    R12,TOMBUFSZ(,R8)
         LTR   R12,R12
         BP    A001282
A0013D2  L     R12,TOMNWLSZ(,R8)
         LH    R15,OPBFSZ(,R8)
         CR    R12,R15
         BNH   A0013E2
         LR    R12,R15
A0013E2  ST    R12,TOMNWLSZ(,R8)
         B     A001474
A0013EA  L     R12,TOMQDATA(,R8)
         LA    R15,80
         C     R15,TOMPLSZ(,R8)                                 ZP60009
         BNL   *+8                                              ZP60009
         L     R15,TOMPLSZ(,R8)                                 ZP60009
         ALR   R15,R12
         L     R14,TOMNWLSZ(,R8)
         CR    R14,R15
         BNH   A001400
         LR    R14,R15
A001400  ST    R14,TOMWRKVJ(,R8)
         SLR   R14,R12
         L     R12,TOMBUFSZ(,R8)
         BCTR  R12,0
         L     R15,TOMBFCNT(,R8)
         SLR   R12,R15
         CR    R14,R12
         BNH   A00141A
         LR    R14,R12
A00141A  STC   R14,TOMWRKSZ(,R8)
         L     R12,TOMMVCNT(,R8)
         ALR   R12,R14
         ST    R12,TOMMVCNT(,R8)
         LR    R11,R14
         BCTR  R11,0
         L     R1,TOMQBPTR(,R8)
         EX    R11,A002036
         SLR   R10,R10
         IC    R10,TOMWRKSZ(,R8)
         BCTR  R10,0
         EX    R10,A00204E
         ALR   R15,R14
         ST    R15,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         EX    R11,A002054
         ST    R12,TOMQDATA(,R8)
         AL    R15,TOMGETMN(,R8)
         ST    R15,TOMBFPTR(,R8)
         AL    R12,OPBUFADR(,R8)
         ST    R12,TOMQBPTR(,R8)
         AL    R14,TOMCOLMN(,R8)
         SL    R14,TOMXCNT(,R8)
         ST    R14,TOMCOLMN(,R8)
         SLR   R12,R12
         ST    R12,TOMXCNT(,R8)
A001474  SLR   R12,R12
         L     R15,TOMNWLSZ(,R8)
         SL    R15,TOMQDATA(,R8)
         CR    R15,R12
         BNH   A001494
         L     R15,TOMBUFSZ(,R8)
         BCTR  R15,0
         SL    R15,TOMBFCNT(,R8)
         CR    R15,R12
         BH    A0013EA
A001494  L     R12,TOMMVCNT(,R8)
         CH    R12,OPBFSZ(,R8)
         BNE   A0014A8
         BAL   R14,ENDDATA                 PERFORM
         B     A0014FE
A0014A8  SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMMVCNT(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A0014D8
         LR    R15,R14
A0014D8  AL    R15,FW15
         CR    R12,R15
         BNL   A0014EA
         OI    TOMWFLG1(R8),TOMENDBF
         B     A0014FE
A0014EA  BAL   R14,NEWLINE                 PERFORM
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A0014FE
         BAL   R14,SCRNPAGE                PERFORM
A0014FE  L     R12,TOMMVCNT(,R8)
         CH    R12,OPBFSZ(,R8)
         BNL   A001512
         TM    TOMWFLG1(R8),TOMENDBF+TOMENDSC
         BZ    A00121E
A001512  L     R14,136(,R4)
         BR    R14                         END OF ASIS
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, ENDDATA  , WILL BE CALLED TO PROCESS LAST TWO    */
*/* BYTES OF USER DATA IF EDIT OR ASIS IS SPECIFIED.                 */
*/*                                                                  */
*/********************************************************************/
ENDDATA  ST    R14,148(,R4)
         LA    R12,2
         L     R15,TOMBFCNT(,R8)
         L     R14,TOMGETMN(,R8)
         ALR   R14,R15
         SLR   R14,R12
         ST    R14,TOMBFPTR(,R8)
         L     R11,OPBUFADR(,R8)
         AL    R11,TOMMVCNT(,R8)
         SLR   R11,R12
         ST    R11,TOMQBPTR(,R8)
         CLI   1(R11),X'24'
         BNE   A001562
*  /******************************************************************/
*  /*                                                                */
*  /*       LAST IS BYPASS. REPLACE WITH START FIELD & BYPASS ATTRIB */
*  /*                                                                */
*  /******************************************************************/
         AL    R15,FW1
         ST    R15,TOMBFCNT(,R8)
         MVC   1(2,R14),SFBYPATT
         OI    TVWAFLG4,TVWANOFB
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         B     A001720
A001562  LH    R12,OPBFSZ(,R8)
         C     R12,FW1
         BNH   A00159E
         L     R12,TOMQBPTR(,R8)
         CLC   0(2,R12),BYPNL
         BE    A001586
         CLC   0(2,R12),BYPCR
         BNE   A00159E
A001586  L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFBYPATT
         OI    TVWAFLG4,TVWANOFB
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         B     A001720
A00159E  L     R12,TOMQBPTR(,R8)
         CLI   1(R12),X'13'
         BNE   A0015D4
         TM    OPOPTNS(R8),X'10'
         BNO   A0015D4
         TM    OPOPTNS(R8),X'20'
         BNZ   A0015D4
*      /**************************************************************/
*      /*                                                            */
*      /* INSERT CURSOR IS VALID AS LAST CHARACTER FOR ASIS MODE DATA*/
*      /*                                                            */
*      /**************************************************************/
         L     R12,TOMBFPTR(,R8)
         MVI   1(R12),X'13'
         OI    TOMWFLG1(R8),TOMCURST
         L     R12,TOMCOLMN(,R8)
         BCTR  R12,0
         ST    R12,TOMCOLMN(,R8)
         B     A001720
*      /**************************************************************/
*      /*                                                            */
*      /* IF VALID BYPASS IS NOT FOUND AND CURSOR IS NOT SET         */
*      /* DO GROUP WILL PUT DEFAULT ATTRIBUTE AT END OF DATA         */
*      /*                                                            */
*      /**************************************************************/
A0015D4  L     R12,TOMQBPTR(,R8)
         CLI   1(R12),X'15'
         BNE   A0015EA
         L     R12,TOMBFCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMBFCNT(,R8)
A0015EA  L     R12,TOMBFCNT(,R8)
         L     R15,TOMGETMN(,R8)
         ALR   R15,R12
         ST    R15,TOMBFPTR(,R8)
         L     R14,TOMCOLMN(,R8)
         SLR   R11,R11
         IC    R11,TOMLNSZ(,R8)
         CR    R14,R11
         BNL   A0016B0
         AL    R12,FW2
         ST    R12,TOMBFCNT(,R8)
         LA    R10,1
         ALR   R14,R10
         ST    R14,TOMCOLMN(,R8)
         MVC   0(2,R15),SFRDATT
         TM    OPOPTNS(R8),X'10'
         BNO   A0016A6
         TM    OPOPTNS(R8),X'20'
         BNZ   A0016A6
         L     R1,TOMQBPTR(,R8)
         CLI   1(R1),X'15'
         BE    A0016A6
         CR    R14,R11
         BNL   A00165A
         ALR   R12,R10
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMLNCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMLNCNT(,R8)
         MVI   2(R15),X'13'
         B     A001720
*          /**********************************************************/
*          /*                                                        */
*          /*  NO SPACE ON THIS LINE PUT ATTRIBUTE ON NEXT LINE      */
*          /*                                                        */
*          /**********************************************************/
A00165A  LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
         SLR   R15,R15
         ST    R15,TOMCOLMN(,R8)
         C     R12,TOMLNNO(,R8)
         BNL   A0016A2
         L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
*              /******************************************************/
*              /*                                                    */
*              /*THIS IS NOT THE LAST LINE. PUT IN ATTRIB AND CURSOR */
*              /*                                                    */
*              /******************************************************/
         BAL   R14,NEWLINE                 PERFORM
         LA    R12,1
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMLNCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMLNCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVI   0(R12),X'13'
A0016A2  B     A001720
A0016A6  SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         B     A001720
A0016B0  LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
         SLR   R15,R15
         ST    R15,TOMCOLMN(,R8)
         C     R12,TOMLNNO(,R8)
         BNL   A001720
         CLI   OPRC(R8),2
         BE    A0016E2
         TM    OPOPTNS(R8),X'10'
         BNO   A001716
         TM    OPOPTNS(R8),X'20'
         BNZ   A001716
A0016E2  BAL   R14,NEWLINE                 PERFORM
         LA    R12,2
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFRDATT
         L     R12,TOMLNCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMLNCNT(,R8)
         LA    R12,1
         AL    R12,TOMCOLMN(,R8)
         ST    R12,TOMCOLMN(,R8)
         B     A001720
A001716  L     R12,TOMLNCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMLNCNT(,R8)
A001720  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
         L     R12,OPBUFADR(,R8)
         AL    R12,TOMMVCNT(,R8)
         ST    R12,TOMQBPTR(,R8)
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BNL   A001758
         TM    TOMWFLG1(R8),TOMCURST
         BO    A001758
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
A001758  L     R14,148(,R4)
         BR    R14                         END OF ENDDATA
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, CNTLHDRS, WILL BUILD THE OUTPUT HEADER FOR CON-  */
*/* TROL OR ASIS MODE USER DATA.                                     */
*/*                                                                  */
*/********************************************************************/
CNTLHDRS ST    R14,144(,R4)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMMVCNT(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A001780
         LR    R15,R14
A001780  AL    R15,FW15
         CR    R12,R15
         BNL   A001792
         OI    TOMWFLG1(R8),TOMENDBF
         B     A0019C2
A001792  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A0018BA
*      /**************************************************************/
*      /*                                                            */
*      /* BUILD CONTROL HEADER (CMD,WCC,SBA ORDER) ONLY IF RU IS     */
*      /* EMPTY.                                                     */
*      /*                                                            */
*      /**************************************************************/
         LA    R15,2
         CH    R15,OPBFSZ(,R8)
         BNL   A0017E4
         L     R14,TOMQBPTR(,R8)
         CLI   0(R14),X'27'
         BNE   A0017E4
*          /**********************************************************/
*          /*                                                        */
*          /* ESC PRESENT, TAKE USER'S COMMAND.                      */
*          /*                                                        */
*          /**********************************************************/
         AL    R12,FW1
         ST    R12,TOMBFCNT(,R8)
         ST    R15,TOMMVCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVC   0(2,R1),1(R14)
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)
         L     R12,OPBUFADR(,R8)
         ALR   R12,R15
         ST    R12,TOMQBPTR(,R8)
         ST    R15,TOMQDATA(,R8)
         B     A0017FC
*          /**********************************************************/
*          /*                                                        */
*          /* ESCAPE CHAR IS NOT PRESENT SO PUT IN DEFAULT WRITE     */
*          /* COMMAND.                                               */
*          /*                                                        */
*          /**********************************************************/
A0017E4  BAL   R14,SCRFORMT                PERFORM
         L     R12,TOMBFCNT(,R8)
         BCTR  R12,0
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         BCTR  R12,0
         ST    R12,TOMBFPTR(,R8)
A0017FC  LA    R12,4
         CH    R12,OPBFSZ(,R8)
         BNL   A00184E
         L     R15,TOMQBPTR(,R8)
         CLI   1(R15),X'11'
         BNE   A00184E
*          /**********************************************************/
*          /*                                                        */
*          /* MOVE USER WRITE CONTROL CHAR AND SBA ORDER INTO        */
*          /* OUTPUT RU.                                             */
*          /*                                                        */
*          /**********************************************************/
         L     R14,TOMBFCNT(,R8)
         ALR   R14,R12
         ST    R14,TOMBFCNT(,R8)
         AL    R12,TOMMVCNT(,R8)
         ST    R12,TOMMVCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVC   0(4,R1),0(R15)
         AL    R14,TOMGETMN(,R8)
         ST    R14,TOMBFPTR(,R8)
         L     R15,OPBUFADR(,R8)
         ALR   R15,R12
         ST    R15,TOMQBPTR(,R8)
         ST    R12,TOMQDATA(,R8)
         OI    TOMWFLG2(R8),TOMTRSBA
         B     A0018BE
A00184E  LA    R12,3
         CH    R12,OPBFSZ(,R8)
         BNL   A00189C
         L     R15,TOMQBPTR(,R8)
         CLI   1(R15),X'1D'
         BNE   A00189C
*      /**************************************************************/
*      /*                                                            */
*      /* IF START FIELD IS PRESENT, TAKE USER'S ATTRIBUTE CHARACTER.*/
*      /*                                                            */
*      /**************************************************************/
         L     R14,TOMBFCNT(,R8)
         ALR   R14,R12
         ST    R14,TOMBFCNT(,R8)
         AL    R12,TOMMVCNT(,R8)
         ST    R12,TOMMVCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVC   0(3,R1),0(R15)
         AL    R14,TOMGETMN(,R8)
         ST    R14,TOMBFPTR(,R8)
         L     R15,OPBUFADR(,R8)
         ALR   R15,R12
         ST    R15,TOMQBPTR(,R8)
         ST    R12,TOMQDATA(,R8)
         B     A0018BE
*          /**********************************************************/
*          /*                                                        */
*          /* USER SBA NOT IN HEADER SO USER DEFAULT WCC AND LINE    */
*          /* ADDRESS                                                */
*          /*                                                        */
*          /**********************************************************/
A00189C  LA    R12,1
         L     R15,TOMBFCNT(,R8)
         ALR   R15,R12
         ST    R15,TOMBFCNT(,R8)
         AL    R12,TOMBFPTR(,R8)
         ST    R12,TOMBFPTR(,R8)
         BAL   R14,NEWLINE                 PERFORM
         B     A0018BE
A0018BA  BAL   R14,NEWLINE                 PERFORM
A0018BE  LH    R12,OPBFSZ(,R8)
         SL    R12,TOMMVCNT(,R8)
         C     R12,FW1
         BNH   A00192A
         L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'1D'
         BNE   A00192A
         TM    OPOPTNS(R8),X'10'
         BNO   A0018F6
         TM    OPOPTNS(R8),X'20'
         BNZ   A0018F6
         TM    OPOPTNS(R8),OPASID
         BNO   A0018F6
         NI    1(R12),X'FE'
A0018F6  LA    R12,2
         L     R15,TOMBFCNT(,R8)
         ALR   R15,R12
         ST    R15,TOMBFCNT(,R8)
         AL    R12,TOMMVCNT(,R8)
         ST    R12,TOMMVCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         L     R15,TOMQBPTR(,R8)
         MVC   0(2,R1),0(R15)
         ST    R12,TOMQDATA(,R8)
         AL    R12,OPBUFADR(,R8)
         ST    R12,TOMQBPTR(,R8)
         B     A0019B6
A00192A  TM    OPOPTNS(R8),X'10'
         BNO   A0019B6
         TM    OPOPTNS(R8),X'20'
         BNZ   A0019B6
         LH    R12,OPBFSZ(,R8)
         SL    R12,TOMMVCNT(,R8)
         C     R12,FW1
         BNE   A001978
         L     R12,TOMQBPTR(,R8)
         CLI   0(R12),X'24'
         BE    A0019B6
         CLI   0(R12),X'13'
         BE    A0019B6
         LA    R12,2
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFWRTATT
         B     A0019B6
A001978  LH    R12,OPBFSZ(,R8)
         SL    R12,TOMMVCNT(,R8)
         C     R12,FW2
         BNE   A0019A0
         L     R12,TOMQBPTR(,R8)
         CLC   0(2,R12),BYPNL
         BE    A0019B6
         CLC   0(2,R12),BYPCR
         BE    A0019B6
A0019A0  LA    R12,2
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFWRTATT
A0019B6  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
A0019C2  L     R14,144(,R4)
         BR    R14                         END OF CNTLHDRS
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, EDIT, DOES THE EDITING FOR EDIT MODE DATA. IN-   */
*/* VALID CHARACTERS ARE REPLACED WITH COLONS. BYPASS ('24'X) IS     */
*/* VALID ONLY AS LAST OR (CONDITIONALLY) NEXT TO LAST CHARACTER.    */
*/*                                                                  */
*/********************************************************************/
EDIT     ST    R14,128(,R4)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMQDATA(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A0019EA
         LR    R15,R14
A0019EA  AL    R15,FW15
         CR    R12,R15
         BNL   A0019FC
         OI    TOMWFLG1(R8),TOMENDBF
         B     A001C14
A0019FC  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A001A0A
         BAL   R14,SCRFORMT                PERFORM
A001A0A  B     A001C00
*/********************************************************************/
*/*                                                                  */
*/* BUILD LINES OF DATA UNTIL DATA,SCREEN, OR RU BUFFER RUNS OUT     */
*/*                                                                  */
*/********************************************************************/
A001A0E  L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A001A4A
         SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         SL    R12,FW7
         L     R15,TOMCOLMN(,R8)
         CR    R15,R12
         BNH   A001A30
         LR    R15,R12
A001A30  SLR   R12,R12
         CR    R15,R12
         BNL   A001A3A
         LR    R15,R12
A001A3A  ST    R15,TOMCOLMN(,R8)
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         B     A001C00
A001A4A  NI    TOMWFLG1(R8),255-TOMEDSAV
         BAL   R14,NEWLINE                 PERFORM
         L     R12,TOMQDATA(,R8)
         LTR   R12,R12
         BNZ   A001AC6
         SLR   R15,R15
         IC    R15,TOMLNSZ(,R8)
         LR    R14,R15
         BCTR  R14,0
         C     R14,TOMCOLMN(,R8)
         BNE   A001AC6
*        /************************************************************/
*        /*                                                          */
*        /* ON FIRST PASS FOR EACH TPUT, PUT ATTRIB IN FRONT OF DATA */
*        /*                                                          */
*        /************************************************************/
         LA    R14,2
         AL    R14,TOMBFCNT(,R8)
         ST    R14,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVC   0(2,R1),SFWRTATT
         L     R11,TOMGETMN(,R8)
         ALR   R11,R14
         ST    R11,TOMBFPTR(,R8)
         LA    R11,1
         AL    R11,TOMLNCNT(,R8)
         ST    R11,TOMLNCNT(,R8)
         LCR   R14,R14
         AL    R14,TOMBUFSZ(,R8)
         LCR   R12,R12
         AH    R12,OPBFSZ(,R8)
         CR    R12,R15
         BNH   A001AAE
         LR    R12,R15
A001AAE  AL    R12,FW15
         CR    R14,R12
         BNL   A001ABC
         OI    TOMWFLG1(R8),TOMENDBF
A001ABC  SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         B     A001C00
A001AC6  L     R12,TOMQDATA(,R8)
         LTR   R12,R12
         BNZ   A001B32
         LA    R12,2
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         MVC   0(2,R1),SFWRTATT
         AL    R12,TOMGETMN(,R8)
         ST    R12,TOMBFPTR(,R8)
         LA    R12,1
         L     R15,TOMCOLMN(,R8)
         ALR   R15,R12
         ST    R15,TOMCOLMN(,R8)
         LCR   R15,R15
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         ALR   R15,R14
         LH    R14,OPBFSZ(,R8)
         CR    R14,R15
         BNH   A001B12
         LR    R14,R15
A001B12  LA    R15,80
         C     R15,TOMPLSZ(,R8)                                 ZP60009
         BNL   *+8                                              ZP60009
         L     R15,TOMPLSZ(,R8)                                 ZP60009
         CR    R14,R15
         BNH   A001B1E
         LR    R14,R15
A001B1E  ST    R14,TOMWRKVJ(,R8)
         CR    R14,R12
         BNL   A001B2A
         LR    R14,R12
A001B2A  STC   R14,TOMWRKSZ(,R8)
         B     A001B5C
A001B32  LH    R12,OPBFSZ(,R8)
         SL    R12,TOMQDATA(,R8)
         SLR   R15,R15
         IC    R15,TOMLNSZ(,R8)
         SL    R15,TOMCOLMN(,R8)
         CR    R12,R15
         BNH   A001B4C
         LR    R12,R15
A001B4C  LA    R15,80
         C     R15,TOMPLSZ(,R8)                                 ZP60009
         BNL   *+8                                              ZP60009
         L     R15,TOMPLSZ(,R8)                                 ZP60009
         CR    R12,R15
         BNH   A001B58
         LR    R12,R15
A001B58  STC   R12,TOMWRKSZ(,R8)
A001B5C  SLR   R12,R12
         IC    R12,TOMWRKSZ(,R8)
         L     R15,TOMCOLMN(,R8)
         ALR   R15,R12
         ST    R15,TOMCOLMN(,R8)
         LR    R15,R12
         BCTR  R15,0
         L     R14,TOMQBPTR(,R8)
         EX    R15,A00205A
         SLR   R11,R11
         IC    R11,TOMWRKSZ(,R8)
         BCTR  R11,0
         EX    R11,A002060
         L     R11,TOMBFCNT(,R8)
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)
         EX    R15,A002054
         L     R15,TOMQDATA(,R8)
         ALR   R15,R12
         ST    R15,TOMQDATA(,R8)
         AL    R11,TOMGETMN(,R8)
         ST    R11,TOMBFPTR(,R8)
         ALR   R14,R12
         ST    R14,TOMQBPTR(,R8)
         ST    R15,TOMMVCNT(,R8)
         CH    R15,OPBFSZ(,R8)
         BL    A001BC2
*        /************************************************************/
*        /*                                                          */
*        /* THIS DO GROUP PROCESSES BYPASS('24'X) WHEN FOUND IN NEXT */
*        /* LAST OR LAST DATA BYTE. A DEFAULT ATTRIBUTE IS PLACED IN */
*        /* RU WHEN A VALID BYPASS IS NOT FOUND                      */
*        /*                                                          */
*        /************************************************************/
         BAL   R14,ENDDATA                 PERFORM
         B     A001C00
*        /************************************************************/
*        /*                                                          */
*        /* MORE USER DATA LEFT SO DO NEW-LINE PROCESSING & CONTINUE */
*        /*                                                          */
*        /************************************************************/
A001BC2  LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMQDATA(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A001BF2
         LR    R15,R14
A001BF2  AL    R15,FW15
         CR    R12,R15
         BNL   A001C00
*            /********************************************************/
*            /*                                                      */
*            /* NOT ENOUGH RU BUFFER FOR LINESZ OF DATA AND PAGING   */
*            /* PROMPT SO SEND CURRENT RU AND CONTINUE               */
*            /*                                                      */
*            /********************************************************/
         OI    TOMWFLG1(R8),TOMENDBF
A001C00  L     R12,TOMQDATA(,R8)
         CH    R12,OPBFSZ(,R8)
         BNL   A001C14
         TM    TOMWFLG1(R8),TOMENDBF+TOMENDSC
         BZ    A001A0E
A001C14  L     R14,128(,R4)
         BR    R14                         END OF EDIT
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THE PROCEDURE, BLDPRMPT, WILL BUILD AUTOPROMPT DATA IN THE OUT-  */
*/* PUT RU. FOR AUTO LINE NUMBERING, THIS ROUTINE WILL BUILD A VARI- */
*/* BLE LENGTH LINE NUMBER OF 5 TO 8 DIGITS WITH LEADING ZEROS TRUN- */
*/* CATED UP TO THE FIRST 3. AUTO CHARACTER PROMPTING WILL CONSIST   */
*/* OF POSITIONING THE CURSOR. THE KEYBOARD WILL BE UNLOCKED ON      */
*/* EITHER TYPE AUTO PROMPT.                                         */
*/*                                                                  */
*/********************************************************************/
BLDPRMPT ST    R14,116(,R4)
         L     R12,TOMBUFSZ(,R8)
         SL    R12,TOMBFCNT(,R8)
         LH    R15,OPBFSZ(,R8)
         SL    R15,TOMMVCNT(,R8)
         SLR   R14,R14
         IC    R14,TOMLNSZ(,R8)
         CR    R15,R14
         BNH   A001C3C
         LR    R15,R14
A001C3C  AL    R15,FW15
         CR    R12,R15
         BNL   A001C4E
         OI    TOMWFLG1(R8),TOMENDBF
         B     A001D72
A001C4E  L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A001C98
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A001C68
         BAL   R14,SCRFORMT                PERFORM
A001C68  SLR   R12,R12
         IC    R12,TOMLNSZ(,R8)
         SL    R12,FW7
         L     R15,TOMCOLMN(,R8)
         CR    R15,R12
         BNH   A001C7E
         LR    R15,R12
A001C7E  SLR   R12,R12
         CR    R15,R12
         BNL   A001C88
         LR    R15,R12
A001C88  ST    R15,TOMCOLMN(,R8)
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
         B     A001D72
A001C98  L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A001CAA
         OI    TOMWFLG2(R8),TOMSCHED
         BAL   R14,SCRFORMT                PERFORM
A001CAA  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    TSBFLG2-TSB(R12),TSBAULST
         BNO   A001D5E
*      /**************************************************************/
*      /*                                                            */
*      /* THIS DO GROUP BUILDS A VARIABLE LENGTH LINE NUMBER         */
*      /*                                                            */
*      /**************************************************************/
         BAL   R14,NEWLINE                 PERFORM
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R12,TSBAUTOS-TSB(,R12)
         CVD   R12,176(,R4)
         OI    183(R4),X'0F'
         UNPK  184(8,R4),176(8,R4)
         LA    R12,1
         ST    R12,TOMWRKVI(,R8)
A001CE0  L     R12,TOMWRKVI(,R8)
         LA    R1,183(R12,R4)
         CLI   0(R1),C'0'
         BNE   A001D04
         LA    R12,1
         AL    R12,TOMWRKVI(,R8)
         ST    R12,TOMWRKVI(,R8)
         C     R12,FW3
         BNH   A001CE0
A001D04  L     R12,TOMWRKVI(,R8)
         LA    R15,8
         SLR   R15,R12
         AL    R15,FW1
         STH   R15,TVWANCNT
         LA    R14,5
         AL    R14,TOMBFCNT(,R8)
         ALR   R14,R15
         ST    R14,TOMBFCNT(,R8)
         AL    R15,FW2
         STH   R15,TVWANCNT
         L     R15,TOMBFPTR(,R8)
         MVC   0(2,R15),SFWRTATT
         LA    R14,11
         SLR   R14,R12
         SL    R14,FW3
         LA    R1,183(R12,R4)
         EX    R14,A002066
         LA    R14,12
         SLR   R14,R12
         ALR   R15,R14
         BCTR  R15,0
         LCR   R12,R12
         AL    R12,FW14
         SLR   R12,R14
         EX    R12,A00206C
A001D5E  L     R12,TOMGETMN(,R8)
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFPTR(,R8)
         OI    TOMWFLG1(R8),TOMCURST
         OI    TOMWFLG2(R8),TOMKBULK
A001D72  L     R14,116(,R4)
         BR    R14                         END OF BLDPRMPT
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, SCRNPAGE, WILL BE CALLED TO BUILD THE PAGING     */
*/* PROMPT MESSAGE AND TO SET APPROPRIATE INDICATORS.                */
*/*                                                                  */
*/********************************************************************/
SCRNPAGE ST    R14,12(,R13)
         NI    OPOPTNS(R8),255-OPNOED      ENSURE NOT NOEDIT    ZP60009
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R12,TSBEXTNT-TSB(,R12)
         TM    TSBXFLG1-TSBX(R12),TSBXDOWN SESSION SHUTTING DOWN?
         BNO   A001DAA                     NO
         MVI   TVWALNCT,1                  YES, SET CURRENT LINE TO 1
         MVC   TOMLNCNT(4,R8),FW1
         OI    TVWAFLG4,TVWAFMSC           FORMAT SCREEN REQUIRED
         NI    TVWAFLG2,255-TVWAPGN        NOT DOING PAGING
         OI    TOMWFLG1(R8),TOMENDBF       END OF RU BUFFER
         B     A001DD0
A001DAA  LA    R12,8                       SET COUNT FOR PAGING MESSAGE
         AL    R12,TOMBFCNT(,R8)
         ST    R12,TOMBFCNT(,R8)
         L     R1,TOMBFPTR(,R8)            PUT PAGE PROMPT IN OUTPUT RU
         MVC   0(8,R1),A0020DB
         AL    R12,TOMGETMN(,R8)           UPDATE RU BUFFER POINTER
         ST    R12,TOMBFPTR(,R8)
         OI    TVWAFLG2,TVWAPGN            PAGING RESPONSE OUTSTANDING
         OI    TOMWFLG2(R8),TOMKBULK       SET KEYBOARD UNLOCK NEEDED
*                                          INDICATE CURSOR SET IN RU
*                                          END OF SCRN HAS BEEN REACHED
*        (LEAVE MAIN LOOP OF BLDDTA)       DATA BLD COMPLETE THIS ENTRY
*        (LEAVE MAIN LOOP OF BLDRU)        RU BUILD COMPLETE THIS ENTRY
A001DD0  OI    TOMWFLG1(R8),TOMCURST+TOMENDSC+TOMEXDTA+TOMEXBRU
         L     R14,12(,R13)
         BR    R14                         END OF SCRNPAGE
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, BLDERR, WILL BE CALLED TO PUT ERROR MESSAGES IN  */
*/* THE OUTPUT RU                                                    */
*/*                                                                  */
*/********************************************************************/
BLDERR   ST    R14,104(,R4)
         SLR   R12,R12
         ST    R12,TOMCOLMN(,R8)
         C     R12,TOMBFCNT(,R8)
         BNE   A001DF0
         BAL   R14,SCRFORMT                PERFORM
A001DF0  BAL   R14,NEWLINE                 PERFORM
         L     R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A001E08
         BAL   R14,SCRNPAGE                PERFORM
         B     A001E70
A001E08  LA    R12,4
         ST    R12,TOMWRKVI(,R8)
         LR    R15,R12
         SLA   R15,2
         L     R1,CVTPTR                   CVT ADDRESS
         L     R1,CVTTCASP-CVT(,R1)
         L     R1,TCASMSGS-TCAST(,R1)
         AL    R1,FWMINUS4
         L     R15,0(R15,R1)
         LH    R14,0(,R15)
         L     R11,TOMBFCNT(,R8)
         ALR   R11,R14
         ALR   R11,R12
         ST    R11,TOMBFCNT(,R8)
         L     R12,TOMBFPTR(,R8)
         MVC   0(2,R12),SFWRTATT
         LR    R10,R14
         BCTR  R10,0
         EX    R10,A002072
         ALR   R12,R14
         MVC   2(2,R12),SFRDATT
         AL    R11,TOMGETMN(,R8)
         ST    R11,TOMBFPTR(,R8)
         NI    TVWAFLG2,255-TVWAERMG
         OI    TVWAFLG1,TVWAULK
         LA    R12,1
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
A001E70  L     R14,104(,R4)
         BR    R14                         END OF BLDERR
         EJECT
*/********************************************************************/
*/*                                                                  */
*/* THIS PROCEDURE, TRANPROC, WILL PERFORM USER AND/OR ASCII TRANS-  */
*/* LATION ON THE OUTBOUND RU.                                       */
*/*                                                                  */
*/********************************************************************/
TRANPROC ST    R14,12(,R13)
         TM    TVWAFLG3,TVWATRAN
         BNO   A001F6C
         SLR   R12,R12
         ST    R12,TOMWRKVJ(,R8)
         LA    R3,3
         B     A001F60
A001E90  SLR   R1,R1
         L     R12,TOMWRKVJ(,R8)
         L     R0,TOMGETMN(,R8)
         ALR   R0,R12
         ST    R0,TOMBFPTR(,R8)
         LCR   R12,R12
         AL    R12,TOMBFCNT(,R8)
         LA    R0,253
         CR    R12,R0
         BNH   A001EB2
         LR    R12,R0
A001EB2  ST    R12,TOMWRKVI(,R8)
         L     R12,TOMBFPTR(,R8)
         L     R15,TOMWRKVI(,R8)
         BCTR  R15,0
         EX    R15,A002078
         BC    8,NONFLG
         LR    R12,R1
         SL    R12,TOMBFPTR(,R8)
         ST    R12,TOMWRKVI(,R8)
         CLI   0(R1),X'11'
         BE    A001EE2
         CLI   0(R1),X'12'
         BNE   A001EEA
A001EE2  LA    R1,3
         B     A001F1E
A001EEA  CLI   0(R1),X'3C'
         BNE   A001EFA
         LA    R1,4
         B     A001F1E
A001EFA  CLI   0(R1),X'1D'
         BNE   A001F0A
         LA    R1,2
         B     A001F1E
A001F0A  CLI   0(R1),X'13'
         BE    A001F1A
         CLI   0(R1),X'05'
         BNE   A001F1E
A001F1A  LA    R1,1
A001F1E  L     R12,TOMWRKVI(,R8)
         CR    R3,R12
         BH    A001F4E
         C     R3,FW1
         BE    NONFLG
         C     R12,FW3
         BL    A001F4E
NONFLG   L     R12,TOMBFPTR(,R8)
         ALR   R12,R3
         BCTR  R12,0
         L     R15,TOMWRKVI(,R8)
         SLR   R15,R3
         L     R14,TVWATABO
         EX    R15,A00207E
A001F4E  LA    R3,1
         L     R12,TOMWRKVJ(,R8)
         AL    R12,TOMWRKVI(,R8)
         ALR   R12,R1
         ST    R12,TOMWRKVJ(,R8)
A001F60  L     R12,TOMWRKVJ(,R8)
         C     R12,TOMBFCNT(,R8)
         BL    A001E90
A001F6C  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R12,TSBEXTNT-TSB(,R12)
         TM    TSBXFLG1-TSBX(R12),TSBXASCI
         BNO   A001FCE
         SLR   R12,R12
         ST    R12,TOMWRKVJ(,R8)
         B     A001FC2
A001F8A  L     R12,TOMWRKVJ(,R8)
         L     R15,TOMGETMN(,R8)
         ALR   R15,R12
         ST    R15,TOMBFPTR(,R8)
         L     R14,TOMBFCNT(,R8)
         SLR   R14,R12
         LA    R11,256
         CR    R14,R11
         BNH   A001FAA
         LR    R14,R11
A001FAA  ST    R14,TOMWRKVI(,R8)
         L     R11,TOMWRKVI(,R8)
         BCTR  R11,0
         L     R1,TVWAATBO
         EX    R11,A002084
         ALR   R12,R14
         ST    R12,TOMWRKVJ(,R8)
A001FC2  L     R12,TOMWRKVJ(,R8)
         C     R12,TOMBFCNT(,R8)
         BL    A001F8A
A001FCE  L     R14,12(,R13)
         BR    R14                         END OF TRANPROC
         EJECT
A001FD4  ST    R14,124(,R4)
         L     R12,TOMBFCNT(,R8)
         LTR   R12,R12
         BNZ   A001FE6
         BAL   R14,SCRFORMT                PERFORM
A001FE6  LA    R12,1
         ST    R12,TOMMVCNT(,R8)
         SLR   R15,R15
         ST    R15,TOMCOLMN(,R8)
         AL    R12,TOMLNCNT(,R8)
         ST    R12,TOMLNCNT(,R8)
         C     R12,TOMLNNO(,R8)
         BL    A00200C
         BAL   R14,NEWLINE                 PERFORM
         BAL   R14,SCRNPAGE                PERFORM
A00200C  L     R14,124(,R4)
         BR    R14                         EXIT
         EJECT
A002012  L     R13,4(,R13)
         LM    R14,R12,12(R13)             RESTORE REGS
         BR    R14                         EXIT
HW1920   DC    H'1920'
HW4095   DC    H'4095'                                          ZP60009
A00201E  MVC   TOMWRKBF+7(0,R8),2(R12)
A002024  MVC   2(0,R10),0(R1)
A00202A  TR    2(0,R10),ASISTAB
A002030  MVC   2(0,R10),80(R1)
A002036  MVC   TOMWRKBF(0,R8),0(R1)
A00203C  MVC   0(0,R12),0(R3)
A002042  TR    0(0,R12),FLSCRTAB
A002048  TRT   0(0,R3),ICTAB
A00204E  TR    TOMWRKBF(0,R8),ASISTAB
A002054  MVC   0(0,R1),TOMWRKBF(R8)
A00205A  MVC   TOMWRKBF(0,R8),0(R14)
A002060  TR    TOMWRKBF(0,R8),EDITTAB
A002066  MVC   2(0,R15),0(R1)
A00206C  MVC   0(0,R15),SFRDATIC
A002072  MVC   2(0,R12),2(R15)
A002078  TRT   0(0,R12),SBATAB
A00207E  TR    0(0,R12),0(R14)
A002084  TR    0(0,R15),0(R1)
FW1      DC    0F'1'
         DC    H'0'
HW1      DC    H'1'
FW2      DC    F'2'
FW3      DC    F'3'
FW4      DC    F'4'
FW7      DC    F'7'
FW14     DC    F'14'
FW15     DC    F'15'
FW18     DC    F'18'
FW19     DC    F'19'
FW40     DC    F'40'
FW64     DC    F'64'
FW80     DC    F'80'
FWMINUS4 DC    F'-4'
         DC    X'07002011'
         DC    F'192'
IKTIDSX1 DC    V(IKTIDSX1)
         DS    0D
A0020D0  EQU   *,11                        SAVE 8 BYTES OF      ZP60009
         DC    X'11C150'                    DUPLICATE DATA      ZP60008
SFWRTATT EQU   *,2
A0020DB  DC    X'1DC85C5C5C1D4013'
A0020E3  DC    X'1DC84FC91D40'
A0020E9  DC    X'C61140401DC8'
A0020EF  DC    X'1DC84F1D40'
SFRDATT  EQU   *,2
SFRDATIC DC    X'1D4013'
BYPCR    DC    X'240D'
BYPNL    DC    X'2415'
EWRTWCCD DC    X'F5C1'
SFBYPATT DC    X'1D4C'
WRTWCC   DC    X'F140'
WRTWCCDF DC    X'F1C1'
A002103  DC    X'5D7F'
A002105  DC    X'C75F'
         DS    0D                                               ZP60009
EDITTAB  DS    CL256
         ORG   EDITTAB
         DC    X'007A7A7A7A7A7A7A7A7A7A7A0C7A7A7A'
         DC    X'7A7A7A7A7A7A7A7A7A197A7A1C7A1E7A'
         DC    X'7A7A7A7A7A7A7A7A7A7A7A7A7A7A7A7A'              ZP60009
         DC    X'7A7A7A7A7A7A7A7A7A7A7A7A7A7A7A3F'              ZP60009
         DC    X'404142434445464748494A4B4C4D4E4F'              ZP60009
         DC    X'505152535455565758595A5B5C5D5E5F'              ZP60009
         DC    X'606162636465666768696A6B6C6D6E6F'              ZP60009
         DC    X'707172737475767778797A7B7C7D7E7F'              ZP60009
         DC    X'808182838485868788898A8B8C8D8E8F'              ZP60009
         DC    X'909192939495969798999A9B9C9D9E9F'              ZP60009
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'              ZP60009
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'              ZP60009
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'              ZP60009
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'              ZP60009
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'              ZP60009
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFE7A'              ZP60009
         ORG   EDITTAB+256
ASISTAB  DS    CL256
         ORG   ASISTAB
         DC    X'007A7A7A7A057A0708090A0B0C7A0E0F'
         DC    X'7A11127A7A7A7A7A18191A1B1C1D1E1F'
         DC    X'202122237A257A7A28292A2B2C7A2E2F'
         DC    X'303132333435363738393A3B3C7A3E3F'
         DC    X'404142434445464748494A4B4C4D4E4F'
         DC    X'505152535455565758595A5B5C5D5E5F'
         DC    X'606162636465666768696A6B6C6D6E6F'
         DC    X'707172737475767778797A7B7C7D7E7F'
         DC    X'808182838485868788898A8B8C8D8E8F'
         DC    X'909192939495969798999A9B9C9D9E9F'
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'
         ORG   ASISTAB+256
FLSCRTAB DS    CL256
         ORG   FLSCRTAB
         DC    X'007A7A7A7A057A0708090A0B0C7A0E0F'
         DC    X'7A1112137A7A7A7A18191A1B1C1D1E1F'
         DC    X'2021222324257A7A28292A2B2C7A2E2F'
         DC    X'303132333435367A38393A3B3C7A3E3F'
         DC    X'404142434445464748494A4B4C4D4E4F'
         DC    X'505152535455565758595A5B5C5D5E5F'
         DC    X'606162636465666768696A6B6C6D6E6F'
         DC    X'707172737475767778797A7B7C7D7E7F'
         DC    X'808182838485868788898A8B8C8D8E8F'
         DC    X'909192939495969798999A9B9C9D9E9F'
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'
         ORG   FLSCRTAB+256
SBATAB   DS    CL256
         ORG   SBATAB
         DC    X'00000000000500000000000000000000'
         DC    X'001112130000000000000000001D0000'
         DC    X'00000000000000000000000000000000'
         DC    X'0000000000000000000000003C000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         ORG   SBATAB+256
BFADRTAB DS    CL64
         ORG   BFADRTAB
         DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'
         ORG   BFADRTAB+64
ICTAB    DS    CL256
         ORG   ICTAB
         DC    X'00000000000000000000000000000000'
         DC    X'00000013000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         DC    X'00000000000000000000000000000000'
         ORG   ICTAB+256
PATCH    DC    76F'0'
         DS    0D                          END OF CSECT
         EJECT
R0       EQU   0                           EQUATES FOR REGISTERS 0-15
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE
TOMWA    EQU   240
TOMAUTOD EQU   TOMWA
TOMWRKBF EQU   TOMWA+200
SAVEREGS EQU   TOMWA+280
TOMBFCNT EQU   TOMWA+352
TOMMVCNT EQU   TOMWA+356
TOMQDATA EQU   TOMWA+360
TOMWRKVI EQU   TOMWA+364
TOMWRKVJ EQU   TOMWA+368
TOMNWLSZ EQU   TOMWA+372
TOMQBPTR EQU   TOMWA+376
TOMGETMN EQU   TOMWA+380
TOMBUFSZ EQU   TOMWA+384
TOMBFPTR EQU   TOMWA+388
TOMWRKSZ EQU   TOMWA+392
TOMSCPRM EQU   TOMWA+396
TOMREQCD EQU   TOMSCPRM
TOMPLNNO EQU   TOMSCPRM+1
TOMLNSZ  EQU   TOMSCPRM+2
TOMLNNO  EQU   TOMSCPRM+4
TOMPLSZ  EQU   TOMSCPRM+8
TOMLNCNT EQU   TOMSCPRM+12
TOMFMTDA EQU   TOMSCPRM+16
TOMSBACD EQU   TOMFMTDA
TOMFMTAD EQU   TOMFMTDA+1
TOMFMATT EQU   TOMFMTDA+3
TOMLNADR EQU   TOMSCPRM+21
TOMROWAD EQU   TOMLNADR
TOMCOLAD EQU   TOMLNADR+1
TOMWFLGS EQU   TOMWA+421
TOMWFLG1 EQU   TOMWFLGS
TOMEDSAV EQU   X'C0'
TOMENDBF EQU   X'20'
TOMCURST EQU   X'10'
TOMENDSC EQU   X'08'
TOMEXDTA EQU   X'04'
TOMEXBRU EQU   X'02'
TOMOWAIT EQU   X'01'
TOMWFLG2 EQU   TOMWFLGS+1
TOMKBULK EQU   X'80'
TOMALARM EQU   X'40'
TOMERMG9 EQU   X'20'
TOMERASE EQU   X'10'
TOMSCHED EQU   X'08'
TOMSTCUR EQU   X'04'
TOMERLCT EQU   X'02'
TOMTRSBA EQU   X'01'
TOMWFLG3 EQU   TOMWFLGS+2
TOMBPREQ EQU   TOMWFLG3
TOMBPRSP EQU   TOMWFLG3
TOMBRKPG EQU   TOMWFLG3
TOMWFLG4 EQU   TOMWFLGS+3
TOMBRKMG EQU   X'80'
TOMEB    EQU   X'40'
TOMPGNSV EQU   X'10'
TOMUPARM EQU   TOMWA+428
TOMUPRM1 EQU   TOMUPARM
TOMUPRM2 EQU   TOMUPARM+4
TOMUPRM3 EQU   TOMUPARM+8
TOMUPRM4 EQU   TOMUPARM+12
TOMUPRM5 EQU   TOMUPARM+16
TOMUPRM6 EQU   TOMUPARM+20
TOMUPRM7 EQU   TOMUPARM+24
TOMUPRM8 EQU   TOMUPARM+28
TOMCOLMN EQU   TOMWA+464
TOMXCNT  EQU   TOMWA+468
OPARMS   EQU   TOMWA+484
OPBUFADR EQU   OPARMS
OPREQ    EQU   OPARMS+4
OPRC     EQU   OPARMS+5
OPBFSZ   EQU   OPARMS+6
OPFLAGS  EQU   OPARMS+8
OPOPTNS  EQU   OPFLAGS+1         BITS 0-3 FROM TPUT SVC GPR1 BITS 4-7
OPHOLD   EQU   X'80'
OPBRK    EQU   X'40'
OPEDIT   EQU   X'30'
OPASID   EQU   X'08'
OPNOED   EQU   X'04'                                            ZP60009
OPNXFLGS EQU   OPARMS+16
OPNXBRK  EQU   OPNXFLGS+1
OPNXEDIT EQU   OPNXFLGS+1
OPNXASID EQU   OPNXFLGS+1
         SPACE
         PRINT NOGEN
         IHAPSA
         CVT   DSECT=YES
         IHAASCB
         IKJTSB EXT=YES
         IKTTVWA
         IKTTCAST
         END   IKT3270O,(C'PLS1952',0701,83271)
/*
//*
//STEP05  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT3270O('ZP60009')
++MOD(IKT0009D) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
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
IKT0009D TITLE 'IKT0009D:  TSO/VTAM SVC 94 ROUTER                      *
                        '
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*            TO SUPPORT ENTRY CODE 17 (GTTERM)
*
IKT0009D CSECT
         BALR  R15,0                       ADDRESS SET
         USING *,R15
         B     @PROLOG
         DC    AL1(16)
         DC    CL16'IKT0009D  78.290'
         DROP  R15
@PROLOG  BALR  R12,0                       ADDRESS SET
@PSTART  DS    0H
         USING @PSTART,R12
*   /*****************************************************************/
*   /*                                                               */
*   /* PARAMETER REGISTER 0 WILL HAVE THE ENTRY CODE FOR THE SVC IN  */
*   /* THE HIGH ORDER POSITION OF REGISTER 0, IF THE ENTRY CODE IS   */
*   /* INVALID A RETURN CODE OF 4 IS RETURNED IN REG 15              */
*   /*                                                               */
*   /*****************************************************************/
         LR    R15,R0
         SRL   R15,24
         CH    R15,HW20
         BH    INVALID
         LTR   R15,R15
         BNP   INVALID
         LR    R5,R8                       COPY TSB POINTER
         LA    R10,A000058
         SLL   R15,2
         L     R15,MODTAB-4(15)
         DROP  R12
         LR    R12,R15
         BR    R15
INVALID  LA    R15,4
         BR    R14                         EXIT
INVALID8 LA    R15,8
         BR    R14                         EXIT
INVALIDC LA    R15,12
         BR    R14                         EXIT
HW20     DC    H'20'
         DC    F'8'
A000058  EQU   *
         DC    F'0'
         DC    F'99999999'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'0'
         DC    F'8288'
MODTAB   EQU   *
TCLEARQ  DC   V(IKT09401)           ENTRY CODE  1
TSEND    DC   A(INVALID)            ENTRY CODE  2,INVALID
MSGS     DC   A(INVALID)            ENTRY CODE  3,INVALID
STBREAK  DC   V(IKT09404)           ENTRY CODE  4
STCOM    DC   V(IKT09405)           ENTRY CODE  5
STTIMEO  DC   A(INVALID8)           ENTRY CODE  6,INVALID
STCC     DC   A(INVALIDC)           ENTRY CODE  7,INVALID
STATTN   DC   A(INVALID8)           ENTRY CODE  8,INVALID
STAUTOL  DC   V(IKT09409)           ENTRY CODE  9
STSIZE   DC   V(IKT0940A)           ENTRY CODE 10
GTSIZE   DC   V(IGG0940B)           ENTRY CODE 11
STAUTOC  DC   V(IKT0940C)           ENTRY CODE 12
SPAUTOP  DC   V(IKT0940D)           ENTRY CODE 13
RTAUTOP  DC   V(IKT0940E)           ENTRY CODE 14
STTRAN   DC   V(IKT0940F)           ENTRY CODE 15
STCLEAR  DC   A(INVALID8)           ENTRY CODE 16,INVALID
GTTERM   DC   V(IKT09411)           ENTRY CODE 17               ZP60009
STFSMODE DC   V(IKT09412)           ENTRY CODE 18
STLINENO DC   V(IKT09413)           ENTRY CODE 19
STTMPMD  DC   V(IKT09414)           ENTRY CODE 20
         SPACE
         DS   0D                    END OF CSECT
         EJECT
R0       EQU   0                    EQUATES FOR REGISTERS 0-15
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         TITLE '******** IKT09411:    GTTERM  - TERMINAL CONTROL MACRO *
                 *******'
IKT09411 CSECT
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS CSECT WAS WRITTEN BY GREG PRICE IN JANUARY 2003 FOR THE  */
*   /* ZP60009 USERMOD TO ADD THE GTTERM MACRO FUNCTION TO TSO/VTAM  */
*   /* ETV0108 WHICH IS SHIPPED WITH MVS 3.8J.                       */
*   /*                                                               */
*   /*****************************************************************/
         USING IKT09411,R12
         USING TSB,R5
         USING PSA,0
         B     @GTTERM
         DC    AL1(16)
         DC    CL16'IKT09411 ZP60009'
*   /*****************************************************************/
*   /*                                                               */
*   /* PARAMETER REGISTER 1 POINTS TO A PARAMETER LIST OF FROM 1 TO  */
*   /* 4 WORDS, EACH CONTAINING A POINTER TO AN OUTPUT AREA.  THE    */
*   /* PARAMETER LIST AND EACH OUTPUT AREA IS ACCESSED IN THE USER   */
*   /* KEY TO VERIFY USER AUTHORITY TO ACCESS THE AREAS.  THE LAST   */
*   /* PARAMETER LIST WORD SHOULD HAVE THE HIGH-ORDER BIT ON.        */
*   /*                                                               */
*   /*****************************************************************/
@GTTERM  LTR   R1,R1                       ANY PARAMETER PASSED?
         BNZ   HAVEPARM                    YES
MISSPARM LA    R15,12                      NO, MISSING PARAMETER
         BR    R14                         RETURN TO ISSUER
*   /*****************************************************************/
*   /*                                                               */
*   /* THE GTTERM MACRO IS ONLY VALID FOR DISPLAY STATION TERMINALS. */
*   /*                                                               */
*   /*****************************************************************/
HAVEPARM TM    TSBSTAT,TSBDSPLY            TERMINAL A DISPLAY STATION?
         BO    ISADSPLY                    YES
         LA    R15,8                       NO, WRONG TERMINAL TYPE
         BR    R14                         RETURN TO ISSUER
*   /*****************************************************************/
*   /*                                                               */
*   /* THE PARAMETERS ARE OUTPUT AREAS FOR:                          */
*   /* (1) PRMSZE - PRIMARY SCREEN SIZE DIMENSIONS - 2 BYTES         */
*   /* (2) ALTSZE - ALTERNATE SCREEN SIZE DIMENSIONS - 2 BYTES       */
*   /* (3) ATTRIB - TERMINAL ATTRIBUTES - 4 BYTES                    */
*   /* (4) TERMID - TERMINAL LU NAME - 8 BYTES                       */
*   /*                                                               */
*   /*****************************************************************/
ISADSPLY L     R8,PSATOLD
         IC    R15,TCBPKF-TCB(,R8)
         SPKA  0(R15)                      GET INTO USER KEY
         SLR   R8,R8
         SLR   R9,R9
         SLR   R10,R10
         SLR   R11,R11
         L     R8,0(,R1)                   LOAD FIRST ADDRESS
         LTR   R8,R8
         BM    GOTPARMS                    ONLY ONE PARAMETER
         L     R9,4(,R1)                   LOAD SECOND ADDRESS
         LTR   R9,R9
         BM    GOTPARMS                    ONLY TWO PARAMETERS
         L     R10,8(,R1)                  LOAD THIRD ADDRESS
         LTR   R10,R10
         BM    GOTPARMS                    ONLY THREE PARAMETERS
         L     R11,12(,R1)                 LOAD FOURTH ADDRESS
GOTPARMS LA    R8,0(,R8)                   ENSURE ADDRESS FORMAT
         LA    R9,0(,R9)
         LA    R10,0(,R10)
         LA    R11,0(,R11)
         LTR   R8,R8                       FIRST ADDRESS PRESENT?
         BNP   *+10                        NO
         XC    0(2,R8),0(R2)               YES, CLEAR 2 BYTES
         LTR   R9,R9                       SECOND ADDRESS PRESENT?
         BNP   *+10                        NO
         XC    0(2,R9),0(R9)               YES, CLEAR 2 BYTES
         LTR   R10,R10                     THIRD ADDRESS PRESENT?
         BNP   *+10                        NO
         XC    0(4,R10),0(R10)             YES, CLEAR 4 BYTES
         LTR   R11,R11                     FOURTH ADDRESS PRESENT?
         BNP   *+10                        NO
         XC    0(8,R11),0(R11)             YES, CLEAR 8 BYTES
         SPKA  0                           REVERT TO KEY ZERO
*   /*****************************************************************/
*   /*                                                               */
*   /* TSO/VTAM REQUIRES THE FIRST PARAMETER (PRMSZE) BE PRESENT.    */
*   /*                                                               */
*   /*****************************************************************/
         LTR   R8,R8                       IS PRMSZE SPECIFIED?
         BNP   MISSPARM                    NO, RETURN CODE 12
         L     R15,TSBEXTNT                YES, POINT TO TSBX
         USING TSBX,R15
         ICM   R2,3,TSBPRMR                COPY PRIMARY ROWS+COLS
         BNZ   PRIMOKAY
         ICM   R2,3,MOD1DIMS               USE MODEL-1 DIMENSIONS
         CLC   TSBXTMBF,MOD1SIZE           REALLY A MODEL-1?
         BE    PRIMOKAY                    YES
         ICM   R2,3,MOD2DIMS               USE MODEL-2 DIMENSIONS
PRIMOKAY STCM  R2,3,0(R8)                  SET PRIMARY ROWS+COLS
         LTR   R9,R9                       IS ALTSZE SPECIFIED?
         BNP   ALTDONE                     NO
         ICM   R2,3,TSBALTR                COPY ALTERNATE ROWS+COLS
         BNZ   ALTOKAY
         ICM   R2,3,MOD1DIMS               USE MODEL-1 DIMENSIONS
         CLC   TSBXTMBF,MOD1SIZE           REALLY A MODEL-1?
         BE    ALTOKAY                     YES
         ICM   R2,3,MOD2DIMS               USE MODEL-2 DIMENSIONS
ALTOKAY  STCM  R2,3,0(R9)                  AET ALTERNATE ROWS+COLS
ALTDONE  LTR   R10,R10                     IS ATTRIB SPECIFIED?
         BNP   ATTRDONE                    NO
         SLR   R8,R8
         IC    R8,TSBTERM2
         LA    R9,TSBASCI7
         NR    R8,R9                       GET ASCII-7/ASCII-8 FLAGS
         SLL   R8,8                        SHIFT TO CORRECT BYTE
         TM    TSBFLG5,TSBVTAM             VTAM TERMINAL?
         BNO   *+8                         NO, HOW DID WE GET HERE?
         LA    R8,X'80'(,R8)               YES, SET FLAG BIT ON
         TM    TSBFLG5,TSBQRY              QUERY BIT ON?
         BNO   *+8                         NO
         LA    R8,X'01'(,R8)               YES, SET FLAG BIT ON
         TM    TSBFLG3,TSBNOBRK            BREAK NOT ALLOWED NOW?
         BNO   *+8                         NO
         LA    R8,X'40'(,R8)               YES, SET FLAG BIT ON
         TM    TSBXFLG1,TSBXASCI           ASCII TERMINAL?
         BNO   *+8                         NO
         LA    R8,X'02'(,R8)               YES, SET FLAG BIT ON
         L     R9,TSBXTVWA
         USING TVWA,R9
         TM    TVWAFLG3,TVWATRAN           TRANSLATE TABLE IN USE?
         BNO   *+8                         NO
         LA    R8,X'20'(,R8)               YES, SET FLAG BIT ON
         TM    TVWAFLG3,TVWATRDF           DEFAULT TRANSLATION?
         BNO   *+8                         NO
         LA    R8,X'10'(,R8)               YES, SET FLAG BIT ON
         TM    TVWAFLG5,TVWAFSM            IN FULLSCREEN MODE?
         BNO   *+8                         NO
         LA    R8,X'08'(,R8)               YES, SET FLAG BIT ON
         STCM  R8,15,0(R10)                SET ATTRIBUTES WORD
         DROP  R9                          TVWA
ATTRDONE LTR   R11,R11                     IS TERMID SPECIFIED?
         BNP   TERMDONE                    NO
         MVC   0(8,R11),TSBTRMID           YES, SHOW TERMINAL NAME
         DROP  R15                         TSBX
TERMDONE SLR   R15,R15                     ZERO RETURN CODE
         BR    R14                         RETURN TO ISSUER
         SPACE
MOD1SIZE DC    AL2(12*40)
MOD1DIMS DC    AL1(12,40)
MOD2DIMS DC    AL1(24,80)
@PATCH11 DC    6D'0'                       PATCH AREA
         SPACE
         PRINT NOGEN
         IHAPSA
         IKJTCB
         IKJTSB EXT=YES
         IKTTVWA
         SPACE
         END   IKT0009D,(C'PLS1425',0701,78290)
/*
//*
//STEP07  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT0009D('ZP60009')
  IDENTIFY IKT09411('ZP60009')
++MOD(IKTXLOG) DISTLIB(AOST4).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP08  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'EXTENDED LOGON ROUTINE'
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*
*   /*****************************************************************/
*   /*                                                               */
*   /*   FIRSTLY PERFORM THE IBM SUPPLIED LOGIC                      */
*   /*                                                               */
*   /*****************************************************************/
IKTXLOG  CSECT
         USING IKTXLOG,R15
         B     @PROLOG
         DC    AL1(32)
         DC    CL16'IKTXLOG  ZP60009'
         DC    CL16' REWORK 20090601'
         DROP  R15
@PROLOG  STM   R14,R12,12(R13)
         LR    R12,R15
         USING IKTXLOG,R12
*
*   INBUF=TVWAVST                  INSERT ADDRESS OF LOGON BUFFER
*                                      IN PARAMETER
         USING PSA,0
         L     R15,0(,R1)          POINT TO PARAMETER
         L     R14,PSAAOLD
         USING ASCB,R14
         L     R3,ASCBTSB
         DROP  R14
         USING TSB,R3
         L     R4,TSBEXTNT
         USING TSBX,R4
         L     R5,TSBXTVWA
         USING TVWA,R5
         L     R0,TVWAVST
         ST    R0,0(,R15)          STORE ADDRESS INTO INBUF
*   /*****************************************************************/
*   /*                                                               */
*   /*   SECONDLY PERFORM THE LOGIC ADDED BY USERMOD ZP60009         */
*   /*                                                               */
*   /*   THIS EXTRA LOGIC IS ONLY FOR 3270 TERMINALS AND HAS         */
*   /*   BEEN ADDED TO FACILITATE ACCESS TO 3270 EXTENSIONS BY       */
*   /*   TSO APPLICATIONS.  THE PURPOSE OF THIS EXTRA LOGIC IS       */
*   /*   TO ASCERTAIN (A) THE PRIMARY AND ALTERNATE SCREEN SIZES     */
*   /*   AND (B) IF THE TERMINAL IS QUERYABLE.                       */
*   /*                                                               */
*   /*   THE RESULTS ARE STORED IN THE TSB, AND MADE AVAILABLE       */
*   /*   TO ISSUERS OF THE GTTERM MACRO.                             */
*   /*                                                               */
*   /*   FOR SNA 3270 SESSIONS THE SCREEN DIMENSIONS ARE INITIALLY   */
*   /*   COPIED FROM THE BIND, AND IF THE QUERY BIT IN THE BIND      */
*   /*   IS NOT SET, NO FURTHER ACTION IS TAKEN.                     */
*   /*                                                               */
*   /*   FOR NON-SNA SESSIONS, AND SNA SESSIONS WITH THE QUERY       */
*   /*   BIT SET IN THE BIND, THE FOLLOWING PROCESSING OCCURS.       */
*   /*                                                               */
*   /*   THE FIRST TPUT OF THE TSO SESSION IS ISSUED HERE.           */
*   /*   A FULLSCREEN TPUT IS USED TO CLEAR THE TERMINAL AID AND     */
*   /*   ENSURE THAT NO INPUT DATA IS PENDING FROM THE TERMINAL      */
*   /*   BUFFER.  FULLSCREEN MODE IS NOT TURNED ON TO AVOID THE      */
*   /*   REQUIREMENT OF AN ADDITIONAL INPUT FROM THE TERMINAL        */
*   /*   OPERATOR CAUSED BY VTAM PAGE PROTECTION ('***').  AS        */
*   /*   A RESULT, NOEDIT INPUT MODE IS ACTIVATED BY DIRECTLY        */
*   /*   SETTING THE FLAG, AND NOT BY AN STFSMODE MACRO.             */
*   /*                                                               */
*   /*   A NOEDIT TPUT IS THEN USED TO ISSUE THE QUERY TO THE        */
*   /*   TERMINAL.  A TGET MACRO IS USED TO GET THE RESPONSE.        */
*   /*   ABNORMAL TGET RETURN CODES TERMINATE THE QUERY PROCESS.     */
*   /*   IF THE RETURN CODE IS NORMAL NON-QUERY RESPONSES ARE        */
*   /*   DISCARDED AND THE TGET REISSUED.  ONLY THE IMPLICIT         */
*   /*   PARTITION SUB-FIELD OF THE QUERY RESPONSE IS EXAMINED,      */
*   /*   FROM WHICH THE PRIMARY AND ALTERNATE SCREEN SIZES ARE       */
*   /*   DISCOVERED.                                                 */
*   /*                                                               */
*   /*   UPON SUCCESSFULLY PROCESSING THE QUERY RESPONSE, THIS       */
*   /*   ROUTINE:                                                    */
*   /*   (1) REGISTERS THE PRIMARY AND ALTERNATE SCREEN SIZES        */
*   /*       IN THE TERMINAL STATUS BLOCK.                           */
*   /*   (2) SETS THE CURRENT SCREEN SIZE TO THE ALTERNATE SCREEN    */
*   /*       SIZE IF THE ALTERNATE SCREEN SIZE HAS AT LEAST 24       */
*   /*       LINES AND 80 COLUMNS.                                   */
*   /*   (3) SETS THE FLAG TO INSTRUCT THE TSO/VTAM 3270 SCREEN      */
*   /*       MANAGER TO RECHECK THE SCREEN DIMENSIONS IF THE         */
*   /*       SCREEN SIZE WAS SET.                                    */
*   /*   (4) SETS THE QUERY BIT ON.                                  */
*   /*   (5) ISSUES THE STLINENO LINE=1,MODE=OFF MACRO WHICH WILL    */
*   /*   (5.1) SET THE NEXT OUTPUT LINE FOR THE FIRST SCREEN LINE.   */
*   /*   (5.2) SET THE FLAG TO INSTRUCT THE TSO/VTAM 3270 SCREEN     */
*   /*         MANAGER TO FORMAT THE SCREEN FOR LINE MODE.           */
*   /*   (5.3) KEEP FULLSCREEN MODE OFF AND TURN OFF NOEDIT INPUT    */
*   /*         MODE.                                                 */
*   /*   (6) SETS THE KEYBOARD LOCKED FLAG SO A SUBSEQUENT UNLOCK    */
*   /*       REQUEST WILL NOT BE GENERATED.                          */
*   /*   (7) RESETS THE UNLOCK REQUESTED FLAG SO LOGON MESSAGES      */
*   /*       CAN BE OUTPUT WITHOUT BEING QUEUED BEHIND AN EXTRA      */
*   /*       TERMINAL INPUT REQUEST.                                 */
*   /*                                                               */
*   /*****************************************************************/
*
*   DETERMINE IF USING A 3270 DISPLAY
*
         TM    TSBSTAT,TSBINUSE+TSBDSPLY+TSB3270
         BNO   FASTEXIT            NOT EXPECTED TERMINAL TYPE
         TM    TSBSTAT,TSBDISC     TERMINAL DISCONNECTED?
         BO    FASTEXIT            YES, CANNOT TALK TO TERMINAL
         CLI   TSBLNNO,0           WOULD GTSIZE SAY ZERO LINES?
         BE    FASTEXIT            YES, TERMINAL IS NOT A SCREEN
*
*   DETERMINE IF SNA SESSION
*
         ICM   R0,15,TSBXBIND+8    SNA BIND PRESENT?
         BZ    QUERYTRM            NO, ALWAYS QUERY IF NON-SNA
*
*   SET SCREEN DIMENSIONS FROM SNA BIND
*
         MVC   TSBPRMR(4),TSBXBIND+19
*
*   DETERMINE IF SNA TERMINAL IS QUERYABLE
*
         TM    TSBXBIND+14,X'80'   QUERY BIT SET?
         BNO   FASTEXIT            NO, DO NOT QUERY TERMINAL
*
*   ACQUIRE WORKING STORAGE
*
QUERYTRM L     R0,@SIZDATD
         GETMAIN R,LV=(0)
         ST    R13,4(,R1)          CHAIN SAVE AREAS
         ST    R1,8(,R13)
         LR    R13,R1
         USING @DATD,R13
*
*   ISSUE QUERY
*
         MVC   TPGPL,TPG           INITIALISE TPG PLIST
         OI    TSBFLG5,TSBNEDIT    ENSURE INPUT NOT EDITED
         LA    R1,RESETAID         RESET THE TERMINAL AID
         LA    R0,L'RESETAID             BEFORE ISSUING THE
         ICM   R1,8,=X'0B'               READ PARTITION
         TPUT  (1),(0),R           TPUT FULLSCR,WAIT,HOLD
         TCLEARQ INPUT             FLUSH ANY TYPE-AHEAD TEXT
         TPG   MF=(E,TPGPL)        ISSUE QUERY TO TSO TERMINAL
         LTR   R15,R15             WAS TPG ISSUED SUCCESSFULLY?
         BNZ   QUERYX              NO, DO NOT EXPECT A RESPONSE
*
*   READ QUERY RESPONSE
*
QUERYGET LA    R1,REPLY            POINT TO TGET BUFFER FOR RESPONSE
         LA    R0,REPLYLEN                  FROM READ PARTITION
         ICM   R1,8,=X'81'         FLAGS FOR TGET ASIS,WAIT
         TGET  (1),(0),R           TGET ASIS,WAIT
         CH    R15,=H'8'           ATTENTION INTERRUPT?
         BE    QUERYX              YES, ASSUME QUERY NOT FUNCTIONAL
         CH    R15,=H'16'          TGET PARAMETER ERROR?
         BE    QUERYX              YES, ASSUME QUERY NOT FUNCTIONAL
         CH    R15,=H'20'          TERMINAL DISCONNECTED?
         BE    QUERYX              YES, ASSUME QUERY NOT FUNCTIONAL
         CLI   REPLY,X'88'         QUERY RESPONSE AID?
         BE    QUERYGOT            YES
         CLI   REPLY,X'F3'         PF3?
         BE    QUERYX              YES, EXIT
         CLI   REPLY,X'C3'         PF15?
         BE    QUERYX              YES, EXIT
*
*   DISPLAY INFORMATION ABOUT (NON-QUERY) INPUT DATA
*
         MVC   TPGPL,REPLY         PRESERVE INPUT DATA HEADER
         MVC   REPLY(RCMSGLEN),RCMSG
         STC   R15,@DATD           SHOW RETURN CODE
         UNPK  REPLY+RCMSGRC(3),@DATD(2)
         TR    REPLY+RCMSGRC(2),HEX-240
         MVI   REPLY+RCMSGRC+2,X'7D'
         STH   R1,@DATD            SHOW TGET INPUT DATA LENGTH
         UNPK  REPLY+RCMSGLN(5),@DATD(3)
         TR    REPLY+RCMSGLN(4),HEX-240
         MVI   REPLY+RCMSGLN+4,X'7D'
         LA    R15,REPLY+RCMSGLEN  POINT TO MESSAGE AREA
         LA    R14,TPGPL           POINT TO PRESERVED DATA
         LA    R0,TPGL             GET PRESERVED DATA LENGTH
         CR    R1,R0               SAVED DATA TRUNCATED?
         BNH   UNPKLOOP            NO
         LR    R1,R0               YES, REDUCE LENGTH
UNPKLOOP UNPK  0(3,R15),0(2,R14)   UNPACK DATA
         TR    0(2,R15),HEX-240    TRANSLATE DATA
         LA    R15,2(,R15)         ADJUST MESSAGE POINTER
         LA    R14,1(,R14)         ADJUST DATA POINTER
         BCT   R1,UNPKLOOP         SHUFFLE THROUGH DATA BYTES
         LA    R1,REPLY            POINT TO MESSAGE
         MVI   0(R15),X'7D'
         LA    R0,1(,R15)
         SR    R0,R1               GET MESSAGE LENGTH
         TPUT  (1),(0),R           DISPLAY DATA ABOUT INPUT DATA
         LA    R1,LPMSG            DISPLAY TRAPDOOR EXIT PLAN
         LA    R0,LPMSGLEN
         TPUT  (1),(0),R
         B     QUERYGET            SEE IF NEXT I/O IS THE GO
*
*   PARSE QUERY RESPONSE
*
QUERYGOT LA    R15,REPLY+1         POINT PAST AID
         SLR   R0,R0               CLEAR FOR INSERT
         BCT   R1,QUERYPRS         DECREMENT FOR AID
         B     QUERYX              ONLY AID WAS RETURNED
QUERYPRS CLI   2(R15),X'81'        QUERY REPLY ID?
         BNE   QUERYX              NO, GIVE UP
         CLI   3(R15),X'A6'        QUERY IMPLICIT PARTITION ID?
         BE    SETSIZES            YES
         ICM   R0,3,0(R15)         NO, GET SUB-FIELD LENGTH
         BZ    QUERYX              AVOID INFINITE LOOP
         ALR   R15,R0              POINT TO NEXT SUB-FIELD
         SR    R1,R0               ADJUST REMAINING LENGTH
         BP    QUERYPRS            CONTINUE PARSING OF REPLY
         B     QUERYX              REQUIRED SUB-FIELD NOT FOUND
*
*   ASCERTAIN PRIMARY AND ALTERNATE SCREEN SIZE DIMENSIONS
*
SETSIZES CLI   6(R15),11           EXPECTED PARAMETER LENGTH?
         BNE   QUERYX              NO, GIVE UP
         CLI   7(R15),1            IMPLICIT PARTITION SIZES?
         BNE   QUERYX              NO, GIVE UP
         ICM   R0,4,10(R15)        GET WIDTH OF PRIMARY SIZE
         ICM   R0,8,12(R15)        GET DEPTH OF PRIMARY SIZE
         ICM   R0,1,14(R15)        GET WIDTH OF ALTERNATE SIZE
         ICM   R0,2,16(R15)        GET DEPTH OF ALTERNATE SIZE
         ST    R0,TSBPRMR          SET INTO TSB
         CLI   TSBALTR,24          AT LEAST 24 ROWS?
         BL    QUERYOK             NO, MAKE NO MORE CHANGES
         CLI   TSBALTC,80          AT LEAST 80 COLUMNS?
         BL    QUERYOK             NO, MAKE NO MORE CHANGES
         STCM  R0,1,TSBLNSZ        SET NEW SCREEN COLUMN COUNT
         STCM  R0,2,TSBLNNO        SET NEW SCREEN LINE COUNT
         OI    TVWAFLG1,TVWAXSCD   EXAMINE SCREEN DIMENSIONS
QUERYOK  OI    TSBFLG5,TSBQRY      SET THE QUERY BIT ON
*
*   CLEAR THE TERMINAL READY FOR LINE MODE USAGE
*
QUERYX   STLINENO LINE=1,MODE=OFF  CLEAR AND FORMAT THE SCREEN
*                                        AND RESET NOEDIT MODE
         OI    TVWAFLG4,TVWAKBDL   REMEMBER KEYBOARD IS LOCKED
         NI    TVWAFLG1,255-TVWAULK  AND NO UNLOCK REQUEST YET
*
*   RELEASE WORKING STORAGE AND EXIT
*
         LR    R1,R13              POINT TO WORKING STORAGE
         L     R0,@SIZDATD         GET WORKING STORAGE SIZE
         L     R13,4(,R13)         POINT TO CALLER'S SAVE AREA
         FREEMAIN R,LV=(0),A=(1)   FREE WORKING STORAGE
FASTEXIT LM    R14,R12,12(R13)     RESTORE REGISTERS
         BR    R14                 RETURN TO CALLER
         EJECT
@DATA    DS    0H
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
TPG      TPG   QUERY,L'QUERY,NOEDIT,WAIT,HOLD,MF=L
TPGL     EQU   *-TPG
*              ESCAPE+ERASE/WRITE+WCC+SBA(LAST)+SF(INLO)+SBA(HOME)+IC
RESETAID DC    X'27F5C3115D7F1D4011404013'
QUERY    DC    X'F3000501FF02'    WRITE STRUCTURED FIELD + QUERY
LPMSG    DC    C'IF LOOPING USE PF3/15 TO END'
LPMSGLEN EQU   *-LPMSG
RCMSG    DC    C'IKTXLOG TGET RC=X'''
RCMSGRC  EQU   *-RCMSG
         DC    C'XX'',LEN=X'''
RCMSGLN  EQU   *-RCMSG
         DC    C'XXXX'',DATA=X'''
RCMSGLEN EQU   *-RCMSG
         LTORG
         DS    0D
HEX      DC    CL16'0123456789ABCDEF'
@PATCH   DC    10D'0'
@ENDDATA DS    0D                 END OF CSECT
@DATD    DSECT
         DS    18F
TPGPL    DS    XL(TPGL)
REPLY    DS    0F
         ORG   @DATD+4096
         DS    0D
REPLYLEN EQU   *-REPLY
@ENDDATD EQU   *
         SPACE
R0       EQU   0                   EQUATES FOR REGISTERS 0-15
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE
         PRINT NOGEN
         IHAPSA
         IHAASCB
         IKJTSB EXT=YES
         IKTTVWA
         SPACE
         END   IKTXLOG
/*
//*
//STEP09  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKTXLOG('ZP60009')
++MOD(IKT3270I) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP10  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '*** IKT3270I: INPUT DATA HANDLING ROUTINE ***          *
                        '
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*
IKT3270I CSECT
         USING IKT3270I,R15
         B     @PROLOG
         DC    AL1(16)
         DC    CL16'IKT3270I  84.045'
         DROP  R15                         IKT3270I
@PROLOG  STM   R14,R12,12(R13)             SAVE REGS
         BALR  R5,0                        ADDRESS SET
         USING *,R5
         USING PSA,0
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         L     R7,TSBEXTNT-TSB(,R12)
         L     R9,TSBXTVWA-TSBX(,R7)
         USING TVWA,R9
         L     R6,TVWATIMW                 TIM WORK AREA ADDRESS
         CLI   TSBPRMR-TSB(R12),0          FIELD POPULATED?     ZP60009
         BE    ROWSOKAY                    NO, PHYSLNNO OKAY    ZP60009
         MVC   2319(1,R6),TSBPRMR-TSB(R12) YES, USE REAL DEPTH  ZP60009
         TM    TVWAFLG6,X'01'              CHECK TVWAALTS       ZP60009
         BNO   ROWSOKAY                    USE PRIMARY DEPTH    ZP60009
         MVC   2319(1,R6),TSBALTR-TSB(R12) USE ALTERNATE DEPTH  ZP60009
ROWSOKAY EQU   *                           PHYSLNNO NOW SET     ZP60009
*  TIMLNNO = MAX(2,MIN(TSBLNNO,PHYSLNNO));                 /*@ZA13627*/
         SLR   R8,R8
         IC    R8,TSBLNNO-TSB(,R12)
         L     R7,2316(,R6)                PHYSLNNO
         CR    R8,R7
         BNH   A000042
         LR    R8,R7
A000042  LA    R7,2
         CR    R8,R7
         BNL   A00004E
         LR    R8,R7
A00004E  ST    R8,2312(,R6)                TIMLNNO
         TM    4(R12),X'08'
         BNO   A000096
         LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         CLC   48(4,R8),FW1500
         BH    A000076
         ALR   R12,R6
         TM    18(R12),X'C0'
         BZ    A00007E
A000076  OI    448(R6),X'02'
         B     A000096
A00007E  OI    448(R6),X'40'
         NI    448(R6),X'FD'
         NI    TVWAFLG4,255-TVWANOFB
         NI    TVWAFLG2,255-TVWABKMG
         MVI   TVWATQL1,0
         MVI   TVWATQL2,0
A000096  TM    TVWAFLG2,TVWAPGN
         BNZ   A0002D4
         LA    R12,2348
         ALR   R12,R6
         TM    16(R12),X'08'
         BNO   A0000B0
         BAL   R14,ASCITRAN                PERFORM
*        /************************************************************/
*        /*  THE FOLLOWING CODE CHECKS THE AVAILIBILITY OF A USER    */
*        /*  EXIT. IF THE ADDRESS OF IKTIDSX2 IS NOT ZERO, THEN THE  */
*        /*  USER IS GIVEN CONTROL WITH REG0 CONTAINING THE ADDRESS  */
*        /*  OF THE DATA TO BE SCANNED AND REG1 CONTAINING THE       */
*        /*  LENGTH OF THAT DATA.                                    */
*        /************************************************************/
A0000B0  L     R12,IKTIDSX2
         LTR   R12,R12
         BZ    A0000F2
         LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         L     R0,32(,R8)
         LA    R8,1500
         ALR   R12,R6
         C     R8,48(,R12)
         BNL   A0000DA
         LR    R1,R8
         B     A0000E4
A0000DA  LA    R12,2348
         ALR   R12,R6
         L     R1,48(,R12)
A0000E4  LR    R2,R13
         LA    R13,2216(,R6)
         L     R15,IKTIDSX2
         BALR  R14,R15                     STD LINKAGE
         LR    R13,R2
*/********************************************************************/
*/*    AT THIS POINT THE INPUT DATA HAS BEEN TRANSLATED IF NECESSARY */
*/*    NOW THE DATA WILL BE SCANNED FOR INPUT LINE DELIMITERS.       */
*/*            THE DATA WILL BE BROKEN INTO LINE SEGMENTS AND        */
*/*    PLACED ON THE INPUT QUEUE.                                    */
*/********************************************************************/
*
*        /************************************************************/
*        /* THIS CODE WILL HELP TOM IN THE PLACEMENT OF THE NEXT     */
*        /* ATTRIBUTE BYTE WHEN VTIOC IS DOING THE SCREEN MANAGEMENT */
*        /************************************************************/
A0000F2  TM    448(R6),X'02'
         BNZ   A000106
         TM    TVWAFLG5,TVWASCAN
         BNO   A000106
         OI    TVWAFLG4,TVWADARC
A000106  LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         L     R4,32(,R8)
         SLR   R8,R8
         ST    R8,2136(,R6)
         ST    R8,2144(,R6)
         LA    R8,1500
         ALR   R12,R6
         C     R8,48(,R12)
         BNL   A000132
         ST    R8,2120(,R6)
         B     A000140
A000132  LA    R12,2348
         ALR   R12,R6
         L     R12,48(,R12)
         ST    R12,2120(,R6)
A000140  TM    448(R6),X'02'
         BNZ   A00014C
         BAL   R14,HEADPROC                PERFORM
         EJECT
*      /**************************************************************/
*      /*  NOW THE DATA WILL BE SCANNED FOR THE LINE LENGTH          */
*      /*  WHEN THE LINE LENGTH IS FOUND, THE DATA WILL BE PUT ON    */
*      /*  THE INPUT QUEUE PRECEDED BY THE HEADER INFORMATION.       */
*      /*  SINCE DATA CAN ONLY BE SCANNED IN 256 BYTE LENGTHS,       */
*      /*  THIS VALUE WILL BE USED WHEN THE REMAINING DATA EXCEEDS   */
*      /*  THE 256 BYTE MAXIMUM.                                     */
*      /**************************************************************/
A00014C  NI    448(R6),X'7F'
         SLR   R1,R1
         ST    R4,2140(,R6)
A000156  LA    R12,256
         L     R8,2120(,R6)
         SL    R8,2136(,R6)
         CR    R8,R12
         BNH   A000170
         ST    R12,2148(,R6)
         B     A00017C
A000170  L     R12,2120(,R6)
         SL    R12,2136(,R6)
         ST    R12,2148(,R6)
A00017C  L     R12,2148(,R6)
         LTR   R12,R12
         BNP   NODELIM
         L     R12,PSAAOLD                 PSA REFERENCE        ZP60009
         L     R12,ASCBTSB-ASCB(,R12)                           ZP60009
         TM    TSBFLG5-TSB(R12),TSBNEDIT   IN NOEDIT MODE?      ZP60009
         BO    NODELIM                     YES, FIELD MARK OKAY ZP60009
         L     R3,2148(,R6)
         BCTR  R3,0
         EX    R3,A00098A
         BC    8,NODELIM
         OI    448(R6),X'80'
         LR    R12,R1
         SLR   R12,R4
         LR    R8,R12
         AL    R8,2144(,R6)
         ST    R8,2144(,R6)
         ST    R12,2148(,R6)
         B     DELIM
NODELIM  L     R12,2148(,R6)
         AL    R12,2144(,R6)
         ST    R12,2144(,R6)
DELIM    L     R12,2148(,R6)
         ALR   R4,R12
         AL    R12,2136(,R6)
         ST    R12,2136(,R6)
         TM    448(R6),X'80'
         BO    A0001DA
         CLC   2136(4,R6),2120(R6)
         BNE   A000156
*         /***********************************************************/
*         /* THIS MARKS THE END OF THE DO WHILE TO SCAN FOR DELIMITER*/
*         /* EITHER A DELIMITER WAS FOUND OR ALL THE DATA IN THE     */
*         /* BUFFER WAS PROCESSED.                                   */
*         /***********************************************************/
A0001DA  TM    448(R6),X'80'
         BNO   A00023C
         TM    437(R6),X'20'
         BNO   A0001EE
         OI    448(R6),X'01'
A0001EE  OI    437(R6),X'80'
         NI    437(R6),X'DF'
         TM    448(R6),X'01'
         BNZ   A000214
         LH    R12,450(,R6)
         LR    R8,R12
         AL    R8,2144(,R6)
         STH   R8,434(,R6)
         STC   R12,438(,R6)
         B     A000220
A000214  L     R12,2144(,R6)
         STH   R12,434(,R6)
         MVI   438(R6),X'00'
A000220  CLI   0(R4),X'1E'
         BNE   A000234
         L     R12,PSAAOLD                 PSA REFERENCE        ZP60009
         L     R12,ASCBTSB-ASCB(,R12)                           ZP60009
         TM    TSBFLG5-TSB(R12),TSBNEDIT   IN NOEDIT MODE?      ZP60009
         BO    A000234                     YES, FIELD MARK OKAY ZP60009
         LA    R12,1
         AL    R12,2136(,R6)
         ST    R12,2136(,R6)
A000234  BAL   R14,ADDEL                   PERFORM
         B     A0002B0
A00023C  TM    437(R6),X'20'
         BNO   A000248
         OI    448(R6),X'01'
A000248  TM    448(R6),X'01'
         BNZ   A000266
         LH    R12,450(,R6)
         LR    R8,R12
         AL    R8,2144(,R6)
         STH   R8,434(,R6)
         STC   R12,438(,R6)
         B     A000272
A000266  MVI   438(R6),X'00'
         L     R12,2144(,R6)
         STH   R12,434(,R6)
A000272  LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         L     R8,48(,R8)
         C     R8,2120(,R6)
         BH    A000290
         ALR   R12,R6
         TM    18(R12),X'C0'
         BZ    A0002A4
A000290  L     R12,2120(,R6)
         ST    R12,2136(,R6)
         OI    437(R6),X'20'
         NI    437(R6),X'7F'
         B     A0002AC
A0002A4  OI    437(R6),X'80'
         NI    437(R6),X'DF'
A0002AC  BAL   R14,ADDEL                   PERFORM
A0002B0  LA    R12,2348
         ALR   R12,R6
         L     R4,32(,R12)
         AL    R4,2136(,R6)
         CLC   2136(4,R6),2120(R6)
         BE    A0002D0
         TM    448(R6),X'10'
         BNO   A00014C
A0002D0  B     A00033C
A0002D4  OI    448(R6),X'10'
         LA    R12,2348
         ALR   R12,R6
         L     R12,48(,R12)
         LA    R8,1500
         CR    R12,R8
         BNH   A0002EE
         LR    R12,R8
A0002EE  ST    R12,2120(,R6)
         MVI   432(R6),X'01'
         MVI   438(R6),X'00'
         OI    437(R6),X'80'
         NI    437(R6),X'DF'
         MVC   434(2,R6),HW1
         LR    R2,R13
         LA    R12,428(,R6)
         ST    R12,404(,R6)
         LA    R1,404(,R6)
         LA    R13,2216(,R6)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASIQM-TCAST(,R12)
         BALR  R14,R15                     STD LINKAGE
         LR    R13,R2
         CLI   433(R6),X'01'
         BNE   A00033C
         L     R12,428(,R6)
         MVC   0(1,R12),TVWARSHW
*      /**************************************************************/
*      /*  AT THIS POINT THE INPUT DATA STREAM HAS BEEN SCANNED AND  */
*      /*  EITHER ALL THE AVAILABLE DATA HAS BEEN PUT ON THE INPUT Q */
*      /*  OR WE HAVE FALLEN THROUGH THE PROCESS LOOP DUE TO AN ERROR*/
*      /*  THE FIRST CHECK MADE IS TO DETERMINE IF ALL THE PROCESSING*/
*      /*  WAS SUCCESSFUL.                                           */
*      /**************************************************************/
A00033C  TM    448(R6),X'10'
         BNZ   A00038C
         LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         L     R8,48(,R8)
         C     R8,2120(,R6)
         BH    A000362
         ALR   R12,R6
         TM    18(R12),X'C0'
         BZ    A00036A
A000362  OI    448(R6),X'02'
         B     A0003CA
A00036A  NI    448(R6),X'FD'
         NI    TVWAFLG4,255-TVWANOFB
*               IF TVWALNCT > TIMLNNO THEN                 /*@ZA13627*/
         SLR   R12,R12
         IC    R12,TVWALNCT
         C     R12,2312(,R6)
         BNH   A000388
         MVI   TVWALNCT,1                  TVWALNCT=1
         OI    TVWAFLG4,TVWAFMSC
A000388  B     A0003CA
A00038C  LA    R12,2348
         LR    R8,R6
         ALR   R8,R12
         L     R8,48(,R8)
         C     R8,2120(,R6)
         BH    A0003AA
         ALR   R12,R6
         TM    18(R12),X'C0'
         BZ    A0003B2
A0003AA  OI    448(R6),X'02'
         B     A0003B6
A0003B2  NI    448(R6),X'FD'
A0003B6  NI    TVWAFLG4,255-TVWANOFB
         NI    TVWAFLG2,255-TVWABKMG
         MVI   TVWATQL1,0
         MVI   TVWATQL2,0
         OI    448(R6),X'40'
A0003CA  TM    448(R6),X'10'
         BNZ   A0003DA
         TM    448(R6),X'02'
         BO    A000414
A0003DA  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    41(R12),X'40'
         BNO   A000414
         LR    R2,R13
         LA    R0,8
         L     R1,A0009CC
         LA    R13,2048
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTABEND-CVT(,R12)
         L     R15,SCVTSTAT-SCVTSECT(,R12)
         BALR  R14,R15                     STD LINKAGE
         LR    R13,R2
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         NI    41(R12),X'BF'
A000414  LM    R14,R12,12(R13)             RESTORE REGS
         BR    R14                         EXIT
         EJECT
*         /***********************************************************/
*         /* THE FOLLOWING CODE WILL CONVERT THE DATA IN THE INPUT   */
*         /* BUFFER FROM ASCII TO EBCDIC. THE LENGTH OF THE DATA     */
*         /* TO BE CONVERTED CAN NOT EXCEED THE BUFFER SIZE, EVEN    */
*         /* THOUGH RPLBUFL MAY INDICATE THAT ADDITIONAL DATA IS     */
*         /* WAITING TO BE RECEIVED. THE DATA WILL BE CONVERTED IN   */
*         /* 256 BYTE INCREMENTS UNTIL REMAINING DATA IS LESS THAN   */
*         /* 256 BYTES.                                              */
*         /* COUNT WILL CONTAIN THE LENGTH TO  BE TRANSLATED         */
*         /* VALUE WILL BE USED TO SUBSTRING THE TRANSLATE INSTRUCT  */
*         /***********************************************************/
ASCITRAN LA    R15,2348
         ALR   R15,R6
         L     R15,48(,R15)
         C     R15,FW1500
         BH    A000434
         ST    R15,2120(,R6)
         B     A00043A
A000434  MVC   2120(4,R6),FW1500
A00043A  NI    448(R6),X'FB'
         LA    R15,2348
         ALR   R15,R6
         L     R4,32(,R15)
         B     A000486
A00044C  LA    R15,256
         L     R12,2120(,R6)
         CR    R12,R15
         BNH   A000468
         ST    R15,2148(,R6)
         SLR   R12,R15
         ST    R12,2120(,R6)
         B     A000474
A000468  L     R15,2120(,R6)
         ST    R15,2148(,R6)
         OI    448(R6),X'04'
A000474  L     R15,2148(,R6)
         BCTR  R15,0
         L     R12,TVWAATBI
         EX    R15,A000990
         AL    R4,2148(,R6)
A000486  TM    448(R6),X'04'
         BZ    A00044C
         BR    R14                         EXIT ASCITRAN
         EJECT
*         /***********************************************************/
*         /* THE SAME PROCESS THAT WAS FOLLOWED FOR ASCII TRANSLATION*/
*         /* WILL BE FOLLOWED FOR USER TABLE TRANSLATION             */
*         /***********************************************************/
USERTRAN L     R15,2320(,R6)
         L     R3,2140(,R6)
         ALR   R3,R15
         LCR   R15,R15
         AL    R15,2144(,R6)
         ST    R15,2124(,R6)
         NI    448(R6),X'FB'
         LTR   R15,R15
         BP    A0004B2
         OI    448(R6),X'04'
A0004B2  B     A000556
A0004B6  SLR   R1,R1
         SLR   R2,R2
         LA    R15,256
         L     R12,2124(,R6)
         CR    R12,R15
         BNH   A0004D6
         ST    R15,2148(,R6)
         SLR   R12,R15
         ST    R12,2124(,R6)
         B     A0004E4
A0004D6  L     R15,2124(,R6)
         ST    R15,2148(,R6)
         SLR   R15,R15
         ST    R15,2124(,R6)
A0004E4  L     R15,2148(,R6)
         BCTR  R15,0
         EX    R15,A000996
         LTR   R2,R2
         BZ    A00050C
         SLR   R1,R3
         LR    R15,R1
         ALR   R15,R2
         LCR   R15,R15
         AL    R15,2148(,R6)
         AL    R15,2124(,R6)
         ST    R15,2124(,R6)
         ST    R1,2148(,R6)
A00050C  SLR   R15,R15
         L     R12,2124(,R6)
         CR    R12,R15
         BH    A000526
         LPR   R12,R12
         ST    R12,2320(,R6)
         ST    R15,2124(,R6)
         OI    448(R6),X'04'
A000526  L     R15,2148(,R6)
         LTR   R15,R15
         BZ    A00053E
         L     R15,2148(,R6)
         BCTR  R15,0
         L     R12,TVWATABI
         EX    R15,A00099C
A00053E  LTR   R2,R2
         BNZ   A00054C
         AL    R3,2148(,R6)
         B     A000556
A00054C  LR    R15,R3
         AL    R15,2148(,R6)
         ALR   R15,R2
         LR    R3,R15
A000556  TM    448(R6),X'04'
         BZ    A0004B6
         BR    R14                         EXIT USERTRAN
         EJECT
*        /************************************************************/
*        /*  CHECK THAT THIS WAS THE PRIMARY RECEIVE TO GET THE DATA */
*        /*  IF REC2FLG=1 THEN A SUBSEQUENT RECEIVE WAS NEEDED       */
*        /*  TO RECEIVE ALL THE DATA FROM THE TERMINAL.              */
*        /************************************************************/
HEADPROC SLR   R15,R15
         STH   R15,450(,R6)
         L     R15,2120(,R6)
         C     R15,FW3
         BNL   A00058C
         LR    R12,R15
         BCTR  R12,0
         EX    R12,A0009A2
         STH   R15,450(,R6)
         ALR   R4,R15
         AL    R15,2136(,R6)
         ST    R15,2136(,R6)
         B     A0005AA
A00058C  MVC   452(3,R6),0(R4)
         LA    R15,3
         LH    R12,450(,R6)
         ALR   R12,R15
         STH   R12,450(,R6)
         ALR   R4,R15
         AL    R15,2136(,R6)
         ST    R15,2136(,R6)
A0005AA  L     R15,2136(,R6)
         C     R15,2120(,R6)
         BNL   A0005DA
         CLI   0(R4),X'11'
         BNE   A0005DA
         MVC   455(3,R6),0(R4)
         LA    R12,3
         LH    R8,450(,R6)
         ALR   R8,R12
         STH   R8,450(,R6)
         ALR   R4,R12
         ALR   R15,R12
         ST    R15,2136(,R6)
A0005DA  BR    R14                         EXIT HEADPROC
         EJECT
*/********************************************************************/
*/*    THE ADDEL ROUTINE IS CALLED TO PLACE INCOMING DATA FROM THE   */
*/*    TERMINAL ON THE INPUT QUEUE, ONE LINE AT A TIME.              */
*/*    NOADDEL IS CHECKED TO SEE IF THE DATA SHOULD BE ADDED TO THE  */
*/*    QUEUE, LINE PROMPTING IS HANDLED, AND THE RETURN CODE FROM    */
*/*    THE QUEUE MANAGER IS CHECKED TO ASSURE A SUCCESSFUL ADD.      */
*/********************************************************************/
ADDEL    ST    R14,2288(,R6)
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    TSBFLG2-TSB(R12),TSBAUTON
         BNO   A000640
         LH    R8,434(,R6)
         CH    R8,450(,R6)
         BH    A00061C
         TM    437(R6),X'20'
         BNZ   A00061C
         TM    448(R6),X'01'
         BNZ   A00061C
         NI    437(R6),X'BF'
         NI    16(R12),X'BF'
         NI    TVWAFLG1,255-TVWAULK
         B     A000640
A00061C  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    16(R12),X'10'
         BNO   A00063C
         TM    437(R6),X'20'
         BNZ   A00063C
         OI    437(R6),X'40'
         B     A000640
A00063C  NI    437(R6),X'BF'
A000640  MVI   432(R6),X'01'
         LR    R2,R13
         LA    R12,428(,R6)
         ST    R12,404(,R6)
         LA    R1,404(,R6)
         LA    R13,2216(,R6)
         L     R12,CVTPTR                  CVT ADDRESS
         L     R12,CVTTCASP-CVT(,R12)
         L     R15,TCASIQM-TCAST(,R12)
         BALR  R14,R15                     STD LINKAGE
         LR    R13,R2
         NI    448(R6),X'FE'
         CLI   433(R6),X'01'
         BNE   A00071C
         CLI   438(R6),X'00'
         BNH   A00068A
         L     R12,428(,R6)
         SLR   R8,R8
         IC    R8,438(,R6)
         BCTR  R8,0
         EX    R8,A0009A8
A00068A  L     R12,2144(,R6)
         LTR   R12,R12
         BNP   A0006B6
         TM    TVWAFLG3,TVWATRAN
         BNO   A0006A0
         BAL   R14,USERTRAN                PERFORM
A0006A0  SLR   R0,R0
         IC    R0,438(,R6)
         AL    R0,428(,R6)
         L     R1,2144(,R6)
         L     R2,2140(,R6)
         LR    R3,R1
         MVCL  R0,R2
A0006B6  NI    448(R6),X'EF'
         L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         TM    16(R12),X'40'
         BNO   A0006FA
         TM    16(R12),X'10'
         BNO   A0006EE
         TM    437(R6),X'20'
         BNZ   A0006EA
         L     R8,48(,R12)
         AL    R8,52(,R12)
         ST    R8,48(,R12)
         OI    16(R12),X'04'
A0006EA  B     A0006FA
A0006EE  L     R12,PSAAOLD                 PSA REFERENCE
         L     R12,ASCBTSB-ASCB(,R12)
         OI    16(R12),X'04'
A0006FA  TM    TVWAFLG5,TVWAFSM
         BNO   A000714
         TM    TVWAFLG5,TVWAWO
         BNZ   A000714
         SLR   R12,R12
         ST    R12,2144(,R6)
         B     A000724
A000714  BAL   R14,LINECNT                 PERFORM
         B     A000724
A00071C  OI    448(R6),X'10'
         OI    TVWAFLG2,TVWAERMG
A000724  L     R14,2288(,R6)
         BR    R14                         EXIT ADDEL
         EJECT
* /*******************************************************************/
* /*  LINECNT IS THE ROUTINE THAT WILL RECEIVE CONTROL WHEN FULL     */
* /*  SCREEN MODE IS NOT IN EFFECT. THE INPUT LINES WILL BE INCRE-   */
* /*  MENTED AS DATA IS RECEIVED. IF THE BYPASS FLAG IS NOT ON, THE  */
* /*  LINE COUNT IS CHECKED AND WHEN WE HAVE REACHED THE LAST TWO    */
* /*  LINES OF DATA ON THE SCREEN, THE DATA WILL BE SAVED AND FLASHED*/
* /*  BACK TO THE TERMINAL USER, EXCEPT IN FULLSCREEN  PROCESSING    */
* /*******************************************************************/
LINECNT  L     R15,PSAAOLD                 PSA REFERENCE
         L     R15,ASCBTSB-ASCB(,R15)
         L     R15,TSBEXTNT-TSB(,R15)
         CLC   74(2,R15),HW480
         BNE   A00074A
         MVC   2128(4,R6),FW40
         B     A000750
A00074A  MVC   2128(4,R6),FW80
         CLI   TSBPRMC-TSB(R15),0          FIELD POPULATED?     ZP60009
         BE    A000750                     NO                   ZP60009
         MVC   2131(1,R6),TSBPRMC-TSB(R15) YES, USE REAL WIDTH  ZP60009
         TM    TVWAFLG6,X'01'              CHECK TVWAALTS       ZP60009
         BNO   A000750                     USE PRIMARY WIDTH    ZP60009
         MVC   2131(1,R6),TSBALTC-TSB(R15) USE ALTERNATE WIDTH  ZP60009
*  /******************************************************************/
*  /*  THE FOLLOWING CODE WILL PROCESS THE DATA IF THE LINE NUMBER   */
*  /*  IS LESS THAN THE NEXT TO LAST LINE ON THE SCREEN OR IF THE    */
*  /*  BYPASS FLAG IS ON.                                            */
*  /******************************************************************/
*
*    IF TVWALNCT < (TIMLNNO-1)   TVWANOFB='1'B   DATALINE=0
*       TVWAFLSC='1'B THEN                                 /*@ZA13627*/
A000750  L     R15,2312(,R6)
         BCTR  R15,0
         SLR   R12,R12
         IC    R12,TVWALNCT
         CR    R15,R12
         BH    A00077C
         TM    TVWAFLG4,TVWANOFB
         BO    A00077C
         L     R15,2144(,R6)
         LTR   R15,R15
         BZ    A00077C
         TM    TVWAFLG4,TVWAFLSC
         BNO   A000812
A00077C  L     R15,PSAAOLD                 PSA REFERENCE
         L     R15,ASCBTSB-ASCB(,R15)
         TM    16(R15),X'04'
         BNZ   A000792
         SLR   R15,R15
         STH   R15,TVWANCNT
A000792  L     R15,2144(,R6)
         L     R12,2132(,R6)
         LR    R8,R15
         ALR   R8,R12
         LH    R7,TVWANCNT
         ALR   R8,R7
         L     R11,2128(,R6)
         CR    R8,R11
         BNH   A0007DE
         ALR   R15,R7
         SLR   R11,R12
         SLR   R15,R11
         ST    R15,2144(,R6)
         AL    R11,2140(,R6)
         ST    R11,2140(,R6)
         LA    R15,1
         SLR   R12,R12
         IC    R12,TVWALNCT
         ALR   R15,R12
         STC   R15,TVWALNCT
         SLR   R15,R15
         ST    R15,2132(,R6)
         STH   R15,TVWANCNT
         B     A000812
A0007DE  TM    437(R6),X'20'
         BNZ   A000800
         SLR   R15,R15
         ST    R15,2132(,R6)
         LA    R15,1
         SLR   R12,R12
         IC    R12,TVWALNCT
         ALR   R15,R12
         STC   R15,TVWALNCT
         B     A00080C
A000800  L     R15,2132(,R6)
         AL    R15,2144(,R6)
         ST    R15,2132(,R6)
A00080C  SLR   R15,R15
         ST    R15,2144(,R6)
*        /************************************************************/
*        /*  AT THIS POINT A CHECK IS MADE TO SEE IF WE ARE PROCESS- */
*        /*  ING THE NEXT TO LAST LINE OF THE SCREEN AND FLASHBACK   */
*        /*  IS PERMITTED. THE INPUT DATA WILL BE SAVED IN THE FIRST */
*        /*  FLASHBACK BUFFER TO BE RETURNED TO THE TERMINAL USER.   */
*        /************************************************************/
*
*        IF TVWALNCT=TIMLNNO-1 & TVWANOFB='0'B & DATALINE>0 THEN
A000812  L     R15,2312(,R6)
         BCTR  R15,0
         SLR   R12,R12
         IC    R12,TVWALNCT
         CR    R15,R12
         BNE   A000896
         TM    TVWAFLG4,TVWANOFB
         BNZ   A000896
         L     R15,2144(,R6)
         LTR   R15,R15
         BNP   A000896
         L     R12,2128(,R6)
         CR    R15,R12
         BNH   A000848
         ST    R12,2148(,R6)
         B     A000850
A000848  L     R15,2144(,R6)
         ST    R15,2148(,R6)
A000850  L     R15,2148(,R6)
         LR    R12,R15
         BCTR  R12,0
         L     R8,2140(,R6)
         EX    R12,A0009AE
         ALR   R8,R15
         ST    R8,2140(,R6)
         L     R12,2144(,R6)
         SLR   R12,R15
         ST    R12,2144(,R6)
         STC   R15,TVWATQL1
         TM    437(R6),X'20'
         BZ    A000882
         LTR   R12,R12
         BNP   A000896
A000882  LA    R15,1
         SLR   R12,R12
         IC    R12,TVWALNCT
         ALR   R15,R12
         STC   R15,TVWALNCT
         OI    TVWAFLG2,TVWABKMG
*        /************************************************************/
*        /*  THE NEXT BLOCK OF CODE WILL BUILD THE DATA FLASHBACK    */
*        /*  WHEN THE LINECOUNT EQUALS OR EXCEEDS THE LAST SCREEN    */
*        /*  LINE AND THE BYPASS FLAG IS NOT SET ON.                 */
*        /************************************************************/
A000896  SLR   R15,R15
         IC    R15,TVWALNCT
         C     R15,2312(,R6)
         BL    A00096E
         TM    TVWAFLG4,TVWANOFB
         BNZ   A00096E
         L     R15,2144(,R6)
         LTR   R15,R15
         BNP   A00096E
*            /********************************************************/
*            /* ALL FOLLOWING CONDITIONS INDICATE MORE THAN 1 LINE   */
*            /* WILL BE FLASHED BACK TO THE USER.                    */
*            /********************************************************/
         C     R15,2128(,R6)
         BH    A0008D2
         L     R15,2136(,R6)
         C     R15,2120(,R6)
         BL    A0008D2
         TM    437(R6),X'20'
         BNO   A000940
A0008D2  L     R15,2128(,R6)
         C     R15,2144(,R6)
         BNL   A0008E6
         ST    R15,2148(,R6)
         B     A0008EE
A0008E6  L     R15,2144(,R6)
         ST    R15,2148(,R6)
A0008EE  L     R15,2148(,R6)
         LR    R12,R15
         BCTR  R12,0
         L     R8,2140(,R6)
         EX    R12,A0009AE
         L     R12,2144(,R6)
         SLR   R12,R15
         ST    R12,2144(,R6)
         STC   R15,TVWATQL1
         ALR   R8,R15
         ST    R8,2140(,R6)
         TM    437(R6),X'20'
         BZ    A000920
         LTR   R12,R12
         BNP   A000938
A000920  LA    R15,1
         SLR   R12,R12
         IC    R12,TVWALNCT
         ALR   R15,R12
         STC   R15,TVWALNCT
         OI    TVWAFLG2,TVWABKMG
         B     A00096E
A000938  NI    TVWAFLG2,255-TVWABKMG
         B     A00096E
A000940  L     R15,2144(,R6)
         LR    R12,R15
         BCTR  R12,0
         L     R8,2140(,R6)
         EX    R12,A0009B4
         STC   R15,TVWATQL2
         OI    TVWAFLG2,TVWABKMG
         SLR   R15,R15
         ST    R15,2144(,R6)
         LA    R15,1
         SLR   R12,R12
         IC    R12,TVWALNCT
         ALR   R15,R12
         STC   R15,TVWALNCT
A00096E  L     R15,2144(,R6)
         LTR   R15,R15
         BP    A000750
         TM    TVWAFLG2,TVWABKMG
         BNO   A000984
         OI    448(R6),X'40'
A000984  BR    R14                         EXIT LINECNT
HW1      DC    H'1'
HW480    DC    H'480'
A00098A  TRT   0(0,R4),NEWLINE
A000990  TR    0(0,R4),0(R12)
A000996  TRT   0(0,R3),SBATAB
A00099C  TR    0(0,R3),0(R12)
A0009A2  MVC   452(0,R6),0(R4)
A0009A8  MVC   0(0,R12),452(R6)
A0009AE  MVC   458(0,R6),0(R8)
A0009B4  MVC   538(0,R6),0(R8)
FW3      DC    F'3'
FW40     DC    F'40'
FW80     DC    F'80'
FW1500   DC    F'1500'
A0009CC  EQU   *
         DC    X'80000000'
         DC    X'07004190'
         DC    F'0'
IKTIDSX2 DC    V(IKTIDSX2)
         DC    X'00509509'
         DS    0D
NEWLINE  DC    30X'00'
         DC    X'1E'
         DC    225X'00'
SBATAB   DC    17AL1(0)
         DC    AL1(3)
         DC    12AL1(0)
         DC    AL1(1)
         DC    225AL1(0)
PATCH    DC    20F'0'
         EJECT
R0       EQU   0                           EQUATES FOR REGISTERS 0-15
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE
         PRINT NOGEN
         IHAPSA
         CVT   DSECT=YES
         IHASCVT
         IHAASCB
         IKJTSB EXT=YES
         IKTTVWA
         IKTTCAST
         END   IKT3270I,(C'PLS1824',0702,84045)
/*
//*
//STEP11  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT3270I('ZP60009')
++MOD(IKT09412) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP12  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '********  IKT09412:  STFSMODE - TERMINAL CONTROL MACRO *
                 *******'
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*            TO HANDLE NOEDIT INPUT MODE AND RESHOW CODES UP TO 24
*
IKT09412 CSECT ,                                                   0001
@MAINENT BALR  R15,0                                               0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'IKT09412  82.015'                                 0001
         DROP  R15
@PROLOG  BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         USING TSB,R5
*        /************************************************************/
*        /* SAVE REGISTER 1.  CLEAR RESHOW KEY VALUE IN      @G58AK3B*/
*        /* RIGHT-MOST BYTE OF REGISTER 1.                   @G58AK3B*/
*        /************************************************************/
*        R6 = R1;                      /*SAVE REGISTER 1     @G58AK3B*/
         LR    R6,R1                                               0024
*        SRL(R1,SHIFT8);               /*SHIFT OUT RSHWKEY   @G58AK3B*/
         SLR   R7,R7                                               0025
         IC    R7,SHIFT8                                           0025
         SRL   R1,0(R7)                                            0025
*        SLL(R1,SHIFT8);               /*REALIGN REGISTER 1  @G58AK3B*/
*                                                                  0026
         SLL   R1,0(R7)                                            0026
*        /************************************************************/
*        /*                                                          */
*        /* SHIFT OUT PARAMETER BIT. IF REMAINING BITS ARE NOT ZERO  */
*        /* RETURN CODE 4 IS ISSUED.                                 */
*        /*                                                          */
*        /************************************************************/
*        SLDL(R0,SHIFT3);              /*SHIFT OUT PARM BITS  OZ11997*/
         SLR   R7,R7                                               0027
         IC    R7,SHIFT3                                           0027
         SLDL  R0,0(R7)                                            0027
*        IF R1 ^= 0 THEN               /*INVALID - RETURN CODE 4     */
         LTR   R1,R1                                               0028
         BZ    @RF00028                                            0028
*          R15 = CODE04;                                           0029
         LA    R15,4                                               0029
*        ELSE                                                      0030
*          /**********************************************************/
*          /*                                                        */
*          /* IF THE TERMINAL IS NOT A DISPLAY TERMINAL, THE INVALID */
*          /* TERMINAL IS NOTED WITH RETURN CODE 8                   */
*          /*                                                        */
*          /**********************************************************/
*          IF TSBDSPLY = '0'B THEN                                 0030
         B     @RC00028                                            0030
@RF00028 TM    TSBSTAT,TSBDSPLY                                    0030
         BNZ   @RF00030                                            0030
*            R15 = CODE08;                                         0031
         LA    R15,8                                               0031
*          ELSE                                                    0032
*            /********************************************************/
*            /*                                                      */
*            /* THE PARAMETER BIT IS RESTORED. FULLSCREEN MODE WILL  */
*            /* BE SET OFF OR ON BASED ON THE PARAMETER BIT.         */
*            /*                                                      */
*            /********************************************************/
*            DO;                                                   0032
         B     @RC00030                                            0032
@RF00030 DS    0H                                                  0033
*              SRDL(R0,SHIFT3);        /*RESTORE PARM BITS    OZ11997*/
         SLR   R7,R7                                               0033
         IC    R7,SHIFT3                                           0033
         SRDL  R0,0(R7)                                            0033
*              R15 = CODE00;           /*SET GOOD RETURN CODE        */
         SLR   R15,R15                                             0034
*              IF R1 >=0 THEN          /*LOOK FOR MODE OFF    OZ11997*/
         LTR   R1,R1                                               0035
         BM    @RF00035                                            0035
*                /****************************************************/
*                /*                                                  */
*                /* TURN OFF FULL SCREEN MODE INDICATORS             */
*                /*                                                  */
*                /****************************************************/
*                DO;                                               0036
         L     R7,TSBEXTNT                                         0037
         L     R7,TSBXTVWA-TSBX(,R7)                               0037
         USING TVWA,R7
         TM    TVWAFLG5,TVWAFSM            ALREADY IN LINE MODE?
         BNO   LINECTOK                    YES
         TM    TVWAFLG5,TVWAWO+TVWAFSW     LINE MODE MSG ON SHOW?
         BNZ   LINECTOK                    YES
*                  TVWALNCT = 1;       /*SET LINE COUNTER    @YM03603*/
         MVI   TVWALNCT,1                                          0037
*                  TVWAFSM = '0'B;     /*FULL SCREEN MODE BIT        */
*                  TVWAWO  = '0'B;     /*FULL SCREEN WRITTEN OVER    */
*                  TVWAFSW = '0'B;     /*FULL SCREEN WAITING         */
LINECTOK NI    TVWAFLG5,255-TVWAFSM-TVWAWO-TVWAFSW                 0040
         OI    TVWAFLG4,TVWAFMSC           FORMAT THE SCREEN
         NI    TSBFLG5,255-TSBNEDIT        RESET NOEDIT INPUT ZP60009
*                  /**************************************************/
*                  /* WHENEVER STFSMODE IS OFF, THE RESHOW   @G58AK3B*/
*                  /* KEY DEFAULTS TO THE PA2 KEY.           @G58AK3B*/
*                  /**************************************************/
*                  TVWARSHW = PA2;     /*RESET RESHOW KEY    @G58AK3B*/
         MVI   TVWARSHW,X'6E'                                      0041
*                END;                                              0042
*              ELSE                                                0043
*                /****************************************************/
*                /*                                                  */
*                /* TURN ON FULL SCREEN MODE INDICATOR               */
*                /*                                                  */
*                /****************************************************/
*                DO;                                               0043
         B     @RC00035                                            0043
@RF00035 DS    0H                                                  0044
*                  TVWAFSM = '1'B;     /*FULL SCREEN MODE BIT        */
         L     R7,TSBEXTNT                                         0044
         L     R7,TSBXTVWA-TSBX(,R7)                               0044
         OI    TVWAFLG5,TVWAFSM                                    0044
*                  SLL(R1,SHIFT1);     /*SHIFT OUT MODE BIT   OZ11997*/
         SLR   R4,R4                                               0045
         IC    R4,SHIFT1                                           0045
         SLL   R1,0(R4)                                            0045
*                  IF R1 < 0 THEN      /*LOOK FOR INITIAL CALLOZ11997*/
         LTR   R1,R1                                               0046
         BNM   @RF00046                                    ZP60009 0046
*                    TVWANFSP = '1'B;  /*SET NO FULL SCR PAGE OZ11997*/
         OI    TVWAFLG5,TVWANFSP                                   0047
*                  ELSE                                            0048
*                    TVWANFSP = '0'B;  /*ALLOW FULL SCR PAGE  OZ11997*/
         B     NOEDCHEK                                    ZP60009 0048
@RF00046 L     R7,TSBEXTNT                                         0048
         L     R7,TSBXTVWA-TSBX(,R7)                               0048
         NI    TVWAFLG5,255-TVWANFSP                               0048
*                  SLL(R1,SHIFT1);     /*SHIFT OUT INITIAL BIT       */
NOEDCHEK SLR   R4,R4                                       ZP60009
         IC    R4,SHIFT1                                   ZP60009
         SLL   R1,0(R4)                                    ZP60009
*                  IF R1 ^= 0 THEN     /*LOOK FOR NOEDIT CALL ZP60009*/
         LTR   R1,R1                                       ZP60009
         BZ    @RC00046                                    ZP60009
*                    TSBNEDIT = '1'B;  /*SET NOEDIT INPUT MODEZP60009*/
         OI    TSBFLG5,TSBNEDIT                            ZP60009
*                  /**************************************************/
*                  /* THE RSHWKEY VALUE IS FOUND IN THE      @G58AK3B*/
*                  /* RIGHT-MOST BYTE OF THE REGISTER.       @G58AK3B*/
*                  /**************************************************/
*                  SLL(R6,SHIFT24); /*ISOLATE RESHOW         @G58AK3B*/
@RC00046 SLR   R7,R7                                               0049
         IC    R7,SHIFT24                                          0049
         SLL   R6,0(R7)                                            0049
*                  SRL(R6,SHIFT24);    /*IN RIGHT-MOST BYTE  @G58AK3B*/
         SRL   R6,0(R7)                                            0050
*                  /**************************************************/
*                  /* WHEN STFSMODE IS ON AND A RSHWKEY HAS  @G58AK3B*/
*                  /* BEEN SPECIFIED, THE SPECIFIED RSHWKEY  @G58AK3B*/
*                  /* BECOMES THE RESHOW KEY.                @G58AK3B*/
*                  /* WHEN STFSMODE IS ON BUT RSHWKEY HAS    @G58AK3B*/
*                  /* NOT BEEN SPECIFIED, THE RESHOW KEY IS  @G58AK3B*/
*                  /* NOT CHANGED.                           @G58AK3B*/
*                  /**************************************************/
*                  IF RSHWKEY ^= ZERO  /*RSHWKEY SPECIFIED?  @ZM20757*/
*                  THEN                /*RSHWKEY SPECIFIED   @G58AK3B*/
         LTR   R6,R6                                               0051
         BZ    @RF00051                                            0051
         LA    R7,25                                       ZP60009
         CR    R6,R7
         BNL   @RF00051
*                    TVWARSHW = KEYS(RSHWKEY); /* SET RSHWKEY@G58AK3B*/
         L     R7,TSBEXTNT                                         0052
         L     R7,TSBXTVWA-TSBX(,R7)                               0052
         LA    R4,KEYS-1(R6)                                       0052
         MVC   TVWARSHW,0(R4)                                      0052
*                  ELSE;               /*RSHWKEY NOT SPECIFD @G58AK3B*/
@RF00051 DS    0H                                                  0054
*                END;                                              0054
*            END;                      /*END OF MODE PROCESSING      */
*        END IKT09412                                              0056
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJTSB  )                                        *
*/*%INCLUDE SYSLIB  (IKTTSBX )                                        *
*/*%INCLUDE SYSLIB  (IKTTVWA )                                        *
*;                                                                 0056
@EL00001 DS    0H                                                  0056
@EF00001 DS    0H                                                  0056
@ER00001 BR    R14                                                 0056
@DATA    DS    0H
@DATD    DSECT
         DS    0F
IKT09412 CSECT
         DS    0F
@DATD    DSECT
         DS    0D
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IKT09412 CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
SHIFT1   DC    AL1(1)
SHIFT3   DC    AL1(3)
SHIFT8   DC    AL1(8)
SHIFT24  DC    AL1(24)
PATCH    DS    CL50
KEYS     DC    CL1'1'
         DC    CL1'2'
         DC    CL1'3'
         DC    CL1'4'
         DC    CL1'5'
         DC    CL1'6'
         DC    CL1'7'
         DC    CL1'8'
         DC    CL1'9'
         DC    CL1':'
         DC    CL1'#'
         DC    CL1'@'
         DC    CL1'A'                                           ZP60009
         DC    CL1'B'                                           ZP60009
         DC    CL1'C'                                           ZP60009
         DC    CL1'D'                                           ZP60009
         DC    CL1'E'                                           ZP60009
         DC    CL1'F'                                           ZP60009
         DC    CL1'G'                                           ZP60009
         DC    CL1'H'                                           ZP60009
         DC    CL1'I'                                           ZP60009
         DC    XL1'4A'                 CENT SIGN                ZP60009
         DC    CL1'.'                                           ZP60009
         DC    CL1'<'                                           ZP60009
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
@RC00030 EQU   @EL00001
@RC00028 EQU   @EL00001
@RC00035 EQU   @EL00001
@ENDDATA EQU   *
         PRINT NOGEN                                            ZP60009
         IKJTSB EXT=YES                                         ZP60009
         IKTTVWA                                                ZP60009
         PRINT GEN                                              ZP60009
         END   IKT09412,(C'PLS2204',0702,82015)
/*
//*
//STEP13  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT09412('ZP60009')
++MOD(IKT09413) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP14  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '******** IKT09413:  STLINENO - TERMINAL CONTROL MACRO  *
                ********'
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*
IKT09413 CSECT ,                                                   0001
@MAINENT BALR  R15,0                                               0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'IKT09413  83.318'                                 0001
         DROP  R15
@PROLOG  BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         USING TSB,R5
*        /************************************************************/
*        /*                                                          */
*        /* SHIFT OUT PARAMETERS FROM INPUT REG.  IF ANY REMAINING   */
*        /* BITS ARE NOT ZERO, ERROR RETURN CODE 4 IS ISSUED.        */
*        /*                                                          */
*        /************************************************************/
*        R2 = R1;                      /*MOVE PARM REG TO WORK REG   */
         LR    R2,R1                                               0025
*        SLL(R2,SHIFT1);               /*SHIFT OUT MODE BIT          */
         SLR   R10,R10                                             0026
         IC    R10,SHIFT1                                          0026
         SLL   R2,0(R10)                                           0026
*        SRL(R2,SHIFT9);               /*SHIFT OUT LINE NUMBER       */
         SLR   R10,R10                                             0027
         IC    R10,SHIFT9                                          0027
         SRL   R2,0(R10)                                           0027
*        IF R2 ^= 0 THEN                                           0028
         LTR   R2,R2                                               0028
         BZ    @RF00028                                            0028
*          R15 = CODE04;                                           0029
         LA    R15,4                                               0029
*        ELSE                                                      0030
*          /**********************************************************/
*          /*                                                        */
*          /* IF THE TERMINAL IN USE IS NOT A DISPLAY TERMINAL, THE  */
*          /* RETURN CODE IS SET TO 8.                               */
*          /*                                                        */
*          /**********************************************************/
*          IF TSBDSPLY = '0'B THEN                                 0030
         B     @RC00028                                            0030
@RF00028 TM    TSBSTAT,TSBDSPLY                                    0030
         BNZ   @RF00030                                            0030
*            R15 = CODE08;                                         0031
         LA    R15,8                                               0031
*          ELSE                                                    0032
*            /********************************************************/
*            /*                                                      */
*            /* THE LINE NUMBER SPECIFIED MUST BE GREATER THAN ZERO  */
*            /* AND NO LARGER THAN THE MAX ALLOWED NUMBER OF LINES   */
*            /* FOR THE TERMINAL IN USE. INVALID LINE NUMBER IS NOTED*/
*            /* WITH RETURN CODE 12.                                 */
*            /*                                                      */
*            /********************************************************/
*            DO;                                                   0032
         B     @RC00030                                            0032
@RF00030 DS    0H                                                  0033
*              R2 = R1;                /*SAVE PARM WITH MODE BIT   0033
*                                        INTACT.                     */
         LR    R2,R1                                               0033
*              SLL(R1,SHIFT1);         /*SHIFT OUT MODE BIT          */
         SLR   R10,R10                                             0034
         IC    R10,SHIFT1                                          0034
         SLL   R1,0(R10)                                           0034
*              SRL(R1,SHIFT1);         /*RESTORE LINE NUMBER         */
         SRL   R1,0(R10)                                           0035
*              IF R1 = 0 THEN          /*ZERO LINE NUMBER IS INVALID */
         LTR   R1,R1                                               0036
         BNZ   @RF00036                                            0036
*                R15 = CODE12;                                     0037
         LA    R15,12                                              0037
*              ELSE                                                0038
*                /****************************************************/
*                /*                                                  */
*                /* IF LINE NUMBER IS NOT TOO LARGE, LINNOPROC IS    */
*                /* CALLED TO SET THE LINE NUMBER AND PERFORM MODE   */
*                /* PROCESSING.                                      */
*                /*                                                  */
*                /****************************************************/
*                                                                  0038
*                IF TSBXTMBF = LGSCREEN THEN /*LOOK FOR LARGE SCREEN */
         B     @RC00036                                            0038
@RF00036 L     R10,TSBEXTNT                                        0038
         CLC   TSBXTMBF-TSBX(2,R10),HW1920                         0038
*ZP60009 BNE   @RF00038                                            0038
         BL    @RF00038                    ASSUME <MOD2 => MOD1 ZP60009
*                  IF R1 > MAXLNLG THEN /*TOO LARGE, SET R15 */
         LA    R11,24                      DEFAULT LINE COUNT   ZP60009
         CLI   TSBPRMR,0                   FIELD POPULATED?     ZP60009
         BE    ROWSOKAY                    NO, TREAT AS MODEL-2 ZP60009
         IC    R11,TSBPRMR                 YES, USE IT INSTEAD  ZP60009
         L     R3,TSBXTVWA-TSBX(,R10)      POINT TO TVWA        ZP60009
         USING TVWA,R3                                          ZP60009
         TM    TVWAFLG6,X'01'              IS TVWAALTS ON?      ZP60009
         BNO   ROWSOKAY                    NO, USE PRIMARY SIZE ZP60009
         IC    R11,TSBALTR                 USE ALTERNATE SIZE   ZP60009
ROWSOKAY CLR   R1,R11                      LARGER THAN MAXIMUM? ZP60009
*ZP60009 CL    R1,FW24                                             0039
         BNH   @RF00039                                            0039
*                    R15 = CODE12;                                 0040
         LA    R15,12                                              0040
*                  ELSE                     /*OK, CALL LNNOPROC      */
*                    GEN (BAL R11,LNNOPROC)                        0041
*                       SETS (R11) REFS (LNNOPROC);                0041
         B     @RC00039                                            0041
@RF00039 DS    0H                                                  0041
         BAL   R11,LNNOPROC
*                ELSE                       /*MUST BE SMALL SCREEN   */
*                  IF R1 > MAXLNSM THEN /*TOO LARGE, SET R15 */
         B     @RC00038                                            0042
@RF00038 LA    R10,12                                              0042
         CLR   R1,R10                                              0042
         BNH   @RF00042                                            0042
*                    R15 = CODE12;                                 0043
         LR    R15,R10                                             0043
*                  ELSE                     /*OK, CALL LNNOPROC      */
*                    GEN (BAL R11,LNNOPROC)                        0044
*                       SETS (R11) REFS (LNNOPROC);                0044
         B     @RC00042                                            0044
@RF00042 DS    0H                                                  0044
         BAL   R11,LNNOPROC
*            END;                                                  0045
*        RETURN;                                           /*@ZM20487*/
@EL00001 DS    0H                                                  0046
@EF00001 DS    0H                                                  0046
@ER00001 BR    R14                                                 0046
*        /************************************************************/
*        /*                                                          */
*        /* LNNOPROC: STORES LINE NUMBER IN TVWALNCT.  SETS FULL     */
*        /*           SCREEN MODE ON OR OFF BASED ON THE MODE BIT    */
*        /*           IN PARAMETER REGISTER 1.                       */
*        /*                                                          */
*        /************************************************************/
*LNNOPROC:                                                         0047
*        R3 = TSBXTVWA;                /*GET ADDRES OF TVWA          */
LNNOPROC L     R10,TSBEXTNT                                        0047
         L     R3,TSBXTVWA-TSBX(,R10)                              0047
*        RFY TVWA BASED(R3);           /*REBASE TVWA                 */
*        GEN (STCM R1,1,TVWALNCT(R3))                              0049
*           REFS (R1) SETS (TVWALNCT); /*STORE LINE NUM IN TVWA      */
         STCM  R1,1,TVWALNCT
*        IF R2 < 0 THEN                /*HIGH ORDER BIT ON?          */
         LTR   R2,R2                                               0050
         BNM   @RF00050                                            0050
*          DO;                                                     0051
*            TVWAFSM = '1'B;           /*YES-SET FULL SCREEN MODE ON */
         OI    TVWAFLG5,TVWAFSM                                    0052
*          END;                                                    0053
*        ELSE                          /*HIGH ORDER BIT OFF          */
*          DO;                         /*TURN OFF FULL SCR MODE INDIC*/
         B     @RC00050                                            0054
@RF00050 DS    0H                                                  0055
*            TVWAFSM = '0'B;           /*FULL SCREEN MODE            */
*            TVWAWO  = '0'B;           /*FULL SCREEN WRITTEN OVER    */
*            TVWAFSW = '0'B;           /*FULL SCREEN TPUT WAITING    */
         NI    TVWAFLG5,255-TVWAFSM-TVWAWO-TVWAFSW                 0057
         NI    TSBFLG5,255-TSBNEDIT    /*EDIT INPUT          ZP60009 */
*          IF TVWALNCT = 1 THEN        /*IS LINENO 1?        AZ74745 */
         CLI   TVWALNCT,1
         BNE   @RC00050
*            TVWAFMSC = '1'B;          /*YES-FORMAT SCREEN   AZ74745 */
         OI    TVWAFLG4,TVWAFMSC                                   0057
*          END;                                                    0058
*        /************************************************************/
*        /*                                                          */
*        /* RETURN TO MAINLINE WITH RETURN CODE 0                    */
*        /*                                                          */
*        /************************************************************/
*        R15 = CODE00;                 /*SET GOOD RETURN CODE        */
@RC00050 SLR   R15,R15                                             0059
*        GEN (BR R11)                                              0060
*           REFS (R11);                /*RETURN TO MAINLINE          */
         BR    R11
*        END   IKT09413                                            0061
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJTSB  )                                        *
*/*%INCLUDE SYSLIB  (IKTTSBX )                                        *
*/*%INCLUDE SYSLIB  (IKTTVWA )                                        *
*;                                                                 0061
         B     @EL00001                                            0061
@DATA    DS    0H
HW1920   DC    H'1920'
*ZP60009 @DATD    DSECT
*ZP60009          DS    0F
*ZP60009 IKT09413 CSECT
*ZP60009          DS    0F
*ZP60009 FW24     DC    F'24'
@DATD    DSECT
         DS    0D
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IKT09413 CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
SHIFT1   DC    AL1(1)
SHIFT9   DC    AL1(9)
         DS    CL2
PATCH    DC    15F'0'
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
@RC00030 EQU   @EL00001
@RC00028 EQU   @EL00001
@RC00042 EQU   @EL00001
@RC00038 EQU   @EL00001
@RC00036 EQU   @EL00001
@RC00039 EQU   @RC00038
@ENDDATA EQU   *
         PRINT NOGEN                                            ZP60009
         IKJTSB EXT=YES                                         ZP60009
         IKTTVWA                                                ZP60009
         PRINT GEN                                              ZP60009
         END   IKT09413,(C'PLS1932',0702,83318)
/*
//*
//STEP15  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT09413('ZP60009')
++MOD(IKT0940A) DISTLIB(AOST3).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP16  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE '******** IKT0940A:    STSIZE  - TERMINAL CONTROL MACRO *
                 *******'
*
*   MODIFIED BY GREG PRICE JANUARY 2003 FOR USERMOD ZP60009
*
IKT0940A CSECT ,                                                   0001
@MAINENT BALR  R15,0                                               0001
         USING *,R15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(16)                                             0001
         DC    C'IKT0940A  77.319'                                 0001
         DROP  R15
@PROLOG  BALR  R12,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,R12                                         0001
         USING TSB,R5
*        R2 = R0;                      /*COPY REGISTERS              */
         LR    R2,R0                                               0027
*        R3 = R1;                                                  0028
         LR    R3,R1                                               0028
*        SLL(R2,SHIFT8);               /*SHIFT OUT ENTRY CODE        */
         SLR   R10,R10                                             0029
         IC    R10,SHIFT8                                          0029
         SLL   R2,0(R10)                                           0029
*        SRL(R2,SHIFT16);              /*SHIFT OUT LINE PARM         */
         SLR   R4,R4                                               0030
         IC    R4,SHIFT16                                          0030
         SRL   R2,0(R4)                                            0030
*        SRL(R3,SHIFT8);               /*SHIFT OUT SIZE PARM         */
*                                                                  0031
         SRL   R3,0(R10)                                           0031
*        IF R2 ^= 0       R3 ^= 0                                  0032
*        THEN                                                      0032
         SLR   R10,R10                                             0032
         CLR   R2,R10                                              0032
         BNE   @RT00032                                            0032
         CLR   R3,R10                                              0032
         BE    @RF00032                                            0032
@RT00032 DS    0H                                                  0033
*          R15 = CODE04;                                           0033
         LA    R15,4                                               0033
*        ELSE                                                      0034
*                                                                  0034
*          /**********************************************************/
*          /*                                                        */
*          /* THE SIZE PARAMETER IN R1 IS REQUIRED WHENEVER          */
*          /* STSIZE IS INVOKED. IF R1 IS ZERO, THE RETURN CODE      */
*          /* IS SET TO 8.                                           */
*          /*                                                        */
*          /**********************************************************/
*                                                                  0034
*          IF R1 = 0                                               0034
*          THEN                                                    0034
         B     @RC00032                                            0034
@RF00032 LTR   R1,R1                                               0034
         BNZ   @RF00034                                            0034
*            R15 = CODE08;                                         0035
         LA    R15,8                                               0035
*          ELSE                                                    0036
*                                                                  0036
*            /********************************************************/
*            /*                                                      */
*            /* SHIFT OUT ENTRY CODE FROM R0 AND ACQUIRE THE         */
*            /* KEY OF THE TSB CREATOR (TCAS).                       */
*            /*                                                      */
*            /********************************************************/
*                                                                  0036
*STSZPROC:   DO;                                                   0036
         B     @RC00034                                            0036
@RF00034 DS    0H                                                  0036
STSZPROC DS    0H                                                  0037
*              GEN(MODESET KEYADDR=TCASKEY,WORKREG=2)              0037
*                  REFS(TCASKEY) SETS(R2);/*GET TCAS KEY             */
         MODESET KEYADDR=TCASKEY,WORKREG=2
*              SLL(R0,SHIFT8);         /*SHIFT OUT ENTRY CODE        */
         SLR   R10,R10                                             0038
         IC    R10,SHIFT8                                          0038
         SLL   R0,0(R10)                                           0038
*              SRL(R0,SHIFT8);                                     0039
*                                                                  0039
         SRL   R0,0(R10)                                           0039
*              /******************************************************/
*              /*                                                    */
*              /* IF THE TERMINAL IN USE IS A DISPLAY AND THE LINE   */
*               /* PARAMETER WAS NOT SPECIFIED, THE RETURN CODE IS   */
*              /* SET TO 8.                                          */
*              /*                                                    */
*              /******************************************************/
*                                                                  0040
*              IF TSBDSPLY = '1'B                                  0040
*              THEN                                                0040
*                                                                  0040
         TM    TSBSTAT,TSBDSPLY                                    0040
         BNO   @RF00040                                            0040
*                IF R0 = 0                                         0041
*                THEN                                              0041
         LTR   R0,R0                                               0041
         BNZ   @RF00041                                            0041
*                  R15 = CODE08;                                   0042
         LA    R15,8                                               0042
*                ELSE                                              0043
*                                                                  0043
*                  /**************************************************/
*                  /*                                                */
*                  /* THE DISPROC ROUTINE IS INVOKED TO UPDATE THE   */
*                  /* TSB AND VERIFY THE SCREEN DIMENSIONS AS        */
*                  /* STANDARD.                                      */
*                  /*                                                */
*                  /**************************************************/
*                                                                  0043
*                  GEN(BAL R11,DISPROC)                            0043
*                      REFS(DISPROC) SETS(R11);                    0043
*                                                                  0043
         B     @RC00041                                            0043
@RF00041 DS    0H                                                  0043
         BAL   R11,DISPROC
*              ELSE                                                0044
*                                                                  0044
*                /****************************************************/
*                /*                                                  */
*                /* IF THE TERMINAL IN USE IS NOT A DISPLAY AND THE  */
*                /* LINE PARAMETER WAS SPECIFIED, THE RETURN CODE IS */
*                /* SET TO 8.                                        */
*                /*                                                  */
*                /****************************************************/
*                                                                  0044
*                IF R0 ^= 0                                        0044
*                THEN                                              0044
         B     @RC00040                                            0044
@RF00040 LTR   R0,R0                                               0044
         BZ    @RF00044                                            0044
*                  R15 = CODE08;                                   0045
         LA    R15,8                                               0045
*                ELSE                                              0046
*                                                                  0046
*                  /**************************************************/
*                  /*                                                */
*                  /* SET VALUE OF SIZE IN TSB AND SET GOOD          */
*                  /* RETURN CODE                                    */
*                  /*                                                */
*                  /**************************************************/
*                                                                  0046
*                  DO;                                             0046
         B     @RC00044                                            0046
@RF00044 DS    0H                                                  0047
*                    TSBLNSZ = R1;                                 0047
         STC   R1,TSBLNSZ                                          0047
*                    R15 = CODE00;                                 0048
         SLR   R15,R15                                             0048
*                  END;                                            0049
*                                                                  0049
*            GEN(MODESET EXTKEY=SUPR); /*RETURN TO SUPR KEY          */
*                                                                  0050
@RC00044 DS    0H                                                  0050
@RC00040 DS    0H                                                  0050
         MODESET EXTKEY=SUPR
*            END STSZPROC;                                         0051
*        RETURN;                                           /*@ZM20487*/
@EL00001 DS    0H                                                  0052
@EF00001 DS    0H                                                  0052
@ER00001 BR    R14                                                 0052
*        /************************************************************/
*        /*                                                          */
*        /* DISPROC IS INVOKED VIA A 'BAL' FROM THE MAINLINE. THIS   */
*        /* ROUTINE SETS THE FIELDS IN THE TSB TO INDICATE THE       */
*        /* SPECIFIED SCREEN DIMENSIONS, AND CHECKS THE DIMENSION    */
*        /* AGAINST KNOWN STANDARDS. NON-STANDARD DIMENSIONS ARE     */
*        /* ALLOWED, BUT CAUSE THE RETURN CODE TO BE SET TO 12.      */
*        /*                                                          */
*        /************************************************************/
*                                                                  0053
*DISPROC:                                                          0053
*                                                                  0053
*        TSBLNSZ = R1;                 /*MOVE IN LINE SIZE           */
DISPROC  STC   R1,TSBLNSZ                                          0053
*        TSBLNNO = R0;                 /*MOVE IN NUMBER OF LINES     */
         STC   R0,TSBLNNO                                          0054
*        TVWAXSCD = '1'B;              /*EXAM SCR DIMEN SW   @G58AK3A*/
*                                                                  0055
         L     R10,TSBEXTNT                                        0055
         L     R10,TSBXTVWA-TSBX(,R10)                             0055
         USING TVWA,R10
         OI    TVWAFLG1,TVWAXSCD                                   0055
*        /***********************************************************/
*        /*                                                         */
*        /* NEW LOGIC FOR USERMOD ZP60009 IS TO CHECK THE NEW       */
*        /* DIMENSIONS AGAINST THE PRIMARY AND ALTERNATE SCREEN     */
*        /* SIZES.  A MATCH TO EITHER IS CONSIDERED TO MEAN THAT    */
*        /* A STANDARD SIZE WAS SPECIFIED NO MATTER WHAT THE        */
*        /* VALUES HAPPEN TO BE.                                    */
*        /*                                                         */
*        /* NO MATCH CAUSES THE ORIGINAL LOGIC TO BE USED.          */
*        /*                                                         */
*        /***********************************************************/
         CLM   R0,1,TSBPRMR                PRIMARY LINES?       ZP60009
         BNE   ALTCHECK                    NO                   ZP60009
         CLM   R1,1,TSBPRMC                PRIMARY COLUMNS?     ZP60009
         BE    @RT00056                    YES, IT IS STANDARD  ZP60009
ALTCHECK CLM   R0,1,TSBALTR                ALTERNATE LINES?     ZP60009
         BNE   STDCHECK                    NO                   ZP60009
         CLM   R1,1,TSBALTC                ALTERNATE COLUMNS?   ZP60009
         BE    @RT00056                    YES, IT IS STANDARD  ZP60009
STDCHECK EQU   *                                                ZP60009
*        /***********************************************************/
*        /*                                                         */
*        /* STANDARD SIZES FOR 3270 DISPLAYS ARE 24X80 AND 12X40.   */
*        /* IF A STANDARD SIZE WAS SPECIFIED, RETURN CODE IS 0. IF  */
*        /* NON-STANDARD SIZE WAS SPECIFIED, RETURN CODE IS 12. NO  */
*        /* OTHER DISPLAYS ARE SUPPORTED BY TSO/VTAM.               */
*        /*                                                         */
*        /***********************************************************/
*                                                                  0056
*        IF (TSBLNNO = LENGTH1 & TSBLNSZ = WIDTH1)                 0056
*           (TSBLNNO = LENGTH2 & TSBLNSZ = WIDTH2)       /*        0056
*                                      WAS STANDARD SIZE GIVEN      */
*        THEN                                                      0056
         CLI   TSBLNNO,24                                          0056
         BNE   @GL00002                                            0056
         CLI   TSBLNSZ,80                                          0056
         BE    @RT00056                                            0056
@GL00002 CLI   TSBLNNO,12                                          0056
         BNE   @RF00056                                            0056
         CLI   TSBLNSZ,40                                          0056
         BNE   @RF00056                                            0056
@RT00056 DS    0H                                                  0057
*          R15 = CODE00;               /*YES - STANDARD SIZE        */
         SLR   R15,R15                                             0057
*        ELSE                                                      0058
*          R15 = CODE12;               /*NO - NON STANDARD SIZE     */
*                                                                  0058
         B     @RC00056                                            0058
@RF00056 LA    R15,12                                              0058
*        /***********************************************************/
*        /*                                                         */
*        /* RETURN TO MAINLINE                                      */
*        /*                                                         */
*        /***********************************************************/
*                                                                  0059
*        GEN(BR R11)                                               0059
*            REFS(R11);                                            0059
@RC00056 DS    0H                                                  0059
         BR    R11
*        END IKT0940A                                              0060
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.      *
*/*%INCLUDE SYSLIB  (IKJTSB  )                                        *
*/*%INCLUDE SYSLIB  (IKTTSBX )                                        *
*/*%INCLUDE SYSLIB  (IKTTVWA )                                        *
*;                                                                 0060
         B     @EL00001                                            0060
@DATA    DS    0H
@DATD    DSECT
         DS    0F
@ZTEMPS  DS    C
@ZTEMPND EQU   *
@ZLEN    EQU   @ZTEMPND-@ZTEMPS
IKT0940A CSECT
         DS    0F
@DATD    DSECT
         DS    0D
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IKT0940A CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
         DS    0D
SHIFT8   DC    AL1(8)
SHIFT16  DC    AL1(16)
TCASKEY  DC    BL1'01100000'
         DS    CL1
PATCH    DC    15F'0'
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
@RC00041 EQU   @RC00040
@RC00034 EQU   @EL00001
@RC00032 EQU   @EL00001
@ENDDATA EQU   *
         PRINT NOGEN                                            ZP60009
         IKJTSB EXT=YES                                         ZP60009
         IKTTVWA                                                ZP60009
         PRINT GEN                                              ZP60009
         END   IKT0940A,(C'PLS2016',0701,77319)
/*
//*
//STEP17  EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IKT0940A('ZP60009')
++MACUPD(IKTTVWA) DISTLIB(ATSOMAC).
./ CHANGE NAME=IKTTVWA
*/*            ZP60009 - ADDING BIT TVWAALTS.                        */ 00147900
TVWAALTS EQU   X'01'             ALTERNATE SCREEN SIZE IN USE   ZP60009 01268400
*     3 TVWAALTS     BIT(1),     /*ALT SCREEN SIZE IN USE     ZP60009*/ 03110400
++MACUPD(STFSMODE) DISTLIB(ATSOMAC).
./ CHANGE NAME=STFSMODE
&NAME    STFSMODE &A,&INITIAL=NO,&RSHWKEY=64,&NOEDIT=NO         ZP60009 00100000
         LCLA  &SYSNEDT                                         ZP60009 01111000
&SYSNEDT SETA  0                                                ZP60009 01121000
         AIF   ('&NOEDIT' NE 'YES' AND '&NOEDIT' NE 'NO').ERROR5  ZP6-9 01125200
         AIF   ('&NOEDIT' EQ 'YES' AND '&A' EQ 'OFF').ERROR4    ZP60009 01125400
         AIF   (&RSHWKEY GT 0 AND &RSHWKEY LT 25).TESTFS        ZP60009 01125700
         AIF   ('&INITIAL' EQ 'NO').TESTNE                      ZP60009 01130000
.TESTNE  ANOP                                                   ZP60009 01135100
         AIF   ('&NOEDIT' EQ 'NO').TESTA                        ZP60009 01135200
&SYSNEDT SETA  32                                               ZP60009 01135300
.ERROR1  IHBERMAC 54,,INITIAL                                   ZP60009 01310000
.ERROR4  IHBERMAC 1020,NOEDIT,&A                                ZP60009 01345000
         MEXIT                                                  ZP60009 01346000
.ERROR5  IHBERMAC 54,,NOEDIT                                    ZP60009 01347000
         MEXIT                                                  ZP60009 01348000
&NAME    LA    1,&SYSFS+&SYSINIT+&SYSNEDT  MODE/INITIAL/NOEDIT  ZP60009 01400000
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP18  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60009)
          .
/*
//*
//STEP19  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60009)
        CHECK
        .
/*
//*
//STEP20  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60009)
        DIS(WRITE)
        .
/*
//
