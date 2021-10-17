//ZP60032  JOB (SYSGEN),'J06 M43: ZP60032',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ALLOW THE GTTERM MACRO TO REPORT THE TSO TERMINAL NAME.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60032)         /* ADD TERMID TO THE GTTERM MACRO */  .
++VER(Z038) FMID(ETI1106) PRE(UZ44753)
 /*
   PROBLEM DESCRIPTION:
     THE GTTERM MACRO CANNOT EXPLOIT ALL FUNCTIONS OF THE INTERFACE.
       THE ZP60009 USERMOD ENHANCED TSO/VTAM TO ALLOW THE SVC 94
       GTTERM INTERFACE TO RETURN THE 8-BYTE VTAM LU NAME OF THE
       TSO TERMINAL, BUT THE GTTERM MACRO OWNED BY TIOC DOES NOT
       SUPPORT THE TERMID OPERAND.

       THIS USERMOD ADDS THE TERMID OPERAND TO THE GTTERM MACRO
       IN A WAY WHICH IS COMPATIBLE WITH TSO/E.


   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 32.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MACROS:
       GTTERM
 */.
++MAC(GTTERM) DISTLIB(ATSOMAC).
         MACRO                                                          00050000
&NAME    GTTERM &PRMSZE=,&ALTSZE=,&ATTRIB=,&TERMID=,&MF=I      @G76XRYU 00070990
.*                                                             @OW03892 00071980
.* A000000-999999                                              @G76XR00 00075000
.* A034000                                                     @OZ90350 00077000
.* NOCHANGE SHIPPED WITH JCLIN                                 @OY20024 00078000
.* NOCHANGE SHIPPED WITH JCLIN                                 @OY26821 00079000
.* ADDED TERMID PARAMETER                                      @OW03892 00080990
.*                                                                      00081980
         LCLC  &NDX                                            @G76XRYU 00085000
&NDX     SETC  '&SYSNDX'                                       @G76XRYU 00090000
         AIF   ('&MF' EQ 'I' AND '&PRMSZE' EQ '' AND '&ATTRIB' EQ '' AN-00799990
               D '&TERMID' EQ '').ERROR1              @G76XRYU @OW03892 01499980
         AIF   ('&MF' EQ 'L').LFORM                                     02450000
         AIF   ('&MF(1)' EQ 'E').EFORM                                  02500000
         AIF   ('&MF' NE 'I').ERROR2                                    02550000
.*********************I -FORM OF MACRO********************************* 02600000
&NAME    CNOP  0,4                                                      02650000
         BAL   1,*+20                   BRANCH AROUND PARMS    @G76XRYU 02672990
.*                                                             @OW03892 02675980
GTRM&NDX DC    A(0)                     ADDRESS OF PRIMARY     @G76XRYU 02680000
         DC    A(0)                     ADDRESS OF ALTERNATE            02690000
         DC    A(0)                     ADDRESS OF ATTRIBUTE   @G76XRYU 02700000
         DC    A(0)                     ADDRESS OF TERMINAL ID @OW03892 03000000
         AGO   .STADDR                                                  03350000
.EFORM   ANOP                                                           03400000
&NAME    CNOP  0,4                                             @OZ90350 03429990
         IHBOPLST ,,&NAME,MF=&MF                                        03450000
.STADDR  ANOP                                                           03500000
.*********COMMON CODE FOR BOTH I AND E FORM OF MACRO******************  03550000
         AIF   ('&PRMSZE' EQ '').LABEL2                                 03600000
         AIF   ('&PRMSZE'(1,1) NE '(').LOADPRM                          03700000
         ST    &PRMSZE(1),0(,1)         STORE PRIMARY ADDRESS           03750000
         AGO   .LABEL2                                                  03800000
.LOADPRM ANOP                                                           03850000
         AIF   ('&PRMSZE'(K'&PRMSZE,1) EQ ')' OR '&MF' NE 'I').LPARM    03857000
         ORG   GTRM&NDX                 PUT ADDR OF PRIMARY    @G76XRYU 03864000
         DC    A(&PRMSZE)               IN PARM LIST           @G76XRYU 03871000
         ORG                                                            03878000
         AGO   .LABEL2                                         @G76XRYU 03885000
.LPARM   ANOP                      ..LA ADDR OR EXECUTE FORM   @G76XRYU 03892000
         LA    0,&PRMSZE                LOAD ADDRESS OF PRIMARY         03900000
         ST    0,0(,1)                  STORE ADDRESS OF PRIMARY        03950000
.LABEL2  ANOP                                                           04000000
         AIF   ('&ALTSZE' EQ '').IEATRCK                       @G76XRYU 04020000
         AIF   ('&ALTSZE'(1,1) NE '(').LOADALT                          04150000
         ST    &ALTSZE(1),4(,1)         STORE ADDRESS OF ALTERNATE      04200000
         AGO   .IEATRCK                                        @G76XRYU 04220000
.LOADALT ANOP                                                           04300000
         AIF   ('&ALTSZE'(K'&ALTSZE,1) EQ ')' OR '&MF' NE 'I').LAALT    04300100
         ORG   GTRM&NDX+4               PUT ALTERNATE SIZE     @G76XRYU 04306100
         DC    A(&ALTSZE)               IN PARM LIST           @G76XRYU 04312100
         ORG                                                            04318100
         AGO   .IEATRCK                                        @G76XRYU 04324100
.LAALT   ANOP                     ...LA ADDR OR EXECUTE FORM   @G76XRYU 04330100
         LA    0,&ALTSZE                LOAD ADDR OF ALTERNATE @G76XRYU 04336100
         ST    0,4(,1)                  STORE ADD OF ALTERNATE @G76XRYU 04350000
.*  PROCESS ATTRIBUTE PARM FOR I AND E FORMS WHEN NOT NULL     @G76XRYU 04450000
.IEATRCK ANOP                                                  @G76XRYU 04451000
         AIF   ('&ATTRIB' EQ '').LABEL3               @G76XRYU @OW03892 04452490
         AIF   ('&ATTRIB'(1,1) EQ '(').REGATR                  @G76XRYU 04453000
         AIF   ('&ATTRIB'(K'&ATTRIB,1) EQ ')' OR '&MF' NE 'I').LAATRIB  04454000
         ORG   GTRM&NDX+8               PUT ATTRIB BYTE ADDR   @G76XRYU 04455000
         DC    A(&ATTRIB)               IN PARM LIST           @G76XRYU 04456000
         ORG                                                            04457000
         AGO   .LABEL3                                @G76XRYU @OW03892 04458490
.LAATRIB ANOP                       .. LA ADDR OR EXECUTE FORM @G76XRYU 04459000
         LA    0,&ATTRIB                GET ADR OF ATTRIB BYTE @G76XRYU 04460000
         ST    0,8(1)                   PUT IN 3RD PARM WORD   @G76XRYU 04461000
         AGO   .LABEL3                                @G76XRYU @OW03892 04462490
.REGATR  ANOP                                                  @G76XRYU 04463000
         ST    &ATTRIB(1),8(1)          REG => 3RD PARM WORD   @G76XRYU 04464000
.*  PROCESS TERMINAL ID PARM FOR I AND E FORMS WHEN NOT NULL   @OW03892 04464040
.LABEL3  ANOP                                                  @OW03892 04464080
         AIF   ('&TERMID' EQ '').SVCENTY                       @OW03892 04464120
         AIF   ('&TERMID'(1,1) NE '(').LOTRMID                 @OW03892 04464160
         ST    &TERMID(1),12(,1)        STORE PRIMARY ADDRESS  @OW03892 04464200
         OI    12(1),128                END OF LIST INDICATOR  @OW03892 04464240
         AGO   .SVCENT2                                        @OW03892 04464280
.LOTRMID ANOP                                                  @OW03892 04464320
         AIF   ('&TERMID'(K'&TERMID,1) EQ ')' OR '&MF' NE 'I').LTERM    04464360
.*                                                             @OW03892 04464400
         ORG   GTRM&NDX+12              PUT ADDR OF TERMID IN  @OW03892 04464440
         DC    XL1'80',AL3(&TERMID)     PARM LIST WITH END OF  @OW03892 04464480
.*                                      LIST INDICATOR         @OW03892 04464520
         ORG                                                            04464560
         AGO   .SVCENT2                                        @OW03892 04464600
.LTERM   ANOP                      ..LA ADDR OR EXECUTE FORM   @OW03892 04464640
         LA    0,&TERMID                LOAD ADDRESS OF TERMINAL ID     04464680
.*                                                             @OW03892 04464720
         ST    0,12(,1)                 STORE ADDRESS OF TERMINAL ID    04464760
.*                                                             @OW03892 04464800
         OI    12(1),128                END OF LIST INDICATOR  @OW03892 04464840
         AGO   .SVCENT2                                        @OW03892 04464880
.SVCENTY ANOP                                                           04465000
         OI    8(1),128                 END OF LIST INDICATOR  @G76XRYU 04470000
.SVCENT2 ANOP                                                           04510000
         LA    0,17                     ENTRY CODE                      04550000
         SLL   0,24                     SHIFT TO HIGH ORDER BYTE        04600000
         SVC   94                       ISSUE SVC                       04650000
         MEXIT                                                          04700000
.***************  L  - FORM  ***************************                04750000
.LFORM   ANOP                                                           04800000
&NAME    DS    0F                                                       04850000
         AIF   ('&PRMSZE' EQ '').NOPRMAD                                04900000
         AIF   ('&PRMSZE'(1,1) EQ '(').NOPRMAD                          04950000
         DC    A(&PRMSZE)               ADDRESS OF PRIMARY PARM ADDR    05000000
         AGO   .CHKALT                                                  05050000
.NOPRMAD ANOP                                                           05100000
         DC    A(0)                     ADDRESS OF PRIMARY PARM ADDR    05150000
.CHKALT  AIF   ('&ALTSZE' EQ '').NOALTAD                                05200000
         AIF   ('&ALTSZE'(1,1) EQ '(').NOALTAD                          05250000
         DC    A(&ALTSZE)               ADDRESS OF ALTERNATE ADDR       05300000
         AGO   .LATTCK                                         @G76XRYU 05320000
.NOALTAD ANOP                                                           05400000
         DC    A(0)                     ADDR OF ALTERNATE      @G76XRYU 05420000
.*  PROCESS ATTRIBUTE PARM FOR LIST FORM                       @G76XRYU 05422000
.LATTCK  ANOP                                                  @G76XRYU 05424000
         AIF   ('&ATTRIB' NE '').CKATTR                        @G76XRYU 05426000
         DC    A(0)                     L-FORM--ATTRIB BYTE    @G76XRYU 05428000
         AGO   .CKTERM                                @G76XRYU @OW03892 05430990
.CKATTR  ANOP                                                  @G76XRYU 05432000
         AIF   ('&ATTRIB'(1,1) NE '(').ATTROK                  @G76XRYU 05434000
         MNOTE 12,'IHB300 INCOMPATIBLE OPERANDS: MF=L AND ATTRIB=&ATTRI*05436000
               B'                                              @G76XRYU 05438000
         AGO   .CKTERM                                @G76XRYU @OW03892 05440990
.ATTROK  ANOP                                                  @G76XRYU 05442000
         DC    A(&ATTRIB)               L-FORM--A(ATTR BYTE)   @G76XRYU 05444000
         AGO   .CKTERM                                         @OW03892 05558990
.CKTERM  AIF   ('&TERMID' EQ '').NOTRMAD                       @OW03892 05567980
         AIF   ('&TERMID'(1,1) EQ '(').NOTRMAD                 @OW03892 05576970
         DC    A(&TERMID)               ADDRESS OF TERMINAL ID ADDR     05585960
.*                                                             @OW03892 05594950
         MEXIT                                                 @OW03892 05603940
.NOTRMAD ANOP                                                  @OW03892 05612930
         DC    A(0)                     ADDRESS OF TERMINAL ID ADDR     05621920
.*                                                             @OW03892 05630910
         MEXIT                                                 @OW03892 05639900
.ERROR1  ANOP                                                           05650000
         IHBERMAC 1006,PRMSZE                                           05700000
         MEXIT                                                          05750000
.ERROR2  IHBERMAC 54,,&MF                                               05800000
         MEXIT                                                          05850000
         MEND                                                           05900000
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60032)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60032)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60032)
        DIS(WRITE)
        .
/*
//
