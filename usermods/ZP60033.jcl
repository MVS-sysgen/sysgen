//ZP60033  JOB (SYSGEN),'J07 M44: ZP60033',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ADD SUPPORT FOR THE LOC PARAMETER TO THE GETMAIN MACRO.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60033)         /* ADD LOC= TO THE GETMAIN MACRO */  .
++VER(Z038) FMID(EBB1102)
 /*
   PROBLEM DESCRIPTION:
     THE GETMAIN MACRO DOES NOT TOLERATE A LOC VALUE SPECIFICATION.
       MANY PROGRAMS SPECIFY VIRTUAL STORAGE LOCATIONS OF 24-BIT
       OR 31-BIT WHEN REQUESTING MORE STORAGE FROM THE SYSTEM.
       FURTHER, SOME PROGRAMS SPECIFY THAT WHEN PAGE-FIXED, THE
       NEW STORAGE MAY BE BACKED IN 24-BIT, 31-BIT OR 64-BIT
       ADDRESSABLE REAL STORAGE.  PROGRAMS WITH SUCH LOC VALUES
       CODED WILL NOT BE ASSEMBLED CORRECTLY ON MVS 3.8.

       THIS USERMOD ADDS SUPPORT FOR THE LOC VALUE TO THE GETMAIN
       MACRO.  THE LOC PARAMETER MAY BE SPECIFIED WITH THE RU AND
       RC FORMS OF GETMAIN.

       THE FIRST VALUE OF LOC MAY BE ONE OF THE FOLLOWING LITERALS:
            'BELOW', '24', 'ANY' OR '31'.

       USE 'BELOW' OR '24' TO REQUEST 24-BIT ADDRESSABLE STORAGE.
       USE 'ANY' OR '31' TO REQUEST 31-BIT ADDRESSABLE STORAGE.

       IF SPECIFIED, THE SECOND VALUE OF LOC MAY BE ONE OF:
            'ANY', '31' OR '64'.

       ANY ONE OF THESE THREE VALUES REQUESTS AN OVERRIDE TO THE
       SYSTEM DEFAULT OF BACKING THE STORAGE BELOW THE 16MB LINE
       IN REAL STORAGE IF THE STORAGE IS EVER PAGE-FIXED.

       THIS VERSION OF GETMAIN WILL GENERATE FLAG SETTINGS COMPATIBLE
       WITH MVS/XA.  A SECOND LOC VALUE OF '64' IS TOLERATED BUT IS
       TREATED AS IF '31' HAD BEEN SPECIFIED.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 33.

     REWORK HISTORY:
       2017-01-26: FIRST SMP VERSION OF THE AUGUST 2016 REVISION
                   OF THE MVS/380 VERSION.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MACROS:
       GETMAIN
 */.
++MAC(GETMAIN) DISTLIB(AMACLIB).
* %/*
         MACRO
&NAME    GETMAIN &MODE,&LV=,&LA=,&A=,&SP=,&MF=I,&HIARCHY=,&BNDRY=,     X
               &KEY=,&BRANCH=,&RELATED=,&LOC=              *MVS380*
.*     OS/VS2 RELEASE 4 VERSION -- 10/21/75
         LCLA  &SVCN,&PRG
         LCLB  &B,&C,&D,&GLBR,&BR,&SPREG,&UNCND,&BND,&L,&V,&CND
         LCLB  &RL,&VA,&VB                                 *MVS380*
         LCLB  &KEYRG,&LVREG
         LCLC  &GNAME
.*0000000400,012200,013000-013200,016800-017600,026600-026800      LC0A
.*    047000                                                       LC0A
&GNAME   SETC  'IHB'.'&SYSNDX'
&PRG     SETA  15                 DEFAULT REGISTER FOR RC AND RU
         AIF   ('&MODE' EQ '').NOMODE
         AIF   ('&MODE'(1,1) NE 'V' AND '&MODE'(1,1) NE 'E' AND        *
               '&MODE'(1,1) NE 'L' AND '&MODE'(1,1) NE 'R' AND         *
               '&MODE'(1,1) NE 'P').ERROR7                     @Z30EN9G
         AIF   (K'&MODE EQ 1).MODE1    SINGLE CHARACTER MODE?
         AIF   ('&MODE'(2,1) NE 'U' AND '&MODE'(2,1) NE 'C').ERROR7
.MODE1   ANOP
&L       SETB  ('&MODE'(1,1) EQ 'V' OR '&MODE'(1,1) EQ 'L')
&V       SETB  ('&MODE'(1,1) EQ 'V')
         AIF   ('&MODE'(1,1) EQ 'R' OR '&MODE'(1,1) EQ 'P').NOMODE
         AIF   (K'&MODE EQ 1).ERROR7
&CND     SETB  ('&MODE'(2,1) EQ 'C')
.NOMODE  AIF   ('&BNDRY' EQ '').NOBNDRY
&BND     SETB  ('&BNDRY' EQ 'PAGE')
.NOBNDRY AIF   ('&LOC' EQ '').NOLOC                        *MVS380*
.*                                                         *MVS380*
.*   LOC ADDED FOR THE MVS/380 PROJECT FOR COMPATIBILITY   *MVS380*
.*   WITH THE X/A AND LATER VERSIONS OF THE SYSTEM.        *MVS380*
.*   BITS ARE (CURRENTLY) IGNORED BY THE SVC 120           *MVS380*
.*   INTERCEPT CODE.                                       *MVS380*
.*   USE RC/RU, AND LV>=16mb FOR ABOVE THE LINE MEMORY     *MVS380*
.*                                                         *MVS380*
.*   REVISION 2016-AUG-25                                  *MVS380* REV
.*   1. Remove LOC=EXPLICIT due to no INADDR parameter.    *MVS380* REV
.*   2. Support '24', '31' and '64' LOC specifications.    *MVS380* REV
.*                                                         *MVS380* REV
.*   R15 low byte "MODE" settings match those of z/OS.     *MVS380* REV
.*   LOC=(,64) and LOC=(,31) are flagged identically.      *MVS380* REV
.*   z/OS sets x'10' in the R15 high byte for LOC=(,64).   *MVS380* REV
.*                                                         *MVS380* REV
         AIF   (N'&LOC LE 2).LOCNUGH   TOO MANY OPERANDS?  *MVS380*
         IHBERMAC 1012,LOC   TOO MANY OPERANDS             *MVS380*
.LOCNUGH AIF   (N'&LOC EQ 1).LOCONE    JUST ONE ?          *MVS380*
         AIF   ('&LOC(2)' EQ '').LOCONE  COMMA ONLY?       *MVS380*
&RL      SETB  ('&LOC(2)' EQ 'ANY' OR '&LOC(2)' EQ '31' OR             X
               '&LOC(2)' EQ '64')                          *MVS380* REV
         AIF   (&RL).LOCONE                                *MVS380* REV
.ERR22   IHBERMAC 1007,LOC(2)     INVALID                  *MVS380*
         MEXIT ,                                           *MVS380*
.LOCONE  AIF   ('&LOC(1)' EQ '').NOLOC                     *MVS380*
         AIF   ('&LOC(1)' EQ 'RES').NOLOC                  *MVS380* REV
&VA      SETB  ('&LOC(1)' EQ 'ANY' OR '&LOC(1)' EQ '31' )  *MVS380* REV
&VB      SETB  ('&LOC(1)' EQ 'BELOW' OR '&LOC(1)' EQ '24' OR           X
               &VA)           Set both bits for LOC=31     *MVS380* REV
&RL      SETB  (&VA OR &RL)   Handle absent &LOC(2)        *MVS380* REV
         AIF   (&VB).NOLOC                                 *MVS380* REV
         IHBERMAC 1007,LOC(1)     INVALID                  *MVS380*
         MEXIT ,                                           *MVS380*
.NOLOC   AIF   ('&KEY' EQ '').SKIP                         *MVS380*
         AIF   ('&MODE' NE 'RC' AND '&MODE' NE 'RU').ERRORA
         AIF   ('&BRANCH' EQ '').ERRORE
         AIF   ('&BRANCH' NE 'YES' AND '&BRANCH'(1,1) NE '(').ERRORA
         AIF   ('&BRANCH(1)' NE 'YES').ERRORE
         AIF   ('&KEY'(1,1) EQ '(').SKIP
         AIF   (T'&KEY NE 'N').SKIP   CAN'T CHECK EQUATED VALUE
         AIF   (&KEY GT 15).ERRORB
.SKIP    AIF   ('&MF' EQ 'L' AND '&BRANCH' NE '').ERRORC
         AIF   ('&BRANCH' EQ '').BRCNT
         AIF   (N'&BRANCH LT 2).BRSNG
       AIF   ('&BRANCH(1)' NE 'YES' OR '&BRANCH(2)' NE 'GLOBAL').ERRORD
         AIF   ('&BRANCH(2)' EQ 'GLOBAL' AND '&MODE' NE 'RC' AND       *
               '&MODE' NE 'RU').ERRORF
&GLBR    SETB  1
&BR      SETB  1
         AGO   .BRCNT
.BRSNG   AIF   ('&BRANCH' NE 'YES').ERRORD
&BR      SETB  1
.BRCNT   ANOP
&SVCN    SETA  4
         AIF   ('&MODE' EQ '' AND '&MF' EQ 'I').ERROR1
         AIF   ('&LV' NE '' AND '&LA' NE '').ERROR5
         AIF   ('&MODE' EQ '').CONT1
         AIF   ('&MODE'(1,1) EQ 'E' AND '&LA' NE '').ERROR6
         AIF   ('&MODE'(1,1) EQ 'R' AND '&LA' NE '').ERROR6
         AIF   ('&BNDRY' NE '' AND '&BNDRY' NE 'DBLWD' AND '&BNDRY'    X
               NE 'PAGE').ERROR10
         AIF   ('&BNDRY' EQ 'PAGE' AND '&MODE' EQ 'R').ERR10A
         AIF   ('&MODE'(1,1) EQ 'L' AND '&LV' NE '').ERROR4
         AIF   ('&MODE'(1,1) EQ 'V' AND '&LV' NE '').ERROR4
         AIF   ('&HIARCHY' EQ '' OR '&HIARCHY' EQ '0' OR '&HIARCHY' EQ X
               '1').CONT1
         IHBERMAC 195
         MEXIT
.CONT1   AIF   ('&MF' EQ 'L').LROUT
         AIF   ('&MF' EQ 'I').IROUT
         AIF   (N'&MF LE 1).ERROR2
         AIF   ('&MF(1)' NE 'E').ERROR2
&NAME    IHBINNRA &MF(2)
         AIF   ('&LV' EQ '').CONTB
         AIF   ('&LV'(1,1) EQ '(').ISAREG
         AIF   (T'&LV NE 'N').CONTBB
         AIF   (&LV LE 4095).CONTAA
.CONTBB  CNOP  0,4
         B     *+8                               BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         MVC   0(4,1),*-4                        MOVE LENGTH INTO LIST
         AGO   .CONTB
.CONTAA  LA    14,&LV.(0,0)                      PICK UP LENGTH
         ST    14,0(0,1)                         STORE INTO LIST
         AGO   .CONTB
.ISAREG  ST    &LV(1),0(0,1)                     STORE LENGTH INTO LIST
.CONTB   AIF   ('&LA' EQ '').CONTD
         AIF   ('&LA'(1,1) EQ '(').ISAREGA
         LA    14,&LA                            PICK UP LIST ADDRESS
         ST    14,0(0,1)                         STORE INTO PARAM LIST
         AGO   .CONTD
.ISAREGA ST    &LA(1),0(0,1)                     STORE LA IN PARAM LIST
.CONTD   AIF   ('&MODE' EQ '' AND '&BNDRY' EQ '').CONTE
         MVI   8(1),B'&L&V&CND&BND.0000'         SET MODE / BNDRY FLGS
.CONTE   AIF   ('&A' EQ '').CONTI
         AIF   ('&A'(1,1) EQ '(').ISAREGB
         LA    14,&A                             LOAD AREA LIST ADDRESS
         ST    14,4(0,1)                         STORE INTO PARAM LIST
         AGO   .CONTI
.ISAREGB ST    &A(1),4(1,0)                      STORE INTO PARAM LIST
.CONTI   AIF   ('&SP' EQ '').FINI
         AIF   ('&SP'(1,1) EQ '(').ISAREGC
         MVI   9(1),&SP                          MOVE IN SUBPOOL VALUE
         AGO   .FINI
.ISAREGC STC   &SP(1),9(1,0)                     STORE SUBPOOL VALUE
         AGO   .FINI
.LROUT   AIF   ('&LV' EQ  '').CONTJ
         AIF   ('&LV'(1,1) EQ '(').ERROR3
&NAME    DC    A(&LV)                            LENGTH
         AGO   .CONTLL
.CONTJ   AIF   ('&LA' EQ '').CONTK
         AIF   ('&LA'(1,1) EQ '(').ERROR3
&NAME    DC    A(&LA)                            ADDR. OF LENGTH LIST
         AGO   .CONTLL
.CONTK   ANOP
&NAME    DC   A(0)                               LA OR LU
.CONTLL  AIF   ('&A' EQ '').CONTM
         AIF   ('&A'(1,1) EQ '(').ERROR3
         DC    A(&A)                             ADDR. OF ADDR. LIST
         AGO   .CONTN
.CONTM   DC    A(0)                              ADDR. OF ADDR. LIST
.CONTN   DC    BL1'&L&V&CND&BND.0000'            MODE AND OPTION FLAGS
         AIF   ('&SP' EQ '').CONTU
         AIF   ('&SP'(1,1) EQ '(').ERROR3
         DC    AL1(&SP)                          SUBPOOL VALUE
         AGO   .FINISH
.CONTU   DC    AL1(0)                            SUBPOOL VALUE
.FINISH  MEXIT
.IROUT   AIF   ('&MODE'(1,1) EQ 'R').RROUT
         AIF   ('&MODE'(1,1) EQ 'P').PROUT                     @Z30EN9G
         AIF   ('&LV' EQ '' AND '&LA' EQ '').ERROR8
         CNOP  0,4
&NAME    BAL   1,*+14                            BRANCH AROUND LIST
         AIF   ('&LV' EQ '').CNTA
         AIF   ('&LV'(1,1) EQ '(').CNTB
         DC    A(&LV)                            LENGTH
         AGO   .CNTC
.CNTB    DC    A(0)                              LENGTH
&B       SETB  1
         AGO   .CNTC
.CNTA    AIF   ('&LA'(1,1) EQ '(').CNTD
         DC    A(&LA)                            ADDR. OF LENGTH LIST
         AGO   .CNTC
.CNTD    DC    A(0)                              ADDR. OF LENGTH LIST
&C       SETB  1
.CNTC    AIF   ('&A' EQ '').ERROR8
         AIF   ('&A'(1,1) EQ '(').CNTE
         DC    A(&A)                  ADDR. OF ADDR. LIST
         AGO   .CNTF
.CNTE    DC    A(0)                              ADDR. OF ADDR. LIST
&D       SETB  1
.CNTF    DC    BL1'&L&V&CND&BND.0000'            MODE AND OPTION FLAGS
         AIF   ('&SP' EQ '').CNTL
         AIF   ('&SP'(1,1) EQ '(').ISAREGQ
         DC    AL1(&SP)                          SUBPOOL VALUE
         AGO   .CNTM
.ISAREGQ DC    AL1(0)                            SUBPOOL VALUE
         STC   &SP(1),9(0,1)                     STORE SP INTO LIST
         AGO   .CNTM
.CNTL    DC    AL1(0)                            SUBPOOL VALUE
.CNTM    AIF   (NOT &B).CNTN
         ST    &LV(1),0(0,1)                     STORE LENGTH INTO LIST
         AGO   .CNTO
.CNTN    AIF   (NOT &C).CNTO
         ST    &LA(1),0(0,1)                     STORE LA INTO LIST
.CNTO    AIF   (NOT &D).FINI
         ST    &A(1),4(0,1)                      STORE INTO PARAM LIST
         AGO   .FINI
.PROUT   ANOP                                                  @Z30EN9G
         AIF   ('&SP' EQ '').ERROR8                            @Z30EN9G
         AIF   ('&BRANCH' EQ '').ERROR8                        @Z30EN9G
         AGO   .PROUT1                                         @Z30EN9G
.*       R-FORM GETMAIN (REGMAIN) OR RC OR RU FORMS
.RROUT   AIF   ('&A' NE '').ERROR9
         AIF   ('&LV' EQ '').ERROR8
         AIF   (K'&MODE EQ 2).NREGM
&SVCN    SETA  10
         AIF   ('&LV'(1,1) EQ '(').ISARGA
         AIF   ('&SP' EQ '').CTUA
.PROUT1  ANOP                                                  @Z30EN9G
         AIF   ('&SP'(1,1) EQ '(').ISARGB
         CNOP  0,4
         AIF   ('&MODE' EQ 'P').PMODE                          @Z30EN9G
&NAME    BAL   1,*+8                             BRANCH AROUND SP+LV
         DC    AL1(&SP)                          SUBPOOL VALUE
         DC    AL3(&LV)                          LENGTH
         L     0,0(0,1)                          LOAD SP AND LV
         AGO   .FINI
.PMODE   ANOP                                                  @Z30EN9G
&NAME    LA    0,&SP.(0,0)      PICK UP SUBPOOL                @Z30EN9G
         SLL   0,24(0)          SHIFT TO HIGH-ORDER BYTE       @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.CTUA    AIF   (T'&LV NE 'N').CTUAA
         AIF   (&LV LE 4095).CONTCC
.CTUAA   CNOP  0,4
&NAME    BAL   1,*+8                             BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         L     0,0(0,1)                          LOAD LENGTH
         AGO   .FINI
.CONTCC  ANOP
&NAME    LA    0,&LV.(0,0)                       LOAD LENGTH
         AGO   .NOP2
.ISARGB  AIF   ('&MODE' EQ 'P').PMODE2                         @Z30EN9G
         AIF   (T'&LV NE 'N').CONTFF                           @Z30EN9G
         AIF   (&LV LE 4095).CONTEE
.CONTFF  CNOP  0,4
&NAME    BAL   1,*+8                             BRANCH AROUND LENGTH
         DC    A(&LV)                            LENGTH
         LR    0,&SP(1)                          PICK UP SUBPOOL
         SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         O     0,0(0,1)                          PACK SP AND LV
         AGO   .FINI
.PMODE2  ANOP                                                  @Z30EN9G
         AIF   ('&SP(1)' EQ '0').PMODE3                        @Z30EN9G
&NAME    LR    0,&SP(1)         PICK UP SUBPOOL                @Z30EN9G
         CNOP  0,4                                             @Z30EN9G
         SLL   0,24(0)          SHIFT TO HIGH-ORDER BYTE       @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.PMODE3  ANOP                                                  @Z30EN9G
         CNOP  0,4                                             @Z30EN9G
&NAME    SLL   0,24(0)          SHIFT SUBPOOL TO HIGH-BYTE     @Z30EN9G
         BAL   1,*+4            INDICATE GETMAIN               @Z30EN9G
         AGO   .FINI                                           @Z30EN9G
.CONTEE  ANOP
&NAME    LR    0,&SP(1)                          PICK UP SUBPOOL
         SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         LA    1,&LV.(0,0)                       LOAD LENGTH
         OR    0,1                               PACK SP AND LV
         AGO   .NOP2
.ISARGA  AIF   ('&LV(1)' EQ '0').ZEROUT
         AIF   ('&SP' EQ '').CTUB
         AIF   ('&SP'(1,1) EQ '(').ISARGC
&NAME    LA    0,&SP.(0,0)                       PICK UP SUBPOOL
.NOP1    SLL   0,24(0)                           SHIFT TO HI-ORDER BYTE
         OR    0,&LV(1)                          PACK SP AND LV
.NOP2    BAL   1,*+4                             INDICATE GETMAIN
         AGO   .FINI
.CTUB    ANOP
&NAME    LR    0,&LV(1)                          LOAD LENGTH
         AGO   .NOP2
.ISARGC  ANOP
&NAME    LR    0,&SP(1)                          PICK UP SUBPOOL
         AGO   .NOP1
.ZEROUT  AIF   ('&SP' NE '').ERROR0
&NAME    BAL   1,*+4                             INDICATE GETMAIN
         AGO   .FINI
.NREGM   ANOP    RC AND RU FORMS OF GETMAIN
&SVCN    SETA  120
&UNCND   SETB  ('&MODE' EQ 'RU')  CONDITIONAL REQUEST FLAG
         AIF   (NOT &BR).COND     CHECK FOR BRANCH ENTRY TO GETMAIN
&PRG     SETA  3                  SET PARAMETER REG FOR BRANCH ENTRY
.COND    AIF   ('&SP' EQ '').CREG1
&SPREG   SETB  ('&SP'(1,1) EQ '(')
.CREG1   AIF   ('&KEY' EQ '').CREG01
&KEYRG   SETB  ('&KEY'(1,1) EQ '(')
.CREG01  ANOP
&LVREG   SETB  ('&LV'(1,1) EQ '(')
&B       SETB  (&SPREG AND &KEYRG)
         CNOP  0,4
&NAME    B     *+12-4*&LVREG-2*&B                BRANCH AROUND DATA
         AIF   (&LVREG).CREG11
         DC    A(&LV)                            LENGTH
.CREG11  ANOP
&GNAME.F DC    AL1(0)                            RESERVED
         AIF   ('&KEY' EQ '').CREG1A             KEY OMITTED ?
         AIF   (&KEYRG).CREG1B                   KEY IN REGISTER?
         DC    AL1(&KEY*16)                      STORAGE KEY
         AGO   .CREG1B
.CREG1A  DC    AL1(0)                            RESERVED
.CREG1B  AIF   ('&SP' EQ '').SPNULL1             SUBPOOL OMITTED?
         AIF   (&SPREG).CREG1C                   SUBPOOL IN REGISTER?
         DC    AL1(&SP)                          SUBPOOL
         AGO   .CREG1C
.CREG1E  AIF   ('&LV(1)' EQ '0').CREG1D                        @ZA07133
         LR    0,&LV(1)                          LOAD LENGTH   @ZA07133
         AGO   .CREG1D                                         @ZA07133
.SPNULL1 DC    AL1(0)                            SUBPOOL
.CREG1C  DC    BL1'0&RL&VA&VB.0&BND&UNCND.0'     MODE BYTE *MVS380*
         AIF   (&LVREG).CREG1E                                 @ZA07133
         L     0,*-8+2*&B                        LOAD LENGTH
.CREG1D  AIF   (&KEYRG OR &SPREG).KORSREG
.*       NEITHER KEY OR SP IS A REGISTER.
         L     &PRG.,&GNAME.F                    LOAD GETMAIN PARMS
         AGO   .LVCHK
.*       EITHER KEY OR SP IS A REGISTER.
.KORSREG AIF   (&KEYRG AND &SPREG).BOTHREG
.*       ONLY ONE OF THEM IS A REGISTER
         AIF   (NOT &SPREG).KEYREG
.*       ONLY SP IS A REGISTER
         AIF   ('&SP(1)' EQ '&PRG').SPINPRG
.*       SP IS NOT IN THE PREFERRED PARM REG.
         LR    &PRG.,&SP(1)                      OBTAIN SUBPOOL ID
.SPINPRG SLL   &PRG.,8(0)                 MOVE SUBPOOL TO BYTE 2 YM1995
         ICM   &PRG.,13,&GNAME.F                 ADD REMAINING PARMS
         AGO   .LVCHK
.* ONLY KEY IS A REGISTER
.KEYREG  AIF   ('&KEY(1)' EQ '&PRG').KYINPRG
.*       KEY IS NOT IN THE PREFERRED REGISTER
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.KYINPRG SLL   &PRG.,16(0)                   SHIFT KEY TO BYTE 1 YM1995
         ICM   &PRG.,11,&GNAME.F                 ADD REMAINING PARMS
         AGO      .LVCHK
.*       BOTH KEY AND SP ARE IN REGISTERS
.BOTHREG AIF   ('&KEY(1)' NE '&SP(1)').NOTSAME
.*       BOTH KEY AND SP ARE IN THE SAME REGISTER.
         AIF   ('&KEY(1)' EQ '&PRG').BOTHPRG
.*       THE COMMON REGISTER IS NOT THE PREFERRED PARM REGISTER.
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.BOTHPRG ICM   &PRG.,9,&GNAME.F                  ADD REMAINING PARMS
         AGO   .LVCHK
.* BOTH ARE IN REGISTERS, BUT THEY ARE DIFFERENT REGISTERS.
.NOTSAME AIF   ('&KEY(1)' EQ '&PRG' OR '&SP(1)' EQ '&PRG').PRGIS1
.*       NEITHER REGISTER IS THE PREFERRED PARM REGISTER.
         LR    &PRG.,&KEY(1)                     GET STORAGE KEY
.ADDSP   SLL   &PRG.,8(0)                  SHIFT KEY OVER FOR SP YM1995
         OR    &PRG.,&SP(1)                      ADD SUBPOOL NUMBER
         SLL   &PRG.,8(0)                 MOVE PAIR TO BYTES 1-2 YM1995
         AGO   .BOTHPRG
.PRGIS1  AIF   ('&KEY(1)' EQ '&PRG').ADDSP
.*       SP IN IN THE PREFERRED PARM REGISTER.
         SLL   &PRG.,8(0)                 MOVE SUBPOOL TO BYTE 2 YM1995
         SLL   &KEY(1),16(0)                 SHIFT KEY TO BYTE 1 YM1995
         OR    &PRG.,&KEY(1)                     COMBINE KEY & SP
         AGO   .BOTHPRG
.LVCHK   ANOP                                                  @ZA07133
.FINI    AIF   ('&MODE' NE 'RC' AND '&MODE' NE 'RU').FINI1
         SR    1,1                               ZERO RESERVED REG 1
.FINI1   AIF   (&BR).SETBE                    TEST FOR BRANCH=YES
         SVC   &SVCN                             ISSUE GETMAIN SVC
         MEXIT
.SETBE   L     15,CVTPTR(0,0)                    LOAD THE CVT ADDRESS
         AIF   ('&MODE' EQ 'P').CBBE                           @Z30EN9G
         AIF   (&SVCN EQ 120).CRBE
         AIF   (&SVCN EQ 10).RMBE
         L     15,CVTGMBR-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.CBBE    ANOP                                                  @Z30EN9G
         L     15,CVTCBBR-CVTMAP(0,15)  GETMAIN ENTRY ADDRESS  @Z30EN9G
         AGO   .SBE                                            @Z30EN9G
.RMBE    L     15,CVTRMBR-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.CRBE    AIF   (&GLBR).GLBE   GLOBAL BRANCH ENTRY?
         L     15,CVTCRMN-CVTMAP(0,15)           GETMAIN ENTRY ADDRESS
         AGO   .SBE
.GLBE    L     4,CVTSPSA-CVTMAP(0,15)            SAVE AREA VECTOR
         L     4,WSAGGMFM-WSAG(0,4)              GLOBAL SAVE AREA ADDR
         L     15,CVTGLMN-CVTMAP(0,15)           GLBRANCH ENTRY ADDR
.SBE     BALR  14,15                             BRANCH TO GETMAIN
         MEXIT
.ERROR0  IHBERMAC 92
         MEXIT
.ERROR1  IHBERMAC 17
         MEXIT
.ERROR2  IHBERMAC 1001,MF,&MF
         MEXIT
.ERROR3  IHBERMAC 69
         MEXIT
.ERROR4  IHBERMAC 89
         MEXIT
.ERROR5  IHBERMAC 91
         MEXIT
.ERROR6  IHBERMAC 90
         MEXIT
.ERROR7  IHBERMAC 1001,MODE,&MODE
         MEXIT
.ERROR8  IHBERMAC 01
         MEXIT
.ERROR9  IHBERMAC 93
         MEXIT
.ERROR10 IHBERMAC 1014,BNDRY
         MEXIT
.ERR10A  IHBERMAC 1020,&BNDRY,&MODE
         MEXIT
.ERRORA  IHBERMAC 1020,KEY,&MODE
         MEXIT
.ERRORB  IHBERMAC 1001,KEY,&KEY
         MEXIT
.ERRORC  IHBERMAC 1020,BRANCH,''MF=L''
         MEXIT
.ERRORD  IHBERMAC 1001,BRANCH,&BRANCH
         MEXIT
.ERRORE  IHBERMAC 1020,KEY,''BRANCH=''&BRANCH
         MEXIT
.ERRORF  IHBERMAC 1020,&BRANCH(2),&MODE
         MEND
* */
* GETMAIN: MACRO KEYS(LV,LA,A,SP,BNDRY,BRANCH,MF,RTCD,KEY,RELATED);
*          ANS('?'³³MACLABEL³³'GETMAINP '³³MACLIST³³MACKEYS³³';');
*%     END GETMAIN;
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60033)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60033)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60033)
        DIS(WRITE)
        .
/*
//
