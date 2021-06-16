//SYS2MAC  JOB (TSO),
//             'Install SYS2 MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYS2.MACLIB
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.MACLIB,DISP=(NEW,CATLG),
//             VOL=SER=MVS000,
//             UNIT=3350,SPACE=(TRK,(44,14,17)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=$$DOC    0101-21120-21120-1735-00027-00011-00000-HERC01  28
SYS2.MACLIB
===========

ORIGINAL MACLIB PART OF TK4- UPDATE 9
TK4- UPDATE BY JUERGEN WINKELMANN WINKELMANN@ID.ETHZ.CH
OFFLOADED BY PHILIP YOUNG

LICENSE:
========

FROM MVS_TK4-_V1.00_USERS_MANUAL.PDF

THE MVS 3.8J TUR(N)KEY 4- DISTRIBUTION ITSELF IS PUT INTO THE PUBLIC
DOMAIN WITHOUT CLAIMING ANY COPYRIGHT BY THE AUTHOR AS FAR AS NO THIRD
PARTY COPYRIGHTS ARE AFFECTED.

JCL USED TO OFFLOAD:
====================

```JCL
//OFFLOAD2 JOB (001),'OFFLOAD PDS',CLASS=A,MSGCLASS=H
//OFFLOAD EXEC PGM=OFFLOAD,REGION=256K
//SYSPRINT DD SYSOUT=*
//PDS      DD DSN=SYS2.MACLIB,DISP=SHR
//SEQ      DD  DSN=SYS2.MACLIB.UNLOAD,
//             UNIT=SYSALLDA,VOL=SER=DEV01,
//             SPACE=(TRK,(44,14),RLSE),
//             DISP=(NEW,CATLG,DELETE)
//SYSIN    DD *
  O I=PDS,O=SEQ,T=IEBUPDTE
//
```

MACROS
======


$ATTR
-----

Generates 3270 attribute bytes

$EPILOG
======

Typical HLASM return routine

$ESC
===

3270 ESCape sequence

$GLOBALS
=======

Sets 3270 globals

$IC
==

Generates 3270 code for INSERT CURSOR order

$IO
==

Generate IO command for 3270 device

$MODEL
=====

Set model number for 3270 macros

$PROLOG
======

This macro will provide entry linkage and optionally multiple base
registers. Also via the `LV=` keyword provide additional user storage
(appended to the save area) addressable from register 13. If no
operands are coded register 12 is assumed from the base. Example:

```hlasm
SECTNAME $PROLOG          = STANDARD REG 12 BASE
SECTNAME $PROLOG 5        = STANDARD, REG 5 BASE
SECTNAME $PROLOG 10,LV=20 = ADD 20 BYTES TO SAVE AREA
                                      REG 10 IS BASE
SECTNAME $PROLOG R10,R11  = REGS 10 AND 11 ARE BASES
```

$RA
==

Generates 3270 Repeat Until Address order

$REGS
====

Equates R0-R15 and REG0-REG15


$SBA
===

Generates 3270 Set Buffer Address Order

$SF
==

Generates 3270 Start Field order (with attribute)

$STCK
====

this macro will provide the day, date, and time from  the time-of-day
clock in gregorian (english) format. if invoked without the optional
'nogen' keyword, the  constants named below will be generated.  if the
'nogen' keyword is used, the user must provide this   routine with
addressability to them as pre-defined constants.

```
DAY    DS   X      A BINARY NUMBER (HEX) RELATIVE TO
                   THE DAY OF THE WEEK AS FOLLOWS
                   0=MONDAY, 1=TUESDAY, 2=WEDNESDAY,
                   3=THURSDAY, 4=FRIDAY,
                   5=SATURDAY, 6=SUNDAY

DATE   DS   CL8    AN EIGHT CHARACTER FIELD CONTAINING
                   THE DATE IN MM/DD/YY FORMAT

TIME   DS   CL8    AN EIGHT CHARACTER FIELD CONTAINING
                   THE TIME IN HH:MM:SS FORMAT
```

note that the caller must provide a register save area that begins on a
doubleword boundary to be used as a work area by this routine
(r-13 based).

$TITLE
=====

Sets title?

$WCC
===

Generates 3270 write control character

@
=

MACRO to create block letters in assembly listing

BLANK
====

Fills an area with chars

BOX
==

Makes a box

BSPAUTH
======

Authorizes a program using SVC244

BSPBEG
=====

BSP header macro

BSPEND
=====

Typical HLASM return routine but BSP


BSPENTER
=======

Provide entry coding and house keeping for assembler modules

BSPGLBLS
=======

Declares globals &BSPAUTH, &BSPCSCT, &BSPMOD ,&BSPPRFX, &BSPPRGM,
&BSPVER, &BSPASVC

BSPPATCH
=======

To reserve 5% or 25 half words in a module for maintenance

BSPRET
=====

???????

BSPSGLBL
=======

Sets the globals listed in BSPGLBLS


DA
=

Data set allocation macro

DBGMSG
=====

Generate debug messages

DDTBRK
=====

?????

DIAG
===

??????

DO
=

Macro for structured programming

ELSE
===

Else: macro for structured programming

ELSEIF
=====

Elseif macro for structured programming

ENDDO
====

Enddo: close a do group in structured programming

ENDIF
====

Endif: macro  close current if level

ENTER
====

?????????

EXIT
===

Exit macro for structured programming

FC
=

Fortune Coookie macro

FILL
===

Fills an area with ' ' (space)

FINISH
=====

????????

ICATCHER
=======

May be used to generate CSECT Cards and standard format module
eye-catchers.  Both the CSECT and module eye-catchers are
optional.  See operand descriptions and examples below.

IEECODES
=======

????????

IEZBITS
======

Sets `BIT0`-`BIT7` with numerical value

IEZREGS
======

Equates R0-R15

IF
=

Starts a new if level for structured programming

IFERR
====

Error messages for structured programming-macros

IFGLO
====

GLOBALS FOR MACROS FOR STRUCTURED PROGRAMMING

IFPRO
====

PROCESSES CONDITION STATEMENTS IN STUCTURED PROGRAMMING

IHAIQE
=====

????????

IHAPIE
=====

A PIE IS CREATED AFTER A PROGRAM CHECK HAS OCCURRED IF THERE
IS A USER-SPECIFIED EXIT ROUTINE TO HANDLE PROGRAM CHECK
INTERRUPTIONS.  A PIE IS USED TO PASS THE NECESSARY DATA TO
THE USER-SPECIFIED EXIT ROUTINE.

MODULEID
=======

May be used to generate CSECT Cards and standard format module
eye-catchers.  Both the CSECT and module eye-catchers are
optional.

MSGPUT
=====

Puts message somewhere?

QTPUT
====

QUICK FORM OF TPUT TERMINAL INTERFACE ROUTINE

REGEQU
=====

Equates R0-R15

REGISTER
=======

Equates R0-R15

RRTEND
=====

????????

RRTENT
=====

????????

RRTTAB
=====

????????

SETMAXCC
=======

Sets max return code

STR$CLN
======

STRING Macro Instruction for Assembler XF with all the JCL from
STR$GSF removed

STR$GSF
======

STRING Macro Instruction for Assembler XF but with JCL and documenation


STRING
======

Alias of STR$CLN

SVC34
====

Use SVC34 to issue a command

SYSGET
=====

????????

SYSPRINT
=======

????????

SYSPUT
=====

????????

TESTENV
======

This program will try to determine the current runtime environment
by scanning MVS control blocks

X2CHRTAB
=======

????????

X2CHRTRN
=======

????????

YREGS
====

Equates R0-R15

./ ADD NAME=$ATTR
         MACRO
         $ATTR  &ATTR
.**********************************************************************
.*
.*  Name:  $ATTR
.*
.*  Type:  Assembler Macro
.*
.*  Desc:  Generates 3270 attribute bytes
.*
.*  Syntax   : $ATTR (list of attributes) Can be:
.*                    (PROT,NUM,MDT,HI,NONDISP,SKIP)
.*                    (UNPROT,ALPHA,NOMDT)
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLA  &I1,&BIT(8)
&I1      SETA  1
.LOOP    ANOP
.*   PROCESS PROT ATTRIBUTE
         AIF ('&ATTR(&I1)' NE 'PROT').UNPROT
&BIT(3)  SETA  1
         AGO    .NXTONE
.*   PROCESS UNPROT ATTRIBUTE
.UNPROT  AIF ('&ATTR(&I1)' NE 'UNPROT').NUM
&BIT(3)  SETA  0
         AGO    .NXTONE
.*   PROCESS NUM ATTRIBUTE
.NUM     AIF ('&ATTR(&I1)' NE 'NUM').ALPHA
&BIT(4)  SETA  1
         AGO    .NXTONE
.*   PROCESS ALPHA ATTRIBUTE
.ALPHA   AIF ('&ATTR(&I1)' NE 'ALPHA').SKIP
&BIT(4)  SETA  0
         AGO    .NXTONE
.*   PROCESS SKIP ATTRIBUTE
.SKIP    AIF ('&ATTR(&I1)' NE 'SKIP').NONDISP
&BIT(3)  SETA  1
&BIT(4)  SETA  1
         AGO    .NXTONE
.*   PROCESS NONDISP ATTRIBUTE
.NONDISP AIF ('&ATTR(&I1)' NE 'NONDISP').HI
&BIT(5)  SETA  1
&BIT(6)  SETA  1
         AGO    .NXTONE
.*   PROCESS HI ATTRIBUTE
.HI      AIF ('&ATTR(&I1)' NE 'HI').MDT
&BIT(5)  SETA  1
         AGO    .NXTONE
.*   PROCESS MDT ATTRIBUTE
.MDT     AIF ('&ATTR(&I1)' NE 'MDT').NOMDT
&BIT(8)  SETA  1
         AGO    .NXTONE
.*   PROCESS NOMDT ATTRIBUTE
.NOMDT   AIF ('&ATTR(&I1)' NE 'NOMDT').ERROR
&BIT(8)  SETA  0
         AGO    .NXTONE
.ERROR   MNOTE 8,'&ATTR(&I1) IS AN INVALID ATTRIBUTE OPTION'
.NXTONE  ANOP
&I1      SETA  &I1+1
         AIF (&I1 LE N'&ATTR).LOOP
.*   CALCULATE VALUE OF OPTION BYTE
&I1      SETA &BIT(8)+&BIT(6)*4+&BIT(5)*8+&BIT(4)*16+&BIT(3)*32+1
         AIF (&I1 LE 64).GETCHAR
         MNOTE 8,'&I1 IS GREATE THAN 64'
&RC      SETA  4
         MEXIT
.*   LOOK UP CHAR FOR CALCULATED OPTION BYTE.
.GETCHAR ANOP
&ATTRIB  SETC  '&TAB3270(&I1)'
&RC      SETA  0
         MEND
./ ADD NAME=$EPILOG
         MACRO                                                          00000010
&LABEL   $EPILOG &RC                                                    00000020
&LABEL   LR    R1,R13              GET SAVEAREA ADDRESS                 00000030
         L     R13,4(R13)          GET BACK CHAIN POINTER               00000040
         L     R0,16(R13)          GET SAVEAREA LENGTH                  00000050
         ST    R15,16(R13)         SAVE REGISTER 15 (RETCODE)           00000060
         FREEMAIN R,LV=(0),A=(1)   FREE SAVEAREA                        00000070
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 00000080
         AIF   (T'&RC EQ 'O').SPEC                                      00000090
         LA    R15,&RC             SET RETURN CODE                      00000100
.SPEC    ANOP                                                           00000110
         BR    R14                 RETURN TO CALLER                     00000120
         MEND                                                           00000130
./ ADD NAME=$ESC
         MACRO
&LABEL   $ESC   &OPTIONS
.**********************************************************************
.*
.*  Name:   $ESC
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 Escape Character
.*
.*  Syntax: $ESC
.*
.**********************************************************************
&LABEL   DC    X'27'                        , ESCape character
         MEND
./ ADD NAME=$GLOBALS
         GBLC  &TAB3270(64),&ATTRIB
         GBLA  &RC,&MODEL
&MODEL      SETA  2
&TAB3270(1) SETC  '40'
&TAB3270(2) SETC  'C1'
&TAB3270(3) SETC  'C2'
&TAB3270(4) SETC  'C3'
&TAB3270(5) SETC  'C4'
&TAB3270(6) SETC  'C5'
&TAB3270(7) SETC  'C6'
&TAB3270(8) SETC  'C7'
&TAB3270(9) SETC  'C8'
&TAB3270(10) SETC 'C9'
&TAB3270(11) SETC '4A'
&TAB3270(12) SETC '4B'
&TAB3270(13) SETC '4C'
&TAB3270(14) SETC '4D'
&TAB3270(15) SETC '4E'
&TAB3270(16) SETC '4F'
&TAB3270(17) SETC '50'
&TAB3270(18) SETC 'D1'
&TAB3270(19) SETC 'D2'
&TAB3270(20) SETC 'D3'
&TAB3270(21) SETC 'D4'
&TAB3270(22) SETC 'D5'
&TAB3270(23) SETC 'D6'
&TAB3270(24) SETC 'D7'
&TAB3270(25) SETC 'D8'
&TAB3270(26) SETC 'D9'
&TAB3270(27) SETC '5A'
&TAB3270(28) SETC '5B'
&TAB3270(29) SETC '5C'
&TAB3270(30) SETC '5D'
&TAB3270(31) SETC '5E'
&TAB3270(32) SETC '5F'
&TAB3270(33) SETC '60'
&TAB3270(34) SETC '61'
&TAB3270(35) SETC 'E2'
&TAB3270(36) SETC 'E3'
&TAB3270(37) SETC 'E4'
&TAB3270(38) SETC 'E5'
&TAB3270(39) SETC 'E6'
&TAB3270(40) SETC 'E7'
&TAB3270(41) SETC 'E8'
&TAB3270(42) SETC 'E9'
&TAB3270(43) SETC '6A'
&TAB3270(44) SETC '6B'
&TAB3270(45) SETC '6C'
&TAB3270(46) SETC '6D'
&TAB3270(47) SETC '6E'
&TAB3270(48) SETC '6F'
&TAB3270(49) SETC 'F0'
&TAB3270(50) SETC 'F1'
&TAB3270(51) SETC 'F2'
&TAB3270(52) SETC 'F3'
&TAB3270(53) SETC 'F4'
&TAB3270(54) SETC 'F5'
&TAB3270(55) SETC 'F6'
&TAB3270(56) SETC 'F7'
&TAB3270(57) SETC 'F8'
&TAB3270(58) SETC 'F9'
&TAB3270(59) SETC '7A'
&TAB3270(60) SETC '7B'
&TAB3270(61) SETC '7C'
&TAB3270(62) SETC '7D'
&TAB3270(63) SETC '7E'
&TAB3270(64) SETC '7F'
./ ADD NAME=$IC
         MACRO
&LABEL   $IC
.**********************************************************************
.*
.*  Name:   $IC
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 code for INSERT CURSOR order
.*
.*  Syntax: $IC
.*
.**********************************************************************
         COPY  $GLOBALS
&LABEL   DC    X'13'               IC
         MEND
./ ADD NAME=$IO
         MACRO
&LABEL   $IO   &CMD
.**********************************************************************
.*
.*  Name:   $IO
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generate IO command for 3270 device
.*
.*  Syntax: $IO  WRITE        (,model-num)
.*               WSF          (,model-num)
.*               ERASE/WRITE  (,model-num)
.*
.*
.*
.*
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLC  &OPCODE,&COMMENT
.*   PROCESS WRITE COMMAND
         AIF ('&CMD' NE 'WRITE').WSF
&OPCODE  SETC  'F1'
         AGO    .DC
.*   PROCESS WRITE STRUCTURED FIELD COMMAND
.WSF     AIF ('&CMD' NE 'WSF').ERWRIT
&OPCODE  SETC  'F3'
         AGO    .DC
.*   PROCESS ERASE/WRITE COMMAND
.ERWRIT  AIF ('&CMD' NE 'ERASE/WRITE').ERROR
&OPCODE  SETC  'F5'
         AIF (&MODEL LE 2).DC
&OPCODE  SETC  '7E'
         AGO    .DC
.*   PROCESS INVALID COMMANDS
.ERROR   MNOTE 8,'&CMD IS AN INVALID 3270 COMMAND'
         MEXIT
.DC      ANOP
&COMMENT SETC '                &CMD FOR 3270 MODEL &MODEL'
&LABEL   DC    X'&OPCODE'&COMMENT
         MEND
./ ADD NAME=$MODEL
         MACRO
         $MODEL &NUM
.**********************************************************************
.*
.*  Name:   $MODEL
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Set model number for 3270 macoros
.*
.*  Syntax: $MODEL model-num
.*
.**********************************************************************
         COPY  $GLOBALS
         AIF   ((&NUM GE 2) AND (&NUM LE 4)).OK
         MNOTE 8,'3270 MODEL &MODEL IS NOT SUPPORTED BY THESE MACROS.'
         MEXIT
.OK      ANOP
&MODEL   SETA  &NUM
         MEND
./ ADD NAME=$PROLOG
         MACRO                                                          00000010
&LABEL   $PROLOG &LV=0                                                  00000020
.********************************************************************** 00000030
.*                                                                      00000040
.*       THIS MACRO WILL PROVIDE ENTRY LINKAGE AND OPTIONALLY           00000050
.*       MULTIPLE BASE REGISTERS.  ALSO, VIA THE 'LV=' KEYWORD          00000060
.*       PROVIDE ADDITIONAL USER STORAGE (APPENDED TO THE               00000070
.*       SAVE AREA) ADDRESSABLE FROM REG 13.  IF NO OPERANDS            00000080
.*       ARE CODED, REG 12 IS ASSUMED THE BASE. EXAMPLE:                00000090
.*              SECTNAME $PROLOG          = STANDARD REG 12 BASE        00000100
.*              SECTNAME $PROLOG 5        = STANDARD, REG 5 BASE        00000110
.*              SECTNAME $PROLOG 10,LV=20 = ADD 20 BYTES TO SAVE AREA   00000120
.*                                             REG 10 IS BASE           00000130
.*              SECTNAME $PROLOG R10,R11  = REGS 10 AND 11 ARE BASES    00000140
.*                                                                      00000150
.********************************************************************** 00000160
         LCLA  &AA,&AB,&AC                                              00000170
         GBLB  &PRORG                                                   00000180
&AC      SETA  4096                                                     00000190
&LABEL   CSECT                                                          00000200
         B     32(R15)             BRANCH AROUND                        00000210
         DC    AL1(26)                                                  00000220
         DC    CL8'&LABEL'         CSECT NAME                           00000230
         DC    C'-'                                                     00000240
         DC    CL8'&SYSDATE'       COMPILE DATE                         00000250
         DC    C'-'                                                     00000260
         DC    CL8'&SYSTIME'       COMPILE TIME                         00000270
         CNOP  0,4                 ALIGNMENT                            00000280
         STM   R14,R12,12(R13)     SAVE REGISTERS                       00000290
         LR    R12,R15             LOAD BASE REG                        00000300
         USING &LABEL,R12          INFORM ASSEMBLER                     00000310
         AIF   (&LV GT 4023).MERR                                       00000320
         LA    R0,&LV+72           LOAD REG 0 WITH LENGTH VARIABLE      00000330
         GETMAIN R,LV=(0)          GET CORE FOR SAVEAREA AND USER       00000340
         AIF   (&LV+72 LE 256).XC2                                      00000350
         AIF   (&LV+72 LE 512).XC1                                      00000360
         MVI   0(R1),X'00'         MOVE X'00' TO FIRST BYTE             00000370
         LR    R2,R1               SAVE POINTER IN EVEN REG             00000380
         LA    R4,1(R1)            SET RECEIVING POINTER                00000390
         LR    R5,R0               SET RECEIVING LENGTH                 00000400
         BCTR  R5,R0               DECREMENT LENGTH                     00000410
         LA    R5,0(R5)            CLEAR HIGH ORDER BYTE                00000420
         LA    R3,1                SET SENDING LENGTH                   00000430
         MVCL  R4,R2               INSTRUCTION PADS WITH X'00'          00000440
         AGO   .STORE                                                   00000450
.XC1     ANOP                                                           00000460
         XC    256(&LV-184,R1),256(R1)  CLEAR SAVE AREA                 00000470
         XC    0(256,R1),0(R1)          CLEAR SAVE AREA                 00000480
         AGO   .STORE                                                   00000490
.XC2     ANOP                                                           00000500
         XC    0(&LV+72,R1),0(R1)       CLEAR SAVE AREA                 00000510
.STORE   ANOP                                                           00000520
         ST    R13,4(R1)           SAVE BACK CHAIN                      00000530
         ST    R1,8(R13)           SET FORWARD CHAIN                    00000540
         LR    R11,R1              SAVE NEW SAVEAREA ADDRESS            00000550
         L     R15,16(R13)         RESTORE REG 15                       00000560
         ST    R0,16(R13)          SAVE SAVEAREA LENGTH                 00000570
         LM    R0,R1,20(R13)       RESTORE REGS USED IN GETMAIN         00000580
         LR    R13,R11             SET SAVEAREA POINTER                 00000590
         AIF   (N'&SYSLIST EQ 0).MEND                                   00000600
         AIF   ('&SYSLIST(1)' EQ 'R12').SKIPIT                          00000610
         AIF   ('&SYSLIST(1)' EQ '12').SKIPIT                           00000620
         LA    &SYSLIST(1),&LABEL  LOAD REQUESTED BASE REG              00000630
         DROP  R12                 DROP ASSUMED BASE REG                00000640
         USING &LABEL,&SYSLIST(1)  INFORM ASSEMBLER                     00000650
.SKIPIT  ANOP                                                           00000660
&AA      SETA  2                                                        00000670
.LOOP    ANOP                                                           00000680
         AIF   (&AA GT N'&SYSLIST).MEXIT                                00000690
&AB      SETA  &AA-1                                                    00000700
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AB))  LOAD NEXT BASE REG    00000710
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AA))  LOAD NEXT BASE REG    00000720
         USING &LABEL+&AC,&SYSLIST(&AA) INFORM ASSEMBLER                00000730
&AC      SETA  &AC+4096                                                 00000740
&AA      SETA  &AA+1                                                    00000750
         AGO   .LOOP                                                    00000760
.MEXIT   ANOP                                                           00000770
         AIF   (&PRORG).MEX2                                            00000780
         SPACE                                                          00000790
         $REGS                                                          00000800
         SPACE                                                          00000810
.MEX2    ANOP                                                           00000820
&AA      SETA  &LV+72                                                   00000830
         MNOTE *,'TOTAL STORAGE AREA RECEIVED = &AA'                    00000840
         MEXIT                                                          00000850
.MEND    ANOP                                                           00000860
         MNOTE *,'NO REGISTER SPECIFIED - R12 ASSUMED'                  00000870
         AGO   .MEXIT                                                   00000880
.MERR    ANOP                                                           00000890
         MNOTE 12,'LV > 4023 - REQUEST IGNORED'                         00000900
         AGO   .MEXIT                                                   00000910
         MEND                                                           00000920
./ ADD NAME=$RA
         MACRO
&LABEL   $RA   &OPTIONS
.**********************************************************************
.*
.*  Name:   $RA
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 Repeat Until Address order
.*
.*  Syntax: $RA  (row,col,char)
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLA  &ROW,&COLUMN,&BUFFLOC,&I1,&I2
         LCLC  &COMMENT,&CHAR
&ROW     SETA  &OPTIONS(1)
&COLUMN  SETA  &OPTIONS(2)
&CHAR    SETC  '&OPTIONS(3)'
.*  CALC OFFSET IN 3270 BUFFER
&BUFFLOC SETA  ((&ROW-1)*80)+(&COLUMN-1)
.*  CONVERT 12 BIT ADDR TO TWO 6 BIT ADDRS
&I1      SETA  &BUFFLOC/64
&I2      SETA  &BUFFLOC-(&I1*64)
.*  CHANGE INDICES INTO 3270 CONVERSION TABLE FROM 0-BASED TO 1-BASED
&I1      SETA  &I1+1
&I2      SETA  &I2+1
&COMMENT SETC  '    RA &OPTIONS'
&LABEL   DC    XL3'3C&TAB3270(&I1)&TAB3270(&I2)',&CHAR&COMMENT
         MEND
./ ADD NAME=$REGS
         MACRO                                                          00000010
         $REGS                                                          00000020
         GBLB  &PRORG                                                   00000030
         AIF   (&PRORG).MEX2                                            00000040
&PRORG   SETB  1                                                        00000050
 SPACE                                                                  00000060
R0       EQU   0                                                        00000070
R1       EQU   1                                                        00000080
R2       EQU   2                                                        00000090
R3       EQU   3                                                        00000100
R4       EQU   4                                                        00000110
R5       EQU   5                                                        00000120
R6       EQU   6                                                        00000130
R7       EQU   7                                                        00000140
R8       EQU   8                                                        00000150
R9       EQU   9                                                        00000160
R10      EQU   10                                                       00000170
R11      EQU   11                                                       00000180
R12      EQU   12                                                       00000190
R13      EQU   13                                                       00000200
R14      EQU   14                                                       00000210
R15      EQU   15                                                       00000220
 SPACE                                                                  00000230
REG0     EQU   0                                                        00000240
REG1     EQU   1                                                        00000250
REG2     EQU   2                                                        00000260
REG3     EQU   3                                                        00000270
REG4     EQU   4                                                        00000280
REG5     EQU   5                                                        00000290
REG6     EQU   6                                                        00000300
REG7     EQU   7                                                        00000310
REG8     EQU   8                                                        00000320
REG9     EQU   9                                                        00000330
REG10    EQU   10                                                       00000340
REG11    EQU   11                                                       00000350
REG12    EQU   12                                                       00000360
REG13    EQU   13                                                       00000370
REG14    EQU   14                                                       00000380
REG15    EQU   15                                                       00000390
 SPACE                                                                  00000400
.MEX2    ANOP                                                           00000410
       MEND                                                             00000420
./ ADD NAME=$SBA
         MACRO
&LABEL   $SBA  &ADDR
.**********************************************************************
.*
.*  Name:   $SBA
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 Set Buffer Address Order
.*
.*  Syntax: $SBA (row,col)
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLA  &ROW,&COLUMN,&BUFFLOC,&I1,&I2
         LCLC  &COMMENT
&ROW     SETA  &ADDR(1)
&COLUMN  SETA  &ADDR(2)
.*  CALC OFFSET IN 3270 BUFFER
&BUFFLOC SETA  ((&ROW-1)*80)+(&COLUMN-1)
.*  CONVERT 12 BIT ADDR TO TWO 6 BIT ADDRS
&I1      SETA  &BUFFLOC/64
&I2      SETA  &BUFFLOC-(&I1*64)
.*  CHANGE INDICES INTO 3270 CONVERSION TABLE FROM 0-BASED TO 1-BASED
&I1      SETA  &I1+1
&I2      SETA  &I2+1
&COMMENT SETC  '          SBA &ADDR'
&LABEL   DC    XL3'11&TAB3270(&I1)&TAB3270(&I2)'&COMMENT
         MEND
./ ADD NAME=$SF
         MACRO
&LABEL   $SF    &ATTR
.**********************************************************************
.*
.*  Name:   $SF
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 Start Field order (with attribute)
.*
.*  Syntax: $SF  attribute-descriptors
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLC  &COMMENT
.*   PROCESS ATTRIBUTE OPTIONS
         AIF (N'&ATTR  EQ 0).NOATTR
         $ATTR &ATTR
         AIF (&RC EQ 0).DC
.* USE DEFAULT ATTRIBUTE BYTE
.NOATTR  ANOP
&ATTRIB  SETC  '40'
.DC      ANOP
&COMMENT SETC  '              SF &ATTR'
&LABEL   DC    X'1D&ATTRIB'&COMMENT
         MEND
./ ADD NAME=$STCK
         MACRO                                                          00000010
&LABEL   $STCK  &NOGEN                                                  00000020
         LCLC   &L                                                      00000030
.********************************************************************** 00000040
.****                                                             ***** 00000050
.****    THIS MACRO WILL PROVIDE THE DAY, DATE, AND TIME FROM     ***** 00000060
.****    THE TIME-OF-DAY CLOCK IN GREGORIAN (ENGLISH) FORMAT.     ***** 00000070
.****                                                             ***** 00000080
.****    IF INVOKED WITHOUT THE OPTIONAL 'NOGEN' KEYWORD, THE     ***** 00000090
.****    CONSTANTS NAMED BELOW WILL BE GENERATED.  IF THE         ***** 00000100
.****    'NOGEN' KEYWORD IS USED, THE USER MUST PROVIDE THIS      ***** 00000110
.****    ROUTINE WITH ADDRESSABILITY TO THEM AS PRE-DEFINED       ***** 00000120
.****    CONSTANTS.                                               ***** 00000130
.****                                                             ***** 00000140
.****    DAY    DS   X      A BINARY NUMBER (HEX) RELATIVE TO     ***** 00000150
.****                       THE DAY OF THE WEEK AS FOLLOWS        ***** 00000160
.****                       0=MONDAY, 1=TUESDAY, 2=WEDNESDAY,     ***** 00000170
.****                       3=THURSDAY, 4=FRIDAY,                 ***** 00000180
.****                       5=SATURDAY, 6=SUNDAY                  ***** 00000190
.****                                                             ***** 00000200
.****    DATE   DS   CL8    AN EIGHT CHARACTER FIELD CONTAINING   ***** 00000210
.****                       THE DATE IN MM/DD/YY FORMAT           ***** 00000220
.****                                                             ***** 00000230
.****    TIME   DS   CL8    AN EIGHT CHARACTER FIELD CONTAINING   ***** 00000240
.****                       THE TIME IN HH:MM:SS FORMAT           ***** 00000250
.****                                                             ***** 00000260
.****    NOTE THAT THE CALLER MUST PROVIDE A REGISTER SAVE AREA   ***** 00000270
.****    THAT BEGINS ON A DOUBLEWORD BOUNDARY TO BE USED AS A     ***** 00000280
.****    WORK AREA BY THIS ROUTINE (R-13 BASED).                  ***** 00000290
.****                                                             ***** 00000300
.********************************************************************** 00000310
&L       SETC  '&SYSNDX'                                                00000320
&LABEL   STCK  8(13)              STORE THE TIME-OF-DAY CLOCK           00000330
         LM    0,1,8(13)          LOAD INTO WORK REGS                   00000340
         L     15,16              GET CVT ADDRESS                       00000350
         A     0,304(15)          ADD LOCAL TIME DEVIATION              00000360
         SRDL  0,12               ISOLATE NUMBER OF MICROSECONDS        00000370
         D     0,=F'60000000'     DIVIDE BY 60M (R1=MINUTES AFT EPOCH)  00000380
         LR    15,0               COPY REMAINDER OF MICS TO GET SECS    00000390
         SR    14,14              CLEAR FOR DIVIDE                      00000400
         D     14,=F'951424'      DIVIDE TO GET REMAINING SECONDS (R5)  00000410
         LR    14,15              COPY TO WORK REG                      00000420
S&L.B    SL    14,=F'60'          DECREMENT BY 60 SECONDS               00000430
         BM    S&L.S              LESS THAN SIXTY, CONTINUE             00000440
         SL    15,=F'60'          MORE THAN SIXTY, ADJUST FOR LEAP      00000450
         AL    1,=F'1'            BUMP MINUTES                          00000460
         B     S&L.B              CHECK AGAIN                           00000470
S&L.S    CVD   15,8(13)           CONVERT SECONDS TO PACKED FORMAT      00000480
         UNPK  24(4,13),14(2,13)  UNPACK SECONDS FOR PRINT              00000490
         OI    27(13),X'F0'       SET UP FOR PRINTING                   00000500
         MVC   TIME+6(2),26(13)   MOVE THE SECONDS                      00000510
         CVD   1,8(13)            CONVERT MINUTES TO PACKED FORMAT      00000520
         DP    8(8,13),=P'60'     DIVIDE INTO HOURS AND MINUTES         00000530
         UNPK  24(4,13),14(2,13)  UNPACK THE MINUTES                    00000540
         OI    27(13),X'F0'       SET UP FOR PRINTING                   00000550
         MVC   TIME+3(2),26(13)   MOVE THE MINUTES                      00000560
         ZAP   8(8,13),8(6,13)    RESET TO FULL LENGTH                  00000570
         DP    8(8,13),=P'24'     DIVIDE INTO DAYS AND HOURS            00000580
         UNPK  24(4,13),14(2,13)  UNPACK THE HOURS                      00000590
         OI    27(13),X'F0'       SET UP FOR PRINTING                   00000600
         MVC   TIME(2),26(13)     MOVE THE HOURS                        00000610
         MVI   TIME+2,C':'        ADD COLIN                             00000620
         MVI   TIME+5,C':'        ADD COLIN                             00000630
         ZAP   8(8,13),8(6,13)    RESET TO FULL LENGTH                  00000640
         DP    8(8,13),=P'7'      DIVIDE BY NUMBER OF DAYS IN A WEEK    00000650
         ZAP   8(8,13),15(1,13)   FILL DOUBLEW WITH THE REMAINDER       00000660
         CVB   0,8(13)            CONVERT RELATIVE DAY TO BINARY        00000670
         STC   0,DAY              SET RELATIVE DAY OF WEEK              00000680
         ZAP   16(8,13),8(8,13)   SAVE THE NUMBER OF DAYS               00000690
         L     1,16               GET CVT ADDRESS                       00000700
         MVC   24(4,13),57(1)     MOVE CVT DATE TO WORK AREA            00000710
         ZAP   8(8,13),=P'0'      ZERO WORK AREA                        00000720
         MVO   14(2,13),24(1,13)  MOVE YEAR PORTION                     00000730
         UNPK  DATE+6(2),14(2,13) UNPACK INTO OUTPUT AREA               00000740
         LA    15,S&L.T           LOAD ADDR OF TABLE                    00000750
         CVB   1,8(13)            CONVERT TO BIN                        00000760
         SLL   1,30               SHIFT ALL BUT LAST TWO                00000770
         LTR   1,1                TEST RESIDUAL                         00000780
         BNZ   *+8                NOT 0, BRANCH                         00000790
         LA    15,S&L.TL          LEAP YEAR                             00000800
         LR    14,15              SAVE TABLE IN USE                     00000810
         ZAP   8(8,13),25(2,13)   MOVE DAY PORTION                      00000820
         CVB   0,8(13)            CONVERT TO BIN                        00000830
         SR    1,1                CLEAR REG                             00000840
         IC    1,0(15)            LOAD CHAR FROM TABLE                  00000850
         LA    15,1(15)           INDEX TABLE POINTER                   00000860
         SR    0,1                SUBTRACT FROM WORK VALUE              00000870
         BP    *-10               STILL POSITIVE, DO AGAIN              00000880
         AR    0,1                ELSE, ADD IT BACK                     00000890
         CVD   0,8(13)            CONVERT TO DEC                        00000900
         UNPK  DATE+3(2),14(2,13) UNPACK DAY INTO OUTPUT                00000910
         SR    15,14              FIND VALUE OF CURRENT INDEX           00000920
         CVD   15,8(13)           CONVERT TO DEC                        00000930
         UNPK  DATE(2),14(2,13)   UNPACK MONTH INTO OUTPUT              00000940
         OI    DATE+1,C'0'        INSURE NUMERICS                       00000950
         OI    DATE+4,C'0'        INSURE NUMERICS                       00000960
         OI    DATE+7,C'0'        INSURE NUMERICS                       00000970
         MVI   DATE+2,C'/'        ADD SLASH                             00000980
         MVI   DATE+5,C'/'        ADD SLASH                             00000990
         B     S&L.E              BRANCH AROUND CONSTANTS               00001000
S&L.T    DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31) MONTH TABLE     00001010
S&L.TL   DC    AL1(31,29,31,30,31,30,31,31,30,31,30,31) LEAP YEAR       00001020
         AIF    (T'&NOGEN NE 'O').NOGEN   NOGEN SPECIFIED, SKIP         00001030
DAY      DC    XL1'00'            RELATIVE DAY OF THE WEEK              00001040
DATE     DC    C'  /  /  '        CURRENT DATE                          00001050
TIME     DC    C'  :  :  '        CURRENT TIME                          00001060
.NOGEN   ANOP                                                           00001070
S&L.E    DS    0H                                                       00001080
         MEND                                                           00001090
./ ADD NAME=$TITLE
         MACRO
&LABEL   $TITLE &ID=,&TITLE=,&HEAD=,&HDR=
         GBLB  &BOXID
         GBLC  &BOXHDR
         LCLA  &I,&J,&N
         LCLB  &BCTR
         LCLC  &BID,&BTITLE
.*
.*       PICK UP ID FIELD
.*
.TESTID  AIF   ('&ID' EQ '' AND '&LABEL' EQ '').TESTHDR
         AIF   ('&ID' EQ '' OR '&LABEL' EQ '').TESTPRV
         MNOTE *,'BOX001: ID SPECIFIED TWICE - LABEL FIELD IGNORED'
.TESTPRV AIF   (NOT &BOXID).SETID
         MNOTE *,'BOX002: ID ALREADY SPECIFIED - THIS ONE IGNORED'
         AGO   .TESTHDR
.SETID   ANOP
&BOXID   SETB  1
&BID     SETC  '&ID'
         AIF   ('&ID' NE '').TESTHDR
&BID     SETC  '&LABEL'
         @     &BID
.*
.*       PICK UP HEADER SPECIFICATION
.*
.TESTHDR AIF   (T'&HDR EQ 'O' AND T'&HEAD EQ 'O').TESTTTL
&BOXHDR  SETC  '&HDR&HEAD'
         AIF   ('&BOXHDR'(1,1) NE '''').TESTTTL
&BOXHDR  SETC  '&BOXHDR'(2,K'&BOXHDR-2)
.*
.*       PICK UP TITLE SPECIFICATION
.*
.TESTTTL AIF   (T'&TITLE EQ 'O').MEXIT
&BTITLE  SETC  '&TITLE'
         AIF   ('&BTITLE'(1,1) NE '''').GENTTL
&BTITLE  SETC  '&BTITLE'(2,K'&BTITLE-2)
.GENTTL  ANOP
&BID     TITLE '&BOXHDR&BTITLE'
.MEXIT   MEXIT
         MEND
./ ADD NAME=$WCC
         MACRO
&LABEL   $WCC   &OPTIONS
.**********************************************************************
.*
.*  Name:   $WCC
.*
.*  Type:   Assembler Macro
.*
.*  Desc:   Generates 3270 write control character
.*
.*  Syntax: $WCC (options)        can be ALARM
.*                                       RESETKBD
.*                                       MDT
.*
.**********************************************************************
         COPY  $GLOBALS
         LCLA  &I1,&BIT(8)
         LCLC  &COMMENT
         AIF   (N'&OPTIONS EQ 0).DEFAULT
&I1      SETA  1
.LOOP    ANOP
.*   PROCESS ALARM ATTRIBUTE
         AIF ('&OPTIONS(&I1)' NE 'ALARM').RESETKB
&BIT(6)  SETA  1
         AGO    .NXTONE
.*   PROCESS RESETKBD ATTRIBUTE
.RESETKB AIF ('&OPTIONS(&I1)' NE 'RESETKBD').MDT
&BIT(7)  SETA  1
         AGO    .NXTONE
.*   PROCESS MDT ATTRIBUTE
.MDT     AIF ('&OPTIONS(&I1)' NE 'MDT').ERROR
&BIT(8)  SETA  1
         AGO    .NXTONE
.*   PROCESS INVALID ATTRIBUTES
.ERROR   MNOTE 8,'&OPTIONS(&I1) IS INVALID FOR WCC'
.NXTONE  ANOP
&I1      SETA  &I1+1
         AIF (&I1 LE N'&OPTIONS).LOOP
.*   CALCULATE VALUE OF OPTION BYTE PLUS ONE
.DEFAULT ANOP
&I1      SETA &BIT(8)+&BIT(7)*2+&BIT(6)*4+1
         AIF (&I1 LE 64).GETCHAR
         MNOTE 8,'&I1 IS GREATE THAN 64'
         MEXIT
.*   LOOK UP CHAR FOR CALCULATED OPTION BYTE.
.GETCHAR ANOP
&COMMENT SETC '                WCC &OPTIONS'
&LABEL   DC    X'&TAB3270(&I1)'&COMMENT
         MEND
./ ADD NAME=@
         MACRO
         @     &LETTERS
.*********************************************************************
.*                                                                   *
.*       MACRO to create block letters in assembly listing           *
.*                                                                   *
.*********************************************************************
         LCLC  &A(50)
         LCLC  &B(62),&C(62),&D(62),&E(62),&F(62),&G(62)
         LCLA  &I,&J,&K
&B(01)   SETC  'A'
&B(02)   SETC  'B'
&B(03)   SETC  'C'
&B(04)   SETC  'D'
&B(05)   SETC  'E'
&B(06)   SETC  'F'
&B(07)   SETC  'G'
&B(08)   SETC  'H'
&B(09)   SETC  'I'
&B(10)   SETC  'J'
&B(11)   SETC  'K'
&B(12)   SETC  'L'
&B(13)   SETC  'M'
&B(14)   SETC  'N'
&B(15)   SETC  'O'
&B(16)   SETC  'P'
&B(17)   SETC  'Q'
&B(18)   SETC  'R'
&B(19)   SETC  'S'
&B(20)   SETC  'T'
&B(21)   SETC  'U'
&B(22)   SETC  'V'
&B(23)   SETC  'W'
&B(24)   SETC  'X'
&B(25)   SETC  'Y'
&B(26)   SETC  'Z'
&B(27)   SETC  '#'
&B(28)   SETC  ','
&B(29)   SETC  '$'
&B(30)   SETC  '0'
&B(31)   SETC  '1'
&B(32)   SETC  '2'
&B(33)   SETC  '3'
&B(34)   SETC  '4'
&B(35)   SETC  '5'
&B(36)   SETC  '6'
&B(37)   SETC  '7'
&B(38)   SETC  '8'
&B(39)   SETC  '9'
&B(40)   SETC  '@'
&B(41)   SETC  '%'
&B(42)   SETC  '*'
&B(43)   SETC  '/'
&B(44)   SETC  '-'
&B(45)   SETC  '('
&B(46)   SETC  ')'
&B(47)   SETC  '&&'
&B(48)   SETC  '+'
&B(49)   SETC  '_'
&B(50)   SETC  '['
&B(51)   SETC  ']'
&B(52)   SETC  '>'
&B(53)   SETC  ':'
&B(54)   SETC  ';'
&B(55)   SETC  '.'
&B(56)   SETC  '?'
&B(57)   SETC  '"'
&B(58)   SETC  '='
&B(59)   SETC  '!'
&B(60)   SETC  '<'
&B(61)   SETC  '^'
&B(62)   SETC  ' '
&C(01)   SETC  '  A  '
&D(01)   SETC  ' A A '
&E(01)   SETC  'A   A'
&F(01)   SETC  'AAAAA'
&G(01)   SETC  'A   A'
&C(02)   SETC  'BBBB '
&D(02)   SETC  'B   B'
&E(02)   SETC  'BBBB '
&F(02)   SETC  'B   B'
&G(02)   SETC  'BBBB '
&C(03)   SETC  ' CCCC'
&D(03)   SETC  'C    '
&E(03)   SETC  'C    '
&F(03)   SETC  'C    '
&G(03)   SETC  ' CCCC'
&C(04)   SETC  'DDDD '
&D(04)   SETC  'D   D'
&E(04)   SETC  'D   D'
&F(04)   SETC  'D   D'
&G(04)   SETC  'DDDD '
&C(05)   SETC  'EEEEE'
&D(05)   SETC  'E    '
&E(05)   SETC  'EEEE '
&F(05)   SETC  'E    '
&G(05)   SETC  'EEEEE'
&C(06)   SETC  'FFFFF'
&D(06)   SETC  'F    '
&E(06)   SETC  'FFFF '
&F(06)   SETC  'F    '
&G(06)   SETC  'F    '
&C(07)   SETC  ' GGGG'
&D(07)   SETC  'G    '
&E(07)   SETC  'G  GG'
&F(07)   SETC  'G   G'
&G(07)   SETC  ' GGGG'
&C(08)   SETC  'H   H'
&D(08)   SETC  'H   H'
&E(08)   SETC  'HHHHH'
&F(08)   SETC  'H   H'
&G(08)   SETC  'H   H'
&C(09)   SETC  ' III '
&D(09)   SETC  '  I  '
&E(09)   SETC  '  I  '
&F(09)   SETC  '  I  '
&G(09)   SETC  ' III '
&C(10)   SETC  '  JJJ'
&D(10)   SETC  '   J '
&E(10)   SETC  '   J '
&F(10)   SETC  'J  J '
&G(10)   SETC  ' JJ  '
&C(11)   SETC  'K  K '
&D(11)   SETC  'K K  '
&E(11)   SETC  'KK   '
&F(11)   SETC  'K K  '
&G(11)   SETC  'K  K '
&C(12)   SETC  'L    '
&D(12)   SETC  'L    '
&E(12)   SETC  'L    '
&F(12)   SETC  'L    '
&G(12)   SETC  'LLLLL'
&C(13)   SETC  'M   M'
&D(13)   SETC  'MM MM'
&E(13)   SETC  'M M M'
&F(13)   SETC  'M   M'
&G(13)   SETC  'M   M'
&C(14)   SETC  'N   N'
&D(14)   SETC  'NN  N'
&E(14)   SETC  'N N N'
&F(14)   SETC  'N  NN'
&G(14)   SETC  'N   N'
&C(15)   SETC  'OOOOO'
&D(15)   SETC  'O   O'
&E(15)   SETC  'O   O'
&F(15)   SETC  'O   O'
&G(15)   SETC  'OOOOO'
&C(16)   SETC  'PPPP '
&D(16)   SETC  'P   P'
&E(16)   SETC  'PPPP '
&F(16)   SETC  'P    '
&G(16)   SETC  'P    '
&C(17)   SETC  ' QQQ '
&D(17)   SETC  'Q   Q'
&E(17)   SETC  'Q Q Q'
&F(17)   SETC  'Q  Q '
&G(17)   SETC  ' QQ Q'
&C(18)   SETC  'RRRR '
&D(18)   SETC  'R   R'
&E(18)   SETC  'RRRR '
&F(18)   SETC  'R  R '
&G(18)   SETC  'R   R'
&C(19)   SETC  ' SSSS'
&D(19)   SETC  'S    '
&E(19)   SETC  ' SSS '
&F(19)   SETC  '    S'
&G(19)   SETC  'SSSS '
&C(20)   SETC  'TTTTT'
&D(20)   SETC  '  T  '
&E(20)   SETC  '  T  '
&F(20)   SETC  '  T  '
&G(20)   SETC  '  T  '
&C(21)   SETC  'U   U'
&D(21)   SETC  'U   U'
&E(21)   SETC  'U   U'
&F(21)   SETC  'U   U'
&G(21)   SETC  ' UUU '
&C(22)   SETC  'V   V'
&D(22)   SETC  'V   V'
&E(22)   SETC  'V   V'
&F(22)   SETC  ' V V '
&G(22)   SETC  '  V  '
&C(23)   SETC  'W   W'
&D(23)   SETC  'W   W'
&E(23)   SETC  'W W W'
&F(23)   SETC  'WW WW'
&G(23)   SETC  'W   W'
&C(24)   SETC  'X   X'
&D(24)   SETC  ' X X '
&E(24)   SETC  '  X  '
&F(24)   SETC  ' X X '
&G(24)   SETC  'X   X'
&C(25)   SETC  'Y   Y'
&D(25)   SETC  ' Y Y '
&E(25)   SETC  '  Y  '
&F(25)   SETC  '  Y  '
&G(25)   SETC  '  Y  '
&C(26)   SETC  'ZZZZZ'
&D(26)   SETC  '   Z '
&E(26)   SETC  '  Z  '
&F(26)   SETC  ' Z   '
&G(26)   SETC  'ZZZZZ'
&C(27)   SETC  ' # # '
&D(27)   SETC  '#####'
&E(27)   SETC  ' # # '
&F(27)   SETC  '#####'
&G(27)   SETC  ' # # '
&C(28)   SETC  '     '
&D(28)   SETC  '     '
&E(28)   SETC  '     '
&F(28)   SETC  ' ,,  '
&G(28)   SETC  '  ,  '
&C(29)   SETC  ' $$$$'
&D(29)   SETC  '$ $  '
&E(29)   SETC  ' $$$ '
&F(29)   SETC  '  $ $'
&G(29)   SETC  '$$$$ '
&C(30)   SETC  ' 000 '
&D(30)   SETC  '0   0'
&E(30)   SETC  '0   0'
&F(30)   SETC  '0   0'
&G(30)   SETC  ' 000 '
&C(31)   SETC  '  1  '
&D(31)   SETC  ' 11  '
&E(31)   SETC  '  1  '
&F(31)   SETC  '  1  '
&G(31)   SETC  ' 111 '
&C(32)   SETC  '2222 '
&D(32)   SETC  '    2'
&E(32)   SETC  '   2 '
&F(32)   SETC  '  2  '
&G(32)   SETC  '22222'
&C(33)   SETC  '3333 '
&D(33)   SETC  '    3'
&E(33)   SETC  ' 333 '
&F(33)   SETC  '    3'
&G(33)   SETC  '3333 '
&C(34)   SETC  '4  4 '
&D(34)   SETC  '4  4 '
&E(34)   SETC  '44444'
&F(34)   SETC  '   4 '
&G(34)   SETC  '   4 '
&C(35)   SETC  '55555'
&D(35)   SETC  '5    '
&E(35)   SETC  '5555 '
&F(35)   SETC  '    5'
&G(35)   SETC  '5555 '
&C(36)   SETC  ' 666 '
&D(36)   SETC  '6    '
&E(36)   SETC  '6666 '
&F(36)   SETC  '6   6'
&G(36)   SETC  ' 666 '
&C(37)   SETC  '77777'
&D(37)   SETC  '   7 '
&E(37)   SETC  '  7  '
&F(37)   SETC  '  7  '
&G(37)   SETC  '  7  '
&C(38)   SETC  ' 888 '
&D(38)   SETC  '8   8'
&E(38)   SETC  ' 888 '
&F(38)   SETC  '8   8'
&G(38)   SETC  ' 888 '
&C(39)   SETC  ' 999 '
&D(39)   SETC  '9   9'
&E(39)   SETC  ' 9999'
&F(39)   SETC  '    9'
&G(39)   SETC  ' 999 '
&C(40)   SETC  '@@@@ '
&D(40)   SETC  '    @'
&E(40)   SETC  '@@@ @'
&F(40)   SETC  '@ @ @'
&G(40)   SETC  '@@@@ '
&C(41)   SETC  '%%  %'
&D(41)   SETC  '   % '
&E(41)   SETC  '  %  '
&F(41)   SETC  ' %   '
&G(41)   SETC  '%  %%'
&C(42)   SETC  '* * *'
&D(42)   SETC  ' *** '
&E(42)   SETC  '*****'
&F(42)   SETC  ' *** '
&G(42)   SETC  '* * *'
&C(43)   SETC  '    /'
&D(43)   SETC  '   / '
&E(43)   SETC  '  /  '
&F(43)   SETC  ' /   '
&G(43)   SETC  '/    '
&C(44)   SETC  '     '
&D(44)   SETC  '     '
&E(44)   SETC  '-----'
&F(44)   SETC  '     '
&G(44)   SETC  '     '
&C(45)   SETC  '   ( '
&D(45)   SETC  '  (  '
&E(45)   SETC  '  (  '
&F(45)   SETC  '  (  '
&G(45)   SETC  '   ( '
&C(46)   SETC  ' )   '
&D(46)   SETC  '  )  '
&E(46)   SETC  '  )  '
&F(46)   SETC  '  )  '
&G(46)   SETC  ' )   '
&C(47)   SETC  ' &&&&&& '
&D(47)   SETC  '&&    '
&E(47)   SETC  ' &&&&  '
&F(47)   SETC  '&&   &&'
&G(47)   SETC  ' &&&&&&&&'
&C(48)   SETC  '  +  '
&D(48)   SETC  '  +  '
&E(48)   SETC  '+++++'
&F(48)   SETC  '  +  '
&G(48)   SETC  '  +  '
&C(49)   SETC  '     '
&D(49)   SETC  '     '
&E(49)   SETC  '     '
&F(49)   SETC  '     '
&G(49)   SETC  '_____'
&C(50)   SETC  '  *  '
&D(50)   SETC  ' C*C '
&E(50)   SETC  'C *  '
&F(50)   SETC  ' C*C '
&G(50)   SETC  '  *  '
&C(51)   SETC  '  ]  '
&D(51)   SETC  '  ]  '
&E(51)   SETC  '  ]  '
&F(51)   SETC  '  ]  '
&G(51)   SETC  '  ]  '
&C(52)   SETC  ' >   '
&D(52)   SETC  '  >  '
&E(52)   SETC  '   > '
&F(52)   SETC  '  >  '
&G(52)   SETC  ' >   '
&C(53)   SETC  ' ..  '
&D(53)   SETC  ' ..  '
&E(53)   SETC  '     '
&F(53)   SETC  ' ..  '
&G(53)   SETC  ' ..  '
&C(54)   SETC  ' ..  '
&D(54)   SETC  ' ..  '
&E(54)   SETC  '     '
&F(54)   SETC  ' ,,  '
&G(54)   SETC  '  ,  '
&C(55)   SETC  '     '
&D(55)   SETC  '     '
&E(55)   SETC  '     '
&F(55)   SETC  ' ..  '
&G(55)   SETC  ' ..  '
&C(56)   SETC  ' ??? '
&D(56)   SETC  '?   ?'
&E(56)   SETC  '   ? '
&F(56)   SETC  '  ?  '
&G(56)   SETC  '  ?  '
&C(57)   SETC  ' '' '' '
&D(57)   SETC  '     '
&E(57)   SETC  '     '
&F(57)   SETC  '     '
&G(57)   SETC  '     '
&C(58)   SETC  '     '
&D(58)   SETC  '====='
&E(58)   SETC  '     '
&F(58)   SETC  '====='
&G(58)   SETC  '     '
&C(59)   SETC  ' ]]  '
&D(59)   SETC  ' ]]  '
&E(59)   SETC  ' ]]  '
&F(59)   SETC  '     '
&G(59)   SETC  ' ..  '
&C(60)   SETC  '   < '
&D(60)   SETC  '  <  '
&E(60)   SETC  ' <   '
&F(60)   SETC  '  <  '
&G(60)   SETC  '   < '
&C(61)   SETC  '     '
&D(61)   SETC  '     '
&E(61)   SETC  '^^^^^'
&F(61)   SETC  '    ^'
&G(61)   SETC  '     '
&C(62)   SETC  '     '
&D(62)   SETC  '     '
&E(62)   SETC  '     '
&F(62)   SETC  '     '
&G(62)   SETC  '     '
&J       SETA  1
&K       SETA  1
         AIF   ('&LETTERS'(1,1) NE '''').JLOOP
.LLOOP   ANOP
&J       SETA  &J+1
.JLOOP   AIF   (K'&LETTERS LT &J).END
         AIF   (K'&LETTERS EQ &J AND '&LETTERS'(&J,1) EQ '''').END
&I       SETA  1
.COMP    AIF   ('&B(&I)' EQ '&LETTERS'(&J,1)).FND
&I       SETA  &I+1
         AIF   (&I LT 62).COMP
.FND     ANOP
&A(&K)   SETC  '&C(&I)'
&A(&K+1) SETC  '&D(&I)'
&A(&K+2) SETC  '&E(&I)'
&A(&K+3) SETC  '&F(&I)'
&A(&K+4) SETC  '&G(&I)'
&K       SETA  &K+5
         AIF   (&K LT 51).LLOOP
.END     ANOP
         SPACE 2
         MNOTE *,'******************************************************
               ****************'
         MNOTE *,' '
&I       SETA  1
.REDO    MNOTE *,' &A(&I)  &A(&I+5)  &A(&I+10)  &A(&I+15)  &A(&I+20)  &*
               A(&I+25)  &A(&I+30)  &A(&I+35)  &A(&I+40)  &A(&I+45)'
&I       SETA  &I+1
         AIF   (&I LT 6).REDO
         MNOTE *,' '
         MNOTE *,'******************************************************
               ****************'
         MEXIT
         SPACE 4
         MEND
./ ADD NAME=BLANK
         MACRO                            ,
&LABEL   BLANK &AREA,&CHAR=C' ',&CC=C' '  ,
         LCLC  &CHAR$,&CC$
&CHAR$   SETC  'C'' '''                   , DEFAULT FILL CHARACTER
&CC$     SETC  'C'' '''                   , DEFAULT CONTROL CHAR
         AIF   ('&CHAR' EQ '').NOFILL     , USE DEFAULT FILL CHAR
&CHAR$   SETC  '&CHAR'                    , USER FILL CHARACTER
.NOFILL  ANOP                             ,
         AIF   ('&CC' EQ '').NOCC         ,
&CC$     SETC  '&CC'                      ,
.NOCC    ANOP                             ,
&LABEL   MVI   &AREA,&CHAR$               , SET FILL BYTE
         MVC   &AREA+1(L'&AREA-1),&AREA   , FILL FIELD
         AIF   ('&CHAR$' EQ '&CC$').MEXIT ,
         MVI   &AREA,&CC$                 , INSERT CONTROL CHARACTER
.MEXIT   MEXIT                            , LEAVE MACRO
         MEND                             , OF MACRO
./ ADD NAME=BOX
         MACRO
&LABEL   BOX   &COMM,&ID=,&TITLE=,&HEAD=,&CTL1=SPACE,&CTL2=SPACE,      -
               &PRINT=,&HDR=,&CTR=NO
         GBLB  &BOXID
         GBLB  &P
         GBLC  &BOXHDR
         LCLA  &I,&J,&N
         LCLB  &BCTR
         LCLC  &BID,&BTITLE,&BCOMM,&BLANK,&BCTL1,&BCTL2
         AIF   (&P).NB01
.NB01    ANOP
&BLANK   SETC  '                                                       -
                              '
&BCTR    SETB  ('&CTR' NE 'NO')
.*
.*       TURN PRINT ON IF NECESSARY
.*
         AIF   (T'&PRINT EQ 'O').TESTID
         PUSH  PRINT
         PRINT ON,GEN
.*
.*       PICK UP ID FIELD
.*
.TESTID  AIF   ('&ID' EQ '' AND '&LABEL' EQ '').TESTHDR
         AIF   ('&ID' EQ '' OR '&LABEL' EQ '').TESTPRV
         MNOTE 4,'BOX001: ID SPECIFIED TWICE - LABEL FIELD IGNORED'
.TESTPRV AIF   (NOT &BOXID).SETID
         MNOTE 4,'BOX002: ID ALREADY SPECIFIED - THIS ONE IGNORED'
         AGO   .TESTHDR
.SETID   ANOP
&BOXID   SETB  1
&BID     SETC  '&ID'
         AIF   ('&ID' NE '').TESTHDR
&BID     SETC  '&LABEL'
.*
.*       PICK UP HEADER SPECIFICATION
.*
.TESTHDR AIF   (T'&HDR EQ 'O' AND T'&HEAD EQ 'O').TESTTTL
&BOXHDR  SETC  '&HDR&HEAD'
         AIF   ('&BOXHDR'(1,1) NE '''').TESTTTL
&BOXHDR  SETC  '&BOXHDR'(2,K'&BOXHDR-2)
.*
.*       PICK UP TITLE SPECIFICATION
.*
.TESTTTL AIF   (T'&TITLE EQ 'O').TSTCTL1
&BTITLE  SETC  '&TITLE'
         AIF   ('&BTITLE'(1,1) NE '''').GENTTL
&BTITLE  SETC  '&BTITLE'(2,K'&BTITLE-2)
.GENTTL  ANOP
&BID     TITLE '&BOXHDR&BTITLE'
         AGO   .GENCOMM
.*
.*       GENERATE LISTING CONTROL IF APPROPRIATE
.*
.TSTCTL1 AIF   (T'&CTL1 EQ 'O').GENCOMM
&BCTL1   SETC  '&CTL1'
         AIF   ('&BCTL1'(1,1) NE '''').GENCTL1
&BCTL1   SETC  '&BCTL1'(2,K'&BCTL1-2)
.GENCTL1 ANOP
         &BCTL1
.*
.*       GENERATE COMMENTS
.*
.GENCOMM AIF   (T'&COMM EQ 'O').MEND
***********************************************************************
*                                                                     *
&N       SETA  N'&SYSLIST
&I       SETA  1
.COMLOOP ANOP
&BCOMM   SETC  '&SYSLIST(&I)'
         AIF   ('&BCOMM' EQ '' OR '&BCOMM' EQ '''''').NXTCOMM
         AIF   ('&BCOMM'(1,1) NE '''').TESTLEN
&BCOMM   SETC  '&BCOMM'(2,K'&BCOMM-2)
.TESTLEN AIF   (K'&BCOMM LE 56).MNOTE
&J       SETA  56
.SCAN    AIF   ('&BCOMM'(&J,1) EQ ' ').MNOTE1
&J       SETA  &J-1
         AIF   (&J GE 10).SCAN
&J       SETA  56
.MNOTE1  AIF   (&BCTR).CENTRE1
&BTITLE  SETC  '&BLANK'(1,8).'&BCOMM'(1,&J-1).'&BLANK'
         AGO   .STAR1
.CENTRE1 ANOP
&BTITLE  SETC  '&BLANK'(1,32-&J/2).'&BCOMM'(1,&J-1).'&BLANK'
.STAR1   ANOP
&BTITLE  SETC  '&BTITLE'(1,68).'*'
         MNOTE *,'&BTITLE'
&BCOMM   SETC  '&BCOMM'(&J+1,K'&BCOMM-&J)
         AIF   (K'&BCOMM GE 56).TESTLEN
.MNOTE   AIF   (&BCTR).CENTRE
&BCOMM   SETC  '&BLANK'(1,8).'&BCOMM'.'&BLANK'
         AGO   .STAR
.CENTRE  ANOP
&BCOMM   SETC  '&BLANK'(1,(63-K'&BCOMM)/2).'&BCOMM'.'&BLANK'
.STAR    ANOP
&BCOMM   SETC  '&BCOMM'(1,68).'*'
         MNOTE *,'&BCOMM'
.NXTCOMM ANOP
&I       SETA  &I+1
         AIF   (&I LE &N).COMLOOP
*                                                                     *
***********************************************************************
.*
.*       TEST SECOND LISTING CONTROL
.*
         AIF   (T'&CTL2 EQ 'O').MEND
&BCTL2   SETC  '&CTL2'
         AIF   ('&BCTL2'(1,1) NE '''').GENCTL2
&BCTL2   SETC  '&BCTL2'(2,K'&BCTL1-2)
.GENCTL2 ANOP
         &BCTL2
.MEND    ANOP
         AIF   (T'&PRINT EQ 'O').NB02
         POP   PRINT
.NB02    ANOP
         MEND
./ ADD NAME=BSPAUTH  8002-74023-74023-1404-00021-00079-00000-HERC02  00
         MACRO
&LABEL BSPAUTH &FUNC                  , default base register
         COPY  BSPGLBLS               , get name of globals
         COPY  BSPSGLBL               , set global values
&LABEL   DS    0H
         AIF   ('&FUNC' EQ 'ON').AUTHON
         AIF   ('&FUNC' EQ 'OFF').AUTHOFF
         MNOTE '*,No parm specified, BSPAUTH OFF assumed'
.AUTHOFF ANOP
         BOX   'Turning of authorization via SVC',CTR=YES
         SR    0,0                    , clear R0, required by SVC
         SR    1,1                    , indicate no authorization
         SVC   &BSPASVC               , issue SVC
         MEXIT
.AUTHON  ANOP
         BOX   'Turning on authorization via SVC',CTR=YES
         SR    0,0                    , clear R0, required by SVC
         LA    1,1                    , indicate authorization req
         SVC   &BSPASVC               , issue SVC
         MEXIT
         MEND
./ ADD NAME=BSPBEG
         MACRO
&LABEL  BSPBEG &BASE=3,               , default base register          *
               &BASE2=,               , second base register           *
               &BASE3=,               , third base register            *
               &BASE4=,               , fourth base register           *
               &HEADER=               , header in title
         COPY  BSPGLBLS               , get name of globals
         COPY  BSPSGLBL               , set global values
&BSPCSCT SETC  'BSPTEMP'              , default pgm name
         AIF   ('&LABEL' EQ '').NOLAB , BIF no label given
&BSPCSCT SETC  '&LABEL'               , else set CSECT name from label
.NOLAB   ANOP                         ,
&H       SETC  ' ===> '               , first part of header
         AIF   ('&HEADER' EQ '').NOHEAD
         AIF   ('&HEADER'(1,1) EQ '''').HEADAPO
&H       SETC  '&HEADER'
         AGO   .NOHEAD
.HEADAPO ANOP
&H       SETC  '&HEADER'(2,K'&HEADER-2)
.NOHEAD  ANOP
&SAVE    SETC  'BSPA'.'&SYSNDX'       , Save Area Label
&SAVEEND SETC  'BSPB'.'&SYSNDX'       , Save Area label, end
&BEG     SETC  'BSPC'.'&SYSNDX'       , Program begin label
&BYP     SETC  'BSPD'.'&SYSNDX'       , End of eyecatcher
&LEN     SETC  'BSPE'.'&SYSNDX'       , Length of eyecatcher
         @     &BSPCSCT               , PGM name in block letters
&BSPCSCT BOX   'Module name................... &BSPCSCT',              -
               'Written by.................... &BSPAUTH',              -
               'Assembly data................. &SYSDATE',              -
               'Assembly time................. &SYSTIME',              -
               TITLE=' HOUSEKEEPING ',                                 -
               HEAD=&H
         SPACE                        ,
         REGISTER                     ,
         EJECT                        ,
&BSPCSCT CSECT                        , Start of module
         USING &BSPCSCT,R15           , Temporary addressabilty
         STM   R14,R12,12(R13)        , Save all registers
         B     &BYP                   , Branch around eyecatcher
&BEG     DS    0H                     , Eyecatcher starts here
         DC    AL2(&BYP-&BEG)         , Length of eyecatcher
         DC    CL8'&BSPCSCT'          , CSECT name
         DC    C' VER. &BSPVER..&BSPMOD ' , Version and Mod Level
         DC    C'&SYSDATE._&SYSTIME'  , ASSEMBLY DATE AND TIME
         DC    C' &BSPAUTH'           , Author
&BYP     DS    0H                     , Eyecatcher ends here
         LR    &BASE,R15              , Load first base from entry
         AIF   ('&BASE2' EQ '').@USING1
         LR    &BASE2,&BASE             Set
         LA    &BASE2,4095(&BASE2)         second
         LA    &BASE2,1(&BASE2)                  base register
         AIF   ('&BASE3' EQ '').@USING2
         LR    &BASE3,&BASE2            SET
         LA    &BASE3,4095(&BASE3)         UP
         LA    &BASE3,1(&BASE3)              THIRD BASE REG
         AIF   ('&BASE4' EQ '').@USING3
         LR    &BASE4,&BASE3            THIS IS
         LA    &BASE4,4095(&BASE4)             THE FOURTH
         LA    &BASE4,1(&BASE4)                           BASE REG
         USING &BSPCSCT,&BASE,&BASE2,&BASE3,&BASE4 ADDRESABILITY
         AGO   .@BASE00
.@USING3 ANOP
         USING &BSPCSCT,&BASE,&BASE2,&BASE3 MAKE IT ADDRESSABLE
         AGO   .@BASE00
.@USING2 ANOP
         USING &BSPCSCT,&BASE,&BASE2    MAKE ADDRESSABILITY PERMANENT
         AGO   .@BASE00
.@USING1 ANOP
         USING &BSPCSCT,&BASE           ESTABLISH ADDRESSABILITY
.@BASE00 ANOP                         ,
         DROP  R15                    , Not needed any more
         LR    R15,R13                , Old save area pointer
         CNOP  0,4                    , Align to fullword boundary
         ST    R15,&SAVE+4            , Store old savearea address
         BAL   R13,&SAVEEND           , Load new save area address
&SAVE    DC    9CL8'&BSPCSCT'         , Save area
&SAVEEND ST    R13,8(0,R15)           , Store into old save area
         MEXIT
         MEND
./ ADD NAME=BSPEND
         MACRO
&LABEL   BSPEND &ENTRY                    , end of module
         COPY   BSPGLBLS
         GBLC   &BSPDSCT,&BSPDSCE,&BSPSP
         GBLB   &BSPRENT
         LCLC   &LBL,&MODLEN
&BSPCSCT CSECT                            , Resume CSECT
         LTORG                            , Literal pool
         X2CHRTAB                         , HEX to CHAR tranlation tbl
         AIF   ('&LABEL' EQ '').NOLABEL
&LBL     SETC  '&LABEL'
         AGO   .END1
.NOLABEL ANOP
&LBL     SETC  '$$1'.'&SYSNDX'
.END1    ANOP
&LBL     DC    H'0'                       , Halfword for module length
&MODLEN  SETC  '$$2'.'&SYSNDX'            ,
&MODLEN  EQU   *-&BSPCSCT                 , MODULE LENGTH
         ORG   &LBL                       , back up some bytes
         DC    AL2(&MODLEN)               , set the length
         ORG                              , restore org
         AIF   (NOT &BSPRENT).NORENT
&BSPDSCT DSECT                            , resume workarea
&BSPDSCE DS    0F                         , end of workarea
&BSPCSCT CSECT                            , resume CSECT
.NORENT  ANOP
         AIF   ('&ENTRY' EQ '').NOENT
         END   &ENTRY                     , of module, define Entry
         MEXIT
.NOENT   ANOP
         END                              , of module
         MEND
./ ADD NAME=BSPENTER 8000-74007-74007-2257-00138-00138-00000-HERC01  00
         MACRO
&NAME BSPENTER &BASE=R3,                  , BASE REGISTER              +
               &RENT=YES,                 , MAKE REENTRANT PGM         +
               &DATA=WORKAREA,            , name of reentrant storage  +
               &SP=1,                     , SUBPOOL FOR SAVE AREA      +
               &CHAIN=YES                 , CHAIN SAVEAREAS
.**********************************************************************
.*                                                                    *
.* Name: BSPENTER                                                     *
.*                                                                    *
.* Type: Assembler macro                                              *
.*                                                                    *
.* Desc: Provide entry coding and house keeping for assembler modules *
.*                                                                    *
.* Use: <NAME> BSPENTER BASE=(REG1,...),                              *
.*                      CSECT=<YES|NO>,                               *
.*                      RENT=<YES|NO>,                                *
.*                      SP=<SUBPOOL NUMBER>                           *
.*                      CHAIN=<YES|NO>                                *
.*                                                                    *
.*       <NAME>   - A SYMBOLIC TAG ASSIGNED TO THE FIRST INSTRUCTION  *
.*                  GENERATED OR, IF APPLICABLE, TO THE CSECT CREATED *
.*                                                                    *
.*       BASE     - A LIST OF BASE REGISTERS TO BE USED.  THE DEFAULT *
.*                  IS REGISTER 3                                     *
.*                                                                    *
.*       RENT     - YES: REENTRANT CODE IS GENERATED BY THIS MACRO    *
.*                  NO:  NON-REENTRANT CODE IS GENERATED              *
.*                                                                    *
.*       SP       - SPECIFIES THE SUBPOOL NUMBER WHERE THE SAVE AREA  *
.*                  FOR REENTRANT CODE IS GETMAINED FROM              *
.*                  THE DEFAULT IS 1                                  *
.*                                                                    *
.*       CHAIN    - YES:  SAVE AREAS ARE TO BE CHAINED                *
.*                  NO: DO NOT CHAIN SAVE AREAS.  THIS IS INTENDED    *
.*                      FOR HIGH-USE REENTRANT MODULES TO AVOID  THE  *
.*                      OVERHEAD OF GETMAIN/FREEMAIN                  *
.*                                                                    *
.**********************************************************************
         COPY  BSPGLBLS                   , GET NAMES OF GLOBALS
         COPY  BSPSGLBL                   , SET GLOBALS
         GBLC  &BSPDSCT,&BSPDSCE,&BSPSP
         GBLB  &BSPRENT
         LCLA  &NUMREGS,&REGNO
         LCLC  &REG,&CHAR,&LAST,&USING,&TEMP,&FIRST,&TMPDATA
         LCLC  &DATAEND,&BEG,&BYP,&LEN
&BSPRENT SETB  0
&BSPSP   SETC  '&SP'
&TMPDATA SETC  'BSPA'.'&SYSNDX'
&DATAEND SETC  'BSPB'.'&SYSNDX'           , SAVE AREA LABEL, END
&BEG     SETC  'BSPC'.'&SYSNDX'           , PROGRAM BEGIN LABEL
&BYP     SETC  'BSPD'.'&SYSNDX'           , END OF EYECATCHER
&LEN     SETC  'BSPE'.'&SYSNDX'           , LENGTH OF EYECATCHER
&BSPDSCT SETC  '&DATA'                    , NAME OF AUTOMATIC STORAGE
&BSPDSCE SETC  '&DATAEND'                 , end of getmain area
         AIF   ('&RENT' NE 'YES').NORENT
&BSPRENT SETB  1
         AIF   ('&DATA' NE '').DATA
&BSPDSCT SETC  '&TMPDATA'                 , NAME OF DATA AREA
         MNOTE 8,'REQUIRED OPERAND ''NAME'' MISSING. &BSPDSCT USED'
.DATA    ANOP
         AIF   ('&SP' NE '').SP
&BSPSP   SETC  '1'
         MNOTE 8,'REQUIRED OPERAND ''SP'' MISSING. SP=&BSPSP USED'
.SP      ANOP
.NORENT  ANOP
&BSPCSCT SETC  '&NAME'
         AIF   ('&NAME' NE '').CSECT
&BSPCSCT SETC  'BSPF'.'&SYSNDX'
         MNOTE 8,'REQUIRED OPERAND ''NAME'' MISSING. &BSPCSCT USED'
.CSECT   ANOP
         @     &BSPCSCT
&BSPCSCT BOX   'Module name .................. &BSPCSCT',              +
               'Writenn by ................... &BSPAUTH',              +
               'Assembly date ................ &SYSDATE',              +
               'Assembly time ................ &SYSTIME',              +
               'Workarea name ................ &BSPDSCT',
&BSPCSCT CSECT
         REGISTER
         USING &BSPCSCT,R15               , Temporary addressability
         B     &BYP                       , branch around eyecatcher
&BEG     EQU   *                          , Beginning of eyecatcher
         DC    AL1(&BYP-&BEG)             , Length of eyecatcher
         DC    CL8'&BSPCSCT'              , CSECT name
         DC    C' VER. &BSPVER..&BSPMOD ' , Version and Mod Level
         DC    C'&SYSDATE._&SYSTIME'      , ASSEMBLY DATE AND TIME
         DC    C' &BSPAUTH'               , Author
&BYP     DS    0H                         , end of eyecatcher
         STM   R14,R12,12(R13)            , save callers registers
&NUMREGS SETA  1
.CKBASE  ANOP
&REG     SETC  '&BASE(&NUMREGS)'(1,3)
         AIF   ('&REG'(1,1) NE 'R').SKIPBAS
&TEMP    SETC  '&REG'(2,2)
&REGNO   SETA  &TEMP
         AIF   (&REGNO LT 2).BADBASE
         AIF   (&REGNO GT 12).BADBASE
         AIF   (&REGNO NE 2).SETBASE
         MNOTE 0,'*** WARNING - R2 IS A BASE REGISTER. TRANSLATE AND TE+
               ST INSTRUCTION WILL DESTROY CONTENTS.'
.SETBASE ANOP
         AIF   ('&FIRST' NE '').SETBAS2
         LR    &REG,R15                   , load base register
&FIRST   SETC  '&REG'
         AGO   .SETLAST
.SETBAS2 LA    &REG,2048(&LAST)           , add 2048 to last base
         LA    &REG,2048(&REG)            , make it 4096 for next base
.SETLAST ANOP
&LAST    SETC  '&REG'
&USING   SETC  '&USING.,&REG'
         AGO   .NEXTBAS
.SKIPBAS MNOTE 8,'*&REG* IS AN INVALID REGISTER FORM, IGNORED'
         AGO   .NEXTBAS
.BADBASE MNOTE 8,'*&REG* IS AN INVALID BASE REGISTER, IGNORED'
.NEXTBAS ANOP
&NUMREGS SETA  &NUMREGS+1
         AIF   (&NUMREGS LE N'&BASE).CKBASE
         DROP  R15                         , forget old base register
         USING &BSPCSCT.&USING             , TELL NEW BASE TO ASSEMBLER
.NOSPM   AIF   ('&CHAIN' EQ 'NO').DONE
         AIF   (&BSPRENT).GETMAIN
         LR    R15,R13                     , OLD SAVE AREA POINTER
         CNOP  0,4                    , Align to fullword boundary
         BAL   R13,&DATAEND           , Load new save area address
&DATA    DC    9CL8'&BSPCSCT'         , Save area
&DATAEND ST    R13,8(0,R15)           , Store into old save area
         AGO   .CHAIN
.GETMAIN ANOP
 MNOTE *,'GETMAIN R,LV=&BSPDSCE-&BSPDSCT,SP=&SP'
         GETMAIN R,LV=&BSPDSCE-&BSPDSCT,SP=&SP
         LR    R15,R13                , old save area pointer
         LR    R13,R1                 , r13 points to new save area
         USING &DATA,R13              , tell assembler
         LM    R0,R1,20(R15)          , restore original R0 and R1
.CHAIN   ST    R15,4(R13)             , address of old SA into new SA
         ST    R13,8(R15)             , address of new SA into old SA
.DONE    ANOP
         MEND
./ ADD NAME=BSPGLBLS 8000-74023-74023-1402-00007-00007-00000-HERC02  00
         GBLC  &BSPAUTH               , Program authors
         GBLC  &BSPCSCT               , Current CSECT name
         GBLC  &BSPMOD                , Modification level
         GBLC  &BSPPRFX               , Program Prefix Chars
         GBLC  &BSPPRGM               , Program id string
         GBLC  &BSPVER                , Program version number
         GBLC  &BSPASVC               , Authorization SVC number
./ ADD NAME=BSPPATCH
         MACRO
         BSPPATCH &N,&M
.**********************************************************************
.*                                                                    *
.* NAME : BSPPATCH                                                    *
.*                                                                    *
.* TYPE: ASSEMBLER MACRO                                              *
.*                                                                    *
.* PURPOSE:   TO RESERVE 5% OR 25 HALF WORDS IN A MODULE              *
.*            FOR MAINTENANCE                                         *
.*            IF &N IS SET IT IS ASSUMED TO BE % OF SPACE             *
.*            TO BE RESERVED                                          *
.*            IF &M IS SET IT IS ASSUMED TO BE NUMBER OF BYTES        *
.*            TO BE RESERVED                                          *
.*                                                                    *
.**********************************************************************
         DS    0H                         , ALIGN TO HALFWORD FIRST
&LBLA    SETC  '$$1'.'&SYSNDX'              ,
&LBLB    SETC  '$$2'.'&SYSNDX'              ,
         AIF   (T'&N NE 'O').PCNT
         AIF   (T'&M NE 'O').NUMB
.DFLT    ANOP  ,
&LBLA    EQU   ((*-&SYSECT+99)/100)*5       , DEFAULT OF 5 PERCENT
&PERC    SETC  '5 PERCENT OF MODULE SIZE '  ,
         AGO   .PTCH
.PCNT    ANOP  ,
         AIF   (T'&N NE 'N').MNT1
&LBLA    EQU   ((*-&SYSECT+99)/100)*&N      , VALUE OF N PERCENT
&PERC    SETC  '&N PERCENT OF MODULE SIZE   '
         AGO   .PTCH
.NUMB    ANOP  ,
         AIF   (T'&M NE 'N').MNT1
&PERC    SETC  '&M BYTES '                ,
&LBLA    EQU   &M                           , VALUE OF &M BYTES
         AGO   .PTCH
.PTCH    ANOP
         MNOTE *,'&PERC SET ASIDE AS PATCH AREA'
         DC    CL28'PATCH AREA - &SYSECT' , EYE CATCHER
&LBLB    DC    25S(*)                     , AT LEAST 25 HALFWORDS
         ORG   &LBLB                      , POSITION TO BEGINNING
         DC    ((&LBLA+1)/2)S(*)          , RESERVE SUFFICIENT STORAGE
         ORG   ,                          , REPOSITION
         MEXIT                            , LEAVE THE MACRO
.MNT1    MNOTE 8,'VALUE MUST BE NUMERIC'
         AGO   .DFLT
         MEND
./ ADD NAME=BSPRET   8000-74008-74008-1709-00040-00040-00000-HERC01  00
         MACRO
&LABEL   BSPRET &RC=
         COPY  BSPGLBLS
         COPY  BSPSGLBL
         GBLB  &BSPRENT
         GBLC  &BSPDSCT,&BSPDSCE,&BSPSP
         LCLC  &RT
&LABEL   DS    0H                     , align just to make sure
         AIF   ('&RC' EQ '').RETR15   , BIF no RC parm give
         AIF   ('&RC'(1,1) EQ '(').LR
         LA    R15,&RC.(0,0)          , set up returncode
         AGO   .RETR15
.LR      ANOP
&RT      SETC  '&RC'(2,K'&RC-2)
         LR    R15,&RT                , Load return code from register
.RETR15  ANOP
         AIF   (NOT &BSPRENT).NORENT
         LR    R1,R13                 , save current save area address
         BALR  R2,0                   , set up base register
         USING *,R2                   , tell assembler
         L     R3,4(R13)              , Get old save area address
         LR    R4,R15                 , save return code
  MNOTE *,'FREEMAIN R,LV=&BSPDSCE-&BSPDSCT,A=(1),SP=&BSPSP'
         FREEMAIN R,LV=&BSPDSCE-&BSPDSCT,A=(1),SP=&BSPSP
         LR    R15,R4                 , restore rc
         LR    R13,R3                 , restor old savearea
         DROP  R2                     , drop temporary base
         AGO   .RENT
.NORENT  ANOP
         L     R13,4(R13)             , Get old save area address
.RENT    ANOP
         L     R14,12(R13)            , restore return address
         LM    R0,R12,20(R13)         , restore remaining registers
         BR    R14                    , and return to caller
         AIF   (NOT &BSPRENT).MEXIT
&BSPDSCT DSECT                        , Automatic storage
         DS    18F                    , standard save area
&BSPCSCT CSECT                        , resume CSECT
.MEXIT   MEXIT
         MEND
./ ADD NAME=BSPSGLBL 8000-74023-74023-1403-00006-00006-00000-HERC02  00
&BSPAUTH SETC  'V.BANDKE, BSP GmbH'   , PROGRAM AUTHORS
&BSPMOD  SETC  '0'                    , MODIFICATION LEVEL
&BSPPRFX SETC  'BSP'                  , PROGRAM PREFIX CHARS
&BSPPRGM SETC  'BSPMAIN'              , PROGRAM ID STRING
&BSPVER  SETC  '1'                    , VERSION NUMBER
&BSPASVC SETC  '244'                  , Authorization SVC number
./ ADD NAME=DA
*          DATA SET SFALLOC    AT LEVEL 006 AS OF 05/17/78              00121300
*********************************************************************** 00121400
*                                                                     * 00121500
*                                                                     * 00121600
*               DDDDDDDDD                     AAAAAAAAAA              * 00121700
*              DDDDDDDDDD                   AAAAAAAAAAAA              * 00121800
*             DD       DD                  AA        AA               * 00121900
*            DD        DD                 AA        AA                * 00122000
*           DD        DD                 AA        AA                 * 00122100
*          DD        DD                 AAAAAAAAAAAA                  * 00122200
*         DD        DD                 AAAAAAAAAAAA                   * 00122300
*        DD        DD                 AA        AA                    * 00122400
*       DD        DD                 AA        AA                     * 00122500
*      DD        DD                 AA        AA                      * 00122600
*     DDDDDDDDDDD                  AA        AA                       * 00122700
*    DDDDDDDDDD                   AA        AA                        * 00122800
*                                                                     * 00122900
* 1978                                                                * 00123000
*        USAGE -THIS MACRO MADE BE USED  MULTIPLE TIMES IN ONE        * 00123100
*              PROGRAM AND GENERATE UNIQUE LABELS VIA THE 'UNQSUF'    * 00123200
*              PARAMETER.  PRIOR TO ISSUING SVC 99 REGISTER 1 MUST    * 00123300
*              CONTAIN THE ADDRESS OF   THIS PARAMETER LIST.          * 00123400
*              EX. -      PARMLIB   DA  DSN='SYS1.PARMLIB'            * 00123500
*                                   ... ANY OTHER CODE                * 00123600
*                                   LA  R1,PARMLIB                    * 00123700
*                                   SVC 99                            * 00123800
*                                   ... ANY OTHER CODE                * 00123900
*        DEFAULTS -THERE ARE NO DEFAULTS FOR ANY PARM.    THE ONLY    * 00124000
*              PARMETERS THAT WILL BE GENERATED FOR DYNAMIC           * 00124100
*              ALLOCATION ARE THE ONES  THAT ARE SPECIFICALLY CODED.  * 00124200
*              THE TYPE OF CALL TO DYNAMIC ALLOCATION DEFAULTS TO     * 00124300
*              ALLOCATION.  BY CODING IN A VALUE FOR DEALLOC, THE     * 00124400
*              PARAMETERS WILL DEALLOCATE THE RESOURCE INSTEAD OF     * 00124500
*              ALLOCATING IT.                                         * 00124600
*                                                                     * 00124700
*        LABELS - LABELS HAVE BEEN PROVIDED FOR OFTEN USED FIELDS.    * 00124800
*              SOME OF THESE ARE :                                    * 00124900
*                      1. DAER    WHICH  POINTS TO THE ERROR CODE     * 00125000
*                                       FROM SVC 99                   * 00125100
*                      2. DAIN    WHICH  POINTS TO THE INFORMATION    * 00125200
*                                       CODE FROM SVC 99              * 00125300
*              ALSO, EACH VALUE SUCH AS THE DSNAME TO ALLOCATE        * 00125400
*              HAS A LABEL ASSOCIATED WITH IT FOR EASY REFERENCE.     * 00125500
*                                                                     * 00125600
*                                                                     * 00125700
*********************************************************************** 00125800
         MACRO                                                          00125900
&NAME    DA    &DSN=,              THE DATASET NAME                    X00126000
               &DDN=,              DDNAME TO ALLOCATE OR DEALLOCATE    X00126100
               &MEM=,              MEMBER NAME FOR PDS OR BIAS FOR GDG X00126200
               &STATUS=,           PRIMARY DISPOSITION                 X00126300
               &DISP=,             DISPOSITION IF NORMAL EOJ           X00126400
               &CDISP=,            DISPOSITION IF ABNORMAL EOJ         X00126500
               &SPACE=,            PRIMARY SPACE REQUIREMENT           X00126600
               &TYPSP=,            TYPE OF SPACE TO ALLOCATE           X00126700
               &SECSP=,            SECONDARY SPACE REQUIREMENT         X00126800
               &VOLSER=,           VOLUME SERIAL #                     X00126900
               &UNIT=,             UNIT DESIGNATION                    X00127000
               &SYSOUT=,           CLASS SPECIFICATION FOR SYSOUT      X00127100
               &FREE=,             DEALLOCATE AT CLOSE                 X00127200
               &COPIES=,           COPIES OF YOUR SYSOUT               X00127300
               &LABEL=,            TYPE LABEL - SL,NL,ETC.             X00127400
               &EXPDT=,            EXPIDATION DATE                     X00127500
               &RETPD=,            RETENTION PERIOD                    X00127600
               &DUMMY=,            ALLOCATE A DUMMY DATA SET           X00127700
               &BLKSIZE=,          BLOCK SIZE                          X00127800
               &FUNC=,             FOR PUNCH OUTPUT INTERPRETING       X00127900
               &FORM=,             FORM NAME SPECIFICATION.            X00128000
               &LRECL=,            LOGICAL RECORD LENGTH               X00128100
               &DIRSP=,            # OF DIRECTORY BLOCKS OR INDEX SPACEX00128200
               &RLSE=,             TO REQUEST THE RELEASE OF UNUSED SP X00128300
               &UNQSUF=,           UNIQUE SUFFIX FOR LABEL NAMES LEN=4 X00128400
               &DCB=,              SPECIFY A DSNAME TO COPY DCB FROM   X00128500
               &DEALLOC=                                                00128600
         LCLA  &A,&K,&A2                                                00128700
         LCLC  &X,&NM                                                   00128800
&X       SETC  '&UNQSUF'                                                00128900
&NAME    DS    0F                                                       00129000
         DC    X'80'                                                    00129100
         DC    AL3(DARB&X)                                              00129200
DARB&X   DS    0F                                                       00129300
         DC    AL1(20)                                                  00129400
         AIF   (T'&DEALLOC EQ 'O').ALLOC                                00129500
         DC    AL1(02)             THIS CONTROL BLOCK IS FOR DEALLOCATE 00129600
         AGO   .PAST                                                    00129700
.ALLOC   DC    AL1(01)             THIS CONTROL IS FOR ALLOCATION       00129800
.PAST    DC    AL2(0)                                                   00129900
DAER&X   DC    AL2(0)              ERROR CODE FROM DYNAMIC ALLOCATION   00130000
DAIN&X   DC    AL2(0)              INFORMATION CODE FROM DYNALLOC       00130100
         DC    A(TS&X)             POINTER TO CALL LIST FOR SVC 99      00130200
         DC    XL8'0'              RESERVED                             00130300
&A       SETA  1                                                        00130400
         DS    0F                  FULLWORD ALIGNED                     00130500
TS&A&X   DC    X'0001'             KEY FOR DDNAME SPECIFICATION         00130600
         DC    XL2'1'                                                   00130700
&K       SETA  K'&DDN                                                   00130800
         DC    XL2'&K'             LENGTH OF DDNAME SPECIFIED           00130900
DADN&X   DC    C'&DDN'             DDNAME PLUGGED IN HERE               00131000
&A       SETA  &A+1                                                     00131100
         AIF   (T'&DSN EQ 'O').A                                        00131200
         DS    0F                                                       00131300
TS&A&X   DC    X'0002'             KEY FOR DSNAME SPECIFICATION         00131400
         DC    XL2'1'                                                   00131500
         DC    AL2(44)             LENGTH OF DSNAME MAX IS 44           00131600
DADS&X   DC    CL44'&DSN'          DSNAME IS PLUGGED IN HERE            00131700
&A       SETA  &A+1                                                     00131800
.A       AIF   (T'&MEM EQ 'O').B                                        00131900
         DS    0F                                                       00132000
TS&A&X   DC    X'0003'             KEY FOR MEMBER OR GDG SPECIFICATION  00132100
         DC    XL2'1'                                                   00132200
&K       SETA  K'&MEM                                                   00132300
         DC    XL2'&K'             LENGTH OF SPECFIED MEMBER NAME       00132400
DAMB&X   DC    C'&MEM'             THE MEMBER NAME IS PLUGGED IN HERE   00132500
&A       SETA  &A+1                                                     00132600
.B       AIF   (T'&STATUS EQ 'O').C                                     00132700
         DS    0F                                                       00132800
TS&A&X   DC    X'0004'             KEY FOR PRIMARY DISPOSITION OF DDN   00132900
         DC    XL2'1'                                                   00133000
         DC    XL2'1'              THIS IS ALWAYS A ONE BYTE CODE       00133100
&A       SETA  &A+1                                                     00133200
         AIF   ('&STATUS' EQ 'OLD').B1       OLD = X'01'                00133300
         AIF   ('&STATUS' EQ 'MOD').B2       MOD = X'02'                00133400
         AIF   ('&STATUS' EQ 'NEW').B3       NEW = X'04'                00133500
         AIF   ('&STATUS' EQ 'SHR').B4       SHR = X'08'                00133600
         MNOTE 8,'&STATUS INVALID FOR STATUS'                           00133700
         AGO   .C                                                       00133800
.B1      DC    X'1'                CONSTANT FOR DISP=OLD                00133900
         AGO   .C                                                       00134000
.B2      DC    X'2'                CONSTANT FOR DISP=MOD                00134100
         AGO   .C                                                       00134200
.B3      DC    X'4'                CONSTANT FOR DISP=NEW                00134300
         AGO   .C                                                       00134400
.B4      DC    X'8'                CONSTANT FOR DISP=SHR                00134500
         AGO   .C                                                       00134600
.C       AIF   (T'&DISP EQ 'O').D                                       00134700
         DS    0F                                                       00134800
TS&A&X   DC    X'0005'             KEY FOR NORMAL EOJ SPECIFICATION     00134900
&A       SETA  &A+1                                                     00135000
         DC    XL2'1'                                                   00135100
         DC    XL2'1'              THIS WILL ALWAYS BE A ONE BYTE KEY   00135200
         AIF   ('&DISP' EQ 'UNCATLG').C1     UNCATLG = X'01'            00135300
         AIF   ('&DISP' EQ 'CATLG').C2       CATLG   = X'02'            00135400
         AIF   ('&DISP' EQ 'DELETE').C3      DELETE  = X'04'            00135500
         AIF   ('&DISP' EQ 'KEEP').C4        KEEP    = X'08'            00135600
         MNOTE 8,'&DISP INVALID FOR DISP'    ALL ELSE IS WRONG          00135700
         AGO   .D                                                       00135800
.C1      DC    X'1'                CONSTANT FOR UNCATLG AT NORMAL EOJ   00135900
         AGO   .D                                                       00136000
.C2      DC    X'2'                CONSTANT FOR CATLG  AT NORMAL EOJ    00136100
         AGO   .D                                                       00136200
.C3      DC    X'4'                CONSTANT FOR DELETE AT NORMAL EOJ    00136300
         AGO   .D                                                       00136400
.C4      DC    X'8'                CONSTANT FOR KEEP   AT NORMAL EOJ    00136500
         AGO   .D                                                       00136600
.D       AIF   (T'&CDISP EQ 'O').E                                      00136700
         DS    0F                                                       00136800
TS&A&X   DC    X'0006'             KEY FOR ABNORMAL EOJ DISPOSITION     00136900
&A       SETA  &A+1                                                     00137000
         DC    XL2'1'                                                   00137100
         DC    XL2'1'              THIS IS ALWAYS A ONE BYTE KEY        00137200
         AIF   ('&CDISP' EQ 'UNCATLG').D1      UNCATLG = X'01'          00137300
         AIF   ('&CDISP' EQ 'CATLG').D2        CATLG   = X'02'          00137400
         AIF   ('&CDISP' EQ 'DELETE').D3       DELETE  = X'04'          00137500
         AIF   ('&CDISP' EQ 'KEEP').D4         KEEP    = X'08'          00137600
         MNOTE 8,'&CDISP INVALID FOR DISP'     ALL ELSE IS WRONG        00137700
         AGO   .E                                                       00137800
.D1      DC    X'1'                CONSTANT FOR UNCATLG IF ABEND        00137900
         AGO   .E                                                       00138000
.D2      DC    X'2'                CONSTANT FOR CATLG  IF ABEND         00138100
         AGO   .E                                                       00138200
.D3      DC    X'4'                CONSTANT FOR DELETE IF ABEND         00138300
         AGO   .E                                                       00138400
.D4      DC    X'08'               CONSTANT FOR KEEP   IF ABEND         00138500
         AGO   .E                                                       00138600
.E       AIF   (T'&TYPSP EQ 'O').I                                      00138700
         DS    0F                                                       00138800
         AIF   ('&TYPSP' EQ 'TRK').E1  IF TYPE OF SPACE SPECIFICATION   00138900
         AIF   ('&TYPSP' EQ 'CYL').E2  IS NOT TRACKS OR CYLINDERS THEN  00139000
TS&A&X   DC    X'0009'                 IT MUST BE IN BLOCKS             00139100
         DC    XL1'1'                                                   00139200
         DC    XL1'3'                                                   00139300
         DC    AL3(&TYPSP)                                              00139400
&A       SETA  &A+1                                                     00139500
         AGO   .F                                                       00139600
.E1      ANOP                                                           00139700
TS&A&X   DC    X'0007'             SPACE IS TO BE ALLOCATED IN TRKS     00139800
         DC    XL2'0'                                                   00139900
&A       SETA  &A+1                                                     00140000
         AGO   .F                                                       00140100
.E2      ANOP                                                           00140200
TS&A&X   DC    X'0008'             SPACE IS TO BE ALLOCATED IN CYLS     00140300
         DC    XL2'0'                                                   00140400
&A       SETA  &A+1                                                     00140500
.F       AIF   (T'&SPACE EQ 'O').FO                                     00140600
         DS    0F                                                       00140700
TS&A&X   DC    X'000A'             KEY TO SPECIFY PRIMARY SPACE         00140800
         DC    XL2'1'                                                   00140900
         DC    XL2'3'              MAXIMUM LENGTH IS THREE              00141000
DASP&X   DC    AL3(&SPACE)                                              00141100
&A       SETA  &A+1                                                     00141200
         AGO   .G                                                       00141300
.FO      MNOTE 8,'PRIMARY SPACE OMITTED'   TYPSP REQUIRES AT LEAST      00141400
         AGO   .I                  THE PRIMARY SPACE SPECIFICATION      00141500
.G       AIF   (T'&SECSP EQ 'O').H                                      00141600
         DS    0F                                                       00141700
TS&A&X   DC    X'000B'             KEY TO SPECIFY SECONDARY SPACE       00141800
         DC    XL2'1'                                                   00141900
         DC    XL2'3'              MAXIMUM LENGTH IS THREE              00142000
DASS&X   DC    AL3(&SECSP)                                              00142100
&A       SETA  &A+1                                                     00142200
.H       AIF   (T'&DIRSP EQ 'O').I                                      00142300
         DS    0F                                                       00142400
TS&A&X   DC    X'000C'             KEY TO SPECIFY DIRECTORY SPACE       00142500
         DC    XL2'1'                                                   00142600
         DC    XL2'3'              MAXIMUM LENGTH IS THREE              00142700
DADI&X   DC    AL3(&DIRSP)                                              00142800
&A       SETA  &A+1                                                     00142900
.I       AIF   (T'&VOLSER EQ 'O').J                                     00143000
         DS    0F                                                       00143100
TS&A&X   DC    X'0010'             KEY TO SPECIFY A PARTICULAR VOLSER   00143200
         DC    XL2'1'                                                   00143300
&K       SETA  K'&VOLSER           PICK UP LENGTH OF NAME SPECIFIED     00143400
         DC    XL2'&K'             AND USE THAT LENGTH                  00143500
DAVO&X   DC    C'&VOLSER'          AND NAME                             00143600
&A       SETA  &A+1                                                     00143700
.J       AIF   (T'&UNIT EQ 'O').K                                       00143800
         DS    0F                                                       00143900
TS&A&X   DC    X'0015'             KEY FOR UNIT SPECIFICATION           00144000
         DC    XL2'1'                                                   00144100
&K       SETA  K'&UNIT             PICK UP LENGTH OF UNIT NAME GIVEN    00144200
         DC    AL2(&K)             THEN USE THAT LENGTH                 00144300
DAUN&X   DC    C'&UNIT'            AND NAME                             00144400
&A       SETA  &A+1                                                     00144500
.K       AIF   (T'&SYSOUT EQ 'O').L                                     00144600
         DS    0F                                                       00144700
TS&A&X   DC    X'0018'             KEY FOR SYSOUT CLASS SPECIFICATION   00144800
         DC    XL2'1'                                                   00144900
&K       SETA  K'&SYSOUT                                                00145000
         DC    AL2(&K)                                                  00145100
DASY&X   DC    C'&SYSOUT'                                               00145200
&A       SETA  &A+1                                                     00145300
.L       AIF   (T'&FORM EQ 'O').M                                       00145400
         DS    0F                                                       00145500
TS&A&X   DC    X'001A'             KEY FOR FORMS ID SPECIFICATION       00145600
         DC    XL2'1'                                                   00145700
&K       SETA  K'&FORM             PICK UP LENGTH OF FORMS ID           00145800
         DC    AL2(&K)             AND USE THAT LENGTH                  00145900
DAFM&X   DC    C'&FORM'            AND THAT FORM NUMBER                 00146000
&A       SETA  &A+1                                                     00146100
.M       AIF   (T'&FREE EQ 'O').N                                       00146200
         AIF   ('&FREE' NE 'CLOSE').M2                                  00146300
         DS    0F                                                       00146400
TS&A&X   DC    X'001C'             KEY TO SPECIFY FREE=CLOSE            00146500
DACL&X   DC    XL2'0'              MUST BE CODED FREE=CLOSE             00146600
&A       SETA  &A+1                ALL ELSE WILL GENERATE MNOTE         00146700
         AGO   .N                                                       00146800
.M2      MNOTE 8,'&FREE INVALID FOR FREE'                               00146900
.N       AIF   (T'&COPIES EQ 'O').O                                     00147000
         DS    0F                                                       00147100
TS&A&X   DC    X'001D'             KEY TO SPECIFY MULTIPLE COPIES       00147200
         DC    XL2'1'              OF OUTPUT.                           00147300
         DC    XL2'1'                                                   00147400
DACO&X   DC    AL1(&COPIES)        NUMBER OF COPIES REQUESTED           00147500
&A       SETA  &A+1                                                     00147600
.O       AIF   (T'&LABEL EQ 'O').P                                      00147700
         DS    0F                                                       00147800
TS&A&X   DC    X'001E'             KEY TO SPECIFY TYPE LABEL            00147900
         DC    XL2'1'                                                   00148000
         DC    XL2'1'              LENGTH OF THIS FIELD IS ALWAYS ONE   00148100
&A       SETA  &A+1                                                     00148200
         AIF   ('&LABEL' EQ 'NL').O1     NL  = X'01'                    00148300
         AIF   ('&LABEL' EQ 'SL').O2     SL  = X'02'                    00148400
         AIF   ('&LABEL' EQ 'NSL').O3    NSL = X'04'                    00148500
         AIF   ('&LABEL' EQ 'SUL').O4    SUL = X'08'                    00148600
         AIF   ('&LABEL' EQ 'BLP').O5    BLP = X'10'                    00148700
         AIF   ('&LABEL' EQ 'LTM').O6    LTM = X'21'                    00148800
         AIF   ('&LABEL' EQ 'AL').O7     AL  = X'40'                    00148900
         AIF   ('&LABEL' EQ 'AUL').O8    AUL = X'48'                    00149000
         MNOTE 8,'&LABEL INVALID FOR LABEL'                             00149100
         AGO   .P                                                       00149200
.O1      DC    X'1'               LABEL = NL                            00149300
         AGO   .P                                                       00149400
.O2      DC    X'2'                LABEL = SL                           00149500
         AGO   .P                                                       00149600
.O3      DC    X'04'               LABEL = NSL                          00149700
         AGO   .P                                                       00149800
.O4      DC    X'08'               LABEL = SUL                          00149900
         AGO   .P                                                       00150000
.O5      DC    X'10'               LABEL = BLP                          00150100
         AGO   .P                                                       00150200
.O6      DC    X'21'               LABEL = LTM                          00150300
         AGO   .P                                                       00150400
.O7      DC    X'40'               LABEL = AL                           00150500
         AGO   .P                                                       00150600
.O8      DC    X'48'               LABEL = AUL                          00150700
         AGO   .P                                                       00150800
.P       AIF   (T'&EXPDT EQ 'O').Q                                      00150900
         DS    0F                                                       00151000
TS&A&X   DC    X'0022'             KEY TO SPECIFY EXPIRATION DATE       00151100
         DC    XL2'1'                                                   00151200
         DC    XL2'5'              FIELD IS FIVE NUMERICS               00151300
DAEX&X   DC    CL5'&EXPDT'         AND IS PUT IN HERE                   00151400
&A       SETA  &A+1                                                     00151500
.Q       AIF   (T'&RETPD EQ 'O').R                                      00151600
         DS    0F                                                       00151700
TS&A&X   DC    X'0023'             KEY TO SPECIFY A RETENTION PERIOD    00151800
         DC    XL2'1'                                                   00151900
         DC    XL2'2'              MAXIMUM LENGTH IS TWO                00152000
DARP&X   DC    AL2(&RETPD)         AND IS PLACED HERE.                  00152100
&A       SETA  &A+1                                                     00152200
.R       AIF   (T'&DUMMY EQ 'O').S                                      00152300
         DS    0F                                                       00152400
TS&A&X   DC    X'0024'             KEY TO ALLOCATE A DUMMY DATA SET     00152500
         DC    XL2'0'                                                   00152600
         DC    XL2'0'                                                   00152700
         DC    X'0'                                                     00152800
&A       SETA  &A+1                                                     00152900
.S       AIF   (T'&BLKSIZE EQ 'O').T                                    00153000
         DS    0F                                                       00153100
TS&A&X   DC    X'0030'             KEY TO SPECIFY THE BLOCKSIZE         00153200
         DC    XL2'1'                                                   00153300
         DC    XL2'2'                                                   00153400
DABS&X   DC    AL2(&BLKSIZE)                                            00153500
&A       SETA  &A+1                                                     00153600
.T       AIF   (T'&FUNC EQ 'O').U                                       00153700
         DS    0F                                                       00153800
         AIF   ('&FUNC' EQ 'I').T2                                      00153900
         MNOTE 8,'&FUNC INVALID FOR FUNC'                               00154000
         AGO   .U                                                       00154100
.T2      ANOP                                                           00154200
TS&A&X   DC    X'005A'                                                  00154300
         DC    XL2'1'                                                   00154400
         DC    XL2'1'                                                   00154500
         DC    X'80'                                                    00154600
&A       SETA  &A+1                                                     00154700
.U       AIF   (T'&LRECL EQ 'O').V                                      00154800
         DS    0F                                                       00154900
TS&A&X   DC    X'0042'             KEY TO SPECIFY THE LRECL             00155000
         DC    XL2'1'                                                   00155100
         DC    XL2'2'                                                   00155200
         DC    AL2(&LRECL)                                              00155300
&A       SETA  &A+1                                                     00155400
.V       AIF   (T'&RLSE EQ 'O').W                                       00155500
         DS    0F                                                       00155600
TS&A&X   DC    X'000D'             KEY  TO SPECIFY RELEASE SPACE        00155700
         DC    XL2'0'                                                   00155800
&A       SETA  &A+1                                                     00155900
.W       AIF   (T'&DCB EQ 'O').X                                        00156000
         DS    0F                                                       00156100
TS&A&X   DC    X'002C' KEY TO SPECIFY DCB REFERENCE NAME                00156200
         DC    XL2'1'                                                   00156300
&K       SETA  K'&DCB                                                   00156400
         DC    XL2'&K'             LENGTH OF DSN SPECIFIED              00156500
DCB&X    DC    C'&DCB'             DSNAME TO COPY DCB FROM              00156600
&A       SETA  &A+1                                                     00156700
.X       ANOP                                                           00156800
.*                                                                      00156900
.*                                                                      00157000
.*                                                                      00157100
.*                                                                      00157200
         DS    0F                                                       00157300
&A2      SETA  1                                                        00157400
&A       SETA  &A-1                                                     00157500
&NM      SETC  'TS&X'                                                   00157600
.CM1     AIF   (&A2 EQ &A).CM2                                          00157700
&NM      DC    A(TS&A2&X)                                               00157800
&NM      SETC  ' '                                                      00157900
&A2      SETA  &A2+1                                                    00158000
         AGO   .CM1                                                     00158100
.CM2     DC    X'80'                                                    00158200
         DC    AL3(TS&A2&X)                                             00158300
         MEND                                                           00158400
./ ADD NAME=DBGMSG   8001-74029-74029-1257-00014-00014-00000-HERC01  00
         MACRO                                                          00010000
&LABEL   DBGMSG &P1,&P2               , create debug message            00020000
         GBLC  &DEBUG                 , DEBUG controller                00030000
         AIF   ('&LABEL' EQ '').NOLAB                                   00040000
&LABEL   DS    0H                     , send debug message              00050000
.NOLAB   ANOP                                                           00060000
         AIF   ('&DEBUG' EQ 'NO').NODEBUG                               00070000
         MSGPUT MSG15I                , insert message body             00080000
         MVC   MSG15I1,&P1            , insert DDNAME to message        00090000
         MVC   MSG15I2,&P2            , insert function message         00100000
         BAL   R14,PUTMSG             , send message                    00110000
.NODEBUG ANOP                                                           00120000
         MEXIT                                                          00130000
         MEND                                                           00140000
./ ADD NAME=DDTBRK
         MACRO
&LABEL   DDTBRK &STRING
.* DDT V4 R0 M0
         LCLA  &DDTSVC#
.* DDT hard-coded breakpoint generation macro

.* If the default DDT SVC number has been changed, the following
.* value must be changed to match the SVC number being used by DDT
&DDTSVC# SETA  233                     DDT SVC number

&LABEL   NOP   DDT&SYSNDX
         SVC   &DDTSVC#
         DC    X'00DEAD'
         AIF   ('&STRING' NE '').STR
.* No text string
         DC    AL1(0)
         AGO   .DONE

.* Determine type of text string argument
.STR     ANOP
         AIF   ('&STRING'(1,1) EQ '(').REG

.* Embedded text string argument
         AIF   (K'&STRING LE 129).OUTSTR
         MNOTE 16,'Text string length greater than 127'
         AGO   .DONE
.OUTSTR  ANOP
         DC    AL1(L'STR&SYSNDX)
STR&SYSNDX DC  C&STRING
         AGO   .DONE

.* Register pointer to text string
.REG     ANOP
         AIF   (N'&STRING NE 1).REGERR
         DC    AL.4(8,&STRING(1))
         AGO   .DONE
.REGERR  ANOP
         MNOTE 16,'Invalid register specification'

.DONE    ANOP
DDT&SYSNDX DS  0H
         MEND
./ ADD NAME=DIAG
         MACRO
&LBL     DIAG  &OP1,&OP2,&OP3
         AIF   ('&LBL.X' EQ 'X').NOLAB
&LBL     DS    0H
.NOLAB   ANOP
         DC    X'83'
         DC    AL1(16*&OP1+&OP2)
         DC    AL2(&OP3)
         MEXIT
         MEND
./ ADD NAME=DO       8003-74021-74024-1511-00157-00151-00000-HERC01  00
         MACRO
&NAME    DO    &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&WHILE=,&FROM=,&C=
.*#-------------------------------------------------------------------*
.*#      DO    MACRO FOR STRUCTURED PROGRAMMING                       *
.*#
.*#   FUNCTION:    STARTS A NEW DO GROUP
.*#
.*#   CALL(1):     DO WHILE=COND1
.*#                   WHILE=COND1,OP1,COND2
.*#                   WHILE=COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CONDI : A VALID ASSEMBLER INSTRUCTION WITH
.*#                            MNEMOTECNIC CONDITION CODE (IN BRACKETS)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI:    IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' IN THE SAME
.*#                            DO GROUP.
.*#
.*#
.*#   CALL(2):     DO  FROM=(REG,INITVAL)
.*#
.*#                -   REG:    LOOP-REGISTER. IT CONTAINS THE NUMBER
.*#                            OF TIMES THE LOOP WILL BE EXECUTED
.*#                -   INITVAL: INITAL VALUE FOR THE LOOP REGISTER
.*#                            THIS PARAMETER MAY BE OMITTED. IN THIS
.*#                            CASE THE MACRO ASSUMES, THAT THE
.*#                            REGISTER IS ALREADY LOADED.
.*#
.*#
.*#   CALL(3):     DO  INF
.*#      OR        DO  FOREVER
.*#
.*#                AN INFINITE LOOP WILL BE GENERATED.
.*#                PLEASE USE THE 'EXIT' OR 'EXITIF' MACRO TO LEAVE
.*#                THE LOOP.
.*#
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLA  &N              TOTAL NESTING LEVEL
         LCLC  &OPND
         COPY  IFGLO
.*--------------------------------------------------------------------*
.*       FIRST DO/IF: INIT GLOBALS                                    *
.*--------------------------------------------------------------------*
         AIF   (&IFINIT).START            INIT ALREADY DONE
&IFINIT  SETB  1
&IFLEVEL SETA  0
&DOLEVEL SETA  0
&IFLABEL SETA  0
&IFLIMIT SETA  100000
&IFPRAEF SETC  '##'                    WAR #I
&IFDEBUG SETB  0
.START   ANOP
&MACNA   SETC  'DO'
.*--------------------------------------------------------------------*
.*       FORMAL TESTS                                                 *
.*--------------------------------------------------------23-09-80-RS-*
.FOR01   ANOP
         AIF   ('&WHILE' EQ '').FOR02     NO WHILE PARAM
         AIF   ('&FROM'  NE '').FEHL12    FROM AND WHILE SPECIFIED
&OPND    SETC  'WHILE='
         AGO   .FOR04
.*
.FOR02   ANOP
         AIF   ('&FROM' EQ '').FOR03      DO WITHOUT FROM/WHILE
         AIF   ('&FROM'(1,1) NE '(').FEHL13 NOT IN BRCKETS
         AIF   ('&P1' NE '').FEHL14       EXCESIVE PARAMETERS
&OPND    SETC  'FROM='
         AGO   .FOR04
.*
.FOR03   ANOP
         AIF   ('&P1' EQ 'FOREVER').FOR03A
         AIF   ('&P1' NE 'INF').FEHL15
.FOR03A  ANOP
         AIF   ('&P2' NE '').FEHL18
.FOR04   ANOP
.*--------------------------------------------------------------------*
.*       INCREMENT LEVEL. GENERATE LABELS FOR FALSE/TRUE              *
.*--------------------------------------------------------------------*
&DOLEVEL SETA  &DOLEVEL+1
         AIF   (&DOLEVEL EQ 50).FEHL06
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC   COM=START,&OPND,&WHILE&FROM,C=&C,                     *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
&OPND    SETC  ''
.*
.OBR00   ANOP
.*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
.*--------------------------------------------------------------------*
.*       SAVE NAME OF DO-GROUP FOR EXIT MACRO                         *
.*--------------------------------------------------------24-09-80-RS-*
&DONAME(&DOLEVEL) SETC '&NAME'
.*--------------------------------------------------------------------*
.*       FROM - CLAUSE                                                *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&P1' EQ 'INF').STA03     DO INFINITE
         AIF   ('&P1' EQ 'FOREVER').STA03     DO INFINITE
         AIF   ('&WHILE' NE '').STA03     NO FROM, SO WHILE
&DOFROM(&DOLEVEL) SETC '&FROM'            GET LOOP REGISTER
         AIF   ('&FROM(2)' EQ '').STA03   NO INITIAL VALUE
&DOFROM(&DOLEVEL) SETC '&FROM(1)'         GET LOOP REGISTER
         LA    &FROM(1),&FROM(2)          GET INITAL LOOP COUNT (DO)
.*--------------------------------------------------------------------*
.*       GENERATE START AND END LABEL                                 *
.*--------------------------------------------------------23-09-80-RS-*
.STA03   ANOP
&DOSTART(&DOLEVEL) SETC '&IFLABEL'
&OPND    SETC  '&IFPRAEF&DOSTART(&DOLEVEL)'
&OPND    DS    0H                         TARGET FOR DO-LOOP
.*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&DOENDLB(&DOLEVEL) SETC '&IFLABEL'        TARGET FOR END OF DO-LOOP
         AIF    ('&FROM' NE '').MACEND    DON'T CALL IF-PROCESSOR
         AIF    ('&P1' EQ  'INF').MACEND  DON'T CALL IF-PROCESSOR
         AIF    ('&P1' EQ  'FOREVER').MACEND  DON'T CALL IF-PROCESSOR
.*--------------------------------------------------------------------*
.*       GENERATE LABEL FOR BRANCH ON TRUE                            *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&DOTRUE  SETC  '&IFLABEL'
.*
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
         IFPRO &DOTRUE,&DOENDLB(&DOLEVEL),&WHILE,                      *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,                            *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL                                               *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&DOTRUE'
&OPND    DS    0H                        TARGET FOR BANCH ON NOT TRUE
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ELSE     8000-74024-74024-1511-00053-00053-00000-HERC01  00
         MACRO
&NAME    ELSE  &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*#  ELSE: MACRO FOR STRUCTURED PROGRAMMING                           *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*#  FUNCTION: IF ALL PRECEDING CONDITIONS IN THE 'IF' OR 'ELESIF'    *
.*#            MACROS OF THE SAME NESTING LEVEL TURN OUT TO BE        *
.*#            NOT FULLFILLED, THE CODE AFTER THE 'ELSE' MACRO        *
.*#            WILL BE EXECUTED.                                      *
.*#                                                                   *
.*#  CODING:   ELSE     (NO OPERANDS)                                 *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
         LCLA  &N
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH TO ENDIF                                     *
.*--------------------------------------------------------23-09-80-RS-*
.*
&MACNA   SETC  'ELSE'
.*
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC  COM=CONT,C=&C
.OBR00   ANOP
         AIF   ('&IFENDLB(&IFLEVEL)' NE '').NOEND  KEIN ENDIF
&IFLABEL SETA  &IFLABEL+1
.*
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFENDLB(&IFLEVEL) SETC '&IFLABEL'
.*
.NOEND   ANOP
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
         B     &OPND                       BRANCH TO ENDIF
.*--------------------------------------------------------------------*
.*       GENERATE TARGET FOR BRANCH ON ELSE                           *
.*--------------------------------------------------------23-09-80-RS-*
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR BRANCH ON ELSE
.*--------------------------------------------------------------------*
.*       SIGNAL TO ENDIF: GENERATE NO ELSE LABEL                      *
.*--------------------------------------------------------23-09-80-RS-*
&IFFALSE(&IFLEVEL) SETC ''
         AGO   .MACEND
.*
         COPY  IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ELSEIF   8000-74024-74024-1512-00089-00089-00000-HERC01  00
         MACRO
&NAME   ELSEIF &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&C=
.*#-------------------------------------------------------------------*
.*#     ELSEIF MACRO FOR STRUCTURED PROGRAMMING                       *
.*#----------------------------------------------------23-09-80-RS----*
.*#
.*#   FUNCTION:    STANDS ON THE PLACE OF AN 'ELSE' AND STARTS A NEW
.*#                CONDITION CLAUSE
.*#
.*#   MODEL:       ELSEIF  COND1
.*#                        COND1,OP1,COND2
.*#                        COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CONDI : A VALID ASSEMBLE INSTRUCTION WITH
.*#                            MENOTECNIC CONDITION CODE (IN BRACKETS)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI:    IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' OPERANDS IN
.*#                            THE SAME ELSEIF.
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLA  &N              TOTAL NESTING LELVEL
         LCLC  &OPND
         COPY  IFGLO
.*
&MACNA   SETC  'ELSEIF'
.*
         AIF   ('&P1' EQ '').FEHL17      FORMAL TEST
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH TO ENDIF                                     *
.*--------------------------------------------------------------------*
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO .OBR00
&NAME    SVDOC   COM=CONT,C=&C,                                        *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
.OBR00   ANOP
         AIF   ('&IFENDLB(&IFLEVEL)' NE '').NOEND  KEIN ENDIF
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFENDLB(&IFLEVEL) SETC '&IFLABEL'
.*
.NOEND   ANOP
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
         B     &OPND                       BRANCH TO ENDIF
.*--------------------------------------------------------------------*
.*       GENERATE TARGET FOR BRANCH ON ELSE                           *
.*--------------------------------------------------------23-09-80-RS-*
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR BRANCH ON ELSE
.*--------------------------------------------------------------------*
.*       GENERATE NEXT ELSE LABEL                                     *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
.*
&IFFALSE(&IFLEVEL) SETC '&IFLABEL'         ID FOR NEXT ELSE ON LEVEL
.*--------------------------------------------------------------------*
.*       GENERATE TRUE LABEL                                          *
.*--------------------------------------------------------23-09-80-RS-*
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFTRUE  SETC  '&IFLABEL'                  TRUE LABEL FOR AND /OR
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
         IFPRO &IFTRUE,&IFFALSE(&IFLEVEL),&P1,&P2,&P3,&P4,&P5,&P6,&P7, *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL                                               *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&IFTRUE'
&OPND    DS    0H                        TARGET FOR BANCH ON NOT TRUE
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ENDDO    8000-74024-74024-1512-00066-00066-00000-HERC01  00
         MACRO
&NAME    ENDDO &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*# ENDDO: CLOSE A DO GROUP IN STRUCTURED PROGRAMMING                 *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*# FUNCTION: CLOSES A DO-LOOP   (= DO GROUP)                         *
.*#                                                                   *
.*#                                                                   *
.*# CODING:   ENDDO           (NO PARAMETERS)                         *
.*#                                                                   *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
         LCLA  &N
.*
&MACNA   SETC  'ENDDO'
.*
         AIF   (&DOLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.*
.OBR20   ANOP
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=END,C=&C
.OBR00   ANOP
.*--------------------------------------------------------------------*
.*       BRANCH BACK TO START IF FROM-REGISTER IS NOT ZERO            *
.*--------------------------------------------------------23-09-80-RS-*
.*
         AIF   ('&DOFROM(&DOLEVEL)' EQ '').WHILE
&OPND    SETC  '&DOFROM(&DOLEVEL),&IFPRAEF&DOSTART(&DOLEVEL)'
         BCT   &OPND                       BRANCH BACK TO START OF LOOP
         AGO   .LOOPEND
.*--------------------------------------------------------------------*
.*       BRANCH BACK TO START OF LOOP IN CASE OF WHILE CONTROL        *
.*--------------------------------------------------------23-09-80-RS-*
.WHILE   ANOP
&OPND    SETC  '&IFPRAEF&DOSTART(&DOLEVEL)'
         B     &OPND                       BRANCH BACK TO START OF LOOP
.*--------------------------------------------------------------------*
.*       GENERATE TARGET LABEL TO LEAVE THE LOOP (PSEUDO ELSE)        *
.*--------------------------------------------------------23-09-80-RS-*
.LOOPEND ANOP
&OPND    SETC  '&IFPRAEF&DOENDLB(&DOLEVEL)'
&OPND    DS    0H                          TARGET TO LEAVE THE LOOP
.*--------------------------------------------------------------------*
.*       RESET FUNCTIONS FOR THIS DO LEVEL                            *
.*--------------------------------------------------------23-09-80-RS-*
.RESET   ANOP
&DOTRUE  SETC  ''
&DOFALSE(&DOLEVEL) SETC ''
&DOENDLB(&DOLEVEL) SETC ''
&DOSTART(&DOLEVEL) SETC ''
&DOFROM(&DOLEVEL)  SETC ''
&DONAME(&DOLEVEL)  SETC ''
&DOLEVEL SETA  &DOLEVEL-1
         AGO   .MACEND
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ENDIF    8000-74024-74024-1512-00053-00053-00000-HERC01  00
         MACRO
&NAME    ENDIF &COMMENT,&C=
.*#-------------------------------------------------------------------*
.*# ENDIF: MACRO  CLOSE CURRENT IF LEVEL                              *
.*#-------------------------------------------------------------------*
.*#                                                                   *
.*# FUNCTION: THE ACTUAL 'IF' LEVEL WILL BE CLOSED.                   *
.*#                                                                   *
.*# CODING:   ENDIF      (NO OPERANDS)                                *
.*#                                                                   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &OPND
         LCLA  &N
.*
&MACNA   SETC  'ENDIF'
.*
.*--------------------------------------------------------------------*
.*       IF WITHOUT ELSE: GENERATE ELSE LABEL                         *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   (&IFLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.*
.OBR20   ANOP
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=END,C=&C
.OBR00   ANOP
         AIF   ('&IFFALSE(&IFLEVEL)' EQ '').NOELSE
&OPND    SETC  '&IFPRAEF&IFFALSE(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR ELSE BRANCH
.NOELSE  ANOP
.*--------------------------------------------------------------------*
.*       GENERATE ENDIF LABEL IF NECESSARY                            *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&IFENDLB(&IFLEVEL)' EQ '').NOENDIF
&OPND    SETC  '&IFPRAEF&IFENDLB(&IFLEVEL)'
&OPND    DS    0H                          TARGET FOR ENDIF BRANCH
.NOENDIF ANOP
.*--------------------------------------------------------------------*
.*       RESET FUNCTIONS OF THIS IF-LEVEL                             *
.*--------------------------------------------------------23-09-80-RS-*
&IFTRUE  SETC  ''
&IFFALSE(&IFLEVEL) SETC ''
&IFENDLB(&IFLEVEL) SETC ''
&IFLEVEL SETA &IFLEVEL-1
         AGO   .MACEND
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=ENTER    0107-07076-08014-1637-00304-00331-00000-T4      00
         MACRO
&LABEL   ENTER &MSG,            Eye catcher for load module            C
               &BASE=10,        Base register                          C
               &FLOAT=NO,       Equate floating point registers        C
               &GPR=YES,        Equate general purpose registers       C
               &LOC=RES,        Getmain location in virtual storage    C
               &LV=72,          Length of getmained area (default)     C
               &ORG=C,          Location counter                       C
               &PL=,            Global flag                            C
               &RENT=NO,        Re-entrant?                            C
               &AMODE=24,       Addressing mode for CSECT              C
               &RMODE=24,       Residency mode for CSECT               C
               &CLEAR=YES,      Clear Getmained area to binary zeros   C
               &GMTYPE=R,       Getmain register type: R, RU, RC       C
               &ASMINFO=YES,    Have ICATCHER insert Assemble info?    C
               &DATEFMT=OLD,    Date format used by ICATCHER           C
               &RSA=,           Register save area                     C
               &SP=0            Getmain subpool number
.*                                                                   *
.*  MSG  'TEXT STRING'                                               *
.*       THIS OPTIONAL PARAMETER WILL OVERRIDE THE DEFAULT STANDARD  *
.*       OS FORMAT MODULE IDENTIFIER GENERATED BY THE ICATCHER MACRO.*
.*       ENTER USES ICATCHER TO GENERATE THE MODULE IDENTIFIER.      *
.*       SEE THE DESCRIPTION OF THE MSG PARAMETER IN THE PROLOGUE    *
.*       MACRO FOR CODING DETAILS.                                   *
.*                                                                   *
.*  BASE={REG�(REG0,REG1,...,REGN)�NO}                               *
.*       BASE REGISTER:  REGISTER SPECIFIED WILL BE USED AS A BASE   *
.*       REGISTER.  DEFAULT IS REGISTER 12.  RANGE 2 - 12.  IF THE   *
.*       LIST FORM OF THE OPERAND IS USED, A USING STATEMENT HAVING  *
.*       ALL THE INDICATED REGISTERS WILL BE GENERATED.  IN THE LIST *
.*       FORM, THE FIRST REGISTER IS CONSIDERED TO BE THE FIRST BASE *
.*       REGISTER.  ALL BASE REGISTERS ARE INITIALIZED.  NO BASE     *
.*       REGISTER IS ESTABLISHED IF BASE=NO IS CODED; ALL OTHER      *
.*       ACTIONS OCCUR AS DESCRIBED.                                 *
.*  LV={LITERAL�EXPRESSION}                                          *
.*       THIS PARAMETER IS APPLICABLE ONLY IF RENT=YES IS CODED.     *
.*       THE VALUE OF THE LITERAL OR EXPRESSION IS USED IN AN R-FORM *
.*       GETMAIN.  THE FIRST 72 BYTES OF THE AREA ARE USED FOR A     *
.*       REGISTER SAVE AREA.  REGISTER 13 WILL BE LOADED WITH THE    *
.*       ADDRESS OF THE AREA.                                        *
.*       LITERAL:  A VALUE FROM 72 TO 4096 INCLUSIVE.                *
.*       EXPRESSION:  ANY VALID ASSEMBLER EXPRESSION OF ANY VALUE.   *
.*  RENT={YES�NO}                                                    *
.*       YES:  GENERATE GETMAIN TO ACQUIRE SAVE AREA.  RENT=YES WILL *
.*       FACILITATE THE USE OF REENTRANT CODING TECHNIQUES.          *
.*       NO:   GENERATE STANDARD IN-LINE 18 WORD SAVE AREA.          *
.*  ORG={L�C}                                                        *
.*       L:  USE CURRENT LOCATION COUNTER VALUE IN USING STATEMENT   *
.*       (E.G.,   USING *,R12   ).                                   *
.*       C:  USE CSECT LABEL (&SYSECT) AS ORIGIN FOR USING STATEMENT *
.*       (E.G.,   USING &SYSECT,R12   ).                             *
.*  PL={ �NO}                                                        *
.*       NO:   THE PROLOG GLOBAL FLAG IS NOT CHECKED, AND THE        *
.*       ICATCHER MACRO IS NOT INVOKED.  THIS PARAMETER SHOULD ONLY  *
.*       BE SPECIFIED WHEN NO MESSAGE AND CSECT GENERATION IS        *
.*       REQUIRED, AND/OR NO ICATCHER MACRO HAS BEEN INVOKED,        *
.*       DIRECTLY OR INDIRECTLY, SINCE THE LAST OCCURRENCE OF ENTER. *
.*                                                                   *
.*** AMODE, RMODE,  ADDED FOR MVS/XA SUPPORT 85.122 -DGA-          ***
.*                                                                   *
.*  AMODE={ �24�31�ANY}                                              *
.*       24:  THE PROGRAM IS DESIGNED TO RECEIVE CONTROL IN 24-BIT   *
.*            MODE.                                                  *
.*       31:  THE PROGRAM IS DESIGNED TO RECEIVE CONTROL IN 31-BIT   *
.*            MODE.                                                  *
.*       ANY: THE PROGRAM IS DESIGNED TO RECEIVE CONTROL IN EITHER   *
.*            24-BIT OR 31-BIT MODE.                                 *
.*  RMODE={ �24�31�ANY}                                              *
.*       24:  THE PROGRAM IS DESIGNED TO RESIDE BELOW THE 16-MEG     *
.*            LINE.                                                  *
.*       ANY: THE PROGRAM IS DESIGNED TO RESIDE IN ANY VIRTUAL       *
.*            STORAGE LOCATION, EITHER ABOVE OR BELOW THE 16-MEG     *
.*            LINE.                                                  *
.*** LOC  ADDED FOR MVS/XA SUPPORT 1985.210 -DGA-                  ***
.*                                                                   *
.*  LOC ={BELOW � (BELOW,ANY) � ANY � (ANY,ANY) � RES � (RES,ANY)}   *
.*  LOCATION IN VIRTUAL AND REAL STORAGE TO ALLOCATE.                *
.*  (SEE GC28-1154 MVS/XA SUPERVISOR SERVICES AND MACRO INSTRUCTIONS *
.*   GETMAIN ... OPERAND LOC)
.*            LINE.                                                  *
.*                                                                   *
.*** SP - Getmain sub-pool                                         ***
.*          See SYSTEM MACROs manual for more info                   *
.*                                                                   *
.*** GMTYPE={R, RU, RC}                                            ***
.*          Getmain Register Type (See SYSTEM MACROS)                *
.*                                                                   *
.*** CLEAR={YES � NO}                                              ***
.*         Clear getmained area to binary zeros.  R2,R3,R4 and R5    *
.*         are modified.                                             *
.*                                                                   *
.*** RSA=                                                          ***
.*         Use Register Save Area at the location pointed to by      *
.*         this field.                                               *
.*                                                                   *
.*** DATEFMT= ICATCHER parm                                        ***
.*** ASMINFO= ICATCHER parm                                        ***
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
.*                                                                   *
.*  EXAMPLES:                                                        *
.*                                                                   *
.* TEST1 ENTER BASE=8,LV=300,RENT=YES                                *
.*     . R8 IS BASE, GETMAIN 300 BYTES, CSECT NAME IS TEST1,         *
.*                                                                   *
.* TEST2 ENTER                                                       *
.*     . R12 IS BASE, IN-LINE SAVEAREA, CSECT NAME IS TEST2,         *
.*                                                                   *
.*       ENTER 'VERSION 1',BASE=(10,11),RENT=YES,LV=WORKEND-WORK     *
.*     . R10,R11 ARE BASES, GETMAIN AL4(WORKEND-WORK) BYTES,         *
.*     . CSECT NAME DEFAULTS TO MAIN                                 *
.*                                                                   *
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
.*  DEFINE GLOBAL AND LOCAL SET SYMBOLS
.*
         GBLC  &VIRT
         GBLB  &RE,&REGS,&PROLOG,&ENTER31
.*
         LCLA  &B1,&BCT,&BICT
         LCLB  &ALV,&CLV,&BT(16),&OT
         LCLC  &BO,&LAB
.*
.**********************************************************************
.***                                                                ***
.**      "ICATCHER" MACRO PROCESSING                                 **
.***                                                                ***
.**********************************************************************
.*
.*
.** SET GLOBAL FOR LEAVE MACRO IF WE ARE FOR 31-BIT MODE
.*
         AIF   ('&AMODE' NE '31').NO31MDE
&ENTER31 SETB  1                 SET GLOBAL ENTER 31-BIT FLAG
.NO31MDE ANOP  ,
.*
.** GENERATE MODULE IDENTIFICATION Prologue.
.*
&LAB     SETC  '&SYSECT'         SET CSECT NAME
         AIF   ((&PROLOG) OR ('&PL' EQ 'NO')).E000A SKIP PROLOGUE
&LAB     SETC  '&LABEL'          SET CSECT TO LABEL NAME
         AIF   ('&LABEL' NE '').E000  GEN PROLOGUE IF &LABEL EXISTS
         MNOTE 0,'CSECT NAME OMMITED - CSECT NAME DEFAULTS TO MAIN'
&LAB     SETC  'MAIN'            SET CSECT TO MAIN
.E000    ANOP  ,
         ICATCHER &LAB,&MSG,RMODE=,AMODE=,                             C
               ASMINFO=&ASMINFO,DATEFMT=&DATEFMT
.*
.**********************************************************************
.***                                                                ***
.**      EXPAND REGISTERS AND SET CONSTANTS                          **
.***                                                                ***
.**********************************************************************
.*
.*
.** GENERATE STANDARD REGISTER EQUATES IF GLOBAL FLAG &REGS = 0
.*
.E000A   ANOP
&PROLOG  SETB  0                 RESET GLOBAL PROLOGUE FLAG
         IEZREGS
         IEZBITS
.*
.*       SET CONSTANTS AND LOGICAL SWITCHES
.*
.E001    ANOP  ,
&OT      SETB  ('&ORG' EQ 'L')               SET ORIGIN TYPE FLAG
&RE      SETB  ('&RENT' EQ 'YES')            SET REENTRANT FLAG
&BCT     SETA  N'&BASE                       SET NUMBER OF BASE REGS
&BICT    SETA  1                             SET BASE REG COUNTER
&ALV     SETB  (('&LV' GE '72') AND ('&LV' LE '4096')) SET LV FLAG
&CLV     SETB  ('&LV'(1,1) LE '0')           SET LV FLAG
.*
.** IF BOTH RENT AND RSA ARE CHOSEN, USE ONLY RSA...
.*
         AIF   (NOT &RE).EIRX             .RENT SPECIFIED?
         AIF   ('&RSA' EQ '').EIRX        .BOTH SPECIFIED?
&RE      SETB  0                          SET REENTRANT FLAG
.EIRX    ANOP  ,
.*
.*  SAVE CALLER'S REGISTERS
.*
         STM   R14,R12,12(R13)            SAVE CALLER'S REGISTERS
.*
.**********************************************************************
.***                                                                ***
.**      HANDLE BASE REGISTERS                                       **
.***                                                                ***
.**********************************************************************
.*
.E002    ANOP
         AIF   ('&BASE' EQ 'NO').E003        SKIP, IF NO BASE REG
         AIF   (&BCT NE 1).E002B             DECIDE WHICH BASE REG RTN
.*
.*  SINGLE BASE REGISTER SPECIFIED
.*
.E002A   ANOP
&B1      SETA  &BASE
&BO      SETC  '&B1'                    SET STRING FOR USING
         AIF   ((&B1 GE 2) AND (&B1 LE 12)).E002A1
         MNOTE 4,'BASE REGISTER SPECIFIED IS INVALID - USING R12'
&B1      SETA  12
.E002A1  ANOP
         AIF   (&OT).E002A2
         LR    R&B1,R15                   LOAD BASE REGISTER
         USING &LAB,R&B1                  GET ADDRESSIBLITY
         AGO   .E003
.E002A2  ANOP
         BALR  R&B1,0                     LOAD BASE REGISTER
         USING *,R&B1                     GET ADDRESSIBLITY
         AGO   .E003
.*
.*  MULTIPLE BASE REGISTERS SPECIFIED
.*
.E002B   ANOP
&B1      SETA  &BASE(&BICT)
         AIF   (NOT &BT(&B1+1)).E002C        CHECK REDUNDANCY
         MNOTE 8,'REG &B1 REDUNDANTLY SPECIFIED - GEN SUPPRESSED'
         MEXIT
.E002C   ANOP
&BT(&B1+1) SETB  1                           SET REG USE BIT
         AIF   ((&B1 GE 2) AND (&B1 LE 12)).E002D
         MNOTE 8,'INVALID SECONDARY BASE REGISTER - GEN SUPPRESSED'
         MEXIT
.E002D   ANOP
&BICT    SETA  &BICT+1
         AIF   (&BICT LE &BCT).E002B      .CHECK NEXT BASE REG
&BO      SETC  '&BASE'(2,K'&BASE-2)       .DONE, SET STRING FOR USING
&BICT    SETA  1                          .SET LOOP CONTROL
         AIF   (&OT).E002E1
         LR    R&BASE(1),R15              LOAD BASE REGISTER
         USING &LAB,&BO                   GET ADDRESSIBILITY
         AGO   .E002E
.E002E1  ANOP
         BALR  R&BASE(1),0                LOAD BASE REGISTER
         USING *,&BO                      GET ADDRESSIBILITY
.E002E   ANOP
&BICT    SETA  &BICT+1                    .INCREMENT LOOP CONTROL
&B1      SETA  &BASE(&BICT)
         AIF   (NOT &BT(&B1+1)).E002E
         LA    R&B1.,4095                 SECONDARY
         LA    R&B1.,1(R&B1.,R&BASE(&BICT-1)) BASE REGISTER
         AIF   (&BICT LT &BCT).E002E         SET UP NEXT BASE REG
.*
.**********************************************************************
.***                                                                ***
.**      SET UP SAVE AREA                                            **
.***                                                                ***
.**********************************************************************
.*
.E003    ANOP
         AIF   (NOT &RE).E003B            .NOT REENTRANT?
.*
.*  RENT=YES:  ISSUE GETMAIN FOR REENTRANT PROG
.*
&VIRT    SETC  '&LV'
         AIF   (&ALV OR &CLV).E003A
         MNOTE 4,'LV SPECIFIED IS INVALID - USING 72 BYTES'
&VIRT    SETC  '72'
.E003A   ANOP
         MNOTE  '         GETMAIN &GMTYPE,LV=&VIRT,SP=&SP'
         GETMAIN &GMTYPE,LV=&VIRT,SP=&SP
         AIF   ('&CLEAR' NE 'YES').E004
*
         LR    R2,R1                      ADDRESS OF GETMAINED AREA
         LR    R3,R0                      LENGTH OF GETMAINED AREA
         SLR   R5,R5                      SET PAD AND LENGTH
         MVCL  R2,R4                      CLEAR GETMAINED AREA TO 0
*
         AGO   .E004                      SKIP RENT=NO PROCESSING
.*
.** RENT=NO:  ESTABLISH DS TYPE SAVE AREA IF RSA= FIELD NOT SPECIFIED
.*
.E003B   ANOP
         AIF   ('&RSA' NE '').ERSA        .IS RSA SUPPLIED?
         CNOP  0,4                        ALIGN FULL WORD
         BAL   R1,*+76                    BRANCH AROUND PROGRAM RSA
         DC    18F'-1'                    RSA (REGISTER SAVE AREA)
         AGO   .E004                      .MOVE ON
.*
.ERSA    ANOP  ,
         AIF   ('&RSA'(1,1) EQ '(').ERSAR .REGISTER FORM?
         LA    R1,&RSA                    LOCATE SUPPLIED SAVE AREA
         AGO   .E004                      .MOVE ON
.ERSAR   ANOP  ,
         LR    R1,&RSA(1)                 LOCATE SUPPLIED SAVE AREA
.ERSARX  ANOP  ,
.*
.**********************************************************************
.***                                                                ***
.**    COMPLETE MACRO: SET UP FORWARD/BACKWARD CHAIN POINTERS        **
.***                                                                ***
.**********************************************************************
.*
.E004    ANOP
         ST    R1,8(,R13)                 SET FORWARD RSA POINTER
         ST    R13,4(,R1)                 SET BACKWARD RSA POINTER
         LR    R13,R1                     GET PROGRAM RSA ADDRESS
         L     R1,4(,R1)                  GET CALLER'S RSA ADDRESS
         AIF   (NOT &RE).E004A            DON'T RECOVER R0 IF NOT REENT
         LM    R0,R1,20(R1)               RESTORE REGISTERS 0, 1
         MEXIT ,
.E004A   ANOP
         L     R1,24(,R1)                 RESTORE REGISTER 1
.*
         MEND
./ ADD NAME=EXIT     8000-74024-74024-1513-00068-00068-00000-HERC01  00
         MACRO
&NAME    EXIT  &DO=,&C=
.*#-------------------------------------------------------------------*
.*#   EXIT     MACRO FOR STRUCTURED PROGRAMMING                       *
.*#-------------------------------------------------------------------*
.*#
.*#   FUNCTION:    UNCONDITIONAL EXIT OF ONE OR MORE DO GROUPS.
.*#
.*#   SYNTAX       EXIT      : EXITS CURRENT DO GROUP
.*#
.*#                EXIT  DO=DOGROUP
.*#
.*#                -   DOGROUP: AN ASSEMBLER LABEL OF A DO-GROUP
.*#                            EXITS THE DO GROUP WITH THIS LABEL
.*#                            EXAMPLE: EXIT DO=FIRST
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX DONAME SCANNING
         LCLA  &N              INDEX PRINTING NAME
         LCLC  &OPND
         COPY  IFGLO
.*
&MACNA   SETC  'EXIT'
.*
         AIF   (&DOLEVEL GT 0).OBR20
    MNOTE 12,' ??????  ADIOS LOGIC !!!,     EMERGENCY MESSAGE FROM STRU*
               CTURED PROGRAMMING SYSTEM.'
         MEXIT
.OBR20   ANOP
.**&N       SETA  &DOLEVEL+&IFLEVEL           GENERATE LEVEL MESSAGE
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
.**         MNOTE *,'&N CONT'                 GENERATE LEVEL MESSAGE
&NAME    SVDOC   COM=CONT,&DO,C=&C
.OBR00   ANOP
         AIF   ('&DO' EQ '').NONAME        NAME OF DO GROUP SPECIFIED
.*--------------------------------------------------------------------*
.*       LOOK UP NAME-TABLE FOR CURRENT NAME                          *
.*--------------------------------------------------------------------*
&I       SETA  1
.LOOP    ANOP
         AIF   (&I GT &DOLEVEL).FEHL19
         AIF   ('&DO' EQ '&DONAME(&I)').ENDLOOP  THE NAME IS IN TABLE
&I       SETA  &I+1                              NEXT ELEMENT
         AGO  .LOOP
.ENDLOOP ANOP
         AGO  .GENER
.*--------------------------------------------------------------------*
.*       EXIT CURRENT LOOP                                            *
.*--------------------------------------------------------------------*
.NONAME  ANOP
&I       SETA  &DOLEVEL
.GENER   ANOP
.*
.*--------------------------------------------------------------------*
.*  GENERATE CODE TO EXIT THE SPECIFIED LOOP                          *
.*--------------------------------------------------------------------*
&OPND    SETC  '&IFPRAEF&DOENDLB(&I)'
         B     &OPND                     LEAVE THE LOOP &DO
.*
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=FC       8000-74023-74023-2143-00018-00018-00000-HERC02  00
         MACRO                                                          00010000
         FC    &X                                                       00020000
         LCLC  &C                                                       00030000
         GBLA  &COUNT                                                   00040000
&COUNT   SETA  &COUNT+1                                                 00050000
         AIF   (&COUNT GT 9000).SETB                                    00060000
&C       SETC  'A&SYSNDX'                                               00070000
         AGO   .TESTIT                                                  00080000
.SETB    ANOP                                                           00090000
         AIF   (&COUNT GT 18000).SETC                                   00100000
&C       SETC  'B&SYSNDX'                                               00110000
         AGO   .TESTIT                                                  00120000
.SETC    ANOP                                                           00130000
&C       SETC  'C&SYSNDX'                                               00140000
.TESTIT  ANOP                                                           00150000
         DC    AL1(L'&C)              , LENGTH OF FORTUNE COOKIE        00160000
&C       DC    C&X                    , FORTUNE COOKIE TEXT             00170000
         MEND                                                           00180000
./ ADD NAME=FILL
         MACRO
&NAME    FILL  &AREA
&NAME    DC      CL(&AREA+L'&AREA-*)' '
         MEND
./ ADD NAME=FINISH   0105-07076-07318-1624-00210-00236-00018-T2      00
         MACRO ,                                                        00000100
&LABEL   FINISH &RC=0,                    Return code                  C00000202
               &LV=,                      Level to freemain            C00000302
               &RR=,                      Return registers             C00000402
               &FMTYPE=R,                 FREEMAIN type                C00000502
               &RENT=NO,                  Re-entrant?                  C00000602
               &GPR=YES,                  General Purpose registers?   C00000702
               &SP=0                      Sub-pool to freemain          00000800
.*                                                                   *  00000900
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00001000
.*                                                                   *  00001100
.*  RC={LITERAL�(REG)}                                               *  00001200
.*       RETURN CODE:  SPECIFY LITERAL 0 - 4095 OR SINGLE REGISTER   *  00001300
.*       (2) - (12) OR (15), TO BE RETURNED TO CALLER IN R15.        *  00001400
.*       DEFAULT RETURN CODE IS ZERO.                                *  00001500
.*                                                                   *  00001600
.*  RR={(REG,...,REGN)}                                              *  00001700
.*       RETURN REGISTER:  CONTENTS OF REGISTERS IN LIST WILL BE     *  00001800
.*       RETURNED TO CALLER (I.E., CALLERS REGISTERS CORRESPONDING   *  00001903
.*       TO REGISTERS SPECIFIED IN LIST WILL NOT BE RESTORED).       *  00002000
.*       SPECIFY REGISTERS 2 - 12 OR 0.  CAPABILITY TO RETURN R0 IS  *  00002100
.*       PROVIDED FOR FORTRAN FUNCTION COMPATIBILITY.  IT SHOULD BE  *  00002200
.*       NOTED THAT IF RR=(0) AND RENT=YES ARE BOTH SELECTED, THEN   *  00002300
.*       THE CONTENTS OF R0 MUST BE STORED IN, AND SUBSEQUENTLY      *  00002400
.*       RESTORED FROM, THE CALLERS SAVE AREA, SINCE THE FREEMAIN    *  00002500
.*       SVC ALTERS THE CONTENTS OF R0.                              *  00002600
.*                                                                   *  00002700
.*  LV={LITERAL}                                                     *  00002800
.*       LITERAL VALUE:  IF RENT=YES IS CODED, THE LITERAL VALUE IS  *  00002900
.*       THE SIZE OF THE AREA TO BE FREED BY THE FREEMAIN SVC.       *  00003000
.*       PARAMETER LV IS NOT REQUIRED IF ENTER MACRO HAS             *  00003101
.*       PREVIOUSLY BEEN USED.  IT IS OBTAINED THROUGH GLOBAL        *  00003200
.*       VARIABLE &VIRT.  IF RENT=NO IS CODED OR ALLOWED TO DEFAULT, *  00003300
.*       THE LV PARAMETER IS IGNORED.                                *  00003400
.*                                                                   *  00003500
.*  RENT={YES�NO}                                                    *  00003600
.*       YES:  GENERATE FREEMAIN TO FREE STORAGE ACQUIRED BY ENTER   *  00003701
.*       MACRO.  IF THE LEAVE MACRO IS BEING USED TO FREE AN AREA    *  00003800
.*       THAT WAS NOT ACQUIRED BY THE ENTER MACRO, THEN THE LV       *  00003901
.*       PARAMETER MUST ALSO BE SPECIFIED.                           *  00004000
.*       NO:   NO FREEMAIN SVC REQUIRED.                             *  00004100
.*                                                                   *  00004200
.*  GPR={YES�NO}                                                     *  00004300
.*       YES:  INVOKE REQUATE TO EQUATE REGISTERS                    *  00004400
.*       NO:   DON'T INVOKE REQUATE TO EQUATE REGISTERS              *  00004500
.*                                                                   *  00004600
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00004700
.*                                                                   *  00004800
.*  EXAMPLES:                                                        *  00004900
.*       LEAVE RC=32,RR=(9,4),LV=300,RENT=YES                        *  00005001
.*       LEAVE RR=(0),RENT=YES                                       *  00005101
.*       LEAVE RC=(5),RR=(6)                                         *  00005201
.*       LEAVE                                                       *  00005301
.*                                                                   *  00005400
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00005500
.*                                                                      00005600
.*  DEFINE GLOBAL AND LOCAL SET SYMBOLS                                 00005700
.*                                                                      00005800
         GBLC  &VIRT                                                    00005900
         GBLB  &RE,&REGS,&ENTER31                                       00006000
.*                                                                      00006100
         LCLA  &R1,&R2,&RRT,&RICT                                       00006200
         LCLB  &ALV,&CLV,&RT(16),&RCR,&CC15                             00006300
         LCLC  &DISP,&CC,&CCT,&NDX,&LAB                                 00006400
.*                                                                      00006500
.** REGISTER EQUATES                                                    00006600
.*                                                                      00006700
         AIF   ('&GPR' EQ 'NO').L000                                    00006800
         AIF   ('&GPR' EQ 'YES').REQ00                                  00006900
         MNOTE ,'GPR=&GPR. IS INVALID, YES ASSUMED'                     00007000
.REQ00   ANOP  ,                                                        00007100
         AIF   (&REGS).L000                                             00007200
         IEZREGS                                                        00007301
.L000    ANOP                                                           00007400
.*                                                                      00007500
.*  SET CONSTANTS AND LOGICAL SWITCHES                                  00007600
.*                                                                      00007700
&RCR     SETB  ('&RC'(1,1) EQ '(')           SET RETURN CODE TYPE FLAG  00007800
&RRT     SETA  N'&RR                         SET NUMBER OF RETRN REGS   00007900
&RICT    SETA  1                             SET RETURN REG COUNTER     00008000
&ALV     SETB  (('&LV' GE '72') AND ('&LV' LE '4096'))   SET LV FLAG    00008100
&CLV     SETB  ('&LV' LE '0')                SET LV FLAG                00008200
&DISP    SETC  '20242832364044485256606468081216' STANDARD S.A. DISPLS  00008300
.*                                                                      00008400
.*  SET RT FLAGS FOR REGISTERS INCLUDED IN RR PARAMETER                 00008500
.*                                                                      00008600
         AIF   (&RRT EQ 0).L002                                         00008700
.L001    ANOP                                                           00008800
&R1      SETA  &RR(&RICT)                                               00008900
         AIF   (NOT &RT(&R1+1)).L001A        CHECK REDUNDANCY           00009000
         MNOTE 8,'REG &R1 REDUNDANTLY SPECIFIED - GEN SUPPRESSED'       00009100
         MEXIT                                                          00009200
.L001A   ANOP                                                           00009300
&RT(&R1+1) SETB  1                           SET REG USE BIT            00009400
         AIF   (((&R1 GE 2) AND (&R1 LE 12)) OR (&R1 EQ 0)).L001B       00009500
         MNOTE 8,'RETURN REG &R1 NOT PERMITTED - GEN SUPPRESSED'        00009600
         MEXIT                                                          00009700
.L001B   ANOP                                                           00009800
&RICT    SETA  &RICT+1                                                  00009900
         AIF   (&RICT LE &RRT).L001                                     00010000
.*                                                                      00010100
.*  CHAIN BACK TO CALLERS SAVE AREA                                     00010200
.*                                                                      00010300
.L002    ANOP                                                           00010400
&LAB     SETC  '&LABEL'                                                 00010500
         AIF   ((NOT &RE) AND ('&RENT' EQ 'NO')).L002C                  00010600
&LAB     LR    R1,R13                   OUR RSA ADDR                    00010700
&LAB     SETC  ''                                                       00010800
.L002C   ANOP                                                           00010900
&LAB     L     R13,4(,R13)                Caller's RSA address          00011000
.*                                                                      00011100
.*  INITIALIZE RETURN CODE REGISTER (R15)                               00011200
.*                                                                      00011300
         AIF   (&RCR).L002B                                             00011400
&CCT     SETC  'A'                         SET RETURN CODE TYPE         00011500
         AIF   ((&RC GE 0) AND (&RC LE 4095)).L002A                     00011600
         MNOTE 4,'INVALID RETURN CODE - USING 4095'                     00011700
&CC      SETC  '4095'                      SET RETURN CODE LITERAL      00011800
         AGO   .L003                                                    00011900
.L002A   ANOP                                                           00012000
&CC      SETC  '&RC'                       SET RETURN CODE LITERAL      00012100
         AGO   .L003                                                    00012200
.L002B   ANOP                                                           00012300
&CCT     SETC  'R'                         SET RETURN CODE TYPE         00012400
&CC      SETC  'R'.'&RC'(2,K'&RC-2)        SET RETURN CODE REGISTER     00012500
&CC15    SETB  ('&CC' EQ 'R15')            SET CC15 FLAG                00012600
         AIF   ((('&CC' GE 'R2') AND ('&CC' LE 'R12')) OR (&CC15)).L003 00012700
         MNOTE 4,'INVALID RETURN CODE REGISTER - USING LITERAL 4095'    00012800
&CCT     SETC  'A'                         SET RETURN CODE TYPE         00012900
&CC      SETC  '4095'                      SET RETURN CODE LITERAL      00013000
.*                                                                      00013100
.*  ISSUE FREEMAIN TO FREE OWN SAVE AREA (IF RENT=YES OR &RE ON)        00013200
.*                                                                      00013300
.L003    ANOP                                                           00013400
         AIF   ((NOT &RE) AND ('&RENT' EQ 'NO')).L004                   00013500
         AIF   (NOT &RT(1)).L003A          IF R0 NOT AN RR PARM, CONT.  00013600
*    ** WARNING **                                                      00013700
*    RR=(0) WITH RENT=YES NECESSITATES MODIFYING THE CALLERS SAVE       00013800
*    AREA (R0 SLOT) BECAUSE THE FREEMAIN SVC ALTERS R0.                 00013900
         ST    R0,20(,R13)        ** SAVE R0 IN CALLER'S RSA **         00014000
.L003A   ANOP                                                           00014100
         AIF   (NOT &CC15).L003A1           IF NOT RC=(15), CONTINUE    00014200
*    ** WARNING **                                                      00014300
*    RC=(15) WITH RENT=YES NECESSITATES MODIFYING THE CALLERS SAVE      00014400
*    AREA (R15 SLOT) BECAUSE THE FREEMAIN SVC ALTERS R15.               00014500
         ST    R15,16(,R13)       ** SAVE R15 IN CALLER'S RSA **        00014600
&CCT     SETC  ''                    * SET UP                           00014700
&CC      SETC  '16(,R13)'            *   TO FORCE                       00014800
&CC15    SETB  0                     *      LOAD ON R15                 00014900
.L003A1  ANOP                                                           00015000
         AIF   ('&VIRT' NE '0').L003C      USE VIRT IF NOT ZERO         00015100
         AIF   (&ALV OR &CLV).L003B        ELSE USE LV IF VALID         00015200
         MNOTE 8,'LV SPECIFIED IS INVALID - GEN SUPPRESSED'             00015300
         MEXIT                                                          00015400
.L003B   ANOP                                                           00015500
&VIRT    SETC  '&LV'                                                    00015600
.L003C   ANOP                                                           00015700
         MNOTE '         FREEMAIN &FMTYPE,LV=&VIRT,A=(1),SP=&SP'        00015800
         FREEMAIN &FMTYPE,LV=&VIRT,A=(1),SP=&SP                         00015900
         AIF   (NOT &RT(1)).L004        IF R0 NOT AN RR PARM, CONT.     00016000
&RT(1)   SETB  0                        TURN OFF RETURN REG FLAG FOR R0 00016100
.*                                                                      00016200
.*  RESTORE 14, LOAD 15                                                 00016300
.*                                                                      00016400
.L004    ANOP                                                           00016500
         AIF   (&CC15).L004A            NO NEED TO LOAD R15 IF CC15 SET 00016600
         L&CCT R15,&CC                    Return code                   00016700
.L004A   ANOP                                                           00016800
         L     R14,12(,R13)               Return address                00016900
         AIF   (&RRT GT 0).L005                                         00017000
         LM    R0,R12,20(R13)             Restore caller's registers    00017100
         AGO   .L006                                                    00017200
.*                                                                      00017300
.*  RESTORE REGS NOT INCLUDED IN RR PARAMETER                           00017400
.*                                                                      00017500
.L005    ANOP                                                           00017600
&RICT    SETA  0                        SET LOOP CONTROL                00017700
.L005A   ANOP                                                           00017800
&R1      SETA  &RICT                                                    00017900
&RICT    SETA  &RICT+1                  INCREMENT LOOP CONTROL          00018000
         AIF   ((&RT(&R1+1)) AND (&R1 LE 12)).L005A                     00018100
         AIF   ((NOT &RT(&R1+2)) AND (&R1 LE 12)).L005B                 00018200
         AIF   (NOT (&R1 LE 12)).L006                                   00018300
&CC      SETC  '&DISP'((&R1*2+1),2)                                     00018400
         L     R&R1.,&CC.(,R13)                                         00018500
         AGO   .L005A                                                   00018600
.L005B   ANOP                                                           00018700
&R2      SETA  &RICT                                                    00018800
&RICT    SETA  &RICT+1                  INCREMENT LOOP CONTROL          00018900
         AIF   ((NOT &RT(&R2+1)) AND (&R2 LE 12)).L005B                 00019000
&R2      SETA  &R2-1                                                    00019100
&CC      SETC  '&DISP'((&R1*2+1),2)                                     00019200
         AIF   (&R1 EQ &R2).L005C                                       00019300
         LM    R&R1.,R&R2.,&CC.(R13)    RESTORE REGS                    00019400
         AGO   .L005D                                                   00019500
.L005C   ANOP                                                           00019600
         L     R&R1.,&CC.(,R13)                                         00019700
.L005D   ANOP                                                           00019800
         AIF   (&RICT LE 12).L005A                                      00019900
.*                                                                      00020000
.*  COMMON END FOR LEAVE MACRO - GENERATE RETURN INSTRUCTION            00020101
.*                                                                      00020200
.L006    ANOP                                                           00020300
         AIF   (&ENTER31).L007                                          00020400
         BR    R14                                                      00020501
.*       BSM   0,R14                      Return to caller              00020601
         MEXIT ,                                                        00020700
.L007    ANOP  ,                                                        00020800
         BSM   0,R14                      Return to caller              00020900
         MEND                                                           00021000
./ ADD NAME=ICATCHER 0109-07076-08014-1706-00252-00270-00000-T4      00
         MACRO                                                          00240100
         ICATCHER &CSECT,       CSECT name                             C00240200
               &MSG,            Eye catcher message                    C00240300
               &DATEFMT=EUR,    Date format defaults to EUR format     C00240400
               &TIMEFMT=STD,    Time format defaults to standard       C00240400
               &ASMINFO=YES,    Insert Assembly date and time?         C00240400
               &USING=15,       Register used during eyecatcher branch C00240500
               &AMODE=24,       Addressing mode                        C00240600
               &RMODE=24        Residency mode                          00240700
.*                                                                   *  00242100
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00242200
.*  May be used to generate CSECT Cards and standard format module   *  00241300
.*  eye-catchers.  Both the CSECT and module eye-catchers are        *  00241400
.*  optional.  See operand descriptions and examples below.          *  00241500
.*                                                                   *  00241600
.*  This macro is conditionally invoked by macro ENTER. If flag      *  00241700
.*  &PROLOG is not set to one, ENTER invokes ICATCHER Note that      *  00241800
.*  the ENTER macro always sets the global flag &PROLOG to zero so   *  00241900
.*  that subsequent ICATCHER or ENTER macros will expand properly.   *  00242000
.*                                                                   *  00242100
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00242200
.*                                                                   *  00242300
.*  CSECT                                                            *  00242400
.*       IF SPECIFIED, THIS POSITIONAL PARAMETER WILL GENERATE A     *  00242500
.*       CSECT CARD USING THE SPECIFIED NAME.  IF IT IS OMITTED BY   *  00242600
.*       CODING A COMMA, THEN NO CSECT CARD IS GENERATED.  IN EITHER *  00242700
.*       CASE, THE FOLLOWING CARDS WILL BE GENERATED:                *  00242800
.*                USING *,15                                         *  00242900
.*                CNOP  0,4                                          *  00243000
.*                B     Past-Eyecatcher                              *  00243100
.*                DC    AL1(Length-of-EyeCatcher)                    *  00243200
.*                                                                   *  00243300
.*  ASMINFO=YES|NO                                                   *  00243400
.*       Generate the eyecatcher with the date and time of the       *  00243500
.*       assembly?                                                   *  00243600
.*                                                                   *  00243700
.*  TIMEFMT=STD | CON                                                *  00243700
.*                                                                   *  00243700
.*       - STD: hh:mm                                                *  00243700
.*       - CON: "hh:mm AM" or "hh:mm PM"                             *  00243700
.*                                                                   *  00243700
.*  DATEFMT=ISO | EUR | OLD                                          *  00243700
.*                                                                   *  00243700
.*       - ISO: ccyy-mm-dd                                           *  00243700
.*       - EUR: dd-mon-ccyy                                          *  00243700
.*       - OLD: mm/dd/yy      (Required if not HLASM)                *  00243700
.*                                                                   *  00243700
.*  MSG                                                              *  00243800
.*       Text to be added to the standard MVS module eyecatcher      *  00243900
.*       which will consist of:                                      *  00244000
.*       - CSECT name                                                *  00244100
.*       - Date of assembly - Converted to Day-Month-Year            *  00244200
.*       - Time of assembly - Converted to standard time             *  00244300
.*       - The &MSG parameter (if any)                               *  00244400
.*                                                                   *  00244500
.*  AMODE                                                            *  00244600
.*       IF SPECIFIED, THIS  PARAMETER WILL GENERATE THE AMODE       *  00244700
.*       CARD FOR THE ASSEMBLER H.  AMODE IS A PROGRAM ATTRIBUTE     *  00244800
.*       WHICH CAN BE SET TO GIVE ADDRESSING MODE.                   *  00244900
.*                                                                   *  00245000
.*  RMODE                                                            *  00245100
.*       IF SPECIFIED, THIS PARAMETER WILL GENERATE THE RMODE        *  00245200
.*       CARD FOR THE ASSEMBLER H.  RMODE IS A PROGRAM ATTRIBUTE     *  00245300
.*       WHICH CAN BE SET TO GIVE RESIDENCY MODE.                    *  00245400
.*                                                                   *  00245500
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00245600
.*                                                                      00246900
.**  DEFINE GLOBAL AND LOCAL SET SYMBOLS                                00247000
.*                                                                      00247100
         GBLB  &PROLOG                                                  00247200
         LCLC  &CS,&ECDATE,&ECTIME,&YEAR                                00247300
         LCLA  &L1,&L2,&L3                                              00247400
&PROLOG  SETB  1                                                        00247500
.*                                                                      00247600
         AIF   (T'&CSECT NE 'O').P001                                   00247700
&CS      SETC  '&SYSECT'                                                00247800
         AGO   .P002                                                    00247900
.*                                                                      00248000
.** GENERATE CSECT, AMODE, AND RMODE CARDS                              00248100
.*                                                                      00248200
.P001    ANOP                                                           00248300
&CS      SETC  '&CSECT'                                                 00248400
&CS      CSECT ,                                                        00248500
.P002    ANOP  ,                                                        00248600
.*-> MNOTE ' *** CSECT=&CSECT CS=&CS PROLOG=&PROLOG'                    00248700
.*                                                                      00248800
.** AMODE and RMODE                                                     00248900
.*                                                                      00249000
         AIF   ('&AMODE' EQ '').AMODEX                                  00249100
&CS      AMODE &AMODE                                                   00249200
.AMODEX  AIF   ('&RMODE' EQ '').RMODEX                                  00249300
&CS      RMODE &RMODE                                                   00249400
.RMODEX  ANOP  ,                                                        00249500
 AIF  ('&ASMINFO' NE 'YES').TIMEX           .Want date and time info?   00249600
.*                                                                      00249700
.** Get Date                                                            00249800
.*                                                                      00249900
 AIF  ('&DATEFMT' EQ 'ISO').DATEISO         Want "ccyy-mm-dd"?
 AIF  ('&DATEFMT' EQ 'OLD').DATEOLD         Want "mm/dd/yy"?
&ECDATE SETC '&SYSDATE'(4,2)'-'             .Get Day of month           00250000
.*                                                                      00250100
&YEAR  SETC '&SYSDATC'(1,4)                   .Grab century from HLASM  00250500
.*                                                                      00250700
 AIF  ('&SYSDATE'(1,2) EQ '01').DATEM01                                 00250800
 AIF  ('&SYSDATE'(1,2) EQ '02').DATEM02                                 00250900
 AIF  ('&SYSDATE'(1,2) EQ '03').DATEM03                                 00251000
 AIF  ('&SYSDATE'(1,2) EQ '04').DATEM04                                 00251100
 AIF  ('&SYSDATE'(1,2) EQ '05').DATEM05                                 00251200
 AIF  ('&SYSDATE'(1,2) EQ '06').DATEM06                                 00251300
 AIF  ('&SYSDATE'(1,2) EQ '07').DATEM07                                 00251400
 AIF  ('&SYSDATE'(1,2) EQ '08').DATEM08                                 00251500
 AIF  ('&SYSDATE'(1,2) EQ '09').DATEM09                                 00251600
 AIF  ('&SYSDATE'(1,2) EQ '10').DATEM10                                 00251700
 AIF  ('&SYSDATE'(1,2) EQ '11').DATEM11                                 00251800
 AIF  ('&SYSDATE'(1,2) EQ '12').DATEM12                                 00251900
 MNOTE 'DA#EYEC-01E Invalid month'                                      00252000
 AGO  .DATEMX                                                           00252100
.*                                                                      00252200
.DATEM01 ANOP ,                                                         00252300
&ECDATE SETC '&ECDATE'.'JAN-&YEAR'                                      00252400
 AGO  .DATEMX                                                           00252500
.DATEM02 ANOP ,                                                         00252600
&ECDATE SETC '&ECDATE'.'FEB-&YEAR'                                      00252700
 AGO  .DATEMX                                                           00252800
.DATEM03 ANOP ,                                                         00252900
&ECDATE SETC '&ECDATE'.'MAR-&YEAR'                                      00253000
 AGO  .DATEMX                                                           00253100
.DATEM04 ANOP ,                                                         00253200
&ECDATE SETC '&ECDATE'.'APR-&YEAR'                                      00253300
 AGO  .DATEMX                                                           00253400
.DATEM05 ANOP ,                                                         00253500
&ECDATE SETC '&ECDATE'.'MAY-&YEAR'                                      00253600
 AGO  .DATEMX                                                           00253700
.DATEM06 ANOP ,                                                         00253800
&ECDATE SETC '&ECDATE'.'JUN-&YEAR'                                      00253900
 AGO  .DATEMX                                                           00254000
.DATEM07 ANOP ,                                                         00254100
&ECDATE SETC '&ECDATE'.'JUL-&YEAR'                                      00254200
 AGO  .DATEMX                                                           00254300
.DATEM08 ANOP ,                                                         00254400
&ECDATE SETC '&ECDATE'.'AUG-&YEAR'                                      00254500
 AGO  .DATEMX                                                           00254600
.DATEM09 ANOP ,                                                         00254700
&ECDATE SETC '&ECDATE'.'SEP-&YEAR'                                      00254800
 AGO  .DATEMX                                                           00254900
.DATEM10 ANOP ,                                                         00255000
&ECDATE SETC '&ECDATE'.'OCT-&YEAR'                                      00255100
 AGO  .DATEMX                                                           00255200
.*                                                                      00255300
.DATEM11 ANOP ,                                                         00255400
&ECDATE SETC '&ECDATE'.'NOV-&YEAR'                                      00255500
 AGO  .DATEMX                                                           00255600
.*                                                                      00255700
.DATEM12 ANOP ,                                                         00255800
&ECDATE SETC '&ECDATE'.'DEC-&YEAR'                                      00255900
.*AGO .DATEMX                                                           00256000
.DATEMX  ANOP ,                                                         00256100
 AGO .DATEX
.*
.DATEOLD ANOP  ,               If not HLASM or higher                   00256200
&ECDATE  SETC  '&SYSDATE'
 AGO .DATEX
.*
.DATEISO ANOP  ,                                                        00256200
&ECDATE  SETC  '&SYSDATC'(1,4).'-'.'&SYSDATC'(5,2).'-'.'&SYSDATC'(7,2)
.DATEX ANOP                                                             00256200
.*                                                                      00256300
.** Time                                                                00256400
.*                                                                      00256500
 AIF  ('&TIMEFMT' EQ 'STD').TIMESTD         Want "hh:mm"
 AIF  ('&SYSTIME'(1,2) LT '12').TIMEN                                   00256600
 AIF  ('&SYSTIME'(1,2) EQ '12').TIMEH12                                 00256700
 AIF  ('&SYSTIME'(1,2) EQ '13').TIMEH13                                 00256800
 AIF  ('&SYSTIME'(1,2) EQ '14').TIMEH14                                 00256900
 AIF  ('&SYSTIME'(1,2) EQ '15').TIMEH15                                 00257000
 AIF  ('&SYSTIME'(1,2) EQ '16').TIMEH16                                 00257100
 AIF  ('&SYSTIME'(1,2) EQ '17').TIMEH17                                 00257200
 AIF  ('&SYSTIME'(1,2) EQ '18').TIMEH18                                 00257300
 AIF  ('&SYSTIME'(1,2) EQ '19').TIMEH19                                 00257400
 AIF  ('&SYSTIME'(1,2) EQ '20').TIMEH20                                 00257500
 AIF  ('&SYSTIME'(1,2) EQ '21').TIMEH21                                 00257600
 AIF  ('&SYSTIME'(1,2) EQ '22').TIMEH22                                 00257700
 AIF  ('&SYSTIME'(1,2) EQ '23').TIMEH23                                 00257800
 AIF  ('&SYSTIME'(1,2) EQ '24').TIMEH24                                 00257900
 AGO  .TIMEX                                                            00258000
.TIMEH12 ANOP ,                                                         00258100
&ECTIME SETC '12.'.'&SYSTIME'(4,2)' PM'                                 00258200
 AGO  .TIMEX                                                            00258300
.TIMEH13 ANOP ,                                                         00258400
&ECTIME SETC '01.'.'&SYSTIME'(4,2)' PM'                                 00258500
 AGO  .TIMEX                                                            00258600
.TIMEH14 ANOP ,                                                         00258700
&ECTIME SETC '02.'.'&SYSTIME'(4,2)' PM'                                 00258800
 AGO  .TIMEX                                                            00258900
.TIMEH15 ANOP ,                                                         00259000
&ECTIME SETC '03.'.'&SYSTIME'(4,2)' PM'                                 00259100
 AGO  .TIMEX                                                            00259200
.TIMEH16 ANOP ,                                                         00259300
&ECTIME SETC '04.'.'&SYSTIME'(4,2)' PM'                                 00259400
 AGO  .TIMEX                                                            00259500
.TIMEH17 ANOP ,                                                         00259600
&ECTIME SETC '05.'.'&SYSTIME'(4,2)' PM'                                 00259700
 AGO  .TIMEX                                                            00259800
.TIMEH18 ANOP ,                                                         00259900
&ECTIME SETC '06.'.'&SYSTIME'(4,2)' PM'                                 00260000
 AGO  .TIMEX                                                            00260100
.TIMEH19 ANOP ,                                                         00260200
&ECTIME SETC '07.'.'&SYSTIME'(4,2)' PM'                                 00260300
 AGO  .TIMEX                                                            00260400
.TIMEH20 ANOP ,                                                         00260500
&ECTIME SETC '08.'.'&SYSTIME'(4,2)' PM'                                 00260600
 AGO  .TIMEX                                                            00260700
.TIMEH21 ANOP ,                                                         00260800
&ECTIME SETC '09.'.'&SYSTIME'(4,2)' PM'                                 00260900
 AGO  .TIMEX                                                            00261000
.TIMEH22 ANOP ,                                                         00261100
&ECTIME SETC '10.'.'&SYSTIME'(4,2)' PM'                                 00261200
 AGO  .TIMEX                                                            00261300
.TIMEH23 ANOP ,                                                         00261400
&ECTIME SETC '11.'.'&SYSTIME'(4,2)' PM'                                 00261500
 AGO  .TIMEX                                                            00261600
.TIMEH24 ANOP ,                                                         00261700
&ECTIME SETC '12.'.'&SYSTIME'(4,2)' AM'                                 00261800
 AGO  .TIMEX                                                            00261900
.TIMEN  ANOP ,                                                          00262000
&ECTIME SETC '&SYSTIME'(1,5)' AM'         .Get Day of month             00262100
 AGO .TIMEX
.TIMESTD ANOP ,
&ECTIME SETC '&SYSTIME'                   .Standard time from ASMblr
.TIMEX ANOP                                                             00262200
.*                                                                      00262300
.** Generate Eye Catcher                                                00262400
.*                                                                      00262500
                USING *,&USING            Get addressibility            00262600
                CNOP  0,4                 Full word alignment           00262700
                B     &CS.ECX             Branch around Eyecatcher      00262800
&CS.LEN         DC    AL1(&CS.ECL-1)                                    00262900
                DC    CL8'&CS'                                          00263000
                DC    C' '
 AIF ('&ASMINFO' NE 'YES').ECDTX          .Want date and time info?     00263100
                DC    C'&ECDATE'                                        00263200
                DC    C' '
                DC    C'&ECTIME'                                        00263300
                DC    C' '
.ECDTX          ANOP  ,                                                 00263400
 AIF ('&MSG' EQ '').ECMSGX                                              00263500
&CS.ECMSG       DC    C&MSG                                             00263600
.ECMSGX         ANOP  ,                                                 00263700
&CS.ECL         EQU   *-&CS.LEN                                         00263800
&CS.ECX         DS    0H                                                00263900
                DROP  &USING                                            00264000
 MEND  ,                                                                00264100
./ ADD NAME=IEECODES 8001-74016-74016-2111-00040-00787-00000-HERC01  00
         MACRO
         IEECODES &ROUTCDE=13,&DESC=,&ID=IEE
         LCLC  &CD(4)
         LCLA  &I,&N
         LCLB  &B(32)
.*
.* DESCRIPTOR CODES
.*
&I       SETA  1
.DCHK    AIF   (T'&DESC EQ 'O').RCHK
&N       SETA  &DESC(&I)
&I       SETA  &I+1
         AIF   (&N GE 1 AND &N LE 16).ASSIGND
         MNOTE 8,'&DESC(&I) IS INVALID DESCRIPTOR - IGNORED'
         AGO   .NXTD
.ASSIGND ANOP
&B(&N)   SETB  1
.NXTD    AIF  (&I LE N'&DESC).DCHK
&I       SETA  1
.*
.* ROUTE CODES
.*
.RCHK    AIF   (T'&ROUTCDE EQ 'O').ASSIGNC
&N       SETA  &ROUTCDE(&I)
&I       SETA  &I+1
         AIF   (&N GE 1 AND &N LE 16).ASSIGNR
         MNOTE 8,'ROUTCDE(&I) IS INVALID ROUTE - IGNORED'
         AGO   .NXTR
.ASSIGNR ANOP
&B(&N+16) SETB  1
.NXTR    AIF   (&I LE N'&ROUTCDE).RCHK
.ASSIGNC ANOP
&I       SETA  1
&CD(&I)   SETC  '&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)'
&CD(&I+1) SETC  '&B(9)&B(10)&B(11)&B(12)&B(13)&B(14)&B(15)&B(16)'
&CD(&I+2) SETC  '&B(17)&B(18)&B(19)&B(20)&B(21)&B(22)&B(23)&B(24)'
&CD(&I+3) SETC  '&B(25)&B(26)&B(27)&B(28)&B(29)&B(30)&B(31)&B(32)'
&ID.DESCD DC   BL2'&CD(1)&CD(2)'
&ID.ROUTC DC   BL2'&CD(3)&CD(4)'
         MEND
./ ADD NAME=IEZBITS  0100-08013-08013-1541-00018-00018-00000-T4      00
         MACRO                                                          00000100
         IEZBITS                                                        00000200
         GBLA  &IEZBITS                                                 00000300
         AIF   (&IEZBITS EQ 0).SETSW                                    00000400
         MEXIT                                                          00000500
.SETSW   ANOP                                                           00000600
&IEZBITS SETA  1                                                        00000700
*                                                                       00000800
BIT0     EQU   128                                                      00000900
BIT1     EQU   64                                                       00001000
BIT2     EQU   32                                                       00001100
BIT3     EQU   16                                                       00001200
BIT4     EQU   8                                                        00001300
BIT5     EQU   4                                                        00001400
BIT6     EQU   2                                                        00001500
BIT7     EQU   1                                                        00001600
*                                                                       00001700
         MEND                                                           00001800
./ ADD NAME=IEZREGS  0100-07292-07292-1804-00027-00027-00000-T3      00
         MACRO                                                          00000100
         IEZREGS &DUMMY                                                 00000200
         GBLA  &DEFREGS                                                 00000300
         AIF   (&DEFREGS EQ 1).NOGEN   FLAG ON ? BYPASS EQUATES         00000401
         SPACE 1                                                        00000500
R0       EQU   0                                                        00000600
R1       EQU   1                                                        00000700
R2       EQU   2                                                        00000800
R3       EQU   3                                                        00000900
R4       EQU   4                                                        00001000
R5       EQU   5                                                        00001100
R6       EQU   6                                                        00001200
R7       EQU   7                                                        00001300
R8       EQU   8                                                        00001400
R9       EQU   9                                                        00001500
R10      EQU   10                                                       00001600
R11      EQU   11                                                       00001700
R12      EQU   12                                                       00001800
R13      EQU   13                                                       00001900
R14      EQU   14                                                       00002000
R15      EQU   15                                                       00002100
         SPACE 1                                                        00002201
.*             EQUATES GENERATED, TURN ON DEFREGS FLAG TO PREVENT       00002301
.*             DUPLICATE EQUATES BEING GENERATED                        00002401
&DEFREGS SETA  1                                                        00002501
.NOGEN   ANOP                                                           00002602
         MEND                                                           00002700
./ ADD NAME=IF       8000-74024-74024-1513-00083-00083-00000-HERC01  00
         MACRO
&NAME    IF    &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20,&C=
.*#-------------------------------------------------------------------*
.*#      IF    MACRO FOR STRUCTURED PROGRAMMING                       *
.*#----------------------------------------------------23-09-80-RS----*
.*#
.*#   FUNCTION:    STARTS A NEW IF LEVEL
.*#
.*#   MODEL:       IF  COND1
.*#                    COND1,OP1,COND2
.*#                    COND1,OP1,COND2,OP2,COND3,...,CONDN
.*#
.*#                -   CONDI : A VALID ASSEMBLE INSTRUCTION WITH
.*#                            MENOTECNIC CONDITION CODE (IN BRACKETS)
.*#                            EXAMPLE: (TM,SWITCH,X'04',O)
.*#                            FOR COMPARE OPERATIONS THE CONDITION-
.*#                            CODE WILL BE PUT BETWEEN THE OPERANDS
.*#                            EXAMPLE: (CLC,FIELD1,EQ,FIELD2)
.*#                -   OPI:    IS ONE OF THE LOGICAL OPERANDS 'AND' OR
.*#                            'OR'
.*#                            DO NOT MIX 'AND' AND 'OR' OPERANDS IN
.*#                            THE SAME IF STATEMENT.
.*#
.*#--------------------------------------------------------------------
         LCLA  &I              INDEX FOR STRING SCANNING
         LCLA  &N              TOTAL NESTING LEVEL
         LCLC  &OPND
         COPY  IFGLO
.*--------------------------------------------------------------------*
.*       FIRST IF: INIT GLOBALS                                       *
.*--------------------------------------------------------------------*
         AIF   (&IFINIT).START            INIT ALREADY DONE
&IFINIT  SETB  1
&IFLEVEL SETA  0
&DOLEVEL SETA  0
&IFLABEL SETA  0
&IFLIMIT SETA  100000
&IFPRAEF SETC  '##'
&IFDEBUG SETB  0
.*--------------------------------------------------------------------*
.*       INCREMENT LEVEL. GENERATE LABELS FOR FALSE/TRUE              *
.*--------------------------------------------------------------------*
.START   ANOP
&MACNA   SETC  'IF'
&IFLEVEL SETA  &IFLEVEL+1
         AIF   (&IFLEVEL EQ 50).FEHL06
         AIF ('&SYSPARM' EQ '').OBR00
         AIF ('&SYSPARM'(1,2) EQ 'NO').OBR00
         AGO     .OBR00
&NAME    SVDOC   COM=START,C=&C,                                       *
               &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,&P11,&P12,&P13,*
               &P14,&P15,&P16,&P17,&P18,&P19,&P20
.*
.OBR00   ANOP
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFFALSE(&IFLEVEL) SETC '&IFLABEL'    LABEL FOR BRANCH ON NOT TRUE
&IFTRUE  SETC  ''
         AIF   ('&SYSLIST(2)' EQ '').STA03 KEIN TRUE LABEL ERFORDERLICH
&IFLABEL SETA  &IFLABEL+1
         AIF   (&IFLABEL GE &IFLIMIT).FEHL06
&IFTRUE  SETC  '&IFLABEL'                  TRUE LABEL FOR AND /OR
.*--------------------------------------------------------------------*
.*       CALL IF-PROCESSOR TO ANALYZE CONDITION                       *
.*--------------------------------------------------------------------*
.STA03   ANOP
         IFPRO &IFTRUE,&IFFALSE(&IFLEVEL),&P1,&P2,&P3,&P4,&P5,&P6,&P7, *
               &P8,&P9,&P10,&P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,   *
               &P19,&P20
.*--------------------------------------------------------------------*
.*       SET TRUE LABEL IF NECESSARY                                  *
.*--------------------------------------------------------------------*
         AIF   ('&IFTRUE' EQ '').MACEND
&OPND    SETC  '&IFPRAEF&IFTRUE'
&OPND    DS    0H                        TARGET FOR BANCH ON NOT TRUE
         AGO   .MACEND
.*
         COPY IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=IFERR
.*#-------------------------------------------------------------------*
.*# IFERR: ERROR MESSAGES FOR STRUCTURED PROGRAMMING-MACROS           *
.*#-------------------------------------------------------------------*
.FEHL01  MNOTE 8,'IFINIT MACRO NOT FIRST MACRO'
         AGO   .EREND
.FEHL02  MNOTE 8,'PRAEFIX BIGGER THAN 7 POSITIONS'
         AGO   .EREND
.FEHL03  MNOTE 8,'DEBUG NOT YES OR NO'
         AGO   .EREND
.FEHL04  MNOTE 8,'TRACE SWITCH NOT YES OR NO'
         AGO   .EREND
.FEHL05  MNOTE 8,'COMPARE CODE NOT IN BRACKETS'
         AGO   .EREND
.FEHL06  MNOTE 8,'TOO MUCH STRUCTURED MACROS IN THIS PROGRAM'
         AGO   .EREND
.FEHL07  MNOTE 8,'SHOULD NOT OCCUR'
         AGO   .EREND
.FEHL08  MNOTE 8,'TOO MUCH OPERANDS IN CONDITION'
         AGO   .EREND
.FEHL09  MNOTE 8,'NOT ENOUGH OPERANDS IN CONDITION'
         AGO   .EREND
.FEHL10  MNOTE 8,'AND/OR CONTINATION MISSING'
         AGO   .EREND
.FEHL11  MNOTE 8,'INVALID COMPARE OPERANDS'
         AGO   .EREND
.FEHL12  MNOTE 8,'FROM AND WHILE ARE EXCLUSIVE'
         AGO   .EREND
.FEHL13  MNOTE 8,'WHILE PARAMETER NOT IN BRACKETS'
         AGO   .EREND
.FEHL14  MNOTE 8,'EXCESSIVE PARAMETERS IN DO STATEMENT'
         AGO   .EREND
.FEHL15  MNOTE 8,'INVALID PARAMETER'
         AGO   .EREND
.FEHL16  MNOTE 8,'EXIT NEEDS CONDITION'
         AGO   .EREND
.FEHL17  MNOTE 8,'PARAMETER MISSING'
         AGO   .EREND
.FEHL18  MNOTE 8,'INVALID OPERAND IN DO STATEMENT'
         AGO   .EREND
.FEHL19  MNOTE 8,'DONAME NOT DEFINED'
         AGO   .EREND
.EREND   ANOP
         MEXIT
.*--------------------------------------------------------------------*
./ ADD NAME=IFGLO
.*#-------------------------------------------------------------------*
.*#     GLOBALS FOR MACROS FOR STRUCTURED PROGRAMMING                 *
.*#-------------------------------------------------------------------*
.*
         GBLC  &IFFALSE(50)    -IFGLO-     TABELLE DER FALSE IDENTIF.
         GBLC  &IFENDLB(50)    -IFGLO-     TABELLE DER ENDIF ID.
         GBLC  &DOSTART(50)    -IFGLO-     TABELLE DER START ID.
         GBLC  &DOFALSE(50)    -IFGLO-     TABELLE DER FALSE ID.
         GBLC  &DOENDLB(50)    -IFGLO-     TABELLE DER ENDDO ID.
         GBLC  &DOFROM(50)     -IFGLO-     TABELLE DER DO LOOP REG
         GBLC  &DONAME(50)     -IFGLO-     TABELLE DER DO LOOP NAMEN
.*                             -IFGLO-
         GBLC  &IFPRAEF        -IFGLO-     PRAFIX ZUR LABEL ERZEUGUNG
         GBLC  &IFTRUE         -IFGLO-     TRUE LABEL FOR AND /OR
         GBLC  &DOTRUE         -IFGLO-     TRUE LABEL FOR AND/OR
.*                             -IFGLO-
         GBLA  &IFLEVEL        -IFGLO-     NESTING LEVEL
         GBLA  &DOLEVEL        -IFGLO-     NESTING LEVEL
         GBLA  &IFLABEL        -IFGLO-     ELSE ID (COUNTS UP)
         GBLA  &IFLIMIT        -IFGLO-     ENDIF ID (COUNTS DOWN)
.*                             -IFGLO-
         GBLB  &IFINIT         -IFGLO-     INIT SWITCH
         GBLB  &IFDEBUG        -IFGLO-     DEBUG MODE
.*       SBU                   -IFGLO-
.*                             -IFGLO-
         GBLC  &MACNA          -IFGLO-     MACRO NAME
.*--------------------------------------------------------------------*
./ ADD NAME=IFPRO
         MACRO
         IFPRO &TRUE,&FALSE,&P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,  *
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20
.*#-------------------------------------------------------------------*
.*#  IFPRO  PROCESSES CONDITION STATEMENTS IN STUCTURED PROGRAMMING   *
.*#-------------------------------------------------------------------*
         COPY  IFGLO
         LCLC  &INST                       INSTRUCTION
         LCLC  &COND                       CONDITION CODE
         LCLC  &OPND                       WORK-FIELD
         LCLA  &I
         LCLC  &OP1
         LCLC  &OP2
         LCLC  &OP3
         LCLC  &OP4
.*--------------------------------------------------------------------*
.*       FORMAL TEST: NUMBER OF OPERANDS  AND 'AND' OR  'OR' PROCESS. *
.*--------------------------------------------------------23-09-80-RS-*
&I       SETA  2                           SKIP TRUE AND FALSE PARAM
.FOR01   ANOP
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ '').FEHL10
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ 'AND').FOR01
         AIF   ('&SYSLIST(&I)' EQ 'OR').FOR01
         AIF   ('&SYSLIST(&I)' NE '').FEHL10
.*--------------------------------------------------------------------*
.*       SCANNING OF THE OPERANDS                                     *
.*--------------------------------------------------------23-09-80-RS-*
&I       SETA  2
.BIGLOP  ANOP
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ '').MACEND
.*--------------------------------------------------------------------*
.*       WORK ON ASSEMBLE INSTRUCTIONS AND GENERATE CODE              *
.*--------------------------------------------------------23-09-80-RS-*
         AIF   ('&SYSLIST(&I)'(1,1) NE '(').FEHL05 NOT IN BRACKETS
         AIF   (N'&SYSLIST(&I) GT 5).FEHL08        TOO MUCH OPERANDS
         AIF   (N'&SYSLIST(&I) LT 4).FEHL09        TOO LESS OPERANDS
.*--------------------------------------------------------------------*
.*       ELIMINATE OP-CODE AND BRANCH CONDITION                       *
.*--------------------------------------------------------------------*
&INST    SETC  '&SYSLIST(&I,1)'                    GET OP CODE
&OP1     SETC  '&SYSLIST(&I,2)'
         AIF   (N'&SYSLIST(&I) EQ 4).OP4           OP,OP1,OP2,OP3
.*
&OP2     SETC  '&SYSLIST(&I,3)'
         AIF   ('&INST'(1,1) EQ 'C').OP5C          COMPARE INSTRUCTION
.*
&OP3     SETC  '&SYSLIST(&I,4)'
&COND    SETC  '&SYSLIST(&I,5)'
         AGO   .OP5END
.*
.OP5C    ANOP
&OP3     SETC  '&SYSLIST(&I,5)'
&COND    SETC  '&SYSLIST(&I,4)'
.*
.OP5END  ANOP
&OPND    SETC  '&OP1'.','.'&OP2'.','.'&OP3'
         AGO   .OPEND
.OP4     ANOP
         AIF   ('&INST'(1,1) EQ 'C').OP4C     COMPARE INSTRUKTION
&OP2     SETC  '&SYSLIST(&I,3)'
&COND    SETC  '&SYSLIST(&I,4)'
         AGO   .OP4END
.*
.OP4C    ANOP
&OP2     SETC  '&SYSLIST(&I,4)'
&COND    SETC  '&SYSLIST(&I,3)'
.*
.OP4END  ANOP
&OPND    SETC  '&OP1,&OP2'
.*
.OPEND   ANOP
.*--------------------------------------------------------------------*
.*       GENERATE ASSEMBLER INSTRUCTION                               *
.*--------------------------------------------------------23-09-80-RS-*
         &INST &OPND
.*--------------------------------------------------------------------*
.*       COMPUTE INVERTED CONDITION CODE                              *
.*--------------------------------------------------------23-09-80-RS-*
.ER00    ANOP
         AIF   ('&COND' NE 'H').ER01
&COND    SETC  '2'
         AGO   .ER99
.*
.ER01    ANOP
         AIF   ('&COND' NE 'EQ').ER02
&COND    SETC  '8'
         AGO   .ER99
.*
.ER02    ANOP
         AIF   ('&COND' NE 'L').ER03
&COND    SETC  '4'
         AGO   .ER99
.*
.ER03    ANOP
         AIF   ('&COND' NE 'LE').ER04
&COND    SETC  '13'
         AGO   .ER99
.*
.ER04    ANOP
         AIF   ('&COND' NE 'NH').ER05
&COND    SETC  '13'
         AGO   .ER99
.*
.ER05    ANOP
         AIF   ('&COND' NE 'NL').ER06
&COND    SETC  '11'
         AGO   .ER99
.*
.ER06    ANOP
         AIF   ('&COND' NE 'NE').ER07
&COND    SETC  '7'
         AGO   .ER99
.*
.ER07    ANOP
         AIF   ('&COND' NE 'O').ER08
&COND    SETC  '1'
         AGO   .ER99
.*
.ER08    ANOP
         AIF   ('&COND' NE 'P').ER09
&COND    SETC  '2'
         AGO   .ER99
.*
.ER09    ANOP
         AIF   ('&COND' NE 'M').ER10
&COND    SETC  '4'
         AGO   .ER99
.*
.ER10    ANOP
         AIF   ('&COND' NE 'NP').ER11
&COND    SETC  '13'
         AGO   .ER99
.*
.ER11    ANOP
         AIF   ('&COND' NE 'NM').ER12
&COND    SETC  '11'
         AGO   .ER99
.*
.ER12    ANOP
         AIF   ('&COND' NE 'Z').ER13
&COND    SETC  '8'
         AGO   .ER99
.*
.ER13    ANOP
         AIF   ('&COND' NE 'NZ').ER14
&COND    SETC  '7'
         AGO   .ER99
.*
.ER14    ANOP
         AIF   ('&COND' NE 'NO').ER15
&COND    SETC  '14'
         AGO   .ER99
.*
.ER15    ANOP
         AIF   ('&COND' NE 'E').ER16
&COND    SETC  '8'
         AGO   .ER99
.*
.ER16    ANOP
         AIF   ('&COND' NE 'GE').ER17
&COND    SETC  '11'
         AGO   .ER99
.*
.ER17    ANOP
         AIF   ('&COND' NE 'GT').ER18
&COND    SETC  '2'
         AGO   .ER99
.*
.ER18    ANOP
         AIF   ('&COND' NE 'LT').ER19
&COND    SETC  '4'
         AGO   .ER99
.*
.ER19    ANOP
         AGO   .FEHL11
.*
.ER99    ANOP
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH IF NOT TRUE LABEL (FOR 'AND' OR LAST PARAM   *
.*--------------------------------------------------------23-09-80-RS-*
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ 'OR').BIGOR
&OPND    SETC  '15-&COND,&IFPRAEF&FALSE'
         BC    &OPND                       BRANCH IF NOT TRUE
         AGO   .BIGLOP
.*--------------------------------------------------------------------*
.*       GENERATE BRANCH IF NOT TRUE LABEL FOR 'OR' PROCESSING        *
.*--------------------------------------------------------23-09-80-RS-*
.BIGOR   ANOP
&OPND    SETC  '&COND,&IFPRAEF&TRUE'
         BC    &OPND                       BRANCH IF TRUE
         AGO   .BIGLOP
.*
         COPY  IFERR
.*
.MACEND  ANOP
         MEXIT
         MEND
./ ADD NAME=IHAIQE   8001-74018-74018-1308-00024-00929-00000-HERC02  00
         MACRO
         IHAIQE &DSECT=YES
         AIF   ('&DSECT' NE 'YES').NODSCT
IQESECT  DSECT
.NODSCT  ANOP
IQELNK   DS    A     . WORD REFERENCE FOR IQELNKA
         ORG   IQELNK
IQESTAT1 DS    XL1   . RESERVED
IQELNKA  DS    AL3   . ADDRESS OF NEXT IQE
IQEPARM  DS    A     . PARMS TO BE PASSED TO ASYN RTN
IQEIRB   DS    A     . WORD REFERENCE FOR IQEIRBA
         ORG   IQEIRB
IQEFLAGS DS    XL1   . FLAG FIELD
IQEPURGE EQU   X'80' . THIS IQE MUST NOT BE SCHEDULED
IQEIRBA  DS    AL3   . ADDR IRB TO BE SCHEDULED
IQETCB   DS    A     . WORD REFERENCE FOR IQETCBA
         ORG   IQETCB
IQESTAT2 DS    C     . RESERVED
IQETCBA  DS    AL3   . ADDR OF TCB ASSOCIATED WITH THIS IQE
IQEDCB   DS    A     . ADDR OF DCB
IQEOUTLM DS    A     . ADDR OF OUTPUT LIMIT
IQEEND   DS    C     . END OF IQE
         MEXIT
         MEND
./ ADD NAME=IHAPIE   0100-07333-07333-1228-00069-00069-00000-T3      00
*        %GOTO PIEBSL;                                               /* 00050000
         MACRO                                                          00100000
         IHAPIE &DSECT=YES                                              00150000
         AIF   ('&DSECT' EQ 'NO').NODSCT                                00200000
PIE      DSECT                                                          00250000
         AGO   .CONT                                                    00300000
.NODSCT  ANOP                                                           00350000
         DS    0D -                                                     00400000
PIE      EQU   * -            PIEPTR                                    00450000
.CONT    ANOP                                                           00500000
         IEZBITS , -          SYMBOLIC BIT DEFINITIONS                  00550000
         SPACE 1                                                        00600000
*********************************************************************** 00650000
*                                                                       00700000
*              PROGRAM INTERRUPT ELEMENT (PIE)                          00750000
*                                                                       00800000
*        A PIE IS CREATED AFTER A PROGRAM CHECK HAS OCCURRED IF THERE   00850000
*        IS A USER-SPECIFIED EXIT ROUTINE TO HANDLE PROGRAM CHECK       00900000
*        INTERRUPTIONS.  A PIE IS USED TO PASS THE NECESSARY DATA TO    00950000
*        THE USER-SPECIFIED EXIT ROUTINE.                               01000000
*                                                                       01050000
*********************************************************************** 01100000
         SPACE 1                                                        01150000
PIEPICA  DS    0F -           ADDRESS OF THE CURRENT PICA               01200000
PIEFLGS  DS    B -            FLAG BYTE                                 01250000
PIENOPI  EQU   BIT0 -         IF ONE, INDICATES THAT THE TASK CANNOT    01300000
*                             ACCEPT FURTHER PI'S                       01350000
PIEPICAA DS    AL3 -          ADDRESS OF THE CURRENT PICA               01400000
PIEPSW   DS    CL8 -          PI OLD PSW STORED AT PROGRAM INTERRUPT    01450000
*                             TIME                                      01500000
PIEGR14  DS    F -            SAVE AREA FOR REGISTER 14                 01550000
PIEGR15  DS    F -            SAVE AREA FOR REGISTER 15                 01600000
PIEGR0   DS    F -            SAVE AREA FOR REGISTER 0                  01650000
PIEGR1   DS    F -            SAVE AREA FOR REGISTER 1                  01700000
PIEGR2   DS    F -            SAVE AREA FOR REGISTER 2                  01750000
         MEND  , */                                                     01800000
*%PIEBSL  :  ;                                                          01850000
*                                                                       01900000
*/* **************************************************************** */ 01950000
*                                                                       02000000
*/*            PROGRAM INTERRUPT ELEMENT (PIE)                       */ 02050000
*                                                                       02100000
*/*      A PIE IS CREATED AFTER A PROGRAM CHECK HAS OCCURRED IF THERE*/ 02150000
*/*      IS A USER-SPECIFIED EXIT ROUTINE TO HANDLE PROGRAM CHECK    */ 02200000
*/*      INTERRUPTIONS.  A PIE IS USED TO PASS THE NECESSARY DATA TO */ 02250000
*/*      THE USER-SPECIFIED EXIT ROUTINE.                            */ 02300000
*                                                                       02350000
*/* **************************************************************** */ 02400000
*                                                                       02450000
*DECLARE                                                                02500000
*  1 PIE      BASED(PIEPTR),                                            02550000
*   2 PIEPICA     FIXED(31),             /* ADDRESS OF THE CURRENT      02600000
*                                           PICA                     */ 02650000
*    3 PIEFLGS     CHAR(1),              /* FLAG BYTE                */ 02700000
*     4 PIENOPI  BIT(1),                 /* IF ONE, INDICATES THAT      02750000
*                                           THE TASK CANNOT  ACCEPT     02800000
*                                           FURTHER PI'S             */ 02850000
*     4 *        BIT(7),                 /* RESERVED                 */ 02900000
*    3 PIEPICAA    PTR(24)  BDY(BYTE),   /* ADDRESS OF THE CURRENT      02950000
*                                           PICA                     */ 03000000
*   2 PIEPSW      CHAR(8),               /* PI OLD PSW STORED AT        03050000
*                                           PROGRAM INTERRUPT  TIME  */ 03100000
*   2 PIEGR14     FIXED(31),             /* SAVE AREA FOR REGISTER      03150000
*                                           14                       */ 03200000
*   2 PIEGR15     FIXED(31),             /* SAVE AREA FOR REGISTER      03250000
*                                           15                       */ 03300000
*   2 PIEGR0      FIXED(31),             /* SAVE AREA FOR REGISTER 0 */ 03350000
*   2 PIEGR1      FIXED(31),             /* SAVE AREA FOR REGISTER 1 */ 03400000
*   2 PIEGR2      FIXED(31);             /* SAVE AREA FOR REGISTER 2 */ 03450000
./ ADD NAME=MODULEID 0104-07339-08013-1540-00212-00256-00000-T4      00
         MACRO                                                          00240100
         MODULEID &CSECT,       CSECT name                             C00240200
               &MSG,            Eye catcher message                    C00240300
               &DATEFMT=OLD,    Date format defaults to EUR format     C00240400
               &TIMEFMT=STD,    Time format defaults to standard       C00240400
               &ASMINFO=YES,    Insert Assembly date and time?         C00240400
               &USING=15        Register used during eyecatcher branch  00240500
.*                                                                   *  00242100
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00242200
.*  May be used to generate CSECT Cards and standard format module   *  00241300
.*  eye-catchers.  Both the CSECT and module eye-catchers are        *  00241400
.*  optional.  See operand descriptions and examples below.          *  00241500
.*                                                                   *  00241600
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00242200
.*                                                                   *  00242300
.*  CSECT                                                            *  00242400
.*       IF SPECIFIED, THIS POSITIONAL PARAMETER WILL GENERATE A     *  00242500
.*       CSECT CARD USING THE SPECIFIED NAME.  IF IT IS OMITTED BY   *  00242600
.*       CODING A COMMA, THEN NO CSECT CARD IS GENERATED.  IN EITHER *  00242700
.*       CASE, THE FOLLOWING CARDS WILL BE GENERATED:                *  00242800
.*                USING *,15                                         *  00242900
.*                CNOP  0,4                                          *  00243000
.*                B     Past-Eyecatcher                              *  00243100
.*                DC    AL1(Length-of-EyeCatcher)                    *  00243200
.*                                                                   *  00243300
.*  ASMINFO=YES|NO                                                   *  00243400
.*       Generate the eyecatcher with the date and time of the       *  00243500
.*       assembly?                                                   *  00243600
.*                                                                   *  00243700
.*  TIMEFMT=STD | CON                                                *  00243700
.*                                                                   *  00243700
.*       - STD: hh:mm                                                *  00243700
.*       - CON: "hh:mm AM" or "hh:mm PM"                             *  00243700
.*                                                                   *  00243700
.*  DATEFMT=ISO | EUR | OLD                                          *  00243700
.*                                                                   *  00243700
.*       - ISO: ccyy-mm-dd                                           *  00243700
.*       - EUR: dd-mon-ccyy                                          *  00243700
.*       - OLD: mm/dd/yy      (Required if not HLASM)                *  00243700
.*                                                                   *  00243700
.*  MSG                                                              *  00243800
.*       Text to be added to the standard MVS module eyecatcher      *  00243900
.*       which will consist of:                                      *  00244000
.*       - CSECT name                                                *  00244100
.*       - Date of assembly - Converted to Day-Month-Year            *  00244200
.*       - Time of assembly - Converted to standard time             *  00244300
.*       - The &MSG parameter (if any)                               *  00244400
.*                                                                   *  00244500
.*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*  00245600
.*                                                                      00246900
.**  DEFINE GLOBAL AND LOCAL SET SYMBOLS                                00247000
.*                                                                      00247100
         LCLC  &CS,&ECDATE,&ECTIME,&YEAR                                00247300
         LCLA  &L1,&L2,&L3                                              00247400
.*                                                                      00247600
&CS      SETC  '&SYSECT'                                                00247800
 AIF  ('&ASMINFO' NE 'YES').TIMEX           .Want date and time info?   00249600
.*                                                                      00249700
.** Get Date                                                            00249800
.*                                                                      00249900
 AIF  ('&DATEFMT' EQ 'ISO').DATEISO         Want "ccyy-mm-dd"?
 AIF  ('&DATEFMT' EQ 'OLD').DATEOLD         Want "mm/dd/yy"?
&ECDATE SETC '&SYSDATE'(4,2)'-'             .Get Day of month           00250000
.*                                                                      00250100
&YEAR  SETC '&SYSDATC'(1,4)                   .Grab century from HLASM  00250500
.*                                                                      00250700
 AIF  ('&SYSDATE'(1,2) EQ '01').DATEM01                                 00250800
 AIF  ('&SYSDATE'(1,2) EQ '02').DATEM02                                 00250900
 AIF  ('&SYSDATE'(1,2) EQ '03').DATEM03                                 00251000
 AIF  ('&SYSDATE'(1,2) EQ '04').DATEM04                                 00251100
 AIF  ('&SYSDATE'(1,2) EQ '05').DATEM05                                 00251200
 AIF  ('&SYSDATE'(1,2) EQ '06').DATEM06                                 00251300
 AIF  ('&SYSDATE'(1,2) EQ '07').DATEM07                                 00251400
 AIF  ('&SYSDATE'(1,2) EQ '08').DATEM08                                 00251500
 AIF  ('&SYSDATE'(1,2) EQ '09').DATEM09                                 00251600
 AIF  ('&SYSDATE'(1,2) EQ '10').DATEM10                                 00251700
 AIF  ('&SYSDATE'(1,2) EQ '11').DATEM11                                 00251800
 AIF  ('&SYSDATE'(1,2) EQ '12').DATEM12                                 00251900
 MNOTE 'DA#EYEC-01E Invalid month'                                      00252000
 AGO  .DATEMX                                                           00252100
.*                                                                      00252200
.DATEM01 ANOP ,                                                         00252300
&ECDATE SETC '&ECDATE'.'JAN-&YEAR'                                      00252400
 AGO  .DATEMX                                                           00252500
.DATEM02 ANOP ,                                                         00252600
&ECDATE SETC '&ECDATE'.'FEB-&YEAR'                                      00252700
 AGO  .DATEMX                                                           00252800
.DATEM03 ANOP ,                                                         00252900
&ECDATE SETC '&ECDATE'.'MAR-&YEAR'                                      00253000
 AGO  .DATEMX                                                           00253100
.DATEM04 ANOP ,                                                         00253200
&ECDATE SETC '&ECDATE'.'APR-&YEAR'                                      00253300
 AGO  .DATEMX                                                           00253400
.DATEM05 ANOP ,                                                         00253500
&ECDATE SETC '&ECDATE'.'MAY-&YEAR'                                      00253600
 AGO  .DATEMX                                                           00253700
.DATEM06 ANOP ,                                                         00253800
&ECDATE SETC '&ECDATE'.'JUN-&YEAR'                                      00253900
 AGO  .DATEMX                                                           00254000
.DATEM07 ANOP ,                                                         00254100
&ECDATE SETC '&ECDATE'.'JUL-&YEAR'                                      00254200
 AGO  .DATEMX                                                           00254300
.DATEM08 ANOP ,                                                         00254400
&ECDATE SETC '&ECDATE'.'AUG-&YEAR'                                      00254500
 AGO  .DATEMX                                                           00254600
.DATEM09 ANOP ,                                                         00254700
&ECDATE SETC '&ECDATE'.'SEP-&YEAR'                                      00254800
 AGO  .DATEMX                                                           00254900
.DATEM10 ANOP ,                                                         00255000
&ECDATE SETC '&ECDATE'.'OCT-&YEAR'                                      00255100
 AGO  .DATEMX                                                           00255200
.*                                                                      00255300
.DATEM11 ANOP ,                                                         00255400
&ECDATE SETC '&ECDATE'.'NOV-&YEAR'                                      00255500
 AGO  .DATEMX                                                           00255600
.*                                                                      00255700
.DATEM12 ANOP ,                                                         00255800
&ECDATE SETC '&ECDATE'.'DEC-&YEAR'                                      00255900
.*AGO .DATEMX                                                           00256000
.DATEMX  ANOP ,                                                         00256100
 AGO .DATEX
.*
.DATEOLD ANOP  ,               If not HLASM or higher                   00256200
&ECDATE  SETC  '&SYSDATE'
 AGO .DATEX
.*
.DATEISO ANOP  ,                                                        00256200
&ECDATE  SETC  '&SYSDATC'(1,4).'-'.'&SYSDATC'(5,2).'-'.'&SYSDATC'(7,2)
.DATEX ANOP                                                             00256200
.*                                                                      00256300
.** Time                                                                00256400
.*                                                                      00256500
 AIF  ('&TIMEFMT' EQ 'STD').TIMESTD         Want "hh:mm"
 AIF  ('&SYSTIME'(1,2) LT '12').TIMEN                                   00256600
 AIF  ('&SYSTIME'(1,2) EQ '12').TIMEH12                                 00256700
 AIF  ('&SYSTIME'(1,2) EQ '13').TIMEH13                                 00256800
 AIF  ('&SYSTIME'(1,2) EQ '14').TIMEH14                                 00256900
 AIF  ('&SYSTIME'(1,2) EQ '15').TIMEH15                                 00257000
 AIF  ('&SYSTIME'(1,2) EQ '16').TIMEH16                                 00257100
 AIF  ('&SYSTIME'(1,2) EQ '17').TIMEH17                                 00257200
 AIF  ('&SYSTIME'(1,2) EQ '18').TIMEH18                                 00257300
 AIF  ('&SYSTIME'(1,2) EQ '19').TIMEH19                                 00257400
 AIF  ('&SYSTIME'(1,2) EQ '20').TIMEH20                                 00257500
 AIF  ('&SYSTIME'(1,2) EQ '21').TIMEH21                                 00257600
 AIF  ('&SYSTIME'(1,2) EQ '22').TIMEH22                                 00257700
 AIF  ('&SYSTIME'(1,2) EQ '23').TIMEH23                                 00257800
 AIF  ('&SYSTIME'(1,2) EQ '24').TIMEH24                                 00257900
 AGO  .TIMEX                                                            00258000
.TIMEH12 ANOP ,                                                         00258100
&ECTIME SETC '12.'.'&SYSTIME'(4,2)' PM'                                 00258200
 AGO  .TIMEX                                                            00258300
.TIMEH13 ANOP ,                                                         00258400
&ECTIME SETC '01.'.'&SYSTIME'(4,2)' PM'                                 00258500
 AGO  .TIMEX                                                            00258600
.TIMEH14 ANOP ,                                                         00258700
&ECTIME SETC '02.'.'&SYSTIME'(4,2)' PM'                                 00258800
 AGO  .TIMEX                                                            00258900
.TIMEH15 ANOP ,                                                         00259000
&ECTIME SETC '03.'.'&SYSTIME'(4,2)' PM'                                 00259100
 AGO  .TIMEX                                                            00259200
.TIMEH16 ANOP ,                                                         00259300
&ECTIME SETC '04.'.'&SYSTIME'(4,2)' PM'                                 00259400
 AGO  .TIMEX                                                            00259500
.TIMEH17 ANOP ,                                                         00259600
&ECTIME SETC '05.'.'&SYSTIME'(4,2)' PM'                                 00259700
 AGO  .TIMEX                                                            00259800
.TIMEH18 ANOP ,                                                         00259900
&ECTIME SETC '06.'.'&SYSTIME'(4,2)' PM'                                 00260000
 AGO  .TIMEX                                                            00260100
.TIMEH19 ANOP ,                                                         00260200
&ECTIME SETC '07.'.'&SYSTIME'(4,2)' PM'                                 00260300
 AGO  .TIMEX                                                            00260400
.TIMEH20 ANOP ,                                                         00260500
&ECTIME SETC '08.'.'&SYSTIME'(4,2)' PM'                                 00260600
 AGO  .TIMEX                                                            00260700
.TIMEH21 ANOP ,                                                         00260800
&ECTIME SETC '09.'.'&SYSTIME'(4,2)' PM'                                 00260900
 AGO  .TIMEX                                                            00261000
.TIMEH22 ANOP ,                                                         00261100
&ECTIME SETC '10.'.'&SYSTIME'(4,2)' PM'                                 00261200
 AGO  .TIMEX                                                            00261300
.TIMEH23 ANOP ,                                                         00261400
&ECTIME SETC '11.'.'&SYSTIME'(4,2)' PM'                                 00261500
 AGO  .TIMEX                                                            00261600
.TIMEH24 ANOP ,                                                         00261700
&ECTIME SETC '12.'.'&SYSTIME'(4,2)' AM'                                 00261800
 AGO  .TIMEX                                                            00261900
.TIMEN  ANOP ,                                                          00262000
&ECTIME SETC '&SYSTIME'(1,5)' AM'         .Get Day of month             00262100
 AGO .TIMEX
.TIMESTD ANOP ,
&ECTIME SETC '&SYSTIME'                   .Standard time from ASMblr
.TIMEX ANOP                                                             00262200
.*                                                                      00262300
.** Generate Eye Catcher                                                00262400
.*                                                                      00262500
                B     &CS._ECX            Branch around Eyecatcher      00262800
&CS._ECLEN      DC    AL1(&CS._ECL-1)                                   00262900
&CS._ECNAME     DC    CL8'&CS'                                          00263000
                DC    CL1' '
 AIF ('&ASMINFO' NE 'YES').ECDTX          .Want date and time info?     00263100
&CS._ECDATE     DC    C'&ECDATE'                                        00263200
                DC    C' '
&CS._ECTIME     DC    C'&ECTIME'                                        00263300
                DC    C' '
.ECDTX          ANOP  ,                                                 00263400
 AIF ('&MSG' EQ '').ECMSGX                                              00263500
&CS._ECMSG      DC    C&MSG                                             00263600
.ECMSGX         ANOP  ,                                                 00263700
&CS._ECL        EQU   *-&CS._ECLEN                                      00263800
&CS._ECX        DS    0H                                                00263900
 MEND  ,                                                                00264100
./ ADD NAME=MSGPUT   8000-74029-74029-1240-00010-00010-00000-HERC01  00
         MACRO                                                          00010000
&LABEL   MSGPUT &AMSG                                                   00020000
         AIF   ('&LABEL' EQ '').NOLAB                                   00030000
&LABEL   DS    0H                                                       00040000
.NOLAB   ANOP                                                           00050000
         BLANK MSGTEXT                  CLEAR MESSAGE TEXT AREA         00060000
         MVC   THEWTO(2),=Y(4+L'&AMSG)  INSERT MESSAGE LENGTH           00070000
         MVC   MSGTEXT(L'&AMSG),&AMSG   INSERT MSG BODY                 00080000
         MEXIT                                                          00090000
         MEND                                                           00100000
./ ADD NAME=QTPUT
         MACRO                                                          00000010
&LABEL   QTPUT &MSG                                                     00000020
.********************************************************************** 00000030
.*                                                                      00000040
.*       QUICK FORM OF TPUT TERMINAL INTERFACE ROUTINE(TPUT).           00000050
.*                                                                      00000060
.*       NOTE THAT THE MESSAGE MUST BE ENCLOSED WITHIN QUOTES.          00000070
.*                                                                      00000080
.********************************************************************** 00000090
&LABEL   DS    0H                                                       00000100
         TPUT  MSG&SYSNDX,LEN&SYSNDX SEND USER MESSAGE TO TERMINAL      00000110
         B     NOP&SYSNDX       BYPASS CONSTANTS                        00000120
MSG&SYSNDX DC  C&MSG            MSG TEXT                                00000130
LEN&SYSNDX EQU *-MSG&SYSNDX     MSG SIZE                                00000140
         DS    0H               ALIGNMENT                               00000150
NOP&SYSNDX EQU *                                                        00000160
         MEND                                                           00000170
./ ADD NAME=REGEQU
         MACRO                                                          00000010
         REGEQU                                                         00000020
R0       EQU   0                                                        00000030
R1       EQU   1                                                        00000040
R2       EQU   2                                                        00000050
R3       EQU   3                                                        00000060
R4       EQU   4                                                        00000070
R5       EQU   5                                                        00000080
R6       EQU   6                                                        00000090
R7       EQU   7                                                        00000100
R8       EQU   8                                                        00000110
R9       EQU   9                                                        00000120
R10      EQU   10                                                       00000130
R11      EQU   11                                                       00000140
R12      EQU   12                                                       00000150
R13      EQU   13                                                       00000160
R14      EQU   14                                                       00000170
R15      EQU   15                                                       00000180
         MEND                                                           00000190
./ ADD NAME=REGISTER
         MACRO                            ,
         REGISTER &DUMMY                  , DUMMY PARAMETER
R0       EQU   0                          , GENERAL PURPOSE REGISTER 0
R1       EQU   1                          , GENERAL PURPOSE REGISTER 1
R2       EQU   2                          , GENERAL PURPOSE REGISTER 2
R3       EQU   3                          , GENERAL PURPOSE REGISTER 3
R4       EQU   4                          , GENERAL PURPOSE REGISTER 4
R5       EQU   5                          , GENERAL PURPOSE REGISTER 5
R6       EQU   6                          , GENERAL PURPOSE REGISTER 6
R7       EQU   7                          , GENERAL PURPOSE REGISTER 7
R8       EQU   8                          , GENERAL PURPOSE REGISTER 8
R9       EQU   9                          , GENERAL PURPOSE REGISTER 9
R10      EQU   10                         , GENERAL PURPOSE REGISTER 10
R11      EQU   11                         , GENERAL PURPOSE REGISTER 11
R12      EQU   12                         , GENERAL PURPOSE REGISTER 12
R13      EQU   13                         , GENERAL PURPOSE REGISTER 13
R14      EQU   14                         , GENERAL PURPOSE REGISTER 14
R15      EQU   15                         , GENERAL PURPOSE REGISTER 15
         MEND                             , END OF MACRO
./ ADD NAME=RRTEND   0100-12086-12086-1345-00008-00008-00000-*MHP*   00
         MACRO
&LABEL   RRTEND
.**********************************************************************
.* (C) COPYRIGHT 2007 MHP (IKJ1234I AT YAHOO DOT COM)                 *
.**********************************************************************
&LABEL   DC     X'FF'
         DC     XL11'0'
         MEND
./ ADD NAME=RRTENT   0100-12086-12086-1345-00017-00017-00000-*MHP*   00
         MACRO
&TERMNM  RRTENT &RMTP1=0,&RMTP2=0
.**********************************************************************
.* (C) COPYRIGHT 2007 MHP (IKJ1234I AT YAHOO DOT COM)                 *
.**********************************************************************
         GBLA  &PNETADR
         GBLA  &RSID
         DC    CL8'&TERMNM'
         DC    AL2(&PNETADR+&RSID)
         DC    XL1'&RMTP1'
         DC    XL1'&RMTP2'
&RSID    SETA  &RSID+1
.* IS RSID=1 SUPPOSED TO BE 'RESERVED' ?
         AIF   (&RSID NE 1).RSIDSKP
&RSID    SETA  &RSID+1
.RSIDSKP ANOP
         MEND
./ ADD NAME=RRTTAB   0100-12086-12086-1345-00037-00037-00000-*MHP*   00
         MACRO
&LABEL   RRTTAB &KEYLN=255,&UNITN=1,&BUFPD=28,&NODEL=0,&SUBAREA=0
.**********************************************************************
.* (C) COPYRIGHT 2007 MHP (IKJ1234I AT YAHOO DOT COM)                 *
.**********************************************************************
         LCLA  &AN(16)
&AN(1)   SETA  32768
&AN(2)   SETA  16384
&AN(3)   SETA  8192
&AN(4)   SETA  4096
&AN(5)   SETA  2048
&AN(6)   SETA  1024
&AN(7)   SETA  512
&AN(8)   SETA  256
&AN(9)   SETA  128
&AN(10)  SETA  64
&AN(11)  SETA  32
&AN(12)  SETA  16
&AN(13)  SETA  8
&AN(14)  SETA  4
&AN(15)  SETA  2
&AN(16)  SETA  1
         GBLA  &PNETADR
&PNETADR SETA  &AN(&NODEL)*&SUBAREA
         GBLA  &RSID
&RSID    SETA  0
RRTHDRS  DS    0H
         DC    AL2(RRTHDRE-RRTHDRS) RRTHDRLN  HEADER SIZE
         DC    AL2(&KEYLN)      +2 RRTKEYLN (TSOMCP UNITSZ?)
         DC    AL1(&UNITN)      +4 RRTUNITN (DCBUNTCT)
         DC    AL1(0)           +5 RRTUNITR
         DC    AL1(0)           +6 RRTKEYLR
         DC    AL1(&BUFPD)      +7 RRTBUFPD
         DC    AL1(0)           +8 RRTXTPD
         DC    AL1(&NODEL)      +9 RRTNODEL #BITS IN SUBAREA
RRTHDRE  DS    0H
         MEND
./ ADD NAME=SETMAXCC 8001-74029-74036-1834-00016-00011-00000-HERC01  00
         MACRO                                                          00010000
&LABEL   SETMAXCC &CODE                                                 00020000
         AIF   ('&LABEL' EQ '').NOLAB                                   00030000
&LABEL   DS    0H                     , Set lastcc and maxcc            00040000
.NOLAB   ANOP                                                           00050000
         MVC   LASTCC,=F'&CODE'       , set LASTCC                      00060000
*        IF    (CLC,LASTCC,GT,MAXCC)  , is LASTCC greater than MAXCC    00070000
         IF    (CLC,LASTCC,GT,MAXCC)  , is LASTCC greater than MAXCC    00080000
         MVC   MAXCC,LASTCC           , set MAXCC                       00090000
         ENDIF                                                          00100000
*        IF    (CLC,LASTCC,GT,MEMCC)  , is LASTCC greater than MEMCC    00110000
         IF    (CLC,LASTCC,GT,MEMCC)  , is LASTCC greater than MAXCC    00120000
         MVC   MEMCC,LASTCC           , update MEMCC                    00130000
         ENDIF                                                          00140000
         MEXIT                                                          00150000
         MEND                                                           00160000
./ ADD NAME=STR$CLN  0103-78239-14278-1900-00839-00025-00072-CBT472  00
*********************************************************************** 00080000
*                                                                     * 00090000
* MACRO NAME = STRING                                                 * 00100000
*                                                                     * 00110000
* DESCRIPTIVE NAME = STRING Macro Instruction for Assembler XF        * 00120001
*                                                                     * 00130000
* FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       * 00140000
*            or STRING (of COBOL) to assembler programs.              * 00150000
*                                                                     * 00160000
* STATUS = R101                                                       * 00170001
*                                                                     * 00180000
* AUTHOR = Gilbert Saint-Flour <carlos@gsf-soft.com>                  * 00190000
*                                                                     * 00200000
* ENVIRONMENT = SEE BELOW                                             * 00210000
*                                                                     * 00220000
*    AMODE  = ANY                                                     * 00230001
*    RMODE  = ANY                                                     * 00240001
*     SCP   = S/360 OS, OS/VS, MVS/370                                * 00250001
* Processor = Assembler XF, Assembler H, High-Level Assembler         * 00260001
*     KEY   = ANY                                                     * 00270000
*     MODE  = ANY                                                     * 00280000
*     APF   = ANY                                                     * 00290001
*                                                                     * 00300000
* OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               * 00310000
*                                                                     * 00320000
* INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              * 00330000
*                                                                     * 00340000
* NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   * 00350000
*                                                                     * 00360000
* CHANGE ACTIVITY                                                     * 00370000
*                                                                     * 00380000
* $101 ASM XF version of STRING R514                                  * 00390001
*                   \  - Backport of (%TIME,LENGTH) syntax   / @GSFR518 00391002
* Juergen Winkelmann \   support from STRING version 518     \ @JW14278 00392002
* 2014/10/05         / - Date format DD.MM.YYYY support        @JW14278 00393002
*                   /  - Circumvention of not PoP compliant    @JW-Z390 00394002
*                  /     MVZ instruction emulation in z390     @JW-Z390 00395002
*********************************************************************** 00400000
         MACRO                                                          00410000
&NAME    STRING &INTO=,&PRINT=NOGEN                                     00420000
         GBLA  &$$LIT                                                   00430001
         GBLB  &$$FEAT(16)             FEATURES                         00440000
.*                                       1 LITERALS                     00450000
.*                                       2 REGISTER (BIN)               00460000
.*                                       3 REGISTER (HEX)               00470000
.*                                       4 PACKED                       00480000
.*                                       5 JDATE                        00490000
.*                                       6 BINARY                       00500000
.*                                       7 HEX                          00510000
.*                                       8 NUMERIC                      00520000
.*                                       9 LEFT JUST (NUMERIC)          00530000
.*                                       10 LEADING ZEROES              00540000
.*                                       11 TRUNCATE (CHAR STRING)      00550000
.*                                       12 %TIME                       00560000
         GBLC  &MACVERS                                                 00570001
         GBLC  &$$LITS(9999)           LITERALS                         00580000
         LCLA  &I,&J,&L,&N,&FLAG,&LEN2,&BLANKS                          00590001
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC,&NUMERIC 00600001
         LCLC  &LABEL,&LQ,&STR,&TO1,&TO2,&P1S,&P2C,&P2L,&P3C,&P3L       00610001
         LCLC  &LIT,&ALLFEAT                                            00620001
&MACVERS SETC '101'                    current version                  00630001
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN                              00640000
         PUSH  PRINT                                                    00650000
         PRINT GEN                                                      00660000
.NOGEN   ANOP                                                           00670001
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X00680000
               AND '&SYSLIST(1)' EQ 'GENERATE').GENL                    00690001
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS            00700000
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE                 00710000
&STR     SETC  ' R&MACVERS XF '                                         00720001
&NAME    L     R15,=A(@STR002)&STR     Routine Address                  00730001
         BALR  R14,R15                 CALL @STRING Routine             00740001
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR    00750000
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR        00760000
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)                00770000
         DC    AL2((&LABEL.P-@STRING)/2) OFFSET TO FIELD DESCRIPTORS    00780001
@STRING  CSECT                         NON-ADDRESSABLE CONSTANTS        00790001
&TO1     SETC  '&INTO(1)'                                               00800000
&TO2     SETC  '&LQ&INTO'                                               00810000
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX        00820000
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)   00830000
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)  00840000
.PUNTO3  ANOP                                                           00850000
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)   00860000
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)   00870000
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1)) 00880000
.PUNTO8  ANOP                                                           00890000
&LABEL.P DC    S(&TO1,&TO2)                                             00900000
.*--------------------------------------------------------------------* 00910000
.*-------      FIELDS       ------------------------------------------* 00920000
.*--------------------------------------------------------------------* 00930000
&I       SETA  1                                                        00940000
.*LOOP                                                                  00950000
.LOOP1   ANOP                                                           00960000
         ACTR  200                            SYSDEBUG/DIAG055          00970000
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS     00980000
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP                   00990000
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00                      01000000
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01         01010000
.*--------------------------------------------------------------------* 01020000
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         * 01030000
.*--------------------------------------------------------------------* 01040000
&P1S     SETC  '&SYSLIST(&I,1)'                                         01050000
&P2L     SETC  '0'                     INPUT LENGTH                     01060000
&P3L     SETC  '0'                     OUTPUT LENGTH                    01070000
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES               01080000
         AIF   ('&SYSLIST(&I,1)' EQ '%TIME').FLD190 %TIME      @GSFR518 01090002
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)               01100000
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250                         01110000
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)             01120000
.FLD115  ANOP                                                           01130000
.*                                                                      01140000
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200                         01150000
.*                                                                      01160000
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)                           01170000
.*                                                                      01180000
&L       SETA  1                                                        01190000
.*--LOOP                                                                01200000
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133                             01210000
&L       SETA  &L+1                                                     01220000
         AIF   (&L LT K'&P1S).FLD131                                    01230000
.*--ENDLOOP                                                             01240000
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF             01250000
         AGO   .FLD134                                                  01260000
.FLD133  ANOP                                                           01270000
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE           01280000
.FLD134  ANOP                                                           01290000
.*                                                                      01300000
.*XF     AIF   (NOT D'&P2L).FLD140                                      01310001
&P2C     SETC  T'&P2L                                                   01320000
.*MNOTE *,'&P1 &P2C'                                                    01330000
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220 01340000
         AIF   ('&P2C' EQ 'G').FLD210  FL2                              01350000
.FLD140  ANOP                                                           01360000
.*                                                                      01370000
.*       EXTRACT PSATOLD FROM PSATOLD-PSA                               01380000
.*                                                                      01390000
&L       SETA  1                                                        01400000
.*--LOOP                                                                01410000
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143                             01420000
         AIF   ('&P2L'(&L,1) EQ '+').FLD143                             01430000
&L       SETA  &L+1                                                     01440000
         AIF   (&L LT K'&P2L).FLD141                                    01450000
.*--ENDLOOP                                                             01460000
&P2L     SETC  '&LQ&P2L'               L'ABCDEF                         01470000
         AGO   .FLD300                                                  01480000
.FLD143  ANOP                                                           01490000
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA       01500000
         AGO   .FLD300                                                  01510000
.*                                                                      01520000
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800         01530000
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12                    01540000
         AIF   (&LAST).FLD186                                           01550000
         DC    X'60',AL1(&P2L)         BLANKS                           01560000
         AGO   .LIT90                                                   01570000
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS                           01580000
         AGO   .LIT90                                                   01590000
.*                                                                      01600000
.FLD190  AIF   (N'&SYSLIST(&I) GT 2).FLD995                    @GSFR518 01610002
&P1S     SETC  '1(14)'                 %TIME                            01620000
&$$FEAT(12) SETB 1                     %TIME                            01630000
&P2L     SETC  '12'                    hh:mm:ss.hh             @GSFR518 01631002
         AIF   (N'&SYSLIST(&I) EQ 1).FLD1905                   @GSFR518 01632002
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD1904               @GSFR518 01633002
&P2L     SETC  '&SYSLIST(&I,2)'        5,8,11,12               @GSFR518 01634002
&L       SETA  &SYSLIST(&I,2)                                  @GSFR518 01635002
         AIF   (&L EQ 5 OR &L EQ 8 OR &L EQ 11 OR &L EQ 12).FLD1905 518 01636002
.FLD1904 MNOTE 8,'Second sub-parameter is invalid: &SYSLIST(&I)'    518 01637002
.FLD1905 ANOP                                                  @GSFR518 01638002
.*MNOTE 1,'FLD1905 &SYSLIST(&I) &P1S &P2L '                    @GSFR518 01639002
         AGO   .FLD800                                                  01640000
.*--------------------------------------------------------------------* 01650000
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    * 01660000
.*--------------------------------------------------------------------* 01670000
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED     01680000
&P2C     SETC  '&SYSLIST(&I,2)'                                         01690000
         AGO   .FLD220                                                  01700000
.*T'&P1=G                                                               01710000
.FLD210  ANOP                                                           01720000
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'                      01730000
&P2C     SETC  'FL&L'                  T'&P1 = 'G'                      01740000
.*                                                                      01750000
.FLD220  ANOP                                                           01760000
&P2L     SETC  '0&P2C'                 (R2) LENGTH                      01770000
         AIF   ('&P2C'(1,1) EQ '(').FLD300                              01780000
&P2L     SETC  '&P2C'                  3(R2) LENGTH                     01790000
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300                         01800000
&P2L     SETC  '0'                                                      01810000
&PACKED  SETB  ('&P2C' EQ 'P')                                          01820000
         AIF   (&PACKED).FLD290                                         01830000
&P2L     SETC  '1'                                                      01840000
         AIF   ('&P2C' EQ 'FL1').FLD240                                 01850000
&P2L     SETC  '3'                                                      01860000
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240                01870000
&P2L     SETC  '7'                                                      01880000
         AIF   ('&P2C' EQ 'FL3').FLD240                                 01890000
&P2L     SETC  '15'                                                     01900000
         AIF   ('&P2C' EQ 'F').FLD240                                   01910000
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD          01920000
         AGO   .FLD300                                                  01930000
.*                                                                      01940000
.FLD240  ANOP                          BINARY VARIABLE                  01950000
&BIN     SETB  1                                                        01960000
         AGO   .FLD300                                                  01970000
.*                                                                      01980000
.FLD250  ANOP                          REGISTER CONTENT                 01990000
&REG     SETB  1                                                        02000000
         AGO   .FLD300                                                  02010000
.*                                                                      02020000
.FLD290  ANOP                          PACKED                           02030000
&P2L     SETC  '1'                                                      02040000
.*--------------------------------------------------------------------* 02050000
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   * 02060000
.*--------------------------------------------------------------------* 02070000
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800                         02080000
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL                    02090000
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE                       02100000
         AIF   (&HEX OR &TRUNC).FLD800                                  02110000
.*                                                                      02120000
&P3C     SETC  '&SYSLIST(&I,3)'                                         02130000
&P3L     SETC  '248'                                                    02140000
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308              02150000
&P3L     SETC  '249'                                                    02160000
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308                02170000
&P3L     SETC  '250'                                                    02180000
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308                02190000
&P3L     SETC  '251'                                                    02200000
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308                02210000
&P3L     SETC  '252'                                                    02220000
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308                02230000
&P3L     SETC  '253'                                                    02240000
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308                02250000
&P3L     SETC  '254'                                           @JW14278 02253002
         AIF   ('&P3C' EQ 'DD.MM.YYYY' AND &PACKED).FLD308     @JW14278 02256002
&P3L     SETC  '0'                                                      02260000
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310                         02270000
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'       02280000
         AGO   .FLD310                                                  02290000
.FLD308  ANOP                                                           02300000
&$$FEAT(5) SETB 1                      JDATE                            02310000
         AGO   .FLD800                                                  02320000
.*--LOOP                                                                02330000
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT                02340000
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT                02350000
         AIF   ('&P3C'(1,1) NE 'L').FLD311                              02360000
&LEFT    SETB  1                                                        02370000
         AGO   .FLD318                                                  02380000
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312                              02390000
&ZERO    SETB  1                                                        02400000
         AGO   .FLD318                                                  02410000
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993       nn in RnnB is not num  02420000
         AIF   ('&P3C'(1,1) GT '9').FLD993       nn in RnnB is not num  02430000
&P3L     SETC  '&P3L'.'&P3C'(1,1)                                       02440000
.FLD318  ANOP                                                           02450000
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'                          02460000
&P3C     SETC  '&P3C '(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER        02470001
         AIF   (K'&P3C GT 0).FLD310                                     02480000
.*--ENDLOOP                                                             02490000
         AIF   (&P3L GT 16).FLD993               nn in RnnB is too big  02500000
.*--------------------------------------------------------------------* 02510000
.FLD800  ANOP                                                           02520000
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))                 02530000
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))                     02540000
&LEFT    SETB  (&LEFT AND &NUMERIC)                                     02550000
         AIF   (NOT &NUMERIC).FLD810                                    02560000
         AIF   (&LEFT OR '&P3L' NE '0').FLD810                          02570000
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))     02580000
         AIF   (&REG).FLD810                                            02590000
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH            02600000
         AIF   ('&P2C' EQ 'FL1').FLD810                                 02610000
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH            02620000
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810                02630000
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH            02640000
.FLD810  ANOP                                                           02650000
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1                           02660000
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L                       02670000
&$$FEAT(2) SETB (&$$FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)    02680000
&$$FEAT(3) SETB (&$$FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)    02690000
&$$FEAT(4) SETB (&$$FEAT(4) OR &PACKED)               PACKED            02700000
&$$FEAT(6) SETB (&$$FEAT(6) OR &BIN)                  BINARY            02710000
&$$FEAT(7) SETB (&$$FEAT(7) OR (&HEX AND NOT &REG))   HEX               02720000
&$$FEAT(8) SETB (&$$FEAT(8) OR &NUMERIC)              BIN,PACKED        02730000
&$$FEAT(9) SETB (&$$FEAT(9) OR (&LEFT AND &NUMERIC))                    02740000
&$$FEAT(10) SETB (&$$FEAT(10) OR &ZERO)                                 02750000
&$$FEAT(11) SETB (&$$FEAT(11) OR &TRUNC)                                02760000
&BIN     SETB  0                    RESET FLAGS                         02770000
&HEX     SETB  0                    RESET FLAGS                         02780000
&REG     SETB  0                    RESET FLAGS                         02790000
&PACKED  SETB  0                    RESET FLAGS                         02800000
&LEFT    SETB  0                    RESET FLAGS                         02810000
&ZERO    SETB  0                    RESET FLAGS                         02820000
&TRUNC   SETB  0                    RESET FLAGS                         02830000
         AIF   (&FLAG GE 10).FLD995                                     02840000
         AIF   (&LAST).FLD816                                           02850000
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)                      02860000
         AGO   .LIT99                                                   02870000
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)                02880000
         AGO   .LIT99                                                   02890000
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'                02900000
         AGO   .LIT99                                                   02910000
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''    02920000
         AGO   .LIT99                                                   02930000
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'      02940000
         AGO   .LIT99                                                   02950000
.*--------------------------------------------------------------------* 02960000
.*------------ LITERALS ----------------------------------------------* 02970000
.*--------------------------------------------------------------------* 02980000
.LIT00   ANOP                                                           02990000
&LIT     SETC  'C&SYSLIST(&I)'                                          03000000
         AGO   .LIT09                                                   03010000
.LIT01   ANOP                                                           03020000
&LIT     SETC  '&SYSLIST(&I)'                                           03030000
.LIT09   ANOP                              calculate length of literal  03040000
&J       SETA  3                                                        03050000
&L       SETA  0                                                        03060000
         ACTR  K'&LIT+K'&LIT+100                                        03070000
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X                              03080000
.*LOOP                                                                  03090000
.LIT11C AIF ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&'(1,1)).LIT12C 03100000
&J       SETA  &J+1                                                     03110000
.LIT12C  ANOP                                                           03120000
&J       SETA  &J+1                                                     03130000
&L       SETA  &L+1                                                     03140000
         AIF   (&J LT K'&LIT).LIT11C                                    03150000
.*ENDLOOP                                                               03160000
         AGO   .LIT15                                                   03170000
.*LOOP                                                                  03180000
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X                             03190000
&L       SETA  &L+1                                                     03200000
.LIT12X  ANOP                                                           03210000
&J       SETA  &J+1                                                     03220000
         AIF   (&J LT K'&LIT).LIT11X                                    03230000
.*ENDLOOP                                                               03240000
&L       SETA  (&L+1)/2                                                 03250000
.LIT15   ANOP                               generate in-line literal    03260000
         AIF   (&L GT 5).LIT40                                          03270000
         AIF   (&LAST).LIT16                                            03280000
         DC    X'4&L',&LIT                                              03290001
         AGO   .LIT90                                                   03300000
.LIT16   DC    X'C&L',&LIT,0S(0)                                        03310000
         AGO   .LIT90                                                   03320000
.LIT40   ANOP                                  check literal table      03330000
         AIF   (&$$LIT EQ 0).LIT50                                      03340000
&N       SETA  1                                                        03350000
         ACTR  &$$LIT*3+200                                             03360000
.LIT41   AIF   ('&LIT' EQ '&$$LITS(&N)').LIT80       LOOP               03370000
&N       SETA  &N+1                                  LOOP               03380000
         AIF   (&N LE &$$LIT).LIT41                  LOOP               03390000
.LIT50   ANOP                                                           03400000
&$$LIT   SETA  &$$LIT+1                                                 03410000
&$$LITS(&$$LIT) SETC '&LIT'                                             03420000
&N       SETA  &$$LIT                                                   03430000
.LIT80   ANOP                               generate remote literal     03440000
&N       SETA  &N+1000                                                  03450000
         AIF   (&LAST).LIT86                                            03460000
         DC    X'40',AL1(&L),AL2($LIT&N-*)                              03470001
         AGO   .LIT90                                                   03480000
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)                        03490001
.LIT90   ANOP                                                           03500000
&$$FEAT(1) SETB 1                      LITERAL                          03510000
.LIT99   ANOP                                                           03520000
.*--------------------------------------------------------------------* 03530000
&I       SETA  1+&I                              LOOP                   03540000
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP                   03550000
.*ENDLOOP                                                               03560000
&SYSECT  CSECT                                                          03570001
         AGO   .MEND                                                    03580000
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'          03590000
         AGO   .MEND                                                    03600000
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'                   03610000
         AGO   .MEND                                                    03620000
.********************************************************************** 03630000
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           * 03640000
.********************************************************************** 03650000
.GENL    ANOP                                                           03660000
&ALLFEAT SETC  '&$$FEAT(1)&$$FEAT(2)&$$FEAT(3)&$$FEAT(4)'               03670000
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(5)&$$FEAT(7)&$$FEAT(6)&$$FEAT(8)'       03680000
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(9)&$$FEAT(10)&$$FEAT(11)&$$FEAT(12)'    03690000
&BLANKS  SETA  2                                                        03700001
         MNOTE *,'STRING R&MACVERS - FEATURES GENERATED: &ALLFEAT'      03710001
@STRING  CSECT                                                          03720001
         AIF   (&$$LIT EQ 0).GENL3                                      03730000
.GENL2   ANOP                                LOOP                       03740000
&N       SETA  &N+1                          LOOP                       03750000
&I       SETA  &N+1000                       LOOP                       03760000
$LIT&I   DC    &$$LITS(&N)                                              03770000
         AIF   (&N LT &$$LIT).GENL2          LOOP                       03780000
.GENL3   DC    0H'0'                                                    03790001
.********************************************************************** 03800000
.*                                                                    * 03810000
.*       STRING SUB-ROUTINE                                           * 03820000
.*                                                                    * 03830000
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         * 03840000
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         * 03850000
.*                                                                    * 03860000
.********************************************************************** 03870000
@00      EQU   0                       WORK REGISTER                    03880000
@01      EQU   1                       WORK REGISTER                    03890000
@02      EQU   2                       WORK REGISTER                    03900000
@03      EQU   3                       WORK REGISTER                    03910000
@04      EQU   4                       WORK REGISTER                    03920000
@05      EQU   5                       WORK REGISTER                    03930000
@06      EQU   6                       WORK REGISTER                    03940000
@13      EQU   13                      CALLER'S SAVE AREA               03950000
@14      EQU   14                      WORK REGISTER                    03960000
@15      EQU   15                      BASE REG                         03970000
         USING @STR002,@15                                              03980001
         USING @STRSAVE,@13                                             03990000
@STR002  B     @STR011                 BRANCH AROUND EYE-CATCHER        04000001
         DC    AL1(@STR003-*),C'@STRING/XF R&MACVERS &ALLFEAT',0H'0'    04010001
@STR003  DC    Y(@STR002-@STRING)      Offset to @STRING                04020001
@STR011  STM   @14,@06,12(@13)         Save caller's registers          04030001
         SLR   @06,@06                 R6=0                             04040000
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET     04050000
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET         04060000
         ALR   @06,@15                 R6 NOW POINTS TO PARM LIST       04070001
         SH    @06,@STR003             R6 NOW POINTS TO PARM LIST       04080001
         USING @STRSCON,@06                                             04090000
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)        04100000
         BAL   @14,@STRS2A             GET ADDRESS IN R2                04120000
         LA    @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD     04130000
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO     04140000
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD 04150000
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)       04160000
         BAL   @14,@STRS2A             GET LENGTH IN R2                 04170000
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD      04180000
         CR    @05,@04                 END ADDRESS?                     04190000
         BL    @STR282                 NO, JUMP                         04200000
         SR    @05,@04                 CALCULATE LENGTH                 04210000
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER           04220000
.********************************************************************** 04230000
.*       MOVE FIELDS TO OUTPUT AREA                                   * 04240000
.********************************************************************** 04250000
         LA    @06,@STRNEXT            POINT TO 1ST FIELD DESC          04260000
         USING @STRPARM,@06                                             04270000
.*LOOP                                                                  04280000
@STR310  EQU   *                                                        04290000
         AIF   (NOT &$$FEAT(1)).FEAT1A                                  04300000
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?               04310000
         BO    @STR372                 YES, JUMP                        04320000
.FEAT1A  ANOP                                                           04330000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23A              04340000
         TM    @STRFLAG,@STRREG        REGISTER?                        04350000
         BO    @STR323                 YES, JUMP                        04360000
.FEAT23A ANOP                                                           04370000
         AIF   (NOT &$$FEAT(12)).FEAT12A                                04380000
         CLI   @STRSCON,X'E0'          IS IT %TIME ?                    04390000
         BE    @STR378                 YES, JUMP                        04400000
.FEAT12A ANOP                                                           04410000
.*                                                                      04420000
.*       IT'S A FIELD (SCON)                                            04430000
.*                                                                      04440000
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)       04450000
         BAL   @14,@STRS2A             GET LENGTH IN R2                 04460000
         AIF   (&$$FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)    04470000
         LTR   @03,@02                 KEEP/TEST LENGTH                 04480000
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT         04490000
         AGO   .FEAT9E2                                                 04500000
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH                 04510000
         BP    @STR313                 LENGTH POSITIVE, JUMP            04520000
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?              04530000
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK        04540000
@STR313  EQU   *                                                        04550000
.FEAT9E2 ANOP                                                           04560000
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)        04570000
         BAL   @14,@STRS2A             GET ADDRESS IN R2                04580000
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO     04590000
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD 04600000
         CR    @03,@02                 END ADDRESS?                     04610000
         BL    @STR314                 NO, JUMP                         04620000
         LA    @00,X'0080'             PSA ADDRESS                      04630000
         CLR   @02,@00                 PSA REFERENCE?                   04640000
         BL    @STR314                 YES, JUMP                        04650000
         SR    @03,@02                 CALCULATE LENGTH                 04660000
@STR314  EQU   *                                                        04670000
         AIF   (NOT &$$FEAT(6)).FEAT6A                                  04680000
         TM    @STRFLAG,@STRBIN        BINARY FIELD?                    04690000
         BO    @STR328                 YES, JUMP                        04700000
.FEAT6A  ANOP                                                           04710000
         AIF   (NOT &$$FEAT(4)).FEAT4A                                  04720000
         TM    @STRFLAG,@STRPACK       PACKED FIELD?                    04730000
         BO    @STR351                 YES, JUMP                        04740000
.FEAT4A  ANOP                                                           04750000
         AIF   (NOT &$$FEAT(7)).FEAT7A                                  04760000
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?                     04770000
         BO    @STR376                 YES, JUMP                        04780000
.FEAT7A  ANOP                                                           04790000
.*                                                                      04800000
.*       TRUNCATE CHARACTER STRING                                      04810000
.*                                                                      04820000
         AIF   (NOT &$$FEAT(11)).FEAT11A                                04830000
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN  04840000
         BNE   @STR390                 NO STRING TRUNCATION, JUMP       04850000
         LA    @01,0(@03,@02)          FIRST BYTE AFTER FIELD           04860000
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP 04870000
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP 04880000
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP 04890000
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP 04900000
         B     @STR398                 BLANK FIELD, DO NOT EDIT         04910000
         AGO   .FEAT11B                                                 04920000
.FEAT11A ANOP                                                           04930000
         AIF (&$$FEAT(2)+&$$FEAT(3)+&$$FEAT(4)+&$$FEAT(6) EQ 0).FEAT11B 04940000
         B     @STR390                 EDIT                             04950000
.FEAT11B ANOP                                                           04960000
.*                                                                      04970000
.*       REGISTER (R0-R13)                                              04980000
.*                                                                      04990000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23B              05000000
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT              05010000
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31         05020000
         EX    @01,@STR323L            COPY R7-R13 INTO R0              05030000
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?                  05040000
         BNL   @STR323T                YES, JUMP                        05050000
         SLL   @01,2                   R1= 000000BB BASE * 4            05060000
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6          05070000
@STR323T EQU   *                                                        05080000
         AIF   (NOT &$$FEAT(3)).FEAT3R REG,HEX                          05090000
         AIF   (NOT &$$FEAT(2)).FEAT2H REG,BIN                          05100000
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?                     05110000
         BNO   @STR330                 NO, EDIT FWD                     05120000
.FEAT2H  ANOP                                                           05130000
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK            05140000
         LA    @03,8                   OUTPUT LENGTH                    05150000
         B     @STR376X                EDIT IN HEX                      05160000
.FEAT3R  ANOP                                                           05170000
         AIF   (NOT &$$FEAT(6)).FEAT23B                                 05180000
         B     @STR330                 EDIT R0                          05190000
.FEAT23B ANOP                                                           05200000
.*                                                                      05210000
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)           05220000
.*                                                                      05230000
         AIF   (NOT &$$FEAT(6)).FEAT6B                                  05240000
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**         05250000
@STR328  SLR   @00,@00                                                  05260000
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE         05270000
.FEAT6B  ANOP                                                           05280000
.*                                                                      05290000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(6)).FEAT6C               05300000
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL         05310000
         AIF   (NOT &$$FEAT(4)).FEAT6C                                  05320000
         B     @STR361                 EDIT DWD                         05330000
.FEAT6C  ANOP                                                           05340000
.*                                                                      05350000
.*       PACKED FIELD                                                   05360000
.*                                                                      05370000
         AIF   (NOT &$$FEAT(4)).FEAT4B                                  05380000
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD                  05390000
@STR351  LA    @03,0(,@02)             FIRST BYTE OF PACKED FIELD       05400000
         BALR  @14,0                                                    05410000
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?           05420000
         LA    @03,1(,@03)              (NEXT BYTE)                     05430000
         BNOR  @14                     NO, LOOP MORE                    05440000
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD       05450000
         BCTR  @03,0                                                    05460000
         EX    @03,@STRZAP             EXECUTE ZAP                      05470000
         AIF   (NOT &$$FEAT(5)).FEAT4B                                  05480000
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?           05490000
         BNL   @STR375                 YES, JUMP                        05500000
.FEAT4B  ANOP                          PACKED                           05510000
.*                                                                      05520000
.*       EDIT @STRDWD (BIN, REG, PACKED)                                05530000
.*                                                                      05540000
         AIF   (NOT &$$FEAT(8)).FEAT8B                                  05550000
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH                    05560000
         LA    @03,X'003F'             MASK FOR "AND"                   05570000
         NR    @03,@00                 OUTPUT LENGTH                    05580000
         MVC   @STRWK16(16),@STRMASK   EDIT MASK                        05590000
.*                                                                      05600000
.*       LEFT-JUSTIFICATION (NUMERIC)                                   05610000
.*                                                                      05620000
         AIF   (NOT &$$FEAT(9)).FEAT9B                                  05630000
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?              05640000
         BNO   @STR367                 NO, JUMP                         05650000
         LA    @01,@STRWK16+15         PREVENT BAD R1                   05660000
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL                    05670000
         LA    @02,0(,@01)             FIRST STRING POSITION            05680000
         LTR   @03,@03                 CHECK OUTPUT LENGTH              05690000
         BNZ   @STR363                 JUMP IF NOT ZERO                 05700000
.*       L0    (LEFT JUSTIFIED, NO PADDING)                             05710000
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING      05720000
         SR    @03,@02                 COMPUTE STRING LENGTH            05730000
         B     @STR390                 MOVE FIELD TO OUTPUT LINE        05740000
.*       L1-L63 (LEFT JUSTIFIED, PADDING)                               05750000
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN  05760000
         BNH   @STR364                 LARGE ENOUGH, JUMP               05770000
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.   05780000
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH         05790000
         LR    @00,@04                 POINTER IN OUTPUT LINE           05800000
         LR    @01,@03                 LENGTH WITH PADDING              05810000
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING      05820000
         SR    @03,@02                 COMPUTE STRING LENGTH            05830000
         B     @STR392                 MOVE FIELD TO OUTPUT LINE        05840000
@STR367  EQU   *                                                        05850000
.FEAT9B  ANOP                                                           05860000
         AIF   (NOT &$$FEAT(10)).FEAT10B                                05870000
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?            05880000
         BNO   @STR368                 NO, JUMP                         05890000
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'        05900000
@STR368  EQU   *                                                        05910000
.FEAT10B ANOP                                                           05920000
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL                    05930000
         LA    @02,@STRWK16+16         FIRST POSITION AFTER STRING      05940000
         SR    @02,@03                 FIRST STRING POSITION            05950000
.FEAT8B  ANOP                                                           05970000
.*                                                                      05980000
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)                          05990000
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)   06000000
.*       BLANKS  (@STRSCON=ZERO)                                        06010000
.*                                                                      06020000
         AIF   (NOT &$$FEAT(1)).FEAT1B                                  06030000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06040000
@STR372  LA    @01,7                   mask for NR                      06050000
         LA    @02,@STRLEN2            1st byte of short literal        06060000
         IC    @03,@STRFLAG            pick up flags+length             06070000
         NR    @03,@01                 R3 has length of short literal   06080000
         BNZ   @STR390                 short literal, go move it        06090000
         SLR   @02,@02                 Clear Address Register           06100000
         IC    @03,@STRLEN2            GET LITERAL LENGTH               06110000
         TM    @STRFLAG,@STRX40        string of spaces?                06120000
         BO    @STR390                 yes, go move them                06130000
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET              06140000
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS   06150000
.FEAT1B  ANOP                                                           06160000
.*                                                                      06170000
.*       CONVERT JULIAN DATE TO YYMMDD                                  06180000
.*                                                                      06190000
         AIF   (NOT &$$FEAT(5)).FEAT5F                                  06200000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06210000
@STR375  LA    @00,248                 MASK FOR 'SLR'                   06220000
         SLR   @01,@01                                                  06230000
         IC    @01,@STRLEN2            248-255                          06240000
         SLR   @01,@00                 000-007                          06250000
         LA    @00,12                  L'@STR375W                       06260000
         MR    @00,@00                 COMPUTE OFFSET                   06270000
         LA    @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE         06280000
         SLR   @03,@03                                                  06290000
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)      06300000
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT 06310000
         BNZ   @STR375B                NO, JUMP                    @JDT 06320000
@STR375Z LA    @02,@BLANKS             WORK AREA                        06330001
&BLANKS  SETA  10                      WE NEED AT LEAST 10 BLANKS       06340001
         B     @STR390                 MOVE FIELD TO OUTPUT LINE        06350000
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248        06360000
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249        06370000
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250        06380000
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251        06390000
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252        06400000
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253        06410000
         DC    AL1(10,C'.',6,7,8,4,5,8,0,1,2,3) DD.MM.YYYY 254 @JW14278 06420002
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255        06430000
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'                06440000
         DC    P'999'                  Prevent S0C7 with 90366          06450000
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?              06460000
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)   06470000
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20           06480000
         BE    @STR375C                CC=01, USE CC=20                 06490000
         CLI   @STRDWD+4+1,X'50'       YY<50?                           06500000
         BL    @STR375C                YES, USE CC=20                   06510000
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19                  06520000
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?                           06530000
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER            06540000
         LA    @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS) 06550000
         TM    @STRDWD+4+1,X'01'       ODD YEARS                        06560000
         BO    @STR375N                  AREN'T LEAP YEARS              06570000
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992 06580000
         BNM   @STR375L                MIXED IN 1982/1990               06590000
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD          06600000
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?    06610000
         BNH   @STR375L                NO, JUMP                         06620000
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD      06630000
.*--LOOP WHILE DDD > 0                                                  06640000
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH    06650000
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE     06660000
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD       06670000
         BP    @STR375L                                                 06680000
.*--ENDLOOP                                                             06690000
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION       06700000
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??               06710000
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD             06720000
         MVZ   @STRWK16+1(5),@STRWK16  temporary circumvention @JW-Z390 06730002
         MVZ   @STRWK16+6(2),@STRWK16  of MVZ bug under z390   @JW-Z390 06733002
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR                        06740000
         LA    @02,@STRWK16+9          WORK AREA                        06750000
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK          06760000
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT 06770000
.FEAT5F  ANOP                          JDATE                            06780000
.*                                                                      06790000
.*       HEX STRING                                                     06800000
.*                                                                      06810000
         AIF   (NOT &$$FEAT(7)).FEAT7B                                  06820000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06830000
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK             06840000
@STR376  LA    @00,8                   MAX LENGTH                       06850000
         CLR   @03,@00                 CHECK LENGTH                     06860000
         BNH   @STR376B                JUMP IF LE 8                     06870000
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH       06880000
@STR376B LR    @01,@03                 INPUT LENGTH                     06890000
         BCTR  @01,0                                                    06900000
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE        06910000
         ALR   @03,@03                 OUTPUT LENGTH                    06920000
         AGO   .FEAT37B                                                 06930000
.FEAT7B  ANOP                                                           06940000
         AIF   (NOT &$$FEAT(3)).FEAT37C                                 06950000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06960000
.FEAT37B ANOP                                                           06970000
@STR376X LA    @02,@STRWK16            WORK AREA                        06980000
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"     06990000
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"     07000000
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'             07010000
.FEAT37C ANOP                                                           07020000
.*                                                                      07030000
.*       %TIME                                                          07040000
.*                                                                      07050000
         AIF   (NOT &$$FEAT(12)).FEAT12B                                07060000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       07070000
@STRTIME DC    X'4021207A20207A20204B20204000'    0X:XX:XX.XX  @GSFR518 07080002
@STR378  LR    @02,@15                 SAVE BASE REG                    07090000
         TIME  DEC                     GET HHMMSSHH                     07100000
         LR    @15,@02                 RESTORE BASE REG                 07110000
         ST    @00,@STRDWD             STORE HHMMSSHH                   07120000
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK                   07130000
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS.HH        @GSFR518 07140002
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON    @JW14278 07145002
         BAL   @14,@STRS2A             GET LENGTH IN R2        @JW14278 07150002
         LR    @03,@02                 GET LENGTH IN R3        @JW14278 07155002
         LA    @02,@STRWK16+1          WORK AREA               @JW14278 07160002
.FEAT12B ANOP                                                           07170000
.*MOVE                                                                  07180000
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN  07190000
         BNH   @STR391                 LARGE ENOUGH, JUMP               07200000
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.   07210000
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH         07220000
         LR    @00,@04                 POINTER IN OUTPUT LINE           07230000
         LR    @01,@03                 PASS REMAINING LENGTH            07240000
         LTR   @02,@02                 BLANKS?                          07250000
         BNZ   @STR392                 NO, JUMP                         07260000
         SLR   @03,@03                 YES, ZERO LENGTH                 07270000
@STR392  ICM   @03,B'1000',@BLANKS     PAD WITH BLANKS                  07280001
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE        07290000
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE       07300000
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR        07310000
         BO    @STR399                 Done, exit                       07320000
         AIF   (NOT &$$FEAT(1)).FEAT1C                                  07330000
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?              07340000
         BM    @STR398L                Literal, not spaces              07350000
         BZ    @STR398X                Neither literal nor spaces       07360000
         LA    @06,@STRSCON            2-byte entry for blank spaces    07370000
         B     @STR310                 PROCESS NEXT ENTRY               07380000
@STR398L LA    @01,7                   mask for NR                      07390000
         IC    @03,@STRFLAG            pick up flags+length             07400000
         NR    @03,@01                 R3 has length of short literal   07410000
         BZ    @STR398T                not an in-line literal, jump     07420000
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal 07430000
         B     @STR310                 PROCESS NEXT ENTRY               07440000
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal   07450000
         B     @STR310                 PROCESS NEXT ENTRY               07460000
.FEAT1C  ANOP                                                           07470000
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY            07480000
         B     @STR310                 PROCESS NEXT ENTRY               07490000
.*ENDLOOP                                                               07500000
.*                                                                      07510000
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS                       07520000
.*                                                                      07530000
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING            07540000
         ICM   @01,B'1000',@BLANKS     SET UP R1 FOR PADDING            07550001
.***     DROP  @06,@13,@15                                              07560000
         LA    @14,2                   INCREMENT                        07570000
         AL    @14,12(,@13)            RETURN ADDRESS                   07580000
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD  07590000
         SL    @15,8(,@13)             CALCULATE LENGTH USED            07600000
         MVCL  @04,@00                 PAD WITH BLANKS                  07610000
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS           07620000
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL       07630000
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN    07640000
         BR    @14                     RETURN TO CALLER                 07650000
.*                                                                      07660000
         AIF   (NOT (&$$FEAT(3) OR &$$FEAT(7))).FEAT37T                 07670000
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION         07680000
.FEAT37T ANOP                                                           07690000
         AIF   (NOT &$$FEAT(8)).FEAT8T                                  07700000
@STRMASK DC    X'4020202020202020,2020202020202120'                     07710000
.FEAT8T  ANOP                                                           07720000
.********************************************************************** 07730000
.*       Convert S-con to address                                     * 07740000
.*             Input: GPR2 points to an S-CON in the remote parm list * 07750000
.*             Output: GPR2 contains the address                      * 07760000
.********************************************************************** 07770000
@STRS2A  SLR   @00,@00                                                  07780000
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD                    07790000
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....      07810000
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)     07820000
         CLI   0(@02),@06*16+15        R7-R13?                          07830000
         BH    @STRS2A3                YES, JUMP                        07840000
.*BASE REG IS R0-R6                                                     07850000
         LTR   @02,@00                 IS R0 THE BASE REG?              07860000
         BNZ   @STRS2A2                NO, JUMP                         07870000
         LTR   @02,@01                 IS THIS A PSA ADDRESS?           07880000
         BNZR  @14                     YES, GOBACK                      07890000
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4            07900000
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE           07910000
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL      07920000
         BR    @14                                                      07930000
.*BASE REG IS R7-R13                                                    07940000
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)   07950000
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL      07960000
         BR    @14                                                      07970000
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL      07980000
@BLANKS  DC    CL((((*+&BLANKS+7-@STRING)/8)*8)-(*-@STRING))' '         07990001
@STRSIZE EQU   *-@STRING               SIZE OF GENERATED CSECT          08000001
         DROP  @06,@13,@15                                              08010000
.********************************************************************** 08020000
.*       WORK AREA (CALLER'S SAVE AREA)                               * 08030000
.********************************************************************** 08040000
@STRSAVE DSECT                         24-BYTE WORK AREA                08050000
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)               08060000
@STRWK16 DS    F'7,8,9,10'             WORK AREA                        08070000
@STRDWD  DS    D'1112'                 WORK AREA                        08080000
@STRPARM DSECT                                                          08090000
@STRFLAG DS    B                   +0  FORMAT, FLAGS                    08100000
@STRLAST EQU   X'80'                     LAST ENTRY                     08110000
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET 08120000
@STRX40  EQU   X'20'                   String of Spaces                 08130000
.*             X'0F'                   CONVERSION REQUIRED              08140000
.*                                     or length of short literal       08150000
@STRHEX  EQU   X'08'                     HEXADECIMAL                    08160000
@STRBIN  EQU   X'04'                     BINARY                         08170000
@STRPACK EQU   X'02'                     PACKED                         08180000
@STRREG  EQU   X'01'                     REGISTER                       08190000
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH            08200000
.*                                     or start of short literal        08210000
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION             08220000
@STRZERO EQU   X'40'                     LEADING ZEROES                 08230000
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.  08240000
@STRSCON DS    S                   +2  FIELD ADDRESS                    08250000
@STRFLEN DS    S                   +4  FIELD LENGTH                     08260000
@STRNEXT EQU   *                   +6                                   08270000
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99                             08280000
         POP   PRINT                                                    08290000
.MEND99  MEND                                                           08300000
./ ALIAS NAME=STRING
./ ADD NAME=STR$GSF  0102-78239-14278-1900-01194-00025-00129-CBT472  00
*/GILBERTF JOB (ACCT#),STRINGXF,                                        00010001
*/ NOTIFY=&SYSUID,                                                      00020001
*/ CLASS=A,MSGCLASS=X,COND=(0,NE)                                       00030001
*/XFASM EXEC PGM=IFOX00,PARM=(OBJECT,NODECK,ESD,NORLD,NOXREF)           00040001
*/STEPLIB DD DSN=GSFSOFT.XFASM.LOAD,DISP=SHR                            00060001
*********************************************************************** 00080000
*                                                                     * 00090000
* MACRO NAME = STRING                                                 * 00100000
*                                                                     * 00110000
* DESCRIPTIVE NAME = STRING Macro Instruction for Assembler XF        * 00120001
*                                                                     * 00130000
* FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       * 00140000
*            or STRING (of COBOL) to assembler programs.              * 00150000
*                                                                     * 00160000
* STATUS = R101                                                       * 00170001
*                                                                     * 00180000
* AUTHOR = Gilbert Saint-Flour <carlos@gsf-soft.com>                  * 00190000
*                                                                     * 00200000
* ENVIRONMENT = SEE BELOW                                             * 00210000
*                                                                     * 00220000
*    AMODE  = ANY                                                     * 00230001
*    RMODE  = ANY                                                     * 00240001
*     SCP   = S/360 OS, OS/VS, MVS/370                                * 00250001
* Processor = Assembler XF, Assembler H, High-Level Assembler         * 00260001
*     KEY   = ANY                                                     * 00270000
*     MODE  = ANY                                                     * 00280000
*     APF   = ANY                                                     * 00290001
*                                                                     * 00300000
* OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               * 00310000
*                                                                     * 00320000
* INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              * 00330000
*                                                                     * 00340000
* NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   * 00350000
*                                                                     * 00360000
* CHANGE ACTIVITY                                                     * 00370000
*                                                                     * 00380000
* $101 ASM XF version of STRING R514                                  * 00390001
*                   \  - Backport of (%TIME,LENGTH) syntax   / @GSFR518 00391002
* Juergen Winkelmann \   support from STRING version 518     \ @JW14278 00392002
* 2014/10/05         / - Date format DD.MM.YYYY support        @JW14278 00393002
*                   /  - Circumvention of not PoP compliant    @JW-Z390 00394002
*                  /     MVZ instruction emulation in z390     @JW-Z390 00395002
*********************************************************************** 00400000
         MACRO                                                          00410000
&NAME    STRING &INTO=,&PRINT=NOGEN                                     00420000
         GBLA  &$$LIT                                                   00430001
         GBLB  &$$FEAT(16)             FEATURES                         00440000
.*                                       1 LITERALS                     00450000
.*                                       2 REGISTER (BIN)               00460000
.*                                       3 REGISTER (HEX)               00470000
.*                                       4 PACKED                       00480000
.*                                       5 JDATE                        00490000
.*                                       6 BINARY                       00500000
.*                                       7 HEX                          00510000
.*                                       8 NUMERIC                      00520000
.*                                       9 LEFT JUST (NUMERIC)          00530000
.*                                       10 LEADING ZEROES              00540000
.*                                       11 TRUNCATE (CHAR STRING)      00550000
.*                                       12 %TIME                       00560000
         GBLC  &MACVERS                                                 00570001
         GBLC  &$$LITS(9999)           LITERALS                         00580000
         LCLA  &I,&J,&L,&N,&FLAG,&LEN2,&BLANKS                          00590001
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC,&NUMERIC 00600001
         LCLC  &LABEL,&LQ,&STR,&TO1,&TO2,&P1S,&P2C,&P2L,&P3C,&P3L       00610001
         LCLC  &LIT,&ALLFEAT                                            00620001
&MACVERS SETC '101'                    current version                  00630001
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN                              00640000
         PUSH  PRINT                                                    00650000
         PRINT GEN                                                      00660000
.NOGEN   ANOP                                                           00670001
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X00680000
               AND '&SYSLIST(1)' EQ 'GENERATE').GENL                    00690001
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS            00700000
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE                 00710000
&STR     SETC  ' R&MACVERS XF '                                         00720001
&NAME    L     R15,=A(@STR002)&STR     Routine Address                  00730001
         BALR  R14,R15                 CALL @STRING Routine             00740001
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR    00750000
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR        00760000
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)                00770000
         DC    AL2((&LABEL.P-@STRING)/2) OFFSET TO FIELD DESCRIPTORS    00780001
@STRING  CSECT                         NON-ADDRESSABLE CONSTANTS        00790001
&TO1     SETC  '&INTO(1)'                                               00800000
&TO2     SETC  '&LQ&INTO'                                               00810000
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX        00820000
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)   00830000
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)  00840000
.PUNTO3  ANOP                                                           00850000
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)   00860000
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)   00870000
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1)) 00880000
.PUNTO8  ANOP                                                           00890000
&LABEL.P DC    S(&TO1,&TO2)                                             00900000
.*--------------------------------------------------------------------* 00910000
.*-------      FIELDS       ------------------------------------------* 00920000
.*--------------------------------------------------------------------* 00930000
&I       SETA  1                                                        00940000
.*LOOP                                                                  00950000
.LOOP1   ANOP                                                           00960000
         ACTR  200                            SYSDEBUG/DIAG055          00970000
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS     00980000
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP                   00990000
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00                      01000000
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01         01010000
.*--------------------------------------------------------------------* 01020000
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         * 01030000
.*--------------------------------------------------------------------* 01040000
&P1S     SETC  '&SYSLIST(&I,1)'                                         01050000
&P2L     SETC  '0'                     INPUT LENGTH                     01060000
&P3L     SETC  '0'                     OUTPUT LENGTH                    01070000
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES               01080000
         AIF   ('&SYSLIST(&I,1)' EQ '%TIME').FLD190 %TIME      @GSFR518 01090002
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)               01100000
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250                         01110000
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)             01120000
.FLD115  ANOP                                                           01130000
.*                                                                      01140000
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200                         01150000
.*                                                                      01160000
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)                           01170000
.*                                                                      01180000
&L       SETA  1                                                        01190000
.*--LOOP                                                                01200000
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133                             01210000
&L       SETA  &L+1                                                     01220000
         AIF   (&L LT K'&P1S).FLD131                                    01230000
.*--ENDLOOP                                                             01240000
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF             01250000
         AGO   .FLD134                                                  01260000
.FLD133  ANOP                                                           01270000
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE           01280000
.FLD134  ANOP                                                           01290000
.*                                                                      01300000
.*XF     AIF   (NOT D'&P2L).FLD140                                      01310001
&P2C     SETC  T'&P2L                                                   01320000
.*MNOTE *,'&P1 &P2C'                                                    01330000
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220 01340000
         AIF   ('&P2C' EQ 'G').FLD210  FL2                              01350000
.FLD140  ANOP                                                           01360000
.*                                                                      01370000
.*       EXTRACT PSATOLD FROM PSATOLD-PSA                               01380000
.*                                                                      01390000
&L       SETA  1                                                        01400000
.*--LOOP                                                                01410000
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143                             01420000
         AIF   ('&P2L'(&L,1) EQ '+').FLD143                             01430000
&L       SETA  &L+1                                                     01440000
         AIF   (&L LT K'&P2L).FLD141                                    01450000
.*--ENDLOOP                                                             01460000
&P2L     SETC  '&LQ&P2L'               L'ABCDEF                         01470000
         AGO   .FLD300                                                  01480000
.FLD143  ANOP                                                           01490000
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA       01500000
         AGO   .FLD300                                                  01510000
.*                                                                      01520000
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800         01530000
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12                    01540000
         AIF   (&LAST).FLD186                                           01550000
         DC    X'60',AL1(&P2L)         BLANKS                           01560000
         AGO   .LIT90                                                   01570000
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS                           01580000
         AGO   .LIT90                                                   01590000
.*                                                                      01600000
.FLD190  AIF   (N'&SYSLIST(&I) GT 2).FLD995                    @GSFR518 01610002
&P1S     SETC  '1(14)'                 %TIME                            01620000
&$$FEAT(12) SETB 1                     %TIME                            01630000
&P2L     SETC  '12'                    hh:mm:ss.hh             @GSFR518 01631002
         AIF   (N'&SYSLIST(&I) EQ 1).FLD1905                   @GSFR518 01632002
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD1904               @GSFR518 01633002
&P2L     SETC  '&SYSLIST(&I,2)'        5,8,11,12               @GSFR518 01634002
&L       SETA  &SYSLIST(&I,2)                                  @GSFR518 01635002
         AIF   (&L EQ 5 OR &L EQ 8 OR &L EQ 11 OR &L EQ 12).FLD1905 518 01636002
.FLD1904 MNOTE 8,'Second sub-parameter is invalid: &SYSLIST(&I)'    518 01637002
.FLD1905 ANOP                                                  @GSFR518 01638002
.*MNOTE 1,'FLD1905 &SYSLIST(&I) &P1S &P2L '                    @GSFR518 01639002
         AGO   .FLD800                                                  01640000
.*--------------------------------------------------------------------* 01650000
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    * 01660000
.*--------------------------------------------------------------------* 01670000
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED     01680000
&P2C     SETC  '&SYSLIST(&I,2)'                                         01690000
         AGO   .FLD220                                                  01700000
.*T'&P1=G                                                               01710000
.FLD210  ANOP                                                           01720000
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'                      01730000
&P2C     SETC  'FL&L'                  T'&P1 = 'G'                      01740000
.*                                                                      01750000
.FLD220  ANOP                                                           01760000
&P2L     SETC  '0&P2C'                 (R2) LENGTH                      01770000
         AIF   ('&P2C'(1,1) EQ '(').FLD300                              01780000
&P2L     SETC  '&P2C'                  3(R2) LENGTH                     01790000
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300                         01800000
&P2L     SETC  '0'                                                      01810000
&PACKED  SETB  ('&P2C' EQ 'P')                                          01820000
         AIF   (&PACKED).FLD290                                         01830000
&P2L     SETC  '1'                                                      01840000
         AIF   ('&P2C' EQ 'FL1').FLD240                                 01850000
&P2L     SETC  '3'                                                      01860000
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240                01870000
&P2L     SETC  '7'                                                      01880000
         AIF   ('&P2C' EQ 'FL3').FLD240                                 01890000
&P2L     SETC  '15'                                                     01900000
         AIF   ('&P2C' EQ 'F').FLD240                                   01910000
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD          01920000
         AGO   .FLD300                                                  01930000
.*                                                                      01940000
.FLD240  ANOP                          BINARY VARIABLE                  01950000
&BIN     SETB  1                                                        01960000
         AGO   .FLD300                                                  01970000
.*                                                                      01980000
.FLD250  ANOP                          REGISTER CONTENT                 01990000
&REG     SETB  1                                                        02000000
         AGO   .FLD300                                                  02010000
.*                                                                      02020000
.FLD290  ANOP                          PACKED                           02030000
&P2L     SETC  '1'                                                      02040000
.*--------------------------------------------------------------------* 02050000
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   * 02060000
.*--------------------------------------------------------------------* 02070000
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800                         02080000
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL                    02090000
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE                       02100000
         AIF   (&HEX OR &TRUNC).FLD800                                  02110000
.*                                                                      02120000
&P3C     SETC  '&SYSLIST(&I,3)'                                         02130000
&P3L     SETC  '248'                                                    02140000
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308              02150000
&P3L     SETC  '249'                                                    02160000
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308                02170000
&P3L     SETC  '250'                                                    02180000
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308                02190000
&P3L     SETC  '251'                                                    02200000
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308                02210000
&P3L     SETC  '252'                                                    02220000
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308                02230000
&P3L     SETC  '253'                                                    02240000
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308                02250000
&P3L     SETC  '254'                                           @JW14278 02253002
         AIF   ('&P3C' EQ 'DD.MM.YYYY' AND &PACKED).FLD308     @JW14278 02256002
&P3L     SETC  '0'                                                      02260000
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310                         02270000
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'       02280000
         AGO   .FLD310                                                  02290000
.FLD308  ANOP                                                           02300000
&$$FEAT(5) SETB 1                      JDATE                            02310000
         AGO   .FLD800                                                  02320000
.*--LOOP                                                                02330000
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT                02340000
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT                02350000
         AIF   ('&P3C'(1,1) NE 'L').FLD311                              02360000
&LEFT    SETB  1                                                        02370000
         AGO   .FLD318                                                  02380000
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312                              02390000
&ZERO    SETB  1                                                        02400000
         AGO   .FLD318                                                  02410000
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993       nn in RnnB is not num  02420000
         AIF   ('&P3C'(1,1) GT '9').FLD993       nn in RnnB is not num  02430000
&P3L     SETC  '&P3L'.'&P3C'(1,1)                                       02440000
.FLD318  ANOP                                                           02450000
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'                          02460000
&P3C     SETC  '&P3C '(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER        02470001
         AIF   (K'&P3C GT 0).FLD310                                     02480000
.*--ENDLOOP                                                             02490000
         AIF   (&P3L GT 16).FLD993               nn in RnnB is too big  02500000
.*--------------------------------------------------------------------* 02510000
.FLD800  ANOP                                                           02520000
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))                 02530000
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))                     02540000
&LEFT    SETB  (&LEFT AND &NUMERIC)                                     02550000
         AIF   (NOT &NUMERIC).FLD810                                    02560000
         AIF   (&LEFT OR '&P3L' NE '0').FLD810                          02570000
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))     02580000
         AIF   (&REG).FLD810                                            02590000
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH            02600000
         AIF   ('&P2C' EQ 'FL1').FLD810                                 02610000
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH            02620000
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810                02630000
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH            02640000
.FLD810  ANOP                                                           02650000
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1                           02660000
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L                       02670000
&$$FEAT(2) SETB (&$$FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)    02680000
&$$FEAT(3) SETB (&$$FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)    02690000
&$$FEAT(4) SETB (&$$FEAT(4) OR &PACKED)               PACKED            02700000
&$$FEAT(6) SETB (&$$FEAT(6) OR &BIN)                  BINARY            02710000
&$$FEAT(7) SETB (&$$FEAT(7) OR (&HEX AND NOT &REG))   HEX               02720000
&$$FEAT(8) SETB (&$$FEAT(8) OR &NUMERIC)              BIN,PACKED        02730000
&$$FEAT(9) SETB (&$$FEAT(9) OR (&LEFT AND &NUMERIC))                    02740000
&$$FEAT(10) SETB (&$$FEAT(10) OR &ZERO)                                 02750000
&$$FEAT(11) SETB (&$$FEAT(11) OR &TRUNC)                                02760000
&BIN     SETB  0                    RESET FLAGS                         02770000
&HEX     SETB  0                    RESET FLAGS                         02780000
&REG     SETB  0                    RESET FLAGS                         02790000
&PACKED  SETB  0                    RESET FLAGS                         02800000
&LEFT    SETB  0                    RESET FLAGS                         02810000
&ZERO    SETB  0                    RESET FLAGS                         02820000
&TRUNC   SETB  0                    RESET FLAGS                         02830000
         AIF   (&FLAG GE 10).FLD995                                     02840000
         AIF   (&LAST).FLD816                                           02850000
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)                      02860000
         AGO   .LIT99                                                   02870000
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)                02880000
         AGO   .LIT99                                                   02890000
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'                02900000
         AGO   .LIT99                                                   02910000
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''    02920000
         AGO   .LIT99                                                   02930000
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'      02940000
         AGO   .LIT99                                                   02950000
.*--------------------------------------------------------------------* 02960000
.*------------ LITERALS ----------------------------------------------* 02970000
.*--------------------------------------------------------------------* 02980000
.LIT00   ANOP                                                           02990000
&LIT     SETC  'C&SYSLIST(&I)'                                          03000000
         AGO   .LIT09                                                   03010000
.LIT01   ANOP                                                           03020000
&LIT     SETC  '&SYSLIST(&I)'                                           03030000
.LIT09   ANOP                              calculate length of literal  03040000
&J       SETA  3                                                        03050000
&L       SETA  0                                                        03060000
         ACTR  K'&LIT+K'&LIT+100                                        03070000
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X                              03080000
.*LOOP                                                                  03090000
.LIT11C AIF ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&'(1,1)).LIT12C 03100000
&J       SETA  &J+1                                                     03110000
.LIT12C  ANOP                                                           03120000
&J       SETA  &J+1                                                     03130000
&L       SETA  &L+1                                                     03140000
         AIF   (&J LT K'&LIT).LIT11C                                    03150000
.*ENDLOOP                                                               03160000
         AGO   .LIT15                                                   03170000
.*LOOP                                                                  03180000
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X                             03190000
&L       SETA  &L+1                                                     03200000
.LIT12X  ANOP                                                           03210000
&J       SETA  &J+1                                                     03220000
         AIF   (&J LT K'&LIT).LIT11X                                    03230000
.*ENDLOOP                                                               03240000
&L       SETA  (&L+1)/2                                                 03250000
.LIT15   ANOP                               generate in-line literal    03260000
         AIF   (&L GT 5).LIT40                                          03270000
         AIF   (&LAST).LIT16                                            03280000
         DC    X'4&L',&LIT                                              03290001
         AGO   .LIT90                                                   03300000
.LIT16   DC    X'C&L',&LIT,0S(0)                                        03310000
         AGO   .LIT90                                                   03320000
.LIT40   ANOP                                  check literal table      03330000
         AIF   (&$$LIT EQ 0).LIT50                                      03340000
&N       SETA  1                                                        03350000
         ACTR  &$$LIT*3+200                                             03360000
.LIT41   AIF   ('&LIT' EQ '&$$LITS(&N)').LIT80       LOOP               03370000
&N       SETA  &N+1                                  LOOP               03380000
         AIF   (&N LE &$$LIT).LIT41                  LOOP               03390000
.LIT50   ANOP                                                           03400000
&$$LIT   SETA  &$$LIT+1                                                 03410000
&$$LITS(&$$LIT) SETC '&LIT'                                             03420000
&N       SETA  &$$LIT                                                   03430000
.LIT80   ANOP                               generate remote literal     03440000
&N       SETA  &N+1000                                                  03450000
         AIF   (&LAST).LIT86                                            03460000
         DC    X'40',AL1(&L),AL2($LIT&N-*)                              03470001
         AGO   .LIT90                                                   03480000
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)                        03490001
.LIT90   ANOP                                                           03500000
&$$FEAT(1) SETB 1                      LITERAL                          03510000
.LIT99   ANOP                                                           03520000
.*--------------------------------------------------------------------* 03530000
&I       SETA  1+&I                              LOOP                   03540000
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP                   03550000
.*ENDLOOP                                                               03560000
&SYSECT  CSECT                                                          03570001
         AGO   .MEND                                                    03580000
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'          03590000
         AGO   .MEND                                                    03600000
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'                   03610000
         AGO   .MEND                                                    03620000
.********************************************************************** 03630000
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           * 03640000
.********************************************************************** 03650000
.GENL    ANOP                                                           03660000
&ALLFEAT SETC  '&$$FEAT(1)&$$FEAT(2)&$$FEAT(3)&$$FEAT(4)'               03670000
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(5)&$$FEAT(7)&$$FEAT(6)&$$FEAT(8)'       03680000
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(9)&$$FEAT(10)&$$FEAT(11)&$$FEAT(12)'    03690000
&BLANKS  SETA  2                                                        03700001
         MNOTE *,'STRING R&MACVERS - FEATURES GENERATED: &ALLFEAT'      03710001
@STRING  CSECT                                                          03720001
         AIF   (&$$LIT EQ 0).GENL3                                      03730000
.GENL2   ANOP                                LOOP                       03740000
&N       SETA  &N+1                          LOOP                       03750000
&I       SETA  &N+1000                       LOOP                       03760000
$LIT&I   DC    &$$LITS(&N)                                              03770000
         AIF   (&N LT &$$LIT).GENL2          LOOP                       03780000
.GENL3   DC    0H'0'                                                    03790001
.********************************************************************** 03800000
.*                                                                    * 03810000
.*       STRING SUB-ROUTINE                                           * 03820000
.*                                                                    * 03830000
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         * 03840000
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         * 03850000
.*                                                                    * 03860000
.********************************************************************** 03870000
@00      EQU   0                       WORK REGISTER                    03880000
@01      EQU   1                       WORK REGISTER                    03890000
@02      EQU   2                       WORK REGISTER                    03900000
@03      EQU   3                       WORK REGISTER                    03910000
@04      EQU   4                       WORK REGISTER                    03920000
@05      EQU   5                       WORK REGISTER                    03930000
@06      EQU   6                       WORK REGISTER                    03940000
@13      EQU   13                      CALLER'S SAVE AREA               03950000
@14      EQU   14                      WORK REGISTER                    03960000
@15      EQU   15                      BASE REG                         03970000
         USING @STR002,@15                                              03980001
         USING @STRSAVE,@13                                             03990000
@STR002  B     @STR011                 BRANCH AROUND EYE-CATCHER        04000001
         DC    AL1(@STR003-*),C'@STRING/XF R&MACVERS &ALLFEAT',0H'0'    04010001
@STR003  DC    Y(@STR002-@STRING)      Offset to @STRING                04020001
@STR011  STM   @14,@06,12(@13)         Save caller's registers          04030001
         SLR   @06,@06                 R6=0                             04040000
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET     04050000
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET         04060000
         ALR   @06,@15                 R6 NOW POINTS TO PARM LIST       04070001
         SH    @06,@STR003             R6 NOW POINTS TO PARM LIST       04080001
         USING @STRSCON,@06                                             04090000
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)        04100000
         BAL   @14,@STRS2A             GET ADDRESS IN R2                04120000
         LA    @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD     04130000
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO     04140000
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD 04150000
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)       04160000
         BAL   @14,@STRS2A             GET LENGTH IN R2                 04170000
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD      04180000
         CR    @05,@04                 END ADDRESS?                     04190000
         BL    @STR282                 NO, JUMP                         04200000
         SR    @05,@04                 CALCULATE LENGTH                 04210000
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER           04220000
.********************************************************************** 04230000
.*       MOVE FIELDS TO OUTPUT AREA                                   * 04240000
.********************************************************************** 04250000
         LA    @06,@STRNEXT            POINT TO 1ST FIELD DESC          04260000
         USING @STRPARM,@06                                             04270000
.*LOOP                                                                  04280000
@STR310  EQU   *                                                        04290000
         AIF   (NOT &$$FEAT(1)).FEAT1A                                  04300000
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?               04310000
         BO    @STR372                 YES, JUMP                        04320000
.FEAT1A  ANOP                                                           04330000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23A              04340000
         TM    @STRFLAG,@STRREG        REGISTER?                        04350000
         BO    @STR323                 YES, JUMP                        04360000
.FEAT23A ANOP                                                           04370000
         AIF   (NOT &$$FEAT(12)).FEAT12A                                04380000
         CLI   @STRSCON,X'E0'          IS IT %TIME ?                    04390000
         BE    @STR378                 YES, JUMP                        04400000
.FEAT12A ANOP                                                           04410000
.*                                                                      04420000
.*       IT'S A FIELD (SCON)                                            04430000
.*                                                                      04440000
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)       04450000
         BAL   @14,@STRS2A             GET LENGTH IN R2                 04460000
         AIF   (&$$FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)    04470000
         LTR   @03,@02                 KEEP/TEST LENGTH                 04480000
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT         04490000
         AGO   .FEAT9E2                                                 04500000
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH                 04510000
         BP    @STR313                 LENGTH POSITIVE, JUMP            04520000
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?              04530000
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK        04540000
@STR313  EQU   *                                                        04550000
.FEAT9E2 ANOP                                                           04560000
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)        04570000
         BAL   @14,@STRS2A             GET ADDRESS IN R2                04580000
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO     04590000
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD 04600000
         CR    @03,@02                 END ADDRESS?                     04610000
         BL    @STR314                 NO, JUMP                         04620000
         LA    @00,X'0080'             PSA ADDRESS                      04630000
         CLR   @02,@00                 PSA REFERENCE?                   04640000
         BL    @STR314                 YES, JUMP                        04650000
         SR    @03,@02                 CALCULATE LENGTH                 04660000
@STR314  EQU   *                                                        04670000
         AIF   (NOT &$$FEAT(6)).FEAT6A                                  04680000
         TM    @STRFLAG,@STRBIN        BINARY FIELD?                    04690000
         BO    @STR328                 YES, JUMP                        04700000
.FEAT6A  ANOP                                                           04710000
         AIF   (NOT &$$FEAT(4)).FEAT4A                                  04720000
         TM    @STRFLAG,@STRPACK       PACKED FIELD?                    04730000
         BO    @STR351                 YES, JUMP                        04740000
.FEAT4A  ANOP                                                           04750000
         AIF   (NOT &$$FEAT(7)).FEAT7A                                  04760000
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?                     04770000
         BO    @STR376                 YES, JUMP                        04780000
.FEAT7A  ANOP                                                           04790000
.*                                                                      04800000
.*       TRUNCATE CHARACTER STRING                                      04810000
.*                                                                      04820000
         AIF   (NOT &$$FEAT(11)).FEAT11A                                04830000
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN  04840000
         BNE   @STR390                 NO STRING TRUNCATION, JUMP       04850000
         LA    @01,0(@03,@02)          FIRST BYTE AFTER FIELD           04860000
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP 04870000
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP 04880000
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP 04890000
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP 04900000
         B     @STR398                 BLANK FIELD, DO NOT EDIT         04910000
         AGO   .FEAT11B                                                 04920000
.FEAT11A ANOP                                                           04930000
         AIF (&$$FEAT(2)+&$$FEAT(3)+&$$FEAT(4)+&$$FEAT(6) EQ 0).FEAT11B 04940000
         B     @STR390                 EDIT                             04950000
.FEAT11B ANOP                                                           04960000
.*                                                                      04970000
.*       REGISTER (R0-R13)                                              04980000
.*                                                                      04990000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23B              05000000
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT              05010000
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31         05020000
         EX    @01,@STR323L            COPY R7-R13 INTO R0              05030000
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?                  05040000
         BNL   @STR323T                YES, JUMP                        05050000
         SLL   @01,2                   R1= 000000BB BASE * 4            05060000
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6          05070000
@STR323T EQU   *                                                        05080000
         AIF   (NOT &$$FEAT(3)).FEAT3R REG,HEX                          05090000
         AIF   (NOT &$$FEAT(2)).FEAT2H REG,BIN                          05100000
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?                     05110000
         BNO   @STR330                 NO, EDIT FWD                     05120000
.FEAT2H  ANOP                                                           05130000
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK            05140000
         LA    @03,8                   OUTPUT LENGTH                    05150000
         B     @STR376X                EDIT IN HEX                      05160000
.FEAT3R  ANOP                                                           05170000
         AIF   (NOT &$$FEAT(6)).FEAT23B                                 05180000
         B     @STR330                 EDIT R0                          05190000
.FEAT23B ANOP                                                           05200000
.*                                                                      05210000
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)           05220000
.*                                                                      05230000
         AIF   (NOT &$$FEAT(6)).FEAT6B                                  05240000
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**         05250000
@STR328  SLR   @00,@00                                                  05260000
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE         05270000
.FEAT6B  ANOP                                                           05280000
.*                                                                      05290000
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(6)).FEAT6C               05300000
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL         05310000
         AIF   (NOT &$$FEAT(4)).FEAT6C                                  05320000
         B     @STR361                 EDIT DWD                         05330000
.FEAT6C  ANOP                                                           05340000
.*                                                                      05350000
.*       PACKED FIELD                                                   05360000
.*                                                                      05370000
         AIF   (NOT &$$FEAT(4)).FEAT4B                                  05380000
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD                  05390000
@STR351  LA    @03,0(,@02)             FIRST BYTE OF PACKED FIELD       05400000
         BALR  @14,0                                                    05410000
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?           05420000
         LA    @03,1(,@03)              (NEXT BYTE)                     05430000
         BNOR  @14                     NO, LOOP MORE                    05440000
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD       05450000
         BCTR  @03,0                                                    05460000
         EX    @03,@STRZAP             EXECUTE ZAP                      05470000
         AIF   (NOT &$$FEAT(5)).FEAT4B                                  05480000
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?           05490000
         BNL   @STR375                 YES, JUMP                        05500000
.FEAT4B  ANOP                          PACKED                           05510000
.*                                                                      05520000
.*       EDIT @STRDWD (BIN, REG, PACKED)                                05530000
.*                                                                      05540000
         AIF   (NOT &$$FEAT(8)).FEAT8B                                  05550000
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH                    05560000
         LA    @03,X'003F'             MASK FOR "AND"                   05570000
         NR    @03,@00                 OUTPUT LENGTH                    05580000
         MVC   @STRWK16(16),@STRMASK   EDIT MASK                        05590000
.*                                                                      05600000
.*       LEFT-JUSTIFICATION (NUMERIC)                                   05610000
.*                                                                      05620000
         AIF   (NOT &$$FEAT(9)).FEAT9B                                  05630000
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?              05640000
         BNO   @STR367                 NO, JUMP                         05650000
         LA    @01,@STRWK16+15         PREVENT BAD R1                   05660000
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL                    05670000
         LA    @02,0(,@01)             FIRST STRING POSITION            05680000
         LTR   @03,@03                 CHECK OUTPUT LENGTH              05690000
         BNZ   @STR363                 JUMP IF NOT ZERO                 05700000
.*       L0    (LEFT JUSTIFIED, NO PADDING)                             05710000
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING      05720000
         SR    @03,@02                 COMPUTE STRING LENGTH            05730000
         B     @STR390                 MOVE FIELD TO OUTPUT LINE        05740000
.*       L1-L63 (LEFT JUSTIFIED, PADDING)                               05750000
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN  05760000
         BNH   @STR364                 LARGE ENOUGH, JUMP               05770000
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.   05780000
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH         05790000
         LR    @00,@04                 POINTER IN OUTPUT LINE           05800000
         LR    @01,@03                 LENGTH WITH PADDING              05810000
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING      05820000
         SR    @03,@02                 COMPUTE STRING LENGTH            05830000
         B     @STR392                 MOVE FIELD TO OUTPUT LINE        05840000
@STR367  EQU   *                                                        05850000
.FEAT9B  ANOP                                                           05860000
         AIF   (NOT &$$FEAT(10)).FEAT10B                                05870000
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?            05880000
         BNO   @STR368                 NO, JUMP                         05890000
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'        05900000
@STR368  EQU   *                                                        05910000
.FEAT10B ANOP                                                           05920000
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL                    05930000
         LA    @02,@STRWK16+16         FIRST POSITION AFTER STRING      05940000
         SR    @02,@03                 FIRST STRING POSITION            05950000
.FEAT8B  ANOP                                                           05970000
.*                                                                      05980000
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)                          05990000
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)   06000000
.*       BLANKS  (@STRSCON=ZERO)                                        06010000
.*                                                                      06020000
         AIF   (NOT &$$FEAT(1)).FEAT1B                                  06030000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06040000
@STR372  LA    @01,7                   mask for NR                      06050000
         LA    @02,@STRLEN2            1st byte of short literal        06060000
         IC    @03,@STRFLAG            pick up flags+length             06070000
         NR    @03,@01                 R3 has length of short literal   06080000
         BNZ   @STR390                 short literal, go move it        06090000
         SLR   @02,@02                 Clear Address Register           06100000
         IC    @03,@STRLEN2            GET LITERAL LENGTH               06110000
         TM    @STRFLAG,@STRX40        string of spaces?                06120000
         BO    @STR390                 yes, go move them                06130000
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET              06140000
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS   06150000
.FEAT1B  ANOP                                                           06160000
.*                                                                      06170000
.*       CONVERT JULIAN DATE TO YYMMDD                                  06180000
.*                                                                      06190000
         AIF   (NOT &$$FEAT(5)).FEAT5F                                  06200000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06210000
@STR375  LA    @00,248                 MASK FOR 'SLR'                   06220000
         SLR   @01,@01                                                  06230000
         IC    @01,@STRLEN2            248-255                          06240000
         SLR   @01,@00                 000-007                          06250000
         LA    @00,12                  L'@STR375W                       06260000
         MR    @00,@00                 COMPUTE OFFSET                   06270000
         LA    @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE         06280000
         SLR   @03,@03                                                  06290000
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)      06300000
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT 06310000
         BNZ   @STR375B                NO, JUMP                    @JDT 06320000
@STR375Z LA    @02,@BLANKS             WORK AREA                        06330001
&BLANKS  SETA  10                      WE NEED AT LEAST 10 BLANKS       06340001
         B     @STR390                 MOVE FIELD TO OUTPUT LINE        06350000
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248        06360000
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249        06370000
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250        06380000
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251        06390000
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252        06400000
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253        06410000
         DC    AL1(10,C'.',6,7,8,4,5,8,0,1,2,3) DD.MM.YYYY 254 @JW14278 06420002
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255        06430000
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'                06440000
         DC    P'999'                  Prevent S0C7 with 90366          06450000
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?              06460000
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)   06470000
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20           06480000
         BE    @STR375C                CC=01, USE CC=20                 06490000
         CLI   @STRDWD+4+1,X'50'       YY<50?                           06500000
         BL    @STR375C                YES, USE CC=20                   06510000
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19                  06520000
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?                           06530000
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER            06540000
         LA    @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS) 06550000
         TM    @STRDWD+4+1,X'01'       ODD YEARS                        06560000
         BO    @STR375N                  AREN'T LEAP YEARS              06570000
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992 06580000
         BNM   @STR375L                MIXED IN 1982/1990               06590000
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD          06600000
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?    06610000
         BNH   @STR375L                NO, JUMP                         06620000
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD      06630000
.*--LOOP WHILE DDD > 0                                                  06640000
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH    06650000
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE     06660000
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD       06670000
         BP    @STR375L                                                 06680000
.*--ENDLOOP                                                             06690000
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION       06700000
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??               06710000
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD             06720000
         MVZ   @STRWK16+1(5),@STRWK16  temporary circumvention @JW-Z390 06730002
         MVZ   @STRWK16+6(2),@STRWK16  of MVZ bug under z390   @JW-Z390 06733002
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR                        06740000
         LA    @02,@STRWK16+9          WORK AREA                        06750000
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK          06760000
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT 06770000
.FEAT5F  ANOP                          JDATE                            06780000
.*                                                                      06790000
.*       HEX STRING                                                     06800000
.*                                                                      06810000
         AIF   (NOT &$$FEAT(7)).FEAT7B                                  06820000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06830000
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK             06840000
@STR376  LA    @00,8                   MAX LENGTH                       06850000
         CLR   @03,@00                 CHECK LENGTH                     06860000
         BNH   @STR376B                JUMP IF LE 8                     06870000
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH       06880000
@STR376B LR    @01,@03                 INPUT LENGTH                     06890000
         BCTR  @01,0                                                    06900000
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE        06910000
         ALR   @03,@03                 OUTPUT LENGTH                    06920000
         AGO   .FEAT37B                                                 06930000
.FEAT7B  ANOP                                                           06940000
         AIF   (NOT &$$FEAT(3)).FEAT37C                                 06950000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       06960000
.FEAT37B ANOP                                                           06970000
@STR376X LA    @02,@STRWK16            WORK AREA                        06980000
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"     06990000
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"     07000000
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'             07010000
.FEAT37C ANOP                                                           07020000
.*                                                                      07030000
.*       %TIME                                                          07040000
.*                                                                      07050000
         AIF   (NOT &$$FEAT(12)).FEAT12B                                07060000
         B     @STR390                 MOVE STRING TO OUTPUT LINE       07070000
@STRTIME DC    X'4021207A20207A20204B20204000'    0X:XX:XX.XX  @GSFR518 07080002
@STR378  LR    @02,@15                 SAVE BASE REG                    07090000
         TIME  DEC                     GET HHMMSSHH                     07100000
         LR    @15,@02                 RESTORE BASE REG                 07110000
         ST    @00,@STRDWD             STORE HHMMSSHH                   07120000
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK                   07130000
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS.HH        @GSFR518 07140002
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON    @JW14278 07145002
         BAL   @14,@STRS2A             GET LENGTH IN R2        @JW14278 07150002
         LR    @03,@02                 GET LENGTH IN R3        @JW14278 07155002
         LA    @02,@STRWK16+1          WORK AREA               @JW14278 07160002
.FEAT12B ANOP                                                           07170000
.*MOVE                                                                  07180000
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN  07190000
         BNH   @STR391                 LARGE ENOUGH, JUMP               07200000
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.   07210000
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH         07220000
         LR    @00,@04                 POINTER IN OUTPUT LINE           07230000
         LR    @01,@03                 PASS REMAINING LENGTH            07240000
         LTR   @02,@02                 BLANKS?                          07250000
         BNZ   @STR392                 NO, JUMP                         07260000
         SLR   @03,@03                 YES, ZERO LENGTH                 07270000
@STR392  ICM   @03,B'1000',@BLANKS     PAD WITH BLANKS                  07280001
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE        07290000
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE       07300000
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR        07310000
         BO    @STR399                 Done, exit                       07320000
         AIF   (NOT &$$FEAT(1)).FEAT1C                                  07330000
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?              07340000
         BM    @STR398L                Literal, not spaces              07350000
         BZ    @STR398X                Neither literal nor spaces       07360000
         LA    @06,@STRSCON            2-byte entry for blank spaces    07370000
         B     @STR310                 PROCESS NEXT ENTRY               07380000
@STR398L LA    @01,7                   mask for NR                      07390000
         IC    @03,@STRFLAG            pick up flags+length             07400000
         NR    @03,@01                 R3 has length of short literal   07410000
         BZ    @STR398T                not an in-line literal, jump     07420000
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal 07430000
         B     @STR310                 PROCESS NEXT ENTRY               07440000
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal   07450000
         B     @STR310                 PROCESS NEXT ENTRY               07460000
.FEAT1C  ANOP                                                           07470000
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY            07480000
         B     @STR310                 PROCESS NEXT ENTRY               07490000
.*ENDLOOP                                                               07500000
.*                                                                      07510000
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS                       07520000
.*                                                                      07530000
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING            07540000
         ICM   @01,B'1000',@BLANKS     SET UP R1 FOR PADDING            07550001
.***     DROP  @06,@13,@15                                              07560000
         LA    @14,2                   INCREMENT                        07570000
         AL    @14,12(,@13)            RETURN ADDRESS                   07580000
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD  07590000
         SL    @15,8(,@13)             CALCULATE LENGTH USED            07600000
         MVCL  @04,@00                 PAD WITH BLANKS                  07610000
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS           07620000
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL       07630000
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN    07640000
         BR    @14                     RETURN TO CALLER                 07650000
.*                                                                      07660000
         AIF   (NOT (&$$FEAT(3) OR &$$FEAT(7))).FEAT37T                 07670000
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION         07680000
.FEAT37T ANOP                                                           07690000
         AIF   (NOT &$$FEAT(8)).FEAT8T                                  07700000
@STRMASK DC    X'4020202020202020,2020202020202120'                     07710000
.FEAT8T  ANOP                                                           07720000
.********************************************************************** 07730000
.*       Convert S-con to address                                     * 07740000
.*             Input: GPR2 points to an S-CON in the remote parm list * 07750000
.*             Output: GPR2 contains the address                      * 07760000
.********************************************************************** 07770000
@STRS2A  SLR   @00,@00                                                  07780000
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD                    07790000
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....      07810000
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)     07820000
         CLI   0(@02),@06*16+15        R7-R13?                          07830000
         BH    @STRS2A3                YES, JUMP                        07840000
.*BASE REG IS R0-R6                                                     07850000
         LTR   @02,@00                 IS R0 THE BASE REG?              07860000
         BNZ   @STRS2A2                NO, JUMP                         07870000
         LTR   @02,@01                 IS THIS A PSA ADDRESS?           07880000
         BNZR  @14                     YES, GOBACK                      07890000
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4            07900000
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE           07910000
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL      07920000
         BR    @14                                                      07930000
.*BASE REG IS R7-R13                                                    07940000
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)   07950000
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL      07960000
         BR    @14                                                      07970000
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL      07980000
@BLANKS  DC    CL((((*+&BLANKS+7-@STRING)/8)*8)-(*-@STRING))' '         07990001
@STRSIZE EQU   *-@STRING               SIZE OF GENERATED CSECT          08000001
         DROP  @06,@13,@15                                              08010000
.********************************************************************** 08020000
.*       WORK AREA (CALLER'S SAVE AREA)                               * 08030000
.********************************************************************** 08040000
@STRSAVE DSECT                         24-BYTE WORK AREA                08050000
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)               08060000
@STRWK16 DS    F'7,8,9,10'             WORK AREA                        08070000
@STRDWD  DS    D'1112'                 WORK AREA                        08080000
@STRPARM DSECT                                                          08090000
@STRFLAG DS    B                   +0  FORMAT, FLAGS                    08100000
@STRLAST EQU   X'80'                     LAST ENTRY                     08110000
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET 08120000
@STRX40  EQU   X'20'                   String of Spaces                 08130000
.*             X'0F'                   CONVERSION REQUIRED              08140000
.*                                     or length of short literal       08150000
@STRHEX  EQU   X'08'                     HEXADECIMAL                    08160000
@STRBIN  EQU   X'04'                     BINARY                         08170000
@STRPACK EQU   X'02'                     PACKED                         08180000
@STRREG  EQU   X'01'                     REGISTER                       08190000
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH            08200000
.*                                     or start of short literal        08210000
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION             08220000
@STRZERO EQU   X'40'                     LEADING ZEROES                 08230000
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.  08240000
@STRSCON DS    S                   +2  FIELD ADDRESS                    08250000
@STRFLEN DS    S                   +4  FIELD LENGTH                     08260000
@STRNEXT EQU   *                   +6                                   08270000
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99                             08280000
         POP   PRINT                                                    08290000
.MEND99  MEND                                                           08300000
         EJECT                                                          08310000
**********************************************************************  08320000
**********************************************************************  08330000
********* TEST PROGRAM FOR THE 'STRING' MACRO ************************  08340000
**********************************************************************  08350000
**********************************************************************  08360000
         MACRO                                            JDATE MACRO   08370001
        @JDATE &DATE                                      JDATE MACRO   08380001
         LA    R1,=P'&DATE'                               JDATE MACRO   08390001
*** STRING ((R1),P),2X,((R1),P),INTO=XXX                  JDATE MACRO   08400001
         STRING ((R1),P),2X,((R1),P,YYMMDD),INTO=XXX,     JDATE MACRO  X08410001
               3X,((R1),P,YY/MM/DD),                      JDATE MACRO  X08420001
               3X,((R1),P,DD/MM/YY),                      JDATE MACRO  X08430001
               3X,((R1),P,MM/DD/YY),                      JDATE MACRO  X08440001
               3X,((R1),P,YYYYMMDD),                      JDATE MACRO  X08450001
               3X,((R1),P,YYYY-MM-DD)                     JDATE MACRO   08460001
         PUT   SYSPRINT,XXX                               JDATE MACRO   08470001
         MEND                                             JDATE MACRO   08480001
**********************************************************************  08490001
         LCLC  &SYSVER,&SYSDATC                                         08500001
&SYSVER  SETC  'XF'                                                     08510001
&SYSDATC SETC  '20'.'&SYSDATE'(7,2)'&SYSDATE'(1,2)'&SYSDATE'(4,2)       08520000
TESTPGM  START X'015000'                                                08530001
         BALR  R12,0                                                    08540000
         USING *,R12                                                    08550000
*LOAD EP=SYSDEBUG                                                       08560001
*LR R15,R0                                                              08570001
*BASSM R14,R15                                                          08580001
*STRING 1X,INTO=XXX                                                     08590000
*RC8     STRING ((R1),,R**B),((R1),,R22Z),((R1),,R16B),INTO=XXX         08600000
         OPEN  (SYSPRINT,OUTPUT)                                        08610000
         STRING 'Assembler is &SYSVER, DATE is &SYSDATC',INTO=XXX       08620000
         PUT   SYSPRINT,XXX                                             08630000
         STRING 'AMPERSAND=&& AND APOSTROPHE='' ',INTO=XXX              08640000
         PUT   SYSPRINT,XXX                                             08650000
RBPREFIX EQU   *                                                        08660000
RBINTCOD EQU   *+6,2,C'H'                                               08670000
         L     R1,PSATOLD-PSA(0,0)                                      08680000
         L     R1,0(,R1)               TCBRBP                           08690000
         L     R2,PSAAOLD-PSA(0,0)     ASCB                             08700000
         STRING 'SVC',(RBINTCOD-RBPREFIX(R1),H,R3Z),                   X08710000
               1X,(WWWW,,T),' - ',     VV.MM OF SVC RTNE               X08720000
               ((R8),,X),1X,           COM-REG ADDR                    X08730000
               'ASID=',(ASCBASID-ASCB(R2),,X),1X,                      X08740001
               PARM1,1X,               MAIN PGM NAME                   X08750000
               INTO=XXX                                                 08760000
         PUT   SYSPRINT,XXX                                             08770000
*                                                                       08780000
         LA    R2,XXX                                                   08790000
         STRING 1X,INTO=((R2),8)                                        08800000
        @JDATE 90058                                                    08810000
        @JDATE 91059                                                    08820000
        @JDATE 93060                                                    08830000
        @JDATE 94365                                                    08840000
        @JDATE 80058                                                    08850000
        @JDATE 84059                                                    08860000
        @JDATE 88060                                                    08870000
        @JDATE 92061                                                    08880000
        @JDATE 00366                                                    08890000
         LA    R2,1234                                                  08900000
         STRING 'CVTPTR=X''',(CVTPTR,4,X),'''',INTO=XXX,               X08910000
               ' 1234=',((R2),,R4Z)                                     08920000
         PUT   SYSPRINT,XXX                                             08930000
         L     R1,CVTPTR(0,0)                                           08940000
         STRING 'CVTDATE=',(56(R1),P,YYMMDD),INTO=XXX                   08950000
         PUT   SYSPRINT,XXX                                             08960000
         LA    R0,1000                                                  08970000
         LA    R3,0033                                                  08980000
         STRING 'D1=/',D1,'/,WWWW=/',WWWW,'/',                         X08990000
               ((R3),,L),'/',((R3),,X),'/',((R0),,L),'/',              X09000000
               ((R3),,R9B),'/',INTO=XXX                                 09010000
         LR    R4,R15                   LENGTH USED                     09020000
         PUT   SYSPRINT,XXX                                             09030000
         STRING WWWW,                                                  X09040000
               (4(R13),4,X),'''',(4(R13),F),'''',                      X09050000
               (4(R13),F,L),'''',                                      X09060000
               (4(R13),F,L11),'''',                                    X09070000
               (4(R13),F,Z9),'''',                                     X09080000
               8X,'R4=',((R4),,L),      LENGTH USED                    X09090000
               INTO=XXX                                                 09100000
         PUT   SYSPRINT,XXX                                             09110000
         STRING %TIME,D1,'B12345678B',5X,(CTR1,P),1X,PARM1,1X,PARM2,   X09120000
               INTO=XXX                                                 09130000
         PUT   SYSPRINT,XXX                                             09140000
         LA    R3,17                                                    09150001
         STRING INTO=XXX,'CCC1234A',(D1,(R3)),'.',(CTR1,P,R7Z)          09160000
         PUT   SYSPRINT,XXX                                             09170000
         STRING C'DDN2(',(D1,,T),')',X'40C1C2,C3C4',                   +09180000
               ' PSATOLD=',(PSATOLD-PSA,,X),                           +09190000
               INTO=XXX                                                 09200000
         PUT   SYSPRINT,XXX                                             09210000
         BALR  R0,0                                                     09220000
         STRING 'R0=',((R0),,X),'   16(R0)=',(16(R0),4,X),INTO=XXX      09230000
         PUT   SYSPRINT,XXX                                             09240000
         STRING 'R0=',((R0),,X),'   CTR1=',(CTR1,P,R5B),INTO=XXX        09250000
         PUT   SYSPRINT,XXX                                             09260000
         SLR   R0,R0                                                    09270000
         STRING 'R0=',((R0),,X),'   CTR1=',(CTR1,P,R5B),INTO=XXX        09280000
         PUT   SYSPRINT,XXX                                             09290000
*                                                                       09300000
EXIT     SLR   R15,R15                                                  09310000
         SVC   3                       GOBACK                           09320000
D1       DC    C'D1-----D1    '                                         09330000
WWWW     DC    C'WWWW'                                                  09340000
CTR1     DC    P'1'                                                     09350000
PARM1    DC    C'<-PARM1->'                                             09360000
PARM2    DC    C'<-PARM2->'                                             09370000
XXX      DS    CL132                                                    09380000
CVTPTR   EQU   0016,4,C'A'                                              09390000
SYSPRINT DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=PM,RECFM=FB,LRECL=121     09400000
         STRING GENERATE                                                09410000
R0       EQU   0                                                        09420001
R1       EQU   1                                                        09430001
R2       EQU   2                                                        09440001
R3       EQU   3                                                        09450001
R4       EQU   4                                                        09460001
R5       EQU   5                                                        09470001
R6       EQU   6                                                        09480001
R7       EQU   7                                                        09490001
R8       EQU   8                                                        09500001
R9       EQU   9                                                        09510001
R10      EQU   10                                                       09520001
R11      EQU   11                                                       09530001
R12      EQU   12                                                       09540001
R13      EQU   13                                                       09550001
R14      EQU   14                                                       09560001
R15      EQU   15                                                       09570001
PSA      DSECT                                                          09580000
PSATOLD  EQU   *+X'21C',4,C'A'                                          09590000
PSAAOLD  EQU   *+X'224',4,C'A'                                          09600000
ASCB     DSECT                                                          09610000
ASCBASID EQU   *+36,2,C'X'                                              09620000
         END                                                            09630000
//SYSPRINT DD SYSOUT=*                                                  09640000
//SYSLIB   DD DSN=SYS1.MACLIB,DISP=SHR                                  09670000
//SYSUT1   DD UNIT=VIO,SPACE=(CYL,2)                                    09680000
//SYSUT2   DD UNIT=VIO,SPACE=(CYL,2)                                    09690001
//SYSUT3   DD UNIT=VIO,SPACE=(CYL,2)                                    09700001
//SYSGO    DD UNIT=VIO,SPACE=(TRK,1),DISP=(,PASS),DCB=BLKSIZE=3120      09710001
//*                                                                     09720000
//GO      EXEC PGM=LOADER,PARM=PRINT,TIME=(,2)                          09730000
//SYSLIN   DD DSN=*.XFASM.SYSGO,DISP=(OLD,DELETE)                       09740001
//SYSLOUT  DD SYSOUT=*                                                  09750000
//SYSPRINT DD SYSOUT=*                                                  09760000
//ABNLTERM DD SYSOUT=*                                                  09770000
//ABNLIGNR DD DUMMY                                                     09780000
//SYSDEBUG DD SYSOUT=*                                                  09790000
//SYSUDUMP DD SYSOUT=*                                                  09800000
                                                                        09810000
          +----------------------------------------+                    09820000
          +                                        +                    09830000
          +  Documentation for the STRINGXF macro  +                    09840001
          +                                        +                    09850000
          +       Last update: 30 July 2006        +                    09860001
          +                                        +                    09870001
          +----------------------------------------+                    09880000
                                                                        09890000
  The STRING macro is functionally similar to the COBOL DISPLAY         09900000
  or PL/I PUT EDIT instructions.                                        09910000
                                                                        09920000
  Using STRING, the assembler programmer can concatenate                09930000
  any number of fields, edit each of them if necessary,                 09940000
  and receive the result in the specified work area.                    09950000
                                                                        09960000
  Formats:                                                              09970000
                                                                        09980000
    (1) label  STRING {field_specification1}                            09990000
                      {,field_specification2}...                        10000000
                      ,INTO=workarea|(workarea,length)                  10010000
                      {,PRINT=GEN|NOGEN}                                10020000
                                                                        10030000
    (2) label  STRING GENERATE                                          10040000
                      {,PRINT=GEN|NOGEN}                                10050000
                                                                        10060000
                                                                        10070000
  field_specification                                                   10080000
                                                                        10090000
    Each field to be printed is described as a positional               10100000
    operand.  Each operand specifies the field address, its             10110000
    length, and its formatting requirements.                            10120000
                                                                        10130000
    Four field description formats are supported:                       10140000
                                                                        10150000
      1.   symbol                                                       10160000
      2.   (symbol,length,format)                                       10170000
      3.   (d(r)|(r),length,format)                                     10180000
      4.   ((r),,format)                                                10190000
      5.   'character string'                                           10200000
                                                                        10210000
  Symbol specifies the field address.  It must be an                    10220000
    S-type (relocatable) address.                                       10230000
                                                                        10240000
  d(r) may be used to specify the field address in S/370                10250000
    base-displacement format.  If d is zero, it may be omitted.         10260000
    If d(r) or (r) is used, length must also be specified.  R14         10270000
    and R15 may not be used.  If d(0) is used, it is handled the        10280000
    way the assembler does, i.e. R0 as a base register is assumed       10290000
    to contain zero: 16(0) is equivalent to 16, CVTPTR or, X'10'.       10300000
                                                                        10310000
  ((r),,format) specifies that (r) contains the value                   10320000
    itself, not an address.  R14 and R15 may not be used.               10330000
                                                                        10340000
  'character string' specifies a literal enclosed in single quotes      10350000
    as specified in a DC instruction.  Hex strings or character         10360000
    strings are supported.  The following expressions are equivalent:   10370000
    'ABC' C'ABC' X'C1C2C3'                                              10380000
                                                                        10390000
  Length specifies the length and/or the type of the input field.       10400000
    It may be specified as an integer, a symbol, a register, or a       10410000
    constant.  When used with symbol, it overrides the assembled        10420000
    length and/or type.  Length is required if field is specified       10430000
    as d(r) or (r).  If a zero length is specified, the field is        10440000
    ignored.                                                            10450000
                                                                        10460000
        nn   field length in bytes                                      10470000
        H    half-word                                                  10480000
        F    full-word                                                  10490000
        FL1  1-byte binary integer                                      10500000
        FL3  3-byte binary integer                                      10510000
        P    packed field                                               10520000
        (r)  length of character string (R0 thru R12)                   10530000
        d(r) length of character string (R1 thru R12)                   10540000
                                                                        10550000
  Notes:  If the field address is specified as a symbol that            10560000
          has been defined previously in the program, the symbol        10570000
          type is known and there is no need to specify it.             10580000
                                                                        10590000
          If the length is specified as (r) or d(r) and the value       10600000
          is greater than the address itself, (r) or d(r) is            10610000
          considered to be the end address +1 instead of the length.    10620000
                                                                        10630000
          The length is not specified for packed fields.  The           10640000
          @STRING subroutine scans the field left-to-right until it     10650000
          finds a byte with a valid sign in the low-order 4 bits.       10660000
                                                                        10670000
          If symbol is an arithmetic expression and no length is coded, 10680000
          the implicit length will be that of the first symbol in the   10690000
          expression;  for example, if symbol is coded as PSATOLD-PSA,  10700000
          then the implicit length will be L'PSATOLD.                   10710000
                                                                        10720000
          All numeric items are assumed positive.                       10730000
                                                                        10740000
  format  optionally indicates editing options that must                10750000
          be applied to a field.                                        10760000
                                                                        10770000
      L        left justified                                           10780000
      R        right justified                                          10790000
      nn       output length                                            10800000
      0        adjust length                                            10810000
      Z        leading zeroes                                           10820000
      B        leading/trailing blanks                                  10830000
      T        truncate character string after last non-blank           10840000
      X        display in hexadecimal                                   10850000
      YYMMDD   convert julian date to YYMMDD                            10860000
      YY/MM/DD convert julian date to YY/MM/DD                          10870000
      DD/MM/YY convert julian date to DD/MM/YY                          10880000
      MM/DD/YY convert julian date to MM/DD/YY                          10890000
      YYYYMMDD convert julian date to YYYYMMDD                          10900000
      YYYY-MM-DD convert julian date to YYYY-MM-DD                      10910000
                                                                        10920000
    The default format depends on the field type:                       10930000
                                                                        10940000
        Type                   Default Format                           10950000
                                                                        10960000
        character string             L                                  10970000
        FL1                          R3B                                10980000
        H or FL2                     R5B                                10990000
        other numeric fields         R7B                                11000000
                                                                        11010000
    Note: L0 and T are equivalent for character strings.                11020000
                                                                        11030000
  'character string' is any character string enclosed in                11040000
  single quotes.  Blank spaces may be specified as nnX,                 11050000
  where nn is the number of X'40' bytes you want to be                  11060000
  inserted in the output line.  %TIME may be specified to               11070000
  obtain the current time in hh.mm.ss.hh format.                        11080000
                                                                        11090000
INTO=workarea|(workarea,length)                                         11100000
                                                                        11110000
  INTO indicates the address and length of the output work area         11120000
  into which the result of the concatenation should be placed           11130000
  (left justified).  If the work area is too small, truncation          11140000
  will occur.  If it is too large, it is padded with blanks.            11150000
                                                                        11160000
  The address may be a symbol, d(r) (S-type address) or (r).            11170000
                                                                        11180000
  The length may be specified as an integer, a symbol, a register,      11190000
  or a constant; it is required if the address is coded as d(r) or      11200000
  (r).  If length is not specified for a symbol-type address, the       11210000
  assembled length of the symbol is used.                               11220000
                                                                        11230000
  Upon return from STRING, R15 contains the length actually used        11240000
  in the output work area (before padding).                             11250000
                                                                        11260000
PRINT=GEN|NOGEN                                                         11270000
                                                                        11280000
  This operand allows you to temporarily override the PRINT             11290000
  specification (GEN or NOGEN).                                         11300000
                                                                        11310000
GENERATE (format 2)                                                     11320000
                                                                        11330000
  The GENERATE format must be specified once at the end of the          11340000
  program.  It generates the @STRING sub-routine as well as all         11350000
  the literals specified in previous invocations of the macro.          11360000
                                                                        11370000
  The GENERATE format is specified as follows:                          11380001
                                                                        11390000
         STRING GENERATE                                                11400000
                                                                        11410000
Examples:                                                               11420000
                                                                        11430000
     STRING 'ERROR===>',LINE1,'<=== POS ',((R6),,L0),INTO=WORKAREA      11440000
                                                                        11450000
     STRING 8X,C'ERRORS FOUND: ',(ERRORS,,L0),INTO=((R7),44)            11460000
                                                                        11470000
     STRING 'CVT ADDR IS ',(CVTPTR,4,X),X'40C1C2C3C4',INTO=LINE         11480000
                                                                        11490000
     LA    R5,WORK+16              end addr +1                          11500000
     STRING 'R4=',((R4),,X),INTO=(WORK,(R5)),PRINT=GEN                  11510000
                                                                        11520000
     STRING '//JOBLIB DD DSN=',(DSN1,,T),',DISP=SHR',INTO=((R2),72)     11530000
                                                                        11540000
     PUT   SYSLIN                  PUT Locate                           11550000
     LH    R0,SYSLIN+82            LRECL                                11560000
     STRING '   NAME  ',(4(R3),8,T),'(R)',INTO=((R1),(R0))              11570000
                                                                        11580000
     STRING GENERATE         Generate literals and sub-routine          11590000
                                                                        11600000
                                                                        11610000
Programming Notes:                                                      11620000
                                                                        11630000
  A STRING macro generates only 8 bytes that need to be covered by      11640001
  base registers.  More code is generated at the beginning of the       11650001
  @STRING CSECT, but this code does not require addressability.         11660001
  This is particularly useful when STRING calls specify a large         11670001
  number of literals.                                                   11680001
                                                                        11690000
  Additionally, STRING does not use A-type constants (ACON), but        11700000
  S-type constants (SCON) which require symbols to be addressable       11710000
  at the point in the program where STRING is issued.                   11720000
                                                                        11730000
  While these addressing techniques reduce the number of base           11740000
  registers required to cover the program's code and make it easier     11750000
  to write reentrant programs, they will produce assembly errors in     11760000
  the following situations:                                             11770000
                                                                        11780000
  a.  the CSECT in which STRING is used is longer than 64K              11790001
                                                                        11800000
  b.  symbols are not addressable at the point in the program           11810001
      where STRING is issued                                            11820000
./ ADD NAME=SVC34    8004-74016-74017-0907-00016-00013-00000-HERC01  00
         MACRO                                                          00010000
&NAME    SVC34 &MESG                  , issue OS command                00020000
         LCLC  &L1,&L2,&L3                                              00030000
&L1      SETC  '$'.'&SYSNDX'.'A'                                        00040000
&L2      SETC  '$'.'&SYSNDX'.'B'                                        00050000
&L3      SETC  '$'.'&SYSNDX'.'C'                                        00060000
&NAME    DS    0H                     , align                           00070000
         LA    1,&L1                  , branch around constant          00080000
         B     &L3                                                      00090000
&L1      DC    AL2(&L2-&L1),AL2(0)    , text length, flags              00100000
         DC    C&MESG                 , command text                    00110000
&L2      EQU   *                      , end of message                  00120000
&L3      DS    0H                     , alignment again                 00130000
         SR    0,0                    , indicate master console         00140000
         SVC   34                     , issue command                   00150000
         MEND                                                           00160000
./ ADD NAME=SYSGET
         MACRO                                                          00000010
&SYM     SYSGET &EODAD=EODAD,&DDNAME=SYSIN,&LRECL=80,&RECFM=,          X00000020
               &BLKSIZE=80                                              00000030
         AIF   (K'&SYM EQ 0).NOSYM                                      00000040
         AIF   (K'&SYM GT 5).ERRSYM                                     00000050
         AIF   (K'&DDNAME GT 8).DDNERR                                  00000060
         AIF   (K'&EODAD GT 8).ERREOD                                   00000070
       MNOTE *,'DCB = DDNAME=&DDNAME,DSORG=PS,MACRF=GL,EODAD=&EODAD'    00000080
       MNOTE *,'          DD CARD CHANGES WILL WORK FOR '               00000090
       MNOTE *,'      LRECL=&LRECL,BLKSIZE=&BLKSIZE,RECFM=&RECFM'       00000100
         DS    0H                                                       00000110
&SYM     NOP   &SYM.S1 .               NOP UNTIL OPEN                   00000120
         OI    &SYM.+1,C'0' .          SET NOP TO UNCONDITIONAL         00000130
         ST    14,&SYM.14 .            SAVE RETURN REG                  00000140
         OPEN  (&SYM.DCB,(INPUT))                                       00000150
&SYM.1   GET   &SYM.DCB                                                 00000160
         L     14,&SYM.14 .            RESTORE RETURN REG               00000170
         BR    14 .                    RETURN                           00000180
&SYM.S1  ST    14,&SYM.14 .            SAVE RETURN POINTER              00000190
         B     &SYM.1 .                GO GET RECORD                    00000200
&SYM.XIT CLC   &SYM.DCB+62(2),=H'0' .  IS BLOCK SIZE THERE              00000210
         BNE   &SYM.3 .                IF SO SKIP                       00000220
         MVC   &SYM.DCB+62(2),=H'&BLKSIZE' .MOVE IN SIZE                00000230
&SYM.3   CLC   &SYM.DCB+82(2),=H'0' .  IS LRECL HERE                    00000240
         BNE   &SYM.4 .                IF SO SKIP                       00000250
         MVC   &SYM.DCB+82(2),=H'&LRECL' .   MOVE IN LRECL              00000260
&SYM.4   CLI   &SYM.DCB+36,X'00' .     IS RECFM THERE                   00000270
         BNE   &SYM.5 .                BRANCH IF THERE                  00000280
         MVI   &SYM.DCB+36,X'90' .     MOVE IN DEFAULT RECFM FB         00000290
&SYM.5   BR    14 .                    RETURN TO OPEN                   00000300
&SYM.EOD CLOSE &SYM.DCB                                                 00000310
         NI    &SYM.+1,X'0F' .         RESET NOP FOR POSSIBLE REOPEN    00000320
         B     &EODAD .                GO TO USERS EODAD RTN            00000330
&SYM.14  DS    1F .                    RETURN REG SAVE AREA             00000340
&SYM.LST DC    X'85' .                 DCB OPEN EXIT                    00000350
         DC    AL3(&SYM.XIT) .         POINTER TO XIT RTN               00000360
&SYM.DCB DCB   DDNAME=&DDNAME,DSORG=PS,MACRF=(GL),EODAD=&SYM.EOD,      ,00000370
               EXLST=&SYM.LST,RECFM=&RECFM                              00000380
         AGO   .END                                                     00000390
.ERRSYM MNOTE 8,'8,*** LABEL ON SYSGET MACRO MUST BE LESS THAN SIX'     00000400
         AGO   .END                                                     00000410
.NOSYM  MNOTE 8,'8,*** SYSGET MACRO MUST HAVE A LABEL'                  00000420
         AGO   .END                                                     00000430
.DDNERR MNOTE 8,'8,*** DDNAME LARGER THAN EIGHT CHARACTERS'             00000440
         AGO   .END                                                     00000450
.ERREOD MNOTE 8,'8,*** MAXIMUM LABEL SIZE IS EIGHT CHARACTERS'          00000460
.END     MEND                                                           00000470
./ ADD NAME=SYSPRINT
         MACRO                                                          00000010
&SYM     SYSPRINT &HEADER1=,&HEADER2=,&LINES=56,&DDNAME=SYSPRINT,      X00000020
               &LRECL=121                                               00000030
         AIF   (K'&SYM EQ 0).NOSYM                                      00000040
         AIF   (K'&SYM GT 5).ERRSYM                                     00000050
         AIF   (K'&DDNAME GT 8).DDNERR                                  00000060
         AIF   ('&LRECL' GT '133').ERRLEN                               00000070
         MNOTE *,'DCB = DDNAME=&DDNAME,LRECL=&LRECL,LINES=&LINES'       00000080
         AIF   ('&HEADER1' EQ '').HD1                                   00000090
         MNOTE *,'    &HEADER1 IS FIRST HEADER LINE'                    00000100
         AIF   ('&HEADER2' EQ '').HD2                                   00000110
         MNOTE *,'    &HEADER2 IS SECOND HEADER LINE'                   00000120
.HD2     MNOTE *,'    THE FIRST BYTE OF ANY BUFFER SPECIFIED WILL'      00000130
         MNOTE *,'    BE USED BY THIS ROUTINE FOR ASA CHARACTERS'       00000140
         MNOTE *,'    AND UNPREDICTABLE RESULTS MAY OCCUR IF USED.'     00000150
         CNOP  0,4                                                      00000160
&SYM     NOP   DRC&SYSNDX.C .          NOP BRANCH SET AFTER OPEN        00000170
         NOP   DRC&SYSNDX.D .          NOP BRANCH SET AFTER OPEN        00000180
         OI    &SYM.+5,C'0' .          SET NOP TO BRANCH                00000190
         OI    &SYM.+1,C'0' .          SET FIRST NOP TO BRANCH          00000200
         STM   14,2,DRC&SYSNDX.B .     SAVE USERS REGS                  00000210
         LA    1,DRC&SYSNDX.A .        LOAD ADDRESS OF OPEN LIST        00000220
         SVC   19 .                    ISSUE OPEN SVC                   00000230
         AIF   ('&HEADER1' EQ '').HD4                                   00000240
DRC&SYSNDX.F MVI &HEADER1,C'1' .       SET CARRIAGE CONTROL             00000250
         PUT   &SYM.DCB,&HEADER1                                        00000260
         AIF   ('&HEADER2' EQ '').HD5                                   00000270
         MVI   &HEADER2,C'-' .         SET TRIPLE SPACE AFTER SKIP      00000280
         PUT   &SYM.DCB,&HEADER2                                        00000290
         AGO   .HD5                                                     00000300
.HD4     L     15,DRC&SYSNDX.B+12 .    GET POINTER TO RECORD            00000310
DRC&SYSNDX.E MVI 0(15),C'1' .            SKIP TO ONE FIRST TIME         00000320
         LA    2,&LINES.+1 .           GET LINE COUNT                   00000330
         AGO   .HD55                                                    00000340
.HD5     L     15,DRC&SYSNDX.B+12 .    PICK UP DATA POINTER             00000350
         MVI   0(15),C'0' .            SET DOUBLE SPACE AFTER SK97      00000360
         LA    2,&LINES.+1 .           PICK UP LINE COUNT               00000370
.HD55    ANOP                                                           00000380
DRC&SYSNDX.G BCT 2,DRC&SYSNDX.H .      BR TO PRINT FOR NUMBER OF LINES  00000390
         AIF   ('&HEADER1' NE '').HD6                                   00000400
         B     DRC&SYSNDX.E .          START NEW PAGE                   00000410
         AGO   .P4                                                      00000420
.HD6     B     DRC&SYSNDX.F .          GO TO DO SKIP AND PRINT HDR      00000430
.P4      ANOP                                                           00000440
DRC&SYSNDX.H PUT &SYM.DCB,(15)                                          00000450
         ST    2,DRC&SYSNDX.J .        SAVE THE LINE COUNT              00000460
         LM    14,2,DRC&SYSNDX.B .     RESTORE USERS REGS               00000470
         MVI   0(1),C' ' .             PLACE BLANK INTO CARRIAGE CONTR  00000480
         MVC   1(&LRECL.-1,1),0(1) .   CLEAR THE BUFFER                 00000490
         BR    14 .                    RETURN                           00000500
 SPACE                                                                  00000510
DRC&SYSNDX.D STM 14,2,DRC&SYSNDX.B .   SAVE REGS ALWAYS                 00000520
         AIF   ('&HEADER1' EQ '').P6                                    00000530
         B     DRC&SYSNDX.F .          GO TO HEADER ROUTINE             00000540
         AGO   .P5                                                      00000550
.P6     ANOP                                                            00000560
         L     15,DRC&SYSNDX.B+12 .    PICK UP POINTER TO DATA          00000570
         B     DRC&SYSNDX.E .          GO TO HEADER RTN                 00000580
.P5      ANOP                                                           00000590
DRC&SYSNDX.C STM 14,2,DRC&SYSNDX.B .   SAVE REGS ALWAYS                 00000600
         L     2,DRC&SYSNDX.J .        RESTORE THE LINE COUNTER         00000610
         L     15,DRC&SYSNDX.B+12 .    PICK UP POINTER TO DATA          00000620
         MVI   0(15),C' ' .            MOVE X'40' TO ASA                00000630
         B     DRC&SYSNDX.G .          GO TEST FOR LINE COUNT           00000640
DRC&SYSNDX.M CLC &SYM.DCB+62(2),=H'0' . IS BLOCK SIZE THERE             00000650
         BNE   DRC&SYSNDX.L .          IF SO SKIP                       00000660
         MVC   &SYM.DCB+62(2),=H'&LRECL'  .     MOVE IN DEFAULT SIZE    00000670
DRC&SYSNDX.L BR 14 .               RETURN TO OPEN                       00000680
 SPACE                                                                  00000690
DRC&SYSNDX.B DC 5F'0' .                PRINT RTNS SAVE AREA             00000700
DRC&SYSNDX.J DC F'0' .                 SAVE FOR LINE COUNTER            00000710
DRC&SYSNDX.K DC X'85' .                EXIT LIST FOR OPEN               00000720
         DC    AL3(DRC&SYSNDX.M) .     POINTER TO THE EXIT ROUTINE      00000730
DRC&SYSNDX.A DC AL1(143) .             OPTION BYTE FOR OPEN             00000740
         DC    AL3(&SYM.DCB) .         DCB ADCON FOR OPEN               00000750
 SPACE                                                                  00000760
&SYM.DCB DCB DDNAME=&DDNAME,LRECL=&LRECL,RECFM=FBA,EXLST=DRC&SYSNDX.K, X00000770
               DSORG=PS,MACRF=(PM)                                      00000780
         MEXIT                                                          00000790
.HD1     AIF   ('&HEADER2' EQ '').HD2                                   00000800
         MNOTE 8,'8,*** HEADER2 INVALID WITHOUT HEADER1 SPECIFIED'      00000810
         MEXIT                                                          00000820
.ERRLEN MNOTE 8,'8,*** LRECL TOO LARGE FOR SYSPRINT'                    00000830
         MEXIT                                                          00000840
.NOSYM  MNOTE 8,'8,*** SYSPRINT MACRO MUST HAVE A LABEL'                00000850
         MEXIT                                                          00000860
.ERRSYM MNOTE 8,'8,*** LABEL ON SYSPRINT MACRO MUST BE LESS THAN SIX'   00000870
         MEXIT                                                          00000880
.DDNERR MNOTE 8,'8 *** MAXIMUM DDNAME SIZE IS EIGHT CHARACTERS'         00000890
         MEXIT                                                          00000900
.BUFERR  MNOTE 8,'8,*** NO BUFFER SUPPLIED, EXECUTION IMPOSSIBLE'       00000910
.END     MEND                                                           00000920
./ ADD NAME=SYSPUT
         MACRO                                                          00000010
&SYM     SYSPUT &DDNAME=SYSPUNCH,&LRECL=80,&RECFM=,&BLKSIZE=80          00000020
         AIF   (K'&SYM GT 5).ERRSYM                                     00000030
         AIF   (K'&SYM EQ 0).NOSYM                                      00000040
         AIF   (K'&DDNAME GT 8).DDNERR                                  00000050
       MNOTE *,'DCB = DDNAME=&DDNAME,DSORG=PS,MACRF=PM'                 00000060
       MNOTE *,'             DD CARD CHANGES WILL WORK FOR'             00000070
       MNOTE *,'      LRECL=&LRECL,BLKSIZE=&BLKSIZE,RECFM=&RECFM'       00000080
         DS    0H                                                       00000090
&SYM     NOP   &SYM.1S .               NOP BRANCH SET AFTER OPEN        00000100
         OI    &SYM.+1,C'0' .          SET NOP TO BRANCH                00000110
         STM   14,1,&SYM.14 .          SAVE REGS                        00000120
         OPEN  (&SYM.DCB,(OUTPUT))                                      00000130
&SYM.1   L     0,&SYM.14+12 .          SET DATA REG                     00000140
         PUT   &SYM.DCB,(0)                                             00000150
         LM    14,1,&SYM.14 .          RESTORE REGS                     00000160
         BR    14 .                    RETURN                           00000170
&SYM.1S  STM   14,1,&SYM.14 .          SAVE REGS                        00000180
         B     &SYM.1 .                GO TO PUT RTN                    00000190
&SYM.XIT CLC   &SYM.DCB+62(2),=H'0' .  IS BLKSIZE THERE                 00000200
         BNE   &SYM.3 .                IF SO SKIP                       00000210
         MVC   &SYM.DCB+62(2),=H'&BLKSIZE' MOVE IN BLKSIZE              00000220
&SYM.3   CLC   &SYM.DCB+82(2),=H'0' .  IS LRECL THERE                   00000230
         BNE   &SYM.4 .                IF SO SKIP                       00000240
         MVC   &SYM.DCB+82(2),=H'&LRECL' MOVE IN LRECL                  00000250
&SYM.4   CLI   &SYM.DCB+36,X'00' .     IS RECFM THERE                   00000260
         BNE   &SYM.5 .                BRANCH IF THERE                  00000270
         MVI   &SYM.DCB+36,X'90' .     MOVE IN DEFAULT RECFM FB         00000280
&SYM.5   BR    14 .                    RETURN TO OPEN                   00000290
&SYM.14  DS    4F .                    REG SAVE AREA                    00000300
&SYM.LST DC    X'85' .                 OPEN EXIT LIST                   00000310
         DC    AL3(&SYM.XIT) .         ENTRY FOR OPEN EXIT              00000320
&SYM.DCB DCB   DDNAME=&DDNAME,DSORG=PS,MACRF=(PM),EXLST=&SYM.LST,      X00000330
               RECFM=&RECFM                                             00000340
         AGO   .END                                                     00000350
.ERRSYM MNOTE 8,'8,*** LABEL ON SYSPUT MACRO MUST BE LESS THAN SIX'     00000360
         AGO   .END                                                     00000370
.NOSYM  MNOTE 8,'8,*** SYSPUT MACRO MUST HAVE A LABEL'                  00000380
         AGO   .END                                                     00000390
.DDNERR MNOTE 8,'8,*** DDNAME LARGER THAN EIGHT CHARACTERS'             00000400
.END     MEND                                                           00000410
./ ADD NAME=TESTENV  8008-74024-74027-1509-00074-00119-00000-HERC01  00
         MACRO
&LABEL   TESTENV  &CICS=NO            , test runtime environment
.**********************************************************************
.* This program will try to determine the current runtime environment *
.* by scanning MVS control blocks                                     *
.*                                                                    *
.* Registers on entry                                                 *
.*                                                                    *
.*  R14 = Return address                                              *
.*  R15 = Entry point address                                         *
.*                                                                    *
.* Registers on Exit                                                  *
.*                                                                    *
.*  R1 = Environment code                                             *
.*   0 : JOB                                                          *
.*   4 : STC                                                          *
.*   8 : TSU                                                          *
.*  12 : Don't know                                                   *
.*  16 : JOB + CICS                                                   *
.*  20 : STC + CICS                                                   *
.*  24 : TSU + CICS                                                   *
.*  28 : Don't know + CICS                                            *
.*                                                                    *
.*  R15 = Return code                                                 *
.*   0 : Environment successfully determined                          *
.*   8 : Error trying to detect environment                           *
.*                                                                    *
**********************************************************************
 BOX 'Test for runtime environment',CTR=YES
         AIF   ('&LABEL' EQ '').NOLAB
&LABEL   DS    0H                     , test environment
.NOLAB   ANOP
         XR    0,0                    , R0 will become Return code
         L     14,16                  , get address of CVT
         L     14,0(14)               , point to TCB/ASCB list
         L     14,8(14)               , R14 has address of ASCB
*  IF (ICM,15,B'1111',X'AC'(14),NZ)           R15 ---> JOBNAME
         IF (ICM,15,B'1111',X'AC'(14),NZ)
         LA  1,0                      , R1 = 0: JOB
*  ELSEIF (ICM,15,B'1111',X'B0'(14),NZ)       R15 ---> STCNAME
         ELSEIF (ICM,15,B'1111',X'B0'(14),NZ)
*     IF (ICM,1,B'1111',X'3C'(14),NZ)         Is there a TSB?
         IF (ICM,1,B'1111',X'3C'(14),NZ)
         LA 1,8                       , R1 = 8: TSO User
*     ELSEIF (CLC,=CL8'INIT',NE,0(15)) otherwise, if not INIT
         ELSEIF (CLC,=CL8'INIT',NE,0(15)) otherwise, if not INIT
         LA 1,4                       , R1 = 4: STC
*     ELSE
         ELSE                         , just an initiator?
         LA    1,12                   , R1 = 12: Oops
*     ENDIF
         ENDIF
*  ELSE
         ELSE
         LA    0,8                    , indicate error
         LA    1,12                   , indicate we don't know
*  ENDIF
         ENDIF
         LR    15,0                   , set return code
   AIF ('&CICS' EQ 'NO').NOCICS
   L   14,X'21C'                      , address current TCB
   L   14,X'D0'(,14)                  , address TCB extension
*  IF  (ICM,14,B'1111',X'14'(14),NZ)  , address AFCB
         IF  (ICM,14,B'1111',X'14'(14),NZ)
*     IF (ICM,14,B'1111',8(14),NZ)    , address of CICS CSA
         IF (ICM,14,B'1111',8(14),NZ)
         LA   1,16(1)                 , add CICS indicator
*     ENDIF
         ENDIF
*   ENDIF
         ENDIF
.NOCICS ANOP
   MEXIT
   MEND
./ ADD NAME=X2CHRTAB
         MACRO
&NAME    X2CHRTAB
         GBLB  &X2CHR
         LCLC  &NAM$
         LCLB  &X2CHRL
&X2CHRL  SETB  1
         AIF   (&X2CHR).BP02
&X2CHR   SETB  1
&X2CHRL  SETB  0
.BP02    AIF   ('&NAME' EQ '').NAM1
&NAM$    SETC  '&NAME'
         AGO   .BEG
.NAM1    ANOP
&NAM$    SETC  'X2CHRTAB'
.BEG     ANOP
         ORG   *-240                    SET TABLE ORIGIN
&NAM$    DS    0CL256                   TABLE NAME
         DS    CL240                    IGNORE UNUSED VALUES
         DC    10AL1(*-&NAM$)           NUMERIC TRANSLATION VALUES
         DC    6AL1(*-&NAM$.-57)        HEX VALUE A-F TRANSLATION
         AIF   (&X2CHRL).BP03
.BP03    ANOP
         MEND
./ ADD NAME=X2CHRTRN
         MACRO
         X2CHRTRN &CFLD,&XFLD,&LEN=0,&TAB=X2CHRTAB,&PRINT=NO
         LCLA  &LXFLD,&CLEN,&XLEN,&REPS,&LREM,&DXFLD,&DCFLD
         LCLB  &X2CHRIN
&X2CHRIN SETB  1
&LXFLD   SETA  &LEN                      LENGTH OF HEX FIELD
&CLEN    SETA  15                        CHAR LENGTH
&XLEN    SETA  8                         HEX LENGTH
         AIF   (&LEN NE 0).SKIPL
&LXFLD   SETA  L'&XFLD
.SKIPL   ANOP
&REPS    SETA  (&LXFLD-1)/7              REPETITIONS REQUIRED
&LREM    SETA  &LXFLD-&REPS*7            LENGTH OF REMAONDER
         AIF   ('&PRINT' NE 'YES').LOOP
.LOOP    ANOP
         AIF   (&REPS LE 0).LAST
         UNPK  &CFLD+&DCFLD.(&CLEN),&XFLD+&DXFLD.(&XLEN)
&DCFLD   SETA  &DCFLD+14
&DXFLD   SETA  &DXFLD+7
&REPS    SETA  &REPS-1
         AGO   .LOOP
.LAST    ANOP
         AIF   (&LREM EQ 0).TR
&XLEN    SETA  &LREM
&CLEN    SETA  2*&LREM-1
         UNPK  &CFLD+&DCFLD.(&CLEN),&XFLD+&DXFLD.(&XLEN)
&DCFLD   SETA  &DCFLD+&CLEN-1
         OI    &CFLD+&DCFLD,X'F0'
&DXFLD   SETA  &DXFLD+&XLEN-1
&DCFLD   SETA  &DCFLD+1
         MVC   &CFLD+&DCFLD.(1),&XFLD+&DXFLD
         OI    &CFLD+&DCFLD,X'F0'
.TR      ANOP
&CLEN    SETA  2*&LXFLD
         TR    &CFLD.(&CLEN),&TAB
         AIF   ('&PRINT' NE 'YES').BP06
.BP06    ANOP
         MEND
./ ADD NAME=YREGS    8000-74013-74013-2122-00023-00023-00000-HERC01  00
         MACRO                                                          00010000
         YREGS &DUMMY                                                   00020000
         GBLA  &REGS                                                    00030000
&REGS    SETA  1                                                        00040000
         SPACE 1                                                        00050000
R0       EQU   0                                                        00060000
R1       EQU   1                                                        00070000
R2       EQU   2                                                        00080000
R3       EQU   3                                                        00090000
R4       EQU   4                                                        00100000
R5       EQU   5                                                        00110000
R6       EQU   6                                                        00120000
R7       EQU   7                                                        00130000
R8       EQU   8                                                        00140000
R9       EQU   9                                                        00150000
R10      EQU   10                                                       00160000
R11      EQU   11                                                       00170000
R12      EQU   12                                                       00180000
R13      EQU   13                                                       00190000
R14      EQU   14                                                       00200000
R15      EQU   15                                                       00210000
         SPACE 1                                                        00220000
         MEND                                                           00230000
