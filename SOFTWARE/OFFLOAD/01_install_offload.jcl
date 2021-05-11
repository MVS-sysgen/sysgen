//OFFLOAD$ JOB (SYS),'INSTALL OFFLOAD',CLASS=A,MSGCLASS=H,
//             USER=HERC01,PASSWORD=CUL8TR
//*
//***********************************************************
//*                                                         *
//*      SAMPLE JCL TO ASSEMBLE AND LINKEDIT THE OFFLOAD    *
//*      PROGRAM.                                           *
//*                                                         *
//***********************************************************
//*
//ASM     EXEC PGM=IFOX00,REGION=1024K,
//             PARM=(TERM,LOAD,NODECK)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//*
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSSQ,SPACE=(CYL,(15,5)),
//             DISP=(,PASS)
//SYSUT2   DD  UNIT=SYSSQ,SPACE=(CYL,(15,5))
//SYSUT3   DD  UNIT=SYSSQ,SPACE=(CYL,(15,5))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSSQ,SPACE=(CYL,(1,1),RLSE),
//             DISP=(MOD,PASS)
//SYSIN    DD  *
         MACRO
&N       #DCBD &DSORG=,&DEVD=                                       DBC
.*                                                                  DBC
.*                                                                  DBC
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DCBD TO #DCBD
.*                                                                  DBC
.* LAST CHANGE DATE - FEBRUARY 2, 1977                              DBC
.*                  - MAILING ADDRESS CHANGE.                       DBC
.*                                                                  DBC
.* LAST CHANGE DATE - APRIL 20, 1976
.*                                                                  DBC
.* THIS MACRO IS A MODIFICATION OF IBM'S DCBD MACRO FROM R21.7 OF   DBC
.* OS. ANY QUESTIONS CONCERNING IT MAY BE ADDRESSED TO:             DBC
.*       809 WHITNEY AVE.                                           DBC
.*       NEW HAVEN, CT. 06511                                       DBC
.*                                                                  DBC
.*                                                                  DBC
.*                                                                  DBC
.*   THIS MACRO DUPLICATES THE FUNCTION OF IBM'S DCBD MACRO BUT     DBC
.* WITH TWO IMPORTANT DIFFERENCES:                                  DBC
.* - THE DSECT STATEMENT IS NOT GENERATED.                          DBC
.* - A FACILITY IS PROVIDED WHEREBY THE FIRST THREE CHARACTERS      DBC
.*   (I.E. "DCB") OF EACH FIELD NAME (BUT NOT BIT NAME) CAN BE      DBC
.*   REPLACED BY ANY ONE, TWO, OR THREE CHARACTERS OF YOUR CHOICE.  DBC
.* AS A RESULT, THE #DCBD MACRO CAN BE USED ANY NUMBER OF TIMES IN  DBC
.* AN ASSEMBLY. REFER TO THE EXAMPLES GIVEN BELOW FOR A             DBC
.* PARTICULARLY USEFUL TECHNIQUE.                                   DBC
.*                                                                  DBC
.*   THIS MACRO USES THE SAME OPERANDS AS DOES THE DCBD MACRO, SO   DBC
.* PLEASE REFER TO IBM'S DATA MANAGEMENT MACROS MANUAL FOR FURTHER  DBC
.* DETAILS CONCERNING THEM.                                         DBC
.*   IN ADDITION TO THE OPERANDS, THE #DCBD MACRO RECOGNIZES THE    DBC
.* NAME FIELD WHICH SHOULD EITHER BE OMITTED OR CONTAIN A ONE, TWO, DBC
.* OR THREE CHARACTER NAME. IF THE NAME FIELD IS OMITTED, THEN THE  DBC
.* CHARACTERS "DCB" ARE USED TO PREFIX ALL FIELD NAMES. IF THE NAME DBC
.* FIELD IS GIVEN, THEN IT RATHER THAN "DCB" IS USED TO PREFIX ALL  DBC
.* FIELD NAMES. IF THE GIVEN NAME IS LONGER THAN THREE CHARACTERS,  DBC
.* THEN ASSEMBLY ERRORS ARE LIKELY TO RESULT.                       DBC
.*                                                                  DBC
.*   AS INDICATED ABOVE, THE GIVEN NAME FIELD AFFECTS ONLY FIELD    DBC
.* NAMES (E.G. "DCBOFLGS" MIGHT BE CHANGED TO "RDROFLGS"), BUT BIT  DBC
.* NAMES (E.G. "DCBOFOPN") ARE NEVER AFFECTED, SINCE THE PREFIX     DBC
.* "DCB" WILL BE USED REGUARDLESS OF WHAT IS GIVEN IN THE NAME      DBC
.* FIELD, SO IN ORDER TO TEST, FOR EXAMPLE, IF A DCB HAS BEEN       DBC
.* OPENED, YOU MIGHT USE  " TM RDROFLGS,DCBOFOPN ".                 DBC
.*   IN ADDITION, A PARTICULAR BIT NAME WILL BE GENERATED ONLY BY   DBC
.* THE FIRST #DCBD MACRO THAT REQUIRES IT, THUS A PARTICULAR BIT    DBC
.* NAME WILL BE GENERATED ONLY ONCE PER ASSEMBLY.                   DBC
.*                                                                  DBC
.*   EXAMPLES - I HAVE FOUND THAT THE MOST VALUABLE USE OF THE      DBC
.* #DCBD MACRO HAS BEEN IN SIMPLIFYING THE PROCEDURE FOR REFERING   DBC
.* TO DCB FIELDS WITHOUT USING A SPECIAL BASE REGISTER. CONSIDER    DBC
.* THE FOLLOWING EXAMPLES:                                          DBC
.*                                                                  DBC
.*       PRINT NOGEN (IF YOU'VE SEEN ONE DCB, YOU'VE SEEN THEM ALL) DBC
.*                                                                  DBC
.*RDR    #DCBD DSORG=QS                                             DBC
.*       ORG   RDRDCB                                               DBC
.*SYSIN  DCB   DDNAME=SYSIN,DSORG=PS, ...                           DBC
.*                                                                  DBC
.*SYSPRINT DCB DDNAME=SYSPRINT,DSORG=PS, ...                        DBC
.*       ORG   SYSPRINT                                             DBC
.*PRT    #DCBD DSORG=QS                                             DBC
.*                                                                  DBC
.*       ORG   *-16                                                 DBC
.*UT1    #DCBD DSORG=IS                                             DBC
.*       ORG   UT1DCB+16                                            DBC
.*SYSUT1 DCB   DDNAME=SYSUT1,DSORG=IS, ...                          DBC
.*                                                                  DBC
.*SYSUT2 DCB   DDNAME=SYSUT2,DSORG=IS, ...                          DBC
.*       ORG   SYSUT2                                               DBC
.*UT2    #DCBD DSORG=IS                                             DBC
.*                                                                  DBC
.*       PRINT GEN                                                  DBC
.*                                                                  DBC
.*   IN THE FIRST AND THIRD EXAMPLES (RDR AND UT1), THE #DCBD MACRO DBC
.* GENERATES FIELD NAMES (E.G. "RDROFLGS" AND "UT1OFLGS"); THE ORG  DBC
.* STATEMENT RESETS THE LOCATION COUNTER TO THE START OF THE FIELD  DBC
.* NAMES; AND THE DCB MACRO GENERATES THE ACTUAL DCB'S ON TOP OF    DBC
.* THE FIELD NAMES.                                                 DBC
.*   IN THE SECOND AND FOURTH EXAMPLES (PRT AND UT2) THE DCB MACRO  DBC
.* GENERATES THE DCB CODE; THE ORG STATEMENT RELOCATES BACK TO THE  DBC
.* START OF THE DCB; AND THEN THE #DCBD MACRO GENERATES THE FIELD   DBC
.* NAMES ON TOP OF THE DCB CODE. NOTE THAT THE DCB CODE IS NOT      DBC
.* DESTROYED BECAUSE THE #DCBD MACRO GENERATES ONLY DS              DBC
.* INSTRUCTIONS.                                                    DBC
.*   BIT NAMES ARE GENERATED ONLY BY THE FIRST AND THIRD #DCBD      DBC
.* MACROS. THE SECOND MACRO HAS THE IDENTICAL EXPANSION PATH AS THE DBC
.* FIRST, SO ALL BIT NAMES THAT IT MIGHT HAVE GENERATED WOULD HAVE  DBC
.* BEEN REDUNDANT, SO NONE IS GENERATED. THE SAME CAN BE SAID FOR   DBC
.* THE FOURTH #DCBD MACRO WITH RESPECT TO THE THIRD. THE THIRD      DBC
.* #DCBD MACRO HAS A DIFFERENT EXPANSION PATH FROM EITHER OF THE    DBC
.* PRECEEDING TWO, SO SOME OF THE BIT NAMES THAT IT WOULD HAVE      DBC
.* GENERATED ARE NOT REDUNDANT. THOSE BIT NAMES THAT ARE REDUNDANT  DBC
.* ARE NOT GENERATED. THOSE BIT NAMES THAT ARE NOT REDUNDANT ARE    DBC
.* GENERATED.                                                       DBC
.*   BOTH THE THIRD AND FOURTH DCB'S ARE ISAM, SO ONLY A SHORT      DBC
.* DEVICE DEPENDANT SECTION IS GENERATED BY THE DCB MACRO. TO       DBC
.* ACCOMPLISH THIS, THE DCB MACRO RELOCATES BACK 16 BYTES,          DBC
.* GENERATES THE DCB NAME, RELOCATES FORWARD 16 BYTES, AND THEN     DBC
.* GENERATES THE DCB CODE. ON THE OTHER HAND, THE PRIMARY FUNCTION  DBC
.* OF THE DCBD MACRO AND, THEREFORE, THE #DCBD MACRO IS TO CREATE   DBC
.* DSECTS. BECAUSE OF THIS IT WOULD BE LOGICALLY INCONSISTANT FOR   DBC
.* THEM TO ATTEMPT TO RELOCATE BACKWARD PRIOR TO DEFINING THE       DBC
.* STARTING POINT. THEREFORE, IN THE CASE OF ISAM (AND BDAM, AND    DBC
.* EXCP, ETC.), THE DCBD AND #DCBD MACROS FIRST DEFINE THE STARTING DBC
.* NAME AND THEN RELOCATE FORWARD (USUALLY 16 BYTES) BEFORE         DBC
.* DEFINING FIELD NAMES. IT IS FOR THIS REASON THAT THE EXTRA ORG   DBC
.* STATEMENT APPEARS IN THE THIRD EXAMPLE ABOVE. IN THE FOURTH      DBC
.* EXAMPLE THE EXTRA ORG STATEMENT IS NOT NEEDED.                   DBC
.*                                                                  DBC
.*                                                                  DBC
.*                                                                  DBC
.* INNER MACROS USED - #DSORG                                       DBC
.*                                                                  DBC
         GBLB  &#DCBDSG                                             DBC
         GBLB  &#DCBSW(150)                                         DBC
         GBLB  &ONESW
         GBLA  &IEZBITS
         LCLA  &A0            FOR DSORG= AND DEVD= ANALYSIS LOOPS
         LCLB  &DSORGIS       SET BY DSORG=IS - ISAM
         LCLB  &DSORGBX       SET BY DSORG=BX OR CX - BTAM
         LCLB  &DSORGDA       SET BY DSORG=DA - BDAM
         LCLB  &DSORGQX       SET BY DSORG=QX OR CX - QTAM
         LCLB  &DSORGCQ       SET BY DSORG=CQ - QTAM DIRECT ACCESS
.*                            MESSAGE QUEUE
         LCLB  &DSORGMQ       SET BY DSORG=MQ - QTAM PROBLEM PROGRAM
.*                            MESSAGE QUEUE INTERFACE
         LCLB  &DSORGXA       SET BY DSORG=XA - EXCP WITH APPENDAGES
         LCLB  &DSORGQS       SET BY DSORG=QS OR PS - QSAM
         LCLB  &DSORGBS       SET BY DSORG=BS OR PS OR PO - BSAM,BPAM
         LCLB  &DSORGXE       SET BY DSORG=XE - EXCP WITH EXTENSION
         LCLB  &DSORGLR       SET BY DSORG=LR - DCBLRECL FIELD ONLY
         LCLB  &DSORGGS       SET BY DSORG=GS - GAM
         LCLB  &DSORGTX       SET BY DSORG=TX - TCAM LINE GROUP
         LCLB  &DSORGTQ       SET BY DSORG=TQ - TCAM MESSAGE QUEUE
         LCLB  &DSORGTR       SET BY DSORG=TR 3705 LINE GROUP    S22024
         LCLB  &DEVDDA        DIRECT ACCESS
         LCLB  &DEVDTA        MAGNETIC TAPE
         LCLB  &DEVDPT        PAPER TAPE
         LCLB  &DEVDRD        READER OR PUNCH, DEVD=RD OR PC
         LCLB  &DEVDPR        PRINTER
         LCLB  &DEVDBS        BINARY SYNCHRONOUS
         LCLB  &DEVDWT        WORLD TRADE TELEGRAPH
         LCLB  &DEVDMR        MAGNETIC CARD READER
         LCLB  &DEVDOR        OPTICAL READER
         LCLC  &C0            SET TO EACH VALUE OF DSORG AND DEVD
         LCLB  &LSW(150)                                            DBC
         LCLC  &P                                                   DBC
&A0      SETA  0                                                    DBC
.LPXYZ   AIF   (&A0 EQ 150).ENDXYZ                                  DBC
&A0      SETA  &A0+1                                                DBC
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &ONESW)                          DBC
         AGO   .LPXYZ                                               DBC
.ENDXYZ  ANOP                                                       DBC
&#DCBDSG SETB  (&#DCBDSG OR &ONESW)                                 DBC
&P       SETC  'DCB'                                                DBC
         AIF   ('&N' EQ '').GOTPFIX                                 DBC
&P       SETC  '&N'                                                 DBC
.GOTPFIX ANOP                                                       DBC
         AIF   (&IEZBITS GT 0).SETBTS
&IEZBITS SETA  1
BIT0     EQU   128
BIT1     EQU   64
BIT2     EQU   32
BIT3     EQU   16
BIT4     EQU   8
BIT5     EQU   4
BIT6     EQU   2
BIT7     EQU   1
.SETBTS  ANOP
.*
.*                  ANALYZE DSORG OPERAND
.*
&A0      SETA  N'&DSORG       SET NUMBER OF DSORG FLEMENTS
.A1      AIF   (&A0 LE 0).D0  IF ZERO, LOOP FINISHED
&C0      SETC  '&DSORG(&A0)'  SET TO A DSORG ELEMENT
.*
.*                  TEST FOR VALID DSORG ELEMENT
.*
         AIF   ('&C0' EQ 'IS' OR '&C0' EQ 'PS' OR '&C0' EQ 'BS' OR     *
               '&C0' EQ 'QS' OR '&C0' EQ 'DA' OR '&C0' EQ 'CX' OR      *
               '&C0' EQ 'CQ' OR '&C0' EQ 'MQ' OR '&C0' EQ 'LR').A2
         AIF   ('&C0' EQ 'XE' OR '&C0' EQ 'XA' OR '&C0' EQ 'PO' OR     *
               '&C0' EQ 'BX' OR '&C0' EQ 'QX' OR '&C0' EQ 'GS' OR      *
               '&C0' EQ 'TX' OR '&C0' EQ 'TQ' OR '&C0' EQ '').A2
         AIF   ('&C0' EQ 'TR').A2                                S22024
         IHBERMAC 156,DSORG,&C0
         AGO   .AA
.*
.*                  SET VARIABLES FOR DSORG
.*
.A2      ANOP
&DSORGIS SETB  (&DSORGIS OR '&C0' EQ 'IS')
&DSORGBX SETB  (&DSORGBX OR '&C0' EQ 'BX' OR '&C0' EQ 'CX')
&DSORGDA SETB  (&DSORGDA OR '&C0' EQ 'DA')
&DSORGQX SETB  (&DSORGQX OR '&C0' EQ 'QX' OR '&C0' EQ 'CX')
&DSORGCQ SETB  (&DSORGCQ OR '&C0' EQ 'CQ')
&DSORGMQ SETB  (&DSORGMQ OR '&C0' EQ 'MQ')
&DSORGXA SETB  (&DSORGXA OR '&C0' EQ 'XA')
&DSORGQS SETB  (&DSORGQS OR '&C0' EQ 'QS' OR '&C0' EQ 'PS')
&DSORGBS SETB  (&DSORGBS OR '&C0' EQ 'BS' OR '&C0' EQ 'PS' OR '&C0' EQ *
               'PO')
&DSORGXE SETB  (&DSORGXE OR '&C0' EQ 'XE')
&DSORGLR SETB  (&DSORGLR OR '&C0' EQ 'LR')
&DSORGGS SETB  (&DSORGGS OR '&C0' EQ 'GS')
&DSORGTX SETB  (&DSORGTX OR '&C0' EQ 'TX')
&DSORGTQ SETB  (&DSORGTQ OR '&C0' EQ 'TQ')
&DSORGTR SETB  (&DSORGTR OR '&C0' EQ 'TR')                       S22024
.AA      ANOP
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER
         AGO   .A1            TO DO NEW LOOP
.*
.*                  TEST FOR ANY VALID DSORG OPERAND
.*
.D0      AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGMQ OR &DSORGXA OR &DSORGQS OR         *
               &DSORGBS OR &DSORGXE OR &DSORGLR OR &DSORGGS).D0A
         AIF   (&DSORGTX OR &DSORGTQ OR &DSORGTR).D0A            S22024
         IHBERMAC 173
.D0A     ANOP
         AIF   (T'&DEVD NE 'O').D1      BRANCH IF DEVD CODED       1318
         AIF   (&DSORGGS).B1
         AIF   (NOT (&DSORGQS OR &DSORGBS OR &DSORGXE)).B1
.*
.*                  SET DEVD DEFAULTS
.*
&DEVDDA  SETB  1
&DEVDTA  SETB  ('&DSORG' NE '(PO)')
&DEVDPT  SETB  ('&DSORG' NE '(PO)')
&DEVDPR  SETB  ('&DSORG' NE '(PO)')
&DEVDRD  SETB  ('&DSORG' NE '(PO)')
&DEVDMR  SETB  ('&DSORG' NE '(PO)')
&DEVDOR  SETB  ('&DSORG' NE '(PO)')
         AIF   ('&DSORG' EQ '(PO)').D1
         IHBERMAC 174
         AGO   .B1
.D1      ANOP
.*
.*                  ANALYZE DEVD OPERAND
.*
&A0      SETA  N'&DEVD        SET ELEMENT COUNT
.D2      ANOP
         AIF   (&A0 LE 0).B1  IF ZERO, LOOP FINISHED
&C0      SETC  '&DEVD(&A0)'   SET TO A DEVD ELEMENT
.*
.*                  TEST FOR VALID DEVD ELEMENT
.*
         AIF   ('&C0' EQ 'DA' OR '&C0' EQ 'TA' OR '&C0' EQ 'PT' OR     *
               '&C0' EQ 'PR' OR '&C0' EQ 'RD' OR '&C0' EQ 'PC' OR      *
               '&C0' EQ 'BS' OR '&C0' EQ 'WT').D3
         AIF   ('&C0' EQ 'MR' OR '&C0' EQ 'OR').D3
         AIF   ('&C0' EQ '').D4
         IHBERMAC 157,DEVD,&C0
         AGO   .D4            TO DO NEW LOOP
.*
.*                  SET VARIABLES FOR DEVD
.*
.D3      ANOP
&DSORGXE SETB  (&DSORGXE OR T'&DSORG EQ 'O') FORCE EXCP EXTENDED
&DEVDDA  SETB  (&DEVDDA OR '&C0' EQ 'DA')    DIRECT ACCESS DEVICE
&DEVDTA  SETB  (&DEVDTA OR '&C0' EQ 'TA')    MAGNETIC TAPE DEVICE
&DEVDPT  SETB  (&DEVDPT OR '&C0' EQ 'PT')    PAPER TAPE DEVICE
&DEVDPR  SETB  (&DEVDPR OR '&C0' EQ 'PR')    PRINTER
&DEVDRD  SETB  (&DEVDRD OR '&C0' EQ 'RD' OR '&C0' EQ 'PC') READER,PUNCH
&DEVDBS  SETB  (&DEVDBS OR '&C0' EQ 'BS')    BINARY SYNCHRONOUS COMM.
&DEVDWT  SETB  (&DEVDWT OR '&C0' EQ 'WT')    WORLD TRADE TELEGRAPH
&DEVDMR  SETB  (&DEVDMR OR '&C0' EQ 'MR')    MAGNETIC CHAR READER
&DEVDOR  SETB  (&DEVDOR OR '&C0' EQ 'OR')    OPTICAL READER
.D4      ANOP
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER
         AGO   .D2            TO DO NEW LOOP
.*
.B1      ANOP
         AIF   ('&DSORG(1)' EQ 'LR').BA SKIP OVER COMMENTS
         SPACE 2
*                       DCB SYMBOLIC DEFINITION FOR
         AIF   (NOT &DSORGIS).B2
*                       INDEXED SEQUENTIAL
.B2      AIF   (NOT (&DSORGQS AND &DSORGBS)).B2A
*                       PHYSICAL SEQUENTIAL
         AGO   .B3
.B2A     AIF   (NOT &DSORGQS).B2B
*                       QSAM
.B2B     AIF   (NOT &DSORGBS).B3
*                       BSAM-BPAM
.B3      AIF   (NOT &DSORGDA).B4
*                       DIRECT ACCESS
.B4      AIF   (NOT (&DSORGBX AND &DSORGQX)).B4A
*                       COMMUNICATIONS LINE GROUP
         AGO   .B5
.B4A     AIF   (NOT &DSORGBX).B4B
*                       BTAM LINE GROUP
.B4B     AIF   (NOT &DSORGQX).B5
*                       QTAM LINE GROUP
.B5      AIF   (NOT &DSORGCQ).B6
*                       COMMUNICATIONS DIRECT ACCESS QUEUE
.B6      AIF   (NOT &DSORGMQ).B6A
*                       QTAM MESSAGE QUEUE
.B6A     AIF   (NOT &DSORGTX).B6B
*                       TCAM LINE GROUP
.B6B     AIF   (NOT &DSORGTQ).B6C                                S22024
*                       TCAM MESSAGE QUEUE
.B6C     AIF   (NOT &DSORGTR).B7                                 S22024
*                       3705 LINE GROUP
.B7      AIF   (NOT (&DSORGXA AND &DSORGXE)).B8
*                       EXCP WITH EXTENSION AND APPENDAGES
         AGO   .BA
.B8      AIF   (NOT &DSORGXE).B9
*                       EXCP WITH EXTENSION
         AGO   .BA
.B9      AIF   (NOT &DSORGXA).B0
*                       EXCP WITH APPENDAGES
         AGO   .BA
.B0      AIF   (NOT &DSORGGS).B00
*                       GRAPHICS WITH APPENDAGES
         AGO   .BA
.B00     AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGMQ OR &DSORGQS OR &DSORGBS OR         *
               &DSORGTX OR &DSORGTQ OR &DSORGLR OR &DSORGTR).BA  S22024
*                       EXCP WITH EXTENSION
&DSORGXE SETB  1
.BA      ANOP
         SPACE 1
**********************************************************************
*   02/29/72          LEVEL=04
*
**********************************************************************
         SPACE 1
         AIF   (&DSORGGS).SLIP
&LSW(001) SETB (1)                                                  DBC
&P.DCB   DS    0A                  FULLWORD ALIGNMENT               DBC
         SPACE 1
         AIF   (NOT (&DEVDDA OR &DEVDTA OR &DEVDPT OR &DEVDPR OR       *
               &DEVDRD OR &DEVDWT OR &DEVDOR OR &DEVDMR)).C4
&LSW(002) SETB (1)                                                  DBC
*                       DEVICE INTERFACES
         SPACE 1
         AIF   (NOT &DEVDDA).C1
&LSW(003) SETB (1)                                                  DBC
*                       DIRECT ACCESS DEVICES
         SPACE 1
&P.RELAD DS    CL4 -          PARTITIONED ORGANIZATION DATA SET -
*                             ADDRESS (IN THE FORM TTRN) OF MEMBER
*                             CURRENTLY USED.  ---
*                             SYS1.LOGREC DATA SET - IF CCH OPTION HAS
*                             BEEN SPECIFIED IN SYSGEN PROCESS, ADDRESS
*                             OF A 12-BYTE PARAMETER IN THE EXPANSION
*                             OF MACRO INSTRUCTION IGFCATAP
&P.KEYCN DS    FL1 -          KEYED BLOCK OVERHEAD CONSTANT
&P.FDAD  DS    CL8 -          FULL DISK ADDRESS IN THE FORM OF MBBCCHHR
*                             OF RECORD THAT WAS JUST READ OR WRITTEN
         SPACE 1
         ORG   &P.FDAD+7
&P.DVTBL DS    0A -           SAME AS DCBDVTBA BELOW
         DS    X -            LAST BYTE OF DCBFDAD
&P.DVTBA DS    AL3 -          ADDRESS OF ENTRY IN I/O DEVICE
*                             CHARACTERISTICS TABLE FOR DEVICE BEING
*                             USED
         DS    FL1 -          DCBKEYLE - KEY LENGTH OF DATA SET
         DS    C -            DCBDEVT - DEVICE TYPE
*   FOR MASKS FOR ISAM DIRECT ACCESS, SEE DCBOVDEV IN ISAM SECTION
         AIF   (&#DCBSW(003)).SKP003A                               DBC
DCBDV311 EQU   X'21' -        2311 DISK DRIVE
DCBDV301 EQU   X'22' -        2301 PARALLEL DRUM
DCBDV303 EQU   X'23' -        2303 SERIAL DRUM
DCBDV302 EQU   X'24' -        2302 DISK STORAGE
DCBDV321 EQU   X'25' -        2321 DATA CELL DRIVE
DCBDV314 EQU   X'28' -        2314 DISK STORAGE FACILITY
.SKP003A ANOP  ,                                                    DBC
&P.TRBAL DS    H -            TRACK BALANCE.  NUMBER OF BYTES REMAINING
*                             ON CURRENT TRACK AFTER A WRITE OPERATION
*                             (THIS QUANTITY MAY BE NEGATIVE IF THERE
*                             ARE NO BYTES REMAINING ON TRACK).
         SPACE 1
.C1      AIF   (NOT &DEVDTA).C2
&LSW(004) SETB (1)                                                  DBC
*                       MAGNETIC TAPE
         SPACE 1
         ORG   &P.DCB
         DS    CL12 -         RESERVED FOR I/O SUPERVISOR
&P.BLKCT DS    F -            BLOCK COUNT FOR EACH VOLUME
&P.TRTCH DS    C -            TAPE RECORDING TECHNIQUE FOR 7-TRACK TAPE
         AIF   (&#DCBSW(004)).SKP004A                               DBC
DCBMTE   EQU   X'23' -        E  - EVEN PARITY
DCBMTT   EQU   X'3B' -        T  - BCD/EBCDIC TRANSLATION
DCBMTC   EQU   X'13' -        C  - DATA CONVERSION
DCBMTET  EQU   X'2B' -        ET - EVEN PARITY AND TRANSLATION
.SKP004A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(004)).SKP004B                               DBC
DCBDVMT  EQU   X'81' -        2400 SERIES MAGNETIC TAPE UNIT (7-TRACK
*                             OR 9-TRACK)
DCBDVMT3 EQU   X'83' -        3400 SERIES MAGNETIC TAPE UNIT     ICB277
.SKP004B ANOP  ,                                                    DBC
&P.DEN   DS    C -            TAPE DENSITY - 2400 SERIES MAGNETIC TAPE
*                             UNITS
         AIF   (&#DCBSW(004)).SKP004C                               DBC
*                             CODE    7-TRACK     9-TRACK
DCBMTDN0 EQU   X'03' -         0       200 BPI       -
DCBMTDN1 EQU   X'43' -         1       556 BPI       -
DCBMTDN2 EQU   X'83' -         2       800 BPI     800 BPI
DCBMTDN3 EQU   X'C3' -         3         -        1600 BPI
.SKP004C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C2      AIF   (NOT &DEVDPT).C3
&LSW(005) SETB (1)                                                  DBC
*                       PAPER TAPE
         SPACE 1
         ORG   &P.DCB+8
&P.LCTBL DS    A -            ADDRESS OF TRANSLATE TABLE
         DS    XL4 -          RESERVED
&P.CODE  DS    C -            PAPER TAPE CODE BEING USED.  THE
*                             APPROPRIATE TRANSLATE TABLE IS MADE
*                             AVAILABLE
         AIF   (&#DCBSW(005)).SKP005A                               DBC
DCBPTCDN EQU   X'80' -        N - NO CONVERSION
DCBPTCDI EQU   X'40' -        I - IBM BCD
DCBPTCDF EQU   X'20' -        F - FRIDEN
DCBPTCDB EQU   X'10' -        B - BURROUGHS
DCBPTCDC EQU   X'08' -        C - NATIONAL CASH REGISTER
DCBPTCDA EQU   X'04' -        A - ASCII (8-TRACK)
DCBPTCDT EQU   X'02' -        T - TELETYPE
.SKP005A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(005)).SKP005B                               DBC
DCBDVPTP EQU   X'50' -        2671 PAPER TAPE READER
.SKP005B ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.PTFLG DS    BL1 -          PAPER TAPE FLAGS
         AIF   (&#DCBSW(005)).SKP005C                               DBC
DCBPTIC  EQU   BIT3 -         INVALID CHARACTER IN LAST RECORD READ
DCBPTECT EQU   BIT4 -         END OF RECORD CHARACTER REACHED IN
*                             TRANSLATION
DCBPTECR EQU   BIT5 -         END OF RECORD CHARACTER DETECTED DURING
*                             READ
DCBPTUCT EQU   BIT6 -         IF ONE, UPPER CASE TRANSLATE.
*                             IF ZERO, LOWER CASE TRANSLATE
DCBPTERR EQU   BIT7 -         ERROR DETECTED ON READ
.SKP005C ANOP  ,                                                    DBC
         SPACE 1
.C3      AIF   (NOT &DEVDPR).C3A
&LSW(006) SETB (1)                                                  DBC
*                       PRINTER
         SPACE 1
         ORG   &P.DCB+16
&P.PRTSP DS    C -            NUMBER INDICATING NORMAL PRINTER SPACING
         AIF   (&#DCBSW(006)).SKP006A                               DBC
DCBPRSP0 EQU   X'01' -        0 - NO SPACING
DCBPRSP1 EQU   X'09' -        1 - SPACE ONE LINE
DCBPRSP2 EQU   X'11' -        2 - SPACE TWO LINES
DCBPRSP3 EQU   X'19' -        3 - SPACE THREE LINES
.SKP006A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(006)).SKP006B                               DBC
DCBDVPR1 EQU   X'48' -        1403 PRINTER AND 1404 PRINTER (CONTINUOUS
*                             FORM SUPPORT ONLY)
DCBDVPR2 EQU   X'4A' -        1443 PRINTER
DCBDVPR3 EQU   X'49' -        3211 PRINTER
.SKP006B ANOP  ,                                                    DBC
&P.PRTOV DS    C -            TEST-FOR-PRINTER-OVERFLOW MASK
*                             (PRTOV MASK)
         AIF   (&#DCBSW(006)).SKP006C                               DBC
DCBPRC9  EQU   X'20' -        9  - TEST FOR CHANNEL 9 OVERFLOW
DCBPRC12 EQU   X'10' -        12 - TEST FOR CHANNEL 12 OVERFLOW
.SKP006C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C3A     AIF   (NOT &DEVDRD).C3B
&LSW(007) SETB (1)                                                  DBC
*                       CARD READER, CARD PUNCH
         SPACE 1
         ORG   &P.DCB+16
&P.MODE  DS    0B -           MODE OF OPERATION FOR 1442 CARD READ
*                             PUNCH (BITS 0-3)
&P.STACK DS    B -            STACKER SELECTION (BITS 4-7)
         AIF   (&#DCBSW(007)).SKP007A                               DBC
DCBMODEC EQU   BIT0 -         COLUMN BINARY MODE
DCBMODEE EQU   BIT1 -         EBCDIC MODE
DCBMODEO EQU   BIT2 -         OPTICAL MARK READ MODE
DCBMODER EQU   BIT3 -         READ COLUMN ELIMINATE MODE
DCBSTCK2 EQU   BIT6 -         STACKER 2
DCBSTCK1 EQU   BIT7 -         STACKER 1
.SKP007A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(007)).SKP007B                               DBC
DCBDVCR0 EQU   X'41' -        2540 CARD READER
DCBDVCP0 EQU   X'42' -        2540 CARD PUNCH
DCBDVCRP EQU   X'43' -        1442 CARD READ PUNCH
DCBDVCR1 EQU   X'44' -        2501 CARD READER
DCBDVCPR EQU   X'45' -        2520 CARD READ PUNCH
DCBDVCR2 EQU   X'46'          3505 CARD READER                   XM0629
DCBDVCP1 EQU   X'4C'          3525 CARD PUNCH                    XM0629
.SKP007B ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.FUNC  DS    B -            FUNCTION INDICATOR FOR THE 3525
         AIF   (&#DCBSW(007)).SKP007C                               DBC
DCBFNCBI EQU   BIT0 -         INTERPRET (PUNCH AND PRINT TWO LINES)
DCBFNCBR EQU   BIT1 -         READ
DCBFNCBP EQU   BIT2 -         PUNCH
DCBFNCBW EQU   BIT3 -         PRINT
DCBFNCBD EQU   BIT4 -         DATA PROTECTION
DCBFNCBX EQU   BIT5 -         THIS DATA SET IS TO BE PRINTED
DCBFNCBT EQU   BIT6 -         TWO-LINE PRINT SUPPORT REQUEST
.SKP007C ANOP  ,                                                    DBC
         SPACE 1
.C3B     AIF   (NOT &DEVDWT).C3C
&LSW(008) SETB (1)                                                  DBC
*                       WORLD TRADE TELEGRAPH
         SPACE 1
         ORG   &P.DCB+16
&P.BQFLG DS    BL1 -          WTTA FLAG BYTE
         AIF   (&#DCBSW(008)).SKP008A                               DBC
DCBBQWRU EQU   BIT1 -         WRU FEATURE IS TO BE USED
DCBBQIAM EQU   BIT2 -         IAM FEATURE IS TO BE USED
DCBBQWRS EQU   BIT3 -         WRU FEATURE TO BE USED IN SEND HEADER
*                             SUBGROUP
DCBBQWRE EQU   BIT4 -         WRU FEATURE TO BE USED IN END SEND
*                             SUBGROUP
.SKP008A ANOP  ,                                                    DBC
&P.WTEOM DS    C -            EOM CHARACTER
&P.WTEOT DS    C -            EOT CHARACTER
&P.WTPAD DS    FL1 -          NUMBER OF PAD (LTRS) CHARACTERS REQUIRED
*                             FOR MOTOR-ON DELAY
         SPACE 1
.C3C     AIF   (NOT (&DEVDOR OR &DEVDMR)).C4
&LSW(009) SETB (1)                                                  DBC
*                       OPTICAL READER AND MAGNETIC CHAR READER
         SPACE 1
         ORG   &P.DCB
&P.WTOID DS    0A -           SAME AS DCBWTOIA BELOW
         DS    X -            RESERVED
&P.WTOIA DS    AL3 -          A BINARY IDENTIFICATION NUMBER ASSIGNED
*                             BY COMMUNICATIONS TASK TO MESSAGE ISSUED
*                             BY WTO MACRO.  THIS NUMBER IS USED BY THE
*                             DOM MACRO WHEN MESSAGE IS NO LONGER
*                             REQUIRED (MCS SUPPORT).  ---
*                             FOR MAGNETIC CHAR READER - AFTER FIRST
*                             READ HAS BEEN ISSUED, CONTAINS ADDRESS OF
*                             MAGNETIC INTERRUPT CONTROL BLOCK (MICB)
*                             BEING USED BY THE APPENDAGES.
         SPACE 1
         AIF   (NOT &DEVDOR).C3D
&LSW(010) SETB (1)                                                  DBC
*                       OPTICAL READER DEVICES
*                       1285, 1287, 1288
         SPACE 1
         ORG   &P.WTOID+4
&P.ERRCN DS    0A -           SAME AS DCBERRCA BELOW
         DS    X -            RESERVED
&P.ERRCA DS    AL3 -          ADDRESS OF 32 BYTES OF DECLARED STORAGE
*                             SPECIFIED BY THE USER IN HIS PROGRAM.
*                             THIS STORAGE WILL BE USED BY THE
*                             PROGRAMMING SUPPORT AS EIGHT 4-BYTE
*                             COUNTERS IN WHICH TOTALS OF CERTAIN 1285,
*                             1287 AND 1288 ERROR CONDITIONS ARE
*                             ACCUMULATED.
&P.DSPLY DS    0A -           SAME AS DCBDSPLA BELOW
         DS    X -            RESERVED
&P.DSPLA DS    AL3 -          ADDRESS OF DSPLY (BSAM) ROUTINE USED FOR
*                             KEYBOARD ENTRY OF A COMPLETE FIELD
&P.RESCN DS    0A -           SAME AS DCBRESCA BELOW
&P.RDLNE DS    0A -           SAME AS DCBRDLNA BELOW
         DS    X -            RESERVED
&P.RESCA DS    0AL3 -         ADDRESS OF RESCN (BSAM) ROUTINE USED TO
*                             FORCE ON-LINE CORRECTION OF UNREADABLE
*                             CHARACTERS
&P.RDLNA DS    AL3 -          ADDRESS OF RDLNE (QSAM) ROUTINE USED TO
*                             FORCE ON-LINE CORRECTION OF UNREADABLE
*                             CHARACTERS
&P.ORBYT DS    BL1 -          OPTICAL READER BYTE USED BY BSAM/QSAM
         AIF   (&#DCBSW(010)).SKP010A                               DBC
DCBORSYN EQU   BIT0 -         SYNAD IN CONTROL
DCBOREOF EQU   BIT1 -         END OF FILE (EOF)
DCBORBFP EQU   BIT2 -         BUFFERS PRIMED (QSAM)
.SKP010A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(010)).SKP010B                               DBC
DCBDVOR5 EQU   X'5A' -        1285 OPTICAL READER
DCBDVOR7 EQU   X'5B' -        1287 OPTICAL READER
DCBDVOR8 EQU   X'5C' -        1288 OPTICAL READER
.SKP010B ANOP  ,                                                    DBC
&P.EIB   DS    BL1 -          ERROR INDICATOR BYTE
         AIF   (&#DCBSW(010)).SKP010C                               DBC
DCBORNRM EQU   BIT1 -         THE 1287 OR 1288 SCANNER WAS UNABLE TO
*                             LOCATE THE REFERENCE MARK
DCBORREJ EQU   BIT2 -         FOR 1287, A STACKER SELECT COMMAND WAS
*                             GIVEN AFTER ALLOTTED TIME HAD ELAPSED AND
*                             THE DOCUMENT HAS BEEN PUT IN REJECT
*                             POCKET.  FOR 1288 UNFORMATTED ONLY,
*                             END-OF-PAGE HAS OCCURRED.
DCBORERR EQU   BIT3 -         A NONRECOVERABLE ERROR HAS OCCURRED.
DCBORECK EQU   BIT4 -         AN EQUIPMENT CHECK RESULTED IN AN
*                             INCOMPLETE READ
DCBORWLR EQU   BIT5 -         A WRONG-LENGTH RECORD CONDITION HAS
*                             OCCURRED
DCBORHPR EQU   BIT6 -         FOR QSAM - OPERATOR ENTERED ONE OR MORE
*                             CHARACTERS FROM THE KEYBOARD.
*                             FOR BSAM - A HOPPER EMPTY CONDITION HAS
*                             OCCURRED
DCBORDCK EQU   BIT7 -         A DATA CHECK HAS OCCURRED
.SKP010C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C3D     AIF   (NOT &DEVDMR).C4
&LSW(011) SETB (1)                                                  DBC
*                       MAGNETIC CHARACTER READER DEVICES
*                       1419 MAGNETIC CHARACTER READER
*                       1275 OPTICAL READER SORTER
         SPACE 1
         ORG   &P.DCB
&P.SSID  DS    CL8 -          BEFORE DCB IS OPENED - NAME OF USER'S
*                             STACKER SELECT ROUTINE.
         SPACE 1
         ORG   &P.SSID
         DS    A -            AFTER DCB IS OPENED - DCBWTOID
&P.SSAD  DS    0A -           ADDRESS OF USER'S STACKER SELECT ROUTINE
         DS    X -            RESERVED
&P.SSADA DS    AL3 -          ADDRESS OF USER'S STACKER SELECT ROUTINE
&P.IMAGE DS    0A -           SAME AS DCBIMAGA BELOW
&P.MRFG  DS    BL1 -          BUFFER INDICATOR
         AIF   (&#DCBSW(011)).SKP011A                               DBC
DCBMRBCT EQU   BIT0+BIT1 -    TWO-BIT BINARY COUNTER WHICH INDICATES
*                             INTO WHICH BUFFER STATUS INFORMATION IS
*                             TO BE POSTED
.SKP011A ANOP  ,                                                    DBC
&P.IMAGA DS    AL3 -          ADDRESS OF PARAMETER LIST USED TO
*                             COMMUNICATE BETWEEN USER'S PROCESSING
*                             ROUTINES AND HIS STACKER SELECT ROUTINES
&P.ECBLT DS    0A -           SAME AS DCBECBLA BELOW
&P.MRIND DS    BL1 -          INDICATOR AND COUNTER BYTE
         AIF   (&#DCBSW(011)).SKP011B                               DBC
DCBMRDCT EQU   BIT0+BIT1+BIT2 THREE-BIT BINARY COUNTER OF NUMBER OF
*                             DOCUMENTS READ AFTER DISENGAGE
DCBMRSCU EQU   BIT3 -         DCB WAS ALTERED WHEN SYNAD ROUTINE WAS
*                             ENTERED DUE TO SECONDARY CONTROL UNIT
*                             (SCU) ERROR
DCBMRPLO EQU   BIT4 -         POCKET LIGHT HAS BEEN TURNED ON
DCBMRPLS EQU   BIT5 -         POCKET LIGHT 0-6 IS BEING SET ON
DCBMRERP EQU   BIT6 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR
*                             PRIMARY CONTROL UNIT (PCU)
DCBMRERS EQU   BIT7 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR
*                             SECONDARY CONTROL UNIT (SCU)
.SKP011B ANOP  ,                                                    DBC
&P.ECBLA DS    AL3 -          ADDRESS OF ECB LIST PASSED TO WAIT MACRO
*                             BY CHECK MACRO WHEN NO 1419/1275 IS
*                             AVAILABLE FOR PROCESSING
&P.MRFLG DS    BL1 -          FLAG BYTE
         AIF   (&#DCBSW(011)).SKP011C                               DBC
DCBMRSCC EQU   BIT0 -         FIRST OR SECOND SECONDARY CONTROL UNIT
*                             COMMAND CHAIN IS BEING USED
DCBMRDBG EQU   BIT1 -         DEBUGGING MODE IN USE
DCBMRDRU EQU   BIT2 -         DISENGAGE REQUESTED BY USER
DCBMRDR  EQU   BIT3 -         DISENGAGE REQUESTED
DCBMRPCC EQU   BIT4+BIT5 -    TWO-BIT BINARY COUNTER INDICATING FIRST,
*                             SECOND OR THIRD PRIMARY CONTROL UNIT
*                             COMMAND CHAIN IS BEING USED
DCBMRDWT EQU   BIT6 -         WTO MESSAGE MUST BE DELETED
DCBMRUE  EQU   BIT7 -         UNIT EXCEPTION
.SKP011C ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(011)).SKP011D                               DBC
DCBDVMR  EQU   X'5D' -        1419 MAGNETIC CHARACTER READER
DCBDVORS EQU   X'5F' -        1275 OPTICAL READER SORTER
.SKP011D ANOP  ,                                                    DBC
&P.APPIN DS    C -            AN INDICATOR USED BY THE APPENDAGES TO
*                             PASS INFORMATION ABOUT ONE CHANNEL CHAIN
*                             TO AN APPENDAGE ASSOCIATED WITH ANOTHER
*                             CHANNEL CHAIN
         DS    X -            RESERVED
         SPACE 1
.C4      AIF   (NOT &DSORGTR).C4A                                S22024
&LSW(012) SETB (1)                                                  DBC
*                       3705 LINE TERMINAL                       S22024
         ORG   &P.DCB+8                                          S22024
&P.IPLTX DS    CL8            NAME OF MODULE TO BE USED TO IPL   S22024
*                             THE 3705                           S22024
&P.BCKUP DS    0A             FULL WORD LABEL                    S22024
         DS    BL1            RESERVED                           S22024
&P.BCKUA DS    AL3            ADDRESS OF THE DCB FOR THE         S22024
*                             BACKUP 3705.                       S22024
.C4A     AIF   (NOT (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR   *
               &DSORGXE)).C5
&LSW(013) SETB (1)                                                  DBC
*                       ACCESS METHOD COMMON INTERFACE
         SPACE 1
         ORG   &P.DCB+16
&P.RELB  DS    0F -           SAME AS DCBREL BELOW
&P.KEYLE DS    FL1 -          KEY LENGTH OF DATA SET
&P.DEVT  DS    0C -           DEVICE TYPE
         AIF   (&#DCBSW(13)).SKP13A                                 DBC
DCBDVTRM EQU   X'4F' -        TERMINAL.  (DD CONTAINS TERM=TS)
.SKP13A  ANOP  ,                                                    DBC
&P.REL   DS    FL3 -          NUMBER OF RELATIVE TRACKS OR BLOCKS IN
*                             THIS DATA SET (BDAM)
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS REQUIRED FOR THIS DATA
*                             SET.  MAY RANGE FROM 0 TO 255.  IF
*                             UNBLOCKED SPANNED RECORDS ARE USED,
*                             NUMBER OF SEGMENT WORK AREAS REQUIRED
*                             FOR THIS DATA SET.
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFL  DS    H -            LENGTH OF BUFFER.  MAY RANGE FROM 0 TO
*                             32,767.
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           ADDRESS OF IOB WHEN CHAINED SCHEDULING IS
*                             USED OR FOR 1419/1275
&P.ODEB  DS    0A -           ADDRESS OF OLD DEB
&P.LNP   DS    0FL1 -         3525 PRINTER LINE POSITION COUNTER
&P.QSLM  DS    BL1 -          QSAM LOCATE MODE LOGICAL RECORD INTERFACE
*                             INDICATOR BYTE FOR UPDAT PROCESSING OF
*                             SPANNED RECORDS
         AIF   (&#DCBSW(013)).SKP013A                               DBC
DCB1DVDS EQU   BIT0 -         ONLY ONE DEVICE IS ALLOCATED TO THIS
*                             DATA SET
DCBUPDCM EQU   BIT1 -         UPDATE COMPLETE, FREE OLD DEB
DCBUPDBT EQU   BIT2+BIT3 -    UPDATE BITS
DCBUPDT  EQU   BIT2 -         UPDATE TO TAKE PLACE
DCBNUPD  EQU   BIT2+BIT3 -    NO UPDATE TO TAKE PLACE
DCBSVDEB EQU   BIT3 -         OLD DEB ADDRESS MUST BE SAVED
.SKP013A ANOP  ,                                                    DBC
&P.IOBAA DS    0AL3 -         SAME AS DCBIOBAD ABOVE
&P.ODEBA DS    AL3 -          ADDRESS OF OLD DEB
         ORG   &P.DCB+28                                     ICBI DCB-4
&P.SVCXL DS    0A -           SAME AS DCBSVCXA BELOW         ICBI DCB-4
         DS    X -            RESERVED                       ICBI DCB-4
&P.SVCXA DS    AL3 -          POINTER TO EXIT LIST OF JES    ICBI DCB-4
*                             C.I. INTERFACE CONTROL SVC     ICBI DCB-4
         SPACE 1
*                       FOUNDATION EXTENSION
         SPACE 1
&P.EODAD DS    0A -           SAME AS DCBEODA BELOW
&P.HIARC DS    0BL1 -         HIERARCHY BITS
&P.BFTEK DS    0BL1 -         BUFFERING TECHNIQUE BITS
&P.BFALN DS    BL1 -          BUFFER ALIGNMENT BITS
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP013B                 DBC
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS
*                             BUFFERS
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW.  FOR BSAM INPUT
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS
*                             WITH KEYS - RECORD OFFSET PROCESSING.
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW (BDAM)
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
.SKP013B ANOP  ,                                                    DBC
&P.EODA  DS    AL3 -          ADDRESS OF A USER-PROVIDED ROUTINE TO
*                             HANDLE END-OF-DATA CONDITIONS
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED LIST OF EXITS
&P.RECFM DS    BL1 -          RECORD FORMAT
         AIF   (&#DCBSW(013)).SKP013C                               DBC
DCBRECLA EQU   BIT0+BIT1+BIT2 RECORD LENGTH INDICATOR - ASCII
DCBRECD  EQU   BIT2 -         ASCII VARIABLE RECORD LENGTH
DCBRECL  EQU   BIT0+BIT1 -    RECORD LENGTH INDICATOR
DCBRECF  EQU   BIT0 -         FIXED RECORD LENGTH
DCBRECV  EQU   BIT1 -         VARIABLE RECORD LENGTH
DCBRECU  EQU   BIT0+BIT1 -    UNDEFINED RECORD LENGTH
DCBRECTO EQU   BIT2 -         TRACK OVERFLOW
DCBRECBR EQU   BIT3 -         BLOCKED RECORDS
DCBRECSB EQU   BIT4 -         FOR FIXED LENGTH RECORD FORMAT - STANDARD
*                             BLOCKS.  FOR VARIABLE LENGTH RECORD
*                             FORMAT - SPANNED RECORDS
DCBRECCC EQU   BIT5+BIT6 -    CONTROL CHARACTER INDICATOR
DCBRECCA EQU   BIT5           ASA CONTROL CHARACTER
DCBRECCM EQU   BIT6 -         MACHINE CONTROL CHARACTER
DCBRECC  EQU   X'00' -        NO CONTROL CHARACTER
DCBRECKL EQU   BIT7 -         KEY LENGTH (KEYLEN) WAS SPECIFIED IN DCB
*                             MACRO INSTRUCTION
.SKP013C ANOP  ,                                                    DBC
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED LIST OF EXITS
         SPACE 1
.C5      AIF   (NOT &DSORGBX).C5B
&LSW(014) SETB (1)                                                  DBC
         AIF   (&DSORGQX AND (&DSORGIS OR &DSORGDA OR &DSORGQS OR      *
               &DSORGBS OR &DSORGXE)).C5A
&LSW(015) SETB (1)                                                  DBC
*                       BTAM LINE GROUP INTERFACE
         SPACE 1
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE).C5A
&LSW(016) SETB (1)                                                  DBC
         ORG   &P.DCB+20
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS OBTAINED BY OPEN
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFL  DS    H -            BUFFER LENGTH
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           BASE FOR ADDRESSING IOB'S (BASE = ADDRESS
*                             OF FIRST IOB MINUS LENGTH OF AN IOB)
         DS    FL1 -          DCBDEVTP - INDEX TO DEVICE ENTRY IN THE
*                             DEVICE I/O DIRECTORY
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE
&P.HIARC DS    0BL1 -         HIERARCHY FLAG BITS
&P.BFTEK DS    BL1 -          BUFFERING TECHNIQUE FLAG BITS
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP016A                 DBC
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS
*                             BUFFERS
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW.  FOR BSAM INPUT
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW (BDAM)
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
.SKP016A ANOP  ,                                                    DBC
         DS    BL1 -          DCBERROP - ERROR RECOVERY PROCEDURE BITS
         DS    FL1 -          DCBBUFCT - MAX NUMBER OF READ BUFFERS
         DS    X -            RESERVED
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED EXIT LIST
         DS    FL1 -          DCBEIOBX - SIZE OF IOB
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED EXIT LIST
         SPACE 1
.C5A     ANOP
         ORG   &P.DCB+33
&P.ERROP DS    BL1 -          ERROR RECOVERY PROCEDURE BITS
         AIF   (&#DCBSW(014)).SKP014A                               DBC
DCBERPT  EQU   BIT3 -         ON-LINE TEST FACILITIES TO BE USED
DCBERPC  EQU   BIT4 -         THRESHOLD AND CUMULATIVE ERROR COUNTS TO
*                             BE MAINTAINED
DCBERPW  EQU   BIT5 -         TEXT-WRITE ERRORS TO BE RETRIED
DCBERPR  EQU   BIT6 -         TEXT-READ ERRORS TO BE RETRIED
DCBERPN  EQU   BIT7 -         IF ZERO, BASIC ERP TO BE FOLLOWED ---
*                             IF ONE, NO ERP TO BE FOLLOWED
.SKP014A ANOP  ,                                                    DBC
&P.BUFCT DS    FL1 -          CONTAINS MAXIMUM NUMBER OF BUFFERS TO BE
*                             OBTAINED BY BTAM FOR READ OPERATION
*                             (DYNAMIC BUFFERING ONLY)
         SPACE 1
         AIF   (&DSORGQX OR &DSORGTX).C5B
&LSW(017) SETB (1)                                                  DBC
         ORG   &P.DCB+28
&P.DEVTP DS    FL1 -          INDEX TO DEVICE ENTRY IN THE DEVICE I/O
*                             DIRECTORY
         SPACE 1
         ORG   &P.DCB+36
&P.EIOBX DS    FL1 -          SIZE OF EXTENDED IOB.  SIZE OF AN IOB
*                             ASSOCIATED WITH THIS DCB
         SPACE 1
.C5B     AIF   (NOT &DSORGTX).C5B1
&LSW(018) SETB (1)                                                  DBC
*                       TCAM LINE GROUP INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.MHA   DS    0A -           SAME AS DCBMH BELOW
&P.BUFIN DS    0BL1 -         NUMBER OF INPUT BUFFERS (BITS 0-3)
&P.BUFOU DS    BL1 -          NUMBER OF OUTPUT BUFFERS (BITS 4-7)
         AIF   (&#DCBSW(018)).SKP018A                               DBC
DCBBFIN  EQU   BIT0+BIT1+BIT2+BIT3 NUMBER OF BUFFERS ASSIGNED
*                             INITIALLY FOR RECEIVING OPERATIONS, FOR
*                             EACH LINE IN LINE GROUP
DCBBFOUT EQU   BIT4+BIT5+BIT6+BIT7 NUMBER OF BUFFERS ASSIGNED
*                             INITIALLY FOR SENDING OPERATIONS, FOR
*                             EACH LINE IN LINE GROUP
.SKP018A ANOP  ,                                                    DBC
&P.MH    DS    AL3 -          ADDRESS OF MESSAGE HANDLER FOR THIS LINE
*                             GROUP
         DS    FL1 -          DCBINTVL - NUMBER OF SECONDS OF
*                             INVITATION DELAY
&P.PCI   DS    BL1 -          PROGRAM CONTROLLED INTERRUPTION HANDLING
         AIF   (&#DCBSW(018)).SKP018B                               DBC
DCBPCIX1 EQU   BIT0 -         PCI=(X,)                       ICBI DCB-8
DCBPCIX2 EQU   BIT1 -         PCI=(,X)                       ICBI DCB-8
DCBPCIA1 EQU   BIT2 -         PCI=(A,)
DCBPCIA2 EQU   BIT3 -         PCI=(,A)
DCBPCIN1 EQU   BIT4 -         PCI=(N,)
DCBPCIN2 EQU   BIT5 -         PCI=(,N)
DCBPCIR1 EQU   BIT6 -         PCI=(R,)
DCBPCIR2 EQU   BIT7 -         PCI=(,R)
.SKP018B ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGQS OR &DSORGBS OR &DSORGXE).C5B2
&LSW(019) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
         AGO   .C5B3
.C5B2    ANOP
&LSW(020) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
.C5B3    ANOP
&P.BUFMA DS    FL1 -          MAXIMUM NUMBER OF BUFFERS TO BE USED FOR
*                             DATA TRANSFER FOR EACH LINE IN THIS GROUP
         SPACE 1
.C5B1    AIF   (NOT (&DSORGQX OR &DSORGTX)).C6
&LSW(021) SETB (1)                                                  DBC
*                       QTAM LINE GROUP INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.CLPS  DS    0A -           ADDRESS OF LINE PROCEDURE SPECIFICATION
*                             ROUTINE
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS REQUESTED FOR A READ
*                             OR WRITE OPERATION
&P.CLPSA DS    AL3 -          SAME AS DCBCLPS ABOVE
&P.INTVL DS    FL1 -          NUMBER OF SECONDS OF INTENTIONAL DELAY
*                             BETWEEN PASSES THROUGH A POLLING LIST
*                             FOR NONSWITCHED LINES
         DS    X -            RESERVED
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *
               &DSORGBS OR &DSORGXE OR &DSORGTX).C5C
&LSW(022) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP022A                 DBC
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY
.SKP022A ANOP  ,                                                    DBC
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         SPACE 1
         AGO   .C6
.C5C     ANOP
&LSW(023) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
         AIF   (NOT &DSORGTX).C5C1
&LSW(024) SETB (1)                                                  DBC
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGCQ OR        *
               &DSORGQS OR &DSORGBS OR &DSORGXE).C5C1
&LSW(025) SETB (1)                                                  DBC
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB
         SPACE 1
         AGO   .C5C2
.C5C1    ANOP
&LSW(026) SETB (1)                                                  DBC
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
         DS    AL3 -          DCBIOBAA - ADDRESS OF FIRST IOB
         SPACE 1
.C5C2    AIF   (NOT &DSORGTX).C5D
&LSW(027) SETB (1)                                                  DBC
         ORG   &P.DCB+32
&P.TRANA DS    0A -           ADDRESS OF TRANSLATION TABLE
         DS    BL1 -          DCBCPRI - COMMUNICATION PRIORITY BITS
&P.TRANS DS    AL3 -          ADDRESS OF TRANSLATION TABLE
         SPACE 1
.C5D     ANOP
         ORG   &P.DCB+32
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP023A                 DBC
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY
.SKP023A ANOP  ,                                                    DBC
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *
               &DSORGBS OR &DSORGXE).C5E
&LSW(028) SETB (1)                                                  DBC
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         SPACE 1
         AGO   .C6
.C5E     ANOP
&LSW(029) SETB (1)                                                  DBC
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST
         SPACE 1
.C6      AIF   (NOT (&DSORGMQ OR &DSORGTQ)).C7
&LSW(030) SETB (1)                                                  DBC
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.TRMAD DS    0A -           ADDRESS OF USER-PROVIDED AREA IN WHICH
*                             THE TERMINAL NAME IS STORED
         AIF   (&DSORGQX OR &DSORGTX).C6A
&LSW(031) SETB (1)                                                  DBC
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS TO BE FILLED FROM THE
*                             DIRECT ACCESS QUEUE
         AGO   .C6B
.C6A     ANOP
&LSW(032) SETB (1)                                                  DBC
         DS    FL1 -          DCBBUFRQ - NUMBER OF BUFFERS TO BE FILLED
*                             FROM THE DIRECT ACCESS QUEUE
.C6B     ANOP
&P.TRMA  DS    AL3 -          SAME AS DCBTRMAD ABOVE
&P.SOWA  DS    H -            SIZE OF USER-PROVIDED WORK AREA
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTX).C6C
&LSW(033) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           BEFORE OPEN - ADDRESS OF AVT  ---
*                             AFTER OPEN - BASE FOR ADDRESSING IOB'S
*                             (BASE = ADDRESS OF FIRST IOB MINUS LENGTH
*                             OF ONE IOB)
         DS    FL1 -          DCBBUFMA - MAXIMUM NUMBER OF BUFFERS TO
*                             BE USED FOR DATA TRANSFER FOR EACH LINE
*                             IN THIS GROUP
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE
         AGO   .C6D
.C6C     ANOP
&LSW(034) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
         DS    A -            DCBIOBAD - BASE FOR ADDRESSING IOB'S
.C6D     ANOP
         SPACE 1
         ORG   &P.DCB+28
&P.SEGAD DS    A -            ADDRESS OF CURRENT SEGMENT
         AIF   (NOT &DSORGTQ).C6D1
&LSW(035) SETB (1)                                                  DBC
&P.THRES DS    FL1 -          FOR NON-REUSABLE MESSAGE QUEUE RECORDS,
*                             PERCENTAGE OF NON-REUSABLE DISK MESSAGE
*                             QUEUE RECORDS TO BE USED BEFORE A FLUSH
*                             CLOSEDOWN OF THE SYSTEM IS INITIATED.
*                             FOR REUSABLE MESSAGE QUEUE RECORDS AND
*                             CHECKPOINT RECORDS, THIS FIELD IS
*                             RESERVED
         AGO   .C6D2
.C6D1    ANOP
&LSW(036) SETB (1)                                                  DBC
         DS    X -            RESERVED
.C6D2    ANOP
         SPACE 1
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE).C7
&LSW(037) SETB (1)                                                  DBC
         ORG   &P.DCB+32
&P.EODAD DS    A -            ADDRESS OF USER-PROVIDED ROUTINE
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6E
&LSW(038) SETB (1)                                                  DBC
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
         AGO   .C6F
.C6E     ANOP
&LSW(039) SETB (1)                                                  DBC
         DS    0A -           DCBEXLST - ADDRESS OF EXIT LIST
.C6F     ANOP
&P.RECFM DS    C -            RECORD FORMAT
         AIF   (&#DCBSW(037)).SKP037A                               DBC
DCBRECR  EQU   X'02' -        RECORD
DCBRECG  EQU   X'04' -        MESSAGE
DCBRECS  EQU   X'08' -        SEGMENT
.SKP037A ANOP  ,                                                    DBC
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6G
&LSW(040) SETB (1)                                                  DBC
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         AGO   .C7
.C6G     ANOP
&LSW(041) SETB (1)                                                  DBC
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST
.C7      ANOP
         SPACE 1
         AIF   (NOT &DSORGCQ OR (&DSORGIS OR &DSORGBX OR &DSORGDA OR   *
               &DSORGQX OR &DSORGQS OR &DSORGBS OR &DSORGXE)).C8
&LSW(042) SETB (1)                                                  DBC
*                       QTAM DIRECT ACCESS MESSAGE QUEUE
         SPACE 1
         ORG   &P.DCB+20
&P.BUFCB DS    0A -           ADDRESS OF TERMINAL TABLE
&P.BUFNO DS    X -            RESERVED
&P.BUFCA DS    AL3 -          ADDRESS OF TERMINAL TABLE
&P.BUFL  DS    H -            SIZE OF THE DATA IN BUFFER EQUATED TO
*                             IECKBUFL
&P       #DSORG                                                     DBC
&P.IOBAD DS    A -            ADDRESS OF IOB
         SPACE 1
.C8      ANOP                                                    S22024
         AIF   (NOT &DSORGTR).C8A7                               S22024
&LSW(043) SETB (1)                                                  DBC
         ORG   &P.DCB+20                                         S22024
&P.DUMPD DS    0A             FULL WORD LABEL                    S22024
&P.UNITN DS    BL1            NUMBER OF UNITS FOR READ FOLLOWING S22024
*                             ATTENTION.                         S22024
&P.DUMPA DS    AL3            ADDRESS OF THE DCB USED FOR        S22024
*                             DUMPING THE 3705                   S22024
         DS    AL1            RESERVED                           S22024
&P.TRSTA DS    BL1            STATUS BYTE. WHEN SET TO 1,        S22024
*                             THE INDICATORS HAVE THE SPECIFIED  S22024
*                             MEANING                            S22024
         AIF   (&#DCBSW(043)).SKP043A                               DBC
DCBAUTOI EQU   BIT0           IPLAUTO=YES WAS SPECIFIED IN THE   S22024
*                             DCB                                S22024
DCBAUTOD EQU   BIT1           DMPAUTO=YES WAS SPECIFIED IN THE   S22024
*                             DCB MACRO.                         S22024
DCBINITL EQU   BIT2           BRINGUP=YES WAS SPECIFIED IN THE   S22024
*                             DCB MACRO.                         S22024
DCBRSTRT EQU   BIT3           RESTART IS IN PROCESS              S22024
DCBIPLED EQU   BIT4           3705 HAS BEEN IPL'D.               S22024
DCBBAKUP EQU   BIT5           BACKUP=YES WAS SPECIFIED IN THE    S22024
*                             DCB MACRO.                         S22024
DCBNIDLE EQU   BIT6           IDLE=NO WAS SPECIFIED IN THE OPEN  S22024
*                             MACRO OR WAS IMPLIED BY DEFAULT    S22024
DCBCHNGL EQU   BIT7           IPL TEXT HAS BEEN CHANGED          S22024
.SKP043A ANOP  ,                                                    DBC
         AIF   (NOT &DSORGTR OR (&DSORGCQ OR &DSORGIS OR &DSORGBX OR   *
               &DSORGDA OR &DSORGQX OR &DSORGQS OR &DSORGBS OR         *
               &DSORGXE OR &DSORGMQ OR &DSORGTQ OR &DSORGTX)).C8A0
&LSW(044) SETB (1)                                                  DBC
&P.DSORG DS    0BL2           DATA SET ORGANIZATION BEING USED   S22024
&P.DSRG1 DS    BL1            FIRST BYTE OF DCBDSORG             S22024
&P.DSRG2 DS    BL1            SECOND BYTE OF DCBDSORG            S22024
         AIF   (&#DCBDSG OR &#DCBSW(44)).SKP044A                    DBC
DCBDSGTR EQU   BIT5           DSORG=TR SPECIFIED                 S22024
&P.IOBAD DS    A              ADDRESS OF IOB                     S22024
.SKP044A ANOP  ,                                                    DBC
         AGO   .C8A1                                             S22024
.C8A0    ANOP                                                    S22024
&LSW(045) SETB (1)                                                  DBC
         DS    H              DCBDSORG                           S22024
         DS    A              DCBIOBAD                           S22024
.C8A1    ANOP                                                    S22024
&P.RNCKD DS    0A             FULL WORD LABEL                    S22024
         DS    BL1            RESERVED                           S22024
&P.RNCKA DS    AL3            ADDRESS OF THE DCB USED TO RETAIN  S22024
*                             INCIDENT CHECKPOINT RECORDS        S22024
*                             GENERATED BY THE 3705.             S22024
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE OR &DSORGBX OR &DSORGQX OR &DSORGTX OR         *
               &DSORGMQ OR &DSORGTQ).C8A2                        S22024
&LSW(046) SETB (1)                                                  DBC
&P.EXLST DS    0A             FULL WORD LABEL FOR EXLIST         S22024
         AGO   .C8A3                                             S22024
.C8A2    ANOP                                                    S22024
&LSW(047) SETB (1)                                                  DBC
         DS    0A             DCBEXLST                           S22024
.C8A3    ANOP                                                    S22024
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C8A4           S22024
&LSW(048) SETB (1)                                                  DBC
&P.EIOBX DS    BL1            SIZE, IN BYTES, OF THE IOB.        S22024
         AGO   .C8A5                                             S22024
.C8A4    ANOP                                                    S22024
&LSW(049) SETB (1)                                                  DBC
         DS    BL1            DCBEIOBX                           S22024
.C8A5    ANOP                                                    S22024
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX OR &DSORGIS OR        *
               &DSORGDA OR &DSORGQS OR &DSORGBS OR &DSORGXE  OR        *
               &DSORGMQ OR &DSORGTQ).C8A6                        S22024
&LSW(050) SETB (1)                                                  DBC
&P.EXLSA DS    AL3            ADDRESS OF THE EXIT LIST.          S22024
         AGO   .C8A7                                             S22024
.C8A6    ANOP                                                    S22024
&LSW(051) SETB (1)                                                  DBC
         DS    AL3            DCBEXLSA                           S22024
.C8A7    AIF   (&DSORGLR AND NOT (&DSORGIS OR &DSORGBX OR &DSORGDA OR  *
               &DSORGQX OR &DSORGCQ OR &DSORGMQ OR &DSORGXA OR         *
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTR)).CF
&LSW(052) SETB (1)                                                  DBC
*                       FOUNDATION BEFORE OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.DDNAM DS    CL8 -          NAME ON THE DD STATEMENT WHICH DEFINES
*                             THE DATA SET ASSOCIATED WITH THIS DCB
&P.OFLGS DS    BL1 -          FLAGS USED BY OPEN ROUTINE
         AIF   (&#DCBSW(052)).SKP052A                               DBC
DCBOFLWR EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS READ OR
*                             POINT.  IF ONE, LAST I/O OPERATION WAS
*                             WRITE.
DCBOFIOD EQU   BIT0 -         DATA SET IS BEING OPENED FOR INPUT OR
*                             OUTPUT (BDAM)
DCBOFLRB EQU   BIT1 -         LAST I/O OPERATION WAS IN READ BACKWARD
*                             MODE
         AIF   (&#DCBSW(108)).SKP052A                               DBC
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE
*                             ROUTINE FOR CONCATENATION OF DATA SETS
*                             WITH UNLIKE ATTRIBUTES
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A
*                             CONCATENATION OF UNLIKE ATTRIBUTES
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1
*                             ON RETURN FROM USER EXIT TO THE I/O
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION
.SKP052A ANOP  ,                                                    DBC
&P.IFLG  DS    BL1 -          FLAGS USED BY IOS IN COMMUNICATING ERROR
*                             CONDITIONS AND IN DETERMINING CORRECTIVE
*                             PROCEDURES
         AIF   (&#DCBSW(052)).SKP052B                               DBC
DCBIBEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR
DCBIFNEP EQU   X'00' -        NOT IN ERROR PROCEDURE
DCBEX    EQU   BIT1           ERROR CORRECTION OR IOS PAGE FIX IN
*                             PROCESS
DCBIFPEC EQU   BIT0+BIT1 -    PERMANENT ERROR CORRECTION
DCBIBPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR
DCBIFC9  EQU   BIT2 -         CHANNEL 9 PRINTER CARRIAGE TAPE PUNCH
*                             SENSED
DCBIFC12 EQU   BIT3 -         CHANNEL 12 PRINTER CARRIAGE TAPE PUNCH
*                             SENSED
DCBIBIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR
DCBIFER  EQU   X'00' -        ALWAYS USE I/O SUPERVISOR ERROR ROUTINE
DCBIFNE1 EQU   BIT5 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE
DCBIFTIM EQU   BIT5 -         TEST IOS MASK (IMSK) FOR ERROR PROCEDURE
*                             (BTAM)
DCBIFNE2 EQU   BIT4 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE
DCBIFNE3 EQU   BIT4+BIT5 -    NEVER USE I/O SUPERVISOR ERROR ROUTINE
.SKP052B ANOP  ,                                                    DBC
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR
         AIF   (&#DCBSW(052)).SKP052E                               DBC
DCBMRECP EQU   BIT0 -         EXECUTE CHANNEL PROGRAM (EXCP) ---
*                             ALWAYS ZERO (BSAM, QSAM, BPAM, BISAM,
*                             QISAM, BDAM) --- RESERVED (QTAM, BTAM)
DCBMRFE  EQU   BIT1 -         FOUNDATION EXTENSION IS PRESENT (EXCP)
DCBMRGET EQU   BIT1 -         GET (QSAM, QISAM, TCAM)
DCBMRPTQ EQU   BIT1 -         PUT FOR MESSAGE GROUP (QTAM) ---
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) ---
*                             RESERVED (BTAM)
DCBMRAPG EQU   BIT2 -         APPENDAGES ARE REQUIRED (EXCP)
         AIF   (&#DCBSW(108)).SKP052C                               DBC
DCBMRRD  EQU   BIT2 -         READ (BSAM, BPAM, BISAM, BDAM, BTAM)
.SKP052C ANOP  ,                                                    DBC
DCBMRWRQ EQU   BIT2 -         WRITE FOR LINE GROUP (QTAM) ---
*                             ALWAYS ZERO (QSAM, QISAM)
DCBMRCI  EQU   BIT3 -         COMMON INTERFACE (EXCP)
DCBMRMVG EQU   BIT3 -         MOVE MODE OF GET (QSAM, QISAM)
DCBMRRDK EQU   BIT3 -         KEY SEGMENT WITH READ (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BSAM, BPAM, QTAM, BTAM)
DCBMRLCG EQU   BIT4 -         LOCATE MODE OF GET (QSAM, QISAM)
DCBMRRDI EQU   BIT4 -         ID ARGUMENT WITH READ (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)
DCBMRABC EQU   BIT5 -         USER'S PROGRAM MAINTAINS ACCURATE BLOCK
*                             COUNT (EXCP)
DCBMRPT1 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)
DCBMRSBG EQU   BIT5 -         SUBSTITUTE MODE OF GET (QSAM)
DCBMRDBF EQU   BIT5 -         DYNAMIC BUFFERING (BISAM, BDAM) ---
*                             ALWAYS ZERO (QISAM) ---
*                             RESERVED (QTAM, BTAM)
DCBPGFXA EQU   BIT6 -         PAGE FIX APPENDAGE IS SPECIFIED (EXCP)
         AIF   (&#DCBSW(108)).SKP052D                               DBC
DCBMRCRL EQU   BIT6 -         CNTRL (BSAM, QSAM)
.SKP052D ANOP  ,                                                    DBC
DCBMRCHK EQU   BIT6 -         CHECK (BISAM)
DCBMRRDX EQU   BIT6 -         READ EXCLUSIVE (BDAM) ---
*                             RESERVED (BPAM, QISAM, QTAM, BTAM)
DCBMRDMG EQU   BIT7 -         DATA MODE OF GET (QSAM)
DCBMRCK  EQU   BIT7 -         CHECK (BDAM) --- RESERVED (EXCP, BSAM,
*                             BPAM, BISAM, QISAM, QTAM, BTAM)
.SKP052E ANOP  ,                                                    DBC
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR
         AIF   (&#DCBSW(052)).SKP052H                               DBC
DCBMRSTL EQU   BIT0 -         SETL (QISAM) --- ALWAYS ZERO (BSAM, QSAM,
*                             BPAM, BISAM, BDAM) ---
*                             RESERVED (EXCP, QTAM, BTAM)
DCBMRPUT EQU   BIT1 -         PUT (QSAM, TCAM) - PUT OR PUTX (QISAM)
DCBMRGTQ EQU   BIT1 -         GET FOR MESSAGE GROUP (QTAM) ---
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) ---
*                             RESERVED (EXCP, BTAM)
         AIF   (&#DCBSW(108)).SKP052F                               DBC
DCBMRWRT EQU   BIT2 -         WRITE (BSAM, BPAM, BISAM, BDAM, BTAM)
.SKP052F ANOP  ,                                                    DBC
DCBMRRDQ EQU   BIT2 -         READ FOR LINE GROUP (QTAM) ---
*                             ALWAYS ZERO (QSAM, QISAM) ---
*                             RESERVED (EXCP)
DCBMRMVP EQU   BIT3 -         MOVE MODE OF PUT (QSAM, QISAM)
DCBMRWRK EQU   BIT3 -         KEY SEGMENT WITH WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)
DCBMR5WD EQU   BIT4 -         FIVE-WORD DEVICE INTERFACE (EXCP)
DCBMRLDM EQU   BIT4 -         LOAD MODE BSAM (CREATE BDAM DATA SET)
*                             (BSAM)
DCBMRLCP EQU   BIT4 -         LOCATE MODE OF PUT (QSAM, QISAM)
DCBMRIDW EQU   BIT4 -         ID ARGUMENT WITH WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
DCBMR4WD EQU   BIT5 -         FOUR-WORD DEVICE INTERFACE (EXCP)
DCBMRPT2 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)
DCBMRTMD EQU   BIT5 -         SUBSTITUTE MODE (QSAM)
DCBMRUIP EQU   BIT5 -         UPDATE IN PLACE (PUTX) (QISAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BDAM, QTAM, BTAM)
DCBMR3WD EQU   BIT6 -         THREE-WORD DEVICE INTERFACE (EXCP)
         AIF   (&#DCBSW(108)).SKP052G                               DBC
DCBMRCTL EQU   BIT6 -         CNTRL (BSAM, QSAM)
.SKP052G ANOP  ,                                                    DBC
DCBMRSTK EQU   BIT6 -         SETL BY KEY (QISAM)
DCBMRAWR EQU   BIT6 -         ADD TYPE OF WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
DCBMR1WD EQU   BIT7 -         ONE-WORD DEVICE INTERFACE (EXCP)
DCBMRSWA EQU   BIT7 -         USER'S PROGRAM HAS PROVIDED A SEGMENT
*                             WORK AREA POOL (BSAM CREATE BDAM, BDAM)
DCBMRDMD EQU   BIT7 -         DATA MODE (QSAM)
DCBMRSTI EQU   BIT7 -         SETL BY ID (QISAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
.SKP052H ANOP  ,                                                    DBC
         SPACE 1
*                       FOUNDATION AFTER OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.TIOT  DS    H -            OFFSET FROM TIOT ORIGIN TO TIOELNGH FIELD
*                             IN TIOT ENTRY FOR DD STATEMENT ASSOCIATED
*                             WITH THIS DCB
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN
         AIF   (&#DCBSW(052)).SKP052I                               DBC
DCBIFEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR
DCBIFPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR
DCBIFIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR
.SKP052I ANOP  ,                                                    DBC
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB
         SPACE 1
         AIF   (NOT (&DSORGBX OR &DSORGDA OR &DSORGQX OR &DSORGBS)).C8A
&LSW(053) SETB (1)                                                  DBC
         ORG   &P.DCB+48
&P.READ  DS    0A -           ADDRESS OF READ MODULE
&P.WRITE DS    A -            ADDRESS OF WRITE MODULE
         SPACE 1
.C8A     AIF   (NOT (&DSORGIS OR &DSORGQX OR &DSORGMQ OR &DSORGQS OR   *
               &DSORGTR)).C8B                                    S22024
&LSW(054) SETB (1)                                                  DBC
         ORG   &P.DCB+48
&P.GET   DS    0A -           ADDRESS OF GET MODULE
&P.PUT   DS    A -            ADDRESS OF PUT MODULE
         SPACE 1
.C8B     ANOP
         AIF   (NOT (&DSORGTX OR &DSORGTR)).C8B1
&LSW(055) SETB (1)                                                  DBC
*                       TCAM LINE GROUP EXTENSION
*                       3705 EXTENSION
         SPACE 1
         ORG   &P.DCB+48
&P.SCTAB DS    0A -           ADDRESS OF SPECIAL CHARACTERS TABLE (SCT)
         DS    BL1 -          DCBOFLGS - FLAGS USED BY OPEN ROUTINE
&P.SCTAD DS    AL3 -          ADDRESS OF SPECIAL CHARACTERS TABLE (SCT)
&P.ILCT  DS    FL1 -          COUNT OF INVITATION LISTS
&P.UNTCT DS    FL1 -          BEFORE OPEN - NUMERICAL VALUE OF SCT.
*                             AFTER OPEN - COUNT OF UNITS FOR 1 BUFFER.
&P.BUFSI DS    H -            SIZE OF ALL BUFFERS USED FOR THIS LINE
*                             GROUP
         AIF   (NOT &DSORGTX).C8B1                               S22024
&LSW(056) SETB (1)                                                  DBC
&P.RESER DS    0CL4 -         NUMBER OF RESERVED BYTES IN BUFFERS
&P.RESB1 DS    FL1 -          NUMBER OF BYTES RESERVED IN THE BUFFER
*                             RECEIVING FIRST INCOMING SEGMENT OF A
*                             MESSAGE
&P.RESB2 DS    FL1 -          NUMBER OF BYTES RESERVED IN ALL BUFFERS
*                             EXCEPT THE ONE CONTAINING FIRST SEGMENT
*                             OF A MESSAGE
         DS    XL2 -          RESERVED
         SPACE 1
*        THE FOLLOWING 4 BYTES MAY BE REPEATED 'N' TIMES
&P.INVLI DS    0A -           ADDRESS OF INVITATION LIST
&P.INVCI DS    BL1 -          TYPE OF COMMUNICATION INTERFACE FOR 2701
*                             DATA ADAPTER UNIT
         AIF   (&#DCBSW(056)).SKP056A                               DBC
DCBINVB1 EQU   BIT2 -         IF ZERO, UNIT (A,)
*                             IF ONE, UNIT (B,)
DCBINVB2 EQU   BIT4 -         IF ZERO, UNIT (,A)
*                             IF ONE, UNIT (,B)
.SKP056A ANOP  ,                                                    DBC
&P.INVLA DS    AL3 -          ADDRESS OF INVITATION LIST
         SPACE 1
.C8B1    ANOP
         AIF   (NOT (&DSORGXA OR &DSORGXE)).C9
&LSW(057) SETB (1)                                                  DBC
*                       EXCP WITH EXTENSION OR APPENDAGES
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).C8C
&LSW(058) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .C8D
.C8C     ANOP
&LSW(059) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.C8D     ANOP
         AIF   (&DSORGQS OR &DSORGBS).C8E
&LSW(060) SETB (1)                                                  DBC
         AIF   (&#DCBSW(60) OR &#DCBSW(83)).SKP060A                 DBC
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,
*                             QSAM)
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH
*                             PREVIOUS, ON RECORD POSITION SENSING
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217
.SKP060A ANOP  ,                                                    DBC
.C8E     ANOP
         DS    XL7 -          RESERVED
         SPACE 1
         AIF   (NOT &DSORGXA).C9
&LSW(061) SETB (1)                                                  DBC
*                       EXCP APPENDAGE LIST
         SPACE 1
         ORG   &P.DCB+60
&P.EOEA  DS    CL2 -          END OF EXTENT APPENDAGE ID
&P.PCIA  DS    CL2 -          PROGRAM CONTROLLED INTERRUPTION
*                             APPENDAGE ID
&P.SIOA  DS    CL2 -          START I/O APPENDAGE ID
&P.CENDA DS    CL2 -          CHANNEL END APPENDAGE ID
&P.XENDA DS    CL2 -          ABNORMAL END APPENDAGE ID
         DS    XL2 -          RESERVED
         SPACE 1
.C9      AIF   (NOT &DSORGIS).CA
&LSW(062) SETB (1)                                                  DBC
*                       BISAM-QISAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.OPTCD DS    BL1 -          OPTION CODES
         AIF   (&DSORGQS OR &DSORGBS).C9A
&LSW(063) SETB (1)                                                  DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP063A  DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP063A ANOP  ,                                                    DBC
         AGO   .C9B
.C9A     ANOP
&LSW(064) SETB (1)                                                  DBC
*        BIT0 IS DCBOPTW - SAME AS BSAM
.C9B     ANOP
         AIF   (&#DCBSW(062)).SKP062A                               DBC
DCBOPTUF EQU   BIT1 -         FULL-TRACK INDEX WRITE
DCBOPTM  EQU   BIT2 -         MASTER INDEXES
DCBOPTI  EQU   BIT3 -         INDEPENDENT OVERFLOW AREA
DCBOPTY  EQU   BIT4 -         CYLINDER OVERFLOW AREA
DCBOPTL  EQU   BIT6 -         DELETE OPTION
DCBOPTR  EQU   BIT7 -         REORGANIZATION CRITERIA
.SKP062A ANOP  ,                                                    DBC
&P.MAC   DS    BL1 -          EXTENSION OF DCBMACRF FIELD FOR ISAM
         AIF   (&#DCBSW(062)).SKP062B                               DBC
DCBMACUR EQU   BIT4 -         UPDATE FOR READ
DCBMACUW EQU   BIT5 -         UPDATE TYPE OF WRITE
DCBMACAW EQU   BIT6 -         ADD TYPE OF WRITE
DCBMACRE EQU   BIT7 -         READ EXCLUSIVE
.SKP062B ANOP  ,                                                    DBC
&P.NTM   DS    FL1 -          NUMBER OF TRACKS THAT DETERMINE THE
*                             DEVELOPMENT OF A MASTER INDEX
*                             MAXIMUM PERMISSABLE VALUE - 99
&P.CYLOF DS    FL1 -          NUMBER OF TRACKS TO BE RESERVED ON EACH
*                             PRIME DATA CYLINDER FOR RECORDS THAT
*                             OVERFLOW FROM OTHER TRACKS ON THAT
*                             CYLINDER
&P.SYNAD DS    A -            ADDRESS OF USER'S SYNAD ROUTINE
&P.RKP   DS    H -            RELATIVE POSITION OF FIRST BYTE OF KEY
*                             WITHIN EACH LOGICAL RECORD
&P.BLKSI DS    H -            BLOCK SIZE
&P.LPDT  DS    0BL8 -         FOR RESUME LOAD,THE LAST PRIME DATA
*                             TRACK ON THE LAST PRIME DATA CYLINDER
*                             IN THE FORM MBBCCHHR.          ICBI DCB-5
&P.MSWA  DS    A -            ADDRESS OF MAIN STORAGE WORK AREA FOR USE
*                             BY CONTROL PROGRAM WHEN NEW RECORDS ARE
*                             BEING ADDED TO AN EXISTING DATA SET
&P.SMSI  DS    H -            NUMBER OF BYTES IN AREA RESERVED TO HOLD
*                             HIGHEST LEVEL INDEX
&P.SMSW  DS    H -            NUMBER OF BYTES IN WORK AREA USED BY
*                             CONTROL PROGRAM WHEN NEW RECORDS ARE
*                             BEING ADDED TO DATA SET
&P.MSHI  DS    0A -           ADDRESS OF MAIN STORAGE AREA TO HOLD
*                             HIGHEST LEVEL INDEX
&P.NCP   DS    FL1 -          NUMBER OF COPIES OF READ-WRITE (TYPE K)
*                             CHANNEL PROGRAMS THAT ARE TO BE
*                             ESTABLISHED FOR THIS DCB.  (99 MAXIMUM)
&P.MSHIA DS    AL3 -          SAME AS DCBMSHI ABOVE
&P.SETL  DS    A -            ADDRESS OF SETL MODULE FOR QISAM.
*                             ADDRESS OF CHECK MODULE FOR BISAM
&P.EXCD1 DS    BL1 -          FIRST BYTE IN WHICH EXCEPTIONAL
*                             CONDITIONS DETECTED IN PROCESSING DATA
*                             RECORDS ARE REPORTED TO THE USER
         AIF   (&#DCBSW(062)).SKP062C                               DBC
DCBEXNKY EQU   BIT0 -         LOWER KEY LIMIT NOT FOUND
DCBEXIDA EQU   BIT1 -         INVALID DEVICE ADDRESS FOR LOWER LIMIT
DCBEXNSP EQU   BIT2 -         SPACE NOT FOUND
DCBEXINV EQU   BIT3 -         INVALID REQUEST
DCBEXIER EQU   BIT4 -         UNCORRECTABLE INPUT ERROR
DCBEXOER EQU   BIT5 -         UNCORRECTABLE OUTPUT ERROR
DCBEXBLI EQU   BIT6 -         BLOCK COULD NOT BE REACHED (INPUT)
DCBEXBLU EQU   BIT7 -         BLOCK COULD NOT BE REACHED (UPDATE)
.SKP062C ANOP  ,                                                    DBC
&P.EXCD2 DS    BL1 -          SECOND BYTE IN WHICH EXCEPTIONAL
*                             CONDITIONS DETECTED IN PROCESSING DATA
*                             RECORDS ARE REPORTED TO THE USER
         AIF   (&#DCBSW(062)).SKP062D                               DBC
DCBEXSEQ EQU   BIT0 -         SEQUENCE CHECK
DCBEXDUP EQU   BIT1 -         DUPLICATE RECORD
DCBEXCLD EQU   BIT2 -         DCB CLOSED WHEN ERROR WAS DETECTED
DCBEXOFL EQU   BIT3 -         OVERFLOW RECORD
DCBEXLTH EQU   BIT4 -         FOR PUT - LENGTH FIELD OF RECORD LARGER
*                             THAN LENGTH INDICATED IN DCBLRECL
DCBEXRDE EQU   BIT4 -         READ EXCLUSIVE
.SKP062D ANOP  ,                                                    DBC
&P.LRECL DS    H -            FOR FIXED-LENGTH RECORD FORMATS, LOGICAL
*                             RECORD LENGTH.  FOR VARIABLE-LENGTH
*                             RECORD FORMATS, MAXIMUM LOGICAL RECORD
*                             LENGTH OR AN ACTUAL LOGICAL RECORD LENGTH
*                             CHANGED DYNAMICALLY BY USER WHEN CREATING
*                             THE DATA SET
&P.ESETL DS    A -            ADDRESS OF ESETL ROUTINE IN GET MODULE
&P.LRAN  DS    A -            ADDRESS OF READ-WRITE K MODULE OR
*                             EXCLUSIVE MODULE
&P.LWKN  DS    A -            ADDRESS OF WRITE KN MODULE
&P.RELSE DS    A -            WORK AREA FOR TEMPORARY STORAGE OF
*                             REGISTER CONTENTS
&P.PUTX  DS    A -            WORK AREA FOR TEMPORARY STORAGE OF
*                             REGISTER CONTENTS
&P.RELEX DS    A -            ADDRESS OF READ EXCLUSIVE MODULE
&P.FREED DS    A -            ADDRESS OF DYNAMIC BUFFERING MODULE
&P.HIRTI DS    FL1 -          NUMBER OF INDEX ENTRIES THAT FIT ON A
*                             PRIME DATA TRACK
&P.FTMI2 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF SECOND LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH)
&P.LEMI2 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN SECOND LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR)
&P.FTMI3 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF THIRD LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH)
&P.LEMI3 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN THIRD LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR)
&P.NLEV  DS    FL1 -          NUMBER OF LEVELS OF INDEX
&P.FIRSH DS    CL3 -          HHR OF FIRST DATA RECORD ON EACH
*                             CYLINDER.  FOR VARIABLE LENGTH RECORD
*                             PROCESSING, R PORTION OF THIS FIELD IS
*                             ALWAYS X'01'.
&P.HMASK DS    CL1 -          BYTE INDICATING 2301 OR NOT
         AIF   (&#DCBSW(062)).SKP062E                               DBC
DCBHMDRM EQU   X'07' -        DEVICE IS 2301 DRUM
DCBHMNDM EQU   X'FF' -        DEVICE IS OTHER THAN 2301 DRUM
.SKP062E ANOP  ,                                                    DBC
&P.LDT   DS    CL2 -          HH IS THE LAST PRIME DATA TRACK ON EACH
*                             CYLINDER
&P.HIRCM DS    CL1 -          HIGHEST POSSIBLE R FOR TRACKS OF THE
*                             CYLINDER AND MASTER INDICES
&P.HIRPD DS    CL1 -          HIGHEST R ON ANY PRIME TRACK IN DATA SET.
*                             FOR VARIABLE-LENGTH RECORDS, THIS
*                             REPRESENTS THE GREATEST NUMBER OF
*                             PHYSICAL RECORDS ON ANY PRIME TRACK IN
*                             THE DATA SET
&P.HIROV DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, HIGHEST
*                             POSSIBLE R FOR OVERFLOW DATA TRACKS.  FOR
*                             VARIABLE-LENGTH RECORD FORMAT, UNUSED.
&P.HIRSH DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, R OF LAST
*                             DATA RECORD ON A SHARED TRACK, IF
*                             APPLICABLE.  FOR VARIABLE-LENGTH RECORD
*                             FORMAT, UNUSED.
&P.TDC   DS    H -            USER-SUPPLIED NUMBER OF RECORDS TAGGED
*                             FOR DELETION.
&P.NCRHI DS    H -            NUMBER OF STORAGE LOCATIONS NEEDED TO
*                             HOLD THE HIGHEST LEVEL INDEX
&P.RORG3 DS    F -            FOR EACH USE OF DATA SET, NUMBER OF READ
*                             OR WRITE ACCESSES TO AN OVERFLOW RECORD
*                             WHICH IS NOT FIRST IN A CHAIN OF SUCH
*                             RECORDS
&P.NREC  DS    F -            NUMBER OF LOGICAL RECORDS IN PRIME DATA
*                             AREA
&P.ST    DS    BL1 -          STATUS INDICATORS
         AIF   (&#DCBSW(062)).SKP062F                               DBC
DCBSTSSM EQU   BIT0 -         SINGLE SCHEDULE MODE
DCBSTKSQ EQU   BIT1 -         KEY SEQUENCE CHECKING IS TO BE PERFORMED
DCBSTLOD EQU   BIT2 -         LOADING HAS COMPLETED.  SET TO 1 BY CLOSE
*                             ROUTINE AND TO 0 BY FIRST EXECUTION OF
*                             PUT ROUTINE.
DCBSTNCY EQU   BIT3 -         EXTENSION OF DATA SET WILL BEGIN ON NEW
*                             CYLINDER
DCBSTNMC EQU   BIT5 -         FIRST MACRO INSTRUCTION NOT YET RECEIVED
DCBSTLBF EQU   BIT6 -         LAST BLOCK FULL
DCBSTLTF EQU   BIT7 -         LAST TRACK FULL
.SKP062F ANOP  ,                                                    DBC
&P.FTCI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF CYLINDER INDEX (IN THE FORM
*                             MBBCCHH).
&P.HIIOV DS    CL1 -          FOR FIXED LENGTH RECORD FORMAT, HIGHEST
*                             POSSIBLE R FOR INDEPENDENT OVERFLOW DATA
*                             TRACKS.  FOR VARIABLE LENGTH RECORD
*                             FORMAT, UNUSED
&P.FTMI1 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF FIRST LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH).
&P.NTHI  DS    FL1 -          NUMBER OF TRACKS OF HIGH-LEVEL INDEX
&P.FTHI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF HIGHEST LEVEL INDEX (IN THE
*                             FORM MBBCCHH).
&P.LPDA  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             PRIME DATA RECORD IN PRIME DATA AREA
*                             (IN THE FORM MBBCCHHR).
&P.LETI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE NORMAL ENTRY OF TRACK INDEX ON
*                             LAST ACTIVE CYLINDER (IN THE FORM CCHHR).
&P.OVDEV DS    CL1 -          DEVICE TYPE FOR INDEPENDENT OVERFLOW
         AIF   (&#DCBSW(062)).SKP062G                               DBC
*        THESE SAME MASKS APPLY TO DCBDEVT FOR ISAM DIRECT ACCESS
DCBDVI11 EQU   X'01' -        2311 DISK DRIVE
DCBDVI01 EQU   X'02' -        2301 PARALLEL DRUM
DCBDVI03 EQU   X'03' -        2303 SERIAL DRUM
DCBDVI02 EQU   X'04' -        2302 DISK STORAGE
DCBDVI21 EQU   X'05' -        2321 DATA CELL DRIVE
DCBDVI14 EQU   X'08' -        2314 DISK STORAGE FACILITY
.SKP062G ANOP  ,                                                    DBC
&P.NBOV  DS    H -            FOR FIXED LENGTH RECORD FORMAT, RESERVED.
*                             FOR VARIABLE LENGTH RECORD FORMAT, IF THE
*                             INDEPENDENT OVERFLOW OPTION IS SELECTED,
*                             CONTAINS, IN BINARY, NUMBER OF BYTES LEFT
*                             ON CURRENT TRACK OF INDEPENDENT OVERFLOW
*                             AREA
&P.LECI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN CYLINDER INDEX (IN THE
*                             FORM CCHHR).
         DS    X -            RESERVED
&P.RORG2 DS    H -            NUMBER OF TRACKS (PARTIALLY OR WHOLLY)
*                             REMAINING IN INDEPENDENT OVERFLOW AREA
&P.LEMI1 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN FIRST LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR).
         DS    X -            RESERVED
&P.NOREC DS    H -            NUMBER OF LOGICAL RECORDS IN AN OVERFLOW
*                             AREA
&P.LIOV  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             AREA (IN THE FORM MBBCCHHR).
&P.RORG1 DS    H -            NUMBER OF CYLINDER OVERFLOW AREAS THAT
*                             ARE FULL
         DS    XL2 -          RESERVED
&P.WKPT1 DS    A -            POINTER TO WORK AREA OR TO CONSTRUCTED
*                             CHANNEL PROGRAM FOR WHICH SPACE IS
*                             OBTAINED BY GETMAIN MACRO INSTRUCTIONS
*                             ISSUED BY OPEN EXECUTORS
&P.WKPT2 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT3 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT4 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT5 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT6 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
         SPACE 1
.CA      AIF   (NOT &DSORGDA).CB
&LSW(065) SETB (1)                                                  DBC
*                       BDAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CA1
&LSW(066) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CA2
.CA1     ANOP
&LSW(067) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CA2     ANOP
         AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS).CA3
&LSW(068) SETB (1)                                                  DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP068A  DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP068A ANOP  ,                                                    DBC
         AGO   .CA4
.CA3     ANOP
&LSW(069) SETB (1)                                                  DBC
*        BIT0 IS DCBOPTW - SAME AS BSAM AND ISAM
.CA4     ANOP
         AIF   (&#DCBSW(065)).SKP065A                               DBC
DCBOPTTO EQU   BIT1 -         TRACK OVERFLOW
DCBOPTE  EQU   BIT2 -         EXTENDED SEARCH
DCBOPTF  EQU   BIT3 -         FEEDBACK
DCBOPTA  EQU   BIT4 -         ACTUAL ADDRESSING
DCBOPTDB EQU   BIT5 -         DYNAMIC BUFFERING
DCBOPTRE EQU   BIT6 -         READ EXCLUSIVE
DCBOPTRB EQU   BIT7 -         RELATIVE BLOCK ADDRESSING
.SKP065A ANOP  ,                                                    DBC
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CAA
&LSW(070) SETB (1)                                                  DBC
&P.SYNAD DS    A -            ADDRESS OF SYNAD ROUTINE
         DS    XL2 -          RESERVED
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE
         AGO   .CAB
.CAA     ANOP
&LSW(071) SETB (1)                                                  DBC
         DS    A -            DCBSYNAD - ADDRESS OF SYNAD ROUTINE
         DS    XL2 -          RESERVED
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE
.CAB     ANOP
&P.IOBSQ DS    A -            ADDRESS OF FIRST IOB ON UNSCHEDULED QUEUE
*                             FOR EITHER A WRITE-ADD REQUEST WHEN
*                             ANOTHER WRITE-ADD IS IN PROGRESS OR A
*                             READ-EXCLUSIVE REQUEST WHEN THE
*                             READ-EXCLUSIVE LIST IS FULL
&P.SQND  DS    A -            ADDRESS OF LAST IOB ON UNSCHEDULED QUEUE
&P.IOBUQ DS    A -            ADDRESS OF FIRST IOB ON UNPOSTED QUEUE
&P.UQND  DS    A -            ADDRESS OF LAST JOB ON UNPOSTED QUEUE
*                             THAT IS MAINTAINED BY THE READ EXCLUSIVE
*                             MODULE
         DS    X -            RESERVED
&P.LIMCT DS    FL3 -          NUMBER OF TRACKS OR NUMBER OF RELATIVE
*                             BLOCKS TO BE SEARCHED (EXTENDED SEARCH
*                             OPTION)
&P.XARG  DS    0A -           ADDRESS OF READ EXCLUSIVE LIST
&P.XCNT  DS    FL1 -          NUMBER OF ENTRIES IN READ EXCLUSIVE LIST
&P.XARGA DS    AL3 -          ADDRESS OF READ EXCLUSIVE LIST
&P.DRDX  DS    0A -           ADDRESS OF READ EXCLUSIVE MODULE
&P.MVXNO DS    FL1 -          TOTAL NUMBER OF EXTENTS IN MULTIVOLUME
*                             DATA SET
&P.DRDXA DS    AL3 -          ADDRESS OF READ EXCLUSIVE MODULE
&P.DFOR  DS    A -            ADDRESS OF A FORMAT MODULE
&P.DFBK  DS    A -            ADDRESS OF A FEEDBACK MODULE
&P.DYNB  DS    A -            FOR DYNAMIC BUFFERING, ADDRESS OF DYNAMIC
*                             BUFFER MODULE.  FOR UNBLOCKED SPANNED
*                             RECORDS WITH BFTEK=R SPECIFIED AND NO
*                             DYNAMIC BUFFERING, ADDRESS OF SEGMENT
*                             WORK AREA CONTROL BLOCK
         SPACE 1
.CB      AIF   (NOT &DSORGQX).CC
&LSW(072) SETB (1)                                                  DBC
*                       QTAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.KSTAT DS    0CL4 -         FOUR THRESHOLD VALUES FOR ERROR COUNTS
&P.KSTA1 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF
*                             TRANSMISSIONS
&P.KSTA2 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF DATA CHECKS
&P.KSTA3 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF
*                             INTERVENTIONS REQUIRED
&P.KSTA4 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF TIMEOUTS
         SPACE 1
*                       QTAM POLLING LIST ORIGIN
         SPACE 1
&P.CPOLL DS    0A -           A 4-BYTE FIELD FOR EACH POLLING LIST
&P.PLBYT DS    BL1 -          ADAPTER TYPE
         AIF   (&#DCBSW(072)).SKP072A                               DBC
DCBCPWTT EQU   BIT4 -         WTTA
.SKP072A ANOP  ,                                                    DBC
&P.CPOLA DS    AL3 -          ADDRESS OF THE POLLING LIST
         SPACE 1
.CC      AIF   (NOT &DSORGTQ).CC1
&LSW(073) SETB (1)                                                  DBC
*                       TCAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1A
&LSW(074) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CC1B
.CC1A    ANOP
&LSW(075) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CC1B    ANOP
         AIF   (&#DCBSW(073)).SKP073A                               DBC
DCBOPTWP EQU   BIT0 -         SOURCE OR DESTINATION NAME PRECEDES
*                             MESSAGE (AFTER CONTROL BYTE)
*                             (TCAM PROCESS QUEUE)
DCBOPTUM EQU   BIT1 -         WORK UNIT IS A MESSAGE.  DEFAULT WORK
*                             UNIT IS A RECORD.  (TCAM PROCESS QUEUE)
DCBOPTCB EQU   BIT2 -         CONTROL BYTE PRECEDES WORK UNIT
*                             (TCAM PROCESS QUEUE)
DCBOPTCP EQU   BIT2 -         CHECKPOINT DATA SET
DCBOPTIM EQU   BIT6 -         NON-REUSABLE MESSAGE QUEUE DATA SET
DCBOPTRM EQU   BIT7 -         REUSABLE MESSAGE QUEUE DATA SET
.SKP073A ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1D
&LSW(076) SETB (1)                                                  DBC
*                                                            ICBI DCB-9
         DS    XL9 -           RESERVED                      ICBI DCB-9
&P.BLKSI DS    H -             BLOCK SIZE                    ICBI DCB-9
         AGO   .CC1E                                         ICBI DCB-9
.CC1D    ANOP                                                ICBI DCB-9
&LSW(077) SETB (1)                                                  DBC
         DS    XL11 -         RESERVED
.CC1E    ANOP                                                ICBI DCB-9
         SPACE 1
.CC1     ANOP
         AIF   (NOT &DSORGMQ).CD
&LSW(078) SETB (1)                                                  DBC
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.RECRD DS    A -            NOT USED BY QTAM
         AIF   (&DSORGIS OR &DSORGDA).CCA
&LSW(079) SETB (1)                                                  DBC
&P.SYNAD DS    A -            ADDRESS OF USER-PROVIDED SYNAD ROUTINE TO
*                             BE ENTERED IF A WORK UNIT IS LONGER THAN
*                             THE WORK AREA PROVIDED FOR INPUT
         AGO   .CCB
.CCA     ANOP
&LSW(080) SETB (1)                                                  DBC
         DS    A -            DCBSYNAD - ADDRESS OF USER-PROVIDED SYNAD
*                             ROUTINE TO BE ENTERED IF A WORK UNIT IS
*                             LONGER THAN THE WORK AREA PROVIDED FOR
*                             INPUT
.CCB     ANOP
&P.EOBLK DS    A -            NOT USED BY QTAM
         SPACE 1
.CD      AIF   (NOT &DSORGBX).CDF
&LSW(081) SETB (1)                                                  DBC
*                       BTAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.LERB  DS    A -            ADDRESS OF LINE ERROR BLOCK
         SPACE 1
         AIF   (NOT &DEVDBS).CDF
&LSW(082) SETB (1)                                                  DBC
*                       BSC INTERFACE
         SPACE 1
         ORG   &P.DCB+56
&P.XMODE DS    BL1 -          MODE OF TRANSMISSION FOR BINARY
*                             SYNCHRONOUS COMMUNICATION (BSC)
         AIF   (&#DCBSW(082)).SKP082A                               DBC
DCBXMIBC EQU   BIT1 -         INTERMEDIATE BLOCK CHECKING IS TO BE
*                             PERFORMED
DCBXMDA1 EQU   BIT2 -         TRANSMISSION IS THROUGH A 2701 DATA
*                             ADAPTER UNIT DUAL COMMUNICATION
*                             INTERFACE B
DCBXMDA2 EQU   BIT4 -         TRANSMISSION IS IN CODE B FOR A 2701
*                             DATA ADAPTER UNIT DUAL CODE FEATURE
.SKP082A ANOP  ,                                                    DBC
&P.XCODE DS    BL1 -          BSC CONTROL STATION FLAG AND
*                             TRANSMISSION CODE
         AIF   (&#DCBSW(082)).SKP082B                               DBC
DCBXCCSF EQU   BIT0 -         BSC CONTROL STATION FLAG ---
*                             IF ZERO, THIS IS THE CONTROL STATION.
*                             IF ONE, THIS IS THE REMOTE STATION.
DCBXCPTP EQU   BIT1 -         IF PTOP IS SPECIFIED IN SYSGEN PROCEDURE
*                             - SCHEDULE AN ASYNCHRONOUS EXIT TO
*                             INTERFACE RESOLUTION ROUTINE
DCBXCTR1 EQU   BIT2 -         6-BIT TRANSCODE IS BEING USED (BIT 4 IS
*                             ALSO ON)
DCBXCAS1 EQU   BIT3 -         USASCII TRANSMISSION CODE IS BEING USED
*                             (BIT 5 IS ALSO ON)
DCBXCEBC EQU   BIT4+BIT5 -    IF BOTH BITS ARE ZERO, EBCDIC
*                             TRANSMISSION CODE IS BEING USED.
DCBXCTR2 EQU   BIT4 -         6-BIT TRANSCODE IS BEING USED (BIT 2 IS
*                             ALSO ON)
DCBXCAS2 EQU   BIT5 -         USASCII TRANSMISSION CODE IS BEING USED
*                             (BIT 3 IS ALSO ON)
.SKP082B ANOP  ,                                                    DBC
&P.BSRSV DS    CL1 -          DLE CONTROL CHARACTER
&P.BSWBT DS    X -            RESERVED
&P.IRRAD DS    0A -           BEFORE OPEN - IF PTOP IS SPECIFIED IN THE
*                             SYSGEN PROCEDURE, ADDRESS OF INTERFACE
*                             RESOLUTION ROUTINE.
*                             AFTER OPEN, THE FOLLOWING 4 CHARACTERS
*                             OCCUPY THIS SPACE.
&P.BSTSX DS    CL1 -          DLE CONTROL CHARACTER
&P.BSSTX DS    CL1 -          STX CONTROL CHARACTER
&P.BSTEX DS    CL1 -          DLE CONTROL CHARACTER
&P.BSETX DS    CL1 -          ETX CONTROL CHARACTER
&P.BSAK0 DS    CL2 -          ACK-0 CONTROL CHARACTER
&P.BSAK1 DS    CL2 -          ACK-1 CONTROL CHARACTER
&P.BSENQ DS    CL1 -          ENQ CONTROL CHARACTER
&P.BSNAK DS    CL1 -          NAK CONTROL CHARACTER
&P.BSETB DS    CL1 -          ETB CONTROL CHARACTER
&P.BSDLE DS    CL1 -          DLE CONTROL CHARACTER
&P.BSEOT DS    CL1 -          EOT CONTROL CHARACTER
&P.BSSYN DS    CL3 -          SYN, SYN, SYN CONTROL CHARACTERS
&P.BSONL DS    CL2 -          SOH % CONTROL CHARACTERS
&P.BSSAK DS    CL2 -          WACK CONTROL CHARACTERS
&P.BSRVI DS    CL2 -          DLE @ CONTROL CHARACTERS
         DS    XL18 -         RESERVED
         SPACE 1
.CDF     AIF   (NOT (&DSORGQS OR &DSORGBS)).FIN
&LSW(083) SETB (1)                                                  DBC
*                       QSAM-BSAM-BPAM COMMON INTERFACE
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGDA).CDA1
&LSW(084) SETB (1)                                                  DBC
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CDA
&LSW(085) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CD2
.CDA1    ANOP
&LSW(086) SETB (1)                                                  DBC
         DS    0A -           DCBGERR, DCBPERR OR DCBCHECK
.CDA     ANOP
&LSW(087) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CD2     ANOP
         AIF   (&#DCBSW(083)).SKP083C                               DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68)).SKP083A                 DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP083A ANOP  ,                                                    DBC
DCBOPTU  EQU   BIT1 -         ALLOW DATA CHECK CAUSED BY INVALID
*                             CHARACTER (1403 PRINTER WITH UCS FEATURE)
*                             (BSAM, BPAM, QSAM)
DCBOPTC  EQU   BIT2 -         CHAINED SCHEDULING USING PCI
*                             (BSAM, BPAM, QSAM)
DCBOPTH  EQU   BIT3 -         1287/1288 OPTICAL READER - HOPPER EMPTY
*                             EXIT (BSAM, BPAM)
DCBOPTO  EQU   BIT3 -         1285/1287 OPTICAL READER - ON-LINE
*                             CORRECTION (QSAM)
DCBBCKPT EQU   BIT3 -         CHANNEL-END APPENDAGE IS TO BYPASS DOS
*                             EMBEDDED CHECKPOINT RECORDS ON TAPE
*                             (BSAM, QSAM)                       ICB226
DCBOPTQ  EQU   BIT4 -         TRANSLATION TO OR FROM ASCII
*                             (BSAM, BPAM, QSAM)
         AIF   (&#DCBSW(060)).SKP083B                               DBC
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,
*                             QSAM)
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH
*                             PREVIOUS, ON RECORD POSITION SENSING
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217
.SKP083B ANOP  ,                                                    DBC
DCBOPTT  EQU   BIT6 -         USER TOTALING (BSAM, QSAM)
.SKP083C ANOP  ,                                                    DBC
         AIF   (&DSORGDA).CD1
&LSW(088) SETB (1)                                                  DBC
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE
         AGO   .CD3
.CD1     ANOP
&LSW(089) SETB (1)                                                  DBC
         DS    AL3 -          DCBGERRA, DCBPERRA OR DCBCHCKA
.CD3     AIF   (&DSORGIS OR &DSORGDA OR &DSORGMQ).CDB
&LSW(090) SETB (1)                                                  DBC
&P.SYNAD DS    0A -           ADDRESS OF USER-PROVIDED SYNAD ROUTINE
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS
&P.SYNA  DS    AL3 -          ADDRESS OF USER-PROVIDED SYNAD ROUTINE
         AGO   .CD4
.CDB     ANOP
&LSW(091) SETB (1)                                                  DBC
         DS    0A -           DCBSYNAD - ADDRESS OF SYNAD ROUTINE
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS
         DS    AL3 -          DCBSYNA - ADDRESS OF SYNAD ROUTINE
.CD4     ANOP
&P.FLAG1 DS    0BL1           TCAM APPLICATION PROGRAM FLAGS ICBI DCB-3
*                             (BSAM, BPAM, QSAM)
&P.CIND1 DS    BL1 -          CONDITION INDICATORS
         AIF   (&#DCBSW(083)).SKP083D                               DBC
DCBCNTOV EQU   BIT0 -         DIRECT ACCESS - TRACK OVERFLOW IN USE
*                             (BSAM, BPAM, QSAM)
*                             2540 CARD PUNCH - DATA SET WAS OPENED BUT
*                             NO DATA WAS WRITTEN (QSAM)
DCBSTQCK EQU   BIT0 -         STOP EQUAL QUICK WAS SPECIFIED FOR
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3
DCBSTFLS EQU   BIT1 -         STOP EQUAL FLUSH WAS SPECIFIED FOR
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3
DCBCNSRD EQU   BIT1 -         SEARCH DIRECT (BSAM, BPAM, QSAM)
DCBCNEVB EQU   BIT2 -         END OF VOLUME - USED BY EOB ROUTINES
*                             (BSAM, BPAM, QSAM)
DCBCNEVA EQU   BIT3 -         END OF VOLUME - USED BY CHANNEL-END
*                             APPENDAGE ROUTINES (BSAM, BPAM, QSAM)
DCBCNBRM EQU   BIT5 -         BLOCKED RECORD BIT MODIFIED (BSAM,BPAM,
*                             QSAM)                          ICBI DCB-2
DCBCNEXB EQU   BIT7 -         EXCHANGE BUFFERING SUPPORTED (QSAM)
.SKP083D ANOP  ,                                                    DBC
&P.CIND2 DS    BL1 -          CONDITION INDICATORS
         AIF   (&#DCBSW(083)).SKP083E                               DBC
DCBCNSTO EQU   BIT0 -         PARTITIONED DATA SET - STOW HAS BEEN
*                             PERFORMED (BSAM, BPAM, QSAM)
*                             SEQUENTIAL DATA SET - UPDATE (BSAM, BPAM)
DCBCNWR0 EQU   BIT1 -         DIRECT ORGANIZATION DATA SET - LAST I/O
*                             WAS A WRITE RECORD ZERO
*                             (BSAM, BPAM, QSAM)
*                             SEQUENTIAL DATA SET - UPDATE EOF IS
*                             INDICATED (BSAM, BPAM)
DCBCNCLO EQU   BIT2 -         CLOSE IN PROCESS (QSAM)
DCBCNIOE EQU   BIT3 -         PERMANENT I/O ERROR (BSAM, BPAM, QSAM)
DCBCNBFP EQU   BIT4 -         OPEN ACQUIRED BUFFER POOL
*                             (BSAM, BPAM, QSAM)
DCBCNCHS EQU   BIT5 -         CHAINED SCHEDULING BEING SUPPORTED
*                             (BSAM, BPAM, QSAM)
DCBCNFEO EQU   BIT6 -         FEOV BIT (BSAM, BPAM, QSAM)
DCBCNQSM EQU   BIT7 -         ALWAYS ZERO (BSAM, BPAM)
*                             THIS IS A QSAM DCB (QSAM)
.SKP083E ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGDA).CDC
&LSW(092) SETB (1)                                                  DBC
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE
         AGO   .CD7
.CDC     ANOP
&LSW(093) SETB (1)                                                  DBC
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE
.CD7     ANOP
&P.WCPO  DS    AL1 -          OFFSET OF WRITE CHANNEL PROGRAM FROM THE
*                             START OF IOB
&P.WCPL  DS    FL1 -          LENGTH OF WRITE CHANNEL PROGRAM
&P.OFFSR DS    AL1 -          OFFSET OF READ CCW FROM BSAM/BPAM PREFIX
*                             OF IOB
&P.OFFSW DS    AL1 -          OFFSET OF WRITE CCW FROM BSAM/BPAM PREFIX
*                             OF IOB
&P.IOBA  DS    A -            FOR NORMAL SCHEDULING, ADDRESS OF QSAM OR
*                             BSAM/BPAM PREFIX OF IOB.  FOR CHAINED
*                             SCHEDULING, ADDRESS OF ICB.  FOR
*                             1419/1275, ADDRESS OF MAGNETIC INTERRUPT
*                             CONTROL BLOCK (MICB) CURRENTLY BEING
*                             PROCESSED BY READ ROUTINE.  FOR TSO
*                             TERMINAL DATA SET OPENED FOR INPUT AND
*                             FORMAT U, SIMULATED LOW-ORDER FOUR BYTES
*                             OF IOBCSW
         SPACE 1
         ORG   &P.DCB+68                                         ICB354
&P.CICB  DS    0A -           SAME AS DCBCICBA BELOW             ICB354
         DS    X -            DCBNCP  (BSAM,BPAM)                ICB354
&P.CICBA DS    AL3 -          POINTER TO JES C.I.                ICB354
*                             CONTROL BLOCK (CICB)               ICB354
         SPACE 1
         ORG   &P.DCB+80                                     ICBI DCB-4
&P.DIRCT DS    0H -           NUMBER OF BYTES USED IN LAST DIRECTORY
*                             BLOCK (RANGE 0-254)  (BSAM, BPAM)  ICB295
&P.QSWS  DS    0BL1 -         FLAG BYTE                          ICB295
&P.USASI DS    B -            FLAG BYTE FOR ASCII TAPES
         AIF   (&#DCBSW(083)).SKP083F                               DBC
DCBBLBP  EQU   BIT1 -         BLOCK PREFIX IS FOUR BYTE FIELD
*                             CONTAINING BLOCK LENGTH IN UNPACKED
*                             DECIMAL (SPECIFIED BY BUFFER=L).
DCBQADFS EQU   BIT2+BIT3+BIT4 USED TO PERFORM SEQUENCE CHECKING WITH
*                             MULTIPLE FUNCTION SUPPORT FOR 3525
*                             (BSAM, QSAM)
DCBQADF1 EQU   BIT2 -         FIRST BIT OF DCBQADFS
DCBQADF2 EQU   BIT3 -         SECOND BIT OF DCBQADFS
DCBQADF3 EQU   BIT4 -         THIRD BIT OF DCBQADFS
DCBQSTRU EQU   BIT7 -         TRUNC ENTRY POINT ENTERED (QSAM)
.SKP083F ANOP  ,                                                    DBC
&P.BUFOF DS    0FL1 -         BLOCK PREFIX LENGTH (0-99), SPECIFIED BY
*                             BUFOFF=N OR BUFOFF=L
&P.DIRCQ DS    FL1 -          NUMBER OF BYTES USED IN LAST DIRECTORY
*                             BLOCK (RANGE 0-254)  (QSAM)        ICB295
         SPACE 1
         AIF   (NOT &DSORGBS).CE
&LSW(094) SETB (1)                                                  DBC
*                       BSAM-BPAM INTERFACE
         SPACE 1
         ORG   &P.DCB+72
&P.EOBR  DS    0A -           ADDRESS OF END-OF-BLOCK MODULE FOR READ
         AIF   (&DSORGIS).CDD
&LSW(095) SETB (1)                                                  DBC
&P.NCP   DS    FL1 -          NUMBER OF CHANNEL PROGRAMS.
         AGO   .CD8
.CDD     ANOP
&LSW(096) SETB (1)                                                  DBC
         DS    FL1 -          DCBNCP - NUMBER OF CHANNEL PROGRAMS.
.CD8     ANOP
*                             NUMBER OF READ OR WRITE REQUESTS WHICH
*                             MAY BE ISSUED PRIOR TO A CHECK, NUMBER
*                             OF IOB'S GENERATED.  (99 MAXIMUM)
&P.EOBRA DS    AL3 -          ADDRESS OF END-OF-BLOCK MODULE FOR READ
&P.EOBW  DS    A -            ADDRESS OF END-OF-BLOCK MODULE FOR WRITE.
*                             FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS WITH BKTEK=R
*                             SPECIFIED, ADDRESS OF SEGMENT WORK AREA
*                             CONTROL BLOCK
         DS    H -            DCBDIRCT - NUMBER OF BYTES USED IN LAST
*                             DIRECTORY BLOCK  (RANGE 0-254)     ICB295
         AIF   (&DSORGIS).CDE
&LSW(097) SETB (1)                                                  DBC
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         AGO   .CD9
.CDE     ANOP
&LSW(098) SETB (1)                                                  DBC
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH
.CD9     ANOP
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE
&P.NOTE  DS    0A -           ADDRESS OF NOTE/POINT MODULE
&P.POINT DS    A -            ADDRESS OF NOTE/POINT MODULE
         SPACE 1
.CE      AIF   (NOT &DSORGQS).FIN
&LSW(099) SETB (1)                                                  DBC
*                       QSAM INTERFACE
         SPACE 1
         AIF   (NOT &DSORGDA).CE1
&LSW(100) SETB (1)                                                  DBC
         ORG   &P.DCB+52
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
         DS    BL1 -          DCBOPTCD - OPTION CODES
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERRA DS    AL3 -          ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
         SPACE 1
.CE1     ANOP
         ORG   &P.DCB+72
&P.LCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF LAST
*                             CCW IN LIST
&P.EOBAD DS    A -            FOR SIMPLE BUFFERING, ADDRESS OF LAST
*                             BYTE OF CURRENT BUFFER
&P.CCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF
*                             CURRENT OR NEXT CCW
&P.RECAD DS    0A -           ADDRESS OF CURRENT OR NEXT LOGICAL RECORD
&P.RECBT DS    BL1 -          FLAG BYTE
         AIF   (&#DCBSW(099)).SKP099A                               DBC
DCBRCREL EQU   BIT0+BIT1+BIT2+BIT3 RELSE MACRO HAS BEEN ISSUED
*                             (QSAM WITH SIMPLE BUFFERING)
DCBRCTRU EQU   BIT0 -         TRUNC MACRO HAS BEEN ISSUED (QSAM LOCATE
*                             MODE)
DCBRCFGT EQU   BIT1 -         FIRST GET AFTER OPEN (QSAM LOCATE MODE)
.SKP099A ANOP  ,                                                    DBC
&P.RECA  DS    AL3 -          ADDRESS OF CURRENT OR NEXT LOGICAL RECORD
         DS    B -            DCBQSWS - FLAG BYTE                ICB295
         DS    FL1 -          DCBDIRCQ - NUMBER OF BYTES USED IN LAST
*                             DIRECTORY BLOCK (RANGE 0-254)      ICB295
         AIF   (&DSORGIS OR &DSORGBS).CEE
&LSW(101) SETB (1)                                                  DBC
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         AGO   .CEEA
.CEE     ANOP
&LSW(102) SETB (1)                                                  DBC
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH
.CEEA    AIF   (&DSORGBS).CEF
&LSW(103) SETB (1)                                                  DBC
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE
         AGO   .CEF1
.CEF     ANOP
&LSW(104) SETB (1)                                                  DBC
         DS    0A -           DCBCNTRL - ADDRESS OF CNTRL MODULE
.CEF1    ANOP
&P.EROPT DS    BL1 -          ERROR OPTION
         AIF   (&#DCBSW(099)).SKP099B                               DBC
DCBERACC EQU   BIT0 -         ACCEPT PERMANENT ERROR
DCBERSKP EQU   BIT1 -         SKIP PERMANENT ERROR
DCBERABE EQU   BIT2 -         ABNORMAL END OF TASK
.SKP099B ANOP  ,                                                    DBC
         AIF   (&DSORGBS).CEF2
&LSW(105) SETB (1)                                                  DBC
&P.CNTRA DS    AL3 -          ADDRESS OF CNTRL MODULE
         AGO   .CEF3
.CEF2    ANOP
&LSW(106) SETB (1)                                                  DBC
         DS    AL3 -          DCBCNTRA - ADDRESS OF CNTRL MODULE
.CEF3    ANOP
         DS    XL2 -          RESERVED
&P.PRECL DS    H -            BLOCK LENGTH, MAXIMUM BLOCK LENGTH OR
*                             DATA LENGTH
&P.EOB   DS    A -            ADDRESS OF END OF BLOCK MODULE
         SPACE 1
.CF      AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS OR NOT &DSORGLR).FIN
&LSW(107) SETB (1)                                                  DBC
         ORG   &P.DCB+82
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         SPACE 1
         AGO   .FIN
.SLIP    ANOP
&LSW(108) SETB (1)                                                  DBC
*                       GRAPHIC DEVICE INTERFACE
         SPACE 1
&P.DCB   DS    0A                  FULLWORD ALIGNMENT               DBC
         DS    XL12 -         RESERVED
&P.BRSA  DS    AL2 -          BUFFER RESTART ADDRESS.  BLANK BEFORE
*                             EXECUTION OF SECOND I/O OPERATION
&P.GTYPE DS    CL1 -          TYPE OF BUFFER MANAGEMENT AND ATTENTION
*                             HANDLING
         AIF   (&#DCBSW(108)).SKP108A                               DBC
DCBGTEXP EQU   X'00' -        EXPRESS
DCBGTBAS EQU   X'01' -        BASIC
.SKP108A ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.BFRST DS    AL2 -          BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             STARTING ADDRESS FOR BUFFER AFTER
*                             EXECUTION OF OPEN ROUTINE
&P.BFRSZ DS    H -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             SIZE OF BUFFER AFTER EXECUTION OF OPEN
*                             ROUTINE.
         SPACE 1
*                       COMMON INTERFACE
         SPACE 1
         DS    XL6 -          RESERVED
&P       #DSORG                                                     DBC
&P.IOBAD DS    A -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             ADDRESS OF STANDARD FIELDS OF FIRST IOB
*                             AFTER EXECUTION OF OPEN ROUTINE
         SPACE 1
*                       FOUNDATION EXTENSION
         SPACE 1
&P.POLST DS    0A -           ADDRESS OF AREA WHERE A DCB LIST IS TO BE
*                             CONSTRUCTED FOR POLLING PURPOSES
&P.GNCP  DS    FL1 -          NUMBER OF I/O INSTRUCTIONS TO BE ISSUED
*                             BEFORE A WAIT MACRO INSTRUCTION
&P.POLSA DS    AL3 -          SAME AS DCBPOLST ABOVE
&P.EXLST DS    0A -           ADDRESS OF USER'S EXIT LIST
         DS    X -            RESERVED
&P.EXLSA DS    AL3 -          ADDRESS OF USER'S EXIT LIST
         SPACE 1
*                       FOUNDATION BEFORE OPEN
         SPACE 1
&P.DDNAM DS    CL8 -          8-BYTE NAME FROM DD STATEMENT THAT
*                             DEFINES DATA SET ASSOCIATED WITH THIS DCB
&P.OFLG  DS    BL1 -          FLAGS USED BY OPEN ROUTINE
         AIF   (&#DCBSW(108)).SKP108B                               DBC
DCBOFGRW EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS GREAD.
*                             IF ONE, LAST I/O OPERATION WAS GWRITE.
         AIF   (&#DCBSW(052)).SKP108B                               DBC
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE
*                             ROUTINE FOR CONCATENATION OF DATA SETS
*                             WITH UNLIKE ATTRIBUTES
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A
*                             CONCATENATION OF UNLIKE ATTRIBUTES
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1
*                             ON RETURN FROM USER EXIT TO THE I/O
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION
.SKP108B ANOP  ,                                                    DBC
&P.IFLG  DS    BL1 -          SET TO ZERO BY GRAPHIC ROUTINES BUT USED
*                             BY IOS IN COMMUNICATING ERROR CONDITIONS
*                             AND IN DETERMINING CORRECTIVE PROCEDURES
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108C                DBC
DCBMRRD  EQU   BIT2 -         READ
DCBMRCRL EQU   BIT6 -         CNTRL
.SKP108C ANOP  ,                                                    DBC
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108D                DBC
DCBMRWRT EQU   BIT2 -         WRITE
DCBMRCTL EQU   BIT6 -         CNTRL
.SKP108D ANOP  ,                                                    DBC
         SPACE 1
*                       FOUNDATION AFTER OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.TIOT  DS    AL2 -          OFFSET FROM TIOT ORIGIN TO DD ENTRY
*                             ASSOCIATED WITH THIS DCB
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB
&P.GIOCR DS    0A -           ADDRESS OF GRAPHICS I/O CONTROL ROUTINE
&P.OFLGS DS    BL1 -          SAME AS DCBOFLG BEFORE OPEN
&P.GIOCA DS    AL3 -          ADDRESS OF GRAPHICS I/O CONTROL ROUTINE
         SPACE 1
.FIN     ANOP
&A0      SETA  0                                                    DBC
.LP1     AIF   (&A0 EQ 150).EXIT                                    DBC
&A0      SETA  &A0+1                                                DBC
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &LSW(&A0))                       DBC
         AGO   .LP1                                                 DBC
.EXIT    ANOP  ,                                                    DBC
         MEND
         MACRO
&N       #DSORG &D
.*
.*
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DSORG TO #DSORG.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - APRIL 1, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       809 WHITNEY AVE.
.*       NEW HAVEN, CT. 06511
.*
.*
.*
.*   THIS MACRO WAS WRITTEN TO BE AN INNER MACRO FOR THE #DCBD MACRO.
.* ITS SOLE PURPOSE IS TO GENERATE DSORG FIELD AND BIT NAMES FOR THE
.* #DCBD MACRO.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLB  &#DCBDSG
         GBLB  &#DCBSW(150)
         LCLC  &P
&P       SETC  'DCB'
         AIF   ('&N' EQ '').GOTPFIX
&P       SETC  '&N'
.GOTPFIX ANOP
&P.DSORG DS    0BL2 -         DATA SET ORGANIZATION BEING USED
&P.DSRG1 DS    BL1 -          FIRST BYTE OF DCBDSORG
         AIF   (&#DCBDSG).SKP1
DCBDSGIS EQU   BIT0 -         IS - INDEXED SEQUENTIAL ORGANIZATION
DCBDSGPS EQU   BIT1 -         PS - SHYSICAL SEQUENTIAL ORGANIZATION
DCBDSGDA EQU   BIT2 -         DA - DIRECT ORGANIZATION
DCBDSGCX EQU   BIT3 -         CX - BTAM OR QTAM LINE GROUP
DCBDSGCQ EQU   BIT4 -         CQ - QTAM DIRECT ACCESS MESSAGE QUEUE
DCBDSGMQ EQU   BIT5 -         MQ - QTAM PROBLEM PROGRAM MESSAGE QUEUE
DCBDSGPO EQU   BIT6 -         PO - PARTITIONED ORGANIZATION
DCBDSGU  EQU   BIT7 -         U  - UNMOVABLE, THE DATA CONTAINS
*                                  LOCATION DEPENDENT INFORMATION
.SKP1    ANOP  ,
&P.DSRG2 DS    BL1 -          SECOND BYTE OF DCBDSORG
         AIF   (&#DCBDSG).SKP2
DCBDSGGS EQU   BIT0 -         GS - GRAPHICS ORGANIZATION
DCBDSGTX EQU   BIT1 -         TX - TCAM LINE GROUP
DCBDSGTQ EQU   BIT2 -         TQ - TCAM MESSAGE QUEUE
DCBACBM  EQU   BIT4 -         ACCESS METHOD CONTROL BLOCK   ICBI DCB-1
         AIF   (&#DCBSW(44)).SKP2
&#DCBSW(44) SETB (1)
DCBDSGTR EQU   BIT5 -         TR - TCAM 3705                    S22024
.SKP2    ANOP
&#DCBDSG SETB  (1)
         MEND
         MACRO
&N       #ENTER &NME,&ESDTYPE=DEFAULT,&BASES=1,&SAVTYPE=LOCAL,&PFIX=
.*
.*
.*                                                            08/85 DBC
.* LAST CHANGE DATE - AUGUST 12, 1985                         08/85 DBC
.*                  - THIS MACRO NO LONGER ASSUMES THAT A     08/85 DBC
.*                    SATISFIED UNCONDITIONAL GETMAIN         08/85 DBC
.*                    REQUEST SETS R15 TO ZERO. ONE CUSTOMER  08/85 DBC
.*                    ACTUALLY HAD A MODIFIED GETMAIN SVC     08/85 DBC
.*                    ROUTINE THAT INSURED THAT R15 WAS       08/85 DBC
.*                    UNCHANGED BY GETMAIN. THIS LED TO       08/85 DBC
.*                    DISASTEROUS RESULTS WHEN HE ATTEMPTED   08/85 DBC
.*                    TO USE THIS MACRO IN THAT               08/85 DBC
.*                    ENVIRONMENT. NOW, CHANGING MY MACRO     08/85 DBC
.*                    WAS A LOT SIMPLER THAN CHANGING HIS     08/85 DBC
.*                    ENVIRONMENT, AND THERE WAS A LOT OF     08/85 DBC
.*                    MONEY AT STAKE, SO ...                  08/85 DBC
.*                                                            08/84 DBC
.* LAST CHANGE DATE - AUGUST 8, 1984                          08/84 DBC
.*                  - WHEN BASES=* IS SPECIFIED, #ENTER NOW   08/84 DBC
.*                    DERIVES THE DESIRED BASE ADDRESS BY     08/84 DBC
.*                    SUBTRACTING AN OFFSET FROM THE          08/84 DBC
.*                    CURRENT ENTRY ADDRESS. PREVIOUSLY, IT   08/84 DBC
.*                    WAS JUST LOADING AN ADCON FOR THE       08/84 DBC
.*                    DESIRED BASE ADDRESS. THIS CAUSED       08/84 DBC
.*                    PROBLEMS IF THE #ENTER MACRO WAS        08/84 DBC
.*                    LOCATED WITHIN DYNAMICALLY RELOCATED    08/84 DBC
.*                    CODE.                                   08/84 DBC
.*                  - SIMILARLY, WHEN SAVTYPE=(REMOTE,NME%)   08/84 DBC
.*                    IS SPECIFIED, THE DESIRED SAVE AREA     08/84 DBC
.*                    IS LOCATED BY ADDING AN OFFSET (WHICH   08/84 DBC
.*                    MAY BE NEGATIVE) TO THE CURRENT ENTRY   08/84 DBC
.*                    ADDRESS.                                08/84 DBC
.*                                                            06/84 DBC
.* LAST CHANGE DATE - JUNE 11, 1984                           06/84 DBC
.*                  - ADDED "SAVTYPE=NONE" SUPPORT.           06/84 DBC
.*                  - WHEN A REMOTE SAVE AREA WAS USED,       06/84 DBC
.*                    #ENTER USE TO GENERATE A "USING"        06/84 DBC
.*                    STATEMENT DECLARING R13 AS A BASE FOR   06/84 DBC
.*                    THAT SAVE AREA. THAT "USING" STATEMENT  06/84 DBC
.*                    IS NO LONGER GENERATED.                 06/84 DBC
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 27, 1983
.*                  - MAILING ADDRESS CHANGE.
.*                  - USE OF IBM'S "SAVE" MACRO HAS BEEN
.*                    REPLACED BY LOCAL CODE.
.*                  - THE ASSEMBLY DATE AND TIME ARE NOW
.*                    INCLUDED IN THE MODULE IDENTIFIER
.*                    TEXT.
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $ENTER TO #ENTER.
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - ADDED ENTRY LINKAGE FOR A PLI ENVIRONMENT.
.*                  - ADDED "#REGS GEN=NO" SUPPORT.
.*
.* LAST CHANGE DATE - JULY 18, 1980
.*                  - BUG FIXED: THE PRECEEDING MODIFICATION INTRODUCED
.*                    AN ERROR WHICH UNDER CERTAIN CIRCUMSTANCES
.*                    GENERATED ASSEMBLY ERRORS.
.*
.* LAST CHANGE DATE - JULY 10, 1980
.*                  - FOR GETMAINED REENTRANT SAVE AREAS, CODE HAS BEEN
.*                    ADDED TO CLEAR THE ENTIRE GETMAINED AREA TO ZEROS
.*                    BEFORE SETING THE CHAIN FIELD.
.*                  - INDIRECT ADDRESSING TO A REMOTE SAVE AREA IS NOW
.*                    SIGNALLED BY A TRAILING PERCENT SIGN RATHER THAN
.*                    A LEADING PERCENT SIGN.
.*
.* LAST CHANGE DATE - OCTOBER 3, 1979
.*                  - CODE HAS BEEN ALTERED SO THAT ADDRESSABILITY TO
.*                    A REMOTE SAVE AREA DOES NOT HAVE TO BE BASED ON
.*                    R15 (I.E., ON THE ENTRY ADDRESS).
.*
.* LAST CHANGE DATE - OCTOBER 3, 1978
.*                  - THE GETMAIN FOR THE RENTRANT SAVE AREA HAS BEEN
.*                    CHANGED SO THAT MORE THAN 4K BYTES CAN BE GOTTEN.
.*
.* LAST CHANGE DATE - FEBRUARY 28, 1978
.*                  - BUG FIXED IN REMOTE SAVE AREA HANDLING
.*
.* LAST CHANGE DATE - JANUARY 29, 1978
.*                  - IN MOST CASES IT IS LOGICALLY INCONSISTANT TO
.*                    CODE 'BASES=*' WHEN ONE OF THE OLD BASES IS R13.
.*                    THIS PROBLEM IS NOW RECOGNIZED AND FLAGGED.
.*
.*                  - A REMOTE SAVE AREA'S NAME CAN NOW BE GIVEN EITHER
.*                    WITH OR WITHOUT A PRECEEDING PERCENT (%) SIGN TO
.*                    INDICATE WHETHER THE NAMED ADDRESS MUST BE
.*                    REACHED BY INDIRECT ADDRESSING.
.*
.* LAST CHANGE DATE - NOVEMBER 4, 1977
.*                  - SUPPORT IS ADDED FOR DEFINING A LOCAL SAVE AREA
.*                    WHOSE LENGTH IS OTHER THAN 72 BYTES.
.*
.* LAST CHANGE DATE - JANUARY 13, 1977
.*                  - THE MF= AND SVID= OPERANDS ARE REPLACED BY THE
.*                    SAVTYPE= OPERAND.
.*                  - SUPPORT FOR THE HANDLING OF A REMOTELY ASSEMBLED
.*                    SAVE AREA.
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - SEPTEMBER 14, 1976
.*                  - IMPLEMENT SUPPORT FOR "BASES=*" WHICH IMPLIES
.*                    THAT BOTH THE BASE ADDRESS AND BASE REGISTERS
.*                    DEFINED BY THE PHYSICALLY PREVIOUS USE OF THE
.*                    #ENTER MACRO ARE TO BE REUSED.
.*
.* LAST CHANGE DATE - AUGUST 23, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES OS STANDARD ENTRY LINKAGE. IT WAS WRITTEN TO
.* PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE METHOD OF GENERATING SUCH
.* LINKAGE. IN ADDITION, FOR ALMOST ALL OPERAND COMBINATIONS, #ENTER
.* WILL GENERATE THE ABSOLUTE MINIMUM AMOUNT OF CODE NECESSARY.
.*
.*   THE #ENTER MACRO WILL GENERATE THE FOLLOWING:
.*     - A CSECT OR ENTRY CARD (IF DESIRED)
.*     - A MODULE IDENTIFIER WHICH WILL INCLUDE THE ASSEMBLY
.*       DATE AND TIME.
.*     - CODE TO SAVE ALL REGISTERS IN THE HIGHER SAVE AREA
.*     - CODE TO LOAD R13 WITH A POINTER TO A LOWER SAVE AREA
.*     - CODE TO CROSS LINK THE LOWER SAVE AREA WITH THE HIGHER SAVE
.*       AREA
.*     - CODE TO LOAD ANY SET OF BASE REGISTERS
.*     - A USING STATEMENT DECLARING THE SET OF BASE REGISTERS
.*     - EITHER THE LOWER SAVE AREA ITSELF OR CODE TO GETMAIN A
.*       REENTRANT SAVE AREA OF 72 OR MORE BYTES OR CODE TO LOAD THE
.*       ADDRESS OF AN ASSEMBLED SAVE AREA THAT IS REMOTE FROM THE
.*       MACRO EXPANSION.
.*     - FOR A GETMAINED REENTRANT SAVE AREA, CODE TO CLEAR THE AREA TO
.*       ZEROS.
.*
.* &N
.*       THIS IS THE ONLY FIELD REQUIRED FOR THE MACRO CALL. IT MUST
.*       SPECIFY THE DESIRED CONTROL SECTION OR ENTRY NAME.
.*
.* &NME
.*       USE THIS FIELD TO SPECIFY OPTIONAL TEXT TO BE
.*       INCLUDED INTO THE MODULE IDENTIFIER. ENCLOSING
.*       QUOTES ARE OPTIONAL.
.*
.* &ESDTYPE=
.*       THIS OPERAND CONTROLS THE TYPE OF EXTERNAL SYMBOL (IF ANY) TO
.*       BE GENERATED USING &N. VALID VALUES FOR THIS OPERAND IMPLY THE
.*       FOLLOWING:
.*              -OMITTED-    ==> ESDTYPE=ENTRY IF SAVTYPE=PLI
.*              -OMITTED-    ==> ESDTYPE=CSECT OTHERWISE
.*             ESDTYPE=CSECT ==>
.*                     &N       CSECT
.*             ESDTYPE=START ==>
.*                     &N       START
.*             ESDTYPE=ENTRY ==>
.*                              ENTRY &N
.*                     &N       DS    0H
.*             ESDTYPE=     <==>
.*             ESDTYPE=NONE  ==>
.*                     &N       DS    0H
.*
.* &BASES=
.*       USE THIS OPERAND TO SPECIFY EITHER HOW MANY OR EXACTLY WHICH
.*       BASE REGISTERS TO DECLARE AND LOAD. VALID VALUES FOR THIS
.*       OPERAND ARE:
.*             BASES= -A SINGLE SELF DEFINING NUMERIC-
.*                   THIS REQUESTS THAT A SPECIFIC NUMBER OF BASES BE
.*                   LOADED AND DECLARED. THE MACRO IS ALLOWED TO
.*                   DETERMINE FOR ITSELF PRECISELY WHICH REGISERS TO
.*                   DECLARE AS FOLLOWS. FOR SAVTYPE=LOCAL (SEE BELOW)
.*                   THE FIRST BASE REGISTER WILL BE R13; OTHERWISE
.*                   (I.E. FOR SAVTYPE=RENT OR SAVTYPE=REMOTE), THE
.*                   FIRST BASE REGISTER WILL BE R12. IN EITHER CASE,
.*                   ADDITIONAL BASES WILL BE SUCCESSIVELY LOWER
.*                   NUMBERED REGISTERS. EXAMPLES:
.*                   BASES=3,SAVTYPE=RENT  ==> R12, R11, AND R10.
.*                   BASES=2,SAVTYPE=LOCAL ==> R13, AND R12.
.*             BASES= -A SUBLIST OF ONE OR MORE REGISTER NAMES-
.*                   THE LISTED REGISTERS ARE LOADED AND DECLARED AS
.*                   BASES. THE LEFTMOST LISTED REGISTER IS LOADED WITH
.*                   THE LOWEST ADDRESS. EXAMPLE:
.*                   BASES=(R5,6,4) ==> R5, R6, AND R4 IN THAT ORDER.
.*             BASES=*
.*                   THE BASE ADDRESS AND BASE REGISTERS DEFINED BY THE
.*                   PHYSICALLY PREVIOUS #ENTER MACRO ARE REUSED.
.*          THE BASES= OPERAND MAY BE NULLIFIED BY SPECIFYING EITHER:
.*             BASES=
.*             BASES=0
.*       IN THIS CASE, NO BASE REGISTERS ARE LOADED OR DECLARED.
.*          IF THE BASES= OPERAND IS OMITTED, THEN A DEFAULT OF BASES=1
.*       WILL BE USED.
.*
.* &SAVTYPE=
.*       THIS OPERAND IDENTIFIES THE TYPE OR LOCATION OF THE SAVE AREA
.*       TO BE GENERATED OR USED. VALID VALUES ARE:
.*             -OMITTED-
.*             SAVTYPE=
.*             SAVTYPE=LOCAL
.*             SAVTYPE=(,-SAVE AREA NAME-)
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-)
.*             SAVTYPE=(LOCAL,,-SAVE AREA LENGTH-)
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-,-SAVE AREA LENGTH-)
.*                     A STANDARD SAVE AREA IS GENERATED IN THE
.*                     THE MACRO EXPANSION AND ITS ADDRESS IS LOADED
.*                     INTO R13. NOTE, DEPENDING UPON THE BASES=
.*                     OPERAND (SEE ABOVE) R13 MAY ALSO BE DECLARED AS
.*                     A PROGRAM BASE.
.*                        IF A -SAVE AREA NAME- IS GIVEN, THEN IT IS
.*                     USED TO LABEL THE SAVE AREA; OTHERWISE, AN
.*                     INTERNAL NAME IS GENERATED.
.*                              IF -SAVE AREA LENGTH- IS GIVEN, THEN IT
.*                              IS USED TO SET THE LENGTH OF THE SAVE
.*                              AREA; OTHERWISE, THE DEFAULT LENGTH OF
.*                              72 BYTES IS USED.
.*             SAVTYPE=(REMOTE,-SAVE AREA ADDRESS-)
.*                     THE ADDRESS OF THE REMOTE SAVE AREA IS
.*                     LOADED INTO R13. NOTE, IN THIS CASE -SAVE AREA
.*                     ADDRESS- IS A REQUIRED SUB-OPERAND. IT MAY BE
.*                     EITHER AN ADDRESS LABEL OR A PARENTHESIZED
.*                     REGISTER NAME OR AN ADDRESS LABEL FOLLOWED BY A
.*                     PERCENT (%) SIGN. IF THE NAME IS JUST AN ADDRESS
.*                     LABEL, THEN A 'LA' INSTRUCTION IS USED TO LOAD
.*                     THE SAVE AREA'S ADDRESS. IF A PERCENT SIGN
.*                     FOLLOWS THE NAME, THEN AN ADDRESS CONSTANT IS
.*                     GENERATED AND A 'L' INSTRUCTION IS USED. IF A
.*                     REGISTER NAME IS GIVEN, THEN A 'LR' INSTRUCTION
.*                     IS USED UNLESS THE MACRO CAN DETERMINE THAT THE
.*                     NAMED REGISTER IS ACTUALLY R13 IN WHICH CASE IT
.*                     IS ASSUMED THAT THE LOWER SAVE AREA IS ALREADY
.*                     PRESENT AND INITIALIZED, SO THE SAVING OF
.*                     REGISTERS AND THE CROSS-CHAINING OF THE SAVE
.*                     AREAS IS BYPASSED.
.*             SAVTYPE=RENT
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),-ERROR ADDRESS-)
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),RETURN)
.*                     THE MACRO EXPANSION IS TO BE REENTRANT. THE SAVE
.*                     AREA IS TO BE GETMAINED. THE SUB-OPERANDS HAVE
.*                     THE FOLLOWING AFFECT:
.*                     -LENGTH- IS OPTIONAL. IF OMITTED, THEN A VALUE
.*                              OF 72 IS USED. IF GIVEN, THEN IT
.*                              INDICATES THE SIZE OF THE SAVE AREA TO
.*                              BE GOTTEN. WARNING, THE VALUE OF
.*                              -LENGTH- SHOULD NEVER BE LESS THAN 72.
.*                     -SUBPOOL- IS OPTIONAL. IF OMITTED, THEN A VALUE
.*                               OF 0 IS IMPLIED. IF GIVEN, THEN IT
.*                               SPECIFIES THE SUBPOOL OUT OF WHICH THE
.*                               SAVE AREA IS TO BE GOTTEN.
.*                     -ERROR ADDRESS- IS OPTIONAL. IF OMITTED, THEN
.*                                     THE SAVE AREA GETMAIN REQUEST IS
.*                                     UNCONDITIONAL. IF GIVEN, THEN
.*                                     THE GETMAIN IS CONDITIONAL, AND
.*                                     IF IT FAILS, THEN CONTROL IS
.*                                     PASSED TO THE INDICATED ADDRESS.
.*                                     NOTE, -ERROR ADDRESS- MAY BE
.*                                     EITHER A STATEMENT LABEL OR A
.*                                     PARENTHESIZED REGISTER NAME.
.*                                     WARNING, -ERROR ADDRESS- MAY BE
.*                                     USED ONLY IN A MVS ENVIRONMENT.
.*                                     IT IS NOT SUPPORTED UNDER MVT.
.*                     RETURN IS A SPECIAL FORM OF -ERROR ADDRESS-
.*                            WHICH, IF GIVEN, CAUSES CONTROL TO BE
.*                            RETURNED IMMEDIATELY TO THE CALLER IN THE
.*                            EVENT OF A GETMAIN FAILURE. ALL REGISTERS
.*                            ARE RESTORED EXCEPT R15 WHICH CONTAINS
.*                            THE RETURN CODE FROM GETMAIN.
.*             SAVTYPE=PLI
.*             SAVTYPE=NONE                                   06/84 DBC
.*                     NO LOCAL LEVEL SAVEAREA IS DEFINED OR  06/84 DBC
.*                     USED.                                  06/84 DBC
.*             SAVTYPE=(PLI,-LENGTH-)
.*                     THE MACRO IS TO EXPAND INTO THE FORMAT OF A PLI
.*                     PROLOG. THE EXPANSION IS REENTRANT. THE SAVE
.*                     AREA WILL BE A PLI DSA. IT WILL PROBABLY BE
.*                     OBTAINED FROM PLI'S ISA. IF -LENGTH- IS GIVEN,
.*                     THEN IT SPECIFIES THE DESIRED LENGTH OF THE DSA
.*                     THAT THIS EXPANSION OBTAINS. WARNING, THE VALUE
.*                     OF -LENGTH- MUST NEVER BE LESS THAN 88.
.*
.* &PFIX=
.*       THE #ENTER MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS
.*       EXPANSION WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE
.*       OF THE REGISTERS IS INDICATED IN THE ASSEMBLER'S CROSS
.*       REFERENCE LISTING. THE PFIX= OPERAND CAN BE USED TO CONTROL
.*       THE SET OF EQUATES USED. FOR EXAMPLE, IF "PFIX=GPR" IS GIVEN,
.*       THEN "GPR1" IS USED WHENEVER THE EXPANSION REFERS FO REGISTER
.*       1.
.*          IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF REGISTER
.*       EQUATES DEFINED BY THE NEXT PRIOR #REGS MACRO IS USED. IF
.*       THERE ARE NO PRIOR #REGS MACROS, THEN SELF-DEFINING NUMERICS
.*       ARE USED.
.*
.* MISCELLANIOUS CONSIDERATIONS
.*     - SINCE THE #ENTER MACRO EXPANSION USUALLY INCLUDES A CSECT
.*       CARD, THE MACRO CALL SHOULD BE PLACED AT THE PHYSICAL
.*       BEGINNING OF A CONTROL SECTION.
.*     - FOR LOCAL SAVE AREAS IT IS BOTH POSSIBLE AND REASONABLE FOR
.*       R13 TO SERVE AS BOTH THE SAVE AREA POINTER AND A PROGRAM BASE.
.*       THERE ARE, HOWEVER, CERTAIN PRECAUTIONS THAT HAVE TO BE TAKEN
.*       FOR VARIOUS SYSTEM EXIT ROUTINES IF THEY ARE INCLUDED IN THE
.*       PROGRAM. HERE ARE SOME EXAMPLES:
.*           - IOS APPENDAGE ROUTINES: NO BASE REGISTER FOR THE MAIN
.*             PROGRAM, INCLUDING R13, IS AVAILABLE FROM WITHIN AN IOS
.*             APPENDAGE.
.*           - DCB OPEN EXITS: R13 REMAINS A VALID BASE REGISTER.
.*           - EOD ROUTINES: R13 REMAINS A VALID BASE REGISTER.
.*           - SYNAD EXITS: R13 REMAINS A VALID BASE REGISTER UNTIL A
.*             SYNADAF MACRO IS ISSUED. AFTER A SUBSEQUENT SYNADRLS
.*             MACRO, R13 IS AGAIN A VALID PROGRAM BASE.
.*
.*
.*
.* INNER MACROS USED - #USING, #TEST, SAVE, GETMAIN
.*
         GBLA  &#TESERR
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     06/84 DBC
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP,&#BS(14)
         LCLA  &A1,&A2,&C1,&C2,&C3,&C4,&B(13),&RMTREGA
         LCLB  &REDUN(13),&OLDBASE,&REMOTE(5),&ALIGND,&LOCAL,&LENGTH
         LCLC  &LID,&@,&#,&N1,&N2,&W2,&W3,&R,&RMTREGC,&SAVLEN,&RMTNAME
         LCLC  &SPOOL,&TB,&ESDT,&C                            06/84 DBC
&#       SETC  '&SYSNDX'
&#ENTRNT SETB  (0)
&#ENTPLI SETB  (0)
&#ENTNUN SETB  (0)
&C1      SETA  11
.*                                                            06/84 DBC
         AIF   ('&SAVTYPE' NE 'NONE').TYPNNON                 06/84 DBC
&#ENTNUN SETB  (1)                                            06/84 DBC
         AGO   .PFXTST                                        06/84 DBC
.TYPNNON ANOP                                                 06/84 DBC
.*                                                            06/84 DBC
         AIF   ('&SAVTYPE(1)' NE 'RENT').TYPNRNT
&#ENTRNT SETB  (1)
&#ENTSIZ SETC  '72'
&#ENTSP  SETC  ''
         #TEST DCODE=&SAVTYPE(2)
&A1      SETA  &#TESRET(1)
         AIF   (&A1 EQ 0).PFXTST
         AIF   ('&#TESRET(2)' EQ '').DFLTLEN
&#ENTSIZ SETC  '&#TESRET(2)'
.DFLTLEN AIF   (&A1 EQ 1).PFXTST
&#ENTSP  SETC  '&#TESRET(3)'
         AGO   .PFXTST
.TYPNRNT ANOP
.*
         AIF   ('&SAVTYPE(1)' NE 'REMOTE').TYPNRMT
&REMOTE(1) SETB (1)
         AIF   ('&SAVTYPE(2)' NE '').GOTRMT2
         MNOTE 12,'ERROR - SAVTYPE(2) (REMOTE AREA''S NAME) OMITTED.'
.GOTRMT2 AIF   ('&SAVTYPE(2)'(1,1) EQ '(').TYPLCL2
&REMOTE(2) SETB (1)
&RMTNAME SETC  '&SAVTYPE(2)'
         AIF   ('&SAVTYPE(2)'(K'&SAVTYPE(2),1) NE '%').PFXTST
&REMOTE(5) SETB (1)
&RMTNAME SETC  '&SAVTYPE(2)'(1,K'&SAVTYPE(2)-1)
         AGO   .PFXTST
.TYPLCL2 #TEST DCODE=&SAVTYPE(2)
&RMTREGC SETC  '&#TESRET(2)'
         #TEST REGS=&RMTREGC
         AIF   (&#TESERR NE 0).PFXTST
&RMTREGA SETA  &#TESRET(1)
         AIF   (&RMTREGA NE 13).PFXTST
&REMOTE(3) SETB (1)
         AGO   .PFXTST
.TYPNRMT ANOP
.*
         AIF   ('&SAVTYPE(1)' NE 'PLI').TYPNPLI
&#ENTPLI SETB  (1)
&C1      SETA  10
         AGO   .PFXTST
.TYPNPLI ANOP
.*
         AIF   ('&SAVTYPE(1)' EQ '' OR '&SAVTYPE(1)' EQ 'LOCAL').TYPLCL
         MNOTE 4,'SAVTYPE(1)=&SAVTYPE(1) IS INVALID.'
         MNOTE 4,'SAVTYPE(1)=LOCAL ASSUMED.'
.TYPLCL  ANOP
&LOCAL   SETB  (1)
&C1      SETA  12
&LID     SETC  'E&#.SVA'
         AIF   ('&SAVTYPE(2)' EQ '').GOTSLID
&LID     SETC  '&SAVTYPE(2)'
.GOTSLID ANOP
&SAVLEN  SETC  '72'
         AIF   ('&SAVTYPE(3)' EQ '').PFXTST
&SAVLEN  SETC  '&SAVTYPE(3)'
.*
.PFXTST  ANOP
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*                                                            06/84 DBC
&A1      SETA  0
         AIF   ('&BASES' NE '*').BSCLR
.BSOLD   AIF   (&A1 EQ 13).BASEND
&A1      SETA  &A1+1
         AIF   ('&#BS(&A1)' EQ '').BSOLD
         #TEST REGS=&#BS(&A1)
&B(&A1)  SETA  16
         AIF   (&#TESERR NE 0).BSOLD
&B(&A1)  SETA  &#TESRET(1)
         AIF   (&B(&A1) NE 13).BSOLD
         AIF   (&A1 NE 13 OR '&#BS(14)' NE '&RMTNAME' OR &#ENTRNT OR &#*
               ENTPLI).BSERROR
&REMOTE(4) SETB (1)
         AGO   .BSOLD
.BSERROR ANOP
         MNOTE 4,'THE OLD BASE REGISTER &B(&A1) CANNOT ALSO FUNCTION'
         MNOTE 4,'AS A SAVE AREA POINTER IN THIS CONTEXT.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
         AGO   .BSOLD
.BSCLR   AIF   (&A1 EQ 14).BSCLRD
&A1      SETA  &A1+1
&#BS(&A1) SETC ''
         AGO   .BSCLR
.BSCLRD  AIF   (K'&BASES EQ 0).BASEND
         AIF   ('&BASES' NE '&BASES(1)').TSTNBSE
         #TEST NUM=&BASES
         AIF   (&#TESERR EQ 0).BSEOKX
         MNOTE 4,'"BASES=&BASES" IS INVALID.'
         MNOTE 4,'"BASES=1" ASSUMED.'
&C3      SETA  1
         AGO   .BSESET
.BSEOKX  ANOP
&C3      SETA  &BASES
         AIF   (&C3 LE &C1).BSESET
         MNOTE 4,'"BASES=&BASES" IS OUTSIDE THE RANGE OF 0...&C1..'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.BSESET  ANOP
&C1      SETA  &C1+1
&C3      SETA  &C1-&C3
&C4      SETA  13
.BOK     AIF   (&C1 LE &C3).BASEND
&#BS(&C4) SETC '&@&C1'
&B(&C4)  SETA  &C1
&C1      SETA  &C1-1
&C4      SETA  &C4-1
         AGO   .BOK
.TSTNBSE ANOP
&C3      SETA  N'&BASES
         AIF   (&C3 LE &C1).NBSOK
         MNOTE 4,'"BASES=&BASES" SPECIFIES TOO MAY REGISTERS.'
         MNOTE 4,'ONLY THE FIRST &C1 REGISTERS WILL BE USED.'
&C3      SETA  &C1
.NBSOK   ANOP
&C1      SETA  &C1+1
&C4      SETA  13
&C2      SETA  0
.GETBSE  AIF   (&C2 GE &C3).BASEND
&C2      SETA  &C2+1
         AIF   ('&BASES(&C2)' EQ '').IGNR
         #TEST REGS=&BASES(&C2)
&B(&C4)  SETA  16
         AIF   (&#TESERR EQ 16).REGUNK
         AIF   (&#TESRET(1) GE 2 AND &#TESRET(1) LE &C1).BSEOK2
         MNOTE 4,'"BASES(&C2)=&BASES(&C2)" IS OUTSIDE THE RANGE OF 2...*
               &C1..'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
         AGO   .BSEOK2
.IGNR    AIF   (&C3 GE N'&BASES).GETBSE
&C3      SETA  &C3+1
         AGO   .GETBSE
.BSEOK2  AIF   (NOT &REDUN(&#TESRET(1))).BSEOK3
         MNOTE 4,'"BASES(&C2)=&BASES(&C2) IS REDUNDANT.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.BSEOK3  ANOP
&REDUN(&#TESRET(1)) SETB (1)
&B(&C4)  SETA &#TESRET(1)
.REGUNK  ANOP
&C1      SETA  12
&#BS(&C4) SETC '&BASES(&C2)'
&C4      SETA  &C4-1
         AGO   .GETBSE
.BASEND  ANOP
.*
&R       SETC  '&@.1'
&C3      SETA  0
.WRLP    AIF   (&C3 GE 13).GOTWRG
&C3      SETA  &C3+1
         AIF   ('&#BS(&C3)' EQ '').WRLP
         AIF   (&B(&C3) EQ 13).GOTWRG
&R       SETC  '&#BS(&C3)'
.GOTWRG  ANOP
.*
&ESDT    SETC  '&ESDTYPE(1)'
         AIF   (&#ENTPLI).PLIGEN
.*
         AIF   ('&ESDT' NE 'DEFAULT').GOTESD
&ESDT    SETC  'CSECT'
.GOTESD  ANOP
&N2      SETC  '&N'
         AIF   ('&ESDT' EQ 'NONE' OR '&ESDT' EQ '' OR '&ESDT' EQ 'ENTRY*
               ').NCSCETC
         AIF   ('&ESDT' NE 'CSECT').ESDNCSC
&N       CSECT ,                   START CONTROL SECTION
         AGO   .ESDDONE
.ESDNCSC AIF   ('&ESDT' NE 'START').ESDNSTA
&N       START ,                   START CONTOL SECTION
         AGO   .ESDDONE
.ESDNSTA ANOP
&W2      SETC  '&ESDT'
&N       &W2   0H'0'               START
.ESDDONE ANOP
&N2      SETC  ''
.*
.NCSCETC AIF   ('&ESDT' NE 'ENTRY').NENTRY
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE
.NENTRY  ANOP
.*
         AIF   (K'&NME EQ 0 AND '&ESDT' NE 'CSECT' AND '&ESDT' NE 'STAR*
               T').NMODID
&N2      B     E&#.ZID-&N.(,&@.15) SKIP AROUND THE MODULE ID
&N2      SETC  'E&#.ZID'
         DC    AL1(&N2-E&#.MID)    LENGTH OF TEXT
E&#.MID  DC    C'&N '              ENTRY NAME
&W2      SETC  '&SYSDATE       '(1,8).' '
         DC    C'&W2'              ASSEMBLY DATE
&W2      SETC  '&SYSTIME    '(1,5)
         AIF   (K'&NME EQ 0).NMEZ1
&W2      SETC  '&W2 - '
.NMEZ1   DC    C'&W2'              ASSEMBLY TIME
         AIF   (K'&NME EQ 0).NMEZ2
         AIF   ('&NME'(1,1) EQ '''').QNME
         DC    C'&NME'
         AGO   .NMEZ2
.QNME    DC    C&NME
.NMEZ2   ANOP
.NMODID  ANOP
.*
         AIF   (&REMOTE(3)).RNTRNT1
&W2      SETC  '&@.14,&@.12,12(&@.13)'
&N2      STM   &W2                 SAVE CALLER'S REGISTERS
&N2      SETC  ''
.*
         AIF   (&#ENTRNT OR &#ENTNUN).RNTRNT1                 06/84 DBC
         LR    &R,&@.13            POINT TO HIGHER SA
         AIF   (&REMOTE(1)).LRMTSV1
&C       SETC  '&@.13,&LID-&N.(,&@.15)'                       06/84 DBC
         LA    &C                  POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV1 AIF   (&REMOTE(2)).LRMTSV3
         #TEST REGS=&R
         AIF   (&RMTREGA NE &#TESRET(1)).LRMTSV4
&A1      SETA  20+&RMTREGA*4-&RMTREGA/13*44
         L     &@.13,&A1.(,&@.13)  POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV4 LR    &@.13,&RMTREGC      POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV3 AIF   (&REMOTE(5)).LRMTSV5
         PUSH  USING               SAVE USING ENVIRONMENT
         USING &N,&@.15            DECLARE TEMP BASE
         LA    &@.13,&RMTNAME      POINT TO LOWER SA
         POP   USING               RESTORE USING ENVIRONMENT
         AGO   .LRMTSV2                                       06/84 DBC
.LRMTSV5 ANOP                                                 06/84 DBC
         LR    &@.13,&@.15         POINT TO LOWER SA          08/84 DBC
&C       SETC  '&@.13,E&#.SAP-&N.(,&@.15)'                    06/84 DBC
         AH    &C                                             08/84 DBC
.LRMTSV2 ST    &@.13,8(,&R)        FORWARD CHAIN THE SAVE AREAS
         ST    &R,4(,&@.13)        BACK CHAIN THE SAVE AREAS
         AIF   ('&R' NE '&@.1').RNTRNT1
         L     &@.1,24(,&@.1)      RESTORE REGISTER 1
.RNTRNT1 AIF   ('&#BS(13)' EQ '').SKIPUSE
&C1      SETA  13
         AIF   ('&BASES' NE '*').BSEADR
         AIF   (&REMOTE(4)).EQUATE
&OLDBASE SETB  (1)
&N2      LR    &#BS(13),&@.15      LOAD 1ST BASE REGISTER     08/84 DBC
&N2      SETC  ''                                             08/84 DBC
&C       SETC  '&#BS(13),E&#.BSE-&N.(,&@.15)'                 06/84 DBC
         SH    &C                                             08/84 DBC
         AGO   .EQUATE
.BSEADR  ANOP
&C2      SETA  15
&#BS(14) SETC  '&N'
         AIF   (&#ENTRNT OR &REMOTE(1) OR &#ENTNUN).FLDTST    06/84 DBC
&C2      SETA  13
&#BS(14) SETC  '&LID'
.FLDTST  AIF   (&B(13) EQ &C2).EQUATE
&N2      LR    &#BS(13),&@&C2      LOAD FIRST BASE REGISTER
&N2      SETC  ''
.EQUATE  ANOP
&W2      SETC  '&#BS(&C1)'
         AIF   (&C1 EQ 2).ENDLA
&C1      SETA  &C1-1
         AIF   ('&#BS(&C1)' EQ '').ENDLA
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT BASE
&N2      SETC  ''
         AGO   .EQUATE
.ENDLA   #USING
.SKIPUSE AIF   (NOT &#ENTRNT).DATACHK
&W2      SETC  ''
&TB      SETC  ''
         AIF   ('&#BS(13)' NE '').GETM2
&N2      LR    &@.14,&@.15         LOAD TEMPORARY BASE
&N2      SETC  ''
         PUSH  USING               SAVE BASES
         DROP  ,                   CLEAR BASES
         USING &N,&@.14            DECLARE TEMPORARY BASE
&TB      SETC  '-&N.(,&@.14)'
.GETM2   ANOP
&N2      L     &@.0,E&#.LEN        LOAD LENGTH (MAYBE SUBPOOL TOO)
&N2      SETC  ''
         AIF   ('&#ENTSP' EQ '' OR '&SAVTYPE(3)' EQ '').GETM4
&SPOOL   SETC  ''
         MNOTE '         GETMAIN RC,LV=(0),SP=&#ENTSP'
         GETMAIN RC,LV=(0),SP=&#ENTSP
         AGO   .GETM5
.GETM4   ANOP
&SPOOL   SETC  '&#ENTSP'
&W3      SETC  'R'
         AIF   ('&SAVTYPE(3)' EQ '').GETM4A
&W3      SETC  'RC'
.GETM4A  MNOTE '         GETMAIN &W3,LV=(0)'
         GETMAIN &W3,LV=(0)
.GETM5   AIF   ('&#BS(13)' NE '').GETM5A
         POP   USING               RESTURE BASES
.GETM5A  AIF   ('&SAVTYPE(3)' EQ '').GETM7
         LTR   &@.15,&@.15         GETMAIN OK?
         AIF   ('&SAVTYPE(3)' EQ 'RETURN').GETM8
         AIF   ('&SAVTYPE(3)'(1,1) EQ '(').GETM6
         BNZ   &SAVTYPE(3)         NO, TAKE ERROR EXIT
         AGO   .GETM7
.GETM6   #TEST DCODE=&SAVTYPE(3)
         BCR   7,&#TESRET(2)       NO, TAKE ERROR EXIT
         AGO   .GETM7
.GETM8   ANOP
&W2      SETC  'E&#.GO'
         BZ    &W2&TB              YES, PROCEED
         L     &@.14,12(,&@.13)    NO, RESTORE REGISTER
         LM    &@.0,&@.12,20(&@.13) RESTORE REGISTERS
         MVI   12(&@.13),X'FF'     SET RETURNED SIGNEL
         BR    &@.14               RETURN TO CALLER
.GETM7   ANOP
&W2      LR    &@.0,&@.1           POINT TO AREA TO CLEAR
         L     &@.1,E&#.LEN&TB     GET LENGTH TO CLEAR
         LR    &@.14,&@.0          SAVE AREA POINTER
         SLR   &@.15,&@.15         CLEAR SRC LEN AND PAD CHAR 08/85 DBC
         MVCL  &@.0,&@.14          CLEAR THE AREA (R15 SET BY GETMAIN)
         ST    &@.14,8(,&@.13)     FORWARD CHAIN THE SAVE AREAS
         ST    &@.13,4(,&@.14)     BACK CHAIN THE SAVE AREAS
         LM    &@.13,&@.1,8(&@.13) RESTORE REGS AND POINT TO LOWER SA
.DATACHK AIF   (NOT &OLDBASE AND NOT &#ENTRNT AND NOT &REMOTE(5) AND NO*
               T &LOCAL).ENDCHK
         AIF   ('&#BS(13)' EQ '').NOUSING
&N2      B     E&#.END             SKIP AROUND DATA AREA
&N2      SETC  ''
         AGO   .DFNDATA
.NOUSING ANOP
&N2      B     E&#.END-&N.(,&@.15) SKIP AROUND DATA AREA
&N2      SETC  ''
.DFNDATA ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &OLDBASE).NOLDBSE                         08/84 DBC
E&#.BSE  DC    Y(&N-&#BS(14))       OLD BASE ADDRESS          08/84 DBC
.NOLDBSE ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &#ENTRNT).NLENGTH                         08/84 DBC
         AIF   ('&SPOOL' EQ '').NSUBPOO
         AIF   (&ALIGND).ALIGND1
         DS    0F                  ALIGNMENT
&ALIGND  SETB  (1)
.ALIGND1 ANOP
E&#.LEN  DC    AL1(&SPOOL),AL3(&#ENTSIZ) SAVE AREA SUBPOOL AND LENGTH
         AGO   .NLENGTH
.NSUBPOO ANOP
E&#.LEN  DC    A(&#ENTSIZ)         SAVE AREA LENGTH
&ALIGND  SETB  (1)
.NLENGTH AIF   (&#ENTRNT OR &#ENTNUN).NSVAREA                 06/84 DBC
         AIF   (&REMOTE(1)).RMTSVPT
         AIF   (&ALIGND).ALIGND2
         DS    0F                  ALIGNMENT
&ALIGND  SETB  (1)
.ALIGND2 ANOP
&LID     DC    (&SAVLEN)X'00'      LOCAL SAVE AREA
         AGO   .NSVAREA
.RMTSVPT ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &REMOTE(5)).NSVAREA                       08/84 DBC
E&#.SAP  DC    Y(&RMTNAME-&N)       PTR TO REMOTE SA          08/84 DBC
.NSVAREA ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AGO   .END                                           08/84 DBC
.*
.PLIGEN  AIF   ('&ESDT' EQ 'DEFAULT').PESDSET
         AIF   (K'&N GT 0 OR '&ESDT' NE 'ENTRY').PESDOK1
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN THE NAME FIELD IS'
         MNOTE 4,'OMITTED FROM THE MACRO CALL.'
         MNOTE 4,'"ESDTYPE=NONE" WILL BE USED INSTEAD.'
&ESDT    SETC  'NONE'
.PESDOK1 ANOP
         AIF   ('&ESDT' EQ 'ENTRY' OR '&ESDT' EQ 'NONE' OR '&ESDT' EQ '*
               ').PESDOK
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN "SAVTYPE=PLI".'
.PESDSET ANOP
&ESDT    SETC  'ENTRY'
         AIF   (K'&N GT 0).PESDOK2
&ESDT    SETC  'NONE'
.PESDOK2 AIF   ('&ESDTYPE' EQ 'DEFAULT').PESDOK
         MNOTE 4,'"ESDTYPE=&ESDT" WILL BE USED INSTEAD.'
.PESDOK  ANOP
         AIF   ('&ESDT' NE 'ENTRY').PNOTENT
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE
.PNOTENT ANOP
.*
&N2      SETC  '&N'
&A1      SETA  K'&N
         AIF   (K'&NME EQ 0).GOTN2
&N2      SETC  '&NME'
&A1      SETA  K'&NME
.GOTN2   ANOP
&N2      SETC  ' '(1,1-(&A1-&A1/2*2)).'&N2'
         DS    0H                  ALIGNMENT
         DC    C'&N2'              ENTRY NAME
         DC    AL1(&A1)            LENGTH OF NAME
.*
&N1      SETC  '&N'
         AIF   (K'&N GT 0).PGOTN1
&N1      SETC  'E&#.ENT'
.PGOTN1  ANOP
         USING &N1,&@.15           DCL LOCAL BASE
&N1      STM   &@.14,&@.12,12(&@.13) SAVE CALLER'S REGISTERS
.*
         #TEST DCODE=&SAVTYPE(2)
&A1      SETA  &#TESRET(1)
&A2      SETA  120
         AIF   (&A1 EQ 0).DSALLA
         AIF   ('&#TESRET(2)' NE '&SAVTYPE(2)').DSALREG
         #TEST NUM=&#TESRET(2)
         AIF   (&#TESERR NE 0).DSALL
&A2      SETA  &#TESRET(2)
         AIF   (&A2 GE 4096-7).DSALL
         AIF   (&A2 GE 120-7).DSALLA
         MNOTE 4,'"SAVTYPE(2)=&SAVTYPE(2)" IS TOO SHORT A LENGTH.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.DSALLA  ANOP
         LA    &@.0,(&A2+7)/8*8    GET DESIRED DSA LENGTH
         AGO   .GOTDSAL
.DSALREG #TEST REGS=&#TESRET(2)
         AIF   (&#TESERR NE 0).DSALLR
         AIF   (&#TESRET(1) EQ 0).GOTLLR
.DSALLR  LR    &@.0,&#TESRET(2)    GET DESIRED DSA LENGTH
.GOTLLR  LA    &@.14,7             ROUND UP -
         AR    &@.0,&@.14           TO -
         OR    &@.0,&@.14            DOUBLE WORD -
         XR    &@.0,&@.14             LENGTH
         AGO   .GOTDSAL
.DSALL   ANOP
&LENGTH  SETB  (1)
         L     &@.0,E&#.LEN        GET DESIRED DSA LENGTH
.GOTDSAL ANOP
.*
         L     &@.1,76(,&@.13)     GET NXT AVAILABLE BLOCK POINTER
         ALR   &@.0,&@.1           --> PAST DESIRED AREA
         CL    &@.0,12(,&@.12)     WOULD THE ISA OVERFLOW?
         BNH   E&#.GOT             NO, PROCEED
         L     &@.15,116(,&@.12)   YES, --> SPECIAL HANDLER
         DROP  &@.15               RELEASE CLOBBERED BASE
         BALR  &@.14,&@.15         GO OBTAIN DESIRED DSA FROM ELSEWHERE
E&#.GOT  LR    &@.14,&@.1          SAVE PTR TO NEW DSA
         LR    &@.15,&@.0          SAVE HI-BYTE OF NAB POINTER REG
         SRL   &@.15,24            ISSOLATE IT
         SLL   &@.15,24            RESTORE ITS POSITION. SET MVCL
*                                  SOURCE LENGTH TO ZERO
         SR    &@.0,&@.1           GET LENGTH OF NEW DSA
         LR    &@.1,&@.0           COPY FOR MVCL SINK LENGTH
         LR    &@.0,&@.14          GET MVCL SINK POINTER
         MVCL  &@.0,&@.14          CLEAR THE NEW DSA
         OR    &@.0,&@.15          RESTORE HI-BYTE TO NAB POINTER REG
         LR    &@.1,&@.0           COPY NEXT AVAILABLE BLOCK POINTER
         L     &@.15,72(,&@.13)    GET LIBRARY WORKSPACE POINTER
         STM   &@.15,&@.1,72(&@.14) STORE INTO OUR NEW DSA
         ST    &@.5,88(,&@.14)     STORE PASSED PARAMETERS POINTER
         ST    &@.13,4(,&@.14)     BACK CHAIN THE DSA
         L     &@.1,24(,&@.13)     RESTORE PLIST POINTER
         LR    &@.13,&@.14         --> NEW DSA (R14 PURIFIED BY MVCL)
         MVI   0(&@.13),X'80'      SET FOR -
         MVI   1(&@.13),X'00'       PLI -
         MVI   86(&@.13),X'91'       ERROR -
         MVI   87(&@.13),X'C0'        HANDLING
.*
&N2      SETC  ''
         AIF   ('&#BS(13)' EQ '').PSKPUSE
&C1      SETA  13
         AIF   ('&BASES' NE '*').PBSEADR
&OLDBASE SETB  (1)
         BALR  &#BS(13),0          LOAD TEMP LOCAL BASE
         L     &#BS(13),E&#.BSE-*(,&#BS(13)) LOAD 1ST PROGRAM BASE
         AGO   .PEQUATE
.PBSEADR ANOP
&N2      SETC  'E&#.BSE'
&#BS(14) SETC  '&N2'
         BALR  &#BS(13),0          LOAD 1ST PROGRAM BASE
.PEQUATE ANOP
&W2      SETC  '&#BS(&C1)'
&C1      SETA  &C1-1
         AIF   (&C1 EQ 1 OR '&#BS(&C1)' EQ '').PENDLA
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT PROGRAM BASE
&N2      SETC  ''
         AGO   .PEQUATE
.PENDLA  #USING ,
.PSKPUSE ANOP
.*
         AIF   (NOT &LENGTH AND NOT &OLDBASE).ENDCHK
         AIF   ('&#BS(13)' NE '').PGOTBAS
&N2      BALR  &@.15,0             LOAD TEMP BASE
&N2      SETC  ''
         B     E&#.END-*(,&@.15)   SKIP DATA AREA
         AGO   .PDFNDAT
.PGOTBAS ANOP
&N2      B     E&#.END             SKIP DATA AREA
&N2      SETC  ''
.PDFNDAT AIF   (NOT &LENGTH).PNOLEN
E&#.LEN  DC    A((&SAVTYPE(2)+7)/8*8) DESIRED DSA LENGTH
.PNOLEN  AIF   (NOT &OLDBASE).PNOOBAS
E&#.BSE  DC    A(&#BS(14))         OLD BASE ADDRESS
.PNOOBAS ANOP
.*
.END     ANOP
E&#.END  DS    0H
.ENDCHK  AIF   ('&N2' EQ '').MEND
&N2      DS    0H
.MEND    MEND
         MACRO
&NME     #EXIT &R,&PFIX=,&RC=,&MODE=LEAVE                     DBC 04/85
.*
.*
.*                                                            DBC 04/85
.* LAST CHANGE DATE - APRIL 29, 1985                          DBC 04/85
.*                  - ADDED SUPPORT FOR THE "MODE=" OPERAND.  DBC 04/85
.*                                                            DBC 10/84
.* LAST CHANGE DATE - OCTOBER 1, 1984                         DBC 10/84
.*                  - DELETED CODE THAT SET A X'FF' "RETURN   DBC 10/84
.*                    INDICATOR" IN THE HI-BYTE OF DSAR14.    DBC 10/84
.*                    IT WAS NOT APPROPRIATE FOR MVS/XA.      DBC 10/84
.*                                                            DBC 06/84
.* LAST CHANGE DATE - JUNE 11, 1984                           DBC 06/84
.*                  - ADDED SUPPORT FOR "SAVTYPE=NONE" ON THE DBC 06/84
.*                    #ENTER MACRO.                           DBC 06/84
.*                  - FOR REENTRANT EXIT LINKAGE, CHANGED     DBC 06/84
.*                    THE FREEMAIN SO THAT IT WOULD NO        DBC 06/84
.*                    LONGER GENERATE AN INLINE PLIST.        DBC 06/84
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - CHANGE THE MACRO NAME FROM $EXIT TO #EXIT
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - ADDED EXIT LINKAGE FOR A PLI ENVIRONMENT.
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - OCTOBER 3, 1978
.*                  - FOR REENTRANT SAVE AREAS, THE FREEMAIN HAS BEEN
.*                    CHANGED SO THAT MORE THAN 4K CAN BE FREED.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 10, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES EITHER OS STANDARD OR PLI STANDARD EXIT
.* LINKAGE. IT WAS WRITTEN TO PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE
.* METHOD FOR GENERATING SUCH LINKAGE. IN ADDITION, FOR ALL POSSIBLE
.* OPERAND COMBINATIONS, #EXIT WILL GENERATE THE ABSOLUTE MINIMUM
.* AMOUNT OF CODE NECESSARY.
.*
.*   THE #EXIT MACRO WILL GENERATE THE FOLLOWING:
.* - CODE TO LOAD REGISTER 13 WITH A POINTER TO THE HIGHER OS SAVE AREA
.*   OR PLI DATA STORAGE AREA
.* - CODE TO RELEASE (VIA FREEMAIN) THE LOWER SAVE AREA IF THE
.*   PRECEEDING #ENTER MACRO CALL GENERATED A REENTRANT EXPANSION
.* - CODE TO RESTORE ANY PARTICULAR SET OF REGISTERS
.* - CODE TO LOAD REGISTER 15 WITH A RETURN CODE THAT IS EITHER AN
.*   ABSOLUTE VALUE OR A VALUE PRELOADED INTO ANY REGISTER
.* - CODE TO RETURN TO THE CALLING PROGRAM VIA REGISTER 14
.*
.*   THE NAME FIELD
.* USE THIS FIELD TO ASSIGN A STATEMENT LABEL TO THE FIRST MACHINE
.* INSTRUCTION OF THE EXPANSION. IF THE NAME FIELD IS OMITTED, THEN NO
.* STATEMENT LABEL IS ASSIGNED.
.*
.*   THE FIRST POSITIONAL OPERAND
.* THIS OPERAND MUST CONSIST OF A SUB-LIST OF ANY NUMBER OF ENTRIES.
.* EACH ENTRY MAY BE EITHER A SINGLE REGISTER NAME OR A PARENTHESIZED
.* PAIR (SEPERATED BY A COMMA) OF REGISTER NAMES - E.G.
.* " (1,11,(7,9),5) ". EACH SINGLE REGISTER NAME SPECIFIES A PARTICULAR
.* REGISTER TO BE RESTORED FROM THE HIGHER SAVE AREA. EACH
.* PARENTHESIZED PAIR OF REGISTER NAMES SPECIFIES A RANGE OF REGISTERS
.* TO BE RESTORED. THUS, THE ABOVE EXAMPLE WOULD CAUSE REGISTERS 1, 5,
.* 7, 8, 9, AND 11 TO BE RESTORED.
.*   SOME NOTES AND WARNINGS:
.* - A REQUEST TO RESTORE REGISTER 13 IS MEANINGLESS AND IS IGNORED.
.* - IF REGISTER 14 IS TO BE LOADED WITH THE RETURN ADDRESS FOUND IN
.*   THE HIGHER SAVE AREA, THEN YOU MUST SPECIFICALLY REQUEST THAT IT
.*   (REGISTER 14) BE RESTORED; OTHERWISE, WHATEVER VALUE IS FOUND IN
.*   REGISTER 14 PRIOR TO THE MACRO CALL WILL BE USED FOR THE RETURN
.*   ADDRESS.
.* - TO RESTORE ALL REGISTERS FROM 14 THROUGH 12, YOU MUST CODE
.*   " ((14,12)) ". CODING " (14,12) " WILL CAUSE ONLY REGISTERS 14 AND
.*   12 TO BE RESTORED.
.* - THE NUMERIC VALUES OF ALL REGISTER NAMES USED IN THIS OPERAND MUST
.*   BE DETERMINABLE AT MACRO PASS TIME. THUS, EACH REGISTER NAME USED
.*   MUST BE EITHER A SELF-DEFINING NUMERIC OR A NAME DEFINED VIA THE
.*   #REGS MACRO.
.* - IF ONLY A SINGLE REGISTER IS TO BE RESTORED, THEN IT NEED NOT BE
.*   ENCLOSED IN PARENTHESES.
.* - IF THE FIRST POSITIONAL OPERAND IS OMITTED, THEN NO REGISTERS ARE
.*   RESTORED.
.*
.*   THE RC= OPERAND
.* THIS OPERAND MUST CONSIST OF A SINGLE VALUE EITHER WITHIN OR NOT
.* WITHIN PARENTHESES. IF ENCLOSED WITHIN PARENTHESES, THEN THE VALUE
.* IS TREATED AS THE NAME OF A REGISTER CONTAINING A RETURN CODE. IF
.* NOT ENCLOSED WITHIN PARENTHESES, THEN THE VALUE IS TREATED AS BEING
.* THE RETURN CODE ITSELF.
.*   IF THE RC= OPERAND SPECIFIES A REGISTER NAME, THEN:
.* - THE VALUE OF THAT NAME NEED NOT BE DETERMINABLE AT MACRO PASS
.*   TIME;
.* - THE REGISTER NAME MAY IDENTIFY ANY REGISTER WHATSOEVER REGUARDLESS
.*   OF WHICH REGISTERS ARE TO BE RESTORED SINCE IN CASES OF POTENTIAL
.*   CONFLICT, THE RETURN CODE IS COPIED INTO REGISTER 15 PRIOR TO
.*   REGISTER RESTORATION;
.* - IT IS ILLOGICAL FOR THE RC= OPERAND TO SPECIFY REGISTER 13.
.* NOTE THAT IF THE RC= OPERAND IS SPECIFIED BUT THE FIRST POSITIONAL
.* OPERAND INDICATES THAT REGISTER 15 IS ALSO TO BE RESTORED, THEN
.* REGISTER 15 IS NOT RESTORED. INSTEAD, IT IS LOADED WITH THE RETURN
.* CODE VALUE.
.*   IF THE RC= OPERAND IS OMITTED, THEN NO CODE IS GENERATED TO LOAD
.* REGISTER 15 WITH A RETURN CODE.
.*                                                            DBC 04/85
.*   THE MODE= OPERAND                                        DBC 04/85
.* THIS FOR MVS/XA. IT CONTROLS WHETHER OR NOT THE #EXIT      DBC 04/85
.* MACRO IS TO GENERATE CODE TO RESTORE THE CALLER'S          DBC 04/85
.* ADDRESSING MODE. THE DEFAULT IS NOT TO DO SO.              DBC 04/85
.*                                                            DBC 04/85
.* - MODE=LEAVE                                               DBC 04/85
.*   THIS IS THE DEFAULT. NO EXPLICIT ATTEMPT IS MADE TO      DBC 04/85
.*   RESTORE THE CALLER'S ADDRESSING MODE.                    DBC 04/85
.*                                                            DBC 04/85
.* - MODE=RESTORE                                             DBC 04/85
.*   THE #EXIT MACRO ATTEMPTS TO RESTORE THE CALLER'S         DBC 04/85
.*   ADDRESSING MODE BASED ON THE HI-ORDER BIT OF THE         DBC 04/85
.*   CALLER'S R14.                                            DBC 04/85
.*
.*   THE PFIX= OPERAND
.* THE #EXIT MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE
.* EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*   CONSIDERATIONS
.* THE #EXIT MACRO WILL GENERATE AN EXPANSION THAT WILL ACCURATELY
.* RESTORE ALL DESIRED REGISTERS AND SET THE RETURN CODE REGUARDLESS OF
.* WHETHER OR NOT A FREEMAIN SVC IS ISSUED TO RELEASE THE LOWER SAVE
.* AREA AND REGUARDLESS OF THE RELATIONSHIP BETWEEN THE RC= OPERAND AND
.* THE SET OF REGISTERS RESTORED.
.*
.*
.*
.* INNER MACROS USED - #REGS #TEST AND FREEMAIN
.*
         GBLA  &#TESERR
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     DBC 06/84
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP
         LCLA  &C1,&R1,&R2,&W1,&W2,&ERRCODE
         LCLB  &RCLA,&RCST,&RSW(16)
         LCLC  &LNME,&@,&RG(16),&REGNME,&RG2SAVE,&RG4SAVE,&RG5SAVE
         LCLC  &C,&#                                          DBC 06/84
&#       SETC  '&SYSNDX'                                      DBC 06/84
&LNME    SETC  '&NME'
.*
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
&C1      SETA  0                                              DBC 06/84
.RLP     AIF   (&C1 GE N'&R).RFIN
&C1      SETA  &C1+1
         #TEST DCODE=&R(&C1)
         AIF   (&#TESERR NE 0).END
&W1      SETA  &#TESRET(1)
         AIF   (&W1 EQ 0).RLP
&REGNME  SETC  '&#TESRET(2)'
         #TEST REGS=&REGNME
&ERRCODE SETA  1
         AIF   (&#TESERR NE 0).REGERR
.REGOK1  ANOP
&R1      SETA  &#TESRET(1)
&R2      SETA  &R1+3-&R1/14*16
&RG(&R2) SETC  '&REGNME'
&RSW(&R1+1) SETB (1)
         AIF   (&W1 EQ 1).RLP
         AIF   (&W1 EQ 2).TWOND
         MNOTE 4,'"&R(&C1)" CONTAINS EXCESS INFORMATION.'
.TWOND   ANOP
&REGNME  SETC  '&#TESRET(3)'
         #TEST REGS=&REGNME
&ERRCODE SETA  2
         AIF   (&#TESERR NE 0).REGERR
.REGOK2  ANOP
&W2      SETA  &#TESRET(1)
&RSW(&W2+1) SETB (1)
.ENTLP   AIF   (&R1 EQ &#TESRET(1)).ENTEND
&R1      SETA  &R1+1
&R2      SETA  &R2+1
         AIF   (&R1 LE 15).R1OK
&R1      SETA  0
.R1OK    AIF   (&R2 LE 16).R2OK
&R2      SETA  1
.R2OK    ANOP
&RG(&R2) SETC  '&@&R1'
         AGO   .ENTLP
.ENTEND  ANOP
&RG(&R2) SETC  '&REGNME'
         AGO   .RLP
.REGERR  AIF   (&#TESRET(1) GE 0 OR &#TESRET(1) LT 0).REGVALU
         MNOTE 0,'THE ABOVE ERROR IS NOT DUE TO A BUG IN THE MACRO.'
         MNOTE 8,'THE VALUE OF "&REGNME" IS NOT DETERMINABLE.'
         MEXIT
.REGVALU AIF   (&#TESRET(1) GE 0 AND &#TESRET(1) LE 15).REGOK
         MNOTE 8,'THE VALUE OF "&REGNME" IS OUTSIDE THE RANGE OF 0 ... *
               15'
         MEXIT
.REGOK   AIF   (&ERRCODE EQ 1).REGOK1
         AGO   .REGOK2
.RFIN    AIF   (NOT &#ENTPLI).RGOK
         AIF   (NOT &RSW(1)).RG0OK
         MNOTE 4,'&RG(3) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'
.RG0OK   AIF   (NOT &RSW(2)).RG1OK
         MNOTE 4,'&RG(4) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'
.RG1OK   ANOP
&RG(3)   SETC  ''
&RG(4)   SETC  ''
.RGOK    ANOP
.*
&RG(16)  SETC  ''
.*
         AIF   (K'&RC EQ 0).NORC
&RG2SAVE SETC  '&RG(2)'
&RG(2)   SETC  ''
         AIF   ('&RG2SAVE' NE '').RG2SOK
&RG2SAVE SETC  '&@.15'
.RG2SOK  ANOP
.*
         AIF   (NOT &RSW(16)).NOPRBLM
         MNOTE 4,'&RG2SAVE SET TO THE RETURN CODE - NOT RESTORED.'
.NOPRBLM ANOP
.*
         AIF   ('&RC' EQ '&RC(1)').RCNTRG
         #TEST REGS=&RC(1)
         AIF   (&#TESERR NE 0).LOADRC
         AIF   ('&#TESRET(1)' NE '13').RCOK
         MNOTE 4,'"RC=&RC" IS ILLOGICAL.'
.RCOK    AIF   ('&#TESRET(1)' EQ '15').NORC
.LOADRC  AIF   ('&RG(1)' EQ '' OR '&RG(3)' EQ '' OR &#ENTPLI).RCLR
&RCST    SETB  (1)
         AGO   .NORC
.RCLR    ANOP
&LNME    LR    &@.15,&RC(1)        LOAD THE RETURN CODE
&LNME    SETC  ''
         AGO   .NORC
.RCNTRG  ANOP
&RCLA    SETB  (1)
         AIF   ('&RG(3)' EQ '' OR '&RG(1)' EQ '').NORC
&RG(2)   SETC  '&RG2SAVE'
.NORC    ANOP
.*
         AIF   (NOT &#ENTPLI).NOTPLI2
&LNME    LR    &@.0,&@.13          COPY OUR DSA POINTER
&LNME    SETC  ''
.NOTPLI2 ANOP
.*
         AIF   (NOT &#ENTRNT OR '&RG(4)' EQ '').NOLRR1
&LNME    LR    &@.1,&@.13          GET SAVE AREA ADDRESS FOR FREEMAIN
&LNME    SETC  ''
.NOLRR1  ANOP
.*
         AIF   (&#ENTNUN).NOLSA                               DBC 06/84
&LNME    L     &@.13,4(,&@.13)     POINT TO THE HIGHER SAVE AREA
&LNME    SETC  ''                                             DBC 06/84
.NOLSA   ANOP                                                 DBC 06/84
         AIF   (NOT &RCST).NORCST
&LNME    ST    &RC(1),16(,&@.13)   STORE THE RC FOR LATER     DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
&RG(2)   SETC  '&RG2SAVE'
.NORCST  ANOP
.*
         AIF   (NOT &#ENTRNT).NTRENT
         AIF   (NOT &RCLA).RG2OK
&RG(2)   SETC  'X'
         AIF   ('&RG(1)&RG(3)' NE '').RG2OK
&RG(2)   SETC  ''
.RG2OK   ANOP
&RG4SAVE SETC  '&RG(4)'
&RG5SAVE SETC  '&RG(5)'
&RG(5)   SETC  'X'
&C1      SETA  0
.STMLP   AIF   (&C1 GE 4).STMEND
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' NE '').STMLP
&R1      SETA  &C1+13-(&C1+13)/16*16
&R2      SETA  &R1-1
&W1      SETA  &C1*4+8
.STMLP2  ANOP
&R2      SETA  &R2+1
         AIF   (&R2 LE 15).STMR2OK
&R2      SETA  0
.STMR2OK ANOP
&RG(&C1) SETC  '&@&R2'
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' EQ '').STMLP2
         AIF   (&R1 EQ &R2).ST
&C       SETC  '&@&R1,&@&R2,&W1.(&@.13)'                      DBC 06/84
&LNME    STM   &C                  SAVE AGAINST FREEMAIN      DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .STMLP
.ST      ANOP                                                 DBC 06/84
&LNME    ST    &@&R1,&W1.(,&@.13)  SAVE AGAINST FREEMAIN      DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .STMLP
.STMEND  ANOP
&RG(5)   SETC  '&RG5SAVE'
.GTR1M   AIF   ('&RG4SAVE' NE '').NOGTR1
&LNME    L     &@.1,8(,&@.13)      GET RSA PTR FOR FREEMAIN   DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
.NOGTR1  ANOP                                                 DBC 06/84
&LNME    L     &@.0,E&#.LEN        GET RSA LEN (AND SUBPOOL)  DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         MNOTE '         FREEMAIN R,A=(1),LV=(0)'             DBC 06/84
         FREEMAIN R,A=(1),LV=(0)                              DBC 06/84
.NTRENT  ANOP
.*
&C1      SETA  0
.LMLP    AIF   (&C1 GE 16).SETRCM
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' EQ '').LMLP
&R1      SETA  &C1
&W1      SETA  &C1*4+8
.LMLP2   ANOP
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' NE '').LMLP2
         AIF   (&R1 EQ &C1-1).L
&C       SETC  '&RG(&R1),&RG(&C1-1),&W1.(&@.13)'              DBC 06/84
&LNME    LM    &C                  RESTORE REGISTERS          DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .LMLP
.L       ANOP                                                 DBC 06/84
&C       SETC  '&RG(&R1),&W1.(,&@.13)'                        DBC 06/84
&LNME    L     &C                  RESTORE THE REGISTER       DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .LMLP
.SETRCM  ANOP
.*
         AIF   (NOT &RCLA).RETURN
         AIF   ('&RC' EQ '0').SR
&LNME    LA    &@.15,&RC           GET THE RETURN CODE        DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .RETURN
.SR      ANOP                                                 DBC 06/84
&LNME    SLR   &@.15,&@.15         ZERO THE RETURN CODE       DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
.RETURN  ANOP
.*
         AIF   (NOT &#ENTPLI).NOTPLI3
&LNME    BALR  &@.1,&@.14          RETURN TO CALLER           DBC 06/84
         MEXIT
.NOTPLI3 ANOP
.*
         AIF   ('&MODE '(1,1) EQ 'R').RETBSM                  DBC 04/85
         AIF   ('&MODE '(1,1) EQ 'L').RETBR                   DBC 04/85
         MNOTE 8,'"MODE=&MODE" NOT RECOGNIZED.'               DBC 04/85
         MNOTE *,'"MODE=RESTORE" OR "MODE=LEAVE" WAS EXPECTED.' C 04/85
.*                                                            DBC 04/85
.RETBR   ANOP                                                 DBC 04/85
&LNME    BR    &@.14               RETURN                     DBC 10/84
&LNME    SETC  ''                                             DBC 04/85
         AGO   .MODEZ                                         DBC 04/85
.RETBSM  ANOP                                                 DBC 04/85
&LNME    BSM   0,&@.14             RESTORE AMODE AND RETURN   DBC 04/85
&LNME    SETC  ''                                             DBC 04/85
.MODEZ   ANOP                                                 DBC 04/85
.*                                                            DBC 06/84
         AIF   (NOT &#ENTRNT).END                             DBC 06/84
         AIF   ('&#ENTSP' EQ '').NOSPOOL                      DBC 06/84
         DS    0F                  ALIGN                      DBC 06/84
&C       SETC  'AL1(&#ENTSP),AL3(&#ENTSIZ)'                   DBC 06/84
E&#.LEN  DC    &C                  RSA SUBPOOL AND LENGTH     DBC 06/84
         MEXIT                                                DBC 06/84
.NOSPOOL ANOP
E&#.LEN  DC    A(&#ENTSIZ)         RSA LENGTH                 DBC 06/84
.END     MEND
         MACRO
&NME     #PUT  &MSG,&PFIX=,&SUBAD=,&MF=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $PUT TO #PUT.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - JULY 18, 1980
.*                  - INDIRECT ADDRESSING IS NOW INDICATED BY A
.*                    TRAILING PERCENT SIGN (%) RATHER THAN A LEADING
.*                    ONE.
.*
.* LAST CHANGE DATE - JANUARY 12, 1977
.*                  - HANDLING OF THE SUBAD= OPERAND IS REWRITTEN.
.*                  - MAILING ADDRESS CHANGE.
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
         GBLA  &#TESERR
         GBLC  &#PUTSUB
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &@,&C1,&N
         AIF   ('&SUBAD' EQ '').SUBOK
         AIF   ('&SUBAD(1)' NE '&SUBAD').SUBOK
&#PUTSUB SETC  '&SUBAD'
.SUBOK   AIF   ('&MF(1)' EQ 'INIT').MEND
&N       SETC  '&NME'
.*
&@       SETC  '&PFIX'
         AIF   ('&PFIX' NE '').PFIXOK
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.PFIXOK  #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
         AIF   ('&MSG(1)' EQ '&MSG').NOTREG
         #TEST REGS=&MSG(1)
         AIF   (&#TESERR NE 0).LR
         AIF   (&#TESRET(1) EQ 1).BAL
.LR      ANOP
&N       LR    &@.1,&MSG(1)        --> MESSAGE LENGTH FIELD
&N       SETC  ''
         AGO   .BAL
.NOTREG  AIF   ('&MSG' EQ '').BAL
         AIF   ('&MSG'(K'&MSG,1) EQ '%').MSGINDR
&N       LA    &@.1,&MSG-1         --> MESSAGE LENGTH FIELD
         AGO   .MSGOK
.MSGINDR ANOP
&C1      SETC  '&MSG'(1,K'&MSG-1)
&N       L     &@.1,=A(&C1-1)      --> MESSAGE LENGTH FIELD
.MSGOK   ANOP
&N       SETC  ''
.BAL     AIF   ('&SUBAD(1)' NE '&SUBAD').BALR
         AIF   ('&#PUTSUB'(1,1) EQ '%').BALINDR
&N       BAL   &@.14,&#PUTSUB      GO DISPLAY THE MESSAGE
         AGO   .MEND
.BALR    ANOP
&N       BALR  &@.14,&SUBAD(1)     GO DISPLAY THE MESSAGE
         AGO   .MEND
.BALINDR ANOP
&A1      SETA  0
.LP1     ANOP
&A1      SETA  &A1+1
         AIF   ('&#PUTSUB'(1,&A1) NE '&#PUTSUB').LP1
&C1      SETC  '&#PUTSUB'(2,&A1-1)
&N       L     &@.15,=A(&C1)       --> MESSAGE PRINTING ROUTINE
         BALR  &@.14,&@.15         GO DISPLAY THE MESSAGE
.MEND    MEND
         MACRO
         #REGS &GEN=YES
.*
.*
.*                                                            09/84 DBC
.* LAST CHANGE DATE - SEPTEMBER 11, 1984                      09/84 DBC
.*                  - ADDED SUPPORT FOR PL/S STYLE REGISTER   09/84 DBC
.*                    NAMES (@00, @01, ---, @15).             09/84 DBC
.*                  - ATTEMPTS TO MULTIPLY DEFINE THE SAME    09/84 DBC
.*                    NAME TO THE SAME VALUE WILL NOW BE      09/84 DBC
.*                    SUPPRESSED WITHOUT ERROR.               09/84 DBC
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $REGS TO #REGS
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - SUPPORT FOR THE "GEN={YES|NO}" OPERAND IS ADDED.
.*
.* LAST CHANGE DATE - DECEMBER 5, 1977
.*                  - SINGLE REGISTER EQUATES NOW LINE UP CORRECTLY IN
.*                    THE LISTING.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - APRIL 1, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THE #REGS MACRO HAS TWO FUNCTIONS. ITS PRIMARY
.* PURPOSE IS TO PROVIDE A SIMPLE MEANS OF DEFINING SETS
.* OF REGISTER NAME EQUATES. ITS SECONDARY PURPOSE IS TO
.* CREATE AN INTERNAL TABLE OF ALL REGISTER NAMES
.* COUPLED WITH THEIR NUMERIC VALUES. THIS TABLE IS THEN
.* MADE AVAILABLE TO CERTAIN OTHER MACROS (E.G. #ENTER
.* AND #EXIT) SO THAT THEY CAN DETERMINE (IF NECESSARY)
.* THE VALUES OF ANY REGISTER NAMES THAT MIGHT BE PASSED
.* TO THEM AS OPERANDS AND SO THAT THEY CAN USE REGISTER
.* NAME EQUATES INSTEAD OF REGISTER NUMBERS IN THE CODE
.* THAT THEY GENERATE. THIS IS SO THAT ALL REFERENCES TO
.* REGISTERS WILL BE INDICATED IN THE ASSEMBLER'S CROSS
.* REFERENCE LISTING.
.*                                                            09/84 DBC
.*   THE #REGS MACRO CAN BE USED ANY NUMBER OF TIMES IN
.* AN ASSEMBLY. EACH TIME THAT IT IS USED, IT CAN BE
.* GIVEN ANY NUMBER OF POSITIONAL OPERANDS. EACH OPERAND      09/84 DBC
.* CAN BE EITHER A SINGLE TERM OR A SUB-LIST OF TWO
.* TERMS.
.*                                                            09/84 DBC
.*   IF AN OPERAND IS A SUB-LIST OF TWO TERMS, THEN THE
.* #REGS MACRO TREATS IT AS A REQUEST TO DEFINE A SINGLE
.* REGISTER NAME AND IT GENERATES A STATEMENT OF THE
.* FORM: " TERM1 EQU TERM2 ". THE FIRST TERM MUST BE ANY
.* VALID NAME NOT PREVIOUSLY DEFINED. THE SECOND TERM
.* MUST BE ANY SELF-DEFINING TERM OR ANY REGISTER NAME
.* THAT HAS BEEN PREVIOUSLY DEFINED BY THIS OR A
.* PREVIOUS #REGS MACRO. IT SHOULD NOT BE AN EXPRESSION,
.* AND IT SHOULD NOT BE ANY NAME NOT PREVIOUSLY DEFINED.
.* THE VALUE OF THE SECOND TERM SHOULD FALL IN THE RANGE
.* OF 0 THROUGH 15. IF THE SECOND TERM FITS THESE
.* REQUIREMENTS, THEN THE REGISTER NAME IS SAVED IN AN
.* INTERNAL TABLE FOR USE BY OTHER MACROS.
.*                                                            09/84 DBC
.*   IF AN OPERAND IS ONLY A SINGLE TERM, THEN THE MACRO
.* TREATS IT AS A REQUEST TO DEFINE A FULL SET OF
.* REGISTER NAME EQUATES WITH THE GIVEN TERM USED AS THE
.* REGISTER NAME PREFIX. AS AN EXAMPLE, ASSUME THAT THE
.* OPERAND IS "GPR". IN THIS CASE, THE #REGS MACRO WILL
.* GENERATE EQUATES DEFINING GPR0, GPR1, ---, GPR15 AND
.* GPRA, GPRB, ---, GPRF (EQUAVALENT TO GPR10, GPR11,
.* ---, GPR15). IN ADDITION, THE GENERATED REGISTER
.* NAMES ARE SAVED IN AN INTERNAL TABLE FOR USE BY OTHER
.* MACROS.
.*                                                            09/84 DBC
.*   A SPECIAL CASE. IF THE SINGLE TERM IS AN "AT SIGN"       09/84 DBC
.* (@), THEN THEN THE GENERATED NAMES WILL BE @00, @01,       09/84 DBC
.* ---, @15. THIS CONFORMS TO PL/S CONVENTIONS.               09/84 DBC
.*                                                            09/84 DBC
.*   IF #REGS IS CALLED WITHOUT OPERANDS, THEN IT IS
.* TREATED AS A REQUEST TO GENERATE A FULL SET OF
.* EQUATES USING "R" AS THE PREFIX.
.*
.*
.*
.* GEN={YES|NO}    (DEFAULT IS GEN=YES)
.*       THIS CONTROLS WHETHER OR NOT THIS MACRO ACTUALLY GENERATES THE
.*       'EQU' STATEMENTS THAT CREATE THE DESIRED REGISTER NAMES. IF
.*       "GEN=NO" IS GIVEN, THEN PRESUMEDLY THE DESIRED NAMES ARE
.*       GENERATED ELSEWHERE. IN THIS CASE THE ONLY FUNCTION PERFORMED
.*       BY THIS MACRO IS TO UPDATE INTERNAL TABLES.
.*
.*
.*
.* INNER MACROS USED - #TEST
.*
         GBLA  &#REGVAL(255)
         GBLA  &#TESERR
         GBLC  &#REGNME(255)
         GBLC  &#TESRET(20)
         LCLA  &ARG,&CTR,&NEXT,&A1
         LCLB  &B1
         LCLC  &LPFX,&C1
&NEXT    SETA  0
.LP1     AIF   (&NEXT GE 255).END1
&NEXT    SETA  &NEXT+1
         AIF   ('&#REGNME(&NEXT)' NE '').LP1
&NEXT    SETA  &NEXT-1
.END1    ANOP
&ARG     SETA  0
.LP2     AIF   (&ARG GE N'&SYSLIST).DONE
&ARG     SETA  &ARG+1
         AIF   (N'&SYSLIST(&ARG) EQ 0).LP2
         AIF   (&NEXT LT 255).NOTFULL
         MNOTE 4,'THE REGISTER NAME SAVE TABLE IS FULL.'
         MNOTE 4,'THE MAXIMUM CAPACITY IS 255 ENTRIES.'
.NOTFULL ANOP
&C1      SETC  '&SYSLIST(&ARG,1)'
         AIF   (N'&SYSLIST(&ARG) GE 2).ONEREG
.NULL    ANOP
&B1      SETB  (1)
         #TEST PFIX=
&LPFX    SETC  '&#TESRET(1)'
.*                                                            09/84 DBC
         AIF   ('&C1' NE '@').NOT@                            09/84 DBC
&CTR     SETA  0-1                                            09/84 DBC
.LP@     AIF   (&CTR EQ 15).END@                              09/84 DBC
&CTR     SETA  &CTR+1                                         09/84 DBC
&C1      SETC  '0&CTR'                                        09/84 DBC
&C1      SETC  '&C1'(K'&C1-1,2)                               09/84 DBC
         #REGS (@&C1,&LPFX&CTR)                               09/84 DBC
         AGO   .LP@                                           09/84 DBC
.END@    AIF   (&NEXT GE 255).LP2                             09/84 DBC
&NEXT    SETA  &NEXT+1                                        09/84 DBC
         AIF   ('&#REGNME(&NEXT)' NE '').END@                 09/84 DBC
&NEXT    SETA  &NEXT-1                                        09/84 DBC
         AGO   .LP2                                           09/84 DBC
.NOT@    ANOP                                                 09/84 DBC
.*                                                            09/84 DBC
&CTR     SETA  0
.LP2A    AIF   (&CTR GE &NEXT).PXSAVE
&CTR     SETA  &CTR+1
         AIF   (&#REGVAL(&CTR) LT 16 OR '&#REGNME(&CTR)' NE '&C1').LP2A
         AGO   .LP2                                           09/84 DBC
.PXSAVE  AIF   (&NEXT GE 255).NOSAVE1                         09/84 DBC
&NEXT    SETA  &NEXT+1
&#REGNME(&NEXT) SETC '&C1'
&#REGVAL(&NEXT) SETA 16
.NOSAVE1 AIF   ('&GEN(1)'(1,1) NE 'Y').LP2
&CTR     SETA  0
.LP3     AIF   (&CTR GT 15).HEX
&C1&CTR  EQU   &LPFX&CTR
&CTR     SETA  &CTR+1
         AGO   .LP3
.HEX     ANOP
&C1.A    EQU   &C1.10
&C1.B    EQU   &C1.11
&C1.C    EQU   &C1.12
&C1.D    EQU   &C1.13
&C1.E    EQU   &C1.14
&C1.F    EQU   &C1.15
         AGO   .LP2
.ONEREG  ANOP
&B1      SETB  (1)
         AIF   (N'&SYSLIST(&ARG) EQ 2).NOXCESS
         MNOTE 4,'"&SYSLIST(&ARG)" CONTAINS EXCESS INFORMATION.'
         MNOTE 4,'THE EXCESS WILL BE IGNORED.'
.NOXCESS #TEST REGS=&SYSLIST(&ARG,2)
         AIF   (&#TESERR EQ 0).REGOK
         MNOTE 4,'THE VALUE OF "&SYSLIST(&ARG,2)" IS NOT DETERMINABLE.'
         AGO   .REGEQU
.REGOK   ANOP                                                 09/84 DBC
&A1      SETA  &#TESRET(1)
&CTR     SETA  0
.LP3A    AIF   (&CTR GE &NEXT).RGSAVE
&CTR     SETA  &CTR+1
         AIF   (&#REGVAL(&CTR) NE &A1 OR '&#REGNME(&CTR)' NE '&C1').LP3*
               A
         AGO   .LP2                                           09/84 DBC
.RGSAVE  AIF   (&NEXT GE 255).REGEQU                          09/84 DBC
&NEXT    SETA  &NEXT+1
&#REGNME(&NEXT) SETC '&C1'
&#REGVAL(&NEXT) SETA &A1
.REGEQU  AIF   ('&GEN(1)'(1,1) NE 'Y').LP2
&C1      EQU   &SYSLIST(&ARG,2)
         AGO   .LP2
.DONE    ANOP
&C1      SETC  'R'
         AIF   (NOT &B1).NULL
         MEND
         MACRO
&N       #TEST &DCODE=OMITTED,&MEXCL=,&NUM=OMITTED,                    *
               &PFIX=OMITTED,&REGS=,&SIZE=,&GEN=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - MARCH 16, 1983
.*                  - COMMENTARY CHANGES
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - DECEMBER 4, 1981
.*                  - THE "SIZE=" FUNCTION HAS BEEN ENHANCED
.*                    TO INCLUDE SUPPORT FOR THE "NE"
.*                    RELATION.
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $TEST TO #TEST
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - THE "NUM" FUNCTION NO LONGER ISSUES AN
.*                    ERROR MESSAGE WHEN IT ENCOUNTERS A
.*                    NON-DIGIT. IT ONLY SETS A RETURN CODE
.*                    OF 16 IN &#TESERR.
.*
.* LAST CHANGE DATE - APRIL 24, 1978
.*                    THE GEN=EBCDIC FUNCTION HAS BEEN ADDED
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - SEPTEMBER 10, 1976
.*                    THE &SIZE= OPERAND SUPPORT IS CHANGED
.*                    TO REQUIRE THREE SUB-OPERANDS WITH THE
.*                    SECOND SPECIFYING ONE OF THE RELATION
.*                    OPERATIONS: LT, LE, EQ, GE, OR GT.
.*                    NOTE, THIS IS NOT COMPATIBLE WITH THE
.*                    PREVIOUS IMPLEMENTATION.
.*
.* LAST CHANGE DATE - FEBRUARY 10, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS
.* CONCERNING IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*                                 GENERAL INFORMATION
.*   THE #TEST MACRO IS INTENDED TO BE USED AS AN INNER
.* MACRO. IT PERFORMS A NUMBER OF DIFFERENT TESTS AND
.* MANIPULATIONS WHICH ARE WHOLELY INDEPENDANT OF EACH OTHER.
.* FOR EACH OF THESE FUNCTIONS, INPUT MIGHT BE SPECIFIED
.* EITHER VIA MACRO OPERANDS OR BOTH MACRO OPERANDS AND GLOBAL
.* SYMBOLS (DEPENDING UPON THE FUNCTION). OUTPUT IS USUALLY
.* COMMUNICATED VIA THE GLOBAL SYMBOLS &#TESERR AND &#TESRET.
.*   &#TESERR IS A SCALER SETA SYMBOL WHICH IS USED IN A
.* MANNER SIMILAR TO A PROGRAM'S COMPLETION CODE TO
.* COMMUNICATE A GROSS INDICATION OF AN UNUSUAL OR ERROR
.* CONDITION. IF UPON RETURN FROM #TEST &#TESERR EQUALS ZERO,
.* THEN THE MACRO FUNCTIONED "OK"; OTHERWISE, THE VALUE OF
.* &#TESERR VARIES DIRECTLY WITH THE SERIOUSNESS OF THE
.* UNUSUAL OR ERROR CONDITION, AND IT IS ALWAYS SET TO
.* REFLECT THE MOST SERIOUS CONDITION ENCOUNTERED DURING A
.* PARTICULAR INVOCATION OF THE #TEST MACRO.
.*   &#TESRET IS A SETC ARRAY WHICH IS USED TO CONTAIN
.* RETURN VALUES FOR THOSE FUNCTIONS FOR WHICH RETURN
.* VALUES ARE APPROPIATE. EACH ELEMENT OF THE ARRAY HOLDS ONE
.* RETURN VALUE. ONLY AS MANY ELEMENTS ARE USED AS ARE
.* NEEDED. THOSE ELEMENTS USED ALWAYS START WITH ELEMENT
.* NUMBER ONE. IF TWO OR MORE TEST FUNCTIONS ARE INVOKED ON
.* A SINGLE CALL AND IF EACH OF THEM GENERATE ONE OR MORE
.* RETURN VALUES, THEN THE FIRST FUNCTION PROCESSED WILL USE
.* THE LOW ORDER ENTRIES IN &#TESRET. THE NEXT FUNCTION WILL
.* USE THE NEXT ENTRIES, ETC. THE VARIOUS TEST FUNCTIONS
.* WILL ALWAYS BE PROCESSED IN THE SAME ORDER WITH WHICH
.* THEY APPEAR BELOW.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLA  &#REGVAL(255),&#TESERR
         GBLC  &#REGNME(255),&#TESRET(20)
         GBLC  &#EBCDIC(256)
         LCLA  &CTR,&RETPTR,&P,&L,&K,&A1,&A2,&RV(22)
         LCLB  &MESW
         LCLC  &RN(22),&BASE
&#TESERR SETA  0
.*
.*
.*
.*                                 THE "DCODE" FUNCTION
.*   THIS FUNCTION WAS WRITTEN BECAUSE THE MACRO LANGUAGE
.* DOES NOT SUPPORT THE DECODING OF "SUB-SUB-LIST" NOTATION.
.*   THE INPUT IS COMMUNICATED VIA THE "DCODE=" OPERAND AS A
.* CHARACTER STRING TO BE DECODED. THIS STRING MUST BE
.* EITHER NULL, UNPARENTHESIZED, OR A PARENTHESIZED LIST OF
.* ELEMENTS SEPERATED FROM EACH OTHER BY COMMAS. THE
.* ELEMENTS THEMSELVES MAY BE NULL.
.*   FOR A STRING OF N ELEMENTS, THE OUTPUT CONSISTS OF N+1
.* ENTRIES IN &#TESRET. THE FIRST ENTRY CONTAINS THE VALUE
.* N. THE REMAINING ENTRIES CONTAIN EACH OF THE ELEMENTS
.* EXTRACTED FROM THE ORIGINAL STRING. IF THE ORIGINAL
.* STRING IS NULL, THEN IT IS TREATED AS A SUB-LIST
.* CONTAINING ZERO ELEMENTS. IF THE STRING IS
.* UNPARENTHESIZED, THEN IT IS TREATED AS A SUB-LIST
.* CONTAINING A SINGLE ELEMENT - NAMELY, ITSELF.
.*   THE DCODE FUNCTION WILL NOT PROPERLY HANDLE THE
.* FOLLOWING CONDITIONS:
.*       A.) A SUB-LIST ELEMENT LONGER THAN EIGHT CHARACTERS;
.*       B.) A SUB-LIST ELEMENT THAT ITSELF CONSISTS OF A
.*           SUB-LIST;
.*       C.) A SUB-LIST CONTAINING MORE THAN NINETEEN
.*           ELEMENTS.
.*
.DCODE   AIF   ('&DCODE' EQ 'OMITTED').DCODEND
&RETPTR  SETA  &RETPTR+1
&CTR     SETA  0
         AIF   (K'&DCODE EQ 0).DCDFIN
         AIF   ('&DCODE'(1,1) EQ '(').DCDSLST
&CTR     SETA  1
&#TESRET(&RETPTR+1) SETC '&DCODE'
         AGO   .DCDFIN
.DCDSLST ANOP
&K       SETA  K'&DCODE
         AIF   ('&DCODE'(&K,1) EQ ')').DCDOK
         MNOTE 8,'"&DCODE" HAS INVALID SUBLIST SYNTAX.'
&#TESERR SETA  16
         AGO   .DCDFIN
.DCDOK   ANOP
&P       SETA  1
&L       SETA  0
.DCDLP1  ANOP
&P       SETA  &P+&L+1
&L       SETA  0-1
&CTR     SETA  &CTR+1
&#TESRET(&RETPTR+&CTR) SETC ''
.DCDLP2  ANOP
&L       SETA  &L+1
         AIF   ('&DCODE'(&P+&L,1) NE ',' AND &P+&L NE &K).DCDLP2
         AIF   (&L EQ 0).DCDLPET
&#TESRET(&RETPTR+&CTR) SETC '&DCODE'(&P,&L)
         AIF   ('&#TESRET(&RETPTR+&CTR)' EQ '&DCODE'(&P,&L)).DCDLPET
         MNOTE 8,'ERROR - THE FOLLOWING TRUNCATION HAS OCCURED:'
         MNOTE *,'        &DCODE'
         MNOTE *,'        &#TESRET(&RETPTR+&CTR)'
.DCDLPET AIF   (&P+&L NE &K).DCDLP1
.DCDFIN  ANOP
&#TESRET(&RETPTR) SETC '&CTR'
&RETPTR  SETA  &RETPTR+&CTR
.DCODEND ANOP
.*
.*
.*
.*                            THE "MEXCL" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE IF TWO OR MORE
.* MUTUALLY EXCLUSIVE OPERANDS (OR WHATEVER) HAVE BEEN
.* PASSED TO THE CALLING MACRO.
.*   THE INPUT IS COMMUNICATED VIA THE "MEXCL=" OPERAND AS A
.* SUB-LIST WITH ANY NUMBER OF ENTRIES. IF THE MEXCL
.* FUNCTION FINDS MORE THAN ONE NON-NULL ENTRY IN THE
.* SUB-LIST, THEN IT ISSUES A SEVERITY 8 ERROR MESSAGE, AND
.* IT SETS &#TESERR TO A VALUE OF 16.
.*
.MEXCL   AIF   (N'&MEXCL LT 2).MEXCEND
&CTR     SETA  0
.MELP    AIF   (&CTR EQ N'&MEXCL).MEXCEND
&CTR     SETA  &CTR+1
         AIF   (K'&MEXCL(&CTR) EQ 0).MELP
         AIF   (&MESW EQ 1).MEERR
&MESW    SETB  (1)
         AGO   .MELP
.MEERR   MNOTE 8,'ERROR - MUTUALLY EXCLUSIVE OPERANDS HAVE BEEN USED:'
         MNOTE *,'        &MEXCL'
&#TESERR SETA  16
.MEXCEND ANOP
.*
.*
.*
.*                            THE "NUM" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE WHETHER OR NOT A
.* GIVEN VALUE CONSISTS ENTIRELY OF DIGITS.
.*   THE INPUT IS COMMUNICATED VIA THE "NUM=" OPERAND. IF
.* THE NUM FUNCTION FINDS THAT ANY CHARACTER IN THE GIVEN
.* STRING IS NOT A DIGIT, THEN IT SETS &#TESERR TO A VALUE
.* OF 16.
.*
.NUM     AIF   ('&NUM' EQ 'OMITTED').NUMEND
         AIF   (K'&NUM EQ 0).NUMERR
&CTR     SETA  0
.NUMLP   AIF   (&CTR EQ K'&NUM).NUMEND
&CTR     SETA  &CTR+1
         AIF   ('&NUM'(&CTR,1) LT '0').NUMERR
         AIF   ('&NUM'(&CTR,1) LE '9').NUMLP
.NUMERR  ANOP
&#TESERR SETA  16
.NUMEND  ANOP
.*
.*
.*
.*                            THE "PFIX" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE THE VALIDITY OF
.* A GIVEN REGISTER NAME PREFIX (E.G. "R" IN "R15").
.*   THE INPUT CONSISTS OF A REGISTER NAME PREFIX
.* COMMUNICATED VIA THE "PFIX=" OPERAND AND A TABLE OF VALID
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS
.* AND CONTAINED IN THE &#REGNME AND &#REGVAL GLOBAL
.* SYMBOLS. IF THE GIVEN PREFIX IS NULL, THEN A DEFAULT IS
.* USED. IF THE GIVEN PREFIX IS INVALID, THEN A SEVERITY 4
.* ERROR MESSAGE IS ISSUED AND A DEFAULT PREFIX IS USED. THE
.* DEFAULT IS EITHER THE FIRST PREFIX DEFINED VIA A PRIOR
.* #REGS MACRO OR NULL IF NO PRIOR #REGS MACRO HAS DEFINED
.* ANY PREFIXES.
.*   FOR OUTPUT, THE NEXT AVAILABLE &#TESRET ENTRY IS FILLED
.* WITH EITHER THE GIVEN PREFIX OR THE DEFAULT PREFIX.
.*
.PFIX    AIF   ('&PFIX' EQ 'OMITTED').PFIXEND
&RETPTR  SETA  &RETPTR+1
&#TESRET(&RETPTR) SETC ''
&CTR     SETA  0
.PFXLP1  AIF   (&CTR GE 255).PFXGDEF
&CTR     SETA  &CTR+1
         AIF   ('&#REGNME(&CTR)' EQ '').PFXGDEF
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP1
&#TESRET(&RETPTR) SETC '&#REGNME(&CTR)'
.PFXGDEF AIF   (K'&PFIX EQ 0).PFIXEND
&CTR     SETA  &CTR-1
.PFXLP2  AIF   (&CTR GE 255).PFXERR
&CTR     SETA  &CTR+1
         AIF   ('&#REGNME(&CTR)' EQ '').PFXERR
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP2
         AIF   ('&PFIX' NE '&#REGNME(&CTR)').PFXLP2
&#TESRET(&RETPTR) SETC '&PFIX'
         AGO   .PFIXEND
.PFXERR  MNOTE 4,'WARNING - "&PFIX" HAS NOT BEEN PREDEFINED.'
         MNOTE *,'          A DEFAULT VALUE WILL BE USED.'
         MNOTE *,'          CHECK YOUR USAGE OF THE #REGS'
         MNOTE *,'          MACRO.'
.PFIXEND ANOP
.*
.*
.*
.*                            THE "REGS" FUNCTION
.*   THIS FUNCTION CAN BE USED TO CONVERT A CERTAIN CLASS OF
.* REGISTER NAMES TO THEIR CORRESPONDING NUMERIC VALUES. IN
.* ORDER FOR A GIVEN NAME TO BE CONVERTED, IT MUST BE EITHER
.* A SELF-DEFINING NUMERIC OR A NAME THAT HAS BEEN DEFINED
.* VIA A PRIOR #REGS MACRO. THE PURPOSE OF THIS FUNCTION IS
.* TO PROVIDE ARITHMETICLY MANIPULATABLE REGISTER NUMBERS.
.*   THE INPUT CONSISTS OF A SUB-LIST OF REGISTER NAMES
.* COMMUNICATED VIA THE "REGS=" OPERAND AND A TABLE OF VALID
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS
.* AND COMMUNICATED VIA THE &#REGNME AND &#REGVAL GLOBAL
.* SYMBOLS.
.*   FOR A SUB-LIST OF N REGISTER NAMES, THE OUTPUT CONSISTS
.* OF N ENTRIES IN &#TESRET EACH CONTAINING THE NUMBER OF
.* THE REGISTER REPRESENTED BY THE CORRESPONDING NAME FROM
.* THE INPUT SUB-LIST.
.*   IF ANY REGISTER NAME CANNOT BE PROPERLY DECODED, THEN
.* &#TESERR IS SET TO A VALUE OF 16. NO ERROR MESSAGE IS
.* ISSUED.
.*
.REGS    AIF   (N'&REGS EQ 0).REGSEND
&CTR     SETA  0
.REGLP1  AIF   (&CTR GE 16).REGLP2
&RN(&CTR+1) SETC '&CTR'
&RV(&CTR+1) SETA &CTR
&CTR     SETA  &CTR+1
         AGO   .REGLP1
.REGLP2  AIF   (&CTR GE 22).REGND2
&CTR     SETA  &CTR+1
&RN(&CTR) SETC 'ABCDEF'(&CTR-16,1)
&RV(&CTR) SETA &CTR-7
         AGO   .REGLP2
.REGND2  ANOP
&CTR     SETA  0
.REGLP3  AIF   (&CTR GE N'&REGS).REGSEND
&CTR     SETA  &CTR+1
&RETPTR  SETA  &RETPTR+1
&#TESRET(&RETPTR) SETC ''
         AIF   (K'&REGS(&CTR) EQ 0).REGLP3
&#TESRET(&RETPTR) SETC '&REGS(&CTR)'
         AIF   (T'&REGS(&CTR) NE 'N').REGLP3A
         AIF   (&REGS(&CTR) LT 0 OR &REGS(&CTR) GT 15).REGERR
         AGO   .REGLP3
.REGLP3A ANOP
&A1      SETA  0
.REGLP4  AIF   (&A1 GE 255).REGND4
&A1      SETA  &A1+1
         AIF   ('&#REGNME(&A1)' EQ '').REGND4
         AIF   (&#REGVAL(&A1) GT 15).REGPFX
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)').REGLP4
&#TESRET(&RETPTR) SETC '&#REGVAL(&A1)'
         AGO   .REGLP3
.REGPFX  ANOP
&A2      SETA  0
.REGLP5  AIF   (&A2 GE 22).REGLP4
&A2      SETA  &A2+1
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)&RN(&A2)').REGLP5
&#TESRET(&RETPTR) SETC '&RV(&A2)'
         AGO   .REGLP3
.REGND4  ANOP
&A2      SETA  0
.REGLP6  AIF   (&A2 GE 16).REGERR
&A2      SETA  &A2+1
         AIF   ('&REGS(&CTR)' NE '&RN(&A2)').REGLP6
         AGO   .REGLP3
.REGERR  ANOP
&#TESERR SETA  16
         AGO   .REGLP3
.REGSEND ANOP
.*
.*
.*
.*                            THE "SIZE" FUNCTION
.*   THIS FUNCTION WAS WRITTEN BECAUSE OF THE LIMITATION
.* THAT MACRO CODE CANNOT ARITHMETICLY MANIPULATE OPERANDS
.* CONSISTING OF EITHER EXPRESSIONS OR EQUATE SYMBOLS.
.* BECAUSE OF THIS, IN SITUATIONS WHERE A PROGRAMMER WOULD
.* NORMALLY WANT TO USE AN EXPRESSION, ETC., HE MAY INSTEAD
.* BE FORCED TO USE A SELF DEFINING NUMERIC. THE PROBLEM IS
.* THAT IF SUBSEQUENT MODIFICATIONS AFFECT THE VALUE OF SUCH
.* AN EXPRESSION, THE PROGRAMMER MIGHT FORGET TO CHANGE THE
.* SELF DEFINING NUMERIC ACCORDINGLY. THE SIZE FUNCTION CAN
.* BE USED TO ALLEVIATE THIS PROBLEM.
.*   THE INPUT IS COMMUNICATED VIA THE "SIZE=" OPERAND AND
.* IT MUST CONSIST OF A THREE ELEMENT SUB-LIST. THE FIRST
.* AND THIRD ELEMENTS MUST BE SUCH THAT THEY RESULTS IN
.* NON-RELOCATABLE VALUES WHEN ASSEMBLED. THE SECOND OPERAND
.* MUST BE ONE OF THE FOLLOWING RELATIONAL OPERATORS:
.*       LT, LE, EQ, GE, GT, NE
.* MEANING "LESS THAN", "LESS THAN OR EQUAL", "EQUAL",
.* "GREATER THAN OR EQUAL", "GREATER THAN", AND "NOT EQUAL"
.* RESPECTIVELY.
.*   THE OUTPUT CONSISTS OF A GENERATED STATEMENT WHICH
.* PRODUCES NO OBJECT CODE BUT WHICH CAUSES AN ERROR
.* WHENEVER THE TWO GIVEN ELEMENTS VIOLATE THE INDICATED
.* RELATION.
.*
.SIZE    AIF   (N'&SIZE EQ 0).SIZEEND
         AIF   ('&SIZE(2)' EQ 'EQ' OR '&SIZE(2)' EQ 'GE' OR '&SIZE(2)' *
               EQ 'LE' OR '&SIZE(2)' EQ 'GT' OR '&SIZE(2)' EQ 'LT' OR '*
               &SIZE(2)' EQ 'NE').OPOK
         MNOTE 8,'ERROR - "&SIZE(2)" NOT A VALID RELATIONAL OPERATOR'
&#TESERR SETA  16
         AGO   .SIZEEND
.OPOK    AIF   ('&SIZE(2)' NE 'EQ').OPNTEQ
         DC    0YL2(X'7FFF'-(&SIZE(1))+&SIZE(3),X'7FFF'-(&SIZE(3))+&SIZ*
               E(1))
         AGO   .SIZEEND
.OPNTEQ  AIF   ('&SIZE(2)' NE 'NE').OPNTNE
         DC    0YL2(X'8000'-(&SIZE(3)-(&SIZE(1)))/(&SIZE(3)-(&SIZE(1)))*
               )
         AGO   .SIZEEND
.OPNTNE  ANOP
&BASE    SETC  '7FFF'
         AIF   ('&SIZE(2)'(2,1) EQ 'E').NOPLUS1
&BASE    SETC  '8000'
.NOPLUS1 AIF   ('&SIZE(2)'(1,1) EQ 'G').OPG
         DC    0YL2(X'&BASE'-(&SIZE(3))+&SIZE(1))
         AGO   .SIZEEND
.OPG     ANOP
         DC    0YL2(X'&BASE'-(&SIZE(1))+&SIZE(3))
.SIZEEND ANOP
.*
.*
.*
.*                                 THE "GEN" FUNCTION
.*    THIS FUNCTION CAN BE USED TO GENERATE VARIOUS SPECIFIC
.* OBJECTS. CURRENTLY, THE SUPPORTED OBJECTS ARE:
.*       EBCDIC - A GLOBAL TABLE CONTAINING THE ENTIRE
.*                256-ENTRY EBCDIC CHARACTER SET SUCH THAT
.*                THE VALUE OF THE ITH ENTRY IS I-1.
.*
.GEN     AIF   (N'&GEN EQ 0).GENEND
&A1      SETA  0
.GENLP   AIF   (&A1 EQ N'&GEN).GENEND
&A1      SETA  &A1+1
         AIF   ('&GEN(&A1)' EQ '').GENLP
         AIF   ('&GEN(&A1)' NE 'EBCDIC').GNTEBCD
         AIF   ('&#EBCDIC(194)' EQ 'A').GENLP
         PUSH  PRINT                                          09/96 X32 44090000
         PRINT OFF                 GENERATE #TESTX-- EQUATES  11/96 X32 44160000
#TESTX00 EQU   000,,X'00'                                     10/97 X33 44230000
#TESTX01 EQU   001,,X'01'                                     09/96 X32 44300000
#TESTX02 EQU   002,,X'02'                                     09/96 X32 44380000
#TESTX03 EQU   003,,X'03'                                     09/96 X32 44450000
#TESTX04 EQU   004,,X'04'                                     09/96 X32 44520000
#TESTX05 EQU   005,,X'05'                                     09/96 X32 44590000
#TESTX06 EQU   006,,X'06'                                     09/96 X32 44660000
#TESTX07 EQU   007,,X'07'                                     09/96 X32 44730000
#TESTX08 EQU   008,,X'08'                                     09/96 X32 44810000
#TESTX09 EQU   009,,X'09'                                     09/96 X32 44880000
#TESTX0A EQU   010,,X'0A'                                     09/96 X32 44950000
#TESTX0B EQU   011,,X'0B'                                     09/96 X32 45020000
#TESTX0C EQU   012,,X'0C'                                     09/96 X32 45090000
#TESTX0D EQU   013,,X'0D'                                     09/96 X32 45160000
#TESTX0E EQU   014,,X'0E'                                     09/96 X32 45230000
#TESTX0F EQU   015,,X'0F'                                     09/96 X32 45310000
#TESTX10 EQU   016,,X'10'                                     09/96 X32 45380000
#TESTX11 EQU   017,,X'11'                                     09/96 X32 45450000
#TESTX12 EQU   018,,X'12'                                     09/96 X32 45520000
#TESTX13 EQU   019,,X'13'                                     09/96 X32 45590000
#TESTX14 EQU   020,,X'14'                                     09/96 X32 45660000
#TESTX15 EQU   021,,X'15'                                     09/96 X32 45740000
#TESTX16 EQU   022,,X'16'                                     09/96 X32 45810000
#TESTX17 EQU   023,,X'17'                                     09/96 X32 45880000
#TESTX18 EQU   024,,X'18'                                     09/96 X32 45950000
#TESTX19 EQU   025,,X'19'                                     09/96 X32 46020000
#TESTX1A EQU   026,,X'1A'                                     09/96 X32 46090000
#TESTX1B EQU   027,,X'1B'                                     09/96 X32 46170000
#TESTX1C EQU   028,,X'1C'                                     09/96 X32 46240000
#TESTX1D EQU   029,,X'1D'                                     09/96 X32 46310000
#TESTX1E EQU   030,,X'1E'                                     09/96 X32 46380000
#TESTX1F EQU   031,,X'1F'                                     09/96 X32 46450000
#TESTX20 EQU   032,,X'20'                                     09/96 X32 46520000
#TESTX21 EQU   033,,X'21'                                     09/96 X32 46590000
#TESTX22 EQU   034,,X'22'                                     09/96 X32 46670000
#TESTX23 EQU   035,,X'23'                                     09/96 X32 46740000
#TESTX24 EQU   036,,X'24'                                     09/96 X32 46810000
#TESTX25 EQU   037,,X'25'                                     09/96 X32 46880000
#TESTX26 EQU   038,,X'26'                                     09/96 X32 46950000
#TESTX27 EQU   039,,X'27'                                     09/96 X32 47020000
#TESTX28 EQU   040,,X'28'                                     09/96 X32 47100000
#TESTX29 EQU   041,,X'29'                                     09/96 X32 47170000
#TESTX2A EQU   042,,X'2A'                                     09/96 X32 47240000
#TESTX2B EQU   043,,X'2B'                                     09/96 X32 47310000
#TESTX2C EQU   044,,X'2C'                                     09/96 X32 47380000
#TESTX2D EQU   045,,X'2D'                                     09/96 X32 47450000
#TESTX2E EQU   046,,X'2E'                                     09/96 X32 47530000
#TESTX2F EQU   047,,X'2F'                                     09/96 X32 47600000
#TESTX30 EQU   048,,X'30'                                     09/96 X32 47670000
#TESTX31 EQU   049,,X'31'                                     09/96 X32 47740000
#TESTX32 EQU   050,,X'32'                                     09/96 X32 47810000
#TESTX33 EQU   051,,X'33'                                     09/96 X32 47880000
#TESTX34 EQU   052,,X'34'                                     09/96 X32 47950000
#TESTX35 EQU   053,,X'35'                                     09/96 X32 48030000
#TESTX36 EQU   054,,X'36'                                     09/96 X32 48100000
#TESTX37 EQU   055,,X'37'                                     09/96 X32 48170000
#TESTX38 EQU   056,,X'38'                                     09/96 X32 48240000
#TESTX39 EQU   057,,X'39'                                     09/96 X32 48310000
#TESTX3A EQU   058,,X'3A'                                     09/96 X32 48380000
#TESTX3B EQU   059,,X'3B'                                     09/96 X32 48460000
#TESTX3C EQU   060,,X'3C'                                     09/96 X32 48530000
#TESTX3D EQU   061,,X'3D'                                     09/96 X32 48600000
#TESTX3E EQU   062,,X'3E'                                     09/96 X32 48670000
#TESTX3F EQU   063,,X'3F'                                     09/96 X32 48740000
#TESTX40 EQU   064,,X'40'                                     09/96 X32 48810000
#TESTX41 EQU   065,,X'41'                                     09/96 X32 48890000
#TESTX42 EQU   066,,X'42'                                     09/96 X32 48960000
#TESTX43 EQU   067,,X'43'                                     09/96 X32 49030000
#TESTX44 EQU   068,,X'44'                                     09/96 X32 49100000
#TESTX45 EQU   069,,X'45'                                     09/96 X32 49170000
#TESTX46 EQU   070,,X'46'                                     09/96 X32 49240000
#TESTX47 EQU   071,,X'47'                                     09/96 X32 49310000
#TESTX48 EQU   072,,X'48'                                     09/96 X32 49390000
#TESTX49 EQU   073,,X'49'                                     09/96 X32 49460000
#TESTX4A EQU   074,,X'4A'                                     09/96 X32 49530000
#TESTX4B EQU   075,,X'4B'                                     09/96 X32 49600000
#TESTX4C EQU   076,,X'4C'                                     09/96 X32 49670000
#TESTX4D EQU   077,,X'4D'                                     09/96 X32 49740000
#TESTX4E EQU   078,,X'4E'                                     09/96 X32 49820000
#TESTX4F EQU   079,,X'4F'                                     09/96 X32 49890000
#TESTX50 EQU   080,,X'50'                                     09/96 X32 49960000
#TESTX51 EQU   081,,X'51'                                     09/96 X32 50030000
#TESTX52 EQU   082,,X'52'                                     09/96 X32 50100000
#TESTX53 EQU   083,,X'53'                                     09/96 X32 50170000
#TESTX54 EQU   084,,X'54'                                     09/96 X32 50250000
#TESTX55 EQU   085,,X'55'                                     09/96 X32 50320000
#TESTX56 EQU   086,,X'56'                                     09/96 X32 50390000
#TESTX57 EQU   087,,X'57'                                     09/96 X32 50460000
#TESTX58 EQU   088,,X'58'                                     09/96 X32 50530000
#TESTX59 EQU   089,,X'59'                                     09/96 X32 50600000
#TESTX5A EQU   090,,X'5A'                                     09/96 X32 50680000
#TESTX5B EQU   091,,X'5B'                                     09/96 X32 50750000
#TESTX5C EQU   092,,X'5C'                                     09/96 X32 50820000
#TESTX5D EQU   093,,X'5D'                                     09/96 X32 50890000
#TESTX5E EQU   094,,X'5E'                                     09/96 X32 50960000
#TESTX5F EQU   095,,X'5F'                                     09/96 X32 51030000
#TESTX60 EQU   096,,X'60'                                     09/96 X32 51100000
#TESTX61 EQU   097,,X'61'                                     09/96 X32 51180000
#TESTX62 EQU   098,,X'62'                                     09/96 X32 51250000
#TESTX63 EQU   099,,X'63'                                     09/96 X32 51320000
#TESTX64 EQU   100,,X'64'                                     09/96 X32 51390000
#TESTX65 EQU   101,,X'65'                                     09/96 X32 51460000
#TESTX66 EQU   102,,X'66'                                     09/96 X32 51530000
#TESTX67 EQU   103,,X'67'                                     09/96 X32 51610000
#TESTX68 EQU   104,,X'68'                                     09/96 X32 51680000
#TESTX69 EQU   105,,X'69'                                     09/96 X32 51750000
#TESTX6A EQU   106,,X'6A'                                     09/96 X32 51820000
#TESTX6B EQU   107,,X'6B'                                     09/96 X32 51890000
#TESTX6C EQU   108,,X'6C'                                     09/96 X32 51960000
#TESTX6D EQU   109,,X'6D'                                     09/96 X32 52040000
#TESTX6E EQU   110,,X'6E'                                     09/96 X32 52110000
#TESTX6F EQU   111,,X'6F'                                     09/96 X32 52180000
#TESTX70 EQU   112,,X'70'                                     09/96 X32 52250000
#TESTX71 EQU   113,,X'71'                                     09/96 X32 52320000
#TESTX72 EQU   114,,X'72'                                     09/96 X32 52390000
#TESTX73 EQU   115,,X'73'                                     09/96 X32 52460000
#TESTX74 EQU   116,,X'74'                                     09/96 X32 52540000
#TESTX75 EQU   117,,X'75'                                     09/96 X32 52610000
#TESTX76 EQU   118,,X'76'                                     09/96 X32 52680000
#TESTX77 EQU   119,,X'77'                                     09/96 X32 52750000
#TESTX78 EQU   120,,X'78'                                     09/96 X32 52820000
#TESTX79 EQU   121,,X'79'                                     09/96 X32 52890000
#TESTX7A EQU   122,,X'7A'                                     09/96 X32 52970000
#TESTX7B EQU   123,,X'7B'                                     09/96 X32 53040000
#TESTX7C EQU   124,,X'7C'                                     09/96 X32 53110000
#TESTX7D EQU   125,,X'7D'                                     09/96 X32 53180000
#TESTX7E EQU   126,,X'7E'                                     09/96 X32 53250000
#TESTX7F EQU   127,,X'7F'                                     09/96 X32 53320000
#TESTX80 EQU   128,,X'80'                                     09/96 X32 53400000
#TESTX81 EQU   129,,X'81'                                     09/96 X32 53470000
#TESTX82 EQU   130,,X'82'                                     09/96 X32 53540000
#TESTX83 EQU   131,,X'83'                                     09/96 X32 53610000
#TESTX84 EQU   132,,X'84'                                     09/96 X32 53680000
#TESTX85 EQU   133,,X'85'                                     09/96 X32 53750000
#TESTX86 EQU   134,,X'86'                                     09/96 X32 53820000
#TESTX87 EQU   135,,X'87'                                     09/96 X32 53900000
#TESTX88 EQU   136,,X'88'                                     09/96 X32 53970000
#TESTX89 EQU   137,,X'89'                                     09/96 X32 54040000
#TESTX8A EQU   138,,X'8A'                                     09/96 X32 54110000
#TESTX8B EQU   139,,X'8B'                                     09/96 X32 54180000
#TESTX8C EQU   140,,X'8C'                                     09/96 X32 54250000
#TESTX8D EQU   141,,X'8D'                                     09/96 X32 54330000
#TESTX8E EQU   142,,X'8E'                                     09/96 X32 54400000
#TESTX8F EQU   143,,X'8F'                                     09/96 X32 54470000
#TESTX90 EQU   144,,X'90'                                     09/96 X32 54540000
#TESTX91 EQU   145,,X'91'                                     09/96 X32 54610000
#TESTX92 EQU   146,,X'92'                                     09/96 X32 54680000
#TESTX93 EQU   147,,X'93'                                     09/96 X32 54760000
#TESTX94 EQU   148,,X'94'                                     09/96 X32 54830000
#TESTX95 EQU   149,,X'95'                                     09/96 X32 54900000
#TESTX96 EQU   150,,X'96'                                     09/96 X32 54970000
#TESTX97 EQU   151,,X'97'                                     09/96 X32 55040000
#TESTX98 EQU   152,,X'98'                                     09/96 X32 55110000
#TESTX99 EQU   153,,X'99'                                     09/96 X32 55180000
#TESTX9A EQU   154,,X'9A'                                     09/96 X32 55260000
#TESTX9B EQU   155,,X'9B'                                     09/96 X32 55330000
#TESTX9C EQU   156,,X'9C'                                     09/96 X32 55400000
#TESTX9D EQU   157,,X'9D'                                     09/96 X32 55470000
#TESTX9E EQU   158,,X'9E'                                     09/96 X32 55540000
#TESTX9F EQU   159,,X'9F'                                     09/96 X32 55610000
#TESTXA0 EQU   160,,X'A0'                                     09/96 X32 55690000
#TESTXA1 EQU   161,,X'A1'                                     09/96 X32 55760000
#TESTXA2 EQU   162,,X'A2'                                     09/96 X32 55830000
#TESTXA3 EQU   163,,X'A3'                                     09/96 X32 55900000
#TESTXA4 EQU   164,,X'A4'                                     09/96 X32 55970000
#TESTXA5 EQU   165,,X'A5'                                     09/96 X32 56040000
#TESTXA6 EQU   166,,X'A6'                                     09/96 X32 56120000
#TESTXA7 EQU   167,,X'A7'                                     09/96 X32 56190000
#TESTXA8 EQU   168,,X'A8'                                     09/96 X32 56260000
#TESTXA9 EQU   169,,X'A9'                                     09/96 X32 56330000
#TESTXAA EQU   170,,X'AA'                                     09/96 X32 56400000
#TESTXAB EQU   171,,X'AB'                                     09/96 X32 56470000
#TESTXAC EQU   172,,X'AC'                                     09/96 X32 56540000
#TESTXAD EQU   173,,X'AD'                                     09/96 X32 56620000
#TESTXAE EQU   174,,X'AE'                                     09/96 X32 56690000
#TESTXAF EQU   175,,X'AF'                                     09/96 X32 56760000
#TESTXB0 EQU   176,,X'B0'                                     09/96 X32 56830000
#TESTXB1 EQU   177,,X'B1'                                     09/96 X32 56900000
#TESTXB2 EQU   178,,X'B2'                                     09/96 X32 56970000
#TESTXB3 EQU   179,,X'B3'                                     09/96 X32 57050000
#TESTXB4 EQU   180,,X'B4'                                     09/96 X32 57120000
#TESTXB5 EQU   181,,X'B5'                                     09/96 X32 57190000
#TESTXB6 EQU   182,,X'B6'                                     09/96 X32 57260000
#TESTXB7 EQU   183,,X'B7'                                     09/96 X32 57330000
#TESTXB8 EQU   184,,X'B8'                                     09/96 X32 57400000
#TESTXB9 EQU   185,,X'B9'                                     09/96 X32 57480000
#TESTXBA EQU   186,,X'BA'                                     09/96 X32 57550000
#TESTXBB EQU   187,,X'BB'                                     09/96 X32 57620000
#TESTXBC EQU   188,,X'BC'                                     09/96 X32 57690000
#TESTXBD EQU   189,,X'BD'                                     09/96 X32 57760000
#TESTXBE EQU   190,,X'BE'                                     09/96 X32 57830000
#TESTXBF EQU   191,,X'BF'                                     09/96 X32 57900000
#TESTXC0 EQU   192,,X'C0'                                     09/96 X32 57980000
#TESTXC1 EQU   193,,X'C1'                                     09/96 X32 58050000
#TESTXC2 EQU   194,,X'C2'                                     09/96 X32 58120000
#TESTXC3 EQU   195,,X'C3'                                     09/96 X32 58190000
#TESTXC4 EQU   196,,X'C4'                                     09/96 X32 58260000
#TESTXC5 EQU   197,,X'C5'                                     09/96 X32 58330000
#TESTXC6 EQU   198,,X'C6'                                     09/96 X32 58410000
#TESTXC7 EQU   199,,X'C7'                                     09/96 X32 58480000
#TESTXC8 EQU   200,,X'C8'                                     09/96 X32 58550000
#TESTXC9 EQU   201,,X'C9'                                     09/96 X32 58620000
#TESTXCA EQU   202,,X'CA'                                     09/96 X32 58690000
#TESTXCB EQU   203,,X'CB'                                     09/96 X32 58760000
#TESTXCC EQU   204,,X'CC'                                     09/96 X32 58840000
#TESTXCD EQU   205,,X'CD'                                     09/96 X32 58910000
#TESTXCE EQU   206,,X'CE'                                     09/96 X32 58980000
#TESTXCF EQU   207,,X'CF'                                     09/96 X32 59050000
#TESTXD0 EQU   208,,X'D0'                                     09/96 X32 59120000
#TESTXD1 EQU   209,,X'D1'                                     09/96 X32 59190000
#TESTXD2 EQU   210,,X'D2'                                     09/96 X32 59260000
#TESTXD3 EQU   211,,X'D3'                                     09/96 X32 59340000
#TESTXD4 EQU   212,,X'D4'                                     09/96 X32 59410000
#TESTXD5 EQU   213,,X'D5'                                     09/96 X32 59480000
#TESTXD6 EQU   214,,X'D6'                                     09/96 X32 59550000
#TESTXD7 EQU   215,,X'D7'                                     09/96 X32 59620000
#TESTXD8 EQU   216,,X'D8'                                     09/96 X32 59690000
#TESTXD9 EQU   217,,X'D9'                                     09/96 X32 59770000
#TESTXDA EQU   218,,X'DA'                                     09/96 X32 59840000
#TESTXDB EQU   219,,X'DB'                                     09/96 X32 59910000
#TESTXDC EQU   220,,X'DC'                                     09/96 X32 59980000
#TESTXDD EQU   221,,X'DD'                                     09/96 X32 60050000
#TESTXDE EQU   222,,X'DE'                                     09/96 X32 60120000
#TESTXDF EQU   223,,X'DF'                                     09/96 X32 60200000
#TESTXE0 EQU   224,,X'E0'                                     09/96 X32 60270000
#TESTXE1 EQU   225,,X'E1'                                     09/96 X32 60340000
#TESTXE2 EQU   226,,X'E2'                                     09/96 X32 60410000
#TESTXE3 EQU   227,,X'E3'                                     09/96 X32 60480000
#TESTXE4 EQU   228,,X'E4'                                     09/96 X32 60550000
#TESTXE5 EQU   229,,X'E5'                                     09/96 X32 60620000
#TESTXE6 EQU   230,,X'E6'                                     09/96 X32 60700000
#TESTXE7 EQU   231,,X'E7'                                     09/96 X32 60770000
#TESTXE8 EQU   232,,X'E8'                                     09/96 X32 60840000
#TESTXE9 EQU   233,,X'E9'                                     09/96 X32 60910000
#TESTXEA EQU   234,,X'EA'                                     09/96 X32 60980000
#TESTXEB EQU   235,,X'EB'                                     09/96 X32 61050000
#TESTXEC EQU   236,,X'EC'                                     09/96 X32 61130000
#TESTXED EQU   237,,X'ED'                                     09/96 X32 61200000
#TESTXEE EQU   238,,X'EE'                                     09/96 X32 61270000
#TESTXEF EQU   239,,X'EF'                                     09/96 X32 61340000
#TESTXF0 EQU   240,,X'F0'                                     09/96 X32 61410000
#TESTXF1 EQU   241,,X'F1'                                     09/96 X32 61480000
#TESTXF2 EQU   242,,X'F2'                                     09/96 X32 61560000
#TESTXF3 EQU   243,,X'F3'                                     09/96 X32 61630000
#TESTXF4 EQU   244,,X'F4'                                     09/96 X32 61700000
#TESTXF5 EQU   245,,X'F5'                                     09/96 X32 61770000
#TESTXF6 EQU   246,,X'F6'                                     09/96 X32 61840000
#TESTXF7 EQU   247,,X'F7'                                     09/96 X32 61910000
#TESTXF8 EQU   248,,X'F8'                                     09/96 X32 61980000
#TESTXF9 EQU   249,,X'F9'                                     09/96 X32 62060000
#TESTXFA EQU   250,,X'FA'                                     09/96 X32 62130000
#TESTXFB EQU   251,,X'FB'                                     09/96 X32 62200000
#TESTXFC EQU   252,,X'FC'                                     09/96 X32 62270000
#TESTXFD EQU   253,,X'FD'                                     09/96 X32 62340000
#TESTXFE EQU   254,,X'FE'                                     09/96 X32 62410000
#TESTXFF EQU   255,,X'FF'                                     09/96 X32 62490000
         POP   PRINT                                          09/96 X32 62560000
&#EBCDIC(001) SETC T'#TESTX00                                 10/97 X33 62630000
&#EBCDIC(002) SETC T'#TESTX01                                 09/96 X32 62700000
&#EBCDIC(003) SETC T'#TESTX02                                 09/96 X32 62770000
&#EBCDIC(004) SETC T'#TESTX03                                 09/96 X32 62840000
&#EBCDIC(005) SETC T'#TESTX04                                 09/96 X32 62920000
&#EBCDIC(006) SETC T'#TESTX05                                 09/96 X32 62990000
&#EBCDIC(007) SETC T'#TESTX06                                 09/96 X32 63060000
&#EBCDIC(008) SETC T'#TESTX07                                 09/96 X32 63130000
&#EBCDIC(009) SETC T'#TESTX08                                 09/96 X32 63200000
&#EBCDIC(010) SETC T'#TESTX09                                 09/96 X32 63270000
&#EBCDIC(011) SETC T'#TESTX0A                                 09/96 X32 63350000
&#EBCDIC(012) SETC T'#TESTX0B                                 09/96 X32 63420000
&#EBCDIC(013) SETC T'#TESTX0C                                 09/96 X32 63490000
&#EBCDIC(014) SETC T'#TESTX0D                                 09/96 X32 63560000
&#EBCDIC(015) SETC T'#TESTX0E                                 09/96 X32 63630000
&#EBCDIC(016) SETC T'#TESTX0F                                 09/96 X32 63700000
&#EBCDIC(017) SETC T'#TESTX10                                 09/96 X32 63770000
&#EBCDIC(018) SETC T'#TESTX11                                 09/96 X32 63850000
&#EBCDIC(019) SETC T'#TESTX12                                 09/96 X32 63920000
&#EBCDIC(020) SETC T'#TESTX13                                 09/96 X32 63990000
&#EBCDIC(021) SETC T'#TESTX14                                 09/96 X32 64060000
&#EBCDIC(022) SETC T'#TESTX15                                 09/96 X32 64130000
&#EBCDIC(023) SETC T'#TESTX16                                 09/96 X32 64200000
&#EBCDIC(024) SETC T'#TESTX17                                 09/96 X32 64280000
&#EBCDIC(025) SETC T'#TESTX18                                 09/96 X32 64350000
&#EBCDIC(026) SETC T'#TESTX19                                 09/96 X32 64420000
&#EBCDIC(027) SETC T'#TESTX1A                                 09/96 X32 64490000
&#EBCDIC(028) SETC T'#TESTX1B                                 09/96 X32 64560000
&#EBCDIC(029) SETC T'#TESTX1C                                 09/96 X32 64630000
&#EBCDIC(030) SETC T'#TESTX1D                                 09/96 X32 64710000
&#EBCDIC(031) SETC T'#TESTX1E                                 09/96 X32 64780000
&#EBCDIC(032) SETC T'#TESTX1F                                 09/96 X32 64850000
&#EBCDIC(033) SETC T'#TESTX20                                 09/96 X32 64920000
&#EBCDIC(034) SETC T'#TESTX21                                 09/96 X32 64990000
&#EBCDIC(035) SETC T'#TESTX22                                 09/96 X32 65060000
&#EBCDIC(036) SETC T'#TESTX23                                 09/96 X32 65130000
&#EBCDIC(037) SETC T'#TESTX24                                 09/96 X32 65210000
&#EBCDIC(038) SETC T'#TESTX25                                 09/96 X32 65280000
&#EBCDIC(039) SETC T'#TESTX26                                 09/96 X32 65350000
&#EBCDIC(040) SETC T'#TESTX27                                 09/96 X32 65420000
&#EBCDIC(041) SETC T'#TESTX28                                 09/96 X32 65490000
&#EBCDIC(042) SETC T'#TESTX29                                 09/96 X32 65560000
&#EBCDIC(043) SETC T'#TESTX2A                                 09/96 X32 65640000
&#EBCDIC(044) SETC T'#TESTX2B                                 09/96 X32 65710000
&#EBCDIC(045) SETC T'#TESTX2C                                 09/96 X32 65780000
&#EBCDIC(046) SETC T'#TESTX2D                                 09/96 X32 65850000
&#EBCDIC(047) SETC T'#TESTX2E                                 09/96 X32 65920000
&#EBCDIC(048) SETC T'#TESTX2F                                 09/96 X32 65990000
&#EBCDIC(049) SETC T'#TESTX30                                 09/96 X32 66070000
&#EBCDIC(050) SETC T'#TESTX31                                 09/96 X32 66140000
&#EBCDIC(051) SETC T'#TESTX32                                 09/96 X32 66210000
&#EBCDIC(052) SETC T'#TESTX33                                 09/96 X32 66280000
&#EBCDIC(053) SETC T'#TESTX34                                 09/96 X32 66350000
&#EBCDIC(054) SETC T'#TESTX35                                 09/96 X32 66420000
&#EBCDIC(055) SETC T'#TESTX36                                 09/96 X32 66490000
&#EBCDIC(056) SETC T'#TESTX37                                 09/96 X32 66570000
&#EBCDIC(057) SETC T'#TESTX38                                 09/96 X32 66640000
&#EBCDIC(058) SETC T'#TESTX39                                 09/96 X32 66710000
&#EBCDIC(059) SETC T'#TESTX3A                                 09/96 X32 66780000
&#EBCDIC(060) SETC T'#TESTX3B                                 09/96 X32 66850000
&#EBCDIC(061) SETC T'#TESTX3C                                 09/96 X32 66920000
&#EBCDIC(062) SETC T'#TESTX3D                                 09/96 X32 67000000
&#EBCDIC(063) SETC T'#TESTX3E                                 09/96 X32 67070000
&#EBCDIC(064) SETC T'#TESTX3F                                 09/96 X32 67140000
&#EBCDIC(065) SETC T'#TESTX40                                 09/96 X32 67210000
&#EBCDIC(066) SETC T'#TESTX41                                 09/96 X32 67280000
&#EBCDIC(067) SETC T'#TESTX42                                 09/96 X32 67350000
&#EBCDIC(068) SETC T'#TESTX43                                 09/96 X32 67430000
&#EBCDIC(069) SETC T'#TESTX44                                 09/96 X32 67500000
&#EBCDIC(070) SETC T'#TESTX45                                 09/96 X32 67570000
&#EBCDIC(071) SETC T'#TESTX46                                 09/96 X32 67640000
&#EBCDIC(072) SETC T'#TESTX47                                 09/96 X32 67710000
&#EBCDIC(073) SETC T'#TESTX48                                 09/96 X32 67780000
&#EBCDIC(074) SETC T'#TESTX49                                 09/96 X32 67850000
&#EBCDIC(075) SETC T'#TESTX4A                                 09/96 X32 67930000
&#EBCDIC(076) SETC T'#TESTX4B                                 09/96 X32 68000000
&#EBCDIC(077) SETC T'#TESTX4C                                 09/96 X32 68070000
&#EBCDIC(078) SETC T'#TESTX4D                                 09/96 X32 68140000
&#EBCDIC(079) SETC T'#TESTX4E                                 09/96 X32 68210000
&#EBCDIC(080) SETC T'#TESTX4F                                 09/96 X32 68280000
&#EBCDIC(081) SETC '&&'                                                 68360000
&#EBCDIC(082) SETC T'#TESTX51                                 09/96 X32 68430000
&#EBCDIC(083) SETC T'#TESTX52                                 09/96 X32 68500000
&#EBCDIC(084) SETC T'#TESTX53                                 09/96 X32 68570000
&#EBCDIC(085) SETC T'#TESTX54                                 09/96 X32 68640000
&#EBCDIC(086) SETC T'#TESTX55                                 09/96 X32 68710000
&#EBCDIC(087) SETC T'#TESTX56                                 09/96 X32 68790000
&#EBCDIC(088) SETC T'#TESTX57                                 09/96 X32 68860000
&#EBCDIC(089) SETC T'#TESTX58                                 09/96 X32 68930000
&#EBCDIC(090) SETC T'#TESTX59                                 09/96 X32 69000000
&#EBCDIC(091) SETC T'#TESTX5A                                 09/96 X32 69070000
&#EBCDIC(092) SETC T'#TESTX5B                                 09/96 X32 69140000
&#EBCDIC(093) SETC T'#TESTX5C                                 09/96 X32 69210000
&#EBCDIC(094) SETC T'#TESTX5D                                 09/96 X32 69290000
&#EBCDIC(095) SETC T'#TESTX5E                                 09/96 X32 69360000
&#EBCDIC(096) SETC T'#TESTX5F                                 09/96 X32 69430000
&#EBCDIC(097) SETC T'#TESTX60                                 09/96 X32 69500000
&#EBCDIC(098) SETC T'#TESTX61                                 09/96 X32 69570000
&#EBCDIC(099) SETC T'#TESTX62                                 09/96 X32 69640000
&#EBCDIC(100) SETC T'#TESTX63                                 09/96 X32 69720000
&#EBCDIC(101) SETC T'#TESTX64                                 09/96 X32 69790000
&#EBCDIC(102) SETC T'#TESTX65                                 09/96 X32 69860000
&#EBCDIC(103) SETC T'#TESTX66                                 09/96 X32 69930000
&#EBCDIC(104) SETC T'#TESTX67                                 09/96 X32 70000000
&#EBCDIC(105) SETC T'#TESTX68                                 09/96 X32 70070000
&#EBCDIC(106) SETC T'#TESTX69                                 09/96 X32 70150000
&#EBCDIC(107) SETC T'#TESTX6A                                 09/96 X32 70220000
&#EBCDIC(108) SETC T'#TESTX6B                                 09/96 X32 70290000
&#EBCDIC(109) SETC T'#TESTX6C                                 09/96 X32 70360000
&#EBCDIC(110) SETC T'#TESTX6D                                 09/96 X32 70430000
&#EBCDIC(111) SETC T'#TESTX6E                                 09/96 X32 70500000
&#EBCDIC(112) SETC T'#TESTX6F                                 09/96 X32 70570000
&#EBCDIC(113) SETC T'#TESTX70                                 09/96 X32 70650000
&#EBCDIC(114) SETC T'#TESTX71                                 09/96 X32 70720000
&#EBCDIC(115) SETC T'#TESTX72                                 09/96 X32 70790000
&#EBCDIC(116) SETC T'#TESTX73                                 09/96 X32 70860000
&#EBCDIC(117) SETC T'#TESTX74                                 09/96 X32 70930000
&#EBCDIC(118) SETC T'#TESTX75                                 09/96 X32 71000000
&#EBCDIC(119) SETC T'#TESTX76                                 09/96 X32 71080000
&#EBCDIC(120) SETC T'#TESTX77                                 09/96 X32 71150000
&#EBCDIC(121) SETC T'#TESTX78                                 09/96 X32 71220000
&#EBCDIC(122) SETC T'#TESTX79                                 09/96 X32 71290000
&#EBCDIC(123) SETC T'#TESTX7A                                 09/96 X32 71360000
&#EBCDIC(124) SETC T'#TESTX7B                                 09/96 X32 71430000
&#EBCDIC(125) SETC T'#TESTX7C                                 09/96 X32 71510000
&#EBCDIC(126) SETC ''''''                                               71580000
&#EBCDIC(127) SETC T'#TESTX7E                                 09/96 X32 71650000
&#EBCDIC(128) SETC T'#TESTX7F                                 09/96 X32 71720000
&#EBCDIC(129) SETC T'#TESTX80                                 09/96 X32 71790000
&#EBCDIC(130) SETC T'#TESTX81                                 09/96 X32 71860000
&#EBCDIC(131) SETC T'#TESTX82                                 09/96 X32 71930000
&#EBCDIC(132) SETC T'#TESTX83                                 09/96 X32 72010000
&#EBCDIC(133) SETC T'#TESTX84                                 09/96 X32 72080000
&#EBCDIC(134) SETC T'#TESTX85                                 09/96 X32 72150000
&#EBCDIC(135) SETC T'#TESTX86                                 09/96 X32 72220000
&#EBCDIC(136) SETC T'#TESTX87                                 09/96 X32 72290000
&#EBCDIC(137) SETC T'#TESTX88                                 09/96 X32 72360000
&#EBCDIC(138) SETC T'#TESTX89                                 09/96 X32 72440000
&#EBCDIC(139) SETC T'#TESTX8A                                 09/96 X32 72510000
&#EBCDIC(140) SETC T'#TESTX8B                                 09/96 X32 72580000
&#EBCDIC(141) SETC T'#TESTX8C                                 09/96 X32 72650000
&#EBCDIC(142) SETC T'#TESTX8D                                 09/96 X32 72720000
&#EBCDIC(143) SETC T'#TESTX8E                                 09/96 X32 72790000
&#EBCDIC(144) SETC T'#TESTX8F                                 09/96 X32 72870000
&#EBCDIC(145) SETC T'#TESTX90                                 09/96 X32 72940000
&#EBCDIC(146) SETC T'#TESTX91                                 09/96 X32 73010000
&#EBCDIC(147) SETC T'#TESTX92                                 09/96 X32 73080000
&#EBCDIC(148) SETC T'#TESTX93                                 09/96 X32 73150000
&#EBCDIC(149) SETC T'#TESTX94                                 09/96 X32 73220000
&#EBCDIC(150) SETC T'#TESTX95                                 09/96 X32 73290000
&#EBCDIC(151) SETC T'#TESTX96                                 09/96 X32 73370000
&#EBCDIC(152) SETC T'#TESTX97                                 09/96 X32 73440000
&#EBCDIC(153) SETC T'#TESTX98                                 09/96 X32 73510000
&#EBCDIC(154) SETC T'#TESTX99                                 09/96 X32 73580000
&#EBCDIC(155) SETC T'#TESTX9A                                 09/96 X32 73650000
&#EBCDIC(156) SETC T'#TESTX9B                                 09/96 X32 73720000
&#EBCDIC(157) SETC T'#TESTX9C                                 09/96 X32 73800000
&#EBCDIC(158) SETC T'#TESTX9D                                 09/96 X32 73870000
&#EBCDIC(159) SETC T'#TESTX9E                                 09/96 X32 73940000
&#EBCDIC(160) SETC T'#TESTX9F                                 09/96 X32 74010000
&#EBCDIC(161) SETC T'#TESTXA0                                 09/96 X32 74080000
&#EBCDIC(162) SETC T'#TESTXA1                                 09/96 X32 74150000
&#EBCDIC(163) SETC T'#TESTXA2                                 09/96 X32 74230000
&#EBCDIC(164) SETC T'#TESTXA3                                 09/96 X32 74300000
&#EBCDIC(165) SETC T'#TESTXA4                                 09/96 X32 74370000
&#EBCDIC(166) SETC T'#TESTXA5                                 09/96 X32 74440000
&#EBCDIC(167) SETC T'#TESTXA6                                 09/96 X32 74510000
&#EBCDIC(168) SETC T'#TESTXA7                                 09/96 X32 74580000
&#EBCDIC(169) SETC T'#TESTXA8                                 09/96 X32 74650000
&#EBCDIC(170) SETC T'#TESTXA9                                 09/96 X32 74730000
&#EBCDIC(171) SETC T'#TESTXAA                                 09/96 X32 74800000
&#EBCDIC(172) SETC T'#TESTXAB                                 09/96 X32 74870000
&#EBCDIC(173) SETC T'#TESTXAC                                 09/96 X32 74940000
&#EBCDIC(174) SETC T'#TESTXAD                                 09/96 X32 75010000
&#EBCDIC(175) SETC T'#TESTXAE                                 09/96 X32 75080000
&#EBCDIC(176) SETC T'#TESTXAF                                 09/96 X32 75160000
&#EBCDIC(177) SETC T'#TESTXB0                                 09/96 X32 75230000
&#EBCDIC(178) SETC T'#TESTXB1                                 09/96 X32 75300000
&#EBCDIC(179) SETC T'#TESTXB2                                 09/96 X32 75370000
&#EBCDIC(180) SETC T'#TESTXB3                                 09/96 X32 75440000
&#EBCDIC(181) SETC T'#TESTXB4                                 09/96 X32 75510000
&#EBCDIC(182) SETC T'#TESTXB5                                 09/96 X32 75590000
&#EBCDIC(183) SETC T'#TESTXB6                                 09/96 X32 75660000
&#EBCDIC(184) SETC T'#TESTXB7                                 09/96 X32 75730000
&#EBCDIC(185) SETC T'#TESTXB8                                 09/96 X32 75800000
&#EBCDIC(186) SETC T'#TESTXB9                                 09/96 X32 75870000
&#EBCDIC(187) SETC T'#TESTXBA                                 09/96 X32 75940000
&#EBCDIC(188) SETC T'#TESTXBB                                 09/96 X32 76020000
&#EBCDIC(189) SETC T'#TESTXBC                                 09/96 X32 76090000
&#EBCDIC(190) SETC T'#TESTXBD                                 09/96 X32 76160000
&#EBCDIC(191) SETC T'#TESTXBE                                 09/96 X32 76230000
&#EBCDIC(192) SETC T'#TESTXBF                                 09/96 X32 76300000
&#EBCDIC(193) SETC T'#TESTXC0                                 09/96 X32 76370000
&#EBCDIC(194) SETC T'#TESTXC1                                 09/96 X32 76440000
&#EBCDIC(195) SETC T'#TESTXC2                                 09/96 X32 76520000
&#EBCDIC(196) SETC T'#TESTXC3                                 09/96 X32 76590000
&#EBCDIC(197) SETC T'#TESTXC4                                 09/96 X32 76660000
&#EBCDIC(198) SETC T'#TESTXC5                                 09/96 X32 76730000
&#EBCDIC(199) SETC T'#TESTXC6                                 09/96 X32 76800000
&#EBCDIC(200) SETC T'#TESTXC7                                 09/96 X32 76870000
&#EBCDIC(201) SETC T'#TESTXC8                                 09/96 X32 76950000
&#EBCDIC(202) SETC T'#TESTXC9                                 09/96 X32 77020000
&#EBCDIC(203) SETC T'#TESTXCA                                 09/96 X32 77090000
&#EBCDIC(204) SETC T'#TESTXCB                                 09/96 X32 77160000
&#EBCDIC(205) SETC T'#TESTXCC                                 09/96 X32 77230000
&#EBCDIC(206) SETC T'#TESTXCD                                 09/96 X32 77300000
&#EBCDIC(207) SETC T'#TESTXCE                                 09/96 X32 77380000
&#EBCDIC(208) SETC T'#TESTXCF                                 09/96 X32 77450000
&#EBCDIC(209) SETC T'#TESTXD0                                 09/96 X32 77520000
&#EBCDIC(210) SETC T'#TESTXD1                                 09/96 X32 77590000
&#EBCDIC(211) SETC T'#TESTXD2                                 09/96 X32 77660000
&#EBCDIC(212) SETC T'#TESTXD3                                 09/96 X32 77730000
&#EBCDIC(213) SETC T'#TESTXD4                                 09/96 X32 77800000
&#EBCDIC(214) SETC T'#TESTXD5                                 09/96 X32 77880000
&#EBCDIC(215) SETC T'#TESTXD6                                 09/96 X32 77950000
&#EBCDIC(216) SETC T'#TESTXD7                                 09/96 X32 78020000
&#EBCDIC(217) SETC T'#TESTXD8                                 09/96 X32 78090000
&#EBCDIC(218) SETC T'#TESTXD9                                 09/96 X32 78160000
&#EBCDIC(219) SETC T'#TESTXDA                                 09/96 X32 78230000
&#EBCDIC(220) SETC T'#TESTXDB                                 09/96 X32 78310000
&#EBCDIC(221) SETC T'#TESTXDC                                 09/96 X32 78380000
&#EBCDIC(222) SETC T'#TESTXDD                                 09/96 X32 78450000
&#EBCDIC(223) SETC T'#TESTXDE                                 09/96 X32 78520000
&#EBCDIC(224) SETC T'#TESTXDF                                 09/96 X32 78590000
&#EBCDIC(225) SETC T'#TESTXE0                                 09/96 X32 78660000
&#EBCDIC(226) SETC T'#TESTXE1                                 09/96 X32 78740000
&#EBCDIC(227) SETC T'#TESTXE2                                 09/96 X32 78810000
&#EBCDIC(228) SETC T'#TESTXE3                                 09/96 X32 78880000
&#EBCDIC(229) SETC T'#TESTXE4                                 09/96 X32 78950000
&#EBCDIC(230) SETC T'#TESTXE5                                 09/96 X32 79020000
&#EBCDIC(231) SETC T'#TESTXE6                                 09/96 X32 79090000
&#EBCDIC(232) SETC T'#TESTXE7                                 09/96 X32 79160000
&#EBCDIC(233) SETC T'#TESTXE8                                 09/96 X32 79240000
&#EBCDIC(234) SETC T'#TESTXE9                                 09/96 X32 79310000
&#EBCDIC(235) SETC T'#TESTXEA                                 09/96 X32 79380000
&#EBCDIC(236) SETC T'#TESTXEB                                 09/96 X32 79450000
&#EBCDIC(237) SETC T'#TESTXEC                                 09/96 X32 79520000
&#EBCDIC(238) SETC T'#TESTXED                                 09/96 X32 79590000
&#EBCDIC(239) SETC T'#TESTXEE                                 09/96 X32 79670000
&#EBCDIC(240) SETC T'#TESTXEF                                 09/96 X32 79740000
&#EBCDIC(241) SETC T'#TESTXF0                                 09/96 X32 79810000
&#EBCDIC(242) SETC T'#TESTXF1                                 09/96 X32 79880000
&#EBCDIC(243) SETC T'#TESTXF2                                 09/96 X32 79950000
&#EBCDIC(244) SETC T'#TESTXF3                                 09/96 X32 80020000
&#EBCDIC(245) SETC T'#TESTXF4                                 09/96 X32 80100000
&#EBCDIC(246) SETC T'#TESTXF5                                 09/96 X32 80170000
&#EBCDIC(247) SETC T'#TESTXF6                                 09/96 X32 80240000
&#EBCDIC(248) SETC T'#TESTXF7                                 09/96 X32 80310000
&#EBCDIC(249) SETC T'#TESTXF8                                 09/96 X32 80380000
&#EBCDIC(250) SETC T'#TESTXF9                                 09/96 X32 80450000
&#EBCDIC(251) SETC T'#TESTXFA                                 09/96 X32 80520000
&#EBCDIC(252) SETC T'#TESTXFB                                 09/96 X32 80600000
&#EBCDIC(253) SETC T'#TESTXFC                                 09/96 X32 80670000
&#EBCDIC(254) SETC T'#TESTXFD                                 09/96 X32 80740000
&#EBCDIC(255) SETC T'#TESTXFE                                 09/96 X32 80810000
&#EBCDIC(256) SETC T'#TESTXFF                                 09/96 X32 80880000
         AGO   .GENLP
.GNTEBCD MNOTE 8,'ERROR - &&GEN(&A1)=&GEN(&A1) IS UNRECOGNIZED'
         AGO   .GENLP
.GENEND  ANOP
.*
.*
.*
.END     MEND
         MACRO
         #USING &D
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $USING TO #USING.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - AUGUST 23, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A USING INSTRUCTION THAT REDECLARES ALL BASES
.* (IF ANY) DECLARED BY A PRIOR #ENTER MACRO EXPANSION.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLC  &#BS(14)
         LCLA  &A1,&DISPL
         AIF   ('&#BS(14)' EQ '').END
&DISPL   SETA  &DISPL-4095
&A1      SETA  14
.LP      AIF   (&A1 EQ 2).END
&A1      SETA  &A1-1
         AIF   ('&#BS(&A1)' EQ '').LP
&DISPL   SETA  &DISPL+4095
         USING &#BS(14)+&DISPL,&#BS(&A1)
         AGO   .LP
.END     MEND
OFFL     TITLE 'OFFLOAD -- OUTPUT PDS MEMBERS TO A SEQUENTIAL FILE'
*          DATA SET CBT880     AT LEVEL 002 AS OF 10/28/84
*  THIS PROGRAM NEEDS DAVE COLE'S MACLIB - CBT TAPE FILE 408.       NWK
******************************************************************* NWK
******************************************************************* NWK
*   THIS VERSION OF DAVE COLE'S OFFLOAD PROGRAM HAS BEEN MODIFIED   NWK
*    WITH A GLOBAL VARIABLE SO THE LRECL OF THE INPUT AND OUTPUT    NWK
*    DATASETS CAN BE CHANGED MERELY BY MODIFYING THE GLOBAL AND     NWK
*    REASSEMBLING THE PROGRAM.  AN EYECATCHER HAS BEEN ADDED SO     NWK
*    THAT YOU CAN SEE THE ASSEMBLED LRECL BY BROWSING THE LOAD      NWK
*    MODULE.  SEE LABEL "EYECATCH".                                 NWK
*                                                                   NWK
*   YOU NEED TO REASSEMBLE THE PROGRAM EVERY TIME YOU WANT TO       NWK
*    CHANGE THE LRECL THAT THE PROGRAM TAKES.  (ADMITTEDLY IT'S     NWK
*    BETTER TO DO THIS WITH A PARM AT EXECUTION TIME.)  BUT THIS    NWK
*    WORKS.  (IT HAS NOT BEEN TESTED WITH LRECL MUCH LESS THAN 80.) NWK
*                                                                   NWK
*             S. GOLOB - NEWSWEEK - MOUNTAIN LAKES, N.J. - JULY 87  NWK
*   This note is superseded by Greg Price's fix for multiple LRECL SBG.
* --------------------------------------------------------------- .SBG.
*   Added capability of handling 8-character ISPF userids. 17.024 SG01
* --------------------------------------------------------------- .SBG.
*   Fixed by EX-PRC-GUY:                                          .SBG.
*                                                                 .SBG.
*   Added an optional DD name of SYSUPLOG, FB-80, which, will     .SBG.
*   flag exception records already beginning with "><" in column  .SBG.
*   1 in each pds member that has them.  When this exception file .SBG.
*   is used in conjunction with the modified PDSLOAD program,     .SBG.
*   then the "><" strings for those records, WILL NOT be changed  .SBG.
*   to "./" upon reload of the pds from the sequential dataset    .SBG.
*   produced by this program.                                     .SBG.
*                                                                 .SBG.
******************************************************************* NWK
*   31MAY93 - ONLY OFFLOAD AN ALIAS IF IT IS IN THE EXPLICIT-SELECT
*             LIST.
*           - REMOVE THE &LRECL GLOBAL VARIABLE AND HANDLE ANY
*             FIXED LRECL FROM 1 TO 256.  NOTE THAT A PDS-SDS-PDS
*             CYCLE WITH OFFLOAD+PDSLOAD CAN BE USED TO TRUNCATE OR
*             EXTENDED THE LOGICAL RECORDS OF A PDS TO ANY VALUE IN
*             THE 1 TO 256 RANGE, BUT THE INTERMEDIATE SEQUENTIAL
*             DATA SET HAS A MINIMUM LRECL OF 80.  THIS IS COMPATIBLE
*             WITH IBM'S IEBUPDTE UTILITY.  IF IT IS LESS THAN 80
*             THEN THIS PROGRAM WILL FORCE LRECL=80,BLKSIZE=3120.
*           - ADD SUPPORT FOR THE SSI OPERAND AS PER IEBUPDTE FORMAT.
*           - ADD SUPPORT FOR ISPF STATISTICS IN THE IEBUPDTE COMMENT
*             AREA AS PER PDSLOAD CONTROL STATEMENT FORMAT.
*
*   SUMMARY - THIS PROGRAM CAN OFFLOAD PDS MEMBERS INTO A SEQUENTIAL
*             DATA SET SUCH THAT THE PDS CAN BE RECREATED FROM THE
*             SEQUENTIAL DATA SET.  THIS MAY BE HANDY FOR TRANSMITTING
*             FILES VIA NJE.  NOTE THAT ALIASES ARE ONLY OFFLOADED IF
*             EXPLICITLY REQUESTED BY NAME, AND THEN ONLY AS IF THEY
*             WERE REAL MEMBERS.  THAT IS, IEBUPDTE'S ALIAS STATEMENT
*             IS NEVER OUTPUT.  SSI OR ISPF STATISTICS DATA WILL ALSO
*             BE OFFLOADED FOR ANY MEMBER THAT IS OFFLOADED.  IBM'S
*             IEBUPDTE CAN BE USED TO RE-CREATE A PDS WITH AN LRECL
*             VALUE IN THE RANGE OF 1 TO 80 INCLUSIVE.  MEMBERS' SSI
*             DATA WILL ALSO BE RESTORED BY IEBUPDTE.  THE PDSLOAD
*             PROGRAM CAN BE USED TO RE-CREATE A PDS WITH AN LRECL
*             VALUE IN THE RANGE OF 1 TO 256 INCLUSIVE.  PDSLOAD WILL
*             ALSO RESTORE MEMBERS' SSI AND ISPF USERDATA.
*     =====>  ONLY FIXED-LENGTH RECORD FILES   <=====
*     =====>  CAN BE PROCESSED BY THIS PROGRAM.<=====
*             TO GET A SEQUENTIAL FILE THAT IS SUITABLE INPUT FOR
*             IEBUPDTE OR PDSLOAD, USE A JOB STREAM LIKE:
*
*     //JOBNAME  JOB (ACCOUNT#),PGMR,CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)
*     //OFFLOAD EXEC PGM=OFFLOAD
*     //SYSPRINT DD  SYSOUT=*
*     //SYSUPLOG DD  DISP=SHR,DSN=seq.file.or.pds(member)  >>OPTIONAL<<
*     //SYSUT1   DD  DSN=THE.INPUT.PDS,DISP=SHR
*     //SYSUT2   DD  DSN=THE.OUTPUT.FILE,DISP=(NEW,CATLG,DELETE),
*     //             UNIT=SYSDA,SPACE=(CYL,(10,10),RLSE),
*     //             DSORG=PS,RECFM=FB,LRECL=###,BLKSIZE=0
*     //SYSIN    DD  *
*       O I=SYSUT1,O=SYSUT2,T=IEBUPDTE
*     /*
*     //
*
*             GREG PRICE,                                        GP@FT
*   31MAY1993 FERNTREE COMPUTER SERVICES, MELBOURNE, AUSTRALIA   GP@FT
*
*   01MAR2008 PRYCROFT SIX PTY LTD  (WWW.PRYCROFT6.COM.AU)       GP@P6
*
*   23OCT2015 OPTIONAL SYSUPLOG ADDED TO LOG EXISTING >< LINES  .UPLOG.
*
*   25OCT2015 EXTRA LINE ADDED TO SYSPRINT REPORT SHOWING         .SBG.
*             SYSUPLOG RECORD COUNT, IF ONE EXISTS, AS FOLLOWS:   .SBG.
*                                                                 .SBG.
*     Exception Report:          39    SYSUPLOG Records Processed .SBG.
*                                                                 .SBG.
*             THIS WILL CLUE YOU IN, TO SEE IF ANY SYSUPLOG       .SBG.
*             PROCESSING IS NEEDED UPON PDS RELOAD.               .SBG.
*             ADDED VERSION NUMBER V10.0 TO LET YOU KNOW THAT     .SBG.
*             YOU ARE USING THIS VERSION OF THE PROGRAM           .SBG.
*             (COORDINATES WITH VERSION 10.0 ON THE PDSLOADW      .SBG.
*             PROGRAM).                                           .SBG.
*
*   01FEB2017 8-CHARACTER ISPF USERIDS.  (Version 10.1)           SG01
*
***********************************************************************
         GBLC  &VER                                               .SBG.
&VER     SETC  'V10.1'                                            .SBG.
         TITLE 'OFFLOAD -- MISCELLANEOUS EQUATES'
OFFLOAD  CSECT
         #REGS R
         SPACE 3
LINECT   EQU   58                  LINES PER PAGE
OSCORE   EQU   4096                SIZE OF OS ELBOW ROOM
BUFSPACE EQU   1000                APPROXIMATE REASONABLE BUF SPACE
         SPACE 3
         #PUT  SUBAD=PUTSYSP,MF=INIT
         TITLE 'OFFLOAD -- DSECTS'
         PRINT NOGEN
         SPACE 3
         CVT   DSECT=YES
         SPACE 3
         IKJTCB
         SPACE 3
TIOT     DSECT
         IEFTIOT1
         SPACE 3
DCB      DSECT
         #DCBD DSORG=(QS,PO)
         SPACE 3
OPENWORK DSECT ,
         ORG   OPENWORK+100
         IEFJFCBN
         SPACE 3
         PRINT GEN
         TITLE 'OFFLOAD -- DSECTS - DATA EVENT CONTROL BLOCK'
DECB     DSECT ,
DECBECB  DS    A                   EVENT CONTROL BLOCK
DECBTYPE DS    2X                  FLAG BYTES
DECBLGTH DS    Y                   KEY PLUS DATA LENGTH
DECBDCB  DS    A                   DCB POINTER
DECBBUFR DS    A                   BUFFER POINTER
DECBIOB  DS    A                   IOB POINTER
DECBLINK DS    A                   LINK TO RELATED DECB
DECBLEN  EQU   *-DECB              DECB LENGTH
         TITLE 'OFFLOAD -- DSECTS - PROGRAM INTERRUPT ELEMENT'
PIE      DSECT ,
PIEPICA  DS    A                   PICA PTR
PIEOPSW  DS    XL8                 OLD PSW
PIER14   DS    A                   R14 SAVE AREA
PIER15   DS    A                   R15 SAVE AREA
PIER0    DS    A                   R0 SAVE AREA
PIER1    DS    A                   R1 SAVE AREA
PIER2    DS    A                   R2 SAVE AREA
         TITLE 'OFFLOAD -- INITIAL ENTRY'
OFFLOAD  #ENTER BASES=3,SAVTYPE=(LOCAL,BASE)
         SPACE 1
         B     EYECATCH                                             NWK
         DC    C' ASSEMBLED FOR ANY FIXED LRECL FROM 1 TO 256  '  GP@FT
EYECATCH DS    0H                                                   NWK
* REINITIALIZE DATA FOR SERIAL REUSABILITY
         SP    PAGECTR,PAGECTR
         XC    LINES2GO,LINES2GO
         MVI   OUTDDNAM,C' '
         MVI   RCD+1,0
         MVI   FLAG,0
         SPACE 1
* OPEN UNIT RECORD DATA SETS
         OPEN  (SYSIN,,SYSPRINT,OUTPUT)
         TM    PRTOFLGS,DCBOFOPN   SYSPRINT OPENED OK?
         BZ    EXIT16              NO, TERMINATE
         #PUT  TITLE1              YES, SET UP A TITLE
         TM    RDROFLGS,DCBOFOPN   SYSIN OPENED OK?
*-       BO    CONTROL             YES, CONTINUE               -,UPLOG.
         BO    UPLOPEN             YES, CONTINUE                ,UPLOG.
         #PUT  NOSYSIN             NO, PRINT AN ERROR MSG
         B     EXIT16              GO TERMINATE
UPLOPEN  SR    R15,R15                                          ,UPLOG.
         ST    R15,UPLSTAT         INDICATE SYSUPLOG ABSENT     ,UPLOG.
         DEVTYPE UPLDDNAM,WORKAREA                              ,UPLOG.
         LTR   R15,R15             IS THERE A SYSUPLOG DDNAME   ,UPLOG.
         BNZ   CONTROL             NO, SKIP OPEN                ,UPLOG.
         MVI   UPLSTAT+3,1         YES, INDICATE IT IS PRESENT  ,UPLOG.
         OPEN  (UPL,OUTPUT)                                     ,UPLOG.
         MVI   UPLCARD,C' '        FILL UPLCARD                 ,UPLOG.
         MVC   UPLCARD+1(79),UPLCARD  WITH BLANKS               ,UPLOG.
         TITLE 'OFFLOAD -- ANALYZE THE SYSIN CONTROL CARDS'
* GET WORKING STORAGE
CONTROL  TM    RDROFLGS,DCBOFOPN   READER STILL OPENED?
         BZ    EOP                 NO, END OF PROGRAM
         GETMAIN VC,LA=GETMQTY,A=WORKSTAR YES, GET WORKING CORE
         LTR   R15,R15             ANY GOTTEN?
         BZ    COREOK              YES, CONTINUE
         #PUT  NOCOREM             NO, ISSUE ERROR MSG
         B     EXIT16              TERMINATE
COREOK   LM    R0,R1,WORKSTAR      GET CORE PTR & LEN
         AR    R1,R0               PNT TO CORE END
         LH    R0,=Y(OSCORE)       GET OS ELBOW ROOM SIZE
         SR    R1,R0               GET END OF CORE TO KEEP
         ST    R1,WORKEND          SAVE FOR LATER
         FREEMAIN R,LV=(0),A=(1)   GIVE OS SOME ELBOW ROOM
         TM    FLAG,UNLOAD         NEXT CARD ALREADY READ?
         MVI   FLAG,0              (CLEAR FLAGS IN ANY CASE)
         BO    GOTCARD             YES, GO PROCESS IT
         SPACE 1
* SETUP TO SKIP TO NEXT "O" CARD
RESTART  MVI   FLAG,0              CLEAR FUNCTION FLAGS
GETCARD1 MVI   FLAG2,0             CLEAR SYNTAX FLAGS
         MVI   PRECARD,C' '        CLEAR ERROR -
         MVC   PRECARD+1(L'PRECARD-2),PRECARD  MSG AREA
         SPACE 1
* GET THE NEXT CARD AND CHECK IT FOR SUBSTANCE
GETCARD2 GET   SYSIN,CARD          GET THE NEXT CARD
         CLC   CARD(71),CARD-1     CARD BLANK?
         BE    GETCARD2            YES, IGNORE IT
         CLI   CARD,C'*'           NO, COMMENT CARD?
         BE    CARDECHO            YES, GO PRINT IT
         SPACE 1
* SEARCH FOR AND IDENTIFY THE CARD'S VERB
GOTCARD  LA    R3,CARD-1           GET CARD COLUMN SCANNER
         LA    R4,1                GET BXLE INCREMENT
         LA    R5,CARD+70          GET BXLE LIMIT
NAMELP   BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         CLI   0(R3),C' '          BLANK YET?
         BNE   NAMELP              NO, STILL THE NAME FIELD
VERBLP   BXH   R3,R4,CARDSYNT      YES, SCAN TO NEXT COLUMN
         CLI   0(R3),C' '          STILL BLANK?
         BE    VERBLP              YES, LOOP BACK
         LA    R3,1(,R3)           NO, POINT PAST THE VERB
         CLI   0(R3),C' '          VERB MUST BE A SINGLE CHARACTER
         BNE   CARDSYNT            IT'S NOT; ERROR
         BCTR  R3,0                IT IS; POINT BACK TO IT
         CLI   0(R3),C'O'          "OFFLOAD" CARD?
         BE    VO                  YES, GO PROCESS
         TM    FLAG,UNLOAD         NO, MUST IT BE?
         BZ    CARDIGNR            YES, IGNORE THE CARD
         CLI   0(R3),C'S'          NO, SELECT CARD?
         BE    VS                  YES, GO PROCESS
         CLI   0(R3),C'E'          NO, EXCLUDE CARD?
         BNE   CARDSYNT            NO, ERROR
         TM    FLAG,SELECT         YES, ALREADY SELECT CARDS?
         BO    CARDNCAP            YES, ERROR
         OI    FLAG,EXCLUDE        NO, REMEMBER EXCLUDE
         B     GETOPND             GO GET OPERANDS
VS       TM    FLAG,EXCLUDE        SELECT GIVEN; PREVIOUS EXCLUDE?
         BO    CARDNCAP            YES, ERROR
         OI    FLAG,SELECT         NO, REMEMBER SELECT
         B     GETOPND             GO GET OPERANDS
VO       TM    FLAG,UNLOAD         OFFLOAD GIVEN; ALREADY GOT ONE?
         BO    PROCESS             YES, GO PERFORM FUNCTION
         OI    FLAG,UNLOAD         NO, GOT ONE NOW
         #PUT  BLNK3               PUT SOME BLANK LINES
         MVI   PRECARD,C'-'        ECHO CARRIAGE CONTROL
         SPACE 1
* SEARCH FOR OPERANDS
GETOPND  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         CLI   0(R3),C' '          STILL BLANK?
         BE    GETOPND             YES, LOOP BACK
         LA    R3,1(,R3)           NO, PNT PAST KEYWORD
         CLI   0(R3),C'='          KEYWD MUST BE 1 CHARACTER LONG
         BNE   CARDSYNT            IT'S NOT; ERROR
         BCTR  R3,0                IT IS; POINT BACK TO IT
         ST    R3,SAVESCAN         SAVE THE POINTER ICO ERROR
         NI    FLAG2,255-DDNAME    CLEAR DDN FLAG
         TM    FLAG,SELECT+EXCLUDE UNLOAD CARD?
         BNZ   NOTUNLD             NO, SKIP
         SPACE 1
* IDENTIFY UNLOAD CARD OPERANDS
         CLI   0(R3),C'T'          "T="?
         BE    TOPND               YES, GO PROCESS
         OI    FLAG2,DDNAME        NO, OPND OBJECTS MUST BE DDNAMES
         CLI   0(R3),C'I'          "I="?
         BE    IOPND               YES, GO PROCESS
         CLI   0(R3),C'O'          NO, "O="?
         BNE   CARDSYNT            NO, ERROR
         SPACE 1
* PROCESS THE "O=" OPERAND
         TM    FLAG2,GOTOUT        YES, ALREADY GOT "O="?
         BO    CARDRDUN            YES, ERROR
         OI    FLAG2,GOTOUT        NO, GOT ONE NOW
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         BAL   R14,GETNAME         SCAN OUT THE DDNAME
         LA    R2,OUT              POINT TO THE OUTPUT DCB
         BAL   R14,CLOSE           ENSURE THAT IT IS CLOSED
         MVC   OUTDDNAM,WORKAREA   RESET THE DCB'S DDNAME
         B     OPNDNEXT            GO SCAN FOR NEXT OPERAND
         SPACE 1
* PROCESS THE "T=" OPERAND
TOPND    TM    FLAG2,GOTTYPE       "T=" ALREADY ENCOUNTERED?
         BO    CARDRDUN            YES, REDUNDANCY ERROR
         OI    FLAG2,GOTTYPE       NO, REMEMBER ICO ANOTHER
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         BAL   R14,GETNAME         GO VERIFY SYNTAX
         LR    R14,R3              SAVE EOD OF OPND PTR
         L     R3,SAVESCAN         PNT BACK TO START OF OPERAND
         LA    R15,=C'IEBUPD'      PNT TO VALID PREFIXES
         LA    R1,5                GET LOOP CONTROL
IEBLOOP  STC   R1,IEBCLC+1         SET CLC LENGTH FIELD
         NOPR  0                   (FOR CP-67 TRACE)
IEBCLC   CLC   0(*-*,R3),0(R15)    PREFIX OK?
         BE    IEBOK1              YES, PROCEED
         LA    R15,1(,R15)         NO, PNT TO NEXT VALID PREFIX
         BCTR  R1,0                GET ITS MACHINE LENGTH
         LTR   R1,R1               ALL VALID PREFIXES TESTED?
         BNM   IEBLOOP             NOPE, GO TEST THIS ONE
IEBOK1   LA    R3,1(R1,R3)         YEP, ASSUME NO PREFIX; PNT PAST PFX
         CLC   0(2,R3),=C'TE'      "T=IEBUPDTE"?
         BNE   IEBTRY2             NO, SKIP
         OI    FLAG,UPDTE          YES, REMEMBER
         B     IEBOK2              DONE HERE
IEBTRY2  CLC   0(2,R3),=C'AT'      NO, "T=IEBUPDAT"?
         BNE   CARDSYNT            NO, SYNTAX ERROR
         OI    FLAG,UPDAT          YES, REMEMBER IT
IEBOK2   LA    R3,2(,R3)           PNT PAST "TE"/"AT"
         CR    R14,R3              AT END OF OPERAND?
         BNE   CARDSYNT            NO, ERROR
         B     OPNDNEXT            YES, GO SCAN NEXT OPERAND
         SPACE 1
* PROCESS THE "I=" OPERAND
IOPND    TM    FLAG2,GOTIN         ALREADY GOT "I="?
         BO    CARDRDUN            YES, ERROR
         OI    FLAG2,GOTIN         NO, GOT ONE NOW
         L     R0,WORKSTAR         GET START OF INPUT DDNAMES LIST
         LR    R1,R0               INITIALIZE IT TO EMPTY
         BAL   R14,PUTNAMES        GO BUILD THE DDNAMES LIST
         ST    R1,ENDDDNS          SAVE END OF DDNAMES LIST
         SPACE 1
* CHECK FOR MORE UNLOAD STATEMENT OPERANDS
OPNDNEXT CLI   0(R3),C','          MORE OPERANDS?
         BNE   CARDEND             NO, SKIP
         CLI   1(R3),C' '          YES, CONTU CARD NEEDED?
         BNE   GETOPND             NO, JUST GO GET NEXT OPERAND
         BAL   R14,NEXTCARD        YES, GET NEXT CARD
         B     GETOPND             GO GET NEXT OPERAND
CARDEND  TM    FLAG2,GOTIN         "I=" GIVEN?
         BZ    CARDMISO            NO, ERROR
         MVC   ENDMNMS,ENDDDNS     INITIALIZE MEMBER NAMES LIST EMPTY
         TM    OUTOFLGS,DCBOFOPN   "O=" OPERAND REQUIRED?
         BO    CARDECHO            NO, GO ECHO THE CARD
         CLI   OUTDDNAM,C' '       YES, "O=" GIVEN?
         BE    CARDMISO            NO, ERROR
         B     CARDECHO            YES, GO ECHO THE CARD
         SPACE 1
* PROCESS S/E CARD OPERANDS
NOTUNLD  CLI   0(R3),C'M'          "M="?
         BNE   CARDSYNT            NO, ERROR
         L     R0,ENDDDNS          YES, PNT TO START OF MBR NMES LIST
         L     R1,ENDMNMS          PNT TO END OF MBR NMES LIST
         BAL   R14,PUTNAMES        GO GET MORE NAMES FROM THIS CARD
         ST    R1,ENDMNMS          STORE NEW END OF LIST
         CLI   0(R3),C' '          MORE OPERANDS?
         BNE   CARDSYNT            ERROR; THERE ARE MORE OPERANDS
         SPACE 1
* ECHO PRINT THE CONTROL CARD
CARDECHO #PUT  PRECARD             ECHO THE GOOD CARD
         B     GETCARD1            GO GET NEXT CARD
         SPACE 1
* SYSIN END OF DATA ROUTINE
RDREOD   LA    R2,SYSIN            POINT TO THE DCB
         BAL   R14,CLOSE           GO CLOSE THE DCB
         TM    FLAG2,CONTINUE      READING CONTINU CARD?
         BZ    EODOK               NO, AOK
         #PUT  EODERR              YES, ERROR
         MVI   RCD+1,8             SET COMPLETION CODE
         B     EOP                 END OF PROGRAM
EODOK    TM    FLAG,UNLOAD         ANYTHING LEFT TO DO?
         BZ    EOP                 NO, GO TERMINATE
         TITLE 'OFFLOAD -- PREPARE TO PERFORM THE CURRENT OFFLOAD FUNCT*
               ION'
* INSERT THE MEMBER NAMES LIST TRAILER
PROCESS  L     R0,WORKEND          PNT TO END OF WORK AREA
         L     R14,ENDMNMS         PNT TO END OF USED PART
         LA    R1,L'DCBDDNAM(,R14) MAKE ROOM FOR TRAILER ENTRY
         SR    R0,R1               ENOUGH ROOM?
         BNM   NOTFULL             YES, CONTINUE
         #PUT  NOCOREM             NO, ISSUE ERROR MSG
         B     RCD8                LOOP TO NEXT FUNCTION
         SPACE 1
* RELEASE ANY UNUSED PART OF THE WORK AREA
NOTFULL  MVC   0(L'DCBDDNAM,R14),EFFS GET TRAILER ENTRY
         ST    R1,WORKEND          SET NEW END OF WORK AREA
         BZ    NOFREE              NONE, SKIP
         FREEMAIN R,LV=(0),A=(1)   SOME; FREE IT
         SPACE 1
* SORT THE MEMBER NAMES LIST
NOFREE   L     R1,ENDDDNS          PNT TO START OF MEMBER NAMES
         L     R15,WORKEND         PNT TO END OF SAME
         LA    R8,L'MEMNAME        GET ENTRY LENGTH
         SR    R15,R1              GET LENGTH OF LIST
         LR    R6,R8               GET ENTRY LENGTH (EL)
SORTSLL  AR    R6,R6               GET EL*(2**N)
         CR    R6,R15              LIST LEN EXCEED YET?
         BNH   SORTSLL             NO, LOOP
         SR    R6,R8               YES, GET EL*((2**N)-1)
SORTLP1  SR    R6,R8
         BZ    SORTED
         SRL   R6,1
         LR    R9,R15
         SR    R9,R6
         LR    R7,R8
SORTLP2  LR    R4,R8
SORTLP3  LA    R2,0(R7,R1)
         SR    R2,R4               GET 1ST COMPARE LOCATION
         LA    R3,0(R6,R2)         GET 2ND COMPARE LOCATION
         CLC   0(L'MEMNAME,R2),0(R3) SHOULD THESE 2 ELE BE SWITCHED?
         BNH   SORTNLP2            NO, SKIP
         XC    0(L'MEMNAME,R2),0(R3) YES, -
         XC    0(L'MEMNAME,R3),0(R2)  DO -
         XC    0(L'MEMNAME,R2),0(R3)   SO
         BXLE  R4,R6,SORTLP3       LOOP TO SEE IF ELE IS IN RIGHT PLACE
SORTNLP2 BXLE  R7,R8,SORTLP2       RIGHT PLACE, LOOP 4 NXT ELE
         B     SORTLP1             LOOP FOR NEXT LEVEL
         SPACE 1
* OPEN THE OUTPUT DATA SET
SORTED   TM    OUTOFLGS,DCBOFOPN   OUTPUT DS ALREADY OPEN?
         BO    OUTOPEND            YES, SKIP
         MVC   OFFLDDNS,OFFLDDNS-1 NO, CLEAR INFO MSG
         MVC   OFFLDDNS(L'DCBDDNAM),OUTDDNAM GET OUTPUT DDN TO MSG
         LA    R1,OFFLDDNS         SETUP FOR BLANK SCAN
OFFLBLNK LA    R1,1(,R1)           PNT TO NEXT COLUMN
         CLI   0(R1),C' '          BLANK YET?
         BNE   OFFLBLNK            NO, LOOP
         ST    R1,OFFLPTR          YES, REMEMBER THIS SPOT
         MVC   1(4,R1),=C'FROM'    INSERT MORE MSG TEXT
         OPEN  (OUT,OUTPUT)        NO, OPEN IT
* NOTE - THE CONTROL CARD SCAN HAS ALREADY VERIFIED THAT THIS OUTPUT DD
* CARD EXISTS.
OUTOPEND L     R10,WORKSTAR        PNT TO INPUT DDNAMES
         TITLE 'OFFLOAD -- OFFLOAD (SELECTED) PDS MEMBERS'
* REGISTER USAGE IN THIS SECTION
*        R10 --> DDNAME OF CURRENT INPUT DATA SET
*        R7 --> CURRENT DIRECTORY ENTRY
*        R6 --> NEXT MEMBER NAME TO BE SELECTED OR EXCLUDED
*        R2 --> CURRENT INPUT DECB
         SPACE 1
* OPEN AN INPUT DATA SET
DATSETLP SR    R0,R0               GET A ZERO
         STH   R0,INBLKSI          ENSURE BLKSIZE CLEAR
         STH   R0,INBUFL           ENSURE BUFL CLEAR
         STC   R0,INBUFNO          ENSURE BUFNO CLEAR
         MVC   INDDNAM,0(R10)      GET INPUT DDNAME
         L     R1,OFFLPTR          PNT TO INFO MSG TEXT
         MVC   6(L'DCBDDNAM,R1),0(R10) GET INPUT DDNAME
         NI    FLAG,255-NOTPDS     ASSUME DS IS PARTITIONED
         OPEN  IN                  OPEN THE INPUT DATA SET
         TM    FLAG,NOTPDS         PDS?
         BZ    ISPDS               YES, AOK
         LA    R1,2+L'DCBDDNAM+SEQFAKLN NO, -
         STH   R1,RDAREA            FAKE A -
         MVC   RDAREA+2(L'DCBDDNAM),0(R10) DIRECTORY -
         MVC   RDAREA+2+L'DCBDDNAM(SEQFAKLN),SEQFAKRY BLOCK.
         B     SEQFAKIT            GO PROCESS AS A SINGLE MEMBER
* NOTE - THE CONTROL CARD SCAN HAS ALREADY VERIFIED THAT THIS INPUT DD
* CARD EXISTS.
         SPACE 1
* PARTITIONED INPUT. OPEN THE DIRECTORY
ISPDS    MVC   DRCDDNAM,0(R10)     GET THE DDNAME
         OPEN  DRCTY               OPEN THE DIRECTORY
         SPACE 1
* SETUP FOR DIRECTORY BLOCK READ AND SCAN
         XC    RDAREA(2),RDAREA    FORCE INITIAL DRCTY BLK READ
SEQFAKIT L     R6,ENDDDNS          PNT TO START OF MEMBER NAMES
DRCTYLP  LH    R1,RDAREA           GET USED LENGTH OF DRCTY BLK
         LA    R1,RDAREA(R1)       PNT PAST THE USED AREA
         ST    R1,DRCTYEND         SAVE FOR LATER
         MVI   RDAREA+1,0          SETUP TO ADVANCE TO 1ST ENTRY
         LA    R7,RDAREA-10        SETUP TO ADVANCE TO 1ST ENTRY
         SPACE 1
* SCAN THE DIRECTORY BLOCK FOR THE NEXT DIRECTORY ENTRY
         USING MEMBER,R7
MEMBERLP NI    MEMC,X'1F'          ISOLATE ENTRY LENGTH
         SR    R1,R1               CLEAR FOR 'IC'
         IC    R1,MEMC             GET ENTRY 'LENGTH'
         LA    R1,12(R1,R1)        GET REAL ENTRY LENGTH
         AR    R7,R1               PNT TO NEXT DIRECTORY ENTRY
         C     R7,DRCTYEND         END OF THIS BLOCK?
         BL    FINDMBR             NO, CONTINUE PROCESSING THIS BLK
         SPACE 1
* READ THE NEXT DIRECTORY BLOCK. HANDLE EOD OR I/O ERRORS
         GET   DRCTY,RDAREA        READ THE DIRECTORY BLOCK
         B     DRCTYLP             LOOP TO SCAN IT
DRCERR   MVC   DRCTYERR+L'DRCTYERR(L'DCBDDNAM),0(R10) NO, GET DDNAME
         #PUT  DRCTYERR            PRINT ERROR MSG
         MVI   RCD+1,8             SET COMPLETION CODE
         B     DSDONE              SKIP TO NEXT DATA SET
         SPACE 1
* DETERMINE IF THE MEMBER POINTED TO BY THE CURRENT DIRECTORY ENTRY
* SHOULD BE OFFLOADED
FINDMBR  CLC   MEMNAME,EFFS        EO DRCTY 4 THIS DS?
         BE    DSDONE              YES, SKIP OUT
NMEGOTST CLC   MEMNAME,0(R6)       NO, CURRENT NAME IN THE LIST?
         BL    NOGOTNME            NO, GO CHECK FUNCTION
         BE    GOTNME              YES, GO CHECK FUNCTION
         LA    R6,L'DCBDDNAM(,R6)  DON'T KNOW; ADVANCE LIST SCANNER
         B     NMEGOTST            GO TEST NAME AGAIN
NOGOTNME TM    FLAG,SELECT         SELECT FUNCTION?
         BO    MEMBERLP            YES, SKIP THIS MEMBER
         TM    MEMC,X'80'          IS THIS AN ALIAS?             GP@FT
         BO    MEMBERLP            YES, SKIP THIS MEMBER         GP@FT
         B     GOTMEMBR            NO, OFFLOAD THIS MEMBER
GOTNME   TM    FLAG,EXCLUDE        EXCLUDE FUNCTION?
         BO    MEMBERLP            YES, SKIP THIS MEMBER
         XC    0(L'DCBDDNAM,R6),0(R6) NO, CLR NME FROM LIST
         SPACE 1
* MEMBER LOCATED - TELL USER ABOUT IT
GOTMEMBR NI    FLAG,255-CTLCDERR   CLEAR IEBUPDTE CTL CD FND ERR FLG
         MVC   OFFLMBRN,MEMNAME    PNT MEMBER NAME INTO INFO MSG
         #PUT  OFFLMSG             TELL USER WHAT'S HAPPENING
         SPACE 1
* POINT THE INPUT DATA SET TO THE MEMBER TO BE OFFLOADED
         TM    FLAG,NOTPDS         SEQUENTIAL DS?
         BO    SKIPFIND            YES, FORGET ABOUT "FIND"
         MVC   BLDLTTR0(3),MEMTTR  SET TTR OF TTR0
         POINT IN,BLDLTTR0         POINT TO THE CORRECT MEMBER
         SR    R15,R15                                            .UPL.
         ST    R15,UPLNUM          CLEAR RECORD COUNTER           .UPL.
         ST    R15,UPLCTR          CLEAR EXCEPTION COUNTER        .UPL.
         SPACE 1
* PRIME THE INPUT BUFFERS
SKIPFIND L     R2,BUFSSTAR         PNT TO 1ST DECB
         USING DECB,R2             DECLARE DECB BASE
PRIMELIB READ  (R2),SF,MF=E        START A READ TO IT
         L     R2,DECBLINK         PNT TO NEXT DECB
         C     R2,BUFSSTAR         ALL READS STARTED YET?
         BNE   PRIMELIB            NO, LOOP FOR NEXT
         SPACE 1
* CREATE AND PUNCH THE ./ADD CARD (IF DESIRED)
         TM    FLAG,UPDTE+UPDAT    ./ADD WANTED?
         BZ    CHECKLIB            NO, SKIP
         MVC   ADDCARD+7(L'ADDCARD-7),ADDCARD+6 CLEAR OLD OPNDS  GP@FT*
         TM    FLAG,UPDAT          IEBUPDAT FORMAT?                   *
         BO    ADDUPDAT            YES                                *
         MVC   ADDCARD+7(5),=C'NAME=' NO, IEBUPDTE FORMAT             *
         MVC   ADDCARD+12(L'MEMNAME),MEMNAME  GET THE MEMBER NAME     *
         NI    MEMC,X'7F'          TURN OFF ALIAS BIT                 *
         CLI   MEMC,2              2 HALFWORDS OF USERDATA?           *
         BE    ADDSSI              YES, SUPPLY SSI INFORMATION        *
         CLI   MEMC,20             20 HALFWORDS OF USERDATA?      A0909
         BNE   ORIGSTAT            NO, CAN'T BE EXTENDED STATS    A0909
         TM    SPFFLAGS,SPFXSTAT   EXTENDED STATS IN 20 H/W?      A0909
         BNO   PUTADD              NO, CAN'T BE ISPF STATISTICS   A0909
*        CLI   SPFBLANK,C' '       USERID CAN BE 8 BYTES NO BLANK SG01
*        BNE   PUTADD              YES. NEW TYPE OF ISPF STATS    SG01
         B     BOTHSTAT            GO PERFORM COMMON CHECKING     A0909
ORIGSTAT EQU   *                   CHECK FOR ORIGINAL ISPF STATS  A0909
         CLI   MEMC,15             15 HALFWORDS OF USERDATA?          *
         BNE   PUTADD              NO, CAN'T BE ISPF STATISTICS       *
         TM    SPFFLAGS,SPFXSTAT   CLASSIC STATS IN 15 H/W?       A0909
         BO    PUTADD              NO, CAN'T BE ISPF STATISTICS   A0909
         CLC   SPFBLANK,ADDCARD+90 USERID UNDER 7 BYTES LONG?     M0909
         BNE   PUTADD              NO, CAN'T BE ISPF STATISTICS   M0909
BOTHSTAT CLI   SPFVM,100           IS VERSION LESS THAN 100?          *
         BNL   PUTADD              NO, CAN'T BE ISPF STATISTICS       *
         CLI   SPFVM+1,100         IS LEVEL LESS THAN 100?            *
         BNL   PUTADD              NO, CAN'T BE ISPF STATISTICS       *
*  NOW CHECK FOR BAD SECONDS INHERITED FROM RESIDUAL DATA         A0909
*  FROM AN SSI VALUE FOR A MEMBER PREVIOUSLY LOADED BY            A0909
*  A PRE-0909 VERSION OF PDSLOAD IN THE SAME PDSLOAD RUN          A0909
*  WHICH RECONSTRUCTED THE STATS FOR THIS MEMBER...               A0909
         CLI   SPFSECS,X'60'       IS SECONDS LESS THAN 60?       A0909
         BL    *+8                 YES, GOOD                      A0909
         MVI   SPFSECS,0           NO, CLEAR IT                   A0909
         MVC   WORKAREA(1),SPFSECS COPY THE SECONDS BYTE          A0909
         NI    WORKAREA,X'0F'      GET THE SECOND DIGIT ONLY      A0909
         CLI   WORKAREA,9          DECIMAL DIGIT?                 A0909
         BNH   *+8                 YES, GOOD                      A0909
         MVI   SPFSECS,0           NO, CLEAR IT                   A0909
*  ... SO NOW NO VERSION OF OFFLOAD WILL ISSUE BAD SECONDS        A0909
*  IN THE ./ ADD STATEMENT COMMENT AREA SO THE 0909 PDSLOAD       A0909
*  CAN SAFELY RIGOROUSLY CHECK THE SECONDS VALUE.                 A0909
         CLI   SPFCREDT+2,X'36'    JULIAN DAY SMALL ENOUGH?       A0909
         BH    PUTADD              NO, CAN'T BE ISPF STATISTICS   A0909
         TM    SPFCREDT+3,X'0F'    EXPECTED DATE DECIMAL SIGN?        *
         BNO   PUTADD              NO, CAN'T BE ISPF STATISTICS       *
         CLI   SPFCHGDT+2,X'36'    JULIAN DAY SMALL ENOUGH?       A0909
         BH    PUTADD              NO, CAN'T BE ISPF STATISTICS   A0909
         TM    SPFCHGDT+3,X'0F'    EXPECTED DATE DECIMAL SIGN?        *
         BNO   PUTADD              NO, CAN'T BE ISPF STATISTICS       *
         SLR   R0,R0                                                  *
         IC    R0,SPFVM                    MODIFICATION VERSION       *
         CVD   R0,WORKAREA                                            *
         OI    WORKAREA+7,X'0F'                                       *
         UNPK  ADDCARD+21(2),WORKAREA+6(2)                            *
         IC    R0,SPFVM+1                  MODIFICATION LEVEL         *
         CVD   R0,WORKAREA                                            *
         OI    WORKAREA+7,X'0F'                                       *
         UNPK  ADDCARD+23(2),WORKAREA+6(2)                            *
         MVI   ADDCARD+25,C'-'                                        *
         UNPK  ADDCARD+26(5),SPFCREDT+1(3)     CREATION DATE          *
         MVI   ADDCARD+31,C'-'                                        *
         UNPK  ADDCARD+32(5),SPFCHGDT+1(3) MODIFICATION DATE          *
         MVI   ADDCARD+37,C'-'                                        *
         UNPK  ADDCARD+38(5),SPFHHMM(3)    MODIFICATION TIME (HHMM)   *
         MVI   ADDCARD+42,C'-'                                        *
         ICM   R0,3,SPFCCNT                     CURRENT SIZE          *
         CVD   R0,WORKAREA                                            *
         OI    WORKAREA+7,X'0F'                                       *
         UNPK  ADDCARD+43(5),WORKAREA+5(3)                            *
         MVI   ADDCARD+48,C'-'                                        *
         ICM   R0,3,SPFICNT                     INITIAL SIZE          *
         CVD   R0,WORKAREA                                            *
         OI    WORKAREA+7,X'0F'                                       *
         UNPK  ADDCARD+49(5),WORKAREA+5(3)                            *
         MVI   ADDCARD+54,C'-'                                        *
         ICM   R0,3,SPFMOD                     MODIFIED COUNT         *
         CVD   R0,WORKAREA                                            *
         OI    WORKAREA+7,X'0F'                                       *
         UNPK  ADDCARD+55(5),WORKAREA+5(3)                            *
         MVI   ADDCARD+60,C'-'                                        *
         MVC   ADDCARD+61(8),SPFUSER       8 BYTES, NOT 7         SG01*
         UNPK  ADDCARD+69(3),SPFSECS(2)    MODIFICATION TIME (SS)     *
         MVI   ADDCARD+71,C' '                                        *
         TM    SPFFLAGS,SPFXSTAT   EXTENDED ISPF STATS?           A0909
         BNO   PUTADD              NO, ALL DONE                   A0909
         ICM   R0,15,SPFXCCNT                   CURRENT SIZE      A0909
         CVD   R0,WORKAREA                                        A0909
         OI    WORKAREA+7,X'0F'                                   A0909
         UNPK  UNPKAREA,WORKAREA                                  A0909
         MVC   ADDCARD+43(5),UNPKAREA+3                           A0909
         MVC   ADDCARD+72(3),UNPKAREA                             A0909
         ICM   R0,15,SPFXICNT                   INITIAL SIZE      A0909
         CVD   R0,WORKAREA                                        A0909
         OI    WORKAREA+7,X'0F'                                   A0909
         UNPK  UNPKAREA,WORKAREA                                  A0909
         MVC   ADDCARD+49(5),UNPKAREA+3                           A0909
         MVC   ADDCARD+75(3),UNPKAREA                             A0909
         ICM   R0,15,SPFXMOD                   MODIFIED COUNT     A0909
         CVD   R0,WORKAREA                                        A0909
         OI    WORKAREA+7,X'0F'                                   A0909
         UNPK  UNPKAREA,WORKAREA                                  A0909
         MVC   ADDCARD+55(5),UNPKAREA+3                           A0909
         MVC   ADDCARD+78(2),UNPKAREA+1                           A0909
         B     PUTADD                                                 *
ADDSSI   LA    R1,ADDCARD+19       POINT TO LAST POSSIBLE MEM NAME CHR*
SSILOOP  CLI   0(R1),C' '          TRAILING BLANK?                    *
         BNE   GETSSI              NO                                 *
         BCT   R1,SSILOOP          YES                                *
GETSSI   MVC   1(5,R1),=C',SSI='                                      *
         UNPK  6(9,R1),12(5,R7)    FETCH AND EXPAND THE SSI           *
         TR    6(8,R1),HEXCHARS-C'0'                                  *
         MVI   14(R1),C' '         TIDY UP HEX DIGITS                 *
         B     PUTADD                                                 *
ADDUPDAT MVC   ADDCARD+15(L'MEMNAME),MEMNAME    IEBUPDAT FORMAT  GP@FT*
         LA    R1,ADDCARD+15       SETUP FOR BLANK SCAN
ADDBLNK  LA    R1,1(,R1)           PNT TO NEXT COLUMN
         CLI   0(R1),C' '          BLANK YET?
         BNE   ADDBLNK             NO, LOOP
         MVC   0(7,R1),=C',00,0,0' YES, GET REST OF UPDAT OPNDS
PUTADD   PUT   OUT,ADDCARD         PUNCH IT
         SPACE 1
* CHECK THE NEXT INPUT BLOCK AND SETUP TO SCAN IT FOR LOGICAL RECORDS
CHECKLIB NI    FLAG2,255-IOERROR   CLEAR I/O ERROR FLAG
         CHECK (R2)                WAIT FOR A READ TO COMPLETE
         L     R3,DECBBUFR         POINT TO 1ST RECORD IN THE BUFFER
         LH    R4,INBUFL           GET BUFFER LENGTH
         LH    R4,INBLKSI          GET BLOCK SIZE                GP@P6
         AR    R4,R3               PNT TO END OF BUFFER
         L     R1,DECBIOB          PNT TO RELATED IOB
         SH    R4,14(,R1)          PNT TO END OF BLOCK READ
         SPACE 1
* SCAN THE INPUT BLOCK AND HANDLE EOB AND I/O ERROR CONDITIONS
CARDLOOP CR    R3,R4               END OF BLOCK YET?
*-       BL    PUTCARD             NO, GO PUT NEXT CARD        -.UPLOG.
         BL    UPLCOUNT            NO, GO PUT NEXT CARD         .UPLOG.
         TM    FLAG2,IOERROR       YES, HAS AN I/O ERR OCCURED?
         BZ    READLIB             NO, SKIP
         #PUT  IOERPFIX            YES, PRINT THE ERROR MSG
READLIB  READ  (R2),SF,MF=E        READ FUTURE BLOCK
         L     R2,DECBLINK         LINK TO NEXT DECB
         B     CHECKLIB            GO CHECK ITS I/O
         SPACE 1
* WRITE TO SYSUPLOG IF CARD ALREADY BEGINS WITH '><'            .UPLOG.
UPLCOUNT CLI   UPLSTAT+3,0         IS SYSUPLOG OPEN             ,UPLOG.
         BE    UPLNO               NOT OPEN, SKIP THIS          ,UPLOG.
         TM    FLAG,UPDAT+UPDTE    WAS T=IEBUPDTE SPECIFIED     .UPLOG.
         BZ    UPLNO               NO, SKIP THIS                ,UPLOG.
         LA    R15,1                                            ,UPLOG.
         A     R15,UPLNUM          COUNT RECORDS                ,UPLOG.
         ST    R15,UPLNUM                                       ,UPLOG.
         CLC   0(2,R3),=C'><'      IF IT DOESN'T START '><'     ,UPLOG.
         BNE   UPLNO               DON'T WRITE TO SYSUPLOG      ,UPLOG.
         LA    R14,1                                            ,UPLOG.
         A     R14,UPLCTR          COUNT UPLOG RECORDS          ,UPLOG.
         ST    R14,UPLCTR                                       ,UPLOG.
         MVC   UPLCARD(8),MEMNAME                               ,UPLOG.
         CVD   R15,WORKAREA        RECORD NUMBER WITHIN MEMBER  ,UPLOG.
         OI    WORKAREA+7,X'0F'                                 ,UPLOG.
         UNPK  UPLCARD+9(7),WORKAREA+4(4)                       ,UPLOG.
         MVC   UPLCARD+17(62),0(R3) ADD 1-62 AS A COMMENT       .UPLOG.
         TR    UPLCARD+17(62),DOTCHARS                          .UPLOG.
         PUT   UPL,UPLCARD                                      ,UPLOG.
UPLNO    EQU   *                                                ,UPLOG.
* TEST FOR IEBUPDTE CONTROL CARDS FROM THE INPUT DATA SET
PUTCARD  CLC   0(2,R3),ADDCARD     IEBUPDTE CTL CARD?
         BNE   PUTITOUT            NO, AOK
         TM    FLAG,UPDAT+UPDTE    YES, ./ADD'S BEING INSERTED?
         BZ    PUTITOUT            NO, AOK
         TM    FLAG,CTLCDERR       YES, ERROR BEFORE FROM SAME MEMBER?
         BO    PRTERRCD            YES, JUST PRINT THE CARD
         OI    FLAG,CTLCDERR       NO, SET THE FLAG
         #PUT  CTLERR1             PRINT ERROR EXPLAINATION
         #PUT  CTLERR2             MORE EXPLAINATION
         CLI   RCD+1,4             COMPLETION CODE ALREADY SET?
         BNL   PRTERRCD            YES, SKIP
         MVI   RCD+1,4             NO, SET IT
PRTERRCD LH    R1,INLRECL          GET THE INPUT RECORD LENGTH   GP@FT
         BCTR  R1,0                DECREMENT FOR EXECUTE         GP@FT
         EX    R1,LOADCRD2         COPY ERROR CARD TO BUFFER
         #PUT  ERRCARD             PRINT IT
         MVC   0(2,R3),=C'><'      KILL THE './'
         SPACE 1
* PUNCH THE NEXT CARD AND LOOP FOR THE FOLLOWING CARD
PUTITOUT LH    R1,INLRECL          GET THE INPUT RECORD LENGTH   GP@FT
         BCTR  R1,0                DECREMENT FOR EXECUTE         GP@FT
         EX    R1,LOADOCRD         MOVE RECORD TO STAGING AREA   GP@FT
         PUT   OUT,OUTCARD         PUT THE NEXT CARD             GP@FT
         AH    R3,INLRECL          PNT TO FOLLOWING CARD
         B     CARDLOOP            LOOP TO PROCESS
LOADCRD2 MVC   CARD2(0),0(R3)      <<< EXECUTED >>>              GP@FT
LOADOCRD MVC   OUTCARD(0),0(R3)    <<< EXECUTED >>>              GP@FT
         DROP  R2                  RELEASE DECB BASE
         SPACE 1
MEMBEREO DS    0H  (NEW EODAD)                                    .SBG.
** -- >>  below     Exception Report Line Printed                 .SBG.
EXCEPCNT DS    0H                                                 .SBG.
         L     R15,UPLCTR                                         .SBG.
         LTR   R15,R15                                            .SBG.
         BZ    EXCEPEND                                           .SBG.
         MVI   LINE-1,X'40'                                       .SBG.
         MVC   LINE,LINE-1                                        .SBG.
         MVC   LINE+3(18),=C'Exception Report: '                  .SBG.
         MVC   LINE+36(26),=C'SYSUPLOG Records Processed'         .SBG.
         CVD   R15,DOUBLE                                         .SBG.
         MVC   LINE+20(12),=X'402020206B2020206B202120'           .SBG.
         ED    LINE+20(12),DOUBLE+3                               .SBG.
         #PUT  LINE                                               .SBG.
EXCEPEND B     MEMBERLP (OLD EODAD)                               .SBG.
** -- >>  above     Exception Report Line Printed                 .SBG.
         SPACE 1
* CURRENT DATA SET FINISHED - CLOSE IT AND FREE ITS BUFFERS
DSDONE   CLOSE (IN,,DRCTY)         CLOSE THE INPUT DATA SET
         LM    R0,R1,BUFSSIZE      GET IN BUFS LEN AND ADDR
         FREEMAIN R,LV=(0),A=(1)   RELEASE THE BUFFERS
         LA    R10,L'DCBDDNAM(,R10) PNT TO NEXT INPUT DDNAME
         C     R10,ENDDDNS         END OF NAMES?
         BL    DATSETLP            NO, LOOP TO PROCESS THIS DS
         SPACE 1
* CURRENT OFFLOADING FUNCTION FINISHED - FOR SELECT FUNCTIONS DETERMINE
* IF ALL GIVEN NAMES WERE SELECTED
         TM    FLAG,SELECT         SELECT FUNCTION?
         BZ    WARLSE              NO, SKIP
         L     R2,ENDDDNS          YES, PNT TO START OF MBR MNES
NOTSELCT C     R2,ENDMNMS          END OF GIVEN NAMES YET?
         BNL   WARLSE              YES, DONE
         OC    0(L'DCBDDNAM,R2),0(R2) NO, THIS NME USED?
         BZ    SELECTD             YES, CONTINUE
         MVC   NTSELMBR,0(R2)      NO, PUT NME INTO MSG
         #PUT  NTSELMSG            TELL USER
         CLI   RCD+1,4             TEST RETURN CODE
         BNL   SELECTD             ALREADY SET
         MVI   RCD+1,4             NOT SET; SET IT
SELECTD  LA    R2,L'DCBDDNAM(,R2)  PNT TO NEXT GIVEN NAME
         B     NOTSELCT            LOOP TO TEST IT
         SPACE 1
* FREE THE WORK AREA AND LOOP FOR MORE CONTROL CARDS
WARLSE   LM    R1,R2,WORKSTAR      YES, GET WA START & END
         SR    R2,R1               GET WORK AREA LENGTH
         FREEMAIN R,LV=(R2),A=(1)  RELEASE THE WORKAREA
         B     CONTROL             GO READ SOME MORE CTL CARDS
         TITLE 'OFFLOAD -- END OF JOB PROCESSING'
EXIT16   MVI   RCD+1,16            SET ERROR COMPLETION CODE
         SPACE 1
* WRITE THE EOP MSG IF SYSPRINT IS OPEN
EOP      TM    PRTOFLGS,DCBOFOPN   SYSPRINT OPEN?
         BZ    RETURN              NO, SKIP
         #PUT  BLNK3               YES, PRINT SOME BLANK LINES
         LA    R1,EOPM-1           ASSUME RELATIVELY GOOD ENDING
         CLI   RCD+1,8             CORRECT?
         BNH   PUTEND              YES, GO PRINT MSG
         LA    R1,PGMTERM-1        NO, PNT TO DIFFERENT MSG
PUTEND   #PUT  (R1)                PRINT THE END OF PROGRAM MSG
         SPACE 1
* CLOSE ALL DATA SETS
         CLI   UPLSTAT+3,0         IF SYSUPLOG IS NOT OPEN      ,UPLOG.
         BE    UPLCLOS             THEN SKIP CLOSE              ,UPLOG.
         LA    R2,UPL              POINT TO THE UPLOG DATA SET  ,UPLOG.
         BAL   R14,CLOSE           CLOSE IT                     ,UPLOG.
UPLCLOS  EQU   *                                                ,UPLOG.
         LA    R2,OUT              POINT TO THE OUTPUT DATA SET
         BAL   R14,CLOSE           CLOSE IT
         LA    R2,SYSPRINT         POINT TO THE SYSPRINT DATA SET
         BAL   R14,CLOSE           CLOSE IT
         SPACE 1
* RETURN TO CALLER
RETURN   LH    R15,RCD             GET THE COMPLETION CODE
         #EXIT ((R14,R12)),RC=(R15) RETURN TO CALLER
         TITLE 'OFFLOAD -- INPUT CONTROL CARD ERROR HANDLING ROUTINE'
CARDNCAP MVC   PRECARD+L'PRECARD-24(23),=C'INCOMPATABLE FUNCTION -'
         B     CARDPUT             GO PRINT THE CARD ERROR
         SPACE 1
CARDSYNT TM    FLAG,UNLOAD         SHOULD THE CARD HAVE BEEN IGNORED?
         BZ    CARDIGNR            YES, GO SAY SO
         MVC   PRECARD+L'PRECARD-15(14),=C'SYNTAX ERROR -' NO,
         B     CARDPUT             GO PRINT THE CARD ERROR
         SPACE 1
CARDIGNR MVC   PRECARD+L'PRECARD-15(14),=C'CARD IGNORED -'
         B     CARDPUT2            GO PRINT THE ERROR CARD
         SPACE 1
CARDMISO MVC   PRECARD+L'PRECARD-28(27),=C'REQUIRED OPERANDS MISSING -'
         B     CARDPUT2            GO PRINT THE ERROR CARD
         SPACE 1
CARDCORE MVC   PRECARD+L'PRECARD-17(16),=C'CORE EXHAUSTED -'
         B     GETSCAN             GO GET ERROR POINTER
         SPACE 1
CARDRDUN MVC   PRECARD+L'PRECARD-21(20),=C'REDUNDANT OPERANDS -'
         B     GETSCAN             GO GET ERROR POINTER
         SPACE 1
CARDBDDN MVC   PRECARD+L'PRECARD-18(17),=C'MISSING DD CARD -'
GETSCAN  L     R3,SAVESCAN         RESET THE ERROR POINTER
         SPACE 1
CARDPUT  #PUT  PRECARD             PRINT THE CARD ERROR
         LA    R2,OUT              POINT TO THE OUTPUT DATA SET
         BAL   R14,CLOSE           ENSURE THAT IT IS CLOSED
         MVI   OUTDDNAM,C' '       FLAG "O=" OPERAND REQUIRED
         MVI   PRECARD,C' '        CLEAR THE ERROR CARD
         MVC   PRECARD+1(L'PRECARD+L'CARD-1),PRECARD CLR ERR CRD
         MVI   0(R3),C'*'          UNDERLINE THE ERROR
CARDPUT2 #PUT  PRECARD             UNDERLINE THE ERROR
RCD8     MVI   RCD+1,8             SET THE COMPLETION CODE
         TM    RDROFLGS,DCBOFOPN   SYSIN STILL OPEN?
         BZ    WARLSE              NO, GO FORCE PGM TERMINATION
         B     RESTART             LOOP FOR NEXT CONTROL STATEMENT
         TITLE 'OFFLOAD -- CARDEXIT - OPEN DCB EXIT FOR OUT, IN, AND SY*
               SIN'
* FORCE THE BLKSIZE AND BUFL TO A (LOWER) MULTIPLE OF THE LRECL
         USING DCB,R1              DECLARE THE DCB BASE
CARDEXIT TM    DCBMACF2,DCBMRPUT   OUTPUT QSAM FILE?             GP@FT*
         BZ    XLRECLOK            NO, GET LRECL FROM DATA SET LABELS *
         CLC   DCBLRECL,=H'80'     IS THE LRECL AT LEAST 80 ?         *
         BNL   XLRECLOK            YES, OUTPUT LRECL IS GOOD          *
         MVC   DCBLRECL,=H'80'     NO, SET IT TO 80                   *
         MVC   DCBBLKSI,=H'3120'   SET BLKSIZE TO 3120           GP@FT*
XLRECLOK LH    R3,DCBBLKSI         GET THE BLOCK SIZE (IF ANY)
         LH    R4,DCBLRECL         GET THE LRECL
         SR    R2,R2               CLEAR FOR DIVIDE
         DR    R2,R4               GET THE BLOCKING FACTOR
         MR    R2,R4               ENSURE BLKSI = N*LRECL
         CR    R3,R4               IS FILE BLOCKED?
         BH    GOTBLKSI            YES, SKIP
         LR    R3,R4               NO, ENSURE BLKSI = LRECL
GOTBLKSI STH   R3,DCBBLKSI         SET THE BLOCK SIZE
         TM    DCBMACF2,DCBMRPUT   OUTPUT QSAM FILE?             GP@P6*
         BZ    *+8                 NO, AVOID S013-4C IF PDS UNBLOCKED *
         STH   R3,DCBBUFL          SET THE BUFL (ASSUME QS)
         SPACE 1
* IF AN NCP (BPAM) OR BUFNO (QSAM OR BPAM) ARE GIVEN VIA JCL OR DATA
* SET LABEL, THEN USE IT. FOR BPAM PREFER A GIVEN NCP OVER A GIVEN
* BUFNO. ALSO FOR BPAM INHIBIT AUTOMATIC BUFFER ALLOCATION.
         LA    R6,DCBBUFNO         POINT TO BUFNO FIELD
         TM    DCBMACF1,DCBMRRD    BSAM/BPAM DS?
         BZ    NOTBASIC            NO, DSORG=PS; SKIP
         LA    R6,DCBNCP           YES, POINT TO NCP (NOT BUFNO)
         CLI   DCBNCP,0            NCP GIVEN?
         BNE   GOTNCP              YES, USE IT
         MVC   DCBNCP,DCBBUFNO     NO, USE BUFNO (IF ANY)
GOTNCP   MVI   DCBBUFNO,0          ENSURE NOW BUFFERS GOTTEN
NOTBASIC CLI   0(R6),0             BUFNO/NCP GIVEN?
         BNE   BUFSGEN             YES, SKIP
         SPACE 1
* BUFNO AND NCP NOT GIVEN - CALCULATE A REASONABLE VALUE AND USE IT.
         LA    R5,BUFSPACE         NO, GET REASONABLE BUFFER SPACE
         SR    R4,R4               CLEAR FRO DEVICE
         DR    R4,R3               GET REASONABLE BUFNO/NCP
         STC   R5,0(,R6)           STORE REASONABLE BUFNO/NCP
         LA    R5,10               GET MAX REASONABLE BUFNO/NCP
         CLI   0(R6),10            MAX EXCEEDED?
         BH    BUFNRSET            YES, GO RESET
         LA    R5,3                NO, GET MIN REASONABLE BUFNO/NCP
         CLI   0(R6),3             MIN EXCEEDED?
         BNL   BUFSGEN             NO, CONTINUE
BUFNRSET STC   R5,0(,R6)           YES, RESET TO MAX/MIN REASONABLE
         SPACE 1
* FOR BPAM CREATE A SPECIAL POOL OF BUFFERS AND DECB'S
BUFSGEN  TM    DCBMACF1,DCBMRRD    BSAM/BPAM DS?
         BCR   8,R14               NO, RETURN TO OPEN
         L     R2,DCBDEBAD         YES, PNT TO OPEN'S WORK AREA
         TM    JFCDSRG1-OPENWORK(R2),JFCORGPO DS ACTUALLY PDS?
         BO    ACTULPDS            YES, SKIP
         OI    FLAG,NOTPDS         NO, REMEMBER THIS
ACTULPDS ST    R14,SAVER14A        SAVE RETURN ADDRESS
         SR    R2,R2               CLEAR FOR 'IC'
         IC    R2,0(,R6)           GET NUMBER OF BUFFERS TO BUILD
         LA    R5,DECBLEN          GET DECB LENGTH
         LA    R1,0(R5,R3)         GET LENGTH OF DECB+BUFFER
         DROP  R1                  RELEASE DCB BASE
         MR    R0,R2               GET TOTAL CORE REQUIRED
         ST    R1,BUFSSIZE         SAVE LENGTH FOR LATER FREEMAIN
         GETMAIN R,LV=(R1)         GET CORE FOR BUFFERS
         ST    R1,BUFSSTAR         SAVE START FOR LATER
         MR    R4,R2               GET TOTAL LENGTH OF DECB'S
         AR    R5,R1               PNT TO 1ST BUFFER
DECBINIT LR    R4,R1               PNT TO NEXT DECB
         USING DECB,R4             DECLARE DECB BASE
         MVC   0(DECBBUFR-DECB,R4),DECBMODL GET SKELETON DECB
         ST    R5,DECBBUFR         SET BUFFER POINTER
         AR    R5,R3               ADVANCE BUFFER PTR
         LA    R1,DECBLEN(,R4)     PNT TO NEXT DECB
         ST    R1,DECBLINK         LINK IT TO THIS ONE
         BCT   R2,DECBINIT         LOOP TO PROCESS NEXT DECB
         MVC   DECBLINK,BUFSSTAR   MAKE DECB LINKS CIRCULAR
         DROP  R4                  RELEASE DECB BASE
         L     R14,SAVER14A        RESTORE RETURN ADDRESS
         BR    R14                 RETURN TO OPEN
         TITLE 'OFFLOAD -- CLOSE - CLOSE A DATA SET AND FREE ITS BUFFER*
               S'
         USING DCB,R2              DECLARE DCB BASE
CLOSE    TM    DCBOFLGS,DCBOFOPN   IS THE DATA SET OPEN?
         BCR   8,R14               NO, RETURN TO CALLER
         ST    R14,SAVER14A        YES, SAVE THE RETURN ADDRESS
         SPACE 1
* CLOSE THE DATA SET
         CLOSE ((R2))              CLOSE THE DATA SET
         SPACE 1
* FREE ITS BUFFERS
         FREEPOOL (R2)             FREE ITS BUFFERS
         L     R14,SAVER14A        RESTORE THE RETURN ADDRESS
         BR    R14                 RETURN TO CALLER
         DROP  R2                  RELEASE THE DCB BASE
         TITLE 'OFFLOAD -- GETNAME - SCAN OUT A NAME FROM AN INPUT CONT*
               ROL STATEMENT'
GETNAME  STM   R14,R1,SAVER14B     SAVE WORK REGISTERS
         LA    R1,1(,R3)           POINT TO NAME TO BE SCANNED
         ST    R1,SAVESCAN         SAVE ICO ERROR
         MVI   WORKAREA,C' '       CLEAR THE -
         MVC   WORKAREA+1(L'DCBDDNAM-1),WORKAREA  WORK AREA
         LA    R15,WORKAREA-1      GET OUTPUT SCANNER
         LR    R0,R4               GET BXLE INCREMENT ("1")
         LA    R1,WORKAREA+L'DCBDDNAM-1 GET BXLE LIMIT
         LA    R14,VALID+L'VALID-10 PNT 2 VALID 1ST CHARACTERS
         SPACE 1
* VALIDATE THE NAME'S SYNTAX
GETNLP1  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
GETNLP2  BCTR  R14,0               POINT TO NEXT VALID CHARACTER
         CLC   0(1,R3),0(R14)      CARD CHARACTER VALID?
         BL    GETNLP2             DON'T KNOW YET; LOOP
         BH    GETNTDLM            NO, GO SEE IF IT'S A DELIMITER
         CLI   0(R3),0             PROBABLE; IS IT X'00'?
         BE    CARDSYNT            YES, CHARACTER IS INVALID
         BXH   R15,R0,CARDSYNT     NO, CHAR VLD; ADV 2 NXT OPUT LOCATN
         MVC   0(1,R15),0(R3)      SAVE THIS CHARACTER
         LA    R14,VALID+L'VALID   PNT TO LIST OF VALID NOT-FIRST CHARS
         B     GETNLP1             LOOP TO TEST NEXT COLUMN
         SPACE 1
* VALIDATE THE DELIMITER FOLLOWING THE NAME
GETNTDLM CLI   0(R3),C','          COMMA DELIMITER?
         BE    GOTNAME             YES, GOT A NAME
         TM    FLAG2,PAREN         NO, WITHIN PARENS?
         BZ    GETNTBLK            NO, DLM MUST BE BLANK
         CLI   0(R3),C')'          YES, DLM = CLOSE PARENS?
         BNE   CARDSYNT            NO, SYNTAX ERROR
         BXH   R3,R4,CARDSYNT      YES, ADVANCE TO NEXT COLUMN
         NI    FLAG2,255-PAREN     CLEAR PARENS FLAG
         B     GETNTDLM            GO TEST NEXT DELIMITER
GETNTBLK CLI   0(R3),C' '          DLM BLANK?
         BNE   CARDSYNT            NO, ERROR
GOTNAME  CLI   WORKAREA,C' '       YES, NULL NAME?
         BE    CARDSYNT            YES, ERROR
         SPACE 1
* IF THE NAME IS A DDNAME, THEN VALIDATE IT AGAINST THE TIOT.
         TM    FLAG2,DDNAME        NO, OPND OBJECT A DDNAME?
         BZ    GOTNRET             NO, GO RETURN TO CALLER
         L     R1,CVTPTR           POINT TO THE CVT
         L     R1,CVTTCBP-CVT(,R1) PNT TO TCB NEW/OLD PTRS
         L     R1,4(,R1)           PNT TO OUR TCB
         L     R1,TCBTIO-TCB(,R1)  PNT TO TIOT
         LA    R1,TIOENTRY-TIOT(,R1) POINT TO 1ST DD ENTRY
         USING TIOENTRY,R1         DECLARE TIOT DD ENTRY BASE
         SR    R0,R0               CLEAR FOR "IC"
TIOTLP   AR    R1,R0               POINT TO NEXT TIOT DD ENTRY
         IC    R0,TIOELNGH         GET ITS LENGTH
         LTR   R0,R0               END OF TIOT?
         BZ    CARDBDDN            YES, MISSING DDNAME ERROR
         CLC   TIOEDDNM,WORKAREA   NO, RIGHT DDNAME?
         BNE   TIOTLP              NO, LOOP FOR NEXT DD ENTRY
GOTNRET  LM    R14,R1,SAVER14B     RESTORE ALL WORK REGISTERS
         BR    R14                 RETURN TO CALLER
         DROP  R1                  RELEASE THE TIOT DD ENTRY BASE
         TITLE 'OFFLIAD -- INERR - INPUT DATA SET SYNAD ROUTINE'
         USING DECB,R2             DECLARE THE INPUT DECB BASE
INERR    SYNADAF ACSMETH=BSAM      NO, GET SYNAD MSG
         L     R13,4(,R13)         RESTORE BASE REGISTER
         MVI   RCD+1,8             SET COMPLETION CODE
         OI    FLAG2,IOERROR       REMEMBER THE ERROR
         MVC   IOERTEXT,68(R1)     SAVE SYNAD MSG TEXT
         SPACE 1
* FORCE THE RESIDUAL COUNT TO A (HIGHER) MULTIPLE OF THE LRECL.
         L     R15,DECBIOB         PNT TO THE IOB
         LH    R1,14(,R15)         GET THE RESIDUAL COUNT
         LH    R15,INLRECL         GET THE LRECL
         AR    R1,R15              ROUND -
         BCTR  R1,0                 THE -
         SR    R0,R0                 RESIDUAL -
         DR    R0,R15                 COUNT -
         MR    R0,R15                  UP
         L     R15,DECBIOB         POINT TO THE IOB AGAIN
         STH   R1,14(,R15)         STORE NEW RESIDUAL COUNT
         SPACE 1
* ISSUE SYNADRLS TO RELEASE SYNAD'S SAVE AREA AND ERROR MESSAGE BUFFER
         L     R13,8(,R13)         RESTORE SYNAD'S SAVE AREA POINTER
         LNR   R15,R13             ENSURE R15 NEGATIVE
         SVC   68                  ISSUE SYNADRLS
         BR    R14                 RETURN TO CHECK ROUTINE
         DROP  R2                  RELEASE DECB ABSE
         TITLE 'OFFLOAD -- NEXTCARD - GET A CONTINUATION CARD'
NEXTCARD STM   R14,R1,SAVER14B     SAVE ALL WORK REGISTERS
         #PUT  PRECARD             ECHO OUT THE CURRENT CARD
         MVI   PRECARD,C' '        CLEAR ANY POSSIBLE CARRIAGE CONTROL
         OI    FLAG2,CONTINUE      FLAG CONTU ICO EOD
         GET   SYSIN,CARD          GET THE NEXT CARD
         SPACE 1
* RE-INITIALIZE THE CARD SCANNER
         LA    R3,CARD             RESET THE CARD SCANNER
         CLI   0(R3),C' '          MUST BE BLANK
         BNE   CARDSYNT            IT'S NOT; ERROR
OPNDLP2  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         CLI   0(R3),C' '          STILL BLANK?
         BE    OPNDLP2             YES, LOOP BACK
         BCTR  R3,0                NO, BACK OFF
         LM    R14,R1,SAVER14B     RESTORE ALL WORK REGISTERS
         BR    R14                 RETURN TO CALLER
         TITLE 'OFFLOAD -- OUTERR - OUTPUT DATA SET SYNAD ROUTINE'
OUTERR   SYNADAF ACSMETH=QSAM      GET SYNAD MSG
         L     R13,4(,R13)         RESTORE LOCAL BASE REGISTER
         MVI   RCD+1,12            SET PROGRAM COMPLETION CODE
         SPACE 1
* PRINT THE SYNAD MESSAGE
         MVC   67-L'IOERPFIX(L'IOERPFIX+1,R1),IOERPFIX-1 MSG PREFIX
         LA    R1,67-L'IOERPFIX(,R1) PNT TO ERR MSG
         #PUT  (R1)                PRINT IT
         SPACE 1
* ISSUE SYNADRLS TO RELEASE SYNAD'S SAVE AREA AND MESSAGE BUFFER.
         L     R13,8(,R13)         PNT BACK TO SYNAD SAVE AREA
         LNR   R15,R13             ENSURE R15 NEGATIVE
         SVC   68                  ISSUE SYNADRLS
         SPACE 1
* FORCE PROGRAM TERMINATION
         LA    R2,SYSIN            PNT TO THE READER DCB
         BAL   R14,CLOSE           CLOSE IT
         L     R10,ENDDDNS         PNT TO END OF DDNAMES LIST
         B     DSDONE              GO RETURN TO CALLER (EVENTUALLY)
         TITLE 'OFFLOAD -- PUTNAMES - SCAN NAMES FROM CONTROL STATEMENT*
                S AND PUT THEM INTO A LIST'
PUTNAMES STM   R14,R15,SAVER14A    SAVE THE RETURN ADDRESS
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
         SPACE 1
* CHECK FOR LEADING PARENS, THEN GET A NAME
         CLI   1(R3),C'('          OPEN PARENS?
         BNE   GNAME               NO, SKIP
         OI    FLAG2,PAREN         YES, REMEMBER IT
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN
GNAME    BAL   R14,GETNAME         GO SCAN OUT THIS NAME
         SPACE 1
* CHECK FOR REDUNDANT NAMES
         LR    R15,R0              POINT TO START OF LIST
RDUNTEST CR    R15,R1              LIST EXAUSTED YET?
         BNL   NEWNAME             YES, NEW NAME IS UNIQUE
         CLC   0(L'DCBDDNAM,R15),WORKAREA NO, NEW NAME UNIQUE?
         BE    CARDRDUN            NO, ERROR
         LA    R15,L'DCBDDNAM(,R15) MAYBE, ADVANCE THE LIST POINTER
         B     RDUNTEST            LOOP TO TEST AGAIN
         SPACE 1
* SAVE THE NEW NAME IN THE LIST (IF THERE IS ROOM FOR IT)
NEWNAME  LA    R14,L'DCBDDNAM(,R1) GET NEW END OF LIST
         C     R14,WORKEND         CORE EXHAUSTED?
         BH    CARDCORE            YES, ERROR
         MVC   0(L'DCBDDNAM,R1),WORKAREA NO, COPY NEW NAME INTO LIST
         LR    R1,R14              SET NEW END OF LIST PTR
         SPACE 1
* PROCESS TRAILING DELIMITERS AND POSSIBILY LOOP BACK FOR ANOTHER
* NAME
         CLI   0(R3),C','          COMMA DELIMITER?
         BNE   PUTNRET             NO, MUST B END OF STATEMENT (CC¬=0)
         TM    FLAG2,PAREN         YES, WITHIN PARENS?
         BZ    PUTNRET             NO, END OF OPERAND (CC=0)
         CLI   1(R3),C' '          YES, CONTINUE CARD NEEDED?
         BNE   GNAME               NO, GO SCAN OUT THE NEXT NAME
         BAL   R14,NEXTCARD        YES, GO GET THE NEXT CARD
         B     GNAME               GO SCAN OUT THE NEXT NAME
PUTNRET  LM    R14,R15,SAVER14A    RESTORE THE RETURN ADDRESS (CC SET)
         BR    R14                 RETURN TO CALLER
         TITLE 'OFFLOAD -- PUTSYSP - PRINT MESSAGES AND CONTROL PAGINAT*
               ION'
PUTSYSP  STM   R14,R1,SAVER14D     SAVE WORK REGISTERS
         MVC   SAVECCC,1(R1)       SAVE CARRIAGE CONTROL CHARACTER
         SPACE 1
* DETERMINE THE CARRIAGE CONTROL CHARACTER.
         LA    R15,CCC+L'CCC       POINT PAST VALID CC CHARACTERS
         LA    R0,L'CCC            GET LOOP CONTROL
CCCLP    BCTR  R15,0               POINT TO NEXT CCC
         CLC   1(1,R1),0(R15)      MATCH?
         BE    GOTCCC              YES, SKIP OUT
         BCT   R0,CCCLP            NO, LOOP FOR NEXT CCC
         SPACE 1
* UNRECOGNIZED CONTROL CHARACTER - SET NEW TITLE LINE AND FORCE PAGE
* EJECT ON NEXT CALL.
         MVC   TITLEBUF,TITLEBUF-1 NO MATCH; CLEAR TITLE BUFFER
         MVC   TITLEMVC+1(1),0(R1) SET MOVE INSTR LENGTH FIELD
TITLEMVC MVC   TITLEBUF(*-*),1(R1) SET NEW TITLE
         STH   R0,LINES2GO         FORCE PAGE EJEXT ON NEXT CALL
         B     PUTSRET             GO RETURN TO CALLER
         SPACE 1
* CARRIAGE CONTROL CHARACTER IDENTIFIED - INCREMENT LINE COUNT FOR THIS
* PAGE.
GOTCCC   BCTR  R0,0                GET # LINES REPRESENTED BY THE CCC
         AH    R0,LINES2GO         GET # LINES LEFT ON PAGE
         BNP   NOEJECT             PAGE NOT DONE YET
         SPACE 1
* PAGE FULL - EJECT TO NEW PAGE AND PRINT THE TITLE LINE WITH A PAGE
* COUNTER.
         AP    PAGECTR,=P'1'       PAGE DONE; INCR PAGE #
         MVC   PAGENO,EDITMASK     GET THE EDIT MASK
         ED    PAGENO-1(L'PAGENO+1),PAGECTR CNVRT TO EBCDIC
         MVI   PRTLRECL+1,121      SET LRECL
         PUT   SYSPRINT,TITLEBUF   PRINT THE TITLE -
         MVI   PRTLRECL+1,L'BLNK1   AND A -
         PUT   SYSPRINT,BLNK1        BLANK LINE
         L     R1,SAVER1D          RESTORE THE ORIGINAL MSG PTR
         MVI   1(R1),C' '          CHANGE TO BLANK CCC
         LH    R0,=Y(3-LINECT)     RESET LINES TO GO
NOEJECT  STH   R0,LINES2GO         STORE NEW LINES 2 GO
         SPACE 1
* PRINT THE MESSAGE AND RETURN TO CALLER.
         MVC   PRTLRECL+1(1),0(R1) SET LRECL
         LA    R0,1(,R1)           POINT TO THE MESSAGE
         PUT   SYSPRINT,(0)        PRINT THE MESSAGE
PUTSRET  LM    R14,R1,SAVER14D     RESTORE WORK REGISTERS
         MVC   1(1,R1),SAVECCC     RESTORE THE ORIGINAL CCC
         BR    R14                 RETURN TO CALLER
         TITLE 'OFFLOAD -- RDRERR - SYSIN DATA SET SYNAD ROUTINE'
RDRERR   ST    R14,SAVER14C        SAVE THE RETURN ADDRESS
         SYNADAF ACSMETH=QSAM      GET THE SYNAD MESSAGE
         LR    R15,R13             SAVE PTR TO SYNAD'S SAVE AREA
         L     R13,4(,R13)         PNT BACK TO OUR OWN SAVE AREA
         MVC   12(60,R15),12(R13)  SAVE ASVE AREA CONTENTS
         MVI   RCD+1,8             SET PGM COMPLETION CODE
         MVC   67-L'IOERPFIX(L'IOERPFIX+1,R1),IOERPFIX-1 MSG PREFIX
         LA    R1,67-L'IOERPFIX(,R1) PNT TO MSG TEXT
         #PUT  (R1)                PRINT SYNAD MSG
         LR    R15,R13             SAVE LOCAL SAVE AREA PTR
         L     R13,8(,R13)         PNT BACK TO SYNAD'S SAVE AREA
         MVC   12(60,R15),12(R13)  RESTORE LOCAL SAVE AREA CONTENTS
         LNR   R15,R13             ENSURE R15 NEGATIVE
         SVC   68                  ISSUE SYNADRLS
         L     R14,SAVER14C        RESTORE RETURN ADDRESS
         BR    R14                 RETURN TO QSAM GET
         TITLE 'OFFLOAD -- DATA - DATA CONTROL BLOCKS'
         PRINT GEN
         SPACE 3
SYSIN    DCB   DDNAME=SYSIN,DSORG=PS,MACRF=GM,RECFM=FB,LRECL=80,       *
               OPTCD=C,EODAD=RDREOD,EXLST=CARDXLST,SYNAD=RDRERR,       *
               EROPT=ACC
         ORG   SYSIN
RDR      #DCBD DSORG=QS
         ORG   ,
         SPACE 3
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=PM,RECFM=UA,LRECL=121,   *
               BLKSIZE=121,OPTCD=C,BUFNO=3,EROPT=ACC
         ORG   SYSPRINT
PRT      #DCBD DSORG=QS
         ORG   ,
         SPACE 3
OUT      DCB   DSORG=PS,MACRF=PM,RECFM=FB,OPTCD=C,               GP@FT *
               EXLST=CARDXLST,SYNAD=OUTERR
         ORG   OUT
OUT      #DCBD DSORG=QS
         ORG   ,
         SPACE 3
IN       DCB   DSORG=PS,MACRF=RP,RECFM=FB,OPTCD=C,               GP@FT *
               EODAD=MEMBEREO,EXLST=CARDXLST,SYNAD=INERR
         ORG   IN
IN       #DCBD DSORG=BS
         ORG   ,
         SPACE 3
DRCTY    DCB   DSORG=PS,MACRF=GM,RECFM=F,LRECL=256,BLKSIZE=256,OPTCD=C,*
               BUFNO=5,EODAD=DRCERR,SYNAD=DRCERR
         ORG   DRCTY
DRC      #DCBD DSORG=QS
         ORG   ,
         SPACE 3
UPL      DCB   DDNAME=SYSUPLOG,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80
*              BLKSIZE MUST BE IN JCL, VTOC, OR SMS             ,UPLOG.
         ORG   UPL                                              ,UPLOG.
UPL      #DCBD DSORG=QS                                         ,UPLOG.
         ORG   ,
         SPACE 3
         PRINT GEN
         TITLE 'OFFLOAD -- DATA - MODEL READ DECB'
         READ  DECBMODL,SF,IN,*-*,MF=L
         ORG   DECBMODL+(DECBBUFR-DECB) SAVE SOME SPACE
         TITLE 'OFFLOAD -- DATA - MISCELLANIOUS'
WORKAREA DS    D                   WORK AREA
DOUBLE   DS    D                   CVD WORK AREA                  .SBG.
UNPKAREA DS    D                   UNPACK AREA                    A0909
RDAREA   DS    XL256               DIRECTORY BLOCK LOCAL BUFFER
         SPACE 3
GETMQTY  DC    A(OSCORE+2048,X'FFFFF8') VARIABLE GETMAIN REQUIREMENTS
         SPACE 1
WORKSTAR DS    A                   GOTTEN CORE START
WORKEND  DS    A                   GOTTEN CORE END
         SPACE 1
BUFSSIZE DS    A                   INPUT BUFFER 'POOL' SIZE
BUFSSTAR DS    A                   INPUT BUFFER 'POOL' START
         SPACE 1
ENDDDNS  DS    A                   END OF INPUT DDNAMES LIST
ENDMNMS  DS    A                   END OF MEMBER NAMES LIST (USUALLY)
         SPACE 1
SAVER14A DS    2A                  R14 - R15 SAVE AREA
SAVER14B DS    4A                  R14 - R1 SAVE AREA
SAVER14C DS    A                   R14 SAVE AREA
SAVER14D DS    3A                  R14 - R0 SAVE AREA
SAVER1D  DS    A                   R1 SAVE AREA
SAVESCAN DS    A                   CONTROL CARD SCANNER SAVE AREA
         SPACE 1
CARDXLST DC    0A(0),X'85',AL3(CARDEXIT) SYSIN, IN, OUT DCB EXIT LIST
DRCTYEND DS    A                   END OF CURRENT DRCTY BLK
OFFLPTR  DS    A                   LOCATION IN MSG TXT 4 IN DDN
         SPACE 3
BLDLPARM DS    0Y                  SPECIAL BLDL PARAMETER LIST
BLDENTCT DC    Y(0)                ENTRY COUNT (THE MAGIC NUMBER)
BLDENTLN DC    Y(12)               ENTRY LENGTH (PROBABLY IGNORED)
BLDLKEY  DC    XL8'00'             COMPARE KEY (MUST BE ZERO)
BLDLTTR0 DC    Y(*-*),AL1(*-*,0)   TTR0 OF DESIRED BLOCK
         SPACE 1
SEQFAKRY DC    Y(0),AL1(1,0)       FAKE TTR0
EFFS     DC    (L'DCBDDNAM)X'FF',Y(0),2AL1(0) FAKE TERMINATER
SEQFAKLN EQU   *-SEQFAKRY          LENGTH OF FAKES
         SPACE 1
RCD      DC    Y(0)                PROGRAM COMPLETION CODE
LINES2GO DC    Y(0)                PRINT PAGE LINES 2 GO
         SPACE 3
FLAG     DS    X                   FUNCTION FLAG BYTE
UNLOAD   EQU   X'80'               UNLOAD CONTROL CARD ENCOUNTERED
SELECT   EQU   X'40'               SELECT CONTROL CARD ENCOUNTERED
EXCLUDE  EQU   X'20'               EXCLUDE CONTROL CARD ENCOUNTERED
UPDAT    EQU   X'10'               OUTPUT 4 IEBUPDAT (NOT IEBUPDTE)
NOTPDS   EQU   X'08'               INPUT DS IS NOT PARTITIONED
CTLCDERR EQU   X'04'               IEBUPDTE CTL CARD FROM PDS
UPDTE    EQU   X'02'               OUTPUT 4 IEBUPDTE (NOT IEBUPDAT)
         SPACE 1
FLAG2    DS    X                   SYNTAX FLAG BYTE
GOTIN    EQU   X'80'               "I=" OPERAND ENCOUNTERED
GOTOUT   EQU   X'40'               "O=" OPERAND ENCOUNTERED
GOTTYPE  EQU   X'20'               "T=" OPERAND ENCOUNTERED
PAREN    EQU   X'10'               OPEN PARENTHESIS ENCOUNTERED
CONTINUE EQU   X'08'               CONTINUATION CARD BEING READ
DDNAME   EQU   X'04'               OPND OBJECT IS A DDNAME
IOERROR  EQU   X'04'               AN I/O ERROR HAS OCCURED
         SPACE 1
HEXCHARS DC    C'0123456789ABCDEF'                               GP@FT
VALID    DC    C' $#@ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
CCC      DC    C'+ 0-'             CARRIAGE CONTROL CHARACTERS
DOTCHARS DC    256AL1(*-DOTCHARS)                               .UPLOG.
         ORG   DOTCHARS
         DC    64C'.' 00-3F
         ORG   DOTCHARS+X'41'
         DC    9C'.' 41-49
         ORG   DOTCHARS+X'51'
         DC    9C'.' 51-59
         ORG   DOTCHARS+X'62'
         DC    8C'.' 62-69
         ORG   DOTCHARS+X'70'
         DC    9C'.' 70-78
         ORG   DOTCHARS+X'80'
         DC    1C'.' 80
         ORG   DOTCHARS+X'8A'
         DC    7C'.' 8A-90
         ORG   DOTCHARS+X'9A'
         DC    7C'.' 9A-A0
         ORG   DOTCHARS+X'AA'
         DC    3C'.' AA-AC
         ORG   DOTCHARS+X'AE'
         DC    2C'.' AE-AF
         ORG   DOTCHARS+X'B1'
         DC    12C'.' B1-BC
         ORG   DOTCHARS+X'BE'
         DC    2C'.' BE-BF
         ORG   DOTCHARS+X'CA'
         DC    6C'.' CA-CF
         ORG   DOTCHARS+X'DA'
         DC    6C'.' DA-DF
         ORG   DOTCHARS+X'E1'
         DC    1C'.' E1
         ORG   DOTCHARS+X'EA'
         DC    6C'.' EA-EF
         ORG   DOTCHARS+X'FA'
         DC    6C'.' FA-FF
         ORG
         SPACE 1
PAGECTR  DC    PL3'0'
EDITMASK DC    X'2020202120'
SAVECCC  DS    C
         DS    C
LINE     DS    CL133                                              .SBG.
         SPACE 3
         LTORG
         TITLE 'OFFLOAD -- DATA - MESSAGES'
ADDCARD  DC    CL256'./ ADD NAME='                               GP@FT
OUTCARD  DC    CL256' '            OUTPUT RECORD STAGING AREA    GP@FT
         SPACE 3
         DC    AL1(L'BLNK1)
BLNK1    DC    C'  '
         SPACE 3
         DC    AL1(L'BLNK3)
BLNK3    DC    C'- '
         SPACE 3
         DC    AL1(L'CTLERR1)
CTLERR1  DC    C' WARNING - THE FOLLOWING CARD(S) RESEMBLE ''IEBUPDTE''*
                CONTROL CARDS'
         SPACE 3
         DC    AL1(L'CTLERR2)
CTLERR2  DC    C'           THE ''./'' WILL BE REPLACED BY ''><'''
         SPACE 3
         DC    AL1(L'IOERPFIX+L'IOERTEXT)
IOERPFIX DC    C' I/O ERROR - '
IOERTEXT DS    CL60
         SPACE 3
         DC    AL1(L'DRCTYERR+L'DCBDDNAM)
DRCTYERR DC    C' I/O ERROR - WHILE READING THE DIRECTORY FOR '
         DS    CL(L'DCBDDNAM)
         SPACE 3
         DC    AL1(L'EODERR)
EODERR   DC    C' ERROR - MISSING CONTINUATION CARD'
         SPACE 3
         DC    AL1(L'EOPM)
EOPM     DC    C'-END OF PROGRAM'
         SPACE 3
         DC    AL1(121)        AL1(L'ERRCARD+L'CARD2)            GP@FT
ERRCARD  DC    CL11' '
CARD2    DC    CL256' '                                          GP@FT
         SPACE 3
         DC    AL1(L'NOCOREM)
NOCOREM  DC    C' ERROR - THERE IS INSUFFICIENT CORE TO PROCESS THIS RE*
               QUEST'
         SPACE 3
         DC    AL1(L'NOSYSIN)
NOSYSIN  DC    C' ERROR - THE SYSIN DD STATEMENT IS MISSING'
         SPACE 3
         DC    AL1(NTSELLEN)
NTSELMSG DC    C' WARNING - '
NTSELMBR DS    CL(L'DCBDDNAM)
         DC    C' NOT FOUND'
NTSELLEN EQU   *-NTSELMSG
         SPACE 3
         DC    AL1(OFFLMLEN)
OFFLMSG  DC    C' OFFLOADING '
OFFLMBRN DS    CL(L'DCBDDNAM)
         DC    C' TO '
OFFLDDNS DS    CL(6+2*(L'DCBDDNAM))'******** FROM ********'
OFFLMLEN EQU   *-OFFLMSG
         SPACE 3
         DC    AL1(L'PGMTERM)
PGMTERM  DC    C'-PROGRAM TERMINATED'
         SPACE 3
         DC    AL1(L'PRECARD+L'CARD)
PRECARD  DC    CL30' '
CARD     DS    CL80
UPLCARD  DS    CL80                                             ,UPLOG.
UPLSTAT  DS    F                                                ,UPLOG.
UPLNUM   DS    F                                                ,UPLOG.
UPLCTR   DS    F                                                ,UPLOG.
         SPACE 3
         DC    C' '
TITLEBUF DC    CL111'1',C'PAGE '
PAGENO   DS    CL5
         SPACE 3
         DC    AL1(L'TITLE1-1)
TITLE1   DC    C'1ADS -- OFFLOAD - PDS OFFLOADING PROGRAM - &VER '
         SPACE 3
         DC    0D'0'               END OF CSECT                  GP@P6
         TITLE ' ISPF STATISTICS STRUCTURE '           DSECT ADDED 0909
MEMBER   DSECT
MEMNAME  DS    D
MEMTTR   DS    XL3
MEMC     DS    XL1                 LENGTH/2 OF MEMUSER
MEMUSER  DS    CL40                SPF STATISTICS OR SSI
         ORG   MEMUSER
SPFVM    DS    XL2                 VERSION, LEVEL
SPFFLAGS DS    X                   FLAGS
SPFSCLM  EQU   X'80'               SCLM-MANAGED
SPFXSTAT EQU   X'20'               EXTENDED STATISTICS
SPFSECS  DS    X                   TIME LAST UPDATED (SS)
SPFCREDT DS    PL4                 DATE CREATED
SPFCHGDT DS    PL4                 DATE LAST UPDATED
SPFHHMM  DS    XL2                 TIME LAST UPDATED (HHMM)
SPFCCNT  DS    H                   CURRENT SIZE
SPFICNT  DS    H                   INITIAL SIZE
SPFMOD   DS    H                   MODS
SPFUSER  DS    CL8                 USERID, CAN BE 8 BYTES         SG01
SPFBLANK DS    CL2                 ONE LESS TO COMPENSATE         SG01
         ORG   SPFBLANK                                           SG01
SPFXCCNT DS    F                   CURRENT SIZE
SPFXICNT DS    F                   INITIAL SIZE
SPFXMOD  DS    F                   MODS
*
*
         END   OFFLOAD
//*
//LKED    EXEC PGM=IEWL,REGION=1024K,
//             PARM='TEST,XREF,LET,LIST,NCAL',
//             COND=(0,LT)
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=SHR
//SYSUT1   DD  DSN=&&SYSUT1,DISP=(OLD,DELETE)
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)
//         DD  *
  NAME OFFLOAD(R)
//