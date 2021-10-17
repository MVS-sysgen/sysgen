//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                     !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM FILE !!
//*        !!                                     !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//*
//TMVS804  JOB (SYSGEN),'J01 M10: TMVS804',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/03/29 @kl TMVS804 Don't autostart primary subsystem
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TMVS804) /* REWORK(20020624) */                 .
++ VER (Z038)
   FMID(EBB1102)
 /*
  PROBLEM DESCRIPTION(S):
    TMVS804 -
      Delete autostart for primary subsystem from master
      scheduler JCL.

  COMPONENT:  5752-SC1B6-EBB1102

  SPECIAL CONDITIONS:
    ACTION:  An IPL is required after installation of this user
      modification.

  COMMENTS:
    LAST CHANGE:  2002/06/24

    THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

    MODULES
      MSTRJCL

    MACROS
      SGIEE0MS
 */.
++ MACUPD   (SGIEE0MS) DISTLIB(AMODGEN ).
./ CHANGE NAME=SGIEE0MS
         DC    CL80'//*START &SSNAME'                          @TMVS804 04900002
??
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TMVS804)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TMVS804)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TMVS804)
        DIS(WRITE)
        .
/*
//
//TMVS816  JOB (SYSGEN),'J02 M11/12: TMVS816',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2020/04/15 @KL Install usermods TMVS816 and TMVS817,
//*                external interface support for four-digit year.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD DATA,DLM='??'
++ USERMOD(TMVS816)     /* REWORK(20200415) */             .
++ VER (Z038)
   FMID(EBB1102)
   PRE  (UZ45157)                                          .
++ IF FMID(FBB1221) THEN REQ (TMVS817)
 /*
   PROBLEM DESCRIPTION(S):
     TMVS816 -
       Four-digit year support for IEE3503D DISPLAY T command;
       Four-digit year support for IEE0603D SET command syntax checker;
       Four-digit year support for IEE6603D SET UTC module.

   COMPONENT: 5752-SC1B8-EBB1102
              5752-SC1CV-EBB1102

   APARS FIXED: TMVS816

   SPECIAL CONDITIONS:
     COPYRIGHT: (C) Copyright 2020 Kevin Leonard.  All rights reserved.

     ACTION:
      An IPL with CLPA is required after installation of this user
      modification.

     DOCUMENTATION:  Text of system message IEE136I modified.

       Publication:  OS/VS2 MVS System Messages
       Form Number:  GC38-1002

       Message IEE136I is modified as follows:

         IEE136I LOCAL: TIME=hh.mm.ss DATE=yyyy.ddd
                 UTC:   TIME=hh.mm.ss DATE=yyyy.ddd

           Explanation:  In response to a DISPLAY T command, this
             message shows the local time and date, as well as
             Coordinated Universal Time (UTC).  In the message
             text, hh specifies the hour (00-23), mm specifies the
             minute (00-59), ss specifies the second (00-59).  yyyy
             specifies the year (1900-2042), and ddd specifies the
             day (001-366).

           Operator Response:  None

    DOCUMENTATION:  Explanation of "SET DATE" command modified.

      Publication:  Operator's Library: OS/VS MVS System Commands
      Form Number:  GC38-0229

      Description of the "SET" command is modified as follows:

      SET Command

        The SET command is used to change the installation performance
        (IPS) value and to set the local time and date.

        The complete syntax of the SET command is:

        { SET | T }   IPS=nn
                    { [DATE=yyyy.ddd] [,CLOCK=hh.mm.ss] }
                      RESET

      Resetting the Performance Specification

        Use the following form of the SET command to respecify the
        parameters the system resources manager uses to control job
        scheduling.  This command should be issued only at the
        direction of your system programmer.

        { SET | T }   IPS=nn

        Note: The local time and date can also be set at this time.

        IPS=nn
          The two alphameric characters indicating the IEAIPSnn
          member of SYS1.PARMLIB containing the new performance
          parameters to be used.  The new parameters take effect
          for jobs in progress as well as for jobs read and scheduled
          after the command.

        Example:

        t ips=00

          The installation performance parameters are changed
          according to the values found in the IEAIPS00 member
          of SYS1.PARMLIB.

      Changing the Local Time and Date

        After system initialization, use the following form of the
        SET command to change the local date and time.

        { SET | T }   { [DATE=yyyy.ddd] [,CLOCK=hh.mm.ss]  }
                        RESET

        Note: IPS can also be changed at this time.

        DATE=yyyy.ddd
          The year (1900-2042) and the day (001-366).  If the new
          time implies a change of date, the new date must be
          explicitly stated.

          Note:  The date may be specified with two digits of years.
          If yy.ddd is specified, it is assumed to be 19yy.ddd.

        CLOCK=hh.mm.ss
          The time in hours (00-23), minutes (00-59), and seconds
          (00-59).  The system does not change the date when the new
          time implies change of date; if you want a new date, use the
          DATE parameter or wait for the time to pass midnight.

        RESET
          The local date and time are set to the values they would
          now contain had you not changed them at system
          initialization or through a previous SET command.

        Examples:

        If, when the displayed local time and date are 19.00.00 and
        2018.231, respectively, you want to set the local time ahead
        to 1:00 a.m. the next day, enter:

          t date=2018.232,clock=01.00.00

        It is necessary to enter DATE since, in this example,
        1:00 a.m. implies a change of date.  If you want to reset
        the time and date to accurate values, enter:

          t reset

   COMMENTS:
     LAST CHANGE: 2020/04/15

     REWORK HISTORY:
      2020/04/15: Corrected zap for IEE6603D to ensure that records
        aren't too long for the input deck.

      2020/04/10: Created.

     CROSS REFERENCE-MODULE/MACRO NAMES TO USERMODS
      IEE3503D  TMVS816
      IEE0603D  TMVS816
      IEE6603D  TMVS816

     CROSS REFERENCE-USERMODS TO MODULE/MACRO NAMES
      TMVS816   IEE3503D IEE0603D IEE6603D

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

     MODULES
      IEE3503D
      IEE0603D
      IEE6603D

     LISTEND
 */.
++ SRC      (IEE0603D) DISTLIB(ASAMPLIB) SYSLIB(SAMPLIB )
                       DISTMOD(AOSB3   ).
         TITLE 'SET COMMAND SYNTAX CHECKER - MODULE IEE0603D'           00030002
IEE0603D CSECT                                                          00032001
* C 000798,001874-001877,128292-128392                           YM4026 00032401
* D 000983-000984,001762,126792-127292,146792-148292             YM4026 00032801
*********************************************************************** 00034001
*                                                                     * 00036001
*TITLE:  IEE0603D---SET COMMAND SYNTAX CHECKER - MODULE IEE0603D      * 00038002
*                                                                     * 00038401
*FUNCTION: ACCEPTS THE PARAMETERS OF THE SET COMMAND: CLOCK,    Y01040* 00038801
*   DATE, RESET, AND IPS, PLUS THE GMT AND ADDR PARAMETERS      Y02759* 00039202
*   (VALID ONLY DURING IPL, VARY CPU ONLINE, AND TOD CLOCK      Y02759* 00039902
*   RECOVERY) AND PROCEEDS AS FOLLOWS:                          Y02759* 00040602
*   IF TIME AND/OR DATE IS SPECIFIED WITHOUT GMT, OR IF RESET   Y02759* 00041302
*   IS SPECIFIED, IEE6503D IS CALLED.  IF GMT OR ADDR IS        Y02759* 00042002
*   SPECIFIED, IEE6603D IS CALLED.  ON RETURN FROM EITHER,      Y02759* 00042702
*   IF IPS WAS SPECIFIED, CONTROL IS PASSED TO IEE0803D.        Y02759* 00043402
*   OTHERWISE, CONTROL IS RETURNED TO THE CALLER.               Y02759* 00044102
*   IN ADDITION, POSITIVE SYNTAX CHECKING IS PERFORMED ON KEYWORDS    * 00044802
*   AND PARAMETERS.                                                   * 00046002
*                                                                     * 00047202
*ENTRY POINT: IEE0603D - FROM COMMAND ROUTER MODULE, IEE0403D.        * 00048402
*ATTRIBUTES: REENTRANT, READ ONLY AND SELF-RELOCATABLE.         Y01040* 00049602
*                                                                     * 00050802
*EXTERNAL REFERENCES:                                                 * 00052002
*   MASTER RESIDENT CORE, USING MACRO IEEBASEA                        * 00053202
*   EXTENDED SAVE AREA(XSA), USING MACRO IEEXSA                       * 00054402
*   COMMUNICATIONS VECTOR TABLE USING MACRO CVT                       * 00055602
*   TIMER SUPERVISOR WORKAREA(TPC), USING MACRO IEAVVTPC        Y02759* 00056002
*   TOD CLOCK WORKAREA                                          Y02759* 00056402
*                                                                     * 00056802
*MACROS: IEEXSA, IEEBASEA, AND CVT                              Y01040* 00058002
*                                                                     * 00059202
*INPUT: REGISTER 2 POINTING TO THE EXTENDED SAVE AREA WHICH MUST      * 00060402
*   CONTAIN: IN XAR, A POINTER TO THE COMMAND BUFFER                  * 00061602
*            IN XAL, A POINTER TO THE PARAMETER LIST (FIRST KEYWORD)  * 00062802
*       REGISTER 10 POINTING TO MASTER RESIDENT CORE (IEEBASEA)   M2081 00064002
*                                                                     * 00065202
*OUTPUT: FOR A NORMAL EXIT TO IEE6503D, THE SET LOCAL TIME ROUTINE:   * 00066402
*          R2 POINTS TO THE EXTENDED SAVE AREA:                 Y02759* 00068402
*          XAS CONTAINS THE TIME (IF SPECIFIED)                 Y02759* 00069002
*          XAV CONTAINS THE DATE (IF SPECIFIED)                 Y02759* 00069602
*                                                                     * 00070402
*        FOR A NORMAL EXIT TO IEE6603D, THE SET TOD CLOCK ROUTINE:    * 00072002
*          R2 POINTS TO THE EXTENDED SAVE AREA:                 Y02759* 00072102
*          XAS CONTAINS THE TIME (IF SPECIFIED)                 Y02759* 00072302
*          XAV CONTAINS THE DATE (IF SPECIFIED)                 Y02759* 00072502
*          IF ADDR WAS SPECIFIED, THE FIRST LOGICAL TCWA        Y02759* 00072702
*          ENTRY AND THE ONE FOR THE SPECIFIED ADDRESS HAVE     Y02759* 00072902
*          BEEN EXCHANGED.                                      Y02759* 00073102
*                                                                     * 00073802
*        FOR A NORMAL EXIT TO IEE0803D, THE CSCB CREATION ROUTINE:    * 00073902
*          R2 POINTS TO THE EXTENDED SAVE AREA:                 Y02759* 00074702
*          XAL CONTAINS A POINTER TO THE FIRST PARAMETER        Y02759* 00075302
*          XAU CONTAINS THE UCMI                                Y02759* 00075902
*          XAR CONTAINS A POINTER TO THE COMMAND BUFFER         Y02759* 00076502
*          XAA CONTAINS THE ASID                                Y02759* 00077102
*          XAN CONTAINS THE VERB CODE                           Y02759* 00077702
*                                                                     * 00078602
*        FOR AN ERROR EXIT TO IEE0503D, THE MESSAGE ASSEMBLER:        * 00078902
*          R2 POINTS TO THE EXTENDED SAVE AREA WHICH CONTAINS:        * 00079202
*           IN XAE, THE MESSAGE CODE (SEE ERROR MESSAGES BELOW)       * 00079802
*           IN XAV, THE VERB IN EBCDIC, LEFT ADJUSTED WITH BLANKS     * 00080602
*                                                                     * 00081402
*EXITS: NORMAL - TO IEE0803D,CALLER                             Y02759* 00082202
*       ERROR  - TO THE MESSAGE ASSEMBLY MODULE, IEE0503D             * 00083002
*                                                                     * 00083802
         EJECT                                                          00084602
*ERROR MESSAGES: THE FOLLOWING CONDITIONS GENERATE THE INDICATED      * 00085402
*   MESSAGES:                                                         * 00086202
*      1.NON-INTERNAL SET COMMAND ISSUED IN TOD CLOCK SYSTEM      20071 00087002
*        'IEE305I SET      COMMAND INVALID'         (CODE=5)    Y01040* 00088602
*      2.NON-NUMERIC OR EXCESSIVE CHARACTERS IN TIME OR DATE, OR THE  * 00089402
*        DAY DATE IS ZERO, OR A NON-LEAP YEAR HAVING 366 DAYS   YM1821* 00090202
*        'IEE306I          INVALID NUMERICS'        (CODE=6)    Y01040* 00091002
*      3.MISPLACED PERIODS IN CLOCK OR DATE PARAMETER                 * 00091802
*        'IEE307I          DELIMITER ERROR'         (CODE=7)    Y01040* 00092602
*      4.FAILURE TO FIND BLANK, COMMA OR END OF BUFFER AFTER PARAMETER* 00093402
*        'IEE308I          TERM LENGTH ERROR'       (CODE=8)    Y01040* 00094202
*      5.KEYWORD NOT FOUND IN KEYWORD TABLE                           * 00095002
*        'IEE309I          KEYWORD MISSPELLED'      (CODE=9)    Y01040* 00095802
*      6.NO KEYWORD,                                                  * 00096602
*        DATE NOT SPECIFIED WHEN CLOCK HAS TO BE SET            Y02759* 00097402
*        'IEE310I          KEYWORD MISSING'         (CODE=10)   Y01040* 00098202
*      7.BLANK AFTER KEYWORD                                          * 00099002
*        'IEE311I          PARAMETER MISSING'       (CODE=11)   Y01040* 00099802
*      8.REPEATED KEYWORD                                             * 00100602
*        'IEE312I SET      PARAMETERS CONFLICT'     (CODE=12)   Y01040* 00103802
*      9.KEYWORD SPECIFIED IS INVALID IN THIS CONTEXT                 * 00104202
*        'IEE535I          INVALID PARAMETER'       (CODE=62)   Y02759* 00104302
*                                                                     * 00104602
* NOTE- IF AN ERROR OCCURS, A CODE OF X'FF' IS PLACED IN        Y02759* 00105602
*       BAIPL(IPL ECB) TO INDICATE THAT AN ERROR OCCURRED       Y02759* 00106502
*       ON THE SET COMMAND.                                     Y02759* 00107402
*                                                                       00108401
*OPERATION:                                                             00108602
*   EACH INCOMING COMMAND HAS ITS LEADING KEYWORD COMPARED WITH A     * 00108701
*   TABLE (KEYTAB) CONTAINING EACH OF THE SIX POSSIBLE          Y01040* 00110101
*   KEYWORDS: CLOCK, DATE, ADDR, GMT, RESET, AND IPS.           Y02759* 00112102
*   SUCCESSFUL COMPARISON RESULTS IN TRANSFER BEING MADE, USING Y01040* 00112501
*   KEYTAB, TO THE APPROPRIATE KEYWORD SUBROUTINE.  SUCCESSFUL  Y01040* 00112601
*   RECOGNITION OF A KEYWORD CAUSES THE SETTING OF AN ID MASK   Y01040* 00112701
*   FOR THAT KEYWORD IN THE XSA.  THE SUBROUTINES FOR CLOCK,    Y01040* 00112802
*   TIME, AND ADDR CHECK FOR VALID NUMERICS THAT DO NOT         Y02759* 00112902
*   EXCEED THE PRE-DETERMINED MAXIMUMS. VALID TIME, DATE, OR    Y01040* 00113202
*   ADDR IS PACKED AND HELD IN THE XSA.                         Y02759* 00113602
*                                                                     * 00113702
*   AFTER RETURNING FROM THE SUBROUTINES, A CHECK IS MADE FOR         * 00113902
*   SUCCESSIVE KEYWORDS. IF THERE ARE MORE, THE KEYTAB SCAN IS        * 00115102
*   REPEATED.                                                         * 00117102
*                                                                     * 00129301
         EJECT                                                          00129401
*TABLES/WORK AREAS: THE EXTENDED SAVE AREA AS USED INTERNALLY IN      * 00129501
*   IEE0603D IS MAPPED AS FOLLOWS:                                    * 00130801
*                                                                     * 00132801
*       ***************************************************           * 00133201
*       *XAP                     *XAD                     *           * 00133301
*       *SAVE AREA FOR PTR TO    *                        *           * 00133401
*       * END OF COMMAND BUFFER. *                        *           * 00134701
*       ***************************************************           * 00136701
*       *XAX                                              *           * 00137102
*       *      CPU ADDRESS IN PACKED DECIMAL              *             00137202
*       *                                                 *           * 00139602
*       ***************************************************           * 00140302
*       *XAE   *XAR+1            *XAL                     *           * 00141602
*       *ID/ER *BUFFER           *SAVE AREA FOR CURRENT   *           * 00143602
*       *CODE  *POINTER          * PARAMETER POINTER.     *           * 00145602
*       ***************************************************           * 00146001
*       *XAV                     *XAV+4                   *           * 00146101
*       *                        * DATE IN PACKED         *           * 00146201
*       *                        *     DECIMAL.           *           * 00146301
*       ***************************************************           * 00146401
*       *XAS                     *XAS+4                   *           * 00147501
*       *                        * TIME IN PACKED         *           * 00149501
*       *                        *     DECIMAL.           *           * 00149601
*       ***************************************************           * 00149701
*                                                                     * 00149801
*   A KEYWORD TABLE (KEYTAB), WHICH CONTROLS KEYWORD SCANNING AND     * 00150102
*   KEYWORD SUBROUTINE BRANCHING.                                     * 00150802
*                                                                     * 00151502
*                                                                     * 00152701
*CHARACTER SET  THE OPERATION OF THIS MODULE DEPENDS UPON AN INTERNAL * 00153101
*   REPRESENTATION OF THE EXTERNAL CHARACTER SET WHICH IS EQUIVALENT  * 00153201
*   TO THE ONE USED AT ASSEMBLY TIME. THE CODING HAS BEEN ARRANGED SO * 00153301
*   THAT REDEFINITION OF 'CHARACTER' CONSTANTS, BY REASSEMBLY, WILL   * 00153401
*   RESULT IN A CORRECT MODULE FOR THE NEW DEFINITIONS.               * 00153701
*                                                                  @L01 00153711
*                                                                  @L01 00153721
*Change activity      =                                            @L01 00153731
*                                                                  @L01 00153741
*   Flag  Date        By    Description                            @L01 00153751
*   ----  ----------  ----  ------------------------------------   @L01 00153761
*   $L01  2020/04/10  KL    TMVS816 Four-digit year support.       @L01 00153771
*                                                                  @L01 00153781
*                                                                     * 00154101
*********************************************************************** 00154201
         EJECT                                                          00154301
*      REGISTER ASSIGNMENTS        FUNCTION(S)                          00154401
R0       EQU   0                   ENTRY CODE TO ESTAE EXIT      Y02759 00155302
R1       EQU   1                   POST                                 00155402
R2       EQU   2                   BASE REGISTER FOR XSA                00155701
R3       EQU   3                   CURRENT PARAMETER PTR AND LINK REG   00156101
R4       EQU   4                   KEYTAB POINTER AND WORK REGISTER     00156501
R5       EQU   5                   ERROR LINKER AND WORK REGISTER       00156901
R6       EQU   6                   WORK REGISTER                 Y02759 00157002
R7       EQU   7                   WORK REGISTER                 Y02759 00157102
R8       EQU   8                   WORK REGISTER                        00157201
R9       EQU   9                   PARAMETER POINTER AND WORK REGISTER  00157601
R10      EQU   10                  BASE FOR MASTER RESIDENT CORE        00158001
R11      EQU   11                  WORK REGISTER                        00158401
SBASE    EQU   12                  BASE REGISTER FOR PROGRAM            00158501
R13      EQU   13                  WORK REGISTER                  20071 00162202
R14      EQU   14                  BRANCH REGISTER                20071 00164201
R15      EQU   15                  LINKAGE REGISTER              Y02759 00166202
*                                                                     * 00166601
* DISPLACEMENTS OF INFORMATION WITHIN KEYWORD TABLE ELEMENTS          * 00166801
KEYELN   EQU   14                  KEYWORD TABLE ELEMENT LENGTH         00167302
KEYLNG   EQU   0                   DISPLACEMENT OF KEYWORD LENGTH       00168301
KEYDIS   EQU   1                   DISPLACEMENT OF DELIMITER            00170301
KEYWRD   EQU   2                   DISPLACEMENT OF KEYWORD              00170401
KEYBRN   EQU   8                   DISPLACEMENT OF BRANCH               00170501
KEYIDM   EQU   12                  DISPLACEMENT OF ID MASK              00171501
KEYFLD   EQU   13                  DISPLACEMENT OF FOLD COUNT    Y02759 00171902
* ERROR CODES FOR MESSAGE MODULE                                        00173501
ERR5     EQU   5                    ERROR CODE                   Y02759 00173602
ERR6     EQU   6                    ERROR CODE                   Y02759 00173902
ERR7     EQU   7                    ERROR CODE                   Y02759 00174202
ERR8     EQU   8                    ERROR CODE                   Y02759 00174502
ERR9     EQU   9                    ERROR CODE                   Y02759 00174802
ERR10    EQU   10                   ERROR CODE                   Y02759 00175102
ERR11    EQU   11                   ERROR CODE                   Y02759 00175402
ERR12    EQU   12                   ERROR CODE                   Y02759 00175702
ERR13    EQU   62                   ERROR CODE                   Y02759 00176002
* PERIOD DISPLACEMENTS FOR DATE AND CLOCK                             * 00176601
PERD1    EQU   2                   DISPLACEMENT OF PERIOD               00176701
PERD2    EQU   5                   DISPLACEMENT OF PERIOD               00176801
PERD3    EQU   4                   4-digit year period location    @L01 00176901
D0       EQU   0                                                 Y02759 00177202
D1       EQU   1                   OFFSET TO GET TO KEYWORD      Y01040 00179502
D2       EQU   2                                                  20071 00179902
D3       EQU   3                                                  20071 00180302
D4       EQU   4                                                   @L01 00181302
D8       EQU   8                                                  20071 00181502
FOX      EQU   15                                                       00181902
D30      EQU   30                                                       00182302
XFF      EQU   X'FF'                                              20071 00183201
C6       EQU   C'6'                                               20071 00184401
C9       EQU   C'9'                                               20071 00184801
LOC16    EQU   16                                                Y02759 00184902
ZERO     EQU   X'00'               NO PARAMETER FOLLOWING        Y01040*00185101
                                        KEYWORD                  Y01040 00185501
CLOCK    EQU   X'01'               SPECIFIC CODE FOR CLOCK       Y02759 00186202
DATE     EQU   X'02'               SPECIFIC CODE FOR DATE        Y02759 00186502
GMT      EQU   X'04'               SPECIFIC CODE FOR GMT         Y02759 00186802
ADDR     EQU   X'08'               SPECIFIC CODE FOR ADDR        Y02759 00187102
RESET    EQU   X'10'               SPECIFIC CODE FOR RESET       Y02759 00187402
IPS      EQU   X'20'               SPECIFIC CODE FOR IPS         Y02759 00187702
* UPPER AND LOWER COMPARANDS FOR SCHECK                               * 00188001
SNUMHI   EQU   C'9'                HIGH NUMERIC                         00188301
SNAWT    EQU   C'0'                CHARACTER ZERO                       00188601
SEINS    EQU   X'01'               BINARY ONE                           00188901
SNUMLO   EQU   SNAWT-SEINS         LOW NUMERIC COMPARAND                00189201
         EJECT                                                          00189501
* MAIN ROUTINE - ESTABLISHES ADDRESSABILITY FOR PROGRAM, XSA, AND     * 00189902
*    MASTER RESIDENT CORE.  USES XSA POINTERS TO COMMAND BUFFER AND   * 00190302
*    LEADING KEYWORD TO IDENTIFY THE KEYWORD. KEYWORD RECOGNITION     * 00190702
*    LEADS TO BRANCHING TO THE KEYWORD'S SUBROUTINE.                  * 00191002
*                                                                     * 00191601
*/*IEE0603D: CHART (FMODE) */                                           00191702
*/* HEADER                                                              00191801
*/*MASTER SCHEDULER (COMMANDS)                             GMT AND L    00196101
*/*OCAL TIME                                                IEE0603D    00198101
*/*                                                              PAG    00198201
*/*E #                                                               */ 00198301
*/*IEE0603D:1A2 E ENTRY FROM IEE0403D */                                00198502
*/*1B2 M ESTABLISH ADDRESSIBILITY */                                    00198602
         BALR  SBASE,0             ESTABLISH PROGRAM ADDRESSABILITY     00198701
         USING MODBASE,SBASE                                     Y02759 00198802
MODBASE  EQU   *                                                 Y02759 00199002
IEE0603D MODID BRANCH=YES                                        Y01886 00199202
         USING XSA,R2              BASE FOR EXTENDED SAVE AREA          00199701
         USING BASE,R10            ESTABLISH BASEA ADDRESSABLTY         00199801
*/*1C2 P SET POINTER TO START OF KEYWORDS AND ESTABLISH RECOVERY EXIT*/ 00199902
         SR    R15,R15             ZEROES TO INITIALIZE ERROR    Y02759 00213302
*                                       EXIT FLAGS               Y02759 00213602
         L     R9,XAL              USE REG 9 AS PTR TO PARMS     Y04925 00213902
         STM   R9,R15,REGSAVE      SAVE REGS 9-15                Y02759 00216702
         LA    R9,0(R9)            ZERO H.O. BYTE TO TEST PTR    Y04925 00216902
         MVC   ESTAELST(ESTAELN),ESTAEL  COPY ESTAE PARAMETER           00217102
*                                       LIST TO XSA                     00217502
         LA    R1,ESTAELST         GET ADDR OF ESTAE LIST FORM          00217602
         LA    R7,EXITPARM         GET RECVY EXIT PARMLIST ADDRESS      00217702
         ESTAE ,CT,PARAM=(7),MF=(E,(1))  ESTABLISH RECOVERY EXIT        00217902
*/*1D2 D (NO,SERR10,YES,) ANY KEYWORDS */                               00218602
         LTR   R9,R9               IS PARAMETER PTR 0?            20071 00218802
         BZ    SERR10              YES, KEYWORDS MISSING          20071 00219002
*/*1E2 P SET UP REGISTERS FOR KEYWORD SCAN TABLE */                     00219902
         L     R7,XAR              LOAD POINTER TO BUFFER               00220902
         LA    R7,0(R7)                 CONTAINING COMMAND              00221902
         LH    R6,0(R7)            LOAD BUFFER LENGTH                   00222902
         N     R6,SFFFF-2          CLEAR POSSIBLE GARBAGE               00223902
*/*1F2 P COMPUTE END OF BUFFER AND SAVE */                              00224902
         AR    R7,R6               COMPUTE END OF BUFFER                00225902
         ST    R7,XAP              SAVE IN FIRST WORD OF XSA            00226902
*/*1G2 M CLEAR KEYWORD MASK BYTE */                                     00227902
         NI    XAE,X'00'           CLEAR ID MASK AREA                   00228902
         SPACE                                                          00229902
*/*SNEWKY:1H2 M SET UP TO CHECK KEYWORD AGAINST TABLE */                00230902
SNEWKY   EQU   *                   EXAMINE KEYWORD                      00231902
         LA    R6,KEYELN           LOAD KEYTAB ELEMENT LENGTH           00232902
*                                       FOR INCREMENT IN BXLE           00233902
         LA    R4,KEYTAB           GET ADDRESS OF KEYWORD TABLE         00234902
         LA    R7,KEYTBL(R4)       GET END OF TABLE ADDRESS             00235902
*                                       FOR COMPARAND IN BXLE           00236902
         SR    R5,R5               LOAD LENGTH OF A                     00237902
SNXTWRD  IC    R5,KEYLNG(R4)            PARTICULAR KEYWORD              00238902
*/*SETCLK:1J2 D (YES,BUFFERCK,NO,) KEYWORD A MATCH */                   00239902
         EX    R5,SETMVC           MOVE POTENTIAL PARM TO WKAREA        00240902
         IC    R5,KEYFLD(R4)       GET LENGTH TO FOLD TO CAPS           00241902
         EX    R5,SETOC            FOLD TO UPPER CASE                   00242902
         IC    R5,KEYLNG(R4)       GET LENGTH TO COMPARE                00243902
         EX    R5,SETCLC           IS THIS THE KEYWORD?                 00244902
         BE    CKPARM              YES, NOW GO CHECK PARAMETER          00245902
*/*1K2 D (YES,SERR09,NO,) END OF TABLE */                               00246902
         BXLE  R4,R6,SNXTWRD       RECHECK IF NOT END OF TABLE          00247902
*/*1K1 P (,SETCLK) INCREMENT TO NEXT KEYWORD */                         00248902
         B     SERR09              ERROR, KEYWORD NOT FOUND             00249902
         SPACE                                                          00250902
*/*BUFFERCK:1B4 D (NO,INCRMT,YES,) SHOULD KEYWORD HAVE PARM */          00251902
CKPARM   LA    R9,1(R9,R5)         POINT REGISTER TO PARAMETER          00252902
         CLI   D1(R4),ZERO         SHOULD THERE BE A PARMETER?   Y01040 00253902
         BE    INCRMT              NO, SKIP CHECK FOR ONE        Y01040 00254902
*/*1C4 D (NO,SERR11,YES,) IS THERE A PARAMETER */                       00255902
         CLI   0(R9),C' '          IS PARAMETER BLANK?                  00256902
         BE    SERR11              YES, ERROR                           00257902
         SPACE                                                          00258902
*/*INCRMT:1D4 D (YES,SERR08,NO,) BUFFER EXCEEDED */                     00259902
INCRMT   EQU   *                                                 Y01040 00260902
         SR    R3,R3               POINT A REGISTER TO THE END OF       00261902
         IC    R3,KEYDIS(R4)            PARAMETER                       00262902
         LA    R3,0(R3,R9)                                              00263902
         C     R3,XAP              IS BUFFER EXCEEDED?                  00264902
         BH    SERR08              YES, ERROR                           00265902
         SPACE                                                          00266902
*/*1E4 P GET KEYWORD */                                                 00267902
         SR    R5,R5               NO, PREPARE TO                       00268902
         IC    R5,KEYIDM(R4)            LOAD KEYWORD'S ID MASK          00269902
*/*1F4 D (YES,SERR12,NO,) KEYWORD A REPEAT */                           00270902
         EX    R5,SETTMP           IS THIS KEYWORD A REPEAT?            00271902
         BO    SERR12              YES, ERROR                           00272902
         SPACE                                                          00273902
*/*1G4 P SAVE KEYWORD ID */                                             00274902
         OC    XAE(1),KEYIDM(R4)   SAVE KEYWORD'S ID IN XSA             00275902
         ST    R3,XAL              SAVE POINTER TO END OF PARAMETER     00276902
*/*1H4 D (CLOK,SCLOCK,DATE,SDATE,ADDR,SADDR,IPS,SETNXT,RSET,SETNXT,GMT, 00277902
*/*SETNXT)                                                              00278902
*/* GO TO APPROPRIATE PROCESSING ROUTINE */                             00279902
         EX    0,KEYBRN(R4)        GO TO THE APPROPRIATE SUBROUTINE     00280902
         EJECT                                                          00281902
* KEYWORD=CLOCK SUBROUTINE - SYNTAX CHECKS THE CLOCK PARAMETER FOR    * 00282902
*    PROPER POSITIONING OF PERIODS, NUMERIC CHARACTERS, AND THE EX-   * 00283902
*    CEEDING OF PROPER LIMITS FOR HOUR, MINUTE OR SECOND. A VALID     * 00284902
*    PARAMETER IS PACKED IN THE XSA TEMPORARILY, IN XAS.              * 00285902
*                                                                     * 00286902
*/*SCLOCK:2A3 D (NO,SERR07,YES,) ARE PERIODS CORRECT */                 00287902
SCLOCK   EQU   *                                                        00288902
         CLI   PERD1(R9),C'.'      CHECK FOR PROPER PLACEMENT           00289902
         BNE   SERR07                        INCORRECT                  00290902
         CLI   PERD2(R9),C'.'           OF PERIODS                      00291902
         BNE   SERR07                        INCORRECT                  00292902
*/*2B3 P REMOVE PERIODS AND SAVE HHMMSS */                              00293902
         MVC   XAS(2),0(R9)        CONSOLIDATE TIME WITHOUT             00294902
         MVC   XAS+2(2),3(R9)           PERIODS                         00295902
         MVC   XAS+4(2),6(R9)                                           00296902
         LA    R11,6               SET LOOP CONTROLLER TO 6             00297902
         LA    R9,XAS              SET POINTER TO TIME                  00298902
*/*2C3 S SCHECK: VALIDATE TIME DIGITS */                                00299902
         BAL R13,SCHECK            VERIFY TIME DIGITS             20071 00300902
*/*2D3 D (YES,SERR06,NO,) DO HOURS EXCEED 23 */                         00301902
         CLC   XAS(2),SETCHR       DO HOURS EXCEED 23?                  00302902
         BH    SERR06              YES, ERROR                           00303902
*/*2E3 D (YES,SERR06,NO,) DO MINUTES EXCEED 59 */                       00304902
         CLC   XAS+2(2),SETCMS     DO MINUTES EXCEED 59?                00305902
         BH    SERR06              YES, ERROR                           00306902
*/*2F3 D (YES,SERR06,NO,) DO SECONDS EXCEED 59 */                       00307902
         CLC   XAS+4(2),SETCMS     DO SECONDS EXCEED 59?                00308902
         BH    SERR06              YES, ERROR                           00309902
*/*2G3 P (,SETNXT) SAVE TIME IN PACKED DECIMAL */                       00310902
         PACK  XAS+4(4),XAS(6)     PACK TIME FOR CONVERSION             00311902
         B     SETNXT              GO CHECK NEXT KEYWORD                00312902
         EJECT                                                          00313902
* KEYWORD=DATE SUBROUTINE - SYNTAX CHECKS THE DATE PARAMETER FOR      * 00314902
*    PROPER POSITIONING OF PERIODS, NUMERIC CHARACTERS, AND THE EX-   * 00315902
*    CEEDING OF PROPER LIMITS FOR THE DAY. A VALID PARAMETER IS       * 00316902
*    PACKED IN THE XSA TEMPORARILY, IN XAV.                           * 00317902
*                                                                     * 00318902
*/*SDATE:2A1 D (NO,SERR07,YES,) IS THE PERIOD CORRECT */                00319902
SDATE    EQU   *                                                        00320902
         CLI   PERD1(R9),C'.'      IS PERIOD POSITIONED?                00321902
         BNE   SYEAR4              No: is it 4-digit year?         @L01 00322902
*/*2B1 P REMOVE THE PERIOD AND SAVE YYDDD */                            00323902
         MVC   XAV(2),0(R9)        CONSOLIDATE DATE WITHOUT             00324902
         MVC   XAV+2(3),3(R9)           PERIODS                         00325902
         LA    R11,5               SET LOOP CONTROLLER TO 5             00326902
         LA    R9,XAV              SET POINTER TO DATE                  00327902
*/*2C1 S SCHECK: VALIDATE DATE DIGITS */                                00328902
         BAL   R13,SCHECK          VERIFY DATE DIGITS             20071 00329902
*/*2D1 D (YES,SERR06,NO,) IS THE DAY 000? */                            00330902
         CLC   XAV+D2(D3),SETDD0   IS DDD = 000?                 YM1821 00331902
         BE    SERR06              YES, INVALID NUMERICS         YM1821 00332902
*/*2E1 M PRESET EXITING ADDRESS TO 'SETNXT' IN REGISTER 13 */           00333902
         LA    R13,SETNXT          SET BRANCH TO NEXT KEYWORD    YM1821 00334902
*/*2F1 D (GT,SERR06,LT,SYEAR,EQ,) DDD = 366? */                         00335902
         CLC   XAV+2(3),SETDDY     DO DAYS EXCEED 366?                  00336902
         BH    SERR06              YES, ERROR                           00337902
         BL    SYEAR               BRANCH IF LESS THAN 366 DAYS  YM1821 00338902
*/*2G1 M PRESET EXITING ADDRESS TO 'LEAPYEAR' IN REGISTER 13 */         00339902
         LA    R13,LEAPYEAR        SET BRANCH TO YEAR VALIDITY   YM1821 00340902
SYEAR    EQU   *                                                 YM1821 00341902
*/*SYEAR:2H1 P SAVE YYDDD IN PACKED DECIMAL */                          00342902
         PACK  XAV+4(4),XAV(5)     PACK DATE FOR CONVERSION             00343902
*/*2J1 D (YES,SETNXT,NO,LEAPYEAR) REGISTER 13 POINTING TO 'SETNXT'? */  00344902
         BR    R13                                               YM1821 00345902
         SPACE 1                                                   @L01 00345907
*----------------------------------------------------------------* @L01 00345912
*        Date is not a valid two-digit year.  Adjust the         * @L01 00345917
*        end-of-parameter pointer (calculated from the           * @L01 00345922
*        parameter table entry for DATE) to count the two        * @L01 00345927
*        digits added by the four-digit year.  If that           * @L01 00345932
*        would exceed the buffer length, go to error.            * @L01 00345937
*----------------------------------------------------------------* @L01 00345942
SYEAR4   LA    R3,2(,R3)           Adjust end pointer              @L01 00345947
         C     R3,XAP              Does it exceed buffer?          @L01 00345952
         BH    SERR08              Yes: error                      @L01 00345957
         SPACE 1                                                   @L01 00345962
*----------------------------------------------------------------* @L01 00345967
*        Test if date could be a four-digit year.  If            * @L01 00345972
*        not, go to INVALID NUMERICS error.                      * @L01 00345977
*----------------------------------------------------------------* @L01 00345982
         CLI   PERD3(R9),C'.'      Is is 4-digit year?             @L01 00345987
         BNE   SERR07              No: error                       @L01 00345992
         SPACE 1                                                   @L01 00345997
*----------------------------------------------------------------* @L01 00346002
*        Date is potentially a valid four-digit year.            * @L01 00346007
*        Remove the period delimiter between year and            * @L01 00346012
*        day to make date "YYYYDDD".                             * @L01 00346017
*----------------------------------------------------------------* @L01 00346022
         MVC   XAV(4),0(R9)        Consolidate date                @L01 00346027
         MVC   XAV+4(3),5(R9)       without periods                @L01 00346032
         SPACE 1                                                   @L01 00346037
*----------------------------------------------------------------* @L01 00346042
*        Call SCHECK subroutine to verify that the date          * @L01 00346047
*        string contains valid zoned decimal numerics.           * @L01 00346052
*        R11 is set to the length of the string to be            * @L01 00346057
*        validated by SCHECK, and R9 points to the start         * @L01 00346062
*        of the string to be validated.                          * @L01 00346067
*----------------------------------------------------------------* @L01 00346072
         LA    R11,7               Set date length                 @L01 00346077
         LA    R9,XAV              Set date address                @L01 00346082
         BAL   R13,SCHECK          Verify date digits              @L01 00346087
         SPACE 1                                                   @L01 00346092
*----------------------------------------------------------------* @L01 00346097
*        The date string is all valid numerics.  The             * @L01 00346102
*        maximum date that can be contained by the 52-bit        * @L01 00346107
*        TOD clock starting in 1900 is September 17, 2042        * @L01 00346112
*        (2042.260).  Make sure the specified year isn't         * @L01 00346117
*        greater than 2042.                                      * @L01 00346122
*----------------------------------------------------------------* @L01 00346127
         CLC   XAV(D4),MAXYR       Year GT 2042?                   @L01 00346132
         BH    SERR06              GT: invalid numerics            @L01 00346137
         BL    SYEAR4A             LT: year OK                     @L01 00346142
         SPACE 1                                                   @L01 00346147
*----------------------------------------------------------------* @L01 00346152
*        Specified year is 2042.  Make sure the day isn't        * @L01 00346157
*        greater than 260, and branch to INVALID NUMERICS        * @L01 00346162
*        error if it is.                                         * @L01 00346167
*----------------------------------------------------------------* @L01 00346172
         CLC   XAV+D4(D3),D260     If year=2042 is day OK?         @L01 00346177
         BH    SERR06              No: invalid numerics            @L01 00346182
         SPACE 1                                                   @L01 00346187
*----------------------------------------------------------------* @L01 00346192
*        Ensure that the specified year is at least 1900.        * @L01 00346197
*        Branch to INVALID NUMERICS error if not.                * @L01 00346202
*----------------------------------------------------------------* @L01 00346207
SYEAR4A  CLC   XAV(D4),MINYR       Year GE 1900?                   @L01 00346212
         BL    SERR06              No: invalid numerics            @L01 00346217
         SPACE 1                                                   @L01 00346222
*----------------------------------------------------------------* @L01 00346227
*        Ensure that the specified day is not 000.  Branch       * @L01 00346232
*        to INVALID NUMERICS error if day specified as 000.      * @L01 00346237
*----------------------------------------------------------------* @L01 00346242
         CLC   XAV+D4(D3),SETDD0   Is DDD=000?                     @L01 00346247
         BE    SERR06              Yes: invalid numerics           @L01 00346252
         SPACE 1                                                   @L01 00346257
*----------------------------------------------------------------* @L01 00346262
*        Assume the specified year is not a leap year,           * @L01 00346267
*        and initialize the SDATE subroutine exit address        * @L01 00346272
*        accordingly to branch to SETNXT.                        * @L01 00346277
*----------------------------------------------------------------* @L01 00346282
         LA    R13,SETNXT          Assume not leap year            @L01 00346287
         SPACE 1                                                   @L01 00346292
*----------------------------------------------------------------* @L01 00346297
*        Test for day specified as 366.  If specified            * @L01 00346302
*        day is greater than 366, it's an error.  If day         * @L01 00346307
*        is less than 366, we don't need to know if this         * @L01 00346312
*        is a leap year and will exit from SDATE to SETNXT.      * @L01 00346317
*----------------------------------------------------------------* @L01 00346322
         CLC   XAV+4(3),SETDDY     Day GT 366?                     @L01 00346327
         BH    SERR06              Yes: error                      @L01 00346332
         BL    SYEAR4B             No: skip leap year validation   @L01 00346337
         SPACE 1                                                   @L01 00346342
*----------------------------------------------------------------* @L01 00346347
*        Specified day is 366.  Change the SDATE exit            * @L01 00346352
*        address to branch to "LEAPYEAR" leap year               * @L01 00346357
*        validity check.                                         * @L01 00346362
*----------------------------------------------------------------* @L01 00346367
         LA    R13,LEAPYEAR        Set leap year validity check    @L01 00346372
         SPACE 1                                                   @L01 00346377
*----------------------------------------------------------------* @L01 00346382
*        Convert the specified date from zoned decimal           * @L01 00346387
*        to packed, and subtract 1900 from the year to           * @L01 00346392
*        leave the date in the form returned by SVC 11.          * @L01 00346397
*----------------------------------------------------------------* @L01 00346402
SYEAR4B  PACK  XAV+4(4),XAV(7)     Make date packed                @L01 00346407
         SP    XAV+4(4),P1900      Subtract 1900 from year         @L01 00346412
         OI    XAV+7,X'0F'         Force unsigned                  @L01 00346417
         SPACE 1                                                   @L01 00346422
*----------------------------------------------------------------* @L01 00346427
*        Exit from SDATE to the next routine, whose              * @L01 00346432
*        address has already been placed in R13.                 * @L01 00346437
*----------------------------------------------------------------* @L01 00346442
         BR    R13                 To next routine                 @L01 00346447
         SPACE 1                                                   @L01 00346452
*----------------------------------------------------------------* @L01 00346457
*        Constants for four-digit year processing.               * @L01 00346462
*----------------------------------------------------------------* @L01 00346467
P1900    DC    PL4'1900000'        For year adjustment             @L01 00346472
D260     DC    CL3'260'            Last non-wrap day in 2042       @L01 00346477
MAXYR    DC    CL4'2042'           Last non-wrap year              @L01 00346482
MINYR    DC    CL4'1900'           First valid year                @L01 00346487
         EJECT ,                                                   @L01 00346492
*********************************************************************** 00346902
*                                                                     * 00347902
* THIS ROUTINE, ENTERED ONLY AFTER A DATE WITH 366 DAYS         Y02759* 00348902
* IS ENCOUNTERED, CHECKS FOR THE VALIDITY OF THE 366 DAYS IN    YM1821* 00349902
* THE YEAR.  THE YEAR IN QUESTION IS DIVIDED BY 4, AND IF A     YM1821* 00350902
* REMAINDER IS PRESENT, THE ROUTINE LEADS TO AN ERROR MESSAGE   YM1821* 00351902
* ISSUED.  NORMAL PROCESSING IS CONTINUED FOR A VALID LEAP      YM1821* 00352902
* YEAR.                                                         YM1821* 00353902
*                                                                     * 00354902
*********************************************************************** 00355902
*/*LEAPYEAR:2J2 P OBTAIN THE YEAR */                                    00356902
LEAPYEAR DS    0H                                                  @L01 00357902
         SR    R4,R4               CLEAR REGISTER                YM1821 00358902
         ST    R4,XAV              CLEAR FIRST WORD FOR 'CVB'    YM1821 00359902
         CVB   R5,XAV              CONVERT TO BINARY             YM1821 00360902
         D     R4,D1000            SHIFT OFF DDD                        00361902
*/*2J3 D (YES,SERR06,NO,) IS IT 1900? */                                00363902
         LTR   R5,R5               TEST FOR 1900                        00371902
         BZ    SERR06              ERROR IF 1900                        00381902
*/*2J4 D (NO,SERR06,YES,SETNXT) IS IT PERFECTLY DIVISIBLE BY 4? */      00383902
         SLL   R5,D30              SHIFT OFF ALL BUT BITS 30-31         00391902
         LTR   R5,R5               EITHER 0NE ON?                       00392302
         BNZ   SERR06              YES, ERROR.  NOT A LEAP YEAR         00402202
         B     SETNXT              GO CHECK NEXT KEYWORD         YM1821 00599602
      EJECT                                                             00629201
* KEYWORD=ADDR SUBROUTINE - VALIDITY CHECKS THE CPU ADDRESS SPECIFIED.* 00631202
*    A VALID ADDRESS IS PACKED IN THE XSA TEMPORARILY, IN XAX.        * 00633202
*                                                                     * 00635202
*/*SADDR:2A2 P SAVE CPU ADDRESS */                                      00639602
SADDR    EQU   *                                                        00640002
         MVC   XAX(2),D0(R9)       SAVE SPECIFIED CPU ADDRESS    Y02759 00641202
         LA    R11,D2              SET LOOP CONTROLLER TO 2      Y02759 00646202
*/*2B2 S SCHECK:VALIDATE ADDRESS DIGITS */                              00651202
         BAL   R13,SCHECK          VERIFY ADDRESS DIGITS         Y02759 00659202
*/*2C2 D (YES,SERR06,NO,) DOES ADDR EXCEED 15? */                       00669202
         CLC   XAX(2),SETCADDR     DOES ADDRESS EXCEED 15?       Y02759 00671202
         BH    SERR06              YES, ERROR                    Y02759 00673202
*/*2D2 P (,SETNXT) SAVE ADDR IN PACKED DECIMAL */                       00675202
         PACK  XAX(8),XAX(2)       PACK CPU ADDRESS FOR          Y02759 00677202
*                                  FURTHER VERIFICATION          Y02759 00677602
         B     SETNXT              GO CHECK NEXT KEYWORD         Y02759 00678002
      EJECT                                                             00678402
* THE FOLLOWING CLOSED SUBROUTINE IS USED BY SCLOCK, SDATE, AND       * 00679202
*    SADDR TO CHECK THAT ALL PARAMETER CHARACTERS ARE NUMERIC. R11    * 00689202
*    IS THE LOOP COUNTER; R3 IS THE RETURN REGISTER; R9 POINTS TO     * 00729202
*    THE PARAMETER.                                                   * 00779202
*                                                                     * 00829201
*/*SCHECK:2A5 E NUMERIC CHECK SUBRT */                                  00879202
SCHECK   EQU   *                                                        00929201
*/*2B5 P CHECK FOR NUMERIC DIGITS */                                    00979202
         SR    R4,R4               SET INCREMENT REGISTERS TO ZERO      01029201
         SR    R6,R6                                                    01079201
         LA    R5,SNUMLO           SET LOWER LIMIT OF DIGIT             01129201
         LA    R7,SNUMHI           SET UPPER LIMIT OF DIGIT             01179201
*/*SDGTLP:2C5 D (YES,SERR06,NO,) DIGIT LESS THAN 0? */                  01229202
SDGTLP   SR    R8,R8               CLEAR FOR LOADING                    01279201
         IC    R8,0(R9)            TAKE ONE CHARACTER                   01329201
         BXLE  R8,R4,SERR06        TEST FOR LOW                         01379201
*/*2D5 D (YES,SERR06,NO,) DIGIT GREATER THAN 9? */                      01429202
         BXH   R8,R6,SERR06        TEST FOR HIGH                        01479201
*/*2E5 P INCREMENT TO NEXT DIGIT */                                     01529202
         LA    R9,1(R9)            INCREMENT CHARACTER POINTER          01579201
*/*2F5 D (NO,SDGTLP,YES,) LAST DIGIT CHECKED? */                        01629202
         BCT   R11,SDGTLP          IS LOOP DONE?                        01679201
*/*2G5 R RETURN TO CALLER */                                            01729202
         BR    R13                 YES, RETURN                    20071 01779201
         EJECT                                                          08129201
* RETURNED TO FROM A KEYWORD SUBROUTINE, THIS PORTION OF THE MAIN     * 08179201
*    ROUTINE CHECKS IF THE SET COMMAND IS COMPLETE OR IF THERE ARE    * 08229201
*    MORE KEYWORDS. A COMMA AS DELIMITER RESULTS IN THE RETURN TO     * 08279201
*    THE KEYWORD TABLE SCAN FOR THE NEW KEYWORD. SUCCESSFUL COMPLE-   * 08329201
*    TION LEADS TO 'SETOUT'.                                          * 08379201
*                                                                     * 08429201
*/*SETNXT:1D1 D (YES,SETOUT,NO,) END OF BUFFER? */                      08479202
SETNXT   EQU   *                                                        08529201
         C     R3,XAP              IS IT VALID END OF BUFFER?           08579201
         BE    SETOUT              YES, END OF SET COMMAND              08629201
*/*1E1 D (YES,SETOUT,NO,) END OF COMMAND? */                            08679202
         CLI   0(R3),C' '          IS NEXT CHARACTER A BLANK?           08729201
         BE    SETOUT              YES, END OF SET COMMAND              08779201
*/*1F1 D (NO,SERR08,YES,) IS THERE A COMMA? */                          08829202
         CLI   0(R3),C','          IS NEXT CHARACTER A COMMA?           08879201
         BNE   SERR08              NO, MUST BE ERROR                    08929201
         LA    R9,1(R3)            YES, BUMP CURRENT PARAMETER          08979201
*/*1G1 D (NO,SERR07,YES,SNEWKY) ANOTHER PARAMETER FOLLOWING? */         09029202
         CLI   0(R9),C' '          IS NEXT CHARACTER BLANK?             09079201
         BE    SERR07              YES, DELIMITER ERROR                 09129201
         B     SNEWKY              GO CHECK NEXT KEYWORD                09179201
         EJECT                                                          09229201
*/*SETOUT:3A1 M PRESET REGISTER 14 TO THE ADDRESS OF 'CKIPS' */         09249202
SETOUT   LA    R14,CKIPS              PICKUP RETURN ADDRESS             09279202
*/*3B1 D (NO,SERR05,YES,) IS IT IPL AND INTERNAL? */                    09329202
         TM    BASFL,BAIN+BAINTSET    IS IT IPL/VARY AND INTERNAL?      12939202
         BZ    SERR05                 IPL/VARY, BUT NOT INTERNAL        12941202
*/*3C1 D (YES,INTSET,NO,) IS IT INTERNAL ONLY? */                       12941602
         TM    BASFL,BAINTSET         IS IT INTERNAL?                   12943202
         BO    INTSET                 YES, BRANCH                       12945202
*/*3D1 D (YES,SERR13,NO,) GMT/ADDR AFTER IPL? */                        12945602
         TM    XAE,GMT+ADDR           GMT OR ADDR SPECFD AFTER IPL?     12947202
         BNZ   SERR13                 YES, ERROR                        12947602
*/*3E1 D (NO,NORESET,YES,) RESET SPECIFIED? */                          12947702
         TM    XAE,RESET              RESET SPECIFIED?                  12948002
         BZ    NORESET                NO, BRANCH                        12948402
*/*3F1 D (YES,SERR12,NO,LCL) RESET WITH DATE/CLOCK? */                  12948502
         TM    XAE,DATE+CLOCK         RESET SPECFD WITH DATE/CLOCK?     12948802
         BNZ   SERR12                 YES, ERROR                        12948902
         B     LCL                                                      12949002
*/*NORESET:3E2 D (NO,CKIPS,YES,) CLOCK/DATE SPECIFIED? */               12952302
NORESET  TM    XAE,CLOCK+DATE         CLOCK AND/OR DATE SPECFD?         12955802
         BCR   D8,R14                 NO, GO CHECK FOR IPS              12959102
*/*LCL:3G1 P GET ADDRESS OF IEE6503D */                                 12961102
LCL      L     R15,VCON65             GET ADDR OF SET LOCAL TIME        12969102
*/*3H1 S (,CKIPS) IEE6503D:SET LOCAL TIME */                            12971102
CALLMOD  BR    R15                    ROUTINE AND BALR TO IT            12972402
         DROP  SBASE                                                    12974402
         USING *,R14                                                    12974802
*/*CKIPS:3J1 D (NO,NOIPS:BT,YES,DESTAE1:RL) IPS SPECIFIED? */           12974902
CKIPS    LM    R9,R14,REGSAVE         RESTORE REGS 9-14                 12975202
         DROP  R14                                                      12975602
         USING MODBASE,SBASE                                            12975702
         TM    XAE,IPS                IPS SPECIFIED?                    12975802
         BZ    NOIPS                  NO, BRANCH                        12989102
         ST    R9,XAL                 RESTORE VERB CODE AND      Y04925 12991102
*                                          POINTER TO FIRST PARM Y04925 12995102
         MVC   XAV(8),SETSET          MOVE VERB NAME TO XSA      Y04925 12999102
*/*DESTAE1:3J2 P DELETE RECOVERY EXIT */                                12999202
         ESTAE 0                      DELETE RECOVERY EXIT              12999302
*/*3K2 R GET ADDRESS OF IEE0803D AND BRANCH TO IT */                    12999702
         L     R15,VCON08             GET ADDR OF CSCB CREATION RTN     13001102
         BR    15                     BRANCH TO CSCB CREATION RTN       13009802
*/*NOIPS:3K1 R DELETE RECOVERY EXIT AND RETURN TO IEE0403D */           13011902
NOIPS    ESTAE 0                      DELETE RECOVERY EXIT              13012302
         BR    R14                    RETURN IF NO IPS                  13014202
         EJECT                                                          13016402
*/*INTSET:3A4 M ESTABLISH ADDRESSABILITY TO TOD CLOCK WORKAREA */       13018602
INTSET   L     R6,LOC16               GET CVT POINTER                   13020802
         USING CVT,R6                                                   13023002
         L     R6,CVTTPC              GET TPC POINTER                   13025202
         USING TPC,R6                                                   13027402
         L     R6,TPCTCWA             GET TCWA POINTER                  13029602
         USING TCWA,R6                                                  13031802
*/*3B4 D (YES,SERR13,NO,) RESET SPECIFIED? */                           13034002
         TM    XAE,RESET              RESET SPECIFIED                   13036202
         BO    SERR13                 YES, RESET IS ERROR ON INT SET    13038402
*/*3C4 D (YES,CKZZZ,NO,) ADDR SPECIFIED? */                             13040602
         TM    XAE,ADDR               ADDR SPECIFIED?                   13042802
         BO    CKZZZ                  YES, BRANCH                       13045002
*/*3D4 D (YES,CKDATE,NO,) MSG IEA886A OR IEA887A ISSUED? */             13047202
         TM    TCWAGFLG,MSGYYY+MSGZZZ  MSG YYY OR ZZZ OUTSTANDING       13049402
         BNZ   CKDATE                 YES, SEE IF DATE SPECFD           13051602
*/*3E4 D (NO,CKGMT,YES,) DATE/CLOCK SPECIFIED? */                       13053802
         TM    XAE,DATE+CLOCK         NO, MUST BE XXX OUTSTANDING       13056002
*                                         SEE IF CLOCK/DATE SPECFD      13058202
         BZ    CKGMT                  IF NOT, CHECK FOR GMT             13060402
*/*3F4 D (NO,LCL,YES,) GMT SPECIFIED? */                                13062602
         TM    XAE,GMT                IF SO, IS GMT SPECIFIED?          13064802
         BZ    LCL                    NO, GO TO LCL TIME RTN            13067002
*/*TOD:3H4 P GET ADDRESS OF IEE6603D */                                 13069202
TOD      L     R15,VCON66             GET ADDR OF SET TOD CLOCK RTN     13071402
*/*3J4 S (,CKIPS) IEE6603D:SET TOD CLOCK */                             13073602
         BR    R15                        AND GO THERE                  13075802
*/*CKDATE:3D5 D (NO,SERR10,YES,TOD) DATE SPECIFIED? */                  13078002
CKDATE   TM    XAE,DATE               DATE SPECIFIED?                   13080202
         BZ    SERR10                 NO, ERROR                         13082402
         B     TOD                    YES, GO TO TOD CLOCK RTN          13084602
*/*CKGMT:3E5 D (YES,SERR13,NO,CKIPS) GMT SPECIFIED? */                  13086802
CKGMT    TM    XAE,GMT                GMT SPECIFIED?                    13089002
         BO    SERR13                 YES, GMT WITHOUT DATE/CLOCK       13091202
*                                         IS AN ERROR                   13093402
         B     CKIPS                  NO, GO CHECK FOR IPS              13095602
*                                                                       13097802
* ADDR WAS SPECIFIED.  SEE IF IT IS VALID, AND ACT ACCORDINGLY          13100002
*                                                                       13102202
*/*CKZZZ:3C3 D (NO,SERR13,YES,) MSG IEA887A ISSUED? */                  13104402
CKZZZ    TM    TCWAGFLG,MSGZZZ        MSG ZZZ OUTSTANDING?              13106602
         BZ    SERR13                 WITHOUT IT, ADDR IS AN ERROR      13108802
*/*3D3 P SEARCH TOD CLOCK WORKAREA ENTRIES FOR SPECIFIED ADDR */        13111002
         CVB   R5,XAX                 CONVERT THE ADDR TO BINARY        13113202
         LA    R7,LTCWA(,R6)          GET PTR TO FIRST TCWA ENTRY       13115402
         USING TCENTRY,R7                                               13117602
         LH    R8,TCCPUCNT            GET NUMBER OF ENTRIES             13119802
NEXT     CH    R5,TCWAIADD            COMPARE SPECIFIED CPU ADDR        13122002
*                                          WITH ADDR IN ENTRY           13124202
         BE    MATCH                  BRANCH IF THEY MATCH              13126402
         LA    R7,LENTRY(,R7)         POINT TO NEXT ENTRY               13128602
         BCT   R8,NEXT                BRANCH IF MORE ENTRIES            13130802
*/*3E3 D (NO,SERR06,YES,) CORRECT ENTRY FOUND? */                       13133002
         B     SERR06                 ELSE, NO MATCH FOUND, ERROR       13135202
*/*3F3 D (NO,SERR06,YES,) IS THIS CLOCK SET? */                         13137402
MATCH    TM    TCWALFLG,TCLCC         THIS CLOCK SET?                   13139602
         BNZ   SERR06                 NO, ERROR                         13141802
         LA    R8,LTCWA(,R6)          GET PTR TO FIRST ENTRY            13144002
*/*3G3 D (YES,NOSWAP:RR,NO,) IS THIS 1ST ENTRY? */                      13146202
         CR    R7,R8                  IS MATCHING ENTRY THE FIRST?      13148402
         BE    NOSWAP                 YES, NO SWAP REQUIRED             13150602
*/*3H3 M SWAP 1ST AND MATCHING ENTRIES */                               13152802
         XC    D0(LENTRY,R7),D0(R8)   EXCHANGE FIRST                    13155002
         XC    D0(LENTRY,R8),D0(R7)         AND MATCHING                13157202
         XC    D0(LENTRY,R7),D0(R8)               ENTRIES               13159402
*/*NOSWAP:3J3 M (,CKIPS:B ) INDICATE TOD CLOCK O.K. */                  13161602
NOSWAP   NI    TCWAGFLG,XFF-TCGNSET   INDICATE TOD CLOCK OK             13163802
         B     CKIPS                  GO CHECK FOR IPS                  13166002
         EJECT                                                          13168202
* THE FOLLOWING INSTRUCTIONS PASS A DESIGNATED ERROR CODE TO THE      * 13170402
*    SET COMMAND ERROR SUBROUTINE, 'SETERR', IN REGISTER 5.           * 13172602
*                                                                     * 13174802
*/*SERR05:4A3 M (,SETERR) SET ERROR CODE FOR COMMAND INVALID */         13177002
SERR05   BAL   R5,SETERR           ERROR CODE = 5                 20071 13179201
         DC    AL1(ERR5)                                          20071 13229201
*                                                                 20071 13279201
*/*SERR06:4B2 M (,SETERR) SET ERROR CODE FOR INVALID NUMERICS */        13329202
SERR06   BAL   R5,SETERR           ERROR CODE = 6                       13379201
         DC    AL1(ERR6)                                                13429201
*                                                                     * 13479201
*/*SERR07:4B4 M (,SETERR) SET ERROR CODE FOR DELIMITER ERROR */         13529202
SERR07   BAL   R5,SETERR           ERROR CODE = 7                       13579201
         DC    AL1(ERR7)                                                13629201
*                                                                     * 13679201
*/*SERR08:4C2 M (,SETERR) SET ERROR CODE FOR TERM LENGTH ERROR */       13729202
SERR08   BAL   R5,SETERR           ERROR CODE = 8                       13779201
         DC    AL1(ERR8)                                                13829201
*                                                                     * 13879201
*/*SERR09:4C4 M (,SETERR) SET ERROR CODE FOR UNIDENTIFIBLE KEYWORD */   13929202
SERR09   BAL   R5,SETERR           ERROR CODE = 9                       13979201
         DC    AL1(ERR9)                                                14029201
*                                                                     * 14079201
*/*SERR10:4D2 M (,SETERR) SET ERROR CODE FOR KEYWORD MISSING */         14129202
SERR10   BAL   R5,SETERR           ERROR CODE = 10                      14179201
         DC    AL1(ERR10)                                               14229201
*                                                                     * 14279201
*/*SERR11:4D4 M (,SETERR) SET ERROR CODE FOR PARAMETER MISSING */       14329202
SERR11   BAL   R5,SETERR           ERROR CODE = 11                      14379201
         DC    AL1(ERR11)                                               14429201
*                                                                     * 14479201
*/*SERR12:4E2 M (,SETERR) SET ERROR CODE FOR PARAMETERS CONFLICT */     14529202
SERR12   BAL   R5,SETERR           ERROR CODE = 12                      14579201
         DC    AL1(ERR12)                                               14629201
*                                                                       14679202
*/*SERR13:4E4 M (,SETERR) SET ERROR CODE FOR INVALID PARAMETER */       14689202
SERR13   BAL   R5,SETERR           ERROR CODE = 62              Y02759  14729202
         DC    AL1(ERR13)                                       Y02759  14799202
         EJECT                                                          14879201
* ERROR SUBROUTINE - RECEIVES IN REGISTER 5 AN ERROR MESSAGE CODE     * 14929201
*    WHICH IS PLACED IN THE XSA (IN XAE). CONTROL GOES TO THE MESSAGE * 14979202
*    ASSEMBLY MODULE, IEE0503D, BY BRANCH.                      Y01040* 15029201
*/*SETERR:4F3 M SET UP TO ISSUE MESSAGE */                              15079202
SETERR   DS    0H                                                       15129201
         TM    BASFL,BAINTSET      INTERNAL COMMAND?                    15131202
         BNO   EXTCMD              NO, BRANCH                           15133202
         MVC   XAV(8),SETRPLY      MOVE VARIABLE TEXT TO XSA     Y02759 15139202
         B     INSERRC             GO GET ERROR CODE                    15149202
EXTCMD   MVC   XAV(8),SETSET       MOVE VARIABLE TEXT TO XSA            15179202
INSERRC  MVC   XAE(1),0(R5)        INSERT ERROR CODE                    15229202
*/*4G3 M INDICATE PARAMETERS NOT ACCEPTED */                            15279202
         MVI   BAIPLCC,XFF         PUT RETURN CODE IN IPL ECB    Y01040 15329202
         LM   R9,R14,REGSAVE       RESTORE REGS 9-14             Y02759 15339202
*/*4H3 P DELETE RECOVERY EXIT */                                        15349202
         ESTAE 0                   DELETE RECOVERY EXIT                 15359202
*/*4J3 P GET ADDRESS OF IEE0503D */                                     15379202
         L    R15,VCON05           GET ADDRESS OF IEE0503D       Y01029 15429201
*/*4K3 R BRANCH TO IEE0503D */                                          15479202
         BR   R15                  BRANCH TO IEE0503D            Y01029 15529201
         EJECT                                                          15629201
*                                                                       15639202
* ERROR RECOVERY EXIT                                                   15649202
*                                                                       15659202
*/*SERREXIT:5A3 E ESTAE EXIT ROUTINE */                                 15661202
SERREXIT BALR  SBASE,0             ESTABLISH ADDRESSABILITY             15669202
         USING *,SBASE                                                  15671202
*/*5B3 M INDICATE PERCOLATION IN REG 15 */                              15671602
         SR    R15,R15             SET RC FOR PERCOLATION               15673202
*/*5C3 D (NO,CMDFAIL,YES,) SDWA SUPPLIED? */                            15673602
         CH    R0,H12              SEE IF SDWA SUPPLIED                 15675202
         BE    FAILCMND            BRANCH IF NOT                        15677202
         L     R2,0(R1)            IF SO, GET PARMLIST ADDR INTO R2     15677602
         USING EXITPARM,R2         PARMLIST ADDRESSABILITY              15677702
*/*5D3 P ISSUE SETRP */                                                 15677802
         SETRP RECPARM=SETID,RC=0, ESTABLISH INTERFACE BACK TO RTM     *15678002
               DUMP=NO                                                  15678102
*/*5E3 M DETERMINE CSECT WHERE ERROR OCCURRED AND SET NAME IN SDWA */   15680102
         TM    CSECTCDE,X'C0'      DETERMINE WHERE ERROR OCCURRED       15682102
         BZ    FAILCMND            BRANCH IF ERROR IN IEE0603D          15684102
*                                     AS CSECT NAME IS CORRECT          15686102
         BO    CSECT66             BRANCH TO CHANGE CSECT NAME IF       15688102
*                                     ERROR IN IEE6603D                 15690102
         USING SDWA,R1                                                  15695802
         MVC   SDWACSCT+3(2),C65   CHANGE CSECT NAME TO IEE6503D        15705802
         B     FAILCMND            BRANCH TO SET FAIL FLAG              15707802
CSECT66  MVC   SDWACSCT+3(2),C66   CHANGE CSECT NAME TO IEE6603D        15709802
         DROP  R1                                                       15711802
*/*CMDFAIL:5F3 M INDICATE PARAMETERS NOT ACCEPTED */                    15711902
FAILCMND L     R10,R10SAVE         GET ADDRESS OF BASEA                 15712202
         MVI   BAIPLCC,XFF         INDICATE COMMAND FAILED              15712302
*/*5G3 R RETURN TO RTM */                                               15719702
         BR    R14                 RETURN TO RTM                        15727402
*/*IEE0603D: END */                                                     15734802
         USING MODBASE,SBASE       RE-ESTABLISH ORIGINAL ADDRESSABILITY 15742202
         USING XSA,R2                                                   15749602
         EJECT                                                          15757002
* THE FOLLOWING TABLE CONTROLS THE SCAN OF INCOMING KEYWORDS AND THE  * 15764402
*   BRANCHING TO SPECIFIC KEYWORD SUBROUTINES.                        * 15771802
*                                                                     * 15779201
         DS    0H                                                       15829201
KEYTAB   EQU   *                                                        15879201
         DC    X'5'                KEYWORD LENGTH MINUS ONE             15929201
         DC    X'8'                PARAMETER DELIMITER                  15979201
         DC    C'CLOCK='           KEYWORD                              16029201
         B     SCLOCK              SPECIFIC CODING                      16079201
         DC    X'01'               KEYWORD ID MASK PATTERN              16129201
         DC    X'4'                CHARS TO FOLD MINUS ONE       Y02759 16179202
*                                                                     * 16229201
         DC    X'4'                KEYWORD LENGTH MINUS ONE             16279201
         DC    X'6'                PARAMETER DELIMITER                  16329201
         DC    C'DATE='            KEYWORD                              16379201
         DC    C' '                FILLER                               16429201
         B     SDATE               SPECIFIC CODING                      16479201
         DC    X'02'               KEYWORD ID MASK PATTERN              16529201
         DC    X'3'                CHARS TO FOLD MINUS ONE       Y02759 16539202
*                                                                     * 16629201
         DC    X'2'                KEYWORD LENGTH MINUS ONE      Y01040 16679202
         DC    X'0'                PARAMETER DELIMITER           Y01040 16729202
         DC    C'GMT'              KEYWORD                       Y01040 16779202
         DC    C'   '              FILLER                        Y01040 16829202
         B     SETNXT              SPECIFIC CODING               Y01040 16879202
         DC    X'04'               KEYWORD ID MASK PATTERN       Y01040 16929202
         DC    X'2'                CHARS TO FOLD MINUS ONE       Y02759 16939202
*                                                                  @L01 16949202
         DC    X'2'                KEYWORD LENGTH MINUS ONE        @L01 16959202
         DC    X'0'                PARAMETER DELIMITER             @L01 16969202
         DC    C'UTC'              KEYWORD                         @L01 16979202
         DC    C'   '              FILLER                          @L01 16989202
         B     SETNXT              SPECIFIC CODING                 @L01 16999202
         DC    X'04'               KEYWORD ID MASK PATTERN         @L01 17009202
         DC    X'2'                CHARS TO FOLD MINUS ONE         @L01 17019202
*                                                                     * 17029202
         DC    X'4'                KEYWORD LENGTH MINUS ONE      Y02759 17079202
         DC    X'2'                PARAMETER DELIMITER           Y02759 17129202
         DC    C'ADDR='            KEYWORD                       Y02759 17179202
         DC    C' '                FILLER                        Y02759 17229202
         B     SADDR               SPECIFIC CODING               Y02759 17279202
         DC    X'08'               KEYWORD ID MASK PATTERN       Y02759 17329202
         DC    X'3'                CHARS TO FOLD MINUS ONE       Y02759 17379202
*                                                                     * 17429201
         DC    X'4'                KEYWORD LENGTH MINUS ONE      Y02759 17479202
         DC    X'0'                PARAMETER DELIMITER           Y02759 17529202
         DC    C'RESET'            KEYWORD                       Y02759 17579202
         DC    C' '                FILLER                        Y02759 17629202
         B     SETNXT              SPECIFIC CODING               Y02759 17679202
         DC    X'10'               KEYWORD ID MASK PATTERN       Y02759 17729202
         DC    X'4'                CHARS TO FOLD MINUS ONE       Y02759 17779202
*                                                                     * 17829202
         DC    X'3'                KEYWORD LENGTH MINUS ONE      Y02759 17879202
         DC    X'2'                PARAMETER DELIMITER           Y02759 17929202
         DC    C'IPS='             KEYWORD                       Y02759 17979202
         DC    C'  '               FILLER                        Y02759 18029202
         B     SETNXT              SPECIFIC CODING               Y02759 18079202
         DC    X'20'               KEYWORD ID MASK PATTERN       Y02759 18129202
         DC    X'2'                CHARS TO FOLD MINUS ONE       Y02759 18179202
KEYTBL   EQU   *-KEYTAB-1          KEYWORD TABLE LENGTH MINUS ONE  @L01 18189202
         EJECT                                                          18229201
* LIST FORM OF ESTAE MACRO                                              18239202
*                                                                       18249202
ESTAEL   ESTAE SERREXIT,PURGE=NONE,ASYNCH=NO,RECORD=YES,MF=L            18259202
ESTAELN  EQU   *-ESTAEL            LENGTH OF ESTAE PARMLIST             18269202
         EJECT                                                          18271202
* CONSTANTS USED BY THE IMMEDIATE COMMANDS ROUTINE                    * 18279201
*                                                                     * 18329201
         DS    0F                                                       18379201
         DC    X'0000'                                                  18429201
SFFFF    DC    X'FFFF'                                                  18479201
*                                                                     * 18529201
SETCADDR DC    C'15'               MAXIMUM CPU ADDR              Y02759 18539202
SETCHR   DC    C'23'               MAXIMUM HOUR                         18579201
SETCMS   DC    C'59'               MAXIMUM MINUTE OR SECOND             18629201
SETDDY   DC    C'366'              MAXIMUM DAY                          18679201
SET99    DC    C'99'               HIGH DATE COMPRAND                   18729201
C65      DC    C'65'               CHARS TO CHANGE CSECT NAME           18739202
C66      DC    C'66'               CHARS TO CHANGE CSECT NAME           18749202
*                                                                     * 18779201
SETSET   DC    C'SET     '         VERB FOR MESSAGE MODULE              18829201
SETRPLY  DC    C'RPLY HAS'                                       Y02759 18879202
SETDD0   DC    C'000'              DDD PORTION OF DATE, 000      Y01040 18929201
SETUC    DC    6X'40'              FOR FOLDING TO UPPER CASE     Y02759 18939202
SETID    DS    0CL24               ID FOR RECOVERY RECORD               18949202
         DC    CL8'IGC0003D'       LOAD MODULE NAME                     18959202
         DC    CL8'IEE0603D'       CSECT NAME                           18969202
         DC    CL8'SETCMFRR'       ERROR IDENTIFIER                     18971202
*                                                                     * 18979201
SETCLC   CLC   FOLDAREA(1),KEYWRD(R4) COMPARE NEW KEYWORD WITH TABLE    19029202
SETTMP   TM    XAE,X'00'           TEST ID MASK PATTERN                 19079202
SETMVC   MVC   FOLDAREA(1),0(R9)   MOVE TEXT TO WORKAREA                19089202
SETOC    OC    FOLDAREA(1),SETUC   FOLD TO CAPS                         19099202
*                                                                     * 19129201
H12      DC    H'12'               TEST VALUE FOR NO SDWA               19139202
D1000    DC    F'1000'             CONSTANT FOR SHIFTING OFF DDD OF     19179202
*                                      YYDDD CONVERTED TO BINARY        19189202
*                                                                     * 19229202
         DS   0F                                                 Y01029 19379201
VCON05   DC   V(IEE0503D)          ADDRESS OF IEE0503D           Y01029 19429201
VCON08   DC   V(IEE0803D)          ADDRESS OF IEE0803D           Y02759 19439202
VCON65   DC   V(IEE6503D)          ADDRESS OF IEE6503D           Y01029 19479201
VCON66   DC   V(IEE6603D)          ADDRESS OF IEE6603D           Y02759 19499202
         EJECT                                                          19509202
SETTCWA  DSECT                                                          19519202
TCWA     DS    0D                      TOD CLOCK WORKAREA               19521202
         DS    52C                                                      19523202
TCWAGFLG DS    1C                      GLOBAL TCWA FLAGS                19525202
MSGXXX   EQU   X'80'                   MSG XXX OUTSTANDING              19527202
MSGYYY   EQU   X'40'                   MSG YYY OUTSTANDING              19527302
MSGZZZ   EQU   X'20'                   MSG ZZZ OUTSTANDING              19527402
TCGNSET  EQU   X'08'                   TOD CLOCK NOT SET                19527502
         DS    1C                                                       19528602
TCCPUCNT DS    1H                      NUMBER OF TCWA ENTRIES           19528702
         DS    56C                                                      19528802
LTCWA    EQU   *-TCWA                  LENGTH OF TCWA                   19528902
         SPACE 10                                                       19529002
SETTCENT DSECT                                                          19529102
TCENTRY  DS    0D                      TOD CLOCK WORKAREA ENTRY         19529202
         DS    12C                                                      19545802
TCWAIADD DS    1H                      I-STREAM ADDRESS                 19555802
         DS    1C                                                       19557802
TCWALFLG DS    1C                      LOCAL FLAGS                      19558202
TCLCC    EQU   X'30'                                                    19558602
LENTRY   EQU   *-TCENTRY               LENGTH OF ENTRY                  19559802
         EJECT                                                          19562502
         IHASDWA                                                        19572502
         EJECT                                                          19574502
SETXSA   DSECT                                                          19579201
         IEEXSA                                                         19629201
         EJECT                                                          19639202
*                                                                       19649202
* THIS MAPS THE XASAVSTD FIELD OF THE EXTENDED XSA. IT IS USED          19659202
* FOR A REGISTER SAVEAREA, AND A WORKAREA FOR BOTH MAINLINE AND         19669202
* RECOVERY.                                                             19673202
*                                                                       19675202
         ORG   XASAVSTD            MAP ON EXTENDED SAVEAREA             19677202
FOLDAREA DS    8C                  WORKAREA FOR FOLDING KEYWORDS        19677602
EXITPARM DS    0CL28               PARM AREA USED BY ESTAE EXIT RTN     19678002
REGSAVE  EQU   EXITPARM            SAVEAREA FOR REGS 9 THRU 15          19678402
R9SAVE   DS    F                                                        19678802
R10SAVE  DS    F                                                        19678902
R11SAVE  DS    F                                                        19679002
R12SAVE  DS    F                                                        19679102
R13SAVE  DS    F                                                        19682402
R14SAVE  DS    F                                                        19684402
R15SAVE  DS    F                                                        19684802
         ORG   R15SAVE             MAP ON FIELD R15SAVE                 19685002
EXITFLGS EQU   *                   FLAGS FOR ESTAE EXIT RTN             19685202
CSECTCDE DS    C                   BITS 0 AND 1 ARE THE ONLY BITS USED  19685602
*                                    '00'B IS MODULE IEE0603D           19685702
*                                    '01'B IS MODULE IEE6503D           19685802
*                                    '11'B IS MODULE IEE6603D           19692002
         DS    3C                  RESERVED                             19695202
ESTAELST ESTAE SERREXIT,PURGE=NONE,ASYNCH=NO,RECORD=YES,MF=L            19697202
         EJECT                                                          19698202
         CVT   DSECT=YES                                                19704402
         EJECT                                                          19710602
         IEAVVTPC                                                       19716802
         EJECT                                                          19723002
         IEEBASEA                                                       19729201
         END                                                            19779201
++ ZAP      (IEE6603D) DISTLIB(AOSC5   ).
NAME IEE6603D
IDRDATA TMVS816
VER 00B4  F342,9008,201D        UNPK   8(5,R9),29(3,R2)   Use specified
VER 00BE  F342,9008,901D        UNPK   8(5,R9),29(3,R9)   Use previous
VER 00C4  F272,9010,900A        PACK   16(8,R9),10(3,R9)  Convert days
VER 00D0  F271,9010,9008        PACK   16(8,R9),8(2,R9)   Convert years
REP 00B4  F363,9008,201C        UNPK   8(7,R9),28(4,R2)   UNPK  TCWAWRK
REP 00BE  F363,9008,901C        UNPK   8(7,R9),28(4,R9)   UNPK  TCWAWRK
REP 00C4  F272,9010,900C        PACK   16(8,R9),12(3,R9)  PACK  TCWAWRK
REP 00D0  F273,9010,9008        PACK   16(8,R9),8(4,R9)   PACK  TCWAWRK
++ SRC      (IEE3503D) DISTLIB(ASAMPLIB) SYSLIB(SAMPLIB )
                       DISTMOD(AOSB3   ).
         TITLE 'IEE3503D - DISPLAY / TRACK ROUTER'                      00100002
*********************************************************************** 00150002
*                                                                     * 00200002
*  MODULE NAME  =  IEE3503D                                     Y02669* 00250002
*                                                                     * 00300002
*  DESCRIPTION  =  ROUTER FOR DISPLAY AND TRACK COMMANDS        Y02669* 00350002
*                                                                     * 00400002
*  COPYRIGHT    =  N/A                                          Y02669* 00450002
*                                                                     * 00500002
*  STATUS       =  VS2 3.8 usermod TMVS816                         @L01 00510002
*                                                                     * 00600002
*  FUNCTION     =  THIS MODULE DETERMINES THE DISPLAY COMMAND   Y02669* 00650002
*                  OPERANDS: D  A          ACTIVE               Y02669* 00700002
*                               C,K        K COMMAND OPERANDS   Y02669* 00800002
*                               DMN        DOMAIN             @Z40BPSV* 00805041
*                               DUMP       DUMP               @G33CPMR* 00825043
*                               CONSOLES(C)CONSOLES             YM1916* 00850002
*                               JOBS (J)   JOBS, STARTED TASKS  Y02669* 00950002
*                               M          MATRIX               Y02651* 01000002
*                               NET        VTAM                 Y02674* 01010002
*                               PFK        PGM FUNCTION KEYS    Y02669* 01050002
*                               R          REQUESTS             Y02669* 01100002
*                               SLIP       SERVICEABILITY     @G17SPTJ* 01110041
*                                           LEVEL INDICATOR   @G17SPTJ* 01120041
*                                            PROGRAM          @G17SPTJ* 01130041
*                               T          TIME                 Y02669* 01150002
*                               TP         TCAM                 Y02669* 01200002
*                               TS         TIME SHARING USERIDS Y02669* 01250002
*                               U          UNITS                Y02669* 01300002
*                               3850       MSS COMMAND        @Z40LPTA* 01310041
*                                                                     * 01350002
*                  AND THE TRACK OPERANDS:                      Y02669* 01400002
*                           TR  A          ACTIVE               Y02669* 01450002
*                               JOBS (J)   JOBS, STARTED TASKS  Y02669* 01550002
*                               TS         TIME SHARING USERIDS Y02669* 01600002
*                  (THE TRACK COMMAND IS A TIMER INTERVAL       Y02669* 01650002
*                  DRIVEN DISPLAY.)                             Y02669* 01660002
*                                                                     * 01670002
*                  IT THEN ROUTES SPECIFIC INFORMATION IN THE   Y02669* 01680002
*                  XSA (EXTENTED SAVE AREA) TO THE APPROPIATE   Y02669* 01690002
*                  PROCESSOR.                                   Y02669* 01692002
*                                                                     * 01694002
*                  ONLY D T (DISPLAY TIME) IS HANDLED           Y02669* 01696002
*                  EXCLUSIVELY IN IEE3503D.                     Y02669* 01698002
*                                                                     * 01700002
     EJECT                                                              01710041
*                                                                     * 01720041
*   OPERATION   =  FOR D U; U,; THE VERB CODE IN XAN IS CHANGED Y02669* 01750002
*                  TO X'52', IEE3503D BRANCHES TO IEE7503D TO   Y02669* 01800002
*                  RESOLVE THE ROUTING OPERAND (L=CCA) OR       Y02669* 01850002
*                  DETERMINE THE DEFAULTS.                      Y02669* 01900002
*                                                                     * 01950002
*                  FOR D M; M,; M= THE VERB CODE IN XAN IS      Y02651* 02000002
*                  CHANGED TO X'32', IEE3503D BRANCHES TO       Y02651* 02010002
*                  IEE7503D TO RESOLVE THE ROUTING OPERAND      Y02651* 02050002
*                  (L=CCA) OR DETERMINE THE DEFAULTS.           Y02651* 02100002
*                                                                     * 02150002
*                  FOR D TS; TS,; JOBS(J); JOBS,(J,); A; A,;    Y02669* 02200002
*                  A BIT IS SET IN XAX (XAXMASK), IEE3503D      Y02669* 02250002
*                  BRANCHES TO IEE7503D TO ESTABLISH ROUTING    Y02669* 02300002
*                  INFORMATION FROM THE L=CCA OPERAND OR        Y02669* 02350002
*                  DETERMINE THE APPROPIATE DEFAULTS            Y02669* 02360002
*                  AND CHECK FOR AND PROCESS THE 'LIST'(L)      Y02669* 02400002
*                  OPERAND. (SEE OUTPUT FOR BIT SETTINGS)       Y02669* 02500002
*                                                                     * 02550002
*                  FOR D C,K; C,K,; CONSOLES(C); CONSOLES,(C,); YM1916* 02600002
*                  THE VERB CODE IN XAN IS CHANGED TO X'2C'     Y02669* 02630002
*                  FOR C,K AND X'3E' FOR CONSOLES; IEE3503D     YM1916* 02660002
*                  BRANCHES TO IEE7503D TO RESOLVE THE ROUTING  Y02669* 02690002
*                  OPERAND (L=CCA) OR DETERMINE THE DEFAULTS.   Y02669* 02720002
*                                                                     * 02750002
*                  FOR D R; R,; THE VERB CODE IN XAN IS         Y02669* 02800002
*                  CHANGED TO X'42', IEE3503D BRANCHES TO       Y02669* 02804002
*                  IEE7503D TO RESOLVE THE ROUTING OPERAND      Y02669* 02810002
*                  (L=CCA) OR DETERMINE THE DEFAULTS.           Y02669* 02820002
*                                                                     * 02900002
*                  FOR D PFK; THE VERB CODE IN XAN IS CHANGED   Y02669* 02950002
*                  TO X'36'AND XAS IS SET TO ZEROES, IEE3503D   Y02669* 02960002
*                  BRANCHES TO IEE0803D TO CREATE A CSCB.       Y02669* 03000002
*                                                                     * 03050002
*                  FOR D T; THE FOLLOWING MESSAGE IS WRITTEN    Y02669* 03100002
*                  TO THE OPERATOR VIA WTO OR TPUT:             Y02669* 03150002
*                   IEE136I LOCAL: TIME=hh.mm.ss DATE=yyyy.ddd    @L01* 03160002
*                           UTC: TIME=hh.mm.ss DATE=yyyy.ddd      @L01* 03170002
*                                                                     * 03300002
*                  FOR D TP,; IEE3503D BRANCHES TO IED1303D.    Y02669* 03350002
*                                                                     * 03400002
*                  FOR D NET,; IEE3503D BRANCHES TO ISTCFF3D.   Y02674* 03450002
*                                                                     * 03460040
*                  FOR D 3850; IEE3503D BRANCHES TO IEE7503D  @Z40LPTA* 03465041
*                                                                     * 03480040
*                  FOR D DMN; IEE3503D BRANCHES TO IEE7503D   @Z40BPSV* 03482041
*                                                                     * 03500002
         EJECT                                                          03503041
*                                                                     * 03506041
*                  FOR D SLIP, THE VERB CODE IN XAN IS        @G17SPTJ* 03509041
*                  CHANGED TO X'A8'.  IEE3503D BRANCHES TO    @G17SPTJ* 03512041
*                  IEE7503D TO RESOLVE THE ROUTING OPERAND    @G17SPTJ* 03515041
*                  (L=CCA) OR TO DETERMINE THE DEFAULTS       @G17SPTJ* 03518041
*                                                                     * 03518443
*                  FOR D DUMP, THE VERB CODE IN XAN IS        @G33CPMR* 03518843
*                  CHANGED TO X'B0'.  IEE3503D BRANCHES TO    @G33CPMR* 03519143
*                  IEE7503D TO RESOLVE THE ROUTING OPERAND    @G33CPMR* 03519443
*                  (L=CCA) OR TO DETERMINE THE DEFAULTS       @G33CPMR* 03519743
*                                                                     * 03520443
*                  **** NOTE ****                             @G17SPTJ* 03524041
*                    FOR 'D DMN', 'D SLIP' AND 'D DUMP':      @G33CPMR* 03525043
*                  WHENEVER THE ABOVE-MENTIONED COMMANDS ARE  @G17SPTJ* 03530041
*                  ENTERED, THEY ARE CHECKED OUT FURTHER TO   @G17SPTJ* 03533041
*                  SEE IF THE SYSTEM SUPPORTS THE SU'S THAT   @G17SPTJ* 03536041
*                  ARE ASSOCIATED WITH THE COMMAND.           @G17SPTJ* 03539041
*                                                             @G17SPTJ* 03542041
*                  FOR TR TS; TS,; JOBS(J); JOBS,(J,); A; A,;   Y02669* 03550002
*                  A BIT IS SET IN XAX (XAXMASK), IEE3503D      Y02669* 03600002
*                  BRANCHES TO IEE7503D TO ESTABLISH ROUTING    Y02669* 03650002
*                  INFORMATION FROM THE L=CCA OPERAND OR        Y02669* 03700002
*                  DETERMINE THE APPROPIATE DEFAULTS AND        Y02669* 03710002
*                  CHECK FOR AND PROCESS THE 'LIST' (L)         Y02669* 03750002
*                  OPERAND.(SEE OUTPUT FOR BIT SETTINGS)        Y02669* 03820002
*                                                                     * 03900002
         EJECT                                                          03910041
*                                                                     * 03920041
*  NOTES                                                        Y02669* 03950002
*                                                                     * 04000002
*  CHARACTER/                                                   Y02669* 04050002
*  CODE                                                         Y02669* 04100002
*  DEPENDENCIES =  ANY CHARACTER CODE OTHER THAN EBCDIC WILL    Y02669* 04150002
*                  REQUIRE REASSEMBLY OF THIS MODULE.           Y02669* 04200002
*                                                                     * 04250002
*  DEPENDENCIES =  IEE0403D TO RECOGNIZE THE DISPLAY AND        Y02669* 04300002
*                  TRACK COMMANDS AND PASS THE OPERAND          Y02669* 04350002
*                  INFORMATION IN THE XSA.                      Y02669* 04400002
*                                                                     * 04450002
*                  SVC 100 WILL REJECT AS INVALID D A AND       Y02669* 04500002
*                  THE TRACK COMMAND FROM ANY TIME SHARING      Y02669* 04550002
*                  TERMINAL INCLUDING THOSE IN OPERATOR MODE.   Y02669* 04600002
*                                                                     * 04650002
*                  ISTCFF3D WILL VALIDITY CHECK FOR NET,.       Y02674* 04700002
*                                                                     * 04920002
*                  MP WILL BE SUPPORTED AS A STANDARD           Y02651* 04930002
*                  AND D M (MATRIX) WILL BE INCLUDED.           Y02651* 04940002
*                                                                     * 04950002
*                  THE MSGRT COMMAND WILL PROVIDE SUPPORT FOR   Y02669* 05000002
*                  A ROUTING DEFAULT FOR D TS; JOBS; A; R;      Y02669* 05050002
*                  AND TR TS; JOBS; AND A.                      Y02669* 05100002
*                                                                     * 05110002
*                  JOBS AND J AND JOBS,LIST AND J,L WILL BE     Y02669* 05120002
*                  SUPPORTED. L CAN SUBSTITUTE FOR 'LIST'.      Y02669* 05130002
*                                                                     * 05150002
*                  A SU TABLE (IHASUBIT) REFLECTING THE SU'S  @G17SPTJ* 05160041
*                  INSTALLED ON THIS SYSTEM.                  @G17SPTJ* 05170041
*                                                                     * 05180041
*  RESTRICTIONS =  TIME SHARING TERMINALS INCLUDING THOSE IN    Y02669* 05200002
*                  OPERATOR MODE ARE NOT PERMITTED TO ISSUE D A.Y02669* 05250002
*                                                                     * 05260002
*                  THE 'L' OPERAND FROM A TIME SHARING USER     Y02669* 05300002
*                  ON D C,K; CONSOLES(C); JOBS; M; TS; R; U     YM1916* 05350002
*                  WILL BE DETECTED AS INVALID IN IEE7503D.     Y02669* 05400002
*                                                                     * 05450002
*                  NO TIME SHARING TERMINAL IS PERMITTED TO     Y02669* 05500002
*                  ISSUE THE TRACK COMMAND.                     Y02669* 05550002
*                                                                     * 05600002
*  REGISTER/                                                    Y02669* 05650002
*  CONVENTIONS  =  NONE                                         Y02669* 05700002
*                                                                     * 05750002
*  PATCH-LABEL  =  PATCH                                        Y02669* 05800002
*                                                                     * 05850002
* MODULE TYPE   =  CSECT                                        Y02669* 05900002
*                                                                     * 05950002
*  PROCESSOR    =  N/A                                          Y02669* 05960002
*                                                                     * 05970002
*  MODULE SIZE  =  See assembly listing.                          @L01* 05980002
*                                                                     * 05982002
*                                                                     * 05983241
*   ATTRIBUTES  =  REENTRANT, SUPERVISOR MODE, KEY ZERO,        Y02669* 05984002
*                  PAGED LPA                                    Y02669* 05986002
*                                                                     * 05988002
*  ENTRY POINT  =  IEE3503D FROM IEE0403D                       Y02669* 05988402
*  PURPOSE      =  TO ROUTE THE DISPLAY OR TRACK                Y02669* 05988802
*                  COMMAND TO THE PROPER PROCESSOR.             Y02669* 05989202
*  LINKAGE      =  BRANCH                                       Y02669* 05989602
*  INPUT DATA   =  REGISTER 2 POINTS TO XSA                     Y02669* 05989702
*                    XAL POINTS TO 1ST OPERAND IN BUFFER        Y02669* 05989802
*                    XAU IS 1-BYTE UCMI (X'00' COMMAND ISSUED   Y02669* 05989902
*                        INTERNALLY OR FROM THE INPUT STREAM)   Y02669* 05990302
*                    XAA CONTAINS THE ASID (X'0000' = NON-TS)   Y02669* 05990602
*                    XAN CONTAINS THE VERB CODE X'68' - DISPLAY Y02669* 06010002
*                                               X'C4' - TRACK   Y02669* 06012002
*                    XAR POINTS TO THE LENGTH FIELD AT THE      Y02669* 06014002
*                        BEGINNING OF THE COMMAND BUFFER        Y02669* 06016002
*                    XAV CONTAINS THE COMMAND VERB              Y02669* 06018002
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06020002
*                                                                     * 06022002
*   REGISTERS                                                   Y02669* 06024002
*    SAVED      =  NONE  (REGISTER 14 REMAINS UNTOUCHED)        Y02669* 06026002
*                                                                     * 06028002
*    CONTENTS DURING                                            Y02669* 06032002
*     PROCESSING=  R0   WORK REGISTER                           Y02669* 06034002
*                  R1   WORK REGISTER                           Y02669* 06036002
*                  R2   INPUT REGISTER - POINTER TO XSA         Y02669* 06038002
*                  R3   MODULE BASE REGISTER                    Y02669* 06040002
*                  R4   USED TO CONTAIN UCMI FOR WTO            Y02669* 06042002
*                  R5   PTR TO COMMAND BUFFER AND               Y02669* 06044002
*                       CONTAINS ASID FOR TPUT                  Y02669* 06046002
*                  R6   USED TO ADDRESS COMPARE TABLE ENTRIES   Y02669* 06048002
*                        AND THE SU BIT STRING                @G17SPTJ* 06049041
*                  R7   UNUSED                                  Y02669* 06050002
*                  R8   UNUSED                                  Y02669* 06052002
*                  R9   WORK REGISTER                           Y02669* 06054002
*                  R10  CONTAINS SU NUMBER                    @G17SPTJ* 06055041
*                  R11  ADDRESSES END OF COMPARE TABLE          Y02669* 06058002
*                  R12  UNUSED                                  Y02669* 06060002
*                  R13  UNUSED                                  Y02669* 06062002
*                  R14  RETURN ADDRESS                          Y02669* 06064002
*                  R15  USED FOR BAL, RC FROM TPUT, AND BR EXIT Y02669* 06066002
*                                                                     * 06068002
*    RESTORED    = NONE                                         Y02669* 06070002
*                                                                     * 06072002
      EJECT                                                             06073041
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06074002
*                                                                     * 06076002
** EXIT - NORMAL = RETURN VIA BR R14 FROM DDTIM                 Y02669* 06078002
*   CONDITIONS   = FOR D T                                      Y02669* 06080002
*   OUTPUT DATA  = IEE136I time display message issued via WTO    @L01* 06081002
*   RETURN CODES = NONE                                         Y02669* 06084002
*                                                                     * 06086002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06088002
*                                                                     * 06090002
** EXIT - NORMAL = RETURN VIA BR R14 FROM XTPUT                 Y02669* 06092002
*   CONDITIONS   = FOR D T                                      Y02669* 06094002
*   OUTPUT DATA  = IEE136I time display message issued via TPUT   @L01* 06095002
*   RETURN CODES = NONE                                         Y02669* 06098002
*                                                                     * 06100002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06102002
*                                                                     * 06104002
** EXIT - NORMAL = BRANCH TO IEE0803D FROM DDPFK                Y02669* 06106002
*   CONDITIONS   = TO CREATE A CSCB FOR D PFK                   Y02669* 06108002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06110002
*                    XAN NOW CONTAINS X'36'                     Y02669* 06112002
*                    XAS CLEARED (WILL CONTAIN ZEROES)          Y02669* 06112402
*                        CONTAINS VALID INPUT TO IEE0803D.      Y02669* 06112502
*                        FOR ALL OTHER DISPLAY FUNCTIONS        Y02669* 06112602
*                        THIS FIELD IS FILLED IN IN IEE7503D    Y02699* 06112702
*                    XAX CLEARED (WILL CONTAIN ZEROES)          Y02669* 06112802
*   RETURN CODES = NONE                                         Y02669* 06114002
*                                                                     * 06116002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06134002
*                                                                     * 06136002
** EXIT - NORMAL = BRANCH TO IEE7503D FROM DD7503D              Y02669* 06138002
* 1 CONDITIONS   = TO VALIDITY CHECK:                           Y02669* 06140002
*                    L=CCA,LIST FOR  D A,  AND A                Y02669* 06142002
*                                      TS,  AND TS              Y02669* 06144002
*                                      JOBS, AND JOBS (J, & J)  Y02669* 06146002
*                                    TR A, AND A                Y02669* 06148002
*                                       TS, AND TS              Y02669* 06150002
*                                       JOBS, & JOBS (J, & J)   Y02669* 06152002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06154002
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06156002
*                    XAN STILL CONTAINS X'68' FOR DISPLAY       Y02669* 06158002
*                                       X'C4' FOR TRACK         Y02669* 06160002
*                    XAXMASK WILL CONTAIN AN INDICATOR          Y02669* 06162002
*                      SPECIFYING THE DESIRED DISPLAY OR        Y02669* 06164002
*                      TRACK FUNCTION:                          Y02669* 06166002
*                      X'80' FOR JOBS                           Y02669* 06168002
*                      X'08' FOR TS                             Y02669* 06170002
*                      X'88' FOR A                              Y02669* 06172002
*   RETURN CODES = NONE                                         Y02669* 06174002
* 2 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D U AND U,       Y02669* 06176002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06178002
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06180002
*                    XAN NOW CONTAINS X'52'                     Y02669* 06182002
*                    XAXMASK WILL CONTAIN ZEROES                Y02669* 06184002
*   RETURN CODES = NONE                                         Y02669* 06186002
* 3 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D C,K; C,K,;     Y02669* 06188002
*                    CONSOLES(C); AND CONSOLES,(C,)             YM1916* 06190002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06192002
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06194002
*                    XAN NOW CONTAINS X'2C' FOR C,K             Y02669* 06196002
*                                     X'3E' FOR CONSOLES(C)     YM1916* 06198002
*                    XAXMASK WILL CONTAIN ZEROES                Y02669* 06200002
*   RETURN CODES = NONE                                         Y02669* 06202002
* 4 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D M; M=; AND M,  Y02651* 06204002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02651* 06206002
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06208002
*                    XAN NOW CONTAINS X'32'                     Y02651* 06210002
*                    XAXMASK WILL CONTAIN ZEROES                Y02651* 06212002
*   RETURN CODES = NONE                                         Y02651* 06214002
* 5 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D R AND R,       Y02669* 06214402
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06214802
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1   Y02669* 06214902
*                    XAN NOW CONTAINS X'42'                     Y02651* 06215002
*                    XAXMASK WILL CONTAIN ZEROES                Y02651* 06215102
*   RETURN CODES = NONE                                         Y02669* 06215202
* 6 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D 3850 AND     @Z40LPTA* 06216041
*                    3850,                                    @Z40LPTA* 06216241
*   OUTPUT DATA  = R2 POINTS TO XSA                           @Z40LPTA* 06216441
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1 @Z40LPTA* 06216641
*                    XAN NOW CONTAINS X'F3'                   @Z40LPTA* 06216841
*                    XAXMASK WILL CONTAIN ZEROES              @Z40LPTA* 06217041
*   RETURN CODES = NONE                                       @Z40LPTA* 06217241
* 7 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D DMN; DMN=;   @Z40BPSV* 06218041
*                    DMN,                                     @Z40BPSV* 06218141
*   OUTPUT DATA  = R2 POINTS TO XSA                           @Z40BPSV* 06218241
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1 @Z40BPSV* 06218341
*                    XAN NOW CONTAINS X'88'                   @Z40BPSV* 06218441
*                    XAXMASK WILL CONTAIN ZEROES              @Z40BPSV* 06218541
*   RETURN CODES = NONE                                       @Z40BPSV* 06218641
* 8 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D SLIP; SLIP=; @G17SPTJ* 06218741
*                    SLIP,                                    @G17SPTJ* 06218841
*   OUTPUT DATA  = R2 POINTS TO XSA                           @G17SPTJ* 06218941
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1 @G17SPTJ* 06219041
*                    XAN NOW CONTAINS X'A8'                   @G17SPTJ* 06219141
*                    XAXMASK WILL CONTAIN ZEROES              @G17SPTJ* 06219241
*   RETURN CODES = NONE                                       @Z40BPSV* 06219341
* 9 CONDITIONS   = TO VALIDITY CHECK L=CCA FOR D DUMP AND     @G33CPMR* 06219443
*                    D DUMP,                                  @G33CPMR* 06219743
*   OUTPUT DATA  = R2 POINTS TO XSA                           @G33CPMR* 06220043
*                    XAP CONTAINS ADDRESS OF END OF BUFFER +1 @G33CPMR* 06220343
*                    XAN NOW CONTAINS X'B0'                   @G33CPMR* 06220643
*                    XAXMASK WILL CONTAIN ZEROES              @G33CPMR* 06220943
*   RETURN CODES = NONE                                       @G33CPMR* 06221243
*                                                                     * 06221943
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06222343
*                                                                     * 06222743
** EXIT - NORMAL = BRANCH TO IED1303D FROM DDTP                 Y02669* 06223643
*   CONDITIONS   = FOR D TP,                                    Y02669* 06224002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02669* 06226002
*   RETURN CODES = NONE                                         Y02669* 06228002
*                                                                     * 06230002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06234402
*                                                                     * 06236002
** EXIT - NORMAL = BRANCH TO ISTCFF3D FROM DDNET                Y02674* 06238002
*   CONDITIONS   = FOR D NET,                                   Y02674* 06240002
*   OUTPUT DATA  = R2 POINTS TO XSA                             Y02674* 06242002
*   RETURN CODES = NONE                                         Y02674* 06244002
*                                                                     * 06246002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06248002
*                                                                     * 06253040
** EXIT - ERROR  = RETURN VIA BR R14 FROM XTPUT                 Y02669* 06253840
*   CONDITIONS   = FOR D T; TPUT FAILED DUE TO OUTSTANDING TPUT Y02669* 06254002
*   OUTPUT DATA  = NONE                                         Y02669* 06256002
*   RETURN CODES = REG 15 CONTAINS X'00000014'                  Y02669* 06258002
*                                                                     * 06260002
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06262002
*                                                                     * 06264002
** EXIT - ERROR  = BRANCH TO IEE0503D FROM D0503D               Y02669* 06266002
*   CONDITIONS   = TO ISSUE MESSAGE 'IEE305I COMMAND INVALID',  Y02669* 06268002
*                  IF AN INVALID DISPLAY OR TRACK OPERAND IS    Y02669* 06270002
*                  FOUND.                                       Y02669* 06270402
*                  TO ISSUE MESSAGE 'IEE026I COMMAND NOT      @G17SPTJ* 06270841
*                  SUPPORTED' WHEN SYSTEM DOES NOT SUPPORT    @G17SPTJ* 06271241
*                  THE SU REQUIRED BY THE COMMAND.            @G17SPTJ* 06271641
*                  TO ISSUE MESSAGE 'IEE311I PARAMETER MISSING',Y02669* 06272002
*                  IF NO OPERAND IS FOUND IN THE COMMAND BUFFER.Y02669* 06274002
*                  TO ISSUE MESSAGE 'IEE341I CLOCK NOT ACTIVE', Y02669* 06274402
*                  IF THE TIME MACRO RETURNS AN ERROR CODE      Y02669* 06274802
*                  FOR DISPLAY TIME (D T).                      Y02669* 06275202
*                  TO ISSUE MESSAGE 'IEE341I TCAM NOT ACTIVE'  @ZA90245 06275300
*                  IF THE ADDRESS OF IED1303D COULD NOT BE     @ZA90245 06275400
*                  RESOLVED                                    @ZA90245 06275500
*   OUTPUT DATA  = ERROR CODES FOR MESSAGES ISSUED IN IEE0503D  Y02669* 06276002
*                  IN XSA POINTED TO BY REG 2                   Y02669* 06278002
*                    XAE WILL CONTAIN  X'05' FOR IEE305I        Y02669* 06280002
*                                      X'0B' FOR IEE311I        Y02669* 06282002
*                                      X'24' FOR IEE341I        Y02669* 06282402
*                    XAV STILL CONTAINS THE COMMAND VERB        Y02669* 06284002
*                        OR THE WORD 'CLOCK' FOR IEE341I        Y02669* 06286002
*                    XAU STILL CONTAINS THE CONSOLE ID          Y02669* 06286802
*                    XAA STILL CONTAINS ASID IF REQUESTER       Y02669* 06287202
*                        IS TIME SHARING USER ELSE X'0000'.     Y02669* 06287602
*   RETURN CODES = NONE                                         Y02669* 06287702
*                                                                     * 06288002
      EJECT                                                             06289041
*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  06290002
*  EXTERNAL                                                     Y02669* 06292002
*   REFERENCES                                                  Y02669* 06294002
*   ROUTINES     = BRANCHES TO   IEE0803D   IEE0503D            Y02669* 06296002
*                                IED1303D   IEE7503D            Y02669* 06298002
*                                ISTCFF3D                       Y02674* 06302002
*                                                                     * 06304002
*   DATA AREAS   = PSEUDO-SACB TO BE USED THROUGHOUT SVC 34     Y02669* 06306002
*                    ORG   XAX                                  Y02669* 06308002
*                  XAXMASK  DS    C       REQUEST FLAGS         Y02669* 06310002
*                  XAXJOBS  EQU   X'80'   JOBS      (J)         Y02669* 06312002
*                  XAXJOBSL EQU   X'40'   JOBS,LIST (J,L)       Y02669* 06314002
*                  XAXTS    EQU   X'08'   TS                    Y02669* 06316002
*                  XAXTSL   EQU   X'04'   TS,LIST   (TS,L)      Y02669* 06318002
*                  XAXA     EQU   X'88'   A                     Y02669* 06320002
*                  XAXAL    EQU   X'44'   A,LIST    (A,L)       Y02669* 06322002
*                  XAXFREE  DS    C       RESERVED              Y02669* 06324002
*                  XAXUTME  DS    H       UTME=NNN VALUE        Y02669* 06326002
*                  XAXSACB  DS    F       ADDRESS OF SACB       Y02669* 06328002
*                                                                     * 06332002
*   CONTROL                                                     Y02669* 06334002
*    BLOCKS      = IEEXSA     R - INPUT - COMMAND BUFFER ADDR   Y02669* 06336002
*                             W - OUTPUT - BITS SET             Y02669* 06338002
*                  IHASUBIT   R                               @G17SPTJ* 06338641
*                  CVT        R                               @G17SPTJ* 06339241
*                                                                     * 06340002
         EJECT                                                          06340402
*    TABLES      = A COMPARE TABLE IS USED TO  DETERMINE THE    Y02669* 06342002
*                  VALIDITY OF THE OPERANDS ON THE DISPLAY AND  Y02669* 06344002
*                  TRACK COMMANDS.                              Y02669* 06376702
*                  THE FORMAT OF EACH ENTRY IS THE SAME.        Y02669* 06384402
*                                                                     * 06392102
*                  DLEN   DC   AL1(L'XXX-1) - LENGTH OF         Y02669* 06399802
*                                             SPECIFIC OPERAND  Y02669* 06407502
*                  XXX    DC   C'YYYYY' - CHARACTER OPERAND     Y02669* 06415202
*                                         REPRESENTATION        Y02669* 06422902
*                                                                     * 06430602
*                  DLEN AND 'XXX' ARE USED IN AN EX OF A CLC.   Y02669* 06438302
*                  IF THE OPERANDS MATCH, A BRANCH IS TAKEN TO  Y02669* 06446002
*                  THE CODE WHICH IN TURN BRANCHES TO THE       Y02669* 06453702
*                  PROPER PROCESSOR OR HANDLES THE COMMAND.     Y02669* 06461402
*                  OTHERWISE, THE REGISTER USED TO ADDRESS THE  Y02669* 06469102
*                  TABLE IS INCREMENTED BY TWO + THE VALUE OF   Y02669* 06476802
*                  DLEN TO REACH THE NEXT TABLE ENTRY.          Y02669* 06484502
*                  THE VALID TRACK OPERANDS ARE PLACED AT THE   Y02669* 06492202
*                  END OF THE COMPARE TABLE AND ADDRESSING OF   Y02669* 06499902
*                  THE TABLE IS ADJUSTED WHEN VALIDITY          Y02669* 06507602
*                  CHECKING THE TRACK COMMAND.                  Y02669* 06515302
*                                                                     * 06523002
*                  A BRANCH TABLE IS USED IN CONNECTION WITH    Y02669* 06530702
*                  THE COMPARE TABLE. IT CONSISTS OF BRANCHES   Y02669* 06538402
*                  TO THE PROPER OPERAND PROCESSORS. AN INDEX   Y02669* 06546102
*                  REGISTER IS MAINTAINED THROUGHOUT THE SCAN   Y02669* 06553802
*                  OF THE COMPARE TABLE. THIS IS USED AT        Y02669* 06561502
*                  DBRANCH TO GIVE CONTROL TO THE INDICATED     Y02669* 06569202
*                  PROCESSOR VIA THE BRANCH TABLE.              Y02669* 06576902
*                                                                     * 06586902
*   MACROS       = MODID                                        Y02669* 06600002
*                  TIME                                         Y02669* 06610002
*                  WTO                                          Y02669* 06620002
*                  TPUT                                         Y02669* 06630002
*                                                                     * 06640002
*   ENQUEUE                                                     Y02669* 06650002
*     RESOURCES  = NONE                                         Y02669* 06660002
*                                                                     * 06664002
*   LOCKS HELD   = NONE                                         Y02669* 06670002
*                                                                     * 06680002
*   CHANGE LEVEL = Y02669                                       Y02669* 06700002
*                  Y02674                                       Y02674* 06750002
*                  Y02651                                       Y02651* 06800002
*                  YM1916                                       YM1916* 07800002
*                  Z40LPTA                                    @Z40LPTA* 07820041
*                  Z40BPSV                                    @Z40BPSV* 07850041
*                  G17SPTJ                                    @G17SPTJ* 07870041
*                  G33CPMR                                    @G33CPMR* 07920043
*                  ZA90245                                    @ZA90245* 08420000
*                                                                     * 09800019
         EJECT                                                          09820002
*   MESSAGES     = THE FOLLOWING MESSAGE IS CONTAINED IN AND    Y02669* 09850002
*                  ISSUED FROM THIS MODULE:                     Y02669* 09853002
*                                                                     * 09856002
*                   IEE136I LOCAL: TIME=hh.mm.ss DATE=yyyy.ddd    @L01* 09857002
*                           UTC: TIME=hh.mm.ss DATE=yyyy.ddd      @L01* 09858002
*                                                                     * 09862002
*                  THE FOLLOWING MESSAGES ARE CONTAINED IN      Y02669* 09865002
*                  AND ISSUED FROM IEE0503D:                    Y02669* 09868002
*                                                                     * 09871002
*                    MSGID    CODE    MESSAGE TEXT              Y02669* 09874002
*                    _____    ____    ____________              Y02669* 09877002
*                   IEE026I   X'1A'   NOT SUPPORTED           @G17SPTJ* 09878041
*                   IEE305I   X'05'   COMMAND INVALID           Y02669* 09880002
*                   IEE311I   X'11'   PARAMETER MISSING         Y02669* 09883002
*                   IEE341I   X'24'   'CLOCK' NOT ACTIVE        Y02669* 09886002
*                                     'TCAM'  NOT ACTIVE      @ZA90245* 09896002
*                                                                     * 09900002
*   SYSGEN       = INCLUDED WITH SVC 34 IN LOAD MODULE          Y02669* 09950002
*                  IGC0003D, SYSGEN MACRO SGIEF441,             Y02669* 09960002
*                  DLIB ASOB3, TARGLIB LPALIB.                  Y02669* 09970002
*                                                                     * 10000002
*   SYSTEM                                                      Y02669* 10050002
*    LIBRARIES   = NONE                                         Y02669* 10100002
*********************************************************************** 14000019
         EJECT                                                          16400019
IEE3503D CSECT                                                          16600019
         WXTRN IED1303D                                        @ZA90245 16618700
         SPACE                                                          16625000
*********************************************************************** 16637500
*                                                                     * 16647500
* Change activity      =                                              * 16657500
*                                                                     * 16667500
*   Flag  Date        By    Description                               * 16677500
*   ----  ----------  ----  ----------------------------------------  * 16687500
*   $L01  2020/04/10  KL    TMVS816 Add support to message        @L01* 16697500
*                           IEE136I for four-digit year and       @L01* 16707500
*                           UTC/local date/time display.          @L01* 16717500
* A - UZ45157 CHECK FOR IED1303D ADDRESS                       @ZA90245 16799600
*                                                                     * 16799700
*********************************************************************** 16800001
         EJECT                                                          16800002
*********************************************************************** 16800019
*                                                                     * 17000019
*                         REGISTER    EQUATES                         * 17200019
*                                                                     * 17400019
*********************************************************************** 17600019
R0       EQU   0                  WORK REGISTER                         17800019
R1       EQU   1                  WORK REGISTER                         18000019
R2       EQU   2                  INPUT - BASE REG FOR XSA              18200002
R3       EQU   3                  MODULE BASE REGISTER                  18400019
R4       EQU   4                  USED FOR UCMI                  Y02669 18600002
R5       EQU   5                  PTR TO OPERAND BUFFER AND ASID Y02669 18800002
R6       EQU   6                  USED TO ADDRESS TABLE ENTRIES         19000019
*                                  AND THE SU BIT STRING       @G17SPTJ 19050041
R7       EQU   7    (UNUSED)      UNUSED                         Y02669 19200002
R8       EQU   8    (UNUSED)      UNUSED                         Y02669 19400002
R9       EQU   9                  WORK REGISTER                         19600019
R10      EQU   10                 SU NUMBER STORAGE AREA       @G17SPTJ 19650041
R11      EQU   11                 ADDRESSES END OF TABLE                20000019
R12      EQU   12   (UNUSED)      UNUSED                         Y02669 20200002
R13      EQU   13   (UNUSED)      UNUSED                         Y02669 20400002
R14      EQU   14                 RETURN ADDRESS                        20600019
R15      EQU   15                 USED FOR BAL AND BRANCH EXIT          20800002
         SPACE                                                          20820041
*********************************************************************** 21000019
*                                                                     * 21200019
*                      DISPLACEMENTS AND LENGTHS                      * 21400019
*                                                                     * 21600019
*********************************************************************** 21800019
D0       EQU   0                  DISPLACEMENTS                         22000002
D1       EQU   1                                                        22200019
D2       EQU   2                                                        22400019
D3       EQU   3                                                        22600019
D4       EQU   4                                                        22650002
D5       EQU   5                                                        23000019
D6       EQU   6                                                        23200019
D7       EQU   7                                                        23400019
L0       EQU   0                  LENGTHS                        Y02669 23450002
L1       EQU   1                                                        23600019
L2       EQU   2                                                        23800019
L3       EQU   3                                                        24000019
L4       EQU   4                                                        24200019
L8       EQU   8                                                        24400019
         EJECT                                                          24420041
*********************************************************************** 24450002
*                                                                     * 24500002
*                           VERB CODES                           Y02669 24550002
*                                                                     * 24600002
*********************************************************************** 24650002
TR       EQU   X'C4'              VERB CODE FOR TRACK            Y02669 25050002
CK       EQU   X'2E'              VERB CODE FOR DISPLAY C,K      Y02669 25652002
DOMAIN   EQU   X'88'              VERB CODE FOR DISPLAY DMN    @Z40BPSV 25653040
MATRIX   EQU   X'32'              VERB CODE FOR DISPLAY MATRIX   Y02669 25656002
PFK      EQU   X'36'              VERB CODE FOR DISPLAY PFK      Y02669 25658002
CONSOLES EQU   X'3E'              VERB CODE FOR DISPLAY CONSOLES Y02669 25658402
REQUESTS EQU   X'42'              VERB CODE FOR DISPLAY REQUESTS Y02669 25660002
UNITS    EQU   X'52'              VERB CODE FOR DISPLAY UNITS    Y02669 25660402
D38      EQU   X'F3'              VERB CODE FOR DISPLAY 3850   @Z40LPTA 25661240
SLIP     EQU   X'A8'              VERB CODE FOR DISPLAY SLIP   @G17SPTJ 25661341
DUMP     EQU   X'B0'              VERB CODE FOR DISPLAY DUMP   @G33CPMR 25661443
         SPACE                                                          25662243
*********************************************************************** 25662343
*                                                                     * 25662443
*                           SU TEST BIT MASKS                  @G17SPTJ 25662543
*                                                                     * 25662643
*********************************************************************** 25662743
MASK7    EQU   7                  SU 7 BIT MASK                @G17SPTJ 25662843
MASK17   EQU   17                 SU 17 BIT MASK               @G17SPTJ 25662943
MASK33   EQU   33                 SU 33 BIT MASK               @G33CPMR 25663043
         SPACE                                                          25663643
*********************************************************************** 25663843
*                                                                     * 25664002
*                           MASKS                                Y02669 25666002
*                                                                     * 25668002
*********************************************************************** 25668402
FLAG     EQU   X'80'              FLAG - DENOTES TPUT RETRY       20030 25670002
XF0      EQU   X'F0'              MASK FOR OI                    Y02669 25680002
X0F      EQU   15                 MASK FOR STORING ALL BYTES     Y02669 25700002
*                                 IN A REGISTER                  Y02669 25702002
M7       EQU   7                  MASK FOR MOVING ADDRESS IN ICM Y02669 25712002
         SPACE                                                          25732041
*********************************************************************** 25752002
*                                                                     * 25802002
*                           ERROR CODES                          Y02669 25852002
*                                                                     * 25902002
*********************************************************************** 25952002
ERR305   EQU   X'05'              MASK FOR IEE305I-INVALID CMD   Y02669 26002002
ERR311   EQU   X'0B'              MASK FOR IEE311I-PARM MISSING  Y02669 26052002
ERR341   EQU   X'24'              MASK FOR IEE341I-NOT ACTIVE    Y02669 26062002
ERR026   EQU   X'1A'              MASK FOR IEE026I-NOT         @G17SPTJ 26072041
*                                                   SUPPORTED  @G17SPTJ 26077041
         EJECT                                                          26100019
*********************************************************************** 26150002
*                                                                     * 26152002
*            ESTABLISH ADDRESSABILITY FOR MODULE AND XSA         Y02669 26152402
*               GET POINTER TO OPERANDS FROM XAL                 Y02669 26152802
*  ERR - IEE311I PARAMETER MISSING - IF THERE ARE NO OPERANDS    Y02669 26153202
*                                                                     * 26154002
*********************************************************************** 26160002
         BALR  R3,0               ESTABLISH PROGRAM ADDRESSABILITY      26200019
         USING *,R3                                                     26300019
         B     PASTID             Branch around ID                 @L01 26310019
         DC    AL1(IDLEN)         Length of ID                     @L01 26320019
IDSTART  DC    C'IEE3503D'        Module name                      @L01 26330019
         DC    C' TMVS816'        Usermod                          @L01 26340019
         DC    C' $L01'           Level                            @L01 26350019
         DC    C' &SYSDATE'       Assembly date                    @L01 26360019
IDLEN    EQU   *-IDSTART          Symbolic length of ID            @L01 26370019
PASTID   DS    0H                 Instruction after ID             @L01 26380019
         USING IEEXSA,R2          ADDRESS EXTENDED SAVE AREA            26400019
         SR    R5,R5              CLEAR THE REGISTER             Y02669 26450002
         ICM   R5,M7,XAL+1        GET 3 BYTE ADDR OF OPERANDS    Y02669 26500002
         BZ    DERR311            NONE, ERR - PARAMETER MISSING  Y02669 27300002
         SPACE  2                                                       27350002
*********************************************************************** 27360002
*                                                                     * 27370002
*                 IS THIS A DISPLAY OR TRACK COMMAND?            Y02669 27380002
*                                                                     * 27390002
*********************************************************************** 27392002
         SPACE 2                                                        27394002
         SR    R9,R9              CLEAR REGISTER FOR OPERAND LENGTH     27400002
         SR    R15,R15            USED TO INDEX BRANCH TABLE            27500019
         XC    XAX(L8),XAX        CLEAR DISPLAY/TRACK FIELDS     Y02669 27550002
         LA    R11,DENTAB         CALCULATE COMPARE TABLE END ADDRESS   27562002
         CLI   XAN,TR             IS THIS A TRACK COMMAND?       Y02669 27570002
         BNE   DISPLAY            NO, USE WHOLE COMPARE TABLE    Y02669 27580002
         EJECT                                                          27582002
*********************************************************************** 27582402
*                                                                     * 27582802
*    LOOP THROUGH COMPARE TABLE LOOKING FOR VALID TRACK OPERAND  Y02669 27583202
*                                                                     * 27583302
*********************************************************************** 27583402
         SPACE 2                                                        27583602
TRACK    EQU   *                                                 Y02669 27583702
         LA    R6,DTRTAB          OTHERWISE, USE ONLY VALID      Y02669 27583802
         USING DTRTAB,R6          TRACK COMPARES                 Y02669 27585102
TRLOOP   EQU   *                                                 Y02669 27587502
         IC    R9,DTRLEN          LENGTH OF COMPARE TABLE ENTRY  Y02669 27587902
         EX    R9,COMPARE         CHECK FOR MATCH                Y02669 27588302
         BE    TRTABLE(R15)       IF MATCH,PROCESS ACCORDINGLY   Y02669 27588702
         LA    R6,D2(R9,R6)       NO MATCH, GO TO NEXT ENTRY     Y02669 27589102
         LA    R15,D4(R15)        INCRE INDEX FOR BRANCH TABLE   Y02669 27589502
         CR    R6,R11             END OF COMPARE TABLE REACHED?  Y02669 27589902
         BL    TRLOOP             NO, GO BACK AND CHECK NEXT     Y02669 27590402
*                                 TABLE ENTRY                    Y02669 27590802
         DROP  R6                 NECESSARY TO RE-EST BASE REG   Y02669 27591202
         B     DERR305            YES, INVALID OPERAND-ERROR     Y02669 27591302
         SPACE 2                                                        27591402
*********************************************************************** 27591802
*                                                                     * 27591902
* LOOP THROUGH COMPARE TABLE LOOKING FOR VALID DISPLAY OPERAND   Y02669 27592002
*                                                                     * 27592302
*********************************************************************** 27592402
         SPACE 2                                                        27692402
DISPLAY  EQU   *                                                 Y02669 27694402
         LA    R6,DTAB            COMPARE TABLE BEGINNING ADDR   Y02669 27700002
         USING DTAB,R6            EST. ADDRESSABILITY TO TABLE          27800019
DLOOP    EQU   *                                                 Y02669 27850002
         IC    R9,DLEN            LENGTH OF COMPARE TABLE ENTRY         27900002
         EX    R9,COMPARE         CHECK FOR MATCH                Y02669 28000002
         BE    BRTABLE(R15)       IF MATCH, PROCESS ACCORDINGLY  Y02669 28100002
         LA    R6,D2(R9,R6)       NO MATCH, GO TO NEXT ENTRY            28200002
         LA    R15,D4(R15)        INCRE INDEX FOR BRANCH TABLE          28300019
         CR    R6,R11             END OF COMPARE TABLE REACHED?   M0467 28340002
         BL    DLOOP              NO, GO BACK AND CHECK NEXT      20030 28380020
*                                 TABLE ENTRY                     20030 28460002
         DROP  R6                 NECESSARY TO RE-EST BASE REG   Y02669 28510002
         B     DERR305            YES, INVALID OPERAND-ERROR     Y02669 28560002
         EJECT                                                          29550002
*********************************************************************** 29600019
*                                                                     * 29700019
*                       DISPLAY UNITS                              O111 29800002
*                                                                     * 29900019
*********************************************************************** 30000019
DDUNIT   EQU   *                                                 Y02669 30100002
         MVI   XAN,UNITS          CHANGE VERB CODE IN XSA        Y02669 30200002
         B     DD7503D            BRANCH TO IEE7503D             Y02669 30200402
         SPACE  5                                                       30200602
*********************************************************************** 30200702
*                                                                     * 30204002
*                DISPLAY CONFIGURATION MATRIX                    Y02651 30206002
*                                                                     * 30206402
*********************************************************************** 30206802
DDMATRIX EQU   *                                                 Y02651 30207202
         MVI   XAN,MATRIX         CHANGE VERB CODE IN XSA        Y02651 30209902
         B     DD7503D            BRANCH TO IEE7503D             Y02651 30212702
         SPACE   5                                                      30215402
*********************************************************************** 30218102
*                                                                     * 30220802
*            TRACK OR DISPLAY JOBS  (J)                          Y02669 30230802
*                             TS                                 Y02669 30234802
*                             A                                  Y02669 30235202
*                                                                     * 30235602
*********************************************************************** 30235702
DDACTIVE EQU   *                                                 Y02669 30235802
         OI    XAXMASK,XAXJOBS+XAXTS  INDICATE ACTIVE OPTION     Y02669 30235902
         B     DD7503D             BRANCH TO IEE7503D            Y02669 30240702
DDJOBS   EQU   *                                                 Y02669 30247002
         OI    XAXMASK,XAXJOBS     INDICATE JOBS OPTION          Y02669 30249002
         B     DD7503D             BRANCH TO IEE7503D            Y02669 30249402
DDTS     EQU   *                                                 Y02669 30249802
         OI    XAXMASK,XAXTS       INDICATE TS OPTION            Y02669 30249902
         B     DD7503D             BRANCH TO IEE7503D            Y02669 30251902
         EJECT                                                          30253202
*********************************************************************** 30263202
*                                                                     * 30265202
*                       DISPLAY CONSOLES (C)                     YM1916 30266702
*                                                                     * 30270002
*********************************************************************** 30280002
DDCONS   EQU   *                                                 Y02669 30330002
         MVI   XAN,CONSOLES       CHANGE VERB CODE IN XSA        Y02669 30380002
         B     DD7503D            BRANCH TO IEE7503D             Y02669 30430002
         SPACE   5                                                      30990802
*********************************************************************** 32900019
*                                                                     * 33000019
*                      DISPLAY REQUESTS                          Y02669 33100002
*                                                                     * 33200019
*********************************************************************** 33300019
DDREQ    EQU   *                                                 Y02669 33400002
         MVI   XAN,REQUESTS       CHANGE VERB CODE IN XSA        Y02669 33450002
         B     DD7503D            BRANCH TO IEE7503D             Y02669 33460002
         SPACE   5                                                      33510002
*********************************************************************** 33510240
*                                                                     * 33510340
*                       DISPLAY 3850                           @Z40LPTA 33510440
*                                                                     * 33510640
*********************************************************************** 33511040
DD3850   EQU   *                                               @Z40LPTA 33511240
         MVI   XAN,D38          RE-SET VERB INDICATOR          @Z40LPTA 33511440
         B     DD7503D          BRANCH TO IEE7503D             @Z40LPTA 33511640
         EJECT                                                          33511941
*********************************************************************** 33512641
*                                                                     * 33512941
*                       DISPLAY DOMAIN                         @Z40BPSV 33513241
*                                                                     * 33513540
*********************************************************************** 33513740
DDDOMAIN EQU   *                                               @Z40BPSV 33513840
         MVI   XAN,DOMAIN       CHANGE VERB CODE IN XSA        @Z40BPSV 33513940
         LA    R10,MASK7        SET SU NUMBER FOR TESTING      @G17SPTJ 33514041
         B     TESTSU           BRANCH TO TEST SU NUMBER       @G17SPTJ 33514341
         SPACE 5                                                        33514441
*********************************************************************** 33514541
*                                                                     * 33514841
*                       DISPLAY SLIP                           @G17SPTJ 33515541
*                                                                     * 33515741
*********************************************************************** 33515941
DDSLIP   EQU   *                                               @G17SPTJ 33516141
         MVI   XAN,SLIP         CHANGE VERB CODE IN XSA        @G17SPTJ 33516341
         LA    R10,MASK17       SET SU NUMBER FOR TESTING      @G17SPTJ 33516441
         B     TESTSU           BRANCH TO TEST SU NUMBER       @G17SPTJ 33516741
         SPACE 5                                                        33516843
*********************************************************************** 33516943
*                                                                     * 33517043
*                       DISPLAY DUMP                           @G33CPMR 33517143
*                                                                     * 33517543
*********************************************************************** 33517643
DDDUMP   EQU   *                                               @G33CPMR 33517743
         MVI   XAN,DUMP         CHANGE VERB CODE IN XSA        @G33CPMR 33517843
         LA    R10,MASK33       SET SU NUMBER FOR TESTING      @G33CPMR 33517943
         B     TESTSU           BRANCH TO TEST SU NUMBER       @G33CPMR 33518043
         SPACE 5                                                        33518543
*********************************************************************** 33518943
*                                                                     * 33519043
*                       DISPLAY C,K                              Y02669 33519143
*                                                                     * 33519243
*********************************************************************** 33519543
DDCK     EQU   *                                                 Y02669 33519643
         MVI   XAN,CK             CHANGE VERB CODE IN XSA        Y02669 33519743
DD7503D  EQU   *                                                 Y02669 33519840
         L     R15,VCON7503       GET ADDRESS OF IEE7503D        Y02669 33539840
         BR    R15                BRANCH TO CHECK ROUTING INFO   Y02669 33590040
         EJECT                                                          33596040
*********************************************************************** 33610002
*                                                                     * 38450001
*                       DISPLAY PFK                               21002 38500002
*                                                                     * 38550001
*********************************************************************** 38600001
DDPFK    EQU   *                                                 Y02669 38650002
         MVI   XAN,PFK            CHANGE VERB CODE IN XSA        Y02669 38700002
         XC    XAS(L8),XAS        CLEAR DEFAULT ROUTING FIELD    Y02669 38710002
*                                 FOR ALL OTHER DISPLAY FUNCTIONSY02669 38720002
*                                 FIELD IS SET IN IEE7503D       Y02669 38730002
         L     R15,VCON0803       GET ADDR OF IEE0803D           Y02669 38750002
         BR    R15                BRANCH TO CSCB CREATION        Y02669 38800002
         SPACE   5                                                      38850002
*********************************************************************** 38900002
*                                                                     * 38950002
*                        DISPLAY TP,                              20002 39000002
*                                                                     * 39050002
*********************************************************************** 39100002
DDTP     EQU   *                                                 Y02669 39150002
         L     R15,VCON1303       GET ADDR OF IED1303D           Y02669 39200002
         LTR   R15,R15            ADDRESS ZERO?                @ZA90245 39216600
         BZ    TCAMERR            YES, GO TO ERROR MSG SETUP   @ZA90245 39233200
         BR    R15                BRANCH TO D TP MODULE          Y02669 39250002
         SPACE  5                                                       39300002
*********************************************************************** 39350002
*                                                                     * 39400002
*                       DISPLAY NET,                             Y02674 39450002
*                                                                     * 39500002
*********************************************************************** 39550002
DDNET    EQU   *                                                 Y02674 39600002
         L     R15,VCONCFF3       GET ADDR OF ISTCFF3D           Y02674 39650002
         BR    R15                BRANCH TO D NET MODULE         Y02674 39700002
         EJECT                                                          39750002
****************************************************************** @L01 39850002
*                                                                * @L01 39950002
*                      DISPLAY  TIME                             * @L01 40050002
*                                                                * @L01 40150002
*        Because of the primitive time interface in MVS 3.8,     * @L01 40250002
*        we must issue the TIME macro twice, once to get         * @L01 40350002
*        local time and again to get UTC.  It's possible         * @L01 40450002
*        that local/UTC time values that should end with         * @L01 40550002
*        the same second may not due to precision issues.        * @L01 40650002
*        If they don't match, we'll retry the local/UTC          * @L01 40750002
*        time calls up to 8192 times in an attempt to get        * @L01 40850002
*        matching local/UTC seconds values.  If we've            * @L01 40950002
*        tried the TIME calls 8192 times without getting         * @L01 41050002
*        a match, we'll give up and display what we did get.     * @L01 41150002
*                                                                * @L01 41250002
****************************************************************** @L01 41350002
DDTIME   EQU   *                                                 Y02669 55400002
         SR    R4,R4                 CLEAR FOR IC                       56000002
         IC    R4,XAU                SAVE UCMI FOR WTO                  56200002
         LH    R5,XAA                SAVE ASID FOR TPUT          Y02669 56300002
         MVC   M136(DDLN),DDMSG      MOVE IN INCOMPLETE WTO MSG    @L01 56310002
         SPACE 1                                                   @L01 56320002
*----------------------------------------------------------------* @L01 56330002
*        Initialize max loop counter in R6.  The counter is      * @L01 56340002
*        set to the number of times we'll repeat the             * @L01 56350002
*        LT/UTC loop to try to get both times with the           * @L01 56360002
*        same last digit of seconds.                             * @L01 56370002
*----------------------------------------------------------------* @L01 56380002
         LA    R6,2048             Set maximum number of loops     @L01 56390002
         SLL   R6,2                 to 8192                        @L01 56400002
         SPACE 1                                                   @L01 56410002
*----------------------------------------------------------------* @L01 56420002
*        Issue TIME twice, once to get local date and time,      * @L01 56430002
*        and again to get UTC date and time.                     * @L01 56440002
*----------------------------------------------------------------* @L01 56450002
DDTLOOP  DS    0H                  Loop here                       @L01 56460002
         TIME  DEC,ZONE=LT,        Get date and time from system   @L01+56470002
               ERRET=DERR341       Branch if TIME error            @L01 56480002
         ST    R0,XAWORKA          Save local time                 @L01 56490002
         ST    R1,XAWORKC          Save local date                 @L01 56500002
         TIME  DEC,ZONE=GMT,       Get date and time from system   @L01+56510002
               ERRET=DERR341       Branch if TIME error            @L01 56520002
         ST    R0,XAWORKE          Save UTC time                   @L01 56530002
         ST    R1,XAWORKG          Save UTC date                   @L01 56540002
         SPACE 1                                                   @L01 56550002
*----------------------------------------------------------------* @L01 56560002
*        Format local date and time into message.                * @L01 56570002
*----------------------------------------------------------------* @L01 56580002
         UNPK  XAWORKA(8),XAWORKA(4)  Unpack local time            @L01 56590002
         MVC   M136LHH,XAWORKA+1   Set local hours in WPL          @L01 56600002
         MVC   M136LMM,XAWORKA+3   Set local minutes in WPL        @L01 56610002
         MVC   M136LSS,XAWORKA+5   Set local seconds in WPL        @L01 56620002
         AP    XAWORKC(4),P1900    Adjust for century              @L01 56630002
         OI    XAWORKC+3,X'0F'     Force printable sign            @L01 56640002
         UNPK  XAWORKC(8),XAWORKC(4)  Unpack local date            @L01 56650002
         MVC   M136LYY,XAWORKC+1   Set local year in WPL           @L01 56660002
         MVC   M136LDDD,XAWORKC+5  Set local day in WPL            @L01 56670002
         SPACE 1                                                   @L01 56680002
*----------------------------------------------------------------* @L01 56690002
*        Format UTC date and time into message.                  * @L01 56700002
*----------------------------------------------------------------* @L01 56710002
         UNPK  XAWORKE(8),XAWORKE(4)  Unpack UTC time              @L01 56720002
         MVC   M136UHH,XAWORKE+1   Set UTC hours in WPL            @L01 56730002
         MVC   M136UMM,XAWORKE+3   Set UTC minutes in WPL          @L01 56740002
         MVC   M136USS,XAWORKE+5   Set UTC seconds in WPL          @L01 56750002
         AP    XAWORKG(4),P1900    Adjust for century              @L01 56760002
         OI    XAWORKG+3,X'0F'     Force printable sign            @L01 56770002
         UNPK  XAWORKG(8),XAWORKG(4)  Unpack UTC date              @L01 56780002
         MVC   M136UYY,XAWORKG+1   Set UTC year in WPL             @L01 56790002
         MVC   M136UDDD,XAWORKG+5  Set UTC day in WPL              @L01 56800002
         SPACE 1                                                   @L01 56810002
*----------------------------------------------------------------* @L01 56820002
*        Test if the last digit of local time seconds is         * @L01 56830002
*        the same as the last digit of the UTC time seconds.     * @L01 56840002
*        If not, loop back to try again.                         * @L01 56850002
*----------------------------------------------------------------* @L01 56860002
         CLC   M136USS+1,M136LSS+1 Last seconds digit match?       @L01 56870002
         BE    DDTDONE             Yes, go issue message           @L01 56880002
         BCT   R6,DDTLOOP          No, retry                       @L01 56890002
         SPACE 1                                                   @L01 56900002
DDTDONE  DS    0H                                                  @L01 56910002
         LTR   R5,R5                 TERMINAL REQUEST             20030 59460002
         BNZ   XTPUT                 YES, PUT TIME MSG TO TERM    20030 59520002
         LR    R0,R4                 PUT UCMI INTO REG 0                59600002
         WTO   MF=(E,M136)           Write IEE136I to operator     @L01 59700002
         BR    R14                   RETURN TO SYSTEM                   60000002
         EJECT                                                          60002002
*********************************************************************** 60004002
*                                                                     * 60006002
*      FOR TPUTS REGISTER 1 POINTS TO THE BEGINNING OF THE       Y02669 60008002
*      MESSAGE TEXT NOT TO THE PRECEDING WORD OF TEXT LENGTH     Y02669 60008102
*      AND MCS FLAGS.  THEREFORE THE HI ORDER BYTE OF THIS WORD  Y02669 60008202
*      IS USED AS THE RETRY INDICATOR. IT WILL BE SET TO X'80'   Y02669 60008302
*      THE FIRST TIME A RETRY IS ATTEMPTED AND CONTROL WILL BE   Y02669 60010602
*      RETURNED TO THE SYSTEM IF THE RETRY FAILS.                Y02669 60012602
*                                                                     * 60013102
*********************************************************************** 60015402
         SPACE 2                                                        60017402
XTPUT    EQU   *                                                  20030 60017702
         LH    R0,M136               Get WPL length                @L01 60027702
         SH    R0,FOUR                                            20030 60030020
         LA    R1,M136+4             Point to message text         @L01 60040020
         TPUT  (1),(0),TJID=(5)      PUT MSG TO TERMINAL          20030 60050002
         CH    R15,ERRCODE           CHECK TPUT RETURN CODE       20030 60060002
         BCR   D7,R14                OK, RETURN TO SYSTEM         20030 60070002
         SPACE  1                                                       60072002
         TM    M136,FLAG             Has a retry been attempted?   @L01 60082002
         BCR   D1,R14                YES, RETURN TO SYSTEM        20030 60090002
         OI    M136,FLAG             Set flag to show retry        @L01 60100002
         B     XTPUT                 RETRY TPUT                   20030 60110002
         EJECT                                                          60110141
*********************************************************************** 60110243
*                                                                     * 60111043
*     FOR SU-DEPENDENT COMMANDS, EXECUTION PASSES HERE TO             * 60113043
*     ASCERTAIN IF THE SYSTEM SUPPORTS THE COMMAND IN QUESTION.       * 60113843
*     THE TEST IS MADE AGAINST A FOUR WORD SU BIT MASK IN THE         * 60114643
*     NUCLEUS WHICH IS POINTED TO BY THE CVTIHASU FIELD.  ON          * 60115443
*     ENTRY TO THIS SUBROUTINE, REGISTER 10 CONTAINS THE SU           * 60116243
*     NUMBER TO BE USED AS THE TEST AGAINST THE BIT MASK FIELD.       * 60117043
*     IF THE SU IS INSTALLED, PROCESSING CONTINUES.  OTHERWISE,       * 60117843
*     AN IEE026I ERROR MESSAGE IS ISSUED.                      @G33CPMR 60118643
*                                                                     * 60119443
*********************************************************************** 60119543
         SPACE                                                          60119643
TESTSU   EQU   *                                               @G17SPTJ 60119743
         L     R6,CVTPTR          ESTABLISH CVT ADDRESSABILITY @G17SPTJ 60119843
         USING CVT,R6                                          @G17SPTJ 60119943
         L     R6,CVTIHASU        ESTABLISH SU ADDRESSABILITY  @G17SPTJ 60120043
         USING SUBITS,R6                                       @G17SPTJ 60120143
         C     R10,SIXFOUR        IS SU NUMBER BELOW 64?       @G17SPTJ 60120943
         BL    BELOW64            YES, BRANCH AROUND REDUCTION @G17SPTJ 60121943
         LM    R4,R5,SUBYTE9      OBTAIN 2ND 2 SU WORDS        @G33CPMR 60122943
         S     R10,SIXFOUR        REDUCE SU NUMBER BY 64       @G17SPTJ 60123943
         B     SHIFT              BRANCH AROUND NEXT INSTR.    @G17SPTJ 60124943
BELOW64  EQU   *                                               @G17SPTJ 60125943
         LM    R4,R5,SUBYTE1      OBTAIN 1ST 2 SU WORDS        @G33CPMR 60126943
SHIFT    EQU   *                                               @G17SPTJ 60130041
         LTR   R10,R10            IS IT SU 0 OR SU 64?         @G17SPTJ 60134041
         BZ    SHIFTED            YES, AVOID SHIFTING          @G17SPTJ 60148041
LOOP     EQU   *                                               @G17SPTJ 60157041
         SLDL  R4,D1              SHIFT ACCORDING TO SU NUMBER @G17SPTJ 60166041
         BCT   R10,LOOP           CONTINUE SHIFTING UNTIL ZERO @G17SPTJ 60175041
SHIFTED  EQU   *                                               @G17SPTJ 60184041
         LTR   R4,R4              IS SU INSTALLED ON SYSTEM?   @G17SPTJ 60193041
         BM    DD7503D            YES, BRANCH TO IEE7503D      @G17SPTJ 60202041
         B     DERR026            NO, SU NOT INSTALLED - ERROR @G33CPMR 60206043
         EJECT                                                          60229041
*********************************************************************** 60249902
*                                                                     * 60259902
*                         ERROR EXITS                            Y02669 60269902
*                                                                     * 60279902
*********************************************************************** 60289902
         SPACE 2                                                        60291902
DERR026  EQU   *                                               @G33CPMR 60293943
         MVI   XAE,ERR026         ERROR CODE = 26 - NO SUPPORT @G33CPMR 60294943
         B     D0503D             BRANCH TO IEE0503D           @G33CPMR 60295943
DERR311  EQU   *                                                 Y02669 60299902
         MVI   XAE,ERR311         ERROR CODE=11 FOR PARM MISSING Y02669 60400002
         B     D0503D             BRANCH TO IEE0503D             Y02669 60450002
DERR305  EQU   *                                                 Y02669 60500002
         MVI   XAE,ERR305         ERROR CODE=5 FOR INVALID COMD  Y02669 60550002
         B     D0503D             BRANCH TO IEE0503D             Y02669 60560002
TCAMERR  EQU   *                                               @ZA90245 60562000
         MVI   XAE,ERR341      ERROR CODE=X'24' FOR NOT ACTIVE @ZA90245 60564000
         MVC   XAV(L8),TCAM       GET KEYWORD 'TCAM'           @ZA90245 60566000
         B     D0503D             BRANCH TO IEE0503D           @ZA90245 60568000
DERR341  EQU   *                                                 Y02669 60570002
         MVI   XAE,ERR341         ERROR CODE=36 FOR NOT ACTIVE   Y02669 60580002
         MVC   XAV(L8),CLOCKERR   GET KEYWORD 'CLOCK'            Y02669 60590002
D0503D   EQU   *                                                 Y02669 60600002
         L     R15,VCON0503       GET ADDR OF IEE0503D           Y02669 60650002
         BR    R15                BRANCH TO MESSAGE MODULE       Y02669 60700002
         EJECT                                                          66450002
*********************************************************************** 66500002
*                                                                     * 66550002
*                         CONSTANTS                              Y02669 66560002
*                                                                     * 66570002
*********************************************************************** 66580002
         SPACE   5                                                      66590002
FOUR     DC    H'4'               USED IN SH INSTRUCTION         Y02669 66592002
SIXFOUR  DC    F'00000064'        COMPARAND OF 64              @G17SPTJ 66593041
ERRCODE  DC    X'0014'            TPUT RETURN CODE - MSG DID NOT  20030 66594002
*                                 GO DUE TO AN OUTSTANDING TPUT   20030 66596002
DDBLNK   DC    C'        '        USED FOR PADDING                      66598002
CLOCKERR DC    C'CLOCK   '        USED FOR D T ERROR MESSAGE     Y02669 66598402
TCAM     DC    C'TCAM    '        NAME FOR MSG IEE341I         @ZA90245 66598500
VCON0503 DC    V(IEE0503D)        ADDRESS OF MESSAGE MODULE      Y02669 66598802
VCON0803 DC    V(IEE0803D)        ADDRESS OF CSCB CREATION MOD   Y02669 66599202
VCON1303 DC    V(IED1303D)        ADDRESS OF D TP MODULE         Y02669 66599602
VCON7503 DC    V(IEE7503D)        ADDRESS OF SDS MODULE          Y02669 66599802
VCONCFF3 DC    V(ISTCFF3D)        ADDRESS OF NET MODULE          Y02674 66599902
         SPACE                                                          66649902
DDMSG    WTO   'IEE136I LOCAL: TIME=hh.mm.ss DATE=yyyy.ddd  UTC: TIME=h+66650902
               h.mm.ss DATE=yyyy.ddd',MF=L,                        @L01+66651902
               DESC=(5),MCSFLAG=(REG0,RESP)                        @L01 66652902
DDLN     EQU   *-DDMSG                                                  66679902
         SPACE                                                          66689902
COMPARE  CLC   D1(L0,R6),D0(R5)   LOOK FOR MATCH IN TABLE        Y02669 66691902
P1900    DC    P'1900000'         Adjustment factor for century    @L01 66692902
         SPACE   5                                                      66693902
*********************************************************************** 66695902
*                                                                     * 66697902
*                        PATCH AREA                              Y02669 66698302
*                                                                     * 66698702
*********************************************************************** 66699102
         SPACE   2                                                      66699341
PATCH    DC      CL50'*****PATCH AREA - IEE3503D******'          Y02669 66699602
         EJECT                                                          66699702
*********************************************************************** 66699902
*                                                                     * 66899902
*                         BRANCH  TABLE                          Y02669 67000002
*                                                                     * 67200019
*        THIS TABLE MUST BE IN A ONE-TO-ONE CORRESPONDENCE WITH  Y02669 67260002
*        THE 'COMPARE TABLE'                                     Y02669 67320002
*        THE BRANCH TABLE FOR THE TRACK COMMAND STARTS WITH      Y02669 67370002
*        TRTABLE                                                 Y02669 67380002
*                                                                     * 67390002
*********************************************************************** 67400019
         SPACE   5                                                      67450002
BRTABLE  DS    0H                                                Y02669 67600002
         B     DDTIME             BRANCH TO PROCESS D T          Y02669 68360002
         B     DDREQ              BRANCH TO PROCESS D R          Y02669 68400002
         B     DDREQ              BRANCH TO PROCESS D R,LIST     Y02669 68450002
         B     DDUNIT             BRANCH TO PROCESS D U          Y02669 68600002
         B     DDUNIT             BRANCH TO PROCESS D U,         Y02669 68700002
         B     DDMATRIX           BRANCH TO PROCESS D M          Y02651 68750002
         B     DDMATRIX           BRANCH TO PROCESS D M=         Y02651 68800002
         B     DDMATRIX           BRANCH TO PROCESS D M,         Y02651 68850002
         B     DDTP               BRANCH TO PROCESS DISPLAY TP,  Y02669 69500002
         B     DDNET              BRANCH TO PROCESS DISPLAY NET, Y02674 69520002
         B     DDCK               BRANCH TO PROCESS DISPLAY C,K  Y02669 69550002
         B     DDCK               BRANCH TO PROCESS DISPLAY C,K, Y02669 69600002
         B     DDCONS             BRANCH TO PROCESS D CONSOLES   YM1916 69630002
         B     DDCONS             BRANCH TO PROCESS D CONSOLES,  YM1916 69670002
         B     DDCONS             BRANCH TO PROCESS D C          YM1916 69710002
         B     DDCONS             BRANCH TO PROCESS D C,         YM1916 69750002
         B     DDPFK              BRANCH TO PROCESS D PFK        Y02669 69790002
         B     DD3850             BRANCH TO PROCESS D 3850     @Z40LPTA 69790140
         B     DD3850             BRANCH TO PROCESS D 3850,    @Z40LPTA 69790240
         B     DDDOMAIN           BRANCH TO PROCESS D DMN      @Z40BPSV 69790340
         B     DDDOMAIN           BRANCH TO PROCESS D DMN=     @Z40BPSV 69790540
         B     DDDOMAIN           BRANCH TO PROCESS D DMN,     @Z40BPSV 69791140
         B     DDSLIP             BRANCH TO PROCESS D SLIP     @G17SPTJ 69791641
         B     DDSLIP             BRANCH TO PROCESS D SLIP,    @G17SPTJ 69792141
         B     DDSLIP             BRANCH TO PROCESS D SLIP=    @G17SPTJ 69792641
         B     DDDUMP             BRANCH TO PROCESS D DUMP     @G33CPMR 69792843
         B     DDDUMP             BRANCH TO PROCESS D DUMP,    @G33CPMR 69792943
         B     DDDUMP             BRANCH TO PROCESS D D        @G33CPMR 69793043
         B     DDDUMP             BRANCH TO PROCESS D D,       @G33CPMR 69793143
TRTABLE  DS    0H                                                Y02669 69793340
         B     DDACTIVE           BRANCH TO PROCESS D OR TR A    Y02669 69795340
         B     DDACTIVE           BRANCH TO PROCESS D OR TR A,   Y02669 69802540
         B     DDJOBS             BRANCH TO PROCESS D OR TR J    Y02669 69804540
         B     DDJOBS             BRANCH TO PROCESS D OR TR J,   Y02669 69806540
         B     DDJOBS             BRANCH TO PROCESS D OR TR JOBS Y02669 69808502
         B     DDJOBS             BRANCH TO PROCESS D OR TR JOBS,Y02669 69818502
         B     DDTS               BRANCH TO PROCESS D OR TR TS   Y02669 69825202
         B     DDTS               BRANCH TO PROCESS D OR TR TS,  Y02669 69841902
         EJECT                                                          69891902
*********************************************************************** 69941902
*   NOTE:  THE FOLLOWING TABLE MUST BE THE LAST ITEM IN THIS     Y02669 69991902
*          MODULE SINCE A USING FOR THE TABLE IS ISSUED ON       Y02669 69993902
*          REGISTER 6 AND THUS CAN REESTABLISH THE MODULE'S      Y02669 69995902
*          BASE REGISTER FOR ANY CODE WHICH FOLLOWS THIS TABLE.  Y02669 69997902
*                                                                     * 70000019
*                        COMPARE TABLE                           Y02669 70200002
*                                                                     * 70400019
*           THIS TABLE MUST BE IN A ONE-TO-ONE CORRESPONDENCE    Y02669 70460002
*           WITH THE BRANCH TABLE                                Y02669 70520002
*           TRACK COMMANDS START WITH DTRTAB                     Y02669 70570002
*********************************************************************** 70600019
DTAB     EQU   *                                                        70800019
DLEN     EQU   *                                                        71000019
         DC    AL1(L'DTIME-1)     LENGTH OF DISPLAY OPERAND       20030 73920002
DTIME    DC    C'T '              DISPLAY OPERAND                 20030 73960002
         DC    AL1(L'DREQ-1)            *                        Y02669 74000002
DREQ     DC    C'R '                    *                        Y02669 74200002
         DC    AL1(L'DREQA-1)           *                        Y02669 74250002
DREQA    DC    C'R,'                    *                        Y02669 74300002
         DC    AL1(L'DUNIT-1)           *                        Y02669 74756002
DUNIT    DC    C'U '                    *                        Y02669 74760002
         DC    AL1(L'DUNITA-1)          *                        Y02669 74840002
DUNITA   DC    C'U,'                    *                        Y02669 74920002
         DC    AL1(L'DMATRIX-1)         *                        Y02651 74970002
DMATRIX  DC    C'M '                    *                        Y02651 75020002
         DC    AL1(L'DMATRIXA-1)        *                        Y02651 75070002
DMATRIXA DC    C'M='                    *                        Y02651 75120002
         DC    AL1(L'DMATRIXB-1)        *                        Y02651 75130002
DMATRIXB DC    C'M,'                    *                        Y02651 75132002
         DC    AL1(L'DTP-1)             *                        Y02669 75140002
DTP      DC    C'TP,'                   *                        Y02669 75160002
         DC    AL1(L'DNET-1)            *                        Y02674 75166002
DNET     DC    C'NET,'                  *                        Y02674 75168002
         DC    AL1(L'DCK-1)             *                         21002 75170002
DCK      DC    C'C,K '                  *                         21002 75180002
         DC    AL1(L'DCKA-1)            *                         21002 75190002
DCKA     DC    C'C,K,'                  *                         21002 75192002
         DC    AL1(L'DCONSOLE-1)        *                         20030 75200002
DCONSOLE DC    C'CONSOLES '             *                         20030 75208002
         DC    AL1(L'DCONSOLA-1)        *                         21002 75216002
DCONSOLA DC    C'CONSOLES,'             *                         21002 75224002
         DC    AL1(L'DC-1)              *                        YM1916 75232002
DC       DC    C'C '                    *                        YM1916 75241002
         DC    AL1(L'DCA-1)             *                        YM1916 75250002
DCA      DC    C'C,'                    *                        YM1916 75259002
         DC    AL1(L'DPFK-1)            *                        Y02669 75269902
DPFK     DC    C'PFK '                  *                        Y02669 75279902
         DC    AL1(L'D3850-1)           *                      @Z40LPTA 75280240
D3850    DC    C'3850 '                 *                      @Z40LPTA 75280440
         DC    AL1(L'D3850C-1)          *                      @Z40LPTA 75280840
D3850C   DC    C'3850,'                 *                      @Z40LPTA 75281140
         DC    AL1(L'DDOMAIN-1)         *                      @Z40BPSV 75281240
DDOMAIN  DC    C'DMN '                  *                      @Z40BPSV 75281340
         DC    AL1(L'DDOMAINE-1)        *                      @Z40BPSV 75282340
DDOMAINE DC    C'DMN='                  *                      @Z40BPSV 75282640
         DC    AL1(L'DDOMAINC-1)        *                      @Z40BPSV 75282940
DDOMAINC DC    C'DMN,'                  *                      @Z40BPSV 75283240
         DC    AL1(L'DSLIP-1)           *                      @G17SPTJ 75283441
DSLIP    DC    C'SLIP '                 *                      @G17SPTJ 75283741
         DC    AL1(L'DSLIPA-1)          *                      @G17SPTJ 75284241
DSLIPA   DC    C'SLIP,'                 *                      @G17SPTJ 75284741
         DC    AL1(L'DSLIPA-1)          *                      @G17SPTJ 75285241
DSLIPB   DC    C'SLIP='                 *                      @G17SPTJ 75285741
         DC    AL1(L'DDUMP-1)           *                      @G33CPMR 75286043
DDUMP    DC    C'DUMP '                 *                      @G33CPMR 75286243
         DC    AL1(L'DDUMPA-1)          *                      @G33CPMR 75286443
DDUMPA   DC    C'DUMP,'                 *                      @G33CPMR 75286643
         DC    AL1(L'DD-1)              *                      @G33CPMR 75286743
DD       DC    C'D '                    *                      @G33CPMR 75286843
         DC    AL1(L'DDA-1)             *                      @G33CPMR 75286943
DDA      DC    C'D,'                    *                      @G33CPMR 75287043
         EJECT                                                          75287441
DTRTAB   EQU   *                                                 Y02669 75287940
DTRLEN   EQU   *                                                 Y02669 75289902
         DC    AL1(L'DACTIVE-1)   LENGTH OF DISPLAY/TRACK OPERANDY02669 75293902
DACTIVE  DC    C'A '              DISPLAY/TRACK OPERAND          Y02669 75295902
         DC    AL1(L'DACTIVEA-1)        *                        Y02669 75297902
DACTIVEA DC    C'A,'                    *                        Y02669 75298302
         DC    AL1(L'DJ-1)              *                        Y02669 75299702
DJ       DC    C'J '                    *                        Y02669 75299802
         DC    AL1(L'DJA-1)             *                        Y02669 75316502
DJA      DC    C'J,'                    *                        Y02669 75326502
         DC    AL1(L'DJOBS-1)           *                        Y02669 75326902
DJOBS    DC    C'JOBS '                 *                        Y02669 75327302
         DC    AL1(L'DJOBSA-1)          *                        Y02669 75327702
DJOBSA   DC    C'JOBS,'                 *                        Y02669 75328102
         DC    AL1(L'DTS-1)             *                        Y02669 75328502
DTS      DC    C'TS '                   *                        Y02669 75330502
         DC    AL1(L'DTSA-1)            *                        Y02669 75332502
DTSA     DC    C'TS,'                   *                        Y02669 75332902
DENTAB   EQU   *                                                 Y02669 75333202
         EJECT                                                          75349902
IEEXSA   DSECT                                                          75599702
         IEEXSA                                                         75649702
XTM      EQU   XAT-D2                                            Y02669 75781602
XDATE    EQU   XAS                                               Y02669 75783602
         SPACE 1                                                   @L01 75783702
*----------------------------------------------------------------* @L01 75783802
*        Remap XSA area for building IEE136I message.            * @L01 75783902
*----------------------------------------------------------------* @L01 75784002
M136     EQU   XASAVLOC,DDLN       Area for IEE136I message        @L01 75784102
M136LTIM EQU   M136+4+20,8,C'C'    Local time                      @L01 75784202
M136LHH  EQU   M136+4+20,2,C'C'    -- Local hours                  @L01 75784302
M136LMM  EQU   M136+4+23,2,C'C'    -- Local minutes                @L01 75784402
M136LSS  EQU   M136+4+26,2,C'C'    -- Local seconds                @L01 75784502
M136LDAT EQU   M136+4+34,8,C'C'    Local date                      @L01 75784602
M136LYY  EQU   M136+4+34,4,C'C'    -- Local year                   @L01 75784702
M136LDDD EQU   M136+4+39,3,C'C'    -- Local day                    @L01 75784802
M136UTIM EQU   M136+4+54,8,C'C'    UTC time                        @L01 75784902
M136UHH  EQU   M136+4+54,2,C'C'    -- UTC hours                    @L01 75785002
M136UMM  EQU   M136+4+57,2,C'C'    -- UTC minutes                  @L01 75785102
M136USS  EQU   M136+4+60,2,C'C'    -- UTC seconds                  @L01 75785202
M136UDAT EQU   M136+4+68,8,C'C'    UTC date                        @L01 75785302
M136UYY  EQU   M136+4+68,4,C'C'    -- UTC year                     @L01 75785402
M136UDDD EQU   M136+4+73,3,C'C'    -- UTC day                      @L01 75785502
         EJECT                                                          75785641
         IHASUBIT                                                       75787641
         EJECT                                                          75789641
CVT      DSECT                                                          75791641
         CVT                                                            75793641
         END                                                            75799602
++ USERMOD(TMVS817)     /* REWORK(20200415) */             .
++ VER (Z038)
   FMID(FBB1221)                                           .
++ IF FMID(EBB1102) THEN REQ (TMVS816)
 /*
   PROBLEM DESCRIPTION(S):
     TMVS817 -
       Four-digit year support for IEAVRTOD IPL interface.

   COMPONENT: 5752-SC1CV-FBB1221

   APARS FIXED: TMVS817

   SPECIAL CONDITIONS:
     COPYRIGHT: (C) Copyright 2020 Kevin Leonard.  All rights reserved.

     ACTION:
      An IPL with CLPA is required after installation of this user
      modification.

    DOCUMENTATION:  Text of system message IEA886A modified.

      Publication:  OS/VS2 MVS System Messages
      Form Number:  GC38-1002

      Message IEA886A is modified as follows:

        IEA886A TOD CLOCK(S) MUST BE SET

          Explanation:  No time-of-day clock is in the set state.

          System Action:  The system waits for the operator to reply.

          Operator Response:  Enter REPLY xx,'prm', where prm may be
            as follows:

              DATE=yyyy.ddd[,CLOCK=hh.mm.ss][,UTC|GMT][,IPS=nn]

                yyyy    is the year 1900-2042.  Note that the year
                        may be specified as two digits 00-99.  If
                        a two-digit year yy is specified, the year
                        is assumed to be 19yy.

                ddd     is the day 001-366.

                hh      is the hour 00-23.

                mm      is the minute 00-59.

                ss      is the second 00-59.

                nn      is a two-character value which when appended
                        to IEAIPS specifies a member name.

            The bracketed parameters are optional.  If UTC or GMT
            is specified, the entered DATE and CLOCK values are
            understood to be Coordinated Universal Time (UTC) values,
            and are used to set the time-of-day (TOD) clock.  If UTC
            or GMT is omitted, they are understood to be local values,
            and will be converted by the system to a UTC value with
            which to set the TOD clock.

    DOCUMENTATION:  Text of system message IEA887A modified.

      Publication:  OS/VS2 MVS System Messages
      Form Number:  GC38-1002

      Message IEA887A is modified as follows:

        IEA887A CPU xx LOCAL DATE=yyyy.ddd,CLOCK=hh.mm.ss
        IEA887A TOD CLOCKS MUST BE SET, OR SELECT ADDRESS

          Explanation:  There are at least 2 set time-of-day clocks
            in the system which are not synchronized.

            The local date and clock values for each set clock
            are displayed. In the message text, yyyy specifies
            the year (1900-2042), ddd specifies the day (001-366),
            hh specifies the hour (00-23), mm specifies the minute
            (00-59), ss specifies the second (00-59), and xx specifies
            the CPU address (00-15).

         System Action:  The system waits for the operator to reply.

         Operator Response:  Reply as in message IEA886A, or enter
           REPLY xx,'ADDR=xx' where xx is the address of the CPU whose
           CLOCK and DATE values are displayed in the message. The IPS
           parameter is also acceptable in the latter reply.

           The 'ADDR=xx' response causes all time-of-day clocks in the
           system to be synchronized to the value in the clock of the
           selected CPU.

     DOCUMENTATION:  Text of system message IEA888A modified.

       Publication:  OS/VS2 MVS System Messages
       Form Number:  GC38-1002

       Message IEA888A is modified as follows:

         IEA888A UTC   DATE=yyyy.ddd,CLOCK=hh.mm.ss
         IEA888A LOCAL DATE=yyyy.ddd,CLOCK=hh.mm.ss
                 REPLY U, OR UTC/LOCAL TIME

           Explanation:  Either there is only one time-of-day clock in
             the system and it is set, or all set clocks are
             synchronized.

             Current values are displayed for the operator's
             verification.  Values displayed are local time and date,
             and Coordinated Universal Time (UTC) and date (also known
             as Greenwich Mean Time or GMT).  In the message text, yyyy
             specifies the year (1900-2042), ddd specifies the day
             (001-366), hh specifies the hour (00-23), mm specifies the
             minute (00-59), and ss specifies the second (00-59).

           System Action:  The system waits for the operator to reply.

           Operator Response:  If the values displayed are acceptable,
             enter:

               REPLY xx,'U'.

             If you wish to change the value of the time-of-day (TOD)
             clock, enter a new date, time, or both as follows:

               REPLY xx,'[DATE=yyyy.ddd][,CLOCK=hh.mm.ss],UTC|GMT'

             If you want to change the value of the local clock, enter a
             new date, time, or both as follows:

               REPLY xx,'[DATE=yyyy.ddd][,CLOCK=hh.mm.ss]'

             If the year is specified as a two-digit number yy, the
             year is assumed to be 19yy.

             Either UTC or GMT can be specified to indicate that the
             time-of-day clock is to be set.

             If you omit UTC or GMT, the system assumes that the local
             date and/or time is to be set.

             The IPS parameter can also be entered, although processing
             of the new value is delayed until initialization has
             proceeded far enough for the system to make the necessary
             changes.  You can enter the IPS parameter by itself, or in
             conjunction with the CLOCK, DATE, and UTC|GMT parameters.

             If the reply is anything except 'U', this message is
             repeated with the changed values displayed.

   COMMENTS:
     LAST CHANGE: 2020/04/15

     REWORK HISTORY:
      2020/04/15: Corrected documentation to ensure records aren't
        too long for the input deck.

      2020/04/10: Created.

     CROSS REFERENCE-MODULE/MACRO NAMES TO USERMODS
      IEAVRTOD  TMVS817

     CROSS REFERENCE-USERMODS TO MODULE/MACRO NAMES
      TMVS817   IEAVRTOD

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

     MODULES
      IEAVRTOD

     LISTEND
 */.
++ SRC      (IEAVRTOD) DISTLIB(ASAMPLIB) SYSLIB(SAMPLIB )
                       DISTMOD(AOSC5   ).
         TITLE 'TOD CLOCK MANAGER--MODULE IEAVRTOD'                     00001000
*                                                                  @L01 00001100
*  Change activity      =                                          @L01 00001200
*                                                                  @L01 00001300
*     Flag  Date        By    Description                          @L01 00001400
*     ----  ----------  ----  ----------------------------------   @L01 00001500
*     $L01  2020/04/10  KL    TMVS817 Four-digit year support.     @L01 00001600
*                                                                  @L01 00001700
IEAVRTOD CSECT ,                                                   0001 00002000
IEAVRINT DS    0H                                                  0001 00003000
         ENTRY IEAVRINT                                                 00004000
@MAINENT DS    0H                                                  0001 00005000
         USING *,@15                                               0001 00006000
         B     @PROLOG                                             0001 00007000
         DC    AL2(@EP00001-@MAINENT)                                   00008000
         DC    AL1(16)                                             0001 00009000
         DC    C'IEAVRTOD TMVS817'                                 @L01 00010000
IEAVRSSC DS    0H                                                  0001 00011000
         USING *,@15                                               0001 00012000
         B     @PROLOG                                             0001 00013000
         DC    AL2(@EP00253-IEAVRSSC)                                   00014000
         ENTRY IEAVRSSC                                                 00015000
IEAVRNOT DS    0H                                                  0001 00016000
         USING *,@15                                               0001 00017000
         B     @PROLOG                                             0001 00018000
         DC    AL2(@EP00829-IEAVRNOT)                                   00019000
         ENTRY IEAVRNOT                                                 00020000
IEAVRCAN DS    0H                                                  0001 00021000
         USING *,@15                                               0001 00022000
         B     @PROLOG                                             0001 00023000
         DC    AL2(@EP00901-IEAVRCAN)                                   00024000
         ENTRY IEAVRCAN                                                 00025000
         DROP  @15                                                      00026000
@PROLOG  STM   @14,@12,12(@13)                                     0001 00027000
         BALR  @09,0                                               0001 00028000
@PSTART  LA    @10,4095(,@09)                                      0001 00029000
         USING @PSTART,@09                                         0001 00030000
         USING @PSTART+4095,@10                                    0001 00031000
         L     @00,@SIZDATD                                        0001 00032000
         GETMAIN  R,LV=(0)                                              00033000
         LR    @08,@01                                             0001 00034000
         USING @DATD,@08                                           0001 00035000
         ST    @13,@SA00001+4                                      0001 00036000
         LM    @15,@01,16(@13)                                     0001 00037000
         ST    @08,8(,@13)                                         0001 00038000
         LR    @13,@08                                             0001 00039000
         AH    @15,4(,@15)                                         0001 00040000
         BR    @15                                                 0001 00041000
@EP00001 DS    0H                                                  0002 00042000
*                                                                  0134 00043000
*/********************************************************************/ 00044000
*/*                                                                  */ 00045000
*/* SAVE THE ADDRESS OF THE SAVEAREA, AND RE-BASE THE CVT FOR MORE   */ 00046000
*/* EFFICIENT CODE GENERATION.                                       */ 00047000
*/*                                                                  */ 00048000
*/********************************************************************/ 00049000
*                                                                  0135 00050000
*BEGTOD:                                                           0135 00051000
*   R13SAV=REG13;                                                  0135 00052000
BEGTOD   ST    REG13,R13SAV                                        0135 00053000
*   LCVTPTR=CVTPTR;                                                0136 00054000
         L     LCVTPTR,CVTPTR                                      0136 00055000
*   RESPECIFY                                                      0137 00056000
*     CVTMAP BASED(LCVTPTR);                                       0137 00057000
*                                                                  0137 00058000
*/********************************************************************/ 00059000
*/*                                                                  */ 00060000
*/* GET THE NUMBER OF CPU'S CURRENTLY ONLINE, AND CALCULATE THE      */ 00061000
*/* NUMBER OF BYTES REQUIRED FOR THE TOD CLOCK WORKAREA AND ONE TOD  */ 00062000
*/* CLOCK WORKAREA ENTRY PER CPU.  THEN, GET THE AMOUNT OF STORAGE   */ 00063000
*/* JUST CALCULATED FROM SUBPOOL 245.                                */ 00064000
*/*                                                                  */ 00065000
*/********************************************************************/ 00066000
*                                                                  0138 00067000
*   REG0=LENGTH(TCWA)+(LENGTH(TCENTRY)*CSDCPUOL);                  0138 00068000
         L     @07,CVTCSD(,LCVTPTR)                                0138 00069000
         LH    REG0,CSDCPUOL(,@07)                                 0138 00070000
         SLA   REG0,4                                              0138 00071000
         AH    REG0,@CH02606                                       0138 00072000
*   GENERATE CODE                                                  0139 00073000
*       (GETMAIN RU,LV=(0),SP=245      OBTAIN WORKAREA FROM SP 245);    00074000
*                                                                  0139 00075000
         GETMAIN RU,LV=(0),SP=245      OBTAIN WORKAREA FROM SP 245      00076000
*/********************************************************************/ 00077000
*/*                                                                  */ 00078000
*/* USE A COMPARE AND SWAP LOOP IN ORDER TO SERIALIZE MANIPULATION   */ 00079000
*/* OF THE TOD CLOCKS.  IF THE POINTER TO THE TOD CLOCK WORKAREA IS  */ 00080000
*/* 0, THEN THE CLOCKS ARE NOT CURRENTLY BEING MANIPULATED.          */ 00081000
*/*                                                                  */ 00082000
*/********************************************************************/ 00083000
*                                                                  0140 00084000
*LOOP1:                                                            0140 00085000
*   REG11=0;                        /* CLEAR REG 11 FOR COMPARE AND     00086000
*                                      SWAP                          */ 00087000
LOOP1    SLR   REG11,REG11                                         0140 00088000
*   CS(REG11,REG1,TPCTCWA);         /* TEST FOR CODE IN EXECUTION    */ 00089000
         L     @07,TPCPTR(,LCVTPTR)                                0141 00090000
         CS    REG11,@01,TPCTCWA(@07)                              0141 00091000
*   BC(7,LOOP1);                    /* BRANCH IF CODE IN EXECUTION   */ 00092000
         BC    7,LOOP1                                             0142 00093000
*/********************************************************************/ 00094000
*/*                                                                  */ 00095000
*/* SAVE THE IPL-TIME TIME ZONE CONSTANT IN THE TPC FOR POTENTIAL    */ 00096000
*/* LATER USE IN A SET COMMAND WITH THE RESET PARAMETER.             */ 00097000
*/*                                                                  */ 00098000
*/********************************************************************/ 00099000
*                                                                  0143 00100000
*   TPCTZORG=CVTTZ;                                                0143 00101000
*                                                                  0143 00102000
         L     @07,TPCPTR(,LCVTPTR)                                0143 00103000
         MVC   TPCTZORG(4,@07),CVTTZ(LCVTPTR)                      0143 00104000
*/********************************************************************/ 00105000
*/*                                                                  */ 00106000
*/* ESTABLISH ADDRESSABILITY FOR THE TOD CLOCK WORKAREA, SAVE THE    */ 00107000
*/* COUNT OF ONLINE CPU'S, CLEAR THE FLAGS IN THE WORKAREA, AND SAVE */ 00108000
*/* ITS LENGTH FOR THE FREEMAIN TO COME.                             */ 00109000
*/*                                                                  */ 00110000
*/********************************************************************/ 00111000
*                                                                  0144 00112000
*   TCWAPTR=REG1;                                                  0144 00113000
         LR    TCWAPTR,REG1                                        0144 00114000
*   RESPECIFY                                                      0145 00115000
*     TCWA BASED(TCWAPTR);                                         0145 00116000
*   TCCPUCNT=CSDCPUOL;                                             0146 00117000
         L     @07,CVTCSD(,LCVTPTR)                                0146 00118000
         LH    @06,CSDCPUOL(,@07)                                  0146 00119000
         STH   @06,TCCPUCNT(,TCWAPTR)                              0146 00120000
*   CSDGDTOD=CSDCPUOL;              /* COUNT OF GOOD TOD CLOCKS TO 0147 00121000
*                                      CSD                           */ 00122000
         ST    @06,CSDGDTOD(,@07)                                  0147 00123000
*   CSDGDCC=CSDCPUOL;               /* COUNT OF GOOD CLK COMP TO CSD */ 00124000
         ST    @06,CSDGDCC(,@07)                                   0148 00125000
*   CSDGDINT=CSDCPUOL;              /* COUNT OF GOOD CPU TIMERS TO 0149 00126000
*                                      CSD                           */ 00127000
         ST    @06,CSDGDINT(,@07)                                  0149 00128000
*   TCWAGFLG='00'X;                                                0150 00129000
         MVI   TCWAGFLG(TCWAPTR),X'00'                             0150 00130000
*   TCWALN=REG0;                                                   0151 00131000
*                                                                  0151 00132000
         STH   REG0,TCWALN                                         0151 00133000
*/********************************************************************/ 00134000
*/*                                                                  */ 00135000
*/* SET UP THE INTERFACE FOR GETTING A PAGE FIXED, AND ISSUE THE     */ 00136000
*/* PGFIX SVC TO FIX THIS MODULE.                                    */ 00137000
*/*                                                                  */ 00138000
*/********************************************************************/ 00139000
*                                                                  0152 00140000
*   REG1=ADDR(BEGTOD);              /* GET POINTER TO START OF MODULE*/ 00141000
         LA    REG1,BEGTOD                                         0152 00142000
*   REG15=ADDR(ENDTOD);             /* GET POINTER TO END OF MODULE  */ 00143000
         LA    REG15,ENDTOD                                        0153 00144000
*   REG0=ADDR(TODECB);              /* GET POINTER TO ECB            */ 00145000
         LA    REG0,TODECB                                         0154 00146000
*   TODECB=0;                       /* CLEAR ECB                     */ 00147000
         SLR   @07,@07                                             0155 00148000
         ST    @07,TODECB                                          0155 00149000
*   GENERATE CODE(PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N);           0156 00150000
         PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N                           00151000
*                                                                  0157 00152000
*/********************************************************************/ 00153000
*/*                                                                  */ 00154000
*/* THIS CODE INITIALIZES A WORKAREA ENTRY FOR EVERY CPU ONLINE.     */ 00155000
*/*                                                                  */ 00156000
*/********************************************************************/ 00157000
*                                                                  0157 00158000
*   PTREBEG=TCWAPTR+LENGTH(TCWA);   /* SAVE PTR TO FIRST TCWA ENTRY  */ 00159000
         LA    @07,112                                             0157 00160000
         ALR   @07,TCWAPTR                                         0157 00161000
         ST    @07,PTREBEG                                         0157 00162000
*   PTREEND=PTREBEG+TCCPUCNT*LENGTH(TCENTRY)-1;/* SAVE PTR TO END OF    00163000
*                                      TCWA ENTRIES                  */ 00164000
         LH    @06,TCCPUCNT(,TCWAPTR)                              0158 00165000
         SLA   @06,4                                               0158 00166000
         ALR   @06,@07                                             0158 00167000
         BCTR  @06,0                                               0158 00168000
         ST    @06,PTREEND                                         0158 00169000
*   REG12=CVTPCCAT+LENGTH(PCCAVT)-LENGTH(PCCAT00P);/* GET PTR TO LAST   00170000
*                                      PCCAVT ENTRY                  */ 00171000
         L     REG12,CVTPCCAT(,LCVTPTR)                            0159 00172000
         SH    REG12,@CH02877                                      0159 00173000
*   RESPECIFY                                                      0160 00174000
*     PCCAVT BASED(REG12);                                         0160 00175000
*   RESPECIFY                                                      0161 00176000
*     PCCA BASED(PTRPCCA);                                         0161 00177000
*   REG11=PTREBEG;                  /* GET PTR TO FIRST TCWA ENTRY   */ 00178000
         LR    REG11,@07                                           0162 00179000
*   RESPECIFY                                                      0163 00180000
*     TCENTRY BASED(REG11);                                        0163 00181000
*   MASKHLDR=CSDCPUAL;              /* GET MASK OF ALIVE CPU'S       */ 00182000
         L     @07,CVTCSD(,LCVTPTR)                                0164 00183000
         SLR   MASKHLDR,MASKHLDR                                   0164 00184000
         ICM   MASKHLDR,3,CSDCPUAL(@07)                            0164 00185000
*/********************************************************************/ 00186000
*/*                                                                  */ 00187000
*/* THIS LOOP DECREMENTS THE INDEX THROUGH THE MASK OF ONLINE CPU'S  */ 00188000
*/* AS LONG AS THE MASK BIT FOR THE CPU IS OFF, INDICATING THAT THE  */ 00189000
*/* CPU IS NOT ONLINE.                                               */ 00190000
*/*                                                                  */ 00191000
*/********************************************************************/ 00192000
*                                                                  0165 00193000
*   DO REG1=LENGTH(CSDCPUAL) TO 1 BY-1;                            0165 00194000
         LA    REG1,16                                             0165 00195000
@DL00165 DS    0H                                                  0166 00196000
*     IF MASKHLDR//2^=0             /* SEE IF THE NEXT BIT IS ON     */ 00197000
*       THEN                                                       0166 00198000
*                                                                  0166 00199000
         LR    @07,MASKHLDR                                        0166 00200000
         N     @07,@CF00096                                        0166 00201000
         LTR   @07,@07                                             0166 00202000
         BZ    @RF00166                                            0166 00203000
*/********************************************************************/ 00204000
*/*                                                                  */ 00205000
*/* WHEN A BIT IN THE MASK IS FOUND TO BE ON, INDICATING THAT THE CPU*/ 00206000
*/* IS ONLINE, FILL IN THE PCCA ADDRESS FIELD AND THE CPU ADDRESS    */ 00207000
*/* FIELD IN THE NEXT AVAILABLE TOD CLOCK WORKAREA ENTRY.            */ 00208000
*/*                                                                  */ 00209000
*/********************************************************************/ 00210000
*                                                                  0167 00211000
*       DO;                                                        0167 00212000
*         TCWAPCCA=PTRPCCA;                                        0168 00213000
         L     @07,PTRPCCA(,REG12)                                 0168 00214000
         ST    @07,TCWAPCCA(,REG11)                                0168 00215000
*         TCWAIADD=PCCACPUA;                                       0169 00216000
         MVC   TCWAIADD(2,REG11),PCCACPUA(@07)                     0169 00217000
*         REG11=REG11+LENGTH(TCENTRY);                             0170 00218000
         AH    REG11,@CH00045                                      0170 00219000
*       END;                                                       0171 00220000
*     MASKHLDR=MASKHLDR/2;          /* SHIFT OFF BIT JUST PROCESSED  */ 00221000
@RF00166 SRL   MASKHLDR,1                                          0172 00222000
*     REG12=REG12-LENGTH(PCCAT00P); /* POINT TO NEXT PCCAVT ENTRY    */ 00223000
         SH    REG12,@CH00108                                      0173 00224000
*   END;                                                           0174 00225000
*                                                                  0174 00226000
         BCTR  REG1,0                                              0174 00227000
         LTR   REG1,REG1                                           0174 00228000
         BP    @DL00165                                            0174 00229000
*/********************************************************************/ 00230000
*/*                                                                  */ 00231000
*/* AFTER INITIALIZING THE TOD CLOCK WORKAREA ENTRIES, WAIT FOR      */ 00232000
*/* COMPLETION OF THE PAGE FIXING OPERATION.                         */ 00233000
*/*                                                                  */ 00234000
*/********************************************************************/ 00235000
*                                                                  0175 00236000
*   REG1=ADDR(TODECB);                                             0175 00237000
         LA    REG1,TODECB                                         0175 00238000
*   GENERATE CODE(WAIT  1,ECB=(1));                                0176 00239000
*                                                                  0176 00240000
         WAIT  1,ECB=(1)                                                00241000
*/********************************************************************/ 00242000
*/*                                                                  */ 00243000
*/* INITIALIZE THE RETURN CODE SO THAT THE FOLLOWING LOOP WILL BE    */ 00244000
*/* EXECUTED AT LEAST ONE TIME.                                      */ 00245000
*/*                                                                  */ 00246000
*/********************************************************************/ 00247000
*                                                                  0177 00248000
*   RETCODE=4;                                                     0177 00249000
*                                                                  0177 00250000
         LA    RETCODE,4                                           0177 00251000
*/********************************************************************/ 00252000
*/*                                                                  */ 00253000
*/* STARTING WITH THE FIRST TOD CLOCK WORKAREA ENTRY, CALL           */ 00254000
*/* THE SYNCHRONIZATION TESTING ROUTINE TO TEST THE SYNCHRONIZATION  */ 00255000
*/* OF ALL OTHER CLOCKS AGAINST THIS ONE.  DO THIS AS LONG AS THERE  */ 00256000
*/* ARE ENTRIES TO PROCESS AND THE TESTING ROUTINE INDICATES THAT    */ 00257000
*/* EITHER THE SUBJECT CLOCK WAS NOT SET (RETURN CODE=4),            */ 00258000
*/* OR THAT THE TESTING PROCEDURE COULD NOT BE COMPLETED IN LESS     */ 00259000
*/* THAN 2**20 MICROSECONDS (RETURN CODE=16).                        */ 00260000
*/*                                                                  */ 00261000
*/********************************************************************/ 00262000
*                                                                  0178 00263000
*   REG11=PTREBEG;                  /* GET PTR TO FIRST TCWA ENTRY   */ 00264000
         L     REG11,PTREBEG                                       0178 00265000
*   DO REG1=TCCPUCNT TO 1 BY-1 WHILE RETCODE=4 RETCODE=16;         0179 00266000
*                                                                  0179 00267000
         LH    REG1,TCCPUCNT(,TCWAPTR)                             0179 00268000
         B     @DE00179                                            0179 00269000
@DL00179 CH    RETCODE,@CH00108                                    0179 00270000
         BE    @DB00179                                            0179 00271000
         CH    RETCODE,@CH00045                                    0179 00272000
         BNE   @DC00179                                            0179 00273000
@DB00179 DS    0H                                                  0180 00274000
*/********************************************************************/ 00275000
*/*                                                                  */ 00276000
*/* MAKE SURE THAT THE ENTRY FOR THE SUBJECT CLOCK IS THE            */ 00277000
*/* FIRST, BY INTERCHANGING THE FIRST AND SUBJECT ENTRIES.           */ 00278000
*/*                                                                  */ 00279000
*/********************************************************************/ 00280000
*                                                                  0180 00281000
*     TCWAWRK=PTREBEG->TCENTRY;     /* SAVE FIRST ENTRY              */ 00282000
         L     @07,PTREBEG                                         0180 00283000
         MVC   TCWAWRK(16,TCWAPTR),TCENTRY(@07)                    0180 00284000
*     PTREBEG->TCENTRY=TCENTRY;     /* NEXT ENTRY TO FIRST           */ 00285000
         MVC   TCENTRY(16,@07),TCENTRY(REG11)                      0181 00286000
*     TCENTRY=TCWAWRK;              /* FIRST ENTRY TO NEXT           */ 00287000
         MVC   TCENTRY(16,REG11),TCWAWRK(TCWAPTR)                  0182 00288000
*/********************************************************************/ 00289000
*/*                                                                  */ 00290000
*/* CALL THE SYNCHRONIZATION TESTING ROUTINE.                        */ 00291000
*/*                                                                  */ 00292000
*/********************************************************************/ 00293000
*                                                                  0183 00294000
*     CALL IEAVRTST;                                               0183 00295000
*                                                                  0183 00296000
         BAL   @14,IEAVRTST                                        0183 00297000
*/********************************************************************/ 00298000
*/*                                                                  */ 00299000
*/* TEST FOR A RETURN CODE OF 0.                                     */ 00300000
*/*                                                                  */ 00301000
*/********************************************************************/ 00302000
*                                                                  0184 00303000
*     IF RETCODE=0 THEN                                            0184 00304000
*                                                                  0184 00305000
         LTR   RETCODE,RETCODE                                     0184 00306000
         BNZ   @RF00184                                            0184 00307000
*/********************************************************************/ 00308000
*/*                                                                  */ 00309000
*/* IF THE RETURN CODE IS 0, INDICATING THAT ALL CLOCKS ARE IN       */ 00310000
*/* SYNCHRONISM OR THAT THE ONLY CLOCK IS SET, TURN ON THE FLAG WHICH*/ 00311000
*/* INDICATES THAT MESSAGE IEA888A SHOULD BE ISSUED.                 */ 00312000
*/*                                                                  */ 00313000
*/********************************************************************/ 00314000
*                                                                  0185 00315000
*       TCGXXX='1'B;                                               0185 00316000
         OI    TCGXXX(TCWAPTR),B'10000000'                         0185 00317000
*     ELSE                                                         0186 00318000
*                                                                  0186 00319000
*/********************************************************************/ 00320000
*/*                                                                  */ 00321000
*/* IF THE RETURN CODE WAS NOT 0, INDICATE THAT PROMPTING MUST       */ 00322000
*/* BE DONE, AND THEN TEST FOR A RETURN CODE OF 8 OR 12.             */ 00323000
*/*                                                                  */ 00324000
*/********************************************************************/ 00325000
*                                                                  0186 00326000
*       DO;                                                        0186 00327000
         B     @RC00184                                            0186 00328000
@RF00184 DS    0H                                                  0187 00329000
*         MSTODWTO='0'B;            /*                        @Y02675*/ 00330000
         L     @07,CVTMSER(,LCVTPTR)                               0187 00331000
         NI    MSTODWTO(@07),B'10111111'                           0187 00332000
*         IF RETCODE=8 RETCODE=12 THEN                             0188 00333000
*                                                                  0188 00334000
         CH    RETCODE,@CH00038                                    0188 00335000
         BE    @RT00188                                            0188 00336000
         CH    RETCODE,@CH00854                                    0188 00337000
         BNE   @RF00188                                            0188 00338000
@RT00188 DS    0H                                                  0189 00339000
*/********************************************************************/ 00340000
*/*                                                                  */ 00341000
*/* IF THE RETURN CODE WAS 8 OR 12, INDICATING THAT THE THE CLOCKS   */ 00342000
*/* WERE OUT OF LOW ORDER OR HIGH ORDER SYNCHRONIZATION RESPECTIVELY,*/ 00343000
*/* THEN LOOP THROUGH THE TOD CLOCK WORKAREA ENTRIES EXAMINING THE   */ 00344000
*/* CONDITION CODE FIELD, TO DETERMINE IF MORE THAN ONE SET CLOCK    */ 00345000
*/* WAS FOUND.  NOTE: IF MORE THAN ONE SET CLOCK WAS FOUND, THE      */ 00346000
*/* VALUES OF ALL SET CLOCKS MUST BE DISPLAYED TO THE OPERATOR.      */ 00347000
*/*                                                                  */ 00348000
*/********************************************************************/ 00349000
*                                                                  0189 00350000
*           DO;                                                    0189 00351000
*             REG12=PTREBEG;        /* GET PTR TO FIRST TCWA ENTRY   */ 00352000
         L     REG12,PTREBEG                                       0190 00353000
*             REG15=1;              /* INITIALIZE COUNTER FOR SET  0191 00354000
*                                      CLOCKS                        */ 00355000
         LA    REG15,1                                             0191 00356000
*/********************************************************************/ 00357000
*/*                                                                  */ 00358000
*/* TEST EACH ENTRY FOR A SET CLOCK UNTIL ALL ENTRIES ARE CHECKED,   */ 00359000
*/* OR MORE THAN 1 SET CLOCK HAS BEEN FOUND.                         */ 00360000
*/*                                                                  */ 00361000
*/********************************************************************/ 00362000
*                                                                  0192 00363000
*             RESPECIFY                                            0192 00364000
*              (REG0) RESTRICTED;                                  0192 00365000
*             DO REG0=TCCPUCNT TO 1 BY-1 WHILE REG15=>0;           0193 00366000
         LH    REG0,TCCPUCNT(,TCWAPTR)                             0193 00367000
         B     @DE00193                                            0193 00368000
@DL00193 LTR   REG15,REG15                                         0193 00369000
         BM    @DC00193                                            0193 00370000
*               IF REG12->TCLCC='00'B/* TEST FOR CC=0                */ 00371000
*                 THEN                                             0194 00372000
         TM    TCLCC(REG12),B'00110000'                            0194 00373000
         BNZ   @RF00194                                            0194 00374000
*                 REG15=REG15-1;                                   0195 00375000
         BCTR  REG15,0                                             0195 00376000
*               REG12=REG12+LENGTH(TCENTRY);/* POINT TO NEXT TCWA  0196 00377000
*                                      ENTRY                         */ 00378000
@RF00194 AH    REG12,@CH00045                                      0196 00379000
*             END;                                                 0197 00380000
         BCTR  REG0,0                                              0197 00381000
@DE00193 LTR   REG0,REG0                                           0197 00382000
         BP    @DL00193                                            0197 00383000
@DC00193 DS    0H                                                  0198 00384000
*             RESPECIFY                                            0198 00385000
*              (REG0) UNRESTRICTED;                                0198 00386000
*                                                                  0198 00387000
*/********************************************************************/ 00388000
*/*                                                                  */ 00389000
*/* TEST FOR ONLY ONE SET CLOCK FOUND.                               */ 00390000
*/*                                                                  */ 00391000
*/********************************************************************/ 00392000
*                                                                  0199 00393000
*             IF REG15=0 THEN                                      0199 00394000
*                                                                  0199 00395000
         LTR   REG15,REG15                                         0199 00396000
         BNZ   @RF00199                                            0199 00397000
*/********************************************************************/ 00398000
*/*                                                                  */ 00399000
*/* IF ONLY ONE SET CLOCK WAS FOUND, TURN ON THE FLAG WHICH INDI-    */ 00400000
*/* CATES THAT MESSAGE IEA888A SHOULD BE ISSUED.                     */ 00401000
*/*                                                                  */ 00402000
*/********************************************************************/ 00403000
*                                                                  0200 00404000
*               TCGXXX='1'B;                                       0200 00405000
         OI    TCGXXX(TCWAPTR),B'10000000'                         0200 00406000
*             ELSE                                                 0201 00407000
*                                                                  0201 00408000
*/********************************************************************/ 00409000
*/*                                                                  */ 00410000
*/* IF MORE THAN ONE SET CLOCK WAS FOUND, TURN ON THE FLAG WHICH IN- */ 00411000
*/* DICATES THAT MESSAGE IEA887A SHOULD BE ISSUED.                   */ 00412000
*/*                                                                  */ 00413000
*/********************************************************************/ 00414000
*                                                                  0201 00415000
*               TCGZZZ='1'B;                                       0201 00416000
         B     @RC00199                                            0201 00417000
@RF00199 OI    TCGZZZ(TCWAPTR),B'00100000'                         0201 00418000
*           END;                                                   0202 00419000
*         ELSE                                                     0203 00420000
*                                                                  0203 00421000
*/********************************************************************/ 00422000
*/*                                                                  */ 00423000
*/* IF THE RETURN CODE WAS NOT 0, 8, OR 12, IT MUST BE 4 OR 16.      */ 00424000
*/* THEREFORE, TEST REG1 FOR A VALUE OF 1, INDICATING THAT THE       */ 00425000
*/* SUBJECT ENTRY WAS THE LAST ENTRY.                                */ 00426000
*/*                                                                  */ 00427000
*/********************************************************************/ 00428000
*                                                                  0203 00429000
*           IF REG1=1 THEN                                         0203 00430000
*                                                                  0203 00431000
         B     @RC00188                                            0203 00432000
@RF00188 CH    REG1,@CH00096                                       0203 00433000
         BNE   @RF00203                                            0203 00434000
*/********************************************************************/ 00435000
*/*                                                                  */ 00436000
*/* IF THE VALUE IS 1, TURN ON THE FLAG WHICH INDICATES THAT MESSAGE */ 00437000
*/* IEA886A SHOULD BE ISSUED. (NOTE: IF THE VALUE IS NOT 1, THE LOOP */ 00438000
*/* WILL BE EXECUTED AGAIN BECAUSE THE RETURN CODE IS 4 OR 16.)      */ 00439000
*/*                                                                  */ 00440000
*/********************************************************************/ 00441000
*                                                                  0204 00442000
*             TCGYYY='1'B;                                         0204 00443000
         OI    TCGYYY(TCWAPTR),B'01000000'                         0204 00444000
*       END;                                                       0205 00445000
@RF00203 DS    0H                                                  0205 00446000
@RC00188 DS    0H                                                  0206 00447000
*     REG11=REG11+LENGTH(TCENTRY);  /* POINT TO NEXT TCWA ENTRY      */ 00448000
@RC00184 AH    REG11,@CH00045                                      0206 00449000
*   END;                                                           0207 00450000
         BCTR  REG1,0                                              0207 00451000
@DE00179 LTR   REG1,REG1                                           0207 00452000
         BP    @DL00179                                            0207 00453000
@DC00179 DS    0H                                                  0208 00454000
*                                                                  0208 00455000
*/********************************************************************/ 00456000
*/*                                                                  */ 00457000
*/* CALL THE COMMUNICATIONS ROUTINE TO WRITE MESSAGES TO THE         */ 00458000
*/* OPERATOR CONCERNING TOD CLOCK AND LOCAL TIME VALUES, AND TO PRO- */ 00459000
*/* CESS HIS REPLIES TO THESE MESSAGES.  THE MESSAGES TO BE WRITTEN  */ 00460000
*/* ARE INDICATED BY THE FLAGS JUST SET IN THE TCWAGFLG FIELD OF THE */ 00461000
*/* TOD CLOCK WORKAREA.                                              */ 00462000
*/*                                                                  */ 00463000
*/********************************************************************/ 00464000
*                                                                  0208 00465000
*   CALL IEAVRCOM;                                                 0208 00466000
*                                                                  0208 00467000
         BAL   @14,IEAVRCOM                                        0208 00468000
*/********************************************************************/ 00469000
*/*                                                                  */ 00470000
*/* CALL THE SYNCHRONIZATION TESTING ROUTINE TO DETERMINE IF THE     */ 00471000
*/* CLOCKS NOW REQUIRE SYNCHRONIZING, SINCE THE OPERATOR MAY HAVE    */ 00472000
*/* MADE CHANGES.                                                    */ 00473000
*/*                                                                  */ 00474000
*/********************************************************************/ 00475000
*                                                                  0209 00476000
*   CALL IEAVRTST;                                                 0209 00477000
*                                                                  0209 00478000
         BAL   @14,IEAVRTST                                        0209 00479000
*/********************************************************************/ 00480000
*/*                                                                  */ 00481000
*/* TEST FOR A RETURN CODE OF 0.                                     */ 00482000
*/*                                                                  */ 00483000
*/********************************************************************/ 00484000
*                                                                  0210 00485000
*   IF RETCODE^=0 THEN                                             0210 00486000
*                                                                  0210 00487000
         LTR   RETCODE,RETCODE                                     0210 00488000
         BZ    @RF00210                                            0210 00489000
*/********************************************************************/ 00490000
*/*                                                                  */ 00491000
*/* IF THE RETURN WAS 0, THERE IS ONLY ONE CPU IN THE CONFIGURATION  */ 00492000
*/* AND ITS CLOCK IS NOW SET,  OR ALL CLOCKS ARE SET AND IN          */ 00493000
*/* SYNCHRONISM.  IF THE RETURN CODE IS NOT 0, TEST FOR A RETURN     */ 00494000
*/* CODE OF 8 OR 12.                                                 */ 00495000
*/*                                                                  */ 00496000
*/********************************************************************/ 00497000
*                                                                  0211 00498000
*     IF RETCODE=8 RETCODE=12 THEN                                 0211 00499000
*                                                                  0211 00500000
         CH    RETCODE,@CH00038                                    0211 00501000
         BE    @RT00211                                            0211 00502000
         CH    RETCODE,@CH00854                                    0211 00503000
         BNE   @RF00211                                            0211 00504000
@RT00211 DS    0H                                                  0212 00505000
*/********************************************************************/ 00506000
*/*                                                                  */ 00507000
*/* IF THE RETURN CODE IS 8 OR 12, THE TOD CLOCKS REQUIRE SYN-       */ 00508000
*/* CHRONIZING.  THEREFORE, CALL THE SYNCHRONIZING ROUTINE TO AT-    */ 00509000
*/* TEMPT SYNCHRONIZATION OF THE CLOCKS, AND THEN CALL THE SYNCHRONI-*/ 00510000
*/* ZATION TESTING ROUTINE TO ENSURE THAT THE CLOCKS WERE ACTUALLY   */ 00511000
*/* SYNCHRONIZED.  (NOTE: THE RETURN CODE NOW SET BY THE TESTING     */ 00512000
*/* ROUTINE WILL BE PASSED BACK TO THE CALLER TO INDICATE THE SUCCESS*/ 00513000
*/* OR FAILURE OF THE SYNCHRONIZING OPERATION.)                      */ 00514000
*/*                                                                  */ 00515000
*/********************************************************************/ 00516000
*                                                                  0212 00517000
*       DO;                                                        0212 00518000
*         CALL IEAVRSYN;                                           0213 00519000
         BAL   @14,IEAVRSYN                                        0213 00520000
*         CALL IEAVRTST;                                           0214 00521000
         BAL   @14,IEAVRTST                                        0214 00522000
*       END;                                                       0215 00523000
*   RCSAVE=REG15;                   /* SAVE THE RETURN CODE FROM   0216 00524000
*                                      IEAVRTST                      */ 00525000
@RF00211 DS    0H                                                  0216 00526000
@RF00210 ST    REG15,RCSAVE                                        0216 00527000
*/********************************************************************/ 00528000
*/*                                                                  */ 00529000
*/* SAVE THE ADDRESS OF THE ASYNCHRONOUS RECOVERY ROUTINE (IEAVRCLA) */ 00530000
*/* IN THE TPC SO THAT IT CAN BE SCHEDULED BY THE SYNCHRONOUS        */ 00531000
*/* ROUTINE (IEAVRCLS).                                              */ 00532000
*/*                                                                  */ 00533000
*/********************************************************************/ 00534000
*                                                                  0217 00535000
*   TPCCLA=ADDR(IEAVRCLA);                                         0217 00536000
*                                                                  0217 00537000
         L     @07,TPCPTR(,LCVTPTR)                                0217 00538000
         LA    @06,IEAVRCLA                                        0217 00539000
         ST    @06,TPCCLA(,@07)                                    0217 00540000
*/********************************************************************/ 00541000
*/*                                                                  */ 00542000
*/* SEARCH THE LOGICAL CCA VECTOR TABLE.  WHEN A NON-ZERO ENTRY      */ 00543000
*/* IS FOUND, SET THE WAIT TIME FIELD IN THE LCCA TO 0, AND SET      */ 00544000
*/* THE TIME AT DISPATCH FIELD TO THE CURRENT TOD CLOCK VALUE.       */ 00545000
*/*                                                                  */ 00546000
*/********************************************************************/ 00547000
*                                                                  0218 00548000
*   REG12=CVTLCCAT;                 /* GET PTR TO LCCAVT             */ 00549000
         L     REG12,CVTLCCAT(,LCVTPTR)                            0218 00550000
*   RESPECIFY                                                      0219 00551000
*     LCCAVT BASED(REG12);                                         0219 00552000
*   DO REG1=DIM(LCCAT00P) TO 1 BY-1;                               0220 00553000
         LA    REG1,16                                             0220 00554000
@DL00220 DS    0H                                                  0221 00555000
*     IF PTRLCCA^=0                 /* TEST FOR NON-ZERO ENTRY       */ 00556000
*       THEN                                                       0221 00557000
         L     @07,PTRLCCA(,REG12)                                 0221 00558000
         LTR   @07,@07                                             0221 00559000
         BZ    @RF00221                                            0221 00560000
*       DO;                                                        0222 00561000
*         LCCAPTR=PTRLCCA;                                         0223 00562000
         LR    LCCAPTR,@07                                         0223 00563000
*         LCCAWTIM='0000000000000000'X;                            0224 00564000
         MVC   LCCAWTIM(8,LCCAPTR),@CB02728                        0224 00565000
*         GENERATE CODE REFS(LCCADTOD,LCCAPTR)(STCK  LCCADTOD(LCCAPTR)) 00566000
*             ;                                                    0225 00567000
         STCK  LCCADTOD(LCCAPTR)                                        00568000
*       END;                                                       0226 00569000
*     REG12=REG12+LENGTH(LCCAT00P); /* POINT TO NEXT LCCAVT ENTRY    */ 00570000
@RF00221 AH    REG12,@CH00108                                      0227 00571000
*   END;                                                           0228 00572000
*                                                                  0228 00573000
         BCTR  REG1,0                                              0228 00574000
         LTR   REG1,REG1                                           0228 00575000
         BP    @DL00220                                            0228 00576000
*/********************************************************************/ 00577000
*/*                                                                  */ 00578000
*/* ZERO OUT THE ACCUMULATED JOB STEP TIME FIELD FOR ALL MEMORIES.   */ 00579000
*/*                                                                  */ 00580000
*/********************************************************************/ 00581000
*                                                                  0229 00582000
*   REG12=CVTASVT;                  /* GET POINTER TO ASVT           */ 00583000
         L     REG12,CVTASVT(,LCVTPTR)                             0229 00584000
*   RESPECIFY                                                      0230 00585000
*     ASVT BASED(REG12);                                           0230 00586000
*   RESPECIFY                                                      0231 00587000
*     ASCB BASED(ASVTENTY);                                        0231 00588000
*   DO REG1=ASVTMAXU TO 1 BY-1;                                    0232 00589000
         L     REG1,ASVTMAXU(,REG12)                               0232 00590000
         B     @DE00232                                            0232 00591000
@DL00232 DS    0H                                                  0233 00592000
*     IF ASVTAVAL='0'B THEN         /* TEST FOR IN-USE ASVT ENTRY    */ 00593000
         TM    ASVTAVAL(REG12),B'10000000'                         0233 00594000
         BNZ   @RF00233                                            0233 00595000
*       ASCBEJST='0000000000000000'X;                              0234 00596000
         L     @07,ASVTENTY(,REG12)                                0234 00597000
         MVC   ASCBEJST(8,@07),@CB02728                            0234 00598000
*     REG12=REG12+LENGTH(ASVTENTY); /* POINT TO NEXT ASVT ENTRY      */ 00599000
@RF00233 AH    REG12,@CH00108                                      0235 00600000
*   END;                                                           0236 00601000
*                                                                  0236 00602000
         BCTR  REG1,0                                              0236 00603000
@DE00232 LTR   REG1,REG1                                           0236 00604000
         BP    @DL00232                                            0236 00605000
*/********************************************************************/ 00606000
*/*                                                                  */ 00607000
*/* CALL THE ROUTINE WHICH ENQUEUES THE JOBSTEP TIMING TQE AND THE   */ 00608000
*/* MIDNIGHT TQE, AND NOTIFIES THE SRM THAT INITIALIZATION OF THE    */ 00609000
*/* TOD CLOCKS IS COMPLETE.                                          */ 00610000
*/*                                                                  */ 00611000
*/********************************************************************/ 00612000
*                                                                  0237 00613000
*   CALL TQEINIT;                                                  0237 00614000
*                                                                  0237 00615000
         LA    @15,TQEINIT                                         0237 00616000
         BALR  @14,@15                                             0237 00617000
*/********************************************************************/ 00618000
*/*                                                                  */ 00619000
*/* INITIALIZE THE TIMER STATUS BYTES IN THE PCCA OF EACH ONLINE CPU */ 00620000
*/*                                                                  */ 00621000
*/********************************************************************/ 00622000
*                                                                  0238 00623000
*   REG11=PTREBEG;                  /* GET PTR TO FIRST TCWA ENTRY   */ 00624000
         L     REG11,PTREBEG                                       0238 00625000
*   DO REG1=TCCPUCNT TO 1 BY-1;                                    0239 00626000
         LH    REG1,TCCPUCNT(,TCWAPTR)                             0239 00627000
         B     @DE00239                                            0239 00628000
@DL00239 DS    0H                                                  0240 00629000
*     TCWAPCCA->PCCATMST=TMSTINIT;                                 0240 00630000
         L     @07,TCWAPCCA(,REG11)                                0240 00631000
         MVC   PCCATMST(4,@07),TMSTINIT                            0240 00632000
*     REG11=REG11+LENGTH(TCENTRY);  /* POINT TO NEXT TCWA ENTRY      */ 00633000
         AH    REG11,@CH00045                                      0241 00634000
*   END;                                                           0242 00635000
*                                                                  0242 00636000
         BCTR  REG1,0                                              0242 00637000
@DE00239 LTR   REG1,REG1                                           0242 00638000
         BP    @DL00239                                            0242 00639000
*/********************************************************************/ 00640000
*/*                                                                  */ 00641000
*/* FREE THE TOD CLOCK WORKAREA AND THE FIXED PAGE.                  */ 00642000
*/*                                                                  */ 00643000
*/********************************************************************/ 00644000
*                                                                  0243 00645000
*   REG0=TCWALN;                    /* GET LENGTH OF TCWA            */ 00646000
         LH    REG0,TCWALN                                         0243 00647000
*   REG1=TCWAPTR;                   /* GET ADDRESS OF TCWA           */ 00648000
         LR    REG1,TCWAPTR                                        0244 00649000
*   GENERATE CODE(FREEMAIN RU,LV=(0),A=(1),SP=245);                0245 00650000
         FREEMAIN RU,LV=(0),A=(1),SP=245                                00651000
*   REG1=ADDR(BEGTOD);              /* GET POINTER TO START OF MODULE*/ 00652000
         LA    REG1,BEGTOD                                         0246 00653000
*   REG15=ADDR(ENDTOD);             /* GET PTR TO END OF MODULE      */ 00654000
         LA    REG15,ENDTOD                                        0247 00655000
*   GENERATE CODE(PGFREE R,A=(1),EA=(15));                         0248 00656000
*                                                                  0248 00657000
         PGFREE R,A=(1),EA=(15)                                         00658000
*/********************************************************************/ 00659000
*/*                                                                  */ 00660000
*/* INDICATE THAT PROMPTING IS REQUIRED FROM NOW ON, AND ZERO THE    */ 00661000
*/* POINTER TO THE TOD CLOCK WORKAREA IN THE TPC TO INDICATE THAT    */ 00662000
*/* MANIPULATION OF THE TOD CLOCKS HAS CEASED.                       */ 00663000
*/*                                                                  */ 00664000
*/********************************************************************/ 00665000
*                                                                  0249 00666000
*   MSTODWTO='0'B;                  /*                        @Y02675*/ 00667000
         L     @07,CVTMSER(,LCVTPTR)                               0249 00668000
         NI    MSTODWTO(@07),B'10111111'                           0249 00669000
*   TPCTCWA=0;                                                     0250 00670000
*                                                                  0250 00671000
         L     @07,TPCPTR(,LCVTPTR)                                0250 00672000
         SLR   @06,@06                                             0250 00673000
         ST    @06,TPCTCWA(,@07)                                   0250 00674000
*/********************************************************************/ 00675000
*/*                                                                  */ 00676000
*/* RESTORE THE SAVEAREA ADDRESS, AND RETURN TO THE CALLER WITH A    */ 00677000
*/* RETURN CODE IN REGISTER 15.                                      */ 00678000
*/*                                                                  */ 00679000
*/********************************************************************/ 00680000
*                                                                  0251 00681000
*   REG13=R13SAV;                                                  0251 00682000
         L     REG13,R13SAV                                        0251 00683000
*   RETURN CODE(RCSAVE);                                           0252 00684000
         L     @07,RCSAVE                                          0252 00685000
         L     @13,4(,@13)                                         0252 00686000
         L     @00,@SIZDATD                                        0252 00687000
         LR    @01,@08                                             0252 00688000
         FREEMAIN R,LV=(0),A=(1)                                        00689000
         LR    @15,@07                                             0252 00690000
         L     @14,12(,@13)                                        0252 00691000
         LM    @00,@12,20(@13)                                     0252 00692000
         BR    @14                                                 0252 00693000
*                                                                  0253 00694000
*/********************************************************************/ 00695000
*/*                                                                  */ 00696000
*/*          IEAVRSSC--TOD SET SPECIFIC CLOCK ROUTINE                */ 00697000
*/*                                                                  */ 00698000
*/* THIS ROUTINE RECEIVES CONTROL FROM IEAVRCLA AND THE VARY CPU     */ 00699000
*/* COMMAND PROCESSOR.  IT BUILDS A WORKAREA CONTAINING A LIST OF 2  */ 00700000
*/* CPU'S, ONE REPRESENTING THE CPU WHOSE CLOCK IS TO BE SET/SYNCHED */ 00701000
*/* AND THE OTHER REPRESENTING THE CPU TO WHICH THE "SPECIFIC" CLOCK */ 00702000
*/* WILL BE SYNCHRONIZED.  IT THEN USES ROUTINES IEAVRTST,IEAVRCOM,  */ 00703000
*/* AND IEAVRSYN TO SET/SYNCH THE "SPECIFIC" CLOCK.                  */ 00704000
*/*                                                                  */ 00705000
*/* FIRST, USING IEAVRTST, AN ATTEMPT IS MADE TO FIND AN ONLINE CPU  */ 00706000
*/* WHOSE CLOCK IS SET.  IF FOUND, IEAVRSYN IS INVOKED TO SYNCHRO-   */ 00707000
*/* NIZE THE "SPECIFIC" CLOCK.  IF NO SET CLOCK CAN BE FOUND, USING  */ 00708000
*/* THE RETURN CODE FROM IEAVRTST, A FLAG IS SET IN THE WORKAREA TO  */ 00709000
*/* INDICATE WHICH MESSAGE SHOULD BE DISPLAYED TO THE OPERATOR.      */ 00710000
*/* THEN IEAVRCOM IS INVOKED TO COMMUNICATE WITH THE OPERATOR, AND   */ 00711000
*/* THE "SPECIFIC" CLOCK IS ULTIMATELY SET.                          */ 00712000
*/*                                                                  */ 00713000
*/* TIMER SUPERVISION RELATED FIELDS IN VARIOUS CONTROL BLOCKS ARE   */ 00714000
*/* UPDATED OR INITIALIZED, DEPENDING ON THE SUCCESS OR FAILURE OF   */ 00715000
*/* THE SETTING/SYNCHRONIZING OPERATION.  CONTROL IS RETURNED TO THE */ 00716000
*/* CALLER WITH A RETURN CODE IN REGISTER 15.                        */ 00717000
*/*                                                                  */ 00718000
*/********************************************************************/ 00719000
*                                                                  0253 00720000
*IEAVRSSC:                                                         0253 00721000
*   ENTRY(NEWPCCA);                                                0253 00722000
*                                                                  0253 00723000
@EP00253 MVC   @PC00001(4),0(@01)                                  0253 00724000
*/********************************************************************/ 00725000
*/*                                                                  */ 00726000
*/* SAVE THE CALLER'S ID, AND THE ADDRESS OF THE SAVEAREA.           */ 00727000
*/*                                                                  */ 00728000
*/********************************************************************/ 00729000
*                                                                  0254 00730000
*   CALLERID=REG0;                                                 0254 00731000
         STH   REG0,CALLERID                                       0254 00732000
*   R13SAV=REG13;                                                  0255 00733000
*                                                                  0255 00734000
         ST    REG13,R13SAV                                        0255 00735000
*/********************************************************************/ 00736000
*/*                                                                  */ 00737000
*/* SAVE THE CODE AND DATA REGISTERS FOR USE IN RECOVERY EXIT.       */ 00738000
*/*                                                                  */ 00739000
*/********************************************************************/ 00740000
*                                                                  0256 00741000
*   STM(REG8,REG10,SSCBASE);                                       0256 00742000
*                                                                  0256 00743000
         STM   REG8,REG10,SSCBASE                                  0256 00744000
*/********************************************************************/ 00745000
*/*                                                                  */ 00746000
*/* ESTABLISH ERROR EXIT FOR UNLOCKED CODE.                          */ 00747000
*/*                                                                  */ 00748000
*/********************************************************************/ 00749000
*                                                                  0257 00750000
*   REG1=ADDR(ESTAEL);              /* GET ADDR OF MACRO PARM LIST   */ 00751000
         LA    REG1,ESTAEL                                         0257 00752000
*   REG2=ADDR(SSCESTAE);            /* GET EXIT ADDRESS              */ 00753000
         LA    REG2,SSCESTAE                                       0258 00754000
*   REG3=ADDR(SSCRPARM);            /* GET EXIT PARMLIST ADDRESS     */ 00755000
         LA    REG3,SSCRPARM                                       0259 00756000
*   GEN CODE REFS(REG1,REG2,REG3)                                  0260 00757000
*       (ESTAE (2),CT,PARAM=(3),PURGE=NONE,RECORD=YES,MF=(E,(1))); 0260 00758000
*                                                                  0260 00759000
         ESTAE (2),CT,PARAM=(3),PURGE=NONE,RECORD=YES,MF=(E,(1))        00760000
*/********************************************************************/ 00761000
*/*                                                                  */ 00762000
*/* RE-BASE THE CVT FOR MORE EFFICIENT CODE GENERATION.              */ 00763000
*/*                                                                  */ 00764000
*/********************************************************************/ 00765000
*                                                                  0261 00766000
*   LCVTPTR=CVTPTR;                                                0261 00767000
*                                                                  0261 00768000
         L     LCVTPTR,CVTPTR                                      0261 00769000
*/********************************************************************/ 00770000
*/*                                                                  */ 00771000
*/* GET A TOD CLOCK WORKAREA WITH 2 ENTRIES.                         */ 00772000
*/*                                                                  */ 00773000
*/********************************************************************/ 00774000
*                                                                  0262 00775000
*   REG0=LENGTH(TCWA)+(2*LENGTH(TCENTRY));                         0262 00776000
         LA    REG0,144                                            0262 00777000
*   GENERATE CODE(GETMAIN RC,LV=(0),SP=245     GET TOD CLOCK WORKAREA); 00778000
*                                                                  0263 00779000
         GETMAIN RC,LV=(0),SP=245     GET TOD CLOCK WORKAREA            00780000
*/********************************************************************/ 00781000
*/*                                                                  */ 00782000
*/* TEST FOR A SUCCESSFUL GETMAIN.                                   */ 00783000
*/*                                                                  */ 00784000
*/********************************************************************/ 00785000
*                                                                  0264 00786000
*   IF RETCODE=0 THEN                                              0264 00787000
*                                                                  0264 00788000
         LTR   RETCODE,RETCODE                                     0264 00789000
         BNZ   @RF00264                                            0264 00790000
*/********************************************************************/ 00791000
*/*                                                                  */ 00792000
*/* IF THE GETMAIN WAS SUCCESSFUL, SERIALIZE THE EXECUTION OF THIS   */ 00793000
*/* CODE.                                                            */ 00794000
*/*                                                                  */ 00795000
*/********************************************************************/ 00796000
*                                                                  0265 00797000
*     DO;                                                          0265 00798000
*LOOP2:                                                            0266 00799000
*       REG11=0;                    /* CLEAR REG11 FOR COMPARE AND 0266 00800000
*                                      SWAP                          */ 00801000
LOOP2    SLR   REG11,REG11                                         0266 00802000
*       CS(REG11,REG1,TPCTCWA);     /* TEST FOR CODE IN EXECUTION    */ 00803000
         L     @07,TPCPTR(,LCVTPTR)                                0267 00804000
         CS    REG11,@01,TPCTCWA(@07)                              0267 00805000
*       BC(7,LOOP2);                /* BRANCH IF CODE IN EXECUTION   */ 00806000
         BC    7,LOOP2                                             0268 00807000
*/********************************************************************/ 00808000
*/*                                                                  */ 00809000
*/* ESTABLISH ADDRESSABILITY TO THE TOD CLOCK WORKAREA, SAVE THE     */ 00810000
*/* NUMBER OF TOD CLOCK WORKAREA ENTRIES, AND SET THE WORKAREA FLAGS */ 00811000
*/* TO ZEROES.                                                       */ 00812000
*/*                                                                  */ 00813000
*/********************************************************************/ 00814000
*                                                                  0269 00815000
*       TCWAPTR=REG1;                                              0269 00816000
         LR    TCWAPTR,REG1                                        0269 00817000
*       TCCPUCNT=2;                                                0270 00818000
         MVC   TCCPUCNT(2,TCWAPTR),@CH00119                        0270 00819000
*       TCWAGFLG='00'X;             /*                       @YM08621*/ 00820000
         MVI   TCWAGFLG(TCWAPTR),X'00'                             0271 00821000
*/********************************************************************/ 00822000
*/*                                                                  */ 00823000
*/* SET UP THE INTERFACE TO GET A PAGE FIXED, AND FIX THIS MODULE    */ 00824000
*/*                                                                  */ 00825000
*/********************************************************************/ 00826000
*                                                                  0272 00827000
*       REG1=ADDR(BEGTOD);          /* GET POINTER TO START OF MODULE*/ 00828000
         LA    REG1,BEGTOD                                         0272 00829000
*       REG15=ADDR(ENDTOD);         /* GET POINTER TO END OF MODULE  */ 00830000
         LA    REG15,ENDTOD                                        0273 00831000
*       REG0=ADDR(TODECB);          /* GET POINTER TO ECB            */ 00832000
         LA    REG0,TODECB                                         0274 00833000
*       TODECB=0;                   /* CLEAR ECB                     */ 00834000
         SLR   @07,@07                                             0275 00835000
         ST    @07,TODECB                                          0275 00836000
*       GENERATE CODE(PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N);       0276 00837000
*                                                                  0276 00838000
         PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N                           00839000
*/********************************************************************/ 00840000
*/*                                                                  */ 00841000
*/* WAIT FOR COMPLETION OF THE PAGE FIXING OPERATION.                */ 00842000
*/*                                                                  */ 00843000
*/********************************************************************/ 00844000
*                                                                  0277 00845000
*       REG1=ADDR(TODECB);          /* GET ECB ADDRESS               */ 00846000
         LA    REG1,TODECB                                         0277 00847000
*       GENERATE CODE(WAIT  1,ECB=(1));                            0278 00848000
*                                                                  0278 00849000
         WAIT  1,ECB=(1)                                                00850000
*/********************************************************************/ 00851000
*/*                                                                  */ 00852000
*/* SET THE RETURN CODE TO 4 SO THAT THE LOOP WILL BE EXECUTED AT    */ 00853000
*/* AT LEAST ONCE.                                                   */ 00854000
*/*                                                                  */ 00855000
*/********************************************************************/ 00856000
*                                                                  0279 00857000
*       RETCODE=4;                                                 0279 00858000
         LA    RETCODE,4                                           0279 00859000
*                                                                  0280 00860000
*/********************************************************************/ 00861000
*/*                                                                  */ 00862000
*/* PERFORM THE FOLLOWING UNTIL A SET CLOCK IS FOUND, OR UNTIL ALL   */ 00863000
*/* OSTENSIBLY GOOD CLOCKS HAVE BEEN TESTED AND HAVE BEEN FOUND NOT  */ 00864000
*/* TO BE SET.  THE SET CLOCK WILL BE USED TO SYNCHRONIZE THE NEW    */ 00865000
*/* CLOCK TO.                                                        */ 00866000
*/*                                                                  */ 00867000
*/********************************************************************/ 00868000
*                                                                  0280 00869000
*       PTREBEG=TCWAPTR+LENGTH(TCWA);/* SAVE PTR TO FIRST TCWA ENTRY */ 00870000
         LA    @07,112                                             0280 00871000
         ALR   @07,TCWAPTR                                         0280 00872000
         ST    @07,PTREBEG                                         0280 00873000
*       REG12=CVTPCCAT+LENGTH(PCCAVT)-LENGTH(PCCAT00P);/* GET PTR TO    00874000
*                                      LAST PCCAVT ENTRY             */ 00875000
         L     REG12,CVTPCCAT(,LCVTPTR)                            0281 00876000
         SH    REG12,@CH02877                                      0281 00877000
*       MASKHLDR=CSDCPUAL;          /* GET MASK OF ALIVE CPU'S       */ 00878000
         L     @07,CVTCSD(,LCVTPTR)                                0282 00879000
         SLR   MASKHLDR,MASKHLDR                                   0282 00880000
         ICM   MASKHLDR,3,CSDCPUAL(@07)                            0282 00881000
*       CPUSLEFT=LENGTH(CSDCPUAL);  /* SAVE THE NUMBER OF BITS IN THE   00882000
*                                      MASK STILL TO BE TESTED       */ 00883000
         MVC   CPUSLEFT(2),@CH00045                                0283 00884000
*       DO WHILE RETCODE=4;                                        0284 00885000
*                                                                  0284 00886000
         B     @DE00284                                            0284 00887000
@DL00284 DS    0H                                                  0285 00888000
*/********************************************************************/ 00889000
*/*                                                                  */ 00890000
*/* SEARCH THE CPU-ALIVE MASK FOR A BIT ON INDICATING AN ONLINE CPU. */ 00891000
*/* WHEN ONE IS FOUND, TEST THE TOD CLOCK STATUS BYTE IN THE PCCA FOR*/ 00892000
*/* THAT CPU TO DETERMINE IF THE CLOCK IS GOOD.                      */ 00893000
*/*                                                                  */ 00894000
*/********************************************************************/ 00895000
*                                                                  0285 00896000
*         DO REG1=CPUSLEFT TO 1 BY-1;                              0285 00897000
         LH    REG1,CPUSLEFT                                       0285 00898000
         B     @DE00285                                            0285 00899000
@DL00285 DS    0H                                                  0286 00900000
*           IF MASKHLDR//2^=0&PCCANUTD='0'B THEN                   0286 00901000
*                                                                  0286 00902000
         LR    @07,MASKHLDR                                        0286 00903000
         N     @07,@CF00096                                        0286 00904000
         LTR   @07,@07                                             0286 00905000
         BZ    @RF00286                                            0286 00906000
         L     @07,PTRPCCA(,REG12)                                 0286 00907000
         TM    PCCANUTD(@07),B'10000000'                           0286 00908000
         BNZ   @RF00286                                            0286 00909000
*/********************************************************************/ 00910000
*/*                                                                  */ 00911000
*/* IF THE CPU IS ONLINE AND ITS CLOCK IS GOOD, INITIALIZE THE TWO   */ 00912000
*/* TOD CLOCK WORKAREA ENTRIES WITH THE THE PCCA ADDRESSES AND THE   */ 00913000
*/* CPU ADDRESSES FOR THE CPU BEING VARY'ED ONLINE, AND FOR THE      */ 00914000
*/* CPU WITH THE GOOD CLOCK.                                         */ 00915000
*/*                                                                  */ 00916000
*/********************************************************************/ 00917000
*                                                                  0287 00918000
*             DO;                                                  0287 00919000
*               REG11=PTREBEG;      /* GET PTR TO FIRST TCWA ENTRY   */ 00920000
         L     REG11,PTREBEG                                       0288 00921000
*               TCWAPCCA=PTRPCCA;                                  0289 00922000
         ST    @07,TCWAPCCA(,REG11)                                0289 00923000
*               TCWAIADD=PCCACPUA;                                 0290 00924000
         MVC   TCWAIADD(2,REG11),PCCACPUA(@07)                     0290 00925000
*               REG11=REG11+LENGTH(TCENTRY);/* POINT TO 2ND TCWA   0291 00926000
*                                      ENTRY                         */ 00927000
         AH    REG11,@CH00045                                      0291 00928000
*               TCWAPCCA=ADDR(NEWPCCA);                            0292 00929000
         L     @07,@PC00001                                        0292 00930000
         ST    @07,TCWAPCCA(,REG11)                                0292 00931000
*               TCWAIADD=ADDR(NEWPCCA)->PCCACPUA;                  0293 00932000
*                                                                  0293 00933000
         MVC   TCWAIADD(2,REG11),PCCACPUA(@07)                     0293 00934000
*/********************************************************************/ 00935000
*/*                                                                  */ 00936000
*/* SET THE RETURN CODE AND LOOP CONTROL TO 0 SO THAT AN EXIT FROM   */ 00937000
*/* THE LOOP WILL OCCUR. THEN,                                       */ 00938000
*/* DECREMENT AND SAVE THE INDEX THROUGH THE CPU-ALIVE MASK SO       */ 00939000
*/* THAT IF THE APPARENTLY GOOD CLOCK TURNS OUT NOT TO BE SET, THE   */ 00940000
*/* THE SEARCH THROUGH THE MASK WILL BEGIN WITH THE NEXT BIT.        */ 00941000
*/*                                                                  */ 00942000
*/********************************************************************/ 00943000
*                                                                  0294 00944000
*               CPUSLEFT=REG1-1;                                   0294 00945000
         LR    @07,REG1                                            0294 00946000
         BCTR  @07,0                                               0294 00947000
         STH   @07,CPUSLEFT                                        0294 00948000
*               REG1=0;                                            0295 00949000
         SLR   REG1,REG1                                           0295 00950000
*               RETCODE=0;                                         0296 00951000
         SLR   RETCODE,RETCODE                                     0296 00952000
*             END;                                                 0297 00953000
*           MASKHLDR=MASKHLDR/2;    /* GET NEXT BIT TO TEST          */ 00954000
@RF00286 SRL   MASKHLDR,1                                          0298 00955000
*           REG12=REG12-LENGTH(PCCAT00P);/* POINT TO NEXT PCCAVT   0299 00956000
*                                      ENTRY                         */ 00957000
         SH    REG12,@CH00108                                      0299 00958000
*         END;                                                     0300 00959000
         BCTR  REG1,0                                              0300 00960000
@DE00285 LTR   REG1,REG1                                           0300 00961000
         BP    @DL00285                                            0300 00962000
*                                                                  0301 00963000
*/********************************************************************/ 00964000
*/*                                                                  */ 00965000
*/* TEST FOR GOOD TOD CLOCK FOUND.                                   */ 00966000
*/*                                                                  */ 00967000
*/********************************************************************/ 00968000
*                                                                  0301 00969000
*         IF RETCODE=0 THEN                                        0301 00970000
*                                                                  0301 00971000
         LTR   RETCODE,RETCODE                                     0301 00972000
         BNZ   @RF00301                                            0301 00973000
*/********************************************************************/ 00974000
*/*                                                                  */ 00975000
*/* IF IT IS 0, CALL THE SYNCHRONIZATION TESTING ROUTINE.            */ 00976000
*/*                                                                  */ 00977000
*/********************************************************************/ 00978000
*                                                                  0302 00979000
*           DO;                                                    0302 00980000
*             CALL IEAVRTST;                                       0303 00981000
*                                                                  0303 00982000
         BAL   @14,IEAVRTST                                        0303 00983000
*/********************************************************************/ 00984000
*/*                                                                  */ 00985000
*/* TEST THE RETURN CODE FROM THE SYNCHRONIZATION TESTING ROUTINE    */ 00986000
*/* FOR A VALUE OF 0 OR 4 INDICATING A SET OR NOT SET SUBJECT CLOCK, */ 00987000
*/* OR A RETURN CODE OF 20 INDICATING A FAILURE TO SIGNAL SOME       */ 00988000
*/* PROCESSOR OTHER THAN THE SUBJECT.                        @ZA15781*/ 00989000
*/*                                                                  */ 00990000
*/********************************************************************/ 00991000
*                                                                  0304 00992000
*             IF RETCODE^=0&RETCODE^=4&RETCODE^=20 THEN/*    @ZA15781*/ 00993000
         LTR   RETCODE,RETCODE                                     0304 00994000
         BZ    @RF00304                                            0304 00995000
         CH    RETCODE,@CH00108                                    0304 00996000
         BE    @RF00304                                            0304 00997000
         CH    RETCODE,@CH00503                                    0304 00998000
         BE    @RF00304                                            0304 00999000
*               DO;                                                0305 01000000
*                                                                  0305 01001000
*/********************************************************************/ 01002000
*/*                                                                  */ 01003000
*/* IF THE RETURN CODE ISN'T 0 OR 4, SEE IF IT IS 16, INDICATING THAT*/ 01004000
*/* THE TESTING COULD NOT BE PERFORMED WITHIN 2**20 MICROSECONDS.    */ 01005000
*/*                                                                  */ 01006000
*/********************************************************************/ 01007000
*                                                                  0306 01008000
*                 IF RETCODE=16 THEN                               0306 01009000
*                                                                  0306 01010000
         CH    RETCODE,@CH00045                                    0306 01011000
         BNE   @RF00306                                            0306 01012000
*/********************************************************************/ 01013000
*/*                                                                  */ 01014000
*/* IF IT IS 16, INDICATE THAT THE NEW CPU REQUIRES SYNCHRONIZING.   */ 01015000
*/*                                                                  */ 01016000
*/********************************************************************/ 01017000
*                                                                  0307 01018000
*                   ADDR(NEWPCCA)->PCCASYNC='1'B;                  0307 01019000
*                                                                  0307 01020000
         L     @07,@PC00001                                        0307 01021000
         OI    PCCASYNC(@07),B'01000000'                           0307 01022000
*/********************************************************************/ 01023000
*/*                                                                  */ 01024000
*/* IF IT WAS 8, 12, OR 16, CALL THE SYNCHRONIZING ROUTINE TO PERFORM*/ 01025000
*/* THE SYNCHING OPERATION, AND THEN CALL THE TESTING ROUTINE TO     */ 01026000
*/* TEST FOR SUCCESSFUL PERFORMANCE.                                 */ 01027000
*/*                                                                  */ 01028000
*/********************************************************************/ 01029000
*                                                                  0308 01030000
*                 CALL IEAVRSYN;                                   0308 01031000
@RF00306 BAL   @14,IEAVRSYN                                        0308 01032000
*                 CALL IEAVRTST;                                   0309 01033000
         BAL   @14,IEAVRTST                                        0309 01034000
*               END;                                               0310 01035000
*           END;                                                   0311 01036000
*         ELSE                                                     0312 01037000
*                                                                  0312 01038000
*/********************************************************************/ 01039000
*/*                                                                  */ 01040000
*/* IF NO GOOD CLOCK WAS FOUND TO WHICH TO SYNCHRONIZE THE NEW CLOCK,*/ 01041000
*/* SET BAIN TO 0 TO DISALLOW EXTERNAL SET COMMANDS, AND THEN DEQUEUE*/ 01042000
*/* THE MIDNIGHT AND JOBSTEP TIMING TQES.                            */ 01043000
*/*                                                                  */ 01044000
*/********************************************************************/ 01045000
*                                                                  0312 01046000
*           DO;                                                    0312 01047000
         B     @RC00301                                            0312 01048000
@RF00301 DS    0H                                                  0313 01049000
*             BAIN='0'B;                                           0313 01050000
         L     @07,CVTMSER(,LCVTPTR)                               0313 01051000
         NI    BAIN(@07),B'01111111'                               0313 01052000
*             REG1=ADDR(TPCMNTQE);  /* GET ADDR OF MIDNIGHT TQE      */ 01053000
         L     REG1,TPCPTR(,LCVTPTR)                               0314 01054000
         LA    REG1,TPCMNTQE(,REG1)                                0314 01055000
*             REG15=ADDR(DEQTQE);   /* GET ADDR OF DEQ ROUTINE       */ 01056000
         L     REG15,CVTQTD00(,LCVTPTR)                            0315 01057000
*             GENERATE CODE REFS(REG15,PSALITA) SETS(REG2);        0316 01058000
         SETLOCK OBTAIN,TYPE=DISP,MODE=UNCOND,                        **01059000
               RELATED=('REQUIRED BY TQE DEQ',(IEAVRTOD,(NOLABEL)))     01060000
         BALR  REG2,REG15     DEQUEUE THE MIDNIGHT TQE                  01061000
*             REG1->TQEVALLH=0;     /* SET TQEVAL OF MIDNIGHT = 0...    01062000
*                                                            @YM06393*/ 01063000
         SLR   @07,@07                                             0317 01064000
         ST    @07,TQEVALLH(,REG1)                                 0317 01065000
*             REG1->TQEVALRH=0;     /* ...TO NOP TIME SVC    @YM06393*/ 01066000
         ST    @07,TQEVALRH(,REG1)                                 0318 01067000
*             LCVTPTR=CVTPTR;       /* RESTORE CVT ADDRESSABILITY    */ 01068000
         L     LCVTPTR,CVTPTR                                      0319 01069000
*             REG1=ADDR(TPCLMTQE);  /* GET ADDR OF JOB STEP TIMING 0320 01070000
*                                      TQE                           */ 01071000
         L     REG1,TPCPTR(,LCVTPTR)                               0320 01072000
         LA    REG1,TPCLMTQE(,REG1)                                0320 01073000
*             REG15=ADDR(DEQTQE);   /* GET ADDR OF DEQ ROUTINE       */ 01074000
         L     REG15,CVTQTD00(,LCVTPTR)                            0321 01075000
*             GENERATE CODE REFS(REG15,PSALITA) SETS(REG2);        0322 01076000
         BALR  REG2,REG15     DEQUEUE THE JOB STEP TIMING TQE           01077000
         SETLOCK RELEASE,TYPE=DISP,                                   **01078000
               RELATED=('REQUIRED BY TQE DEQ',(IEAVRTOD,(NOLABEL)))     01079000
*             LCVTPTR=CVTPTR;       /* RESTORE CVT ADDRESSABILITY    */ 01080000
         L     LCVTPTR,CVTPTR                                      0323 01081000
*             REG13=R13SAV;         /* RESTORE SAVE AREA POINTER     */ 01082000
         L     REG13,R13SAV                                        0324 01083000
*/********************************************************************/ 01084000
*/*                                                                  */ 01085000
*/* INITIALIZE THE FIRST TOD CLOCK WORKAREA ENTRY FOR THE NEW CPU,   */ 01086000
*/* SET THE COUNT OF CPU'S TO 1, AND CALL THE TESTING ROUTINE TO     */ 01087000
*/* TEST THE STATUS OF THE NEW CLOCK BY ITSELF.                      */ 01088000
*/*                                                                  */ 01089000
*/********************************************************************/ 01090000
*                                                                  0325 01091000
*             REG11=PTREBEG;        /* GET PTR TO FIRST ENTRY        */ 01092000
         L     REG11,PTREBEG                                       0325 01093000
*             TCWAPCCA=ADDR(NEWPCCA);/* FILL IN PCCA ADDRESS         */ 01094000
         L     @07,@PC00001                                        0326 01095000
         ST    @07,TCWAPCCA(,REG11)                                0326 01096000
*             TCWAIADD=ADDR(NEWPCCA)->PCCACPUA;/* FILL IN CPU ADDRESS*/ 01097000
         MVC   TCWAIADD(2,REG11),PCCACPUA(@07)                     0327 01098000
*             TCCPUCNT=1;                                          0328 01099000
         MVC   TCCPUCNT(2,TCWAPTR),@CH00096                        0328 01100000
*             CALL IEAVRTST;        /* CALL SYNCHRONIZATION TESTING     01101000
*                                      ROUTINE                       */ 01102000
         BAL   @14,IEAVRTST                                        0329 01103000
*/********************************************************************/ 01104000
*/*                                                                  */ 01105000
*/* TEST FOR A RETURN CODE OF 0 INDICATING THAT THE CLOCK IS SET.    */ 01106000
*/*                                                                  */ 01107000
*/********************************************************************/ 01108000
*                                                                  0330 01109000
*             IF RETCODE=0 THEN                                    0330 01110000
*                                                                  0330 01111000
         LTR   RETCODE,RETCODE                                     0330 01112000
         BNZ   @RF00330                                            0330 01113000
*/********************************************************************/ 01114000
*/*                                                                  */ 01115000
*/* IF THE RETURN CODE IS 0, INDICATE THAT MESSAGE IEA888A SHOULD    */ 01116000
*/* BE ISSUED.                                                       */ 01117000
*/*                                                                  */ 01118000
*/********************************************************************/ 01119000
*                                                                  0331 01120000
*               TCGXXX='1'B;                                       0331 01121000
         OI    TCGXXX(TCWAPTR),B'10000000'                         0331 01122000
*             ELSE                                                 0332 01123000
*                                                                  0332 01124000
*/********************************************************************/ 01125000
*/*                                                                  */ 01126000
*/* IF THE RETURN CODE IS NOT 0, INDICATE THAT MESSAGE IEA886A       */ 01127000
*/* SHOULD BE ISSUED.                                                */ 01128000
*/*                                                                  */ 01129000
*/********************************************************************/ 01130000
*                                                                  0332 01131000
*               TCGYYY='1'B;                                       0332 01132000
*                                                                  0332 01133000
         B     @RC00330                                            0332 01134000
@RF00330 OI    TCGYYY(TCWAPTR),B'01000000'                         0332 01135000
*/********************************************************************/ 01136000
*/*                                                                  */ 01137000
*/* CALL THE COMMUNICATIONS ROUTINE TO ISSUE THE INDICATED MESSAGE.  */ 01138000
*/*                                                                  */ 01139000
*/********************************************************************/ 01140000
*                                                                  0333 01141000
*             CALL IEAVRCOM;                                       0333 01142000
*                                                                  0333 01143000
@RC00330 BAL   @14,IEAVRCOM                                        0333 01144000
*/********************************************************************/ 01145000
*/*                                                                  */ 01146000
*/* CALL THE SYNCHRONIZATION TESTING ROUTINE TO ENSURE THAT THE CLOCK*/ 01147000
*/* IS STILL SET, SINCE THE OPERATOR MAY HAVE CHANGED IT.  (NOTE:    */ 01148000
*/* THE RETURN CODE FROM THIS TEST CAN ONLY BE 0 OR 4.)              */ 01149000
*/*                                                                  */ 01150000
*/********************************************************************/ 01151000
*                                                                  0334 01152000
*             CALL IEAVRTST;                                       0334 01153000
*                                                                  0334 01154000
         BAL   @14,IEAVRTST                                        0334 01155000
*/********************************************************************/ 01156000
*/*                                                                  */ 01157000
*/* SEE IF THE RETURN CODE IS NON-0, AND IF IT IS SET IT TO 16.      */ 01158000
*/* (NOTE: THE ONLY POSSIBLE NON-0 RETURN CODE AT THIS POINT IS 4,   */ 01159000
*/* WHICH WILL CAUSE RE-CHECKING FOR ANOTHER ONLINE CPU HAVING A     */ 01160000
*/* CLOCK THAT MAY BE SYNCHRONIZED TO.  THIS MUST BE AVOIDED BECAUSE */ 01161000
*/* THE PRESENT PATH WAS TAKEN WHEN IT WAS FOUND THAT THERE WERE     */ 01162000
*/* NO MORE ONLINE CPU'S.)                                           */ 01163000
*/*                                                                  */ 01164000
*/********************************************************************/ 01165000
*                                                                  0335 01166000
*             IF RETCODE^=0 THEN                                   0335 01167000
         LTR   RETCODE,RETCODE                                     0335 01168000
         BZ    @RF00335                                            0335 01169000
*               RETCODE=16;                                        0336 01170000
         LA    RETCODE,16                                          0336 01171000
*             ELSE                                                 0337 01172000
*                                                                  0337 01173000
*/********************************************************************/ 01174000
*/*                                                                  */ 01175000
*/* IF THE RETURN CODE IS 0, THE NEW CLOCK IS ALL RIGHT, SO TURN ON  */ 01176000
*/* THE FLAG WHICH INDICATES THAT THE MIDNIGHT AND JOB STEP TIMING   */ 01177000
*/* TQES SHOULD BE RE-ENQUEUED, AND THAT THE SRM SHOULD BE NOTIFIED. */ 01178000
*/*                                                                  */ 01179000
*/********************************************************************/ 01180000
*                                                                  0337 01181000
*               TCGTQE='1'B;                                       0337 01182000
         B     @RC00335                                            0337 01183000
@RF00335 OI    TCGTQE(TCWAPTR),B'00000001'                         0337 01184000
*           END;                                                   0338 01185000
@RC00335 DS    0H                                                  0339 01186000
*       END;                                                       0339 01187000
@RC00301 DS    0H                                                  0339 01188000
@DE00284 CH    RETCODE,@CH00108                                    0339 01189000
         BE    @DL00284                                            0339 01190000
*                                                                  0340 01191000
*/********************************************************************/ 01192000
*/*                                                                  */ 01193000
*/* IF AN APPARENTLY GOOD CLOCK WAS FOUND, SEE IF THE TESTING ROUTINE*/ 01194000
*/* SENT BACK A RETURN CODE OF 8, INDICATING THAT A SYNC CHECK OC-   */ 01195000
*/* CURED.                                                           */ 01196000
*/*                                                                  */ 01197000
*/********************************************************************/ 01198000
*                                                                  0340 01199000
*       IF RETCODE=8 THEN                                          0340 01200000
*                                                                  0340 01201000
         CH    RETCODE,@CH00038                                    0340 01202000
         BNE   @RF00340                                            0340 01203000
*/********************************************************************/ 01204000
*/*                                                                  */ 01205000
*/* IF THAT IS THE CASE, SEARCH ALL ONLINE CPU'S FOR ONE WHICH IS NOT*/ 01206000
*/* THE NEW CPU, AND WHOSE CLOCK IS MARKED AS NOT USEABLE.           */ 01207000
*/*                                                                  */ 01208000
*/********************************************************************/ 01209000
*                                                                  0341 01210000
*         DO;                                                      0341 01211000
*           MASKHLDR=CSDCPUAL;      /* GET THE MASK OF ALIVE CPU'S   */ 01212000
         L     @07,CVTCSD(,LCVTPTR)                                0342 01213000
         SLR   MASKHLDR,MASKHLDR                                   0342 01214000
         ICM   MASKHLDR,3,CSDCPUAL(@07)                            0342 01215000
*           REG12=CVTPCCAT+LENGTH(PCCAVT)-LENGTH(PCCAT00P);/* GET PTR   01216000
*                                      TO LAST PCCAVT ENTRY          */ 01217000
         L     REG12,CVTPCCAT(,LCVTPTR)                            0343 01218000
         SH    REG12,@CH02877                                      0343 01219000
*           DO REG1=LENGTH(CSDCPUAL) TO 1 BY-1;                    0344 01220000
         LA    REG1,16                                             0344 01221000
@DL00344 DS    0H                                                  0345 01222000
*             IF MASKHLDR//2^=0&PTRPCCA^=ADDR(NEWPCCA)&PCCANUTD='1'B    01223000
*               THEN                                               0345 01224000
*                                                                  0345 01225000
         LR    @07,MASKHLDR                                        0345 01226000
         N     @07,@CF00096                                        0345 01227000
         LTR   @07,@07                                             0345 01228000
         BZ    @RF00345                                            0345 01229000
         L     @07,PTRPCCA(,REG12)                                 0345 01230000
         L     @06,@PC00001                                        0345 01231000
         CR    @07,@06                                             0345 01232000
         BE    @RF00345                                            0345 01233000
         TM    PCCANUTD(@07),B'10000000'                           0345 01234000
         BNO   @RF00345                                            0345 01235000
*/********************************************************************/ 01236000
*/*                                                                  */ 01237000
*/* IF ONE IS FOUND, ASSUME THAT IT CAUSED THE SYNC CHECK.  ZERO THE */ 01238000
*/* LOOP CONTROL TO EXIT FROM THE LOOP, AND SET THE RETURN CODE FOR  */ 01239000
*/* THE CALLER TO 0 TO INDICATE SUCCESS.                             */ 01240000
*/*                                                                  */ 01241000
*/********************************************************************/ 01242000
*                                                                  0346 01243000
*               DO;                                                0346 01244000
*                 REG1=0;                                          0347 01245000
         SLR   REG1,REG1                                           0347 01246000
*                 RETCODE=0;                                       0348 01247000
         SLR   RETCODE,RETCODE                                     0348 01248000
*               END;                                               0349 01249000
*             MASKHLDR=MASKHLDR/2;  /* GET NEXT BIT TO TEST          */ 01250000
@RF00345 SRL   MASKHLDR,1                                          0350 01251000
*             REG12=REG12-LENGTH(PCCAT00P);/* POINT TO NEXT PCCAVT 0351 01252000
*                                      ENTRY                         */ 01253000
         SH    REG12,@CH00108                                      0351 01254000
*           END;                                                   0352 01255000
         BCTR  REG1,0                                              0352 01256000
         LTR   REG1,REG1                                           0352 01257000
         BP    @DL00344                                            0352 01258000
*         END;                                                     0353 01259000
*                                                                  0353 01260000
*/********************************************************************/ 01261000
*/*                                                                  */ 01262000
*/* SAVE THE RETURN CODE TO PASS BACK TO THE CALLER.  THEN GET THE   */ 01263000
*/* ADDRESS OF THE ESTAE PARAMETER LIST FOR COMPATABILITY WITH AN    */ 01264000
*/* ENTRY FROM THE ESTAE EXIT AT SSCRETRY.                           */ 01265000
*/*                                                                  */ 01266000
*/********************************************************************/ 01267000
*                                                                  0354 01268000
*       RCSAVE=RETCODE;                                            0354 01269000
@RF00340 ST    RETCODE,RCSAVE                                      0354 01270000
*       REG1=ADDR(SSCRPARM);                                       0355 01271000
*                                                                  0355 01272000
         LA    REG1,SSCRPARM                                       0355 01273000
*/********************************************************************/ 01274000
*/*                                                                  */ 01275000
*/* RESTORE BASE REGS, DATA REG, AND CVT POINTER FOR POSSIBLE ENTRY  */ 01276000
*/* FROM THE ESTAE EXIT.                                             */ 01277000
*/*                                                                  */ 01278000
*/********************************************************************/ 01279000
*                                                                  0356 01280000
*SSCRETRY:                                                         0356 01281000
*       GEN CODE REFS(REG1) SETS(REG8,REG9,REG10)                  0356 01282000
*           (LM    REG8,REG10,0(REG1));                            0356 01283000
SSCRETRY LM    REG8,REG10,0(REG1)                                       01284000
*       LCVTPTR=CVTPTR;                                            0357 01285000
*                                                                  0357 01286000
         L     LCVTPTR,CVTPTR                                      0357 01287000
*/********************************************************************/ 01288000
*/*                                                                  */ 01289000
*/* TEST THE CONTENTS OF THE RETURN CODE FOR 0.  IT WILL BE 0 IF     */ 01290000
*/* THE NEW CLOCK WAS SUCCESSFULLY SET OR SYNCHRONIZED, AND NON 0 IF */ 01291000
*/* IT WAS NOT, OR IF ENTRY HERE IS FROM THE ESTAE EXIT.             */ 01292000
*/*                                                                  */ 01293000
*/********************************************************************/ 01294000
*                                                                  0358 01295000
*       IF RCSAVE^=0 THEN                                          0358 01296000
*                                                                  0358 01297000
         SLR   @07,@07                                             0358 01298000
         C     @07,RCSAVE                                          0358 01299000
         BE    @RF00358                                            0358 01300000
*/********************************************************************/ 01301000
*/*                                                                  */ 01302000
*/* IF THE RETURN CODE IS NOT 0, SEE IF THE CALLER WAS THE VARY      */ 01303000
*/* COMMAND PROCESSOR.                                               */ 01304000
*/*                                                                  */ 01305000
*/********************************************************************/ 01306000
*                                                                  0359 01307000
*         DO;                                                      0359 01308000
*           IF CALLERID=0 THEN                                     0360 01309000
*                                                                  0360 01310000
         CH    @07,CALLERID                                        0360 01311000
         BNE   @RF00360                                            0360 01312000
*/********************************************************************/ 01313000
*/*                                                                  */ 01314000
*/* IF THE CALLER IS VARY, INDICATE THAT THE VARY PROCESS SHOULD BE  */ 01315000
*/* STOPPED, AND RELEASE THE NEW CPU FROM ITS SPIN.                  */ 01316000
*/*                                                                  */ 01317000
*/********************************************************************/ 01318000
*                                                                  0361 01319000
*             DO;                                                  0361 01320000
*               ADDR(NEWPCCA)->PCCAVKIL='1'B;                      0362 01321000
         L     @07,@PC00001                                        0362 01322000
         OI    PCCAVKIL(@07),B'00100000'                           0362 01323000
*               ADDR(NEWPCCA)->PCCAPSAV->PSALCCAV->LCCATIMR='0'B;  0363 01324000
         L     @07,PCCAPSAV(,@07)                                  0363 01325000
         L     @07,PSALCCAV(,@07)                                  0363 01326000
         NI    LCCATIMR(@07),B'11101111'                           0363 01327000
*             END;                                                 0364 01328000
*                                                                  0364 01329000
*/********************************************************************/ 01330000
*/*                                                                  */ 01331000
*/* IF THE CALLER IS HARDWARE RECOVERY, SEE IF THE MIDNIGHT AND      */ 01332000
*/* JOBSTEP TIMING TQE'S WERE DEQUEUED BECAUSE THERE WERE NO GOOD    */ 01333000
*/* TOD CLOCKS IN THE CONFIGURATION.  IF SO, SET THE VAULE OF THE    */ 01334000
*/* MIDNIGHT TQEVAL FIELD TO 1.048576 SECONDS SO THAT THE TIME SVC   */ 01335000
*/* ROUTINE WILL NOT NOP.                                            */ 01336000
*/*                                                                  */ 01337000
*/********************************************************************/ 01338000
*                                                                  0365 01339000
*           ELSE                    /*                       @YM06393*/ 01340000
*             DO;                   /*                       @YM06393*/ 01341000
         B     @RC00360                                            0365 01342000
@RF00360 DS    0H                                                  0366 01343000
*               IF TCGTQE='1'B THEN /* IF TQE'S DEQUEUED...  @YM06393*/ 01344000
         TM    TCGTQE(TCWAPTR),B'00000001'                         0366 01345000
         BNO   @RF00366                                            0366 01346000
*                 MNIGHTLH=1;       /* ...SET MIDNIGHT TQEVAL TO   0367 01347000
*                                      NON-0                 @YM06393*/ 01348000
         L     @07,TPCPTR(,LCVTPTR)                                0367 01349000
         MVC   MNIGHTLH(4,@07),@CF00096                            0367 01350000
*             END;                  /*                       @YM06393*/ 01351000
*         END;                                                     0369 01352000
*       ELSE                                                       0370 01353000
*                                                                  0370 01354000
*/********************************************************************/ 01355000
*/*                                                                  */ 01356000
*/* IF THE RETURN CODE WAS 0, SEE IF THE JOB STEP TIMING AND MIDNIGHT*/ 01357000
*/* TQES NEED TO BE RE-ENQUEUED, AND THE SRM NOTIFIED THAT THERE IS  */ 01358000
*/* ONCE AGAIN A GOOD TOD CLOCK AND CLOCK COMPARATOR. IF SO, CALL THE*/ 01359000
*/* ROUTINE TO PERFORM THESE ACTIONS.                                */ 01360000
*/*                                                                  */ 01361000
*/********************************************************************/ 01362000
*                                                                  0370 01363000
*         DO;                                                      0370 01364000
         B     @RC00358                                            0370 01365000
@RF00358 DS    0H                                                  0371 01366000
*           IF TCGTQE='1'B THEN                                    0371 01367000
         TM    TCGTQE(TCWAPTR),B'00000001'                         0371 01368000
         BNO   @RF00371                                            0371 01369000
*             CALL TQEINIT;                                        0372 01370000
*                                                                  0372 01371000
         LA    @15,TQEINIT                                         0372 01372000
         BALR  @14,@15                                             0372 01373000
*/********************************************************************/ 01374000
*/*                                                                  */ 01375000
*/* GET THE DISPATCHER LOCK FOR SETTING THE TPCABND FLAG, AND        */ 01376000
*/* INVOKING THE SET CC ROUTINE.                                     */ 01377000
*/*                                                                  */ 01378000
*/********************************************************************/ 01379000
*                                                                  0373 01380000
*           GENERATE CODE REFS(PSALITA);                           0373 01381000
*                                                                  0373 01382000
@RF00371 DS    0H                                                  0373 01383000
*                                    /*                                 01384000
         SETLOCK  OBTAIN,TYPE=DISP,MODE=UNCOND,                       **01385000
               RELATED=('REAL TQE QUEUE',(IEAVRTOD,(NOLABEL)))  */      01386000
*/********************************************************************/ 01387000
*/*                                                                  */ 01388000
*/* SEE IF THE CALLER WAS VARY.                                      */ 01389000
*/*                                                                  */ 01390000
*/********************************************************************/ 01391000
*                                                                  0374 01392000
*           IF CALLERID=0 THEN                                     0374 01393000
*                                                                  0374 01394000
         ICM   @07,3,CALLERID                                      0374 01395000
         BNZ   @RF00374                                            0374 01396000
*/********************************************************************/ 01397000
*/*                                                                  */ 01398000
*/* IF THE CALLER IS VARY, INITIALIZE THE TIMER STATUS BYTES IN THE  */ 01399000
*/* PCCA FOR THE NEW CPU.                                            */ 01400000
*/*                                                                  */ 01401000
*/********************************************************************/ 01402000
*                                                                  0375 01403000
*             DO;                                                  0375 01404000
*               ADDR(NEWPCCA)->PCCATMST=TMSTINIT;                  0376 01405000
*                                                                  0376 01406000
         L     @07,@PC00001                                        0376 01407000
         MVC   PCCATMST(4,@07),TMSTINIT                            0376 01408000
*/********************************************************************/ 01409000
*/*                                                                  */ 01410000
*/* ADD 1 TO THE COUNT OF GOOD TOD CLOCKS IN THE CSD.                */ 01411000
*/*                                                                  */ 01412000
*/********************************************************************/ 01413000
*                                                                  0377 01414000
*               CURRENT=CSDGDTOD;                                  0377 01415000
         L     @07,CVTCSD(,LCVTPTR)                                0377 01416000
         L     CURRENT,CSDGDTOD(,@07)                              0377 01417000
*ADD1:                                                             0378 01418000
*               NEW=CURRENT+1;      /* SET UP FOR COMPARE AND SWAP   */ 01419000
ADD1     LA    NEW,1                                               0378 01420000
         ALR   NEW,CURRENT                                         0378 01421000
*               CS(CURRENT,NEW,CSDGDTOD);                          0379 01422000
         L     @07,CVTCSD(,LCVTPTR)                                0379 01423000
         CS    CURRENT,@01,CSDGDTOD(@07)                           0379 01424000
*               BC(7,ADD1);                                        0380 01425000
*                                                                  0380 01426000
         BC    7,ADD1                                              0380 01427000
*/********************************************************************/ 01428000
*/*                                                                  */ 01429000
*/* ADD 1 TO THE COUNT OF GOOD CLOCK COMPARATORS IN THE CSD.         */ 01430000
*/*                                                                  */ 01431000
*/********************************************************************/ 01432000
*                                                                  0381 01433000
*               CURRENT=CSDGDCC;                                   0381 01434000
         L     @07,CVTCSD(,LCVTPTR)                                0381 01435000
         L     CURRENT,CSDGDCC(,@07)                               0381 01436000
*ADD7:                                                             0382 01437000
*               NEW=CURRENT+1;      /* SET UP FOR COMPARE AND SWAP   */ 01438000
ADD7     LA    NEW,1                                               0382 01439000
         ALR   NEW,CURRENT                                         0382 01440000
*               CS(CURRENT,NEW,CSDGDCC);                           0383 01441000
         L     @07,CVTCSD(,LCVTPTR)                                0383 01442000
         CS    CURRENT,@01,CSDGDCC(@07)                            0383 01443000
*               BC(7,ADD7);                                        0384 01444000
*                                                                  0384 01445000
         BC    7,ADD7                                              0384 01446000
*/********************************************************************/ 01447000
*/*                                                                  */ 01448000
*/* ADD 1 TO THE COUNT OF GOOD CPU TIMERS IN THE CSD.                */ 01449000
*/*                                                                  */ 01450000
*/********************************************************************/ 01451000
*                                                                  0385 01452000
*               CURRENT=CSDGDINT;                                  0385 01453000
         L     @07,CVTCSD(,LCVTPTR)                                0385 01454000
         L     CURRENT,CSDGDINT(,@07)                              0385 01455000
*ADD8:                                                             0386 01456000
*               NEW=CURRENT+1;      /* SET UP FOR COMPARE AND SWAP   */ 01457000
ADD8     LA    NEW,1                                               0386 01458000
         ALR   NEW,CURRENT                                         0386 01459000
*               CS(CURRENT,NEW,CSDGDINT);                          0387 01460000
         L     @07,CVTCSD(,LCVTPTR)                                0387 01461000
         CS    CURRENT,@01,CSDGDINT(@07)                           0387 01462000
*               BC(7,ADD8);                                        0388 01463000
*                                                                  0388 01464000
         BC    7,ADD8                                              0388 01465000
*/********************************************************************/ 01466000
*/*                                                                  */ 01467000
*/* INDICATE THAT TASKS WITH REAL TQE'S NO LONGER NEED BE ABEND'ED   */ 01468000
*/* SINCE THERE IS NOW A GOOD TOD CLOCK AND CLOCK COMPARATOR IN THE  */ 01469000
*/* SYSTEM.  (NOTE: UNLIKE RELEASE 2, THE NEW CPU IS NOT RELEASED    */ 01470000
*/* FROM ITS SPIN WHEN THE SETTING/SYNCHRONIZING PROCEDURE IS SUC-   */ 01471000
*/* CESSFUL.  THE RELEASE WILL BE PERFORMED BY THE VARY CPU COMMAND  */ 01472000
*/* PROCESSOR.)                                                      */ 01473000
*/*                                                                  */ 01474000
*/********************************************************************/ 01475000
*/* D207820                                                  @Y30CQYA*/ 01476000
*                                                                  0389 01477000
*               TPCABND='0'B;                                      0389 01478000
         L     @07,TPCPTR(,LCVTPTR)                                0389 01479000
         NI    TPCABND(@07),B'01111111'                            0389 01480000
*             END;                                                 0390 01481000
*                                                                  0390 01482000
*/********************************************************************/ 01483000
*/*                                                                  */ 01484000
*/* MAKE SURE THAT THE QUEUE OF REAL TIME TQE'S IS KEPT ALIVE.       */ 01485000
*/*                                                                  */ 01486000
*/********************************************************************/ 01487000
*                                                                  0391 01488000
*           REG12=TPCCKQ;                                          0391 01489000
@RF00374 L     @07,TPCPTR(,LCVTPTR)                                0391 01490000
         L     REG12,TPCCKQ(,@07)                                  0391 01491000
*           GENERATE CODE REFS(REG12) SETS(REG2)(BALR  REG2,REG12);     01492000
         BALR  REG2,REG12                                               01493000
*           LCVTPTR=CVTPTR;         /* RESTORE THE CVT BASE          */ 01494000
         L     LCVTPTR,CVTPTR                                      0393 01495000
*/********************************************************************/ 01496000
*/*                                                                  */ 01497000
*/* RELEASE THE DISPATCHER LOCK.                                     */ 01498000
*/*                                                                  */ 01499000
*/********************************************************************/ 01500000
*                                                                  0394 01501000
*           GENERATE CODE REFS(PSALITA);                           0394 01502000
*                                   /*                                  01503000
         SETLOCK RELEASE,TYPE=DISP,                                   **01504000
               RELATED=('REAL TQE QUEUE',(IEAVRTOD,(NOLABEL)))  */      01505000
*         END;                                                     0395 01506000
*                                                                  0396 01507000
*/********************************************************************/ 01508000
*/*                                                                  */ 01509000
*/* ENSURE THAT EXTERNAL SET COMMANDS ARE NOW ALLOWED.               */ 01510000
*/*                                                                  */ 01511000
*/********************************************************************/ 01512000
*                                                                  0396 01513000
*       BAIN='1'B;                                                 0396 01514000
*                                                                  0396 01515000
@RC00358 L     @07,CVTMSER(,LCVTPTR)                               0396 01516000
         OI    BAIN(@07),B'10000000'                               0396 01517000
*/********************************************************************/ 01518000
*/*                                                                  */ 01519000
*/* UNCONDITIONALLY FREE THE TIME OF DAY CLOCK WORKAREA.             */ 01520000
*/*                                                                  */ 01521000
*/********************************************************************/ 01522000
*                                                                  0397 01523000
*       REG0=LENGTH(TCWA)+(2*LENGTH(TCENTRY));/* GET LENGTH OF TCWA  */ 01524000
         LA    REG0,144                                            0397 01525000
*       REG1=TCWAPTR;               /* GET ADDRESS OF TCWA           */ 01526000
         LR    REG1,TCWAPTR                                        0398 01527000
*       GENERATE CODE                                              0399 01528000
*           (FREEMAIN RU,LV=(0),A=(1),SP=245       FREE WORKAREA); 0399 01529000
*                                                                  0399 01530000
         FREEMAIN RU,LV=(0),A=(1),SP=245       FREE WORKAREA            01531000
*/********************************************************************/ 01532000
*/*                                                                  */ 01533000
*/* FREE THE PAGE WITH THE MODULE AND ZERO THE POINTER IN THE TPC TO */ 01534000
*/* THE TOD CLOCK WORKAREA.                                          */ 01535000
*/*                                                                  */ 01536000
*/********************************************************************/ 01537000
*                                                                  0400 01538000
*       REG1=ADDR(BEGTOD);          /* GET POINTER TO START OF MODULE*/ 01539000
         LA    REG1,BEGTOD                                         0400 01540000
*       REG15=ADDR(ENDTOD);         /* GET POINTER TO END OF MODULE  */ 01541000
         LA    REG15,ENDTOD                                        0401 01542000
*       GENERATE CODE(PGFREE R,A=(1),EA=(15));                     0402 01543000
         PGFREE R,A=(1),EA=(15)                                         01544000
*       TPCTCWA=0;                                                 0403 01545000
         L     @07,TPCPTR(,LCVTPTR)                                0403 01546000
         SLR   @06,@06                                             0403 01547000
         ST    @06,TPCTCWA(,@07)                                   0403 01548000
*     END;                                                         0404 01549000
*   ELSE                                                           0405 01550000
*                                                                  0405 01551000
*/********************************************************************/ 01552000
*/*                                                                  */ 01553000
*/* IF THE GETMAIN FOR THE WORKAREA WAS UNSUCCESSFUL, SAVE THE RE-   */ 01554000
*/* TURN CODE AS A FAILURE INDICATOR TO THE CALLER.                  */ 01555000
*/*                                                                  */ 01556000
*/********************************************************************/ 01557000
*                                                                  0405 01558000
*     DO;                                                          0405 01559000
         B     @RC00264                                            0405 01560000
@RF00264 DS    0H                                                  0406 01561000
*       RCSAVE=RETCODE;                                            0406 01562000
*                                                                  0406 01563000
         ST    RETCODE,RCSAVE                                      0406 01564000
*/********************************************************************/ 01565000
*/*                                                                  */ 01566000
*/* SEE IF THE CALLER IS VARY.                                       */ 01567000
*/*                                                                  */ 01568000
*/********************************************************************/ 01569000
*                                                                  0407 01570000
*       IF CALLERID=0 THEN                                         0407 01571000
*                                                                  0407 01572000
         ICM   @07,3,CALLERID                                      0407 01573000
         BNZ   @RF00407                                            0407 01574000
*/********************************************************************/ 01575000
*/*                                                                  */ 01576000
*/* IF THE CALLER IS VARY, INDICATE THAT THE PROCESS SHOULD BE       */ 01577000
*/* KILLED, AND RELEASE THE NEW CPU FROM ITS SPIN.                   */ 01578000
*/*                                                                  */ 01579000
*/********************************************************************/ 01580000
*                                                                  0408 01581000
*         DO;                                                      0408 01582000
*           ADDR(NEWPCCA)->PCCAVKIL='1'B;                          0409 01583000
         L     @07,@PC00001                                        0409 01584000
         OI    PCCAVKIL(@07),B'00100000'                           0409 01585000
*           ADDR(NEWPCCA)->PCCAPSAV->PSALCCAV->LCCATIMR='0'B;      0410 01586000
         L     @07,PCCAPSAV(,@07)                                  0410 01587000
         L     @07,PSALCCAV(,@07)                                  0410 01588000
         NI    LCCATIMR(@07),B'11101111'                           0410 01589000
*         END;                                                     0411 01590000
*     END;                                                         0412 01591000
*                                                                  0412 01592000
@RF00407 DS    0H                                                  0413 01593000
*/********************************************************************/ 01594000
*/*                                                                  */ 01595000
*/* DELETE ERROR EXIT FOR UNLOCKED CODE.                             */ 01596000
*/*                                                                  */ 01597000
*/********************************************************************/ 01598000
*                                                                  0413 01599000
*   GEN CODE(ESTAE 0);                                             0413 01600000
*                                                                  0413 01601000
@RC00264 DS    0H                                                  0413 01602000
         ESTAE 0                                                        01603000
*/********************************************************************/ 01604000
*/*                                                                  */ 01605000
*/* RESTORE THE SAVEAREA ADDRESS, AND RETURN TO THE CALLER WITH A    */ 01606000
*/* RETURN CODE IN REGISTER 15.                                      */ 01607000
*/*                                                                  */ 01608000
*/********************************************************************/ 01609000
*                                                                  0414 01610000
*   REG13=R13SAV;                                                  0414 01611000
         L     REG13,R13SAV                                        0414 01612000
*   RETURN CODE(RCSAVE);                                           0415 01613000
         L     @07,RCSAVE                                          0415 01614000
         L     @13,4(,@13)                                         0415 01615000
         L     @00,@SIZDATD                                        0415 01616000
         LR    @01,@08                                             0415 01617000
         FREEMAIN R,LV=(0),A=(1)                                        01618000
         LR    @15,@07                                             0415 01619000
         L     @14,12(,@13)                                        0415 01620000
         LM    @00,@12,20(@13)                                     0415 01621000
         BR    @14                                                 0415 01622000
*                                                                  0416 01623000
*/********************************************************************/ 01624000
*/*                                                                  */ 01625000
*/*             TQEINIT--TQE INITIALIZATION ROUTINE                  */ 01626000
*/*                                                                  */ 01627000
*/* THIS ROUTINE RECEIVES CONTROL FROM IEAVRINT AFTER THE TOD        */ 01628000
*/* CLOCK(S) HAVE BEEN INITIALIZED, AND FROM IEAVRSSC WHEN THE CLOCK */ 01629000
*/* IN THE CPU BEING VARY'ED ONLINE OR IN THE CPU WITH THE FAILING   */ 01630000
*/* CLOCK HAD TO BE SET BECAUSE THERE WAS NO OTHER CLOCK ALREADY IN  */ 01631000
*/* THE CONFIGURATION WITH WHICH TO SYNCHRONIZE IT.  THE DISPATCHER  */ 01632000
*/* LOCK IS HELD UPON ENTRY.                                         */ 01633000
*/*                                                                  */ 01634000
*/* IT CALCULATES THE INITIAL VALUES FOR THE MIDNIGHT AND JOB STEP   */ 01635000
*/* TIMING TQES AND ENQUEUES THEM, AND THEN NOTIFIES THE SRM.        */ 01636000
*/*                                                                  */ 01637000
*/********************************************************************/ 01638000
*                                                                  0416 01639000
*TQEINIT:                                                          0416 01640000
*   PROCEDURE OPTIONS(NOSAVE,NOSAVEAREA,ENTREG,RETREG,NOPARMREG);  0416 01641000
*                                                                  0416 01642000
TQEINIT  DS    0H                                                  0417 01643000
*/********************************************************************/ 01644000
*/*                                                                  */ 01645000
*/* GET THE NUMBER OF DAYS (LOCAL) ELAPSED SINCE THE EPOCH PLUS 1.   */ 01646000
*/*                                                                  */ 01647000
*/********************************************************************/ 01648000
*                                                                  0417 01649000
*   RETSAVE=REG14;                  /* SAVE RETURN ADDRESS           */ 01650000
         ST    REG14,RETSAVE                                       0417 01651000
*   TMICSECR=TCELDAYS;                                             0418 01652000
*                                                                  0418 01653000
         L     TMICSECR,TCELDAYS(,TCWAPTR)                         0418 01654000
*/********************************************************************/ 01655000
*/*                                                                  */ 01656000
*/* CALCULATE THE VALUE FOR THE MIDNIGHT TQE.                        */ 01657000
*/*                                                                  */ 01658000
*/********************************************************************/ 01659000
*                                                                  0419 01660000
*   GENERATE CODE REFS(MINSDAY,MICSMIN) SETS(TMICSECL,TMICSECR);   0419 01661000
*                                                                  0419 01662000
          MH    TMICSECR,MINSDAY    CONVERT DAYS TO MINUTES             01663000
          M     TMICSECL,MICSMIN    CONVERT MINUTES TO MICROSECONDS     01664000
          SLDL  TMICSECL,12         SHIFT MICROSECONDS TO BIT 51        01665000
*/********************************************************************/ 01666000
*/*                                                                  */ 01667000
*/* ADJUST CALCULATED VALUE BY TIME ZONE CONSTANT AND PUT INTO TQE.  */ 01668000
*/*                                                                  */ 01669000
*/********************************************************************/ 01670000
*                                                                  0420 01671000
*   TMICSECL=TMICSECL-CVTTZ;                                       0420 01672000
         SL    TMICSECL,CVTTZ(,LCVTPTR)                            0420 01673000
*   REG15=ADDR(TPCMNTQE);                                          0421 01674000
         L     REG15,TPCPTR(,LCVTPTR)                              0421 01675000
         LA    REG15,TPCMNTQE(,REG15)                              0421 01676000
*   STM(TMICSECL,TMICSECR,REG15->TQEVAL);                          0422 01677000
*                                                                  0422 01678000
         STM   TMICSECL,TMICSECR,TQEVAL(REG15)                     0422 01679000
*/********************************************************************/ 01680000
*/*                                                                  */ 01681000
*/* OBTAIN THE DISPATCHER LOCK FOR ENQUEING THE JOBTEP TIMING AND    */ 01682000
*/* MIDNIGHT TQES.                                                   */ 01683000
*/*                                                                  */ 01684000
*/********************************************************************/ 01685000
*                                                                  0423 01686000
*   GENERATE CODE REFS(PSALITA);                                   0423 01687000
*                                                                  0423 01688000
*                                 /*                                    01689000
         SETLOCK OBTAIN,TYPE=DISP,MODE=UNCOND,                        **01690000
               RELATED=('REQUIRED BY TQE ENQ',(IEAVRTOD,(NOLABEL))) */  01691000
*/********************************************************************/ 01692000
*/*                                                                  */ 01693000
*/* SET REGISTER 1 TO ADDRESS OF MIDNIGHT TQE, AND ENQUEUE THE TQE.  */ 01694000
*/*                                                                  */ 01695000
*/********************************************************************/ 01696000
*                                                                  0424 01697000
*   TQEPTR=REG15;                                                  0424 01698000
         LR    TQEPTR,REG15                                        0424 01699000
*   REG15=ADDR(ENQTQE);                                            0425 01700000
         L     REG15,CVTQTE00(,LCVTPTR)                            0425 01701000
*   GENERATE CODE REFS(REG15) SETS(REG2)(BALR  REG2,REG15);        0426 01702000
         BALR  REG2,REG15                                               01703000
*   LCVTPTR=CVTPTR;                 /* RE-ESTABLISH ADDRESSABILITY TO   01704000
*                                      THE CVT                       */ 01705000
         L     LCVTPTR,CVTPTR                                      0427 01706000
*/********************************************************************/ 01707000
*/*                                                                  */ 01708000
*/* GET THE ADDRESS OF THE JOBSTEP TIMING TQE, AND THEN TEST TO SEE  */ 01709000
*/* IF THIS IS EITHER A MULTIPROCESSOR OR HALF DUPLEX CONFIGURATION. */ 01710000
*/* IF SO, SIGNAL THE CPU REPRESENTED BY THE FIRST TCWA ENTRY TO     */ 01711000
*/* STORE ITS CLOCK INTO THE JOB STEP TIMING TQE.  (NOTE: THE CPU    */ 01712000
*/* REPRESENTED BY THE FIRST TCWA ENTRY IS SIGNALLED BECAUSE IF THE  */ 01713000
*/* THE ENTRY WAS FROM IEAVRSSC, THAT ENTRY REPRESENTS THE ONLY      */ 01714000
*/* VALID CLOCK IN THE SYSTEM.)                                      */ 01715000
*/*                                                                  */ 01716000
*/********************************************************************/ 01717000
*                                                                  0428 01718000
*   TQEPTR=ADDR(TPCLMTQE);                                         0428 01719000
         L     TQEPTR,TPCPTR(,LCVTPTR)                             0428 01720000
         LA    TQEPTR,TPCLMTQE(,TQEPTR)                            0428 01721000
*   IF CSDMP='1'B THEN                                             0429 01722000
         L     @07,CVTCSD(,LCVTPTR)                                0429 01723000
         TM    CSDMP(@07),B'10000000'                              0429 01724000
         BNO   @RF00429                                            0429 01725000
*     DO;                           /* IF NOT STRICTLY A           0430 01726000
*                                      UNIPROCESSOR,                 */ 01727000
*       REG11=ADDR(TQEVAL);         /* POINT PARMREG TO TQEVAL OF JST   01728000
*                                      TQE                           */ 01729000
         LA    REG11,TQEVAL(,TQEPTR)                               0431 01730000
*       REG12=ADDR(DELTA3);         /* GET ADDR OF ROUTINE TO STCK   */ 01731000
         LA    REG12,DELTA3                                        0432 01732000
*       REG1=PTREBEG->TCWAPCCA;     /* GET PCCA ADDR OF CPU TO SIGNAL*/ 01733000
         L     @07,PTREBEG                                         0433 01734000
         L     REG1,TCWAPCCA(,@07)                                 0433 01735000
*       GENERATE CODE REFS(CVTIPCRI,CVT)                           0434 01736000
*           (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));             0434 01737000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        01738000
*       TQEPTR=ADDR(TPCLMTQE);      /* RESTORE TQE POINTER           */ 01739000
         L     TQEPTR,TPCPTR(,LCVTPTR)                             0435 01740000
         LA    TQEPTR,TPCLMTQE(,TQEPTR)                            0435 01741000
*     END;                                                         0436 01742000
*   ELSE                                                           0437 01743000
*                                                                  0437 01744000
*/********************************************************************/ 01745000
*/*                                                                  */ 01746000
*/* IF STRICTLY UNIPROCESSOR, STCK INTO JOB STEP TIMING TQE.         */ 01747000
*/*                                                                  */ 01748000
*/********************************************************************/ 01749000
*                                                                  0437 01750000
*     DO;                                                          0437 01751000
         B     @RC00429                                            0437 01752000
@RF00429 DS    0H                                                  0438 01753000
*       GENERATE CODE REFS(TQEPTR) SETS(TQEVAL)(STCK  TQEVAL(TQEPTR));  01754000
         STCK  TQEVAL(TQEPTR)                                           01755000
*     END;                                                         0439 01756000
*                                                                  0439 01757000
*/********************************************************************/ 01758000
*/*                                                                  */ 01759000
*/* ADD 1 TO STORED VALUE  AND ENQUEUE THE TQE SO THAT JOB STEP      */ 01760000
*/* TIMING TQE WILL START INTERRUPTING.                              */ 01761000
*/*                                                                  */ 01762000
*/********************************************************************/ 01763000
*                                                                  0440 01764000
*   TQEVALLH=TQEVALLH+1;                                           0440 01765000
@RC00429 LA    @07,1                                               0440 01766000
         AL    @07,TQEVALLH(,TQEPTR)                               0440 01767000
         ST    @07,TQEVALLH(,TQEPTR)                               0440 01768000
*   REG15=ADDR(ENQTQE);                                            0441 01769000
         L     REG15,CVTQTE00(,LCVTPTR)                            0441 01770000
*   GENERATE CODE REFS(REG15) SETS(REG2)                           0442 01771000
*       (BALR  REG2,REG15     ENQUEUE THE JST TQE);                0442 01772000
         BALR  REG2,REG15     ENQUEUE THE JST TQE                       01773000
*   LCVTPTR=CVTPTR;                 /* RE-ESTABLISH CVT            0443 01774000
*                                      ADDRESSABILITY                */ 01775000
         L     LCVTPTR,CVTPTR                                      0443 01776000
*/********************************************************************/ 01777000
*/*                                                                  */ 01778000
*/* RELEASE THE DISPATCHER LOCK.                                     */ 01779000
*/*                                                                  */ 01780000
*/********************************************************************/ 01781000
*                                                                  0444 01782000
*   GENERATE CODE REFS(PSALITA);                                   0444 01783000
*                                                                  0444 01784000
*                                  /*                                   01785000
         SETLOCK RELEASE,TYPE=DISP,                                   **01786000
               RELATED=('REQUIRED BY TQE ENQ',(IEAVRTOD,(NOLABEL))) */  01787000
*/********************************************************************/ 01788000
*/*                                                                  */ 01789000
*/* GET THE ENTRY CODE, AND NOTIFY THE SRM THAT THE CLOCKS HAVE BEEN */ 01790000
*/* BEEN INITIALIZED/CHANGED.                                        */ 01791000
*/*                                                                  */ 01792000
*/********************************************************************/ 01793000
*                                                                  0445 01794000
*   REG1=1;                                                        0445 01795000
         LA    REG1,1                                              0445 01796000
*   GENERATE CODE(SYSEVENT TIMEREXP);                              0446 01797000
*                                                                  0446 01798000
         SYSEVENT TIMEREXP                                              01799000
*/********************************************************************/ 01800000
*/*                                                                  */ 01801000
*/* RESTORE RETURN ADDRESS AND RETURN TO CALLER.                     */ 01802000
*/*                                                                  */ 01803000
*/********************************************************************/ 01804000
*                                                                  0447 01805000
*   REG14=RETSAVE;                                                 0447 01806000
         L     REG14,RETSAVE                                       0447 01807000
*   END TQEINIT;                                                   0448 01808000
@EL00002 DS    0H                                                  0448 01809000
@EF00002 DS    0H                                                  0448 01810000
@ER00002 BR    @14                                                 0448 01811000
*   RESPECIFY                                                      0449 01812000
*    (LCVTPTR) UNRESTRICTED;                                       0449 01813000
*                                                                  0450 01814000
*/********************************************************************/ 01815000
*/*                                                                  */ 01816000
*/*        SSCESTAE--IEAVRSSC RECOVERY FOR UNLOCKED CODE             */ 01817000
*/*                                                                  */ 01818000
*/* THIS ROUTINE RECEIVES CONTROL WHEN AN ERROR OCCURS IN UNLOCKED   */ 01819000
*/* TOD SET SPECIFIC CLOCK CODE, OR WHEN PERCOLATED TO FROM AN       */ 01820000
*/* ERROR EXIT FOR CODE INVOKED BY TOD SET SPECIFIC CLOCK.  AFTER    */ 01821000
*/* ESTABLISHING ADDRESSABILITY, IT DETERMINES WHO IT WAS CALLED     */ 01822000
*/* BY, AND IF THE CALLER WAS IEEVCPU (FOR VARYING A CPU ONLINE) IT  */ 01823000
*/* SETS INDICATORS THAT WILL RELEASE THE CPU BEING VARY'ED ONLINE   */ 01824000
*/* FROM THE LOOP IT IS SPINNING IN, AND TERMINATE THE VARY PROCESS. */ 01825000
*/*                                                                  */ 01826000
*/********************************************************************/ 01827000
*                                                                  0450 01828000
*SSCESTAE:                                                         0450 01829000
*   PROC OPTIONS(NOSAVE,NOSAVEAREA,NOENTREG,RETREG);               0450 01830000
SSCESTAE DS    0H                                                  0451 01831000
*   RESPECIFY                                                      0451 01832000
*    (REG2) RESTRICTED;                                            0451 01833000
*   RESPECIFY                                                      0452 01834000
*     CVTMAP BASED(CVTPTR);                                        0452 01835000
*                                                                  0452 01836000
*/********************************************************************/ 01837000
*/*                                                                  */ 01838000
*/* ESTABLISH TEMPORARY ADDRESSABILITY.                              */ 01839000
*/*                                                                  */ 01840000
*/********************************************************************/ 01841000
*                                                                  0453 01842000
*   GEN CODE SETS(REG12) REFS(REG8,REG9,REG10);                    0453 01843000
*                                                                  0453 01844000
         BALR  REG12,0                                                  01845000
         USING *,REG12                                                  01846000
         DROP  REG8,REG9,REG10                                          01847000
*/********************************************************************/ 01848000
*/*                                                                  */ 01849000
*/* SEE IF AN SDWA IS SUPPLIED.                                      */ 01850000
*/*                                                                  */ 01851000
*/********************************************************************/ 01852000
*                                                                  0454 01853000
*   IF REG0^=12 THEN                                               0454 01854000
*                                                                  0454 01855000
         CH    REG0,@CH00854                                       0454 01856000
         BE    @RF00454                                            0454 01857000
*/********************************************************************/ 01858000
*/*                                                                  */ 01859000
*/* IF SO, PUT POINTER TO PARAMETER AREA INTO REG 2.  (NOTE: IF NOT, */ 01860000
*/* THE POINTER IS ALREADY IN REG 2.)  ISSUE SETRP TO ESTABLISH      */ 01861000
*/* INTERFACE BACK TO RTM.                                           */ 01862000
*/*                                                                  */ 01863000
*/********************************************************************/ 01864000
*                                                                  0455 01865000
*     DO;                                                          0455 01866000
*       REG2=SDWAPARM;                                             0456 01867000
         L     REG2,SDWAPARM(,SDWAPTR)                             0456 01868000
*       DO;                         /* SETRP RECPARM(SSCID)RC(4)FRESD   01869000
*                                      WA(YES)RETADDR(RETRYSSC)DUMP(N   01870000
*                                      O)                            */ 01871000
*         RESPECIFY                                                0458 01872000
*          (GPR00P,                                                0458 01873000
*           GPR01P,                                                0458 01874000
*           GPR14P,                                                0458 01875000
*           GPR15P) RSTD;                                          0458 01876000
*         GPR01P->SDWARCDE=4;       /* STORE RC INTO SDWA            */ 01877000
         MVI   SDWARCDE(GPR01P),X'04'                              0459 01878000
*         GPR01P->SDWARTYA=ADDR(RETRYSSC);/* SAVE RETRY ADDRESS      */ 01879000
         MVC   SDWARTYA(4,GPR01P),SSCRTYA                          0460 01880000
*         GPR01P->SDWAFREE='1'B;    /* SET FLAG TO FREE SDWA         */ 01881000
         OI    SDWAFREE(GPR01P),B'00000100'                        0461 01882000
*         GPR15P=ADDR(SSCID);       /* ACCESS RECORD PARAMETER LIST  */ 01883000
         LA    GPR15P,SSCID                                        0462 01884000
*         GPR01P->SDWARECP=GPR15P->I256C(1:24);/* COPY RECORD      0463 01885000
*                                      PARAMETERS                    */ 01886000
         MVC   SDWARECP(24,GPR01P),I256C(GPR15P)                   0463 01887000
*         GPR01P->SDWAREQ='0'B;     /* TURN OFF DUMP INDICATOR       */ 01888000
         NI    SDWAREQ(GPR01P),B'01111111'                         0464 01889000
*         RESPECIFY                                                0465 01890000
*          (GPR00P,                                                0465 01891000
*           GPR01P,                                                0465 01892000
*           GPR14P,                                                0465 01893000
*           GPR15P) UNRSTD;                                        0465 01894000
*       END;                                                       0466 01895000
*     END;                                                         0467 01896000
*                                                                  0467 01897000
*/********************************************************************/ 01898000
*/*                                                                  */ 01899000
*/* IF NOT, PUT RETRY ADDRESS IN REG 0, AND SET REG 15 TO 4 TO       */ 01900000
*/* INDICATE RETRY.                                                  */ 01901000
*/*                                                                  */ 01902000
*/********************************************************************/ 01903000
*                                                                  0468 01904000
*   ELSE                                                           0468 01905000
*     DO;                                                          0468 01906000
         B     @RC00454                                            0468 01907000
@RF00454 DS    0H                                                  0469 01908000
*       REG0=SSCRTYA;                                              0469 01909000
         L     REG0,SSCRTYA                                        0469 01910000
*       RETCODE=4;                                                 0470 01911000
         LA    RETCODE,4                                           0470 01912000
*     END;                                                         0471 01913000
*                                                                  0471 01914000
*/********************************************************************/ 01915000
*/*                                                                  */ 01916000
*/* ESTABLISH PERMANENT ADDRESSABILITY TO DATA BY RESTORING DATA     */ 01917000
*/* REGISTER SAVED BY IEAVRSSC.                                      */ 01918000
*/*                                                                  */ 01919000
*/********************************************************************/ 01920000
*                                                                  0472 01921000
*   GEN CODE REFS(@DATD) SETS(REG8);                               0472 01922000
*                                                                  0472 01923000
@RC00454 DS    0H                                                  0472 01924000
         L     REG8,0(REG2)                                             01925000
         USING @DATD,REG8                                               01926000
*/********************************************************************/ 01927000
*/*                                                                  */ 01928000
*/* SET RETURN CODE TO NON-ZERO TO INDICATE FAILURE.                 */ 01929000
*/*                                                                  */ 01930000
*/********************************************************************/ 01931000
*                                                                  0473 01932000
*   RCSAVE=16;                                                     0473 01933000
*                                                                  0473 01934000
         MVC   RCSAVE(4),@CF00045                                  0473 01935000
*/********************************************************************/ 01936000
*/*                                                                  */ 01937000
*/* RETURN TO RTM.                                                   */ 01938000
*/*                                                                  */ 01939000
*/********************************************************************/ 01940000
*                                                                  0474 01941000
*   END SSCESTAE;                                                  0474 01942000
@EL00003 DS    0H                                                  0474 01943000
@EF00003 DS    0H                                                  0474 01944000
@ER00003 BR    @14                                                 0474 01945000
*   RESPECIFY                                                      0475 01946000
*    (REG2) UNRESTRICTED;                                          0475 01947000
*   GENERATE CODE REFS(REG9,REG10,REG12,@PSTART);                  0476 01948000
         USING @PSTART,REG9                                             01949000
         USING @PSTART+4095,REG10                                       01950000
         DROP  REG12                                                    01951000
*                                                                  0477 01952000
*/********************************************************************/ 01953000
*/*                                                                  */ 01954000
*/*          IEAVRTST--TOD CLOCK STATUS TESTING ROUTINE              */ 01955000
*/*                                                                  */ 01956000
*/* THIS ROUTINE RECEIVES CONTROL FROM ROUTINES IEAVRINT AND         */ 01957000
*/* IEAVRSSC, AND IS PASSED A LIST OF CPU'S WHOSE CLOCK STATUS IS TO */ 01958000
*/* BE TESTED.  ALL OTHER CLOCKS ARE TESTED FOR SYNCHRONISM AGAINST  */ 01959000
*/* THE FIRST CLOCK IN THE LIST.  (NOTE: IF THERE IS ONLY ONE CLOCK  */ 01960000
*/* IN THE LIST, IT IS ONLY TESTED TO SEE IF IT IS SET.)  BEFORE RE- */ 01961000
*/* TURNING, A CODE IS SET IN REGISTER 15 TO INDICATE THE RESULTS OF */ 01962000
*/* THE TEST.  THE RETURN CODES ARE DEFINED AS FOLLOWS:              */ 01963000
*/*                                                                  */ 01964000
*/*          0   ALL CLOCKS SET AND IN SYNCHRONISM, OR THE ONLY      */ 01965000
*/*              CLOCK IS SET                                        */ 01966000
*/*          4   FIRST OR ONLY CLOCK IN LIST NOT SET, OR SIGP TO     */ 01967000
*/*              IT'S CPU FAILED                                     */ 01968000
*/*          8   CLOCKS OUT OF LOW ORDER SYNCHHRONISM                */ 01969000
*/*         12   CLOCKS OUT OF HIGH ORDER SYNCHRONISM                */ 01970000
*/*         16   TRIED 5 TIMES BUT COULD NOT COMPLETE TESTING        */ 01971000
*/*              PROCEDURE IN ALLOTTED TIME, STATUS UNKNOWN          */ 01972000
*/*         20   SIGP FAILED TRYING TO SIGNAL SOME CPU OTHER THAN    */ 01973000
*/*              THE FIRST IN THE LIST                               */ 01974000
*/*                                                                  */ 01975000
*/********************************************************************/ 01976000
*                                                                  0477 01977000
*IEAVRTST:                                                         0477 01978000
*   PROCEDURE OPTIONS(NOSAVEAREA,NOSAVE(15));                      0477 01979000
*                                                                  0477 01980000
         B     @PB00004                                            0477 01981000
IEAVRTST ST    @14,12(,@13)                                        0477 01982000
         STM   @00,@12,20(@13)                                     0477 01983000
*/********************************************************************/ 01984000
*/*                                                                  */ 01985000
*/* SET THE RETURN CODE TO 16 SO THAT THE LOOP WILL BE EXECUTED AT   */ 01986000
*/* LEAST ONCE, AND THEN PERFORM THE LOOP A MAXIMUM OF 5 TIMES.      */ 01987000
*/*                                                                  */ 01988000
*/********************************************************************/ 01989000
*                                                                  0478 01990000
*   RESPECIFY                                                      0478 01991000
*     CVTMAP BASED(CVTPTR);                                        0478 01992000
*   RETCODE=16;                                                    0479 01993000
         LA    RETCODE,16                                          0479 01994000
*   DO LOOPCTR=5 TO 1 BY-1 WHILE RETCODE=16;                       0480 01995000
         LA    @02,5                                               0480 01996000
         ST    @02,LOOPCTR                                         0480 01997000
@DL00480 CH    RETCODE,@CH00045                                    0480 01998000
         BNE   @DC00480                                            0480 01999000
*     REG11=PTREBEG;                /* GET PTR TO FIRST TCWA ENTRY 0481 02000000
*                                                            @YM01705*/ 02001000
         L     @02,PTREBEG                                         0481 02002000
         LR    REG11,@02                                           0481 02003000
*/********************************************************************/ 02004000
*/*                                                                  */ 02005000
*/* TEST FOR STRICTLY UNI-PROCESSOR CONFIGURATION.  IF SO, INVOKE    */ 02006000
*/* ROUTINE TO STORE TOD CLOCK VIA BRANCH AND LINK.  IF NOT, INVOKE  */ 02007000
*/* THE ROUITNE VIA THE IPC.                                         */ 02008000
*/*                                                                  */ 02009000
*/********************************************************************/ 02010000
*                                                                  0482 02011000
*     IF CSDMP='0'B THEN                                           0482 02012000
         L     @07,CVTPTR                                          0482 02013000
         L     @07,CVTCSD(,@07)                                    0482 02014000
         TM    CSDMP(@07),B'10000000'                              0482 02015000
         BNZ   @RF00482                                            0482 02016000
*       DO;                                                        0483 02017000
*         REG15=ADDR(DELTA1);       /* GET ENTRY POINT ADDRESS       */ 02018000
         LA    REG15,DELTA1                                        0484 02019000
*         REG1=PTREBEG;             /* GET ADDRESS OF FIRST TCWA   0485 02020000
*                                      ENTRY                         */ 02021000
         LR    REG1,@02                                            0485 02022000
*         GENERATE CODE REFS(REG15) SETS(REG14)(BALR  REG14,REG15);/*   02023000
*                                      BALR TO ROUTINE               */ 02024000
         BALR  REG14,REG15                                              02025000
*       END;                                                       0487 02026000
*     ELSE                                                         0488 02027000
*                                                                  0488 02028000
*/********************************************************************/ 02029000
*/*                                                                  */ 02030000
*/* SET UP THE IPC INTERFACE AND THEN SIGNAL THE CPU REPRESENTED BY  */ 02031000
*/* THE FIRST TOD CLOCK WORKAREA ENTRY TO STORE ITS CLOCK            */ 02032000
*/* AND THE CONDITION CODE RESULTING FROM THE STCK INSTRUCTION INTO  */ 02033000
*/* ITS WORKAREA ENTRY.  (NOTE: BEFORE ACTUALLY INVOKING IPC, THE    */ 02034000
*/* CONDITION CODE BITS IN THE ENTRY ARE SET TO NON-ZERO, SO THAT A  */ 02035000
*/* FAILURE TO GET THROUGH TO THE OTHER CPU WILL BE TREATED AS IF THE*/ 02036000
*/* CLOCK WAS NOT SET.)                                              */ 02037000
*/*                                                                  */ 02038000
*/********************************************************************/ 02039000
*                                                                  0488 02040000
*       DO;                                                        0488 02041000
         B     @RC00482                                            0488 02042000
@RF00482 DS    0H                                                  0489 02043000
*         TCLCC='11'B;              /* DEFAULT COND CODE TO NON-ZERO */ 02044000
         OI    TCLCC(REG11),B'00110000'                            0489 02045000
*         REG1=TCWAPCCA;            /* GET PCCA POINTER              */ 02046000
         L     REG1,TCWAPCCA(,REG11)                               0490 02047000
*         REG12=ADDR(DELTA1);       /* GET ROUTINE ADDRESS           */ 02048000
         LA    REG12,DELTA1                                        0491 02049000
*         GENERATE CODE REFS(CVTIPCRI,CVT)                         0492 02050000
*             (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));           0492 02051000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        02052000
*       END;                                                       0493 02053000
*                                                                  0493 02054000
*/********************************************************************/ 02055000
*/*                                                                  */ 02056000
*/* TEST THE RESULTING CONDITION CODE FOR 0, INDICATING THE CLOCK IS */ 02057000
*/* SET.                                                             */ 02058000
*/********************************************************************/ 02059000
*                                                                  0494 02060000
*     IF TCLCC='00'B THEN                                          0494 02061000
*                                                                  0494 02062000
@RC00482 TM    TCLCC(REG11),B'00110000'                            0494 02063000
         BNZ   @RF00494                                            0494 02064000
*/********************************************************************/ 02065000
*/*                                                                  */ 02066000
*/* IF THE CLOCK IS SET, THEN SEE IF THERE IS ONLY ONE CPU IN THE    */ 02067000
*/* CONFIGURATION.                                                   */ 02068000
*/*                                                                  */ 02069000
*/********************************************************************/ 02070000
*                                                                  0495 02071000
*       DO;                                                        0495 02072000
*         IF TCCPUCNT=1 THEN                                       0496 02073000
*                                                                  0496 02074000
         CLC   TCCPUCNT(2,TCWAPTR),@CH00096                        0496 02075000
         BNE   @RF00496                                            0496 02076000
*/********************************************************************/ 02077000
*/*                                                                  */ 02078000
*/* IF THERE IS ONLY ONE CPU, SET THE RETURN CODE TO 0 TO INDICATE   */ 02079000
*/* THAT ITS CLOCK IS SET.                                           */ 02080000
*/*                                                                  */ 02081000
*/********************************************************************/ 02082000
*                                                                  0497 02083000
*           RETCODE=0;                                             0497 02084000
         SLR   RETCODE,RETCODE                                     0497 02085000
*         ELSE                                                     0498 02086000
*                                                                  0498 02087000
*/********************************************************************/ 02088000
*/*                                                                  */ 02089000
*/* IF THERE IS MORE THAN ONE CPU, SET UP THE IPC INTERFACE, AND     */ 02090000
*/* SIGNAL THE CPU WITH THE SET CLOCK TO PERFORM A ROUTINE WHICH WHEN*/ 02091000
*/* DONE TWICE WILL CAUSE A DELAY OF A MINIMUM OF 1 SECOND.  THIS IS */ 02092000
*/* TO SATISFY THE REQUIREMENT OF THE SYNC CHECK DEFINITION.  (NOTE: */ 02093000
*/* THE LAST VALUE STORED BY THIS ROUTINE WILL BE USED TO BEGIN      */ 02094000
*/* TIMING A 2**20 MICROSECOND INTERVAL.)                            */ 02095000
*/*                                                                  */ 02096000
*/********************************************************************/ 02097000
*                                                                  0498 02098000
*           DO;                                                    0498 02099000
         B     @RC00496                                            0498 02100000
@RF00496 DS    0H                                                  0499 02101000
*             REG12=ADDR(DELTA4);   /* GET ROUTINE ADDRESS           */ 02102000
         LA    REG12,DELTA4                                        0499 02103000
*             REG11=ADDR(TCWA);     /* GET PARAMETER ADDRESS         */ 02104000
         LR    REG11,TCWAPTR                                       0500 02105000
*             RESPECIFY                                            0501 02106000
*              (REG2) RESTRICTED;                                  0501 02107000
*             DO REG2=2 TO 1 BY-1;                                 0502 02108000
         LA    REG2,2                                              0502 02109000
@DL00502 DS    0H                                                  0503 02110000
*               REG1=PTREBEG->TCWAPCCA;/* GET PCCA PTR FROM FIRST  0503 02111000
*                                      TCWA ENTRY                    */ 02112000
         L     @07,PTREBEG                                         0503 02113000
         L     REG1,TCWAPCCA(,@07)                                 0503 02114000
*               GENERATE CODE REFS(CVTIPCRI,CVT)                   0504 02115000
*                   (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));     0504 02116000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        02117000
*             END;                                                 0505 02118000
         BCTR  REG2,0                                              0505 02119000
         LTR   REG2,REG2                                           0505 02120000
         BP    @DL00502                                            0505 02121000
*             RESPECIFY                                            0506 02122000
*              (REG2) UNRESTRICTED;                                0506 02123000
*                                                                  0506 02124000
*/********************************************************************/ 02125000
*/*                                                                  */ 02126000
*/* BEGINNING WITH THE LAST TOD CLOCK WORKAREA ENTRY, USE THE IPC TO */ 02127000
*/* SIGNAL EACH CPU TO STORE ITS TOD CLOCK AND THE RESULTING CONDI-  */ 02128000
*/* TION CODE, AND TO TURN OFF THE SYNC CHECK INDICATOR IN ITS PCCA. */ 02129000
*/*                                                                  */ 02130000
*/* (NOTE1: BEFORE ACTUALLY INVOKING IPC, THE CONDITION CODE BITS IN */ 02131000
*/* THE ENTRY ARE SET TO NON-ZERO, SO THAT A FAILURE TO GET THROUGH  */ 02132000
*/* TO THE OTHER CPU WILL BE TREATED AS IF THE CLOCK WAS NOT SET.)   */ 02133000
*/*                                                                  */ 02134000
*/* (NOTE2: THE SUBJECT CPU STORES ITS CLOCK LAST.  THIS IS THE      */ 02135000
*/* ENDING VALUE FOR THE 2**20 MICROSECOND INTERVAL.)                */ 02136000
*/*                                                                  */ 02137000
*/********************************************************************/ 02138000
*                                                                  0507 02139000
*             REG11=PTREBEG+(TCCPUCNT-1)*LENGTH(TCENTRY);/* GET PTR     02140000
*                                      TO LAST TCWA ENTRY            */ 02141000
         LH    @02,TCCPUCNT(,TCWAPTR)                              0507 02142000
         LR    REG11,@02                                           0507 02143000
         BCTR  REG11,0                                             0507 02144000
         SLA   REG11,4                                             0507 02145000
         AL    REG11,PTREBEG                                       0507 02146000
*             REG12=ADDR(DELTA1);   /* GET ROUTINE ADDRESS           */ 02147000
         LA    REG12,DELTA1                                        0508 02148000
*             RESPECIFY                                            0509 02149000
*              (REG2) RESTRICTED;                                  0509 02150000
*             DO REG2=TCCPUCNT TO 1 BY-1;                          0510 02151000
         LH    @07,TCCPUCNT(,TCWAPTR)                              0510 02152000
         LR    REG2,@07                                            0510 02153000
         B     @DE00510                                            0510 02154000
@DL00510 DS    0H                                                  0511 02155000
*               TCLCC='11'B;        /* DEFAULT STCK CC TO NON-ZERO   */ 02156000
         OI    TCLCC(REG11),B'00110000'                            0511 02157000
*               REG1=TCWAPCCA;      /* GET ADDRESS PCCA              */ 02158000
         L     REG1,TCWAPCCA(,REG11)                               0512 02159000
*               GENERATE CODE REFS(CVTIPCRI,CVT)                   0513 02160000
*                   (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));     0513 02161000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        02162000
*               REG11=REG11-LENGTH(TCENTRY);/* POINT TO NEXT TCWA  0514 02163000
*                                      ENTRY                         */ 02164000
         SL    REG11,@CF00045                                      0514 02165000
*               IF RETCODE^=0 THEN  /* DID SIGP FAIL?        @ZA15781*/ 02166000
         LTR   RETCODE,RETCODE                                     0515 02167000
         BZ    @RF00515                                            0515 02168000
*                 DO;               /* YES                   @ZA15781*/ 02169000
*                   IF REG2=1 THEN  /* SUBJECT CPU?          @ZA15781*/ 02170000
         CH    REG2,@CH00096                                       0517 02171000
         BNE   @RF00517                                            0517 02172000
*                     RETCODE=4;    /* IEAVRTST R. C. = 4    @ZA15781*/ 02173000
         LA    RETCODE,4                                           0518 02174000
*                   ELSE            /* SOME OTHER CPU        @ZA15781*/ 02175000
*                     DO;           /*                       @ZA15781*/ 02176000
         B     @RC00517                                            0519 02177000
@RF00517 DS    0H                                                  0520 02178000
*                       RETCODE=20; /* IEAVRTST R. C. = 20   @ZA15781*/ 02179000
         LA    RETCODE,20                                          0520 02180000
*                       REG2=1;     /* FORCE LOOP EXIT       @ZA15781*/ 02181000
         LA    REG2,1                                              0521 02182000
*                     END;          /*                       @ZA15781*/ 02183000
*                 END;              /* END SIGP FAILED       @ZA15781*/ 02184000
*               ELSE                /* SIGP DID NOT FAIL     @ZA15781*/ 02185000
*                 RETCODE=16;       /* IEAVRTST R. C. = 16   @ZA15781*/ 02186000
         B     @RC00515                                            0524 02187000
@RF00515 LA    RETCODE,16                                          0524 02188000
*             END;                                                 0525 02189000
@RC00515 BCTR  REG2,0                                              0525 02190000
@DE00510 LTR   REG2,REG2                                           0525 02191000
         BP    @DL00510                                            0525 02192000
*             RESPECIFY                                            0526 02193000
*              (REG2) UNRESTRICTED;                                0526 02194000
*           END;                                                   0527 02195000
*       END;                                                       0528 02196000
*     ELSE                                                         0529 02197000
*                                                                  0529 02198000
*/********************************************************************/ 02199000
*/*                                                                  */ 02200000
*/* IF THE SUBJECT CLOCK WAS NOT SET, SET THE RETURN CODE TO 4.      */ 02201000
*/*                                                                  */ 02202000
*/********************************************************************/ 02203000
*                                                                  0529 02204000
*       RETCODE=4;                                                 0529 02205000
*                                                                  0529 02206000
         B     @RC00494                                            0529 02207000
@RF00494 LA    RETCODE,4                                           0529 02208000
*/********************************************************************/ 02209000
*/*                                                                  */ 02210000
*/* SEE IF THE RETURN CODE IS 16, ITS INITIAL VALUE.                 */ 02211000
*/*                                                                  */ 02212000
*/********************************************************************/ 02213000
*                                                                  0530 02214000
*     IF RETCODE=16 THEN                                           0530 02215000
*                                                                  0530 02216000
@RC00494 CH    RETCODE,@CH00045                                    0530 02217000
         BNE   @RF00530                                            0530 02218000
*/********************************************************************/ 02219000
*/*                                                                  */ 02220000
*/* IF IT IS 16, SEE IF ALL CLOCKS WERE STORED WITHIN 2**20          */ 02221000
*/* MICROSECONDS.  (NOTE: IF THEY WERE, THE HIGH ORDER 32 BITS OF THE*/ 02222000
*/* BEGINNING AND ENDING VALUES WILL BE EQUAL.)                      */ 02223000
*/*                                                                  */ 02224000
*/********************************************************************/ 02225000
*                                                                  0531 02226000
*       DO;                                                        0531 02227000
*         IF PTREBEG->TCWACLKL=TCWAA2L THEN                        0532 02228000
*                                                                  0532 02229000
         L     @02,PTREBEG                                         0532 02230000
         CLC   TCWACLKL(4,@02),TCWAA2L(TCWAPTR)                    0532 02231000
         BNE   @RF00532                                            0532 02232000
*/********************************************************************/ 02233000
*/*                                                                  */ 02234000
*/* IF SO, ENABLE FOR SYNC CHECKS.  (NOTE: IF A SYNC CHECK OCCURS,   */ 02235000
*/* THE TIMER SLIH WILL FIELD THE INTERRUPT, AND THEN DISABLE        */ 02236000
*/* FURTHER SYNC CHECKS.)                                            */ 02237000
*/*                                                                  */ 02238000
*/********************************************************************/ 02239000
*                                                                  0533 02240000
*           DO;                                                    0533 02241000
*             GENERATE CODE REFS(TCWAPTR) SETS(TCWACTL)            0534 02242000
*                 (STCTL 0,0,TCWACTL(TCWAPTR)  SAVE CONTROL REG 0);     02243000
         STCTL 0,0,TCWACTL(TCWAPTR)  SAVE CONTROL REG 0                 02244000
*             TCSYNCK='1'B;         /* TURN ON SYNC CK BIT IN CTL REG   02245000
*                                      0                             */ 02246000
         OI    TCSYNCK(TCWAPTR),B'00010000'                        0535 02247000
*             GENERATE CODE REFS(TCWAPTR,TCWACTL)                  0536 02248000
*                 (LCTL  0,0,TCWACTL(TCWAPTR)  ENABLE SYNC CHECKS);     02249000
         LCTL  0,0,TCWACTL(TCWAPTR)  ENABLE SYNC CHECKS                 02250000
*                                                                  0537 02251000
*/********************************************************************/ 02252000
*/*                                                                  */ 02253000
*/* IF THE TPCSYNC FLAG IS NOT ON, NO SYNC CHECK OCCURRED, AND       */ 02254000
*/* SYNC CHECK INTERRUPTS ARE STILL ENABLED, SO DISABLE THEM.        */ 02255000
*/*                                                                  */ 02256000
*/********************************************************************/ 02257000
*                                                                  0537 02258000
*             IF TPCSYNC='0'B THEN                                 0537 02259000
         L     @02,CVTPTR                                          0537 02260000
         L     @02,TPCPTR(,@02)                                    0537 02261000
         TM    TPCSYNC(@02),B'01000000'                            0537 02262000
         BNZ   @RF00537                                            0537 02263000
*               DO;                                                0538 02264000
*                 GENERATE CODE REFS(TCWAPTR) SETS(TCWACTL)        0539 02265000
*                     (STCTL 0,0,TCWACTL(TCWAPTR)  SAVE CONTROL REG 0); 02266000
         STCTL 0,0,TCWACTL(TCWAPTR)  SAVE CONTROL REG 0                 02267000
*                 TCSYNCK='0'B;     /* TURN OFF SYNC CK BIT IN CTL 0540 02268000
*                                      REG 0                         */ 02269000
         NI    TCSYNCK(TCWAPTR),B'11101111'                        0540 02270000
*                 GENERATE CODE REFS(TCWAPTR,TCWACTL)              0541 02271000
*                     (LCTL  0,0,TCWACTL(TCWAPTR)  DISABLE SYNC CHECKS) 02272000
*                     ;                                            0541 02273000
         LCTL  0,0,TCWACTL(TCWAPTR)  DISABLE SYNC CHECKS                02274000
*               END;                                               0542 02275000
*             ELSE                                                 0543 02276000
*                                                                  0543 02277000
*/********************************************************************/ 02278000
*/*                                                                  */ 02279000
*/* IF THE TPCSYNC FLAG IS ON INDICATING THAT A SYNC CHECK OCCURRED. */ 02280000
*/* SET THE RETURN CODE TO 8 TO INDICATE AN OUT OF LOW ORDER SYNC    */ 02281000
*/* CONDITION, AND THEN SET THE PCCASYNC FLAG FOR                    */ 02282000
*/* ALL OTHER CPU'S (EXCEPTING THE SUBJECT) TO INDICATE              */ 02283000
*/* THAT THEIR CLOCKS ARE OUT OF SYNC. (NOTE: THE PCCASYNC FLAG NOW  */ 02284000
*/* SERVES TO INDICATE WHICH CLOCKS REQUIRE SYNCHRONIZING.)          */ 02285000
*/*                                                                  */ 02286000
*/********************************************************************/ 02287000
*                                                                  0543 02288000
*               DO;                                                0543 02289000
         B     @RC00537                                            0543 02290000
@RF00537 DS    0H                                                  0544 02291000
*                 REG11=PTREBEG+LENGTH(TCENTRY);/* POINT TO 2ND TCWA    02292000
*                                      ENTRY                         */ 02293000
         LA    REG11,16                                            0544 02294000
         AL    REG11,PTREBEG                                       0544 02295000
*                 RETCODE=8;        /* SET RETURN CODE TO 8          */ 02296000
         LA    RETCODE,8                                           0545 02297000
*                 TPCSYNC='0'B;     /* TURN OFF SYNC CK INDICATOR IN    02298000
*                                      TPC                           */ 02299000
         L     @02,CVTPTR                                          0546 02300000
         L     @02,TPCPTR(,@02)                                    0546 02301000
         NI    TPCSYNC(@02),B'10111111'                            0546 02302000
*                 RESPECIFY                                        0547 02303000
*                  (REG2) RESTRICTED;                              0547 02304000
*                 DO REG2=TCCPUCNT-1 TO 1 BY-1;                    0548 02305000
         LH    REG2,TCCPUCNT(,TCWAPTR)                             0548 02306000
         BCTR  REG2,0                                              0548 02307000
         B     @DE00548                                            0548 02308000
@DL00548 DS    0H                                                  0549 02309000
*                   TCWAPCCA->PCCASYNC='1'B;                       0549 02310000
         L     @07,TCWAPCCA(,REG11)                                0549 02311000
         OI    PCCASYNC(@07),B'01000000'                           0549 02312000
*                   REG11=REG11+LENGTH(TCENTRY);/* POINT TO NEXT TCWA   02313000
*                                      ENTRY                         */ 02314000
         AL    REG11,@CF00045                                      0550 02315000
*                 END;                                             0551 02316000
         BCTR  REG2,0                                              0551 02317000
@DE00548 LTR   REG2,REG2                                           0551 02318000
         BP    @DL00548                                            0551 02319000
*                 RESPECIFY                                        0552 02320000
*                  (REG2) UNRESTRICTED;                            0552 02321000
*               END;                                               0553 02322000
*             RESPECIFY                                            0554 02323000
*               TPC BASED(TPCPTR);                                 0554 02324000
*                                                                  0554 02325000
@RC00537 DS    0H                                                  0555 02326000
*/********************************************************************/ 02327000
*/*                                                                  */ 02328000
*/* SEE IF THE RETURN CODE IS STILL SET TO 16.                       */ 02329000
*/*                                                                  */ 02330000
*/********************************************************************/ 02331000
*                                                                  0555 02332000
*             IF RETCODE=16 THEN                                   0555 02333000
*                                                                  0555 02334000
         LA    @02,16                                              0555 02335000
         CR    RETCODE,@02                                         0555 02336000
         BNE   @RF00555                                            0555 02337000
*/********************************************************************/ 02338000
*/*                                                                  */ 02339000
*/* IF IT IS, THERE IS MORE THAN ONE CPU IN THE CONFIGURATION, THE   */ 02340000
*/* CLOCKS WERE STORED WITHIN 2**20 MICROSECONDS, AND THEY ARE IN LOW*/ 02341000
*/* ORDER SYNCHRONISM.  NOW, CHECK TO SEE IF ALL CLOCKS ARE SET      */ 02342000
*/* AND IN HIGH ORDER SYNCHRONISM.                                   */ 02343000
*/*                                                                  */ 02344000
*/********************************************************************/ 02345000
*                                                                  0556 02346000
*               DO;                                                0556 02347000
*                 REG11=PTREBEG+LENGTH(TCENTRY);                   0557 02348000
         AL    @02,PTREBEG                                         0557 02349000
         LR    REG11,@02                                           0557 02350000
*                 RESPECIFY                                        0558 02351000
*                  (REG2) RESTRICTED;                              0558 02352000
*                 DO REG2=TCCPUCNT-1 TO 1 BY-1;                    0559 02353000
*                                                                  0559 02354000
         LH    REG2,TCCPUCNT(,TCWAPTR)                             0559 02355000
         BCTR  REG2,0                                              0559 02356000
         B     @DE00559                                            0559 02357000
@DL00559 DS    0H                                                  0560 02358000
*/********************************************************************/ 02359000
*/*                                                                  */ 02360000
*/* SEE IF THE CLOCK FOR THE ENTRY IS SET, AND IF ITS VALUE FALLS    */ 02361000
*/* BETWEEN THE SUBJECT'S BEGINNING AND ENDING VALUES.               */ 02362000
*/*                                                                  */ 02363000
*/********************************************************************/ 02364000
*                                                                  0560 02365000
*                   IF TCLCC^='00'B TCWACLKL^=TCWAA2L TCWACLKR<=TCWAA2R 02366000
*                        TCWACLKR>=PTREBEG->TCWACLKR THEN          0560 02367000
*                                                                  0560 02368000
         TM    TCLCC(REG11),B'00110000'                            0560 02369000
         BNZ   @RT00560                                            0560 02370000
         CLC   TCWACLKL(4,REG11),TCWAA2L(TCWAPTR)                  0560 02371000
         BNE   @RT00560                                            0560 02372000
         L     @07,TCWACLKR(,REG11)                                0560 02373000
         CL    @07,TCWAA2R(,TCWAPTR)                               0560 02374000
         BNH   @RT00560                                            0560 02375000
         L     @06,PTREBEG                                         0560 02376000
         CL    @07,TCWACLKR(,@06)                                  0560 02377000
         BL    @RF00560                                            0560 02378000
@RT00560 DS    0H                                                  0561 02379000
*/********************************************************************/ 02380000
*/*                                                                  */ 02381000
*/* FOR EVERY ONE THAT DOES NOT MEET THESE CRITERIA, SET THE OUT OF  */ 02382000
*/* SYNC INDICATOR IN ITS PCCA, AND SET THE RETURN CODE TO 12        */ 02383000
*/* INDICATING AN OUT OF HIGH ORDER SYNC CONDITION.                  */ 02384000
*/*                                                                  */ 02385000
*/********************************************************************/ 02386000
*                                                                  0561 02387000
*                     DO;                                          0561 02388000
*                       TCWAPCCA->PCCASYNC='1'B;                   0562 02389000
         L     @07,TCWAPCCA(,REG11)                                0562 02390000
         OI    PCCASYNC(@07),B'01000000'                           0562 02391000
*                       RETCODE=12;                                0563 02392000
         LA    RETCODE,12                                          0563 02393000
*                     END;                                         0564 02394000
*                   REG11=REG11+LENGTH(TCENTRY);/* POINT TO NEXT TCWA   02395000
*                                      ENTRY                         */ 02396000
@RF00560 AL    REG11,@CF00045                                      0565 02397000
*                 END;                                             0566 02398000
         BCTR  REG2,0                                              0566 02399000
@DE00559 LTR   REG2,REG2                                           0566 02400000
         BP    @DL00559                                            0566 02401000
*                 RESPECIFY                                        0567 02402000
*                  (REG2) UNRESTRICTED;                            0567 02403000
*                                                                  0567 02404000
*/********************************************************************/ 02405000
*/*                                                                  */ 02406000
*/* AGAIN, SEE IF THE RETURN CODE IS STILL 16.                       */ 02407000
*/*                                                                  */ 02408000
*/********************************************************************/ 02409000
*                                                                  0568 02410000
*                 IF RETCODE=16 THEN                               0568 02411000
*                                                                  0568 02412000
         CH    RETCODE,@CH00045                                    0568 02413000
         BNE   @RF00568                                            0568 02414000
*/********************************************************************/ 02415000
*/*                                                                  */ 02416000
*/* IF IT STILL IS, ALL CLOCKS ARE SET AND IN SYNCHRONISM, SO SET    */ 02417000
*/* THE RETURN CODE TO 0 TO INDICATE THIS FACT TO THE CALLER.        */ 02418000
*/*                                                                  */ 02419000
*/********************************************************************/ 02420000
*                                                                  0569 02421000
*                   RETCODE=0;                                     0569 02422000
         SLR   RETCODE,RETCODE                                     0569 02423000
*               END;                                               0570 02424000
@RF00568 DS    0H                                                  0571 02425000
*           END;                                                   0571 02426000
@RF00555 DS    0H                                                  0572 02427000
*       END;                                                       0572 02428000
@RF00532 DS    0H                                                  0573 02429000
*   END;                                                           0573 02430000
*                                                                  0573 02431000
@RF00530 SLR   @02,@02                                             0573 02432000
         BCTR  @02,0                                               0573 02433000
         AL    @02,LOOPCTR                                         0573 02434000
         ST    @02,LOOPCTR                                         0573 02435000
         LTR   @02,@02                                             0573 02436000
         BP    @DL00480                                            0573 02437000
@DC00480 DS    0H                                                  0574 02438000
*/********************************************************************/ 02439000
*/*                                                                  */ 02440000
*/* RETURN TO THE CALLER.                                            */ 02441000
*/*                                                                  */ 02442000
*/********************************************************************/ 02443000
*                                                                  0574 02444000
*   END IEAVRTST;                                                  0574 02445000
@EL00004 DS    0H                                                  0574 02446000
@EF00004 DS    0H                                                  0574 02447000
@ER00004 L     @14,12(,@13)                                        0574 02448000
         LM    @00,@12,20(@13)                                     0574 02449000
         BR    @14                                                 0574 02450000
*                                                                  0575 02451000
*/********************************************************************/ 02452000
*/*                                                                  */ 02453000
*/*            IEAVRCOM--OPERATOR COMMUNICATIONS ROUTINE             */ 02454000
*/*                                                                  */ 02455000
*/* THIS ROUTINE RECEIVES CONTROL FROM ROUTINES IEAVRINT AND         */ 02456000
*/* IEAVRSSC.  IT TESTS FLAGS IN THE TOD CLOCK WORKAREA TO DETERMINE */ 02457000
*/* WHICH MESSAGE TO ISSUE, AND THEN ISSUES THE MESSAGE.  (NOTE: IF  */ 02458000
*/* THE INSTALLATION HAS CHOSEN THE IMPROVED IPL OPTION, AND ALL     */ 02459000
*/* CLOCKS ARE SET AND IN SYNCHRONISM, NO MESSAGES ARE ISSUED.)  IT  */ 02460000
*/* THEN ACCEPTS THE OPERATOR'S REPLY AND ISSUES AN INTERNAL SET     */ 02461000
*/* COMMAND TO PROCESS THE REPLY.  IF THE REPLY IS NOT SUCCESSFULLY  */ 02462000
*/* PROCESSED, THE MESSAGE IS REPEATED.  OTHERWISE, THE NEXT MESSAGE */ 02463000
*/* IS ISSUED, IF REQUIRED, AND ULTIMATELY, RETURN IS MADE TO THE    */ 02464000
*/* CALLER.                                                          */ 02465000
*/*                                                                  */ 02466000
*/********************************************************************/ 02467000
*                                                                  0575 02468000
*IEAVRCOM:                                                         0575 02469000
*   PROCEDURE OPTIONS(NOSAVEAREA);                                 0575 02470000
*                                                                  0575 02471000
IEAVRCOM STM   @14,@12,12(@13)                                     0575 02472000
*/********************************************************************/ 02473000
*/*                                                                  */ 02474000
*/* SET UP THE COMMAND LENGTH AND NAME FOR THE INTERNAL SET COMMAND. */ 02475000
*/*                                                                  */ 02476000
*/********************************************************************/ 02477000
*                                                                  0576 02478000
*   RESPECIFY                                                      0576 02479000
*    (LCVTPTR) RESTRICTED;                                         0576 02480000
*   RESPECIFY                                                      0577 02481000
*     CVTMAP BASED(LCVTPTR);                                       0577 02482000
*   TCWARLEN=LENGTH(TCWARPLY);      /* LENGTH TO TCWA                */ 02483000
         MVC   TCWARLEN(2,TCWAPTR),@CH02651                        0578 02484000
*   TCWASET=CHARSET;                /* NAME TO TCWA                  */ 02485000
         MVC   TCWASET(4,TCWAPTR),CHARSET                          0579 02486000
*/********************************************************************/ 02487000
*/*                                                                  */ 02488000
*/* TURN ON THE TOD CLOCK NOT SET INDICATOR SO THAT THE LOOP WILL BE */ 02489000
*/* EXECUTED AT LEAST ONCE.                                          */ 02490000
*/*                                                                  */ 02491000
*/********************************************************************/ 02492000
*                                                                  0580 02493000
*   TCGNSET='1'B;                                                  0580 02494000
*                                                                  0580 02495000
         OI    TCGNSET(TCWAPTR),B'00001000'                        0580 02496000
*/********************************************************************/ 02497000
*/*                                                                  */ 02498000
*/* SEE IF MESSAGE IEA886A SHOULD BE ISSUED.                         */ 02499000
*/*                                                                  */ 02500000
*/********************************************************************/ 02501000
*                                                                  0581 02502000
*   IF TCGYYY='1'B THEN                                            0581 02503000
*                                                                  0581 02504000
         TM    TCGYYY(TCWAPTR),B'01000000'                         0581 02505000
         BNO   @RF00581                                            0581 02506000
*/********************************************************************/ 02507000
*/*                                                                  */ 02508000
*/* IF SO, SET UP THE PARAMETER LIST FOR THE MESSAGE.                */ 02509000
*/*                                                                  */ 02510000
*/********************************************************************/ 02511000
*                                                                  0582 02512000
*     DO;                                                          0582 02513000
*       WPLPTR=ADDR(MSGYYY);        /* ADDRESSABILITY FOR PARMLIST   */ 02514000
         LA    WPLPTR,MSGYYY                                       0583 02515000
*       WPLRPTRA=ADDR(TCWATXT);     /* REPLY ADDRESS TO PARMLIST     */ 02516000
         LA    @07,TCWATXT(,TCWAPTR)                               0584 02517000
         STCM  @07,7,WPLRPTRA(WPLPTR)                              0584 02518000
*       WPLRECB=ADDR(TODECB);       /* ECB ADDRESS TO PARMLIST       */ 02519000
         LA    @07,TODECB                                          0585 02520000
         ST    @07,WPLRECB(,WPLPTR)                                0585 02521000
*/********************************************************************/ 02522000
*/*                                                                  */ 02523000
*/* KEEP ISSUING THE MESSAGE UNTIL THE OPERATOR'S REPLY IS           */ 02524000
*/* SYNTACTICALLY CORRECT (AS DETERMINED BY THE SET COMMAND PROCES-  */ 02525000
*/* SOR--MODULE IEE0603D) AND A TOD CLOCK HAS BEEN SET.              */ 02526000
*/*                                                                  */ 02527000
*/********************************************************************/ 02528000
*                                                                  0586 02529000
*       DO WHILE BAIPLCC^='00'X TCGNSET='1'B;                      0586 02530000
         B     @DE00586                                            0586 02531000
@DL00586 DS    0H                                                  0587 02532000
*         TODECB=0;                 /* ZERO THE REPLY ECB            */ 02533000
         SLR   @07,@07                                             0587 02534000
         ST    @07,TODECB                                          0587 02535000
*         TCWATXT='';               /* CLEAR THE REPLY AREA          */ 02536000
         MVI   TCWATXT(TCWAPTR),C' '                               0588 02537000
         MVC   TCWATXT+1(39,TCWAPTR),TCWATXT(TCWAPTR)              0588 02538000
*         WPLPTR=ADDR(MSGYYY);                                     0589 02539000
         LA    WPLPTR,MSGYYY                                       0589 02540000
*         GENERATE CODE(WTOR  MF=(E,(1)));                         0590 02541000
*                                                                  0590 02542000
         WTOR  MF=(E,(1))                                               02543000
*/********************************************************************/ 02544000
*/*                                                                  */ 02545000
*/* WAIT FOR REPLY.                                                  */ 02546000
*/*                                                                  */ 02547000
*/********************************************************************/ 02548000
*                                                                  0591 02549000
*         REG1=ADDR(TODECB);                                       0591 02550000
         LA    REG1,TODECB                                         0591 02551000
*         GENERATE CODE(WAIT  1,ECB=(1));                          0592 02552000
*                                                                  0592 02553000
         WAIT  1,ECB=(1)                                                02554000
*/********************************************************************/ 02555000
*/*                                                                  */ 02556000
*/* AFTER RECEIVING THE OPERATOR'S REPLY, INDICATE THAT THIS IS AN   */ 02557000
*/* INTERNAL SET COMMAND AND ISSUE SVC 34 TO EXECUTE IT.             */ 02558000
*/*                                                                  */ 02559000
*/********************************************************************/ 02560000
*                                                                  0593 02561000
*         BAIPLCC='00'X;            /* CLEAR ERROR BYTE IN BASEA     */ 02562000
         L     @07,CVTMSER(,LCVTPTR)                               0593 02563000
         MVI   BAIPLCC(@07),X'00'                                  0593 02564000
*         REG0=0;                                                  0594 02565000
         SLR   REG0,REG0                                           0594 02566000
*         REG1=ADDR(TCWARPLY);                                     0595 02567000
         LA    REG1,TCWARPLY(,TCWAPTR)                             0595 02568000
*         BAINTSET='1'B;            /* INDICATE INTERNAL SET COMMAND */ 02569000
         OI    BAINTSET(@07),B'00100000'                           0596 02570000
*         GENERATE CODE(MGCR  (1));                                0597 02571000
         MGCR  (1)                                                      02572000
*         BAINTSET='0'B;                                           0598 02573000
         L     @07,CVTMSER(,LCVTPTR)                               0598 02574000
         NI    BAINTSET(@07),B'11011111'                           0598 02575000
*       END;                                                       0599 02576000
@DE00586 L     @07,CVTMSER(,LCVTPTR)                               0599 02577000
         CLI   BAIPLCC(@07),X'00'                                  0599 02578000
         BNE   @DL00586                                            0599 02579000
         TM    TCGNSET(TCWAPTR),B'00001000'                        0599 02580000
         BO    @DL00586                                            0599 02581000
*     END;                                                         0600 02582000
*   ELSE                                                           0601 02583000
*                                                                  0601 02584000
*/********************************************************************/ 02585000
*/*                                                                  */ 02586000
*/* IF MESSAGE IEA886A SHOULD NOT BE ISSUED, SEE IF MESSAGE IEA887A  */ 02587000
*/* SHOULD BE.                                                       */ 02588000
*/*                                                                  */ 02589000
*/********************************************************************/ 02590000
*                                                                  0601 02591000
*     IF TCGZZZ='1'B THEN                                          0601 02592000
*                                                                  0601 02593000
         B     @RC00581                                            0601 02594000
@RF00581 TM    TCGZZZ(TCWAPTR),B'00100000'                         0601 02595000
         BNO   @RF00601                                            0601 02596000
*/********************************************************************/ 02597000
*/*                                                                  */ 02598000
*/* IF SO, SET UP THE PARAMETER LIST FOR THE MESSAGE.                */ 02599000
*/*                                                                  */ 02600000
*/********************************************************************/ 02601000
*                                                                  0602 02602000
*       DO;                                                        0602 02603000
*         WPLPTR=ADDR(MSGZZZ2);     /* ADDRESSABILITY FOR PARMLIST   */ 02604000
         LA    WPLPTR,MSGZZZ2                                      0603 02605000
*         WPLRPTRA=ADDR(TCWATXT);   /* REPLY ADDRESS TO PARMLIST     */ 02606000
         LA    @07,TCWATXT(,TCWAPTR)                               0604 02607000
         STCM  @07,7,WPLRPTRA(WPLPTR)                              0604 02608000
*         WPLRECB=ADDR(TODECB);     /* ECB ADDRESS TO PARMLIST       */ 02609000
         LA    @07,TODECB                                          0605 02610000
         ST    @07,WPLRECB(,WPLPTR)                                0605 02611000
*/********************************************************************/ 02612000
*/*                                                                  */ 02613000
*/* KEEP ISSUING THIS MESSAGE USING THE SAME CRITERIA AS ABOVE.      */ 02614000
*/*                                                                  */ 02615000
*/********************************************************************/ 02616000
*                                                                  0606 02617000
*         DO WHILE BAIPLCC^='00'X TCGNSET='1'B;                    0606 02618000
*                                                                  0606 02619000
         B     @DE00606                                            0606 02620000
@DL00606 DS    0H                                                  0607 02621000
*/********************************************************************/ 02622000
*/*                                                                  */ 02623000
*/* SEARCH THE TOD CLOCK WORKAREA ENTRIES FOR CLOCKS THAT ARE SET.   */ 02624000
*/*                                                                  */ 02625000
*/********************************************************************/ 02626000
*                                                                  0607 02627000
*           DO REG11=PTREBEG TO PTREEND BY LENGTH(TCENTRY);        0607 02628000
         L     REG11,PTREBEG                                       0607 02629000
         B     @DE00607                                            0607 02630000
@DL00607 DS    0H                                                  0608 02631000
*             IF TCLCC='00'B THEN                                  0608 02632000
*                                                                  0608 02633000
         TM    TCLCC(REG11),B'00110000'                            0608 02634000
         BNZ   @RF00608                                            0608 02635000
*/********************************************************************/ 02636000
*/*                                                                  */ 02637000
*/* IF THE CLOCK IS SET (AS DETERMINED BY THE CONDITION CODE FIELD   */ 02638000
*/* OF THE ENTRY), STORE THE CURRENT CLOCK VALUE.  THEN,             */ 02639000
*/* CALCULATE THE LOCAL TIME AND DATE ACCORDING TO THE CLOCK VALUE   */ 02640000
*/* JUST STORED, AND WRITE THE FIRST LINE OF MESSAGE IEA887A FOR     */ 02641000
*/* THAT CPU.                                                        */ 02642000
*/*                                                                  */ 02643000
*/********************************************************************/ 02644000
*                                                                  0609 02645000
*               DO;                                                0609 02646000
*                 REG12=ADDR(DELTA3);                              0610 02647000
         LA    REG12,DELTA3                                        0610 02648000
*                 REG1=TCWAPCCA;                                   0611 02649000
         L     REG1,TCWAPCCA(,REG11)                               0611 02650000
*                 GENERATE CODE REFS(CVTIPCRI,CVT)                 0612 02651000
*                     (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));   0612 02652000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        02653000
*                 TCWAA2L=TCWACLKL+CVTTZ;/* CALCULATE LOCAL CLOCK  0613 02654000
*                                      VALUE                         */ 02655000
         L     @07,TCWACLKL(,REG11)                                0613 02656000
         AL    @07,CVTTZ(,LCVTPTR)                                 0613 02657000
         ST    @07,TCWAA2L(,TCWAPTR)                               0613 02658000
*                 TCWAA2R=TCWACLKR; /* USE L. O. BITS        @ZA10129*/ 02659000
         MVC   TCWAA2R(4,TCWAPTR),TCWACLKR(REG11)                  0614 02660000
*                 TCWAPTP=ADDR(TCWAPLT);                           0615 02661000
         LA    @07,TCWAPLT(,TCWAPTR)                               0615 02662000
         ST    @07,TCWAPTP(,TCWAPTR)                               0615 02663000
*                 TCWAPDP=ADDR(TCWAPLD);                           0616 02664000
         LA    @07,TCWAPLD(,TCWAPTR)                               0616 02665000
         ST    @07,TCWAPDP(,TCWAPTR)                               0616 02666000
*                 TCWACVP=ADDR(TCWAA2);                            0617 02667000
         LA    @07,TCWAA2(,TCWAPTR)                                0617 02668000
         ST    @07,TCWACVP(,TCWAPTR)                               0617 02669000
*                 REG1=ADDR(TCWAPTP);                              0618 02670000
         LA    REG1,TCWAPTP(,TCWAPTR)                              0618 02671000
*                 CALL CUTODATE;    /* CALL CONVERSION ROUTINE       */ 02672000
         BAL   @14,CUTODATE                                        0619 02673000
*/********************************************************************/ 02674000
*/*                                                                  */ 02675000
*/* ASSEMBLE THE VARIABLE TEXT INTO THE MESSAGE AND DISPLAY THE LOCAL*/ 02676000
*/* TIME AND DATE FOR THIS CPU'S CLOCK.                              */ 02677000
*/*                                                                  */ 02678000
*/********************************************************************/ 02679000
*                                                                  0620 02680000
*                 WPLPTR=ADDR(MSGZZZ1);                            0620 02681000
         LA    WPLPTR,MSGZZZ1                                      0620 02682000
*                 DATEZ=DTEPATRN;   /* EDITING PATTERN TO MSG        */ 02683000
*                 ED(DATEZ,TCWAPLD(2:4));/* EDIT DATE INTO MSG       */ 02685000
*                 DATEZ(1)='=';     /* PUT = SIGN AFTER DATE         */ 02687000
         ZAP   TCWAWRK1(4,TCWAPTR),TCWAPLD(4,TCWAPTR)              @L01+02687050
                                   Copy packed date                @L01 02687100
         AP    TCWAWRK1(4,TCWAPTR),P1900                           @L01+02687150
                                   Adjust for 4-digit year         @L01 02687200
         OI    TCWAWRK1+3(TCWAPTR),X'0F'                           @L01+02687250
                                   Force printable sign            @L01 02687300
         MVC   DATEZ(9,WPLPTR),DTEPATRN                            @L01+02687350
                                   Move in edit pattern            @L01 02687400
         ED    DATEZ(9,WPLPTR),TCWAWRK1(TCWAPTR)                   @L01+02687450
                                   Edit date into message          @L01 02687500
         MVI   DATEZ(WPLPTR),C'='                                  0623 02688000
*                 CLOCKZ=CLKPATRN;  /* EDITING PATTERN TO MSG        */ 02689000
         MVC   CLOCKZ(9,WPLPTR),CLKPATRN                           0624 02690000
*                 ED(CLOCKZ,TCWAPLT);/* EDIT CLOCK INTO MSG          */ 02691000
         ED    CLOCKZ(9,WPLPTR),TCWAPLT(TCWAPTR)                   0625 02692000
*                 CLOCKZ(1)='=';    /* PUT = SIGN AFTER CLOCK        */ 02693000
         MVI   CLOCKZ(WPLPTR),C'='                                 0626 02694000
*                 CVD(TCWAIADD,TCWAWRK1);                          0627 02695000
         SLR   @07,@07                                             0627 02696000
         ICM   @07,3,TCWAIADD(REG11)                               0627 02697000
         CVD   @07,TCWAWRK1(,TCWAPTR)                              0627 02698000
*                 UNPK(CPUADDRZ,TCWAWRK1(7:8));                    0628 02699000
         UNPK  CPUADDRZ(2,WPLPTR),TCWAWRK1+6(2,TCWAPTR)            0628 02700000
*                 CPUADDRZ(2)=CPUADDRZ(2) 'F0'X;                   0629 02701000
         OI    CPUADDRZ+1(WPLPTR),X'F0'                            0629 02702000
*                 GENERATE CODE(WTO   MF=(E,(1)));                 0630 02703000
         WTO   MF=(E,(1))                                               02704000
*               END;                                               0631 02705000
*           END;                                                   0632 02706000
@RF00608 AL    REG11,@CF00045                                      0632 02707000
@DE00607 C     REG11,PTREEND                                       0632 02708000
         BNH   @DL00607                                            0632 02709000
*           TODECB=0;               /* ZERO REPLY ECB                */ 02710000
         SLR   @07,@07                                             0633 02711000
         ST    @07,TODECB                                          0633 02712000
*           TCWATXT='';             /* CLEAR REPLY AREA              */ 02713000
         MVI   TCWATXT(TCWAPTR),C' '                               0634 02714000
         MVC   TCWATXT+1(39,TCWAPTR),TCWATXT(TCWAPTR)              0634 02715000
*           WPLPTR=ADDR(MSGZZZ2);                                  0635 02716000
*                                                                  0635 02717000
         LA    WPLPTR,MSGZZZ2                                      0635 02718000
*/********************************************************************/ 02719000
*/*                                                                  */ 02720000
*/* THEN ISSUE THE SECOND LINE OF MESSAGE IEA887A TO PROMPT THE      */ 02721000
*/* OPERATOR FOR A REPLY.                                            */ 02722000
*/*                                                                  */ 02723000
*/********************************************************************/ 02724000
*                                                                  0636 02725000
*           GENERATE CODE;                                         0636 02726000
*                                                                  0636 02727000
*                            /*                                         02728000
         WTOR  MF=(E,(1))                                          */   02729000
*/********************************************************************/ 02730000
*/*                                                                  */ 02731000
*/* WAIT FOR REPLY.                                                  */ 02732000
*/*                                                                  */ 02733000
*/********************************************************************/ 02734000
*                                                                  0637 02735000
*           REG1=ADDR(TODECB);                                     0637 02736000
         LA    REG1,TODECB                                         0637 02737000
*           GENERATE CODE(WAIT  1,ECB=(1));                        0638 02738000
*                                                                  0638 02739000
         WAIT  1,ECB=(1)                                                02740000
*/********************************************************************/ 02741000
*/*                                                                  */ 02742000
*/* INDICATE AN INTERNAL SET COMMAND AND ISSUE SVC 34 USING THE OPER-*/ 02743000
*/* ATOR'S REPLY AS THE COMMAND OPERAND.                             */ 02744000
*/*                                                                  */ 02745000
*/********************************************************************/ 02746000
*                                                                  0639 02747000
*           BAIPLCC='00'X;          /* CLEAR ERROR BYTE IN BASEA     */ 02748000
         L     @07,CVTMSER(,LCVTPTR)                               0639 02749000
         MVI   BAIPLCC(@07),X'00'                                  0639 02750000
*           REG0=0;                                                0640 02751000
         SLR   REG0,REG0                                           0640 02752000
*           REG1=ADDR(TCWARPLY);                                   0641 02753000
         LA    REG1,TCWARPLY(,TCWAPTR)                             0641 02754000
*           BAINTSET='1'B;          /* INDICATE AN INTERNAL SET    0642 02755000
*                                      COMMAND                       */ 02756000
         OI    BAINTSET(@07),B'00100000'                           0642 02757000
*           GENERATE CODE(MGCR  (1));                              0643 02758000
         MGCR  (1)                                                      02759000
*           BAINTSET='0'B;                                         0644 02760000
         L     @07,CVTMSER(,LCVTPTR)                               0644 02761000
         NI    BAINTSET(@07),B'11011111'                           0644 02762000
*         END;                                                     0645 02763000
@DE00606 L     @07,CVTMSER(,LCVTPTR)                               0645 02764000
         CLI   BAIPLCC(@07),X'00'                                  0645 02765000
         BNE   @DL00606                                            0645 02766000
         TM    TCGNSET(TCWAPTR),B'00001000'                        0645 02767000
         BO    @DL00606                                            0645 02768000
*       END;                                                       0646 02769000
*                                                                  0646 02770000
*/********************************************************************/ 02771000
*/*                                                                  */ 02772000
*/* AFTER MESSAGE IEA886A OR MESSAGE IEA887A IS ISSUED, OR IF        */ 02773000
*/* NEITHER WAS TO BE ISSUED, ISSUE MESSAGE IEA888A UNTIL THE OPERA- */ 02774000
*/* TOR'S SYNTAX IS CORRECT AND A CLOCK HAS BEEN SET.                */ 02775000
*/*                                                                  */ 02776000
*/* FIRST, INDICATE THAT NEITHER MESSAGE IEA886A NOR IEA887A IS OUT- */ 02777000
*/* STANDING AND THAT THE CONVERSION ROUTINE SHOULD NOT SAVE ELAPSED */ 02778000
*/* DAYS.                                                            */ 02779000
*/*                                                                  */ 02780000
*/* THEN INDICATE THAT MESSAGE IEA888A IS OUTSTANDING, AND THAT THE  */ 02781000
*/* CVTDATE MUST BE SET.  TURN ON THE CLOCK NOT SET INDICATOR TO     */ 02782000
*/* ENSURE THAT THE LOOP WILL BE EXECUTED AT LEAST ONCE.             */ 02783000
*/*                                                                  */ 02784000
*/********************************************************************/ 02785000
*                                                                  0647 02786000
*   DO;                                                            0647 02787000
@RF00601 DS    0H                                                  0647 02788000
@RC00581 DS    0H                                                  0648 02789000
*     TCGYYY='0'B;                  /* NOT MSG IEA886A       @YM08621*/ 02790000
*     TCGZZZ='0'B;                  /* NOT MSG IEA887A       @YM08621*/ 02791000
*     TCGLELD='0'B;                 /* DO NOT SAVE DAYS      @YM08621*/ 02792000
*     TCGXXX='1'B;                  /* MSG IEA888A           @YM08621*/ 02793000
*     TCGCVTD='1'B;                 /* SET CVTDATE           @YM08621*/ 02794000
*     TCGNSET='1'B;                 /* CLOCK NOT SET         @YM08621*/ 02795000
         OI    TCGXXX(TCWAPTR),B'10001010'                         0653 02796000
         NI    TCGYYY(TCWAPTR),B'10011011'                         0653 02797000
*/********************************************************************/ 02798000
*/*                                                                  */ 02799000
*/* SET UP THE INTERFACE TO HAVE THE CPU REPRESENTED BY THE          */ 02800000
*/* FIRST TOD CLOCK WORKAREA ENTRY STORE ITS CLOCK IN ORDER TO GET   */ 02801000
*/* A CURRENT VALUE.                                                 */ 02802000
*/*                                                                  */ 02803000
*/********************************************************************/ 02804000
*                                                                  0654 02805000
*     DO WHILE BAIPLCC^='00'X TCGNSET='1'B;                        0654 02806000
*                                                                  0654 02807000
         B     @DE00654                                            0654 02808000
@DL00654 DS    0H                                                  0655 02809000
*/********************************************************************/ 02810000
*/*                                                                  */ 02811000
*/* TEST FOR STRICTLY UNI-PROCESSOR CONFIGURATION.  IF SO, INVOKE    */ 02812000
*/* ROUTINE TO STORE TOD CLOCK VIA BRANCH AND LINK.  IF NOT, INVOKE  */ 02813000
*/* THE ROUITNE VIA THE IPC.                                         */ 02814000
*/*                                                                  */ 02815000
*/********************************************************************/ 02816000
*                                                                  0655 02817000
*       IF CSDMP='0'B THEN                                         0655 02818000
         L     @07,CVTCSD(,LCVTPTR)                                0655 02819000
         TM    CSDMP(@07),B'10000000'                              0655 02820000
         BNZ   @RF00655                                            0655 02821000
*         DO;                                                      0656 02822000
*           REG15=ADDR(DELTA3);     /* GET ENTRY POINT ADDRESS       */ 02823000
         LA    REG15,DELTA3                                        0657 02824000
*           REG1=ADDR(TCWAA1);      /* GET ADDRESS OF STCK AREA      */ 02825000
         LA    REG1,TCWAA1(,TCWAPTR)                               0658 02826000
*           GENERATE CODE REFS(REG15) SETS(REG14)(BALR  REG14,REG15);   02827000
         BALR  REG14,REG15                                              02828000
*                                   /* BALR TO ROUTINE               */ 02829000
*         END;                                                     0660 02830000
*       ELSE                                                       0661 02831000
*                                                                  0661 02832000
*/********************************************************************/ 02833000
*/*                                                                  */ 02834000
*/* SET UP IPC INTERFACE.                                            */ 02835000
*/*                                                                  */ 02836000
*/********************************************************************/ 02837000
*                                                                  0661 02838000
*         DO;                                                      0661 02839000
         B     @RC00655                                            0661 02840000
@RF00655 DS    0H                                                  0662 02841000
*           REG11=ADDR(TCWAA1);     /* GET PARAMETER ADDRESS         */ 02842000
         LA    REG11,TCWAA1(,TCWAPTR)                              0662 02843000
*           REG12=ADDR(DELTA3);     /* GET ROUTINE ADDRESS           */ 02844000
         LA    REG12,DELTA3                                        0663 02845000
*           REG1=PTREBEG->TCWAPCCA; /* GET ADDRESS PCCA              */ 02846000
         L     @07,PTREBEG                                         0664 02847000
         L     REG1,TCWAPCCA(,@07)                                 0664 02848000
*           GENERATE CODE REFS(CVTIPCRI,CVT)                       0665 02849000
*               (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));         0665 02850000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        02851000
*         END;                                                     0666 02852000
*                                                                  0666 02853000
*/********************************************************************/ 02854000
*/*                                                                  */ 02855000
*/* ADD THE TIME ZONE CONSTANT TO THE STORED VALUE TO GET THE LOCAL  */ 02856000
*/* CLOCK VALUE, AND SET UP THE PARAMETER LIST POINTERS FOR THE TIME */ 02857000
*/* VALUE TO BE CONVERTED.                                           */ 02858000
*/*                                                                  */ 02859000
*/********************************************************************/ 02860000
*                                                                  0667 02861000
*       TCWAA2L=TCWAA1L+CVTTZ;                                     0667 02862000
@RC00655 L     @07,TCWAA1L(,TCWAPTR)                               0667 02863000
         AL    @07,CVTTZ(,LCVTPTR)                                 0667 02864000
         ST    @07,TCWAA2L(,TCWAPTR)                               0667 02865000
*       TCWAA2R=TCWAA1R;            /* USE LOW ORDER BITS TOO      0668 02866000
*                                                            @ZA10129*/ 02867000
         MVC   TCWAA2R(4,TCWAPTR),TCWAA1R(TCWAPTR)                 0668 02868000
*       TCWAPTP=ADDR(TCWAPLT);                                     0669 02869000
         LA    @07,TCWAPLT(,TCWAPTR)                               0669 02870000
         ST    @07,TCWAPTP(,TCWAPTR)                               0669 02871000
*       TCWACVP=ADDR(TCWAA2);                                      0670 02872000
*                                                                  0670 02873000
         LA    @07,TCWAA2(,TCWAPTR)                                0670 02874000
         ST    @07,TCWACVP(,TCWAPTR)                               0670 02875000
*/********************************************************************/ 02876000
*/*                                                                  */ 02877000
*/* THEN SEE IF THE CVTDATE REQUIRES INITIALIZING.                   */ 02878000
*/*                                                                  */ 02879000
*/********************************************************************/ 02880000
*                                                                  0671 02881000
*       IF TCGCVTD='1'B THEN                                       0671 02882000
*                                                                  0671 02883000
         TM    TCGCVTD(TCWAPTR),B'00000010'                        0671 02884000
         BNO   @RF00671                                            0671 02885000
*/********************************************************************/ 02886000
*/*                                                                  */ 02887000
*/* IF SO, SET UP THE DATE PARAMETER POINTER SO THAT THE CONVERSION  */ 02888000
*/* ROUTINE WILL STORE THE DATE INTO  THE CVT.  THEN INDICATE THAT   */ 02889000
*/* IT HAS BEEN INITIALIZED.                                         */ 02890000
*/*                                                                  */ 02891000
*/********************************************************************/ 02892000
*                                                                  0672 02893000
*         DO;                                                      0672 02894000
*           TCWAPDP=ADDR(CVTDATE);                                 0673 02895000
         LA    @07,CVTDATE(,LCVTPTR)                               0673 02896000
         ST    @07,TCWAPDP(,TCWAPTR)                               0673 02897000
*           TCGCVTD='0'B;                                          0674 02898000
         NI    TCGCVTD(TCWAPTR),B'11111101'                        0674 02899000
*         END;                                                     0675 02900000
*       ELSE                                                       0676 02901000
*                                                                  0676 02902000
*/********************************************************************/ 02903000
*/*                                                                  */ 02904000
*/* IF CVTDATE HAS BEEN INITIALIZED, HAVE THE CONVERSION ROUTINE     */ 02905000
*/* STORE THE DATE INTO THE TOD CLOCK WORKAREA.                      */ 02906000
*/*                                                                  */ 02907000
*/********************************************************************/ 02908000
*                                                                  0676 02909000
*         TCWAPDP=ADDR(TCWAPLD);                                   0676 02910000
*                                                                  0676 02911000
         B     @RC00671                                            0676 02912000
@RF00671 LA    @07,TCWAPLD(,TCWAPTR)                               0676 02913000
         ST    @07,TCWAPDP(,TCWAPTR)                               0676 02914000
*/********************************************************************/ 02915000
*/*                                                                  */ 02916000
*/* POINT THE PARAMETER REGISTER TO THE PARAMETER LIST AND CALL THE  */ 02917000
*/* ROUTINE THAT CONVERTS A TOD CLOCK VALUE TO PACKED DATE AND TIME. */ 02918000
*/* INDICATE TO THE CONVERSION ROUTINE TO SAVE DAYS ELAPSED PLUS 1   */ 02919000
*/* FOR LATER USE IN CALCULATING THE VALUE OF THE MIDNIGHT TQE.      */ 02920000
*/*                                                                  */ 02921000
*/********************************************************************/ 02922000
*                                                                  0677 02923000
*       REG1=ADDR(TCWAPTP);                                        0677 02924000
@RC00671 LA    REG1,TCWAPTP(,TCWAPTR)                              0677 02925000
*       TCGLELD='1'B;                                              0678 02926000
         OI    TCGLELD(TCWAPTR),B'00000100'                        0678 02927000
*       CALL CUTODATE;                                             0679 02928000
         BAL   @14,CUTODATE                                        0679 02929000
*       TCGLELD='0'B;                                              0680 02930000
*                                                                  0680 02931000
         NI    TCGLELD(TCWAPTR),B'11111011'                        0680 02932000
*/********************************************************************/ 02933000
*/*                                                                  */ 02934000
*/* SEE IF THE OPERATOR IS TO BE PROMPTED.                           */ 02935000
*/*                                                                  */ 02936000
*/********************************************************************/ 02937000
*                                                                  0681 02938000
*       IF MSTODWTO='0'B            /*                        @Y02675*/ 02939000
*/********************************************************************/ 02940000
*/*                                                                  */ 02941000
*/* IF SO, SET UP THE INTERFACE TO GET THE TOD CLOCK VALUE JUST      */ 02942000
*/* STORED INTO PACKED GREENWICH MEAN TIME AND DATE, AND CALL THE    */ 02943000
*/* CONVERSION ROUTINE.                                              */ 02944000
*/*                                                                  */ 02945000
*/********************************************************************/ 02946000
*                                                                  0681 02947000
*         THEN                                                     0681 02948000
         L     @07,CVTMSER(,LCVTPTR)                               0681 02949000
         TM    MSTODWTO(@07),B'01000000'                           0681 02950000
         BNZ   @RF00681                                            0681 02951000
*         DO;                                                      0682 02952000
*           TCWAPTP=ADDR(TCWAPGT);                                 0683 02953000
         LA    @07,TCWAPGT(,TCWAPTR)                               0683 02954000
         ST    @07,TCWAPTP(,TCWAPTR)                               0683 02955000
*           TCWAPDP=ADDR(TCWAPGD);                                 0684 02956000
         LA    @07,TCWAPGD(,TCWAPTR)                               0684 02957000
         ST    @07,TCWAPDP(,TCWAPTR)                               0684 02958000
*           TCWACVP=ADDR(TCWAA1);                                  0685 02959000
         LA    @07,TCWAA1(,TCWAPTR)                                0685 02960000
         ST    @07,TCWACVP(,TCWAPTR)                               0685 02961000
*           REG1=ADDR(TCWAPTP);                                    0686 02962000
         LA    REG1,TCWAPTP(,TCWAPTR)                              0686 02963000
*           CALL CUTODATE;                                         0687 02964000
*                                                                  0687 02965000
         BAL   @14,CUTODATE                                        0687 02966000
*/********************************************************************/ 02967000
*/*                                                                  */ 02968000
*/* ASSEMBLE THE MESSAGE FOR DISPLAYING THE GREENWICH MEAN TIME AND  */ 02969000
*/* DATE, AND WRITE THE MESSAGE.                                     */ 02970000
*/*                                                                  */ 02971000
*/********************************************************************/ 02972000
*                                                                  0688 02973000
*           WPLPTR=ADDR(MSGXXX1);                                  0688 02974000
*                                                                  0688 02975000
         LA    WPLPTR,MSGXXX1                                      0688 02976000
*/* C271100-271500                                           @YM04380*/ 02977000
*                                                                  0689 02978000
*           DATEX1=DTEPATRN;        /* EDITING PATTERN TO MSG        */ 02979000
*           ED(DATEX1,TCWAPGD(2:4));/* EDIT DATE INTO MSG            */ 02981000
         ZAP   TCWAWRK1(4,TCWAPTR),TCWAPGD(4,TCWAPTR)              @L01+02981100
                                   Copy packed date                @L01 02981200
         AP    TCWAWRK1(4,TCWAPTR),P1900                           @L01+02981300
                                   Adjust for 4-digit year         @L01 02981400
         OI    TCWAWRK1+3(TCWAPTR),X'0F'                           @L01+02981500
                                   Force printable sign            @L01 02981600
         MVC   DATEX1(9,WPLPTR),DTEPATRN                           @L01+02981700
                                   Move in edit pattern            @L01 02981800
         ED    DATEX1(9,WPLPTR),TCWAWRK1(TCWAPTR)                  @L01+02981900
                                   Edit date into message          @L01 02982000
*           DATEX1(1)='=';          /* PUT = SIGN AFTER DATE         */ 02983000
         MVI   DATEX1(WPLPTR),C'='                                 0691 02984000
*           CLOCKX1=CLKPATRN;       /* EDITING PATTERN TO MSG        */ 02985000
         MVC   CLOCKX1(9,WPLPTR),CLKPATRN                          0692 02986000
*           ED(CLOCKX1,TCWAPGT);    /* EDIT CLOCK INTO MSG           */ 02987000
         ED    CLOCKX1(9,WPLPTR),TCWAPGT(TCWAPTR)                  0693 02988000
*           CLOCKX1(1)='=';         /* PUT = SIGN AFTER CLOCK        */ 02989000
         MVI   CLOCKX1(WPLPTR),C'='                                0694 02990000
*           GENERATE CODE(WTO    MF=(E,(1)));                      0695 02991000
*                                                                  0695 02992000
         WTO    MF=(E,(1))                                              02993000
*/********************************************************************/ 02994000
*/*                                                                  */ 02995000
*/* ASSEMBLE THE MESSAGE FOR DISPLAYING THE LOCAL TIME AND DATE,     */ 02996000
*/* AND WRITE THE MESSAGE.                                           */ 02997000
*/*                                                                  */ 02998000
*/********************************************************************/ 02999000
*                                                                  0696 03000000
*           WPLPTR=ADDR(MSGXXX2)+LENGTH(WPLRF);                    0696 03001000
*                                                                  0696 03002000
         LA    @07,MSGXXX2                                         0696 03003000
         LA    WPLPTR,8                                            0696 03004000
         ALR   WPLPTR,@07                                          0696 03005000
*/* C272600-273000                                           @YM04380*/ 03006000
*                                                                  0697 03007000
*           DATEX2=DTEPATRN;        /* EDITING PATTERN TO MSG        */ 03008000
*           ED(DATEX2,CVTDATE(2:4));/* EDIT DATE INTO MSG            */ 03010000
         ZAP   TCWAWRK1(4,TCWAPTR),CVTDATE(4,LCVTPTR)              @L01+03010100
                                   Copy packed date                @L01 03010200
         AP    TCWAWRK1(4,TCWAPTR),P1900                           @L01+03010300
                                   Adjust for 4-digit year         @L01 03010400
         OI    TCWAWRK1+3(TCWAPTR),X'0F'                           @L01+03010500
                                   Force printable sign            @L01 03010600
         MVC   DATEX2(9,WPLPTR),DTEPATRN                           @L01+03010700
                                   Move in edit pattern            @L01 03010800
         ED    DATEX2(9,WPLPTR),TCWAWRK1(TCWAPTR)                  @L01+03010900
                                   Edit date into message          @L01 03011000
*           DATEX2(1)='=';          /* PUT = SIGN AFTER DATE         */ 03012000
         MVI   DATEX2(WPLPTR),C'='                                 0699 03013000
*           CLOCKX2=CLKPATRN;       /* EDITING PATTERN TO MSG        */ 03014000
         MVC   CLOCKX2(9,WPLPTR),CLKPATRN                          0700 03015000
*           ED(CLOCKX2,TCWAPLT);    /* EDIT CLOCK INTO MSG           */ 03016000
         ED    CLOCKX2(9,WPLPTR),TCWAPLT(TCWAPTR)                  0701 03017000
*           CLOCKX2(1)='=';         /* PUT = SIGN AFTER CLOCK        */ 03018000
         MVI   CLOCKX2(WPLPTR),C'='                                0702 03019000
*           WPLPTR=ADDR(MSGXXX2);   /* MSG ADDR TO WPL               */ 03020000
         LR    WPLPTR,@07                                          0703 03021000
*           WPLRPTRA=ADDR(TCWATXT); /* REPLY ADDR TO WPL             */ 03022000
         LA    @07,TCWATXT(,TCWAPTR)                               0704 03023000
         STCM  @07,7,WPLRPTRA(WPLPTR)                              0704 03024000
*           WPLRECB=ADDR(TODECB);   /* ECB ADDR TO WPL               */ 03025000
         LA    @07,TODECB                                          0705 03026000
         ST    @07,WPLRECB(,WPLPTR)                                0705 03027000
*           TODECB=0;                                              0706 03028000
         SLR   @07,@07                                             0706 03029000
         ST    @07,TODECB                                          0706 03030000
*           TCWATXT='';                                            0707 03031000
         MVI   TCWATXT(TCWAPTR),C' '                               0707 03032000
         MVC   TCWATXT+1(39,TCWAPTR),TCWATXT(TCWAPTR)              0707 03033000
*           GENERATE CODE;                                         0708 03034000
*                                                                  0708 03035000
*                                /*                                     03036000
         WTOR  MF=(E,(1))                                         */    03037000
*/********************************************************************/ 03038000
*/*                                                                  */ 03039000
*/* WAIT FOR REPLY.                                                  */ 03040000
*/*                                                                  */ 03041000
*/********************************************************************/ 03042000
*                                                                  0709 03043000
*           REG1=ADDR(TODECB);                                     0709 03044000
         LA    REG1,TODECB                                         0709 03045000
*           GENERATE CODE(WAIT  1,ECB=(1));                        0710 03046000
         WAIT  1,ECB=(1)                                                03047000
*         END;                                                     0711 03048000
*       ELSE                                                       0712 03049000
*                                                                  0712 03050000
*/********************************************************************/ 03051000
*/*                                                                  */ 03052000
*/* IF NO PROMPTING IS REQUIRED, SIMULATE THE OPERATORS ACCEPTANCE   */ 03053000
*/* OF THE CLOCK VALUE BY PLACING A CAPITAL U IN THE REPLY BUFFER.   */ 03054000
*/*                                                                  */ 03055000
*/********************************************************************/ 03056000
*                                                                  0712 03057000
*         TCWATXT(1:2)=CAPU;                                       0712 03058000
*                                                                  0712 03059000
         B     @RC00681                                            0712 03060000
@RF00681 MVC   TCWATXT(2,TCWAPTR),CAPU                             0712 03061000
*/********************************************************************/ 03062000
*/*                                                                  */ 03063000
*/* SEE IF THE OPERATOR HAS ACCEPTED THE CLOCK VALUES.               */ 03064000
*/*                                                                  */ 03065000
*/********************************************************************/ 03066000
*                                                                  0713 03067000
*       BAIPLCC='00'X;              /* CLEAR ERROR BYTE IN BASEA     */ 03068000
@RC00681 L     @07,CVTMSER(,LCVTPTR)                               0713 03069000
         MVI   BAIPLCC(@07),X'00'                                  0713 03070000
*       TCGNSET='0'B;               /* TURN OFF CLOCK NOT SET      0714 03071000
*                                      INDICATOR                     */ 03072000
         NI    TCGNSET(TCWAPTR),B'11110111'                        0714 03073000
*       IF TCWATXT(1:2)^=CAPU&TCWATXT(1:2)^=SMALLU THEN            0715 03074000
*                                                                  0715 03075000
         CLC   TCWATXT(2,TCWAPTR),CAPU                             0715 03076000
         BE    @RF00715                                            0715 03077000
         CLC   TCWATXT(2,TCWAPTR),SMALLU                           0715 03078000
         BE    @RF00715                                            0715 03079000
*/********************************************************************/ 03080000
*/*                                                                  */ 03081000
*/* IF NOT, INDICATE AN INTERNAL SET COMMAND, AND ISSUE SVC 34 USING */ 03082000
*/* THE OPERATOR'S REPLY AS THE COMMAND OPERAND(S).                  */ 03083000
*/* UPON RETURN, TURN OFF THE INTERNAL SET INDICATOR, AND TURN ON    */ 03084000
*/* THE CLOCK NOT SET INDICATOR.  THE LATTER ENSURES THAT MESSAGE    */ 03085000
*/* IEA888A  WILL BE ISSUED AGAIN, DISPLAYING THE NEW VALUES TO THE  */ 03086000
*/* OPERATOR.                                                        */ 03087000
*/*                                                                  */ 03088000
*/********************************************************************/ 03089000
*                                                                  0716 03090000
*         DO;                                                      0716 03091000
*           REG0=0;                                                0717 03092000
         SLR   REG0,REG0                                           0717 03093000
*           REG1=ADDR(TCWARPLY);                                   0718 03094000
         LA    REG1,TCWARPLY(,TCWAPTR)                             0718 03095000
*           BAINTSET='1'B;          /* INDICATE INTERNAL SET COMMAND */ 03096000
         OI    BAINTSET(@07),B'00100000'                           0719 03097000
*           GENERATE CODE(MGCR  (1));                              0720 03098000
         MGCR  (1)                                                      03099000
*           BAINTSET='0'B;                                         0721 03100000
         L     @07,CVTMSER(,LCVTPTR)                               0721 03101000
         NI    BAINTSET(@07),B'11011111'                           0721 03102000
*           TCGNSET='1'B;                                          0722 03103000
         OI    TCGNSET(TCWAPTR),B'00001000'                        0722 03104000
*         END;                                                     0723 03105000
*     END;                                                         0724 03106000
@RF00715 DS    0H                                                  0724 03107000
@DE00654 L     @07,CVTMSER(,LCVTPTR)                               0724 03108000
         CLI   BAIPLCC(@07),X'00'                                  0724 03109000
         BNE   @DL00654                                            0724 03110000
         TM    TCGNSET(TCWAPTR),B'00001000'                        0724 03111000
         BO    @DL00654                                            0724 03112000
*   END;                                                           0725 03113000
*                                                                  0725 03114000
*/********************************************************************/ 03115000
*/*                                                                  */ 03116000
*/* RETURN TO THE CALLER.                                            */ 03117000
*/*                                                                  */ 03118000
*/********************************************************************/ 03119000
*                                                                  0726 03120000
*   RESPECIFY                                                      0726 03121000
*     CVTMAP BASED(CVTPTR);                                        0726 03122000
*                                                                  0727 03123000
*/********************************************************************/ 03124000
*/*                                                                  */ 03125000
*/* THIS PROCEDURE CONVERTS TOD CLOCK VALUES TO PACKED DATE AND TIME.*/ 03126000
*/* IT USES A 2 STEP PROCESS.  THE FIRST STEP FACTORS COMPLETE DAYS  */ 03127000
*/* ELAPSED SINCE THE EPOCH OUT OF THE HIGH ORDER 39 BITS OF THE TOD */ 03128000
*/* CLOCK VALUE, AND THEN CONVERTS TO YEAR AND DAY OF YEAR.  THE     */ 03129000
*/* SECOND STEP TAKES THE REMAINDER OF THE FIRST STEP, CONCATENATES  */ 03130000
*/* IT TO THE LOW ORDER 13 BITS OF THE TOD CLOCK, AND CONVERTS TO    */ 03131000
*/* HOURS, MINUTES, AND SECONDS ELAPSED SINCE THE LAST MIDNIGHT.     */ 03132000
*/*                                                                  */ 03133000
*/********************************************************************/ 03134000
*                                                                  0727 03135000
*CUTODATE:                                                         0727 03136000
*   PROCEDURE(PARMTIME,PARMDATE,CONVALUE) OPTIONS(NOSAVE,NOSAVEAREA);   03137000
         B     @PB00006                                            0727 03138000
CUTODATE MVC   @PC00006(12),0(@01)                                 0727 03139000
*   DECLARE                                                        0728 03140000
*     PARMTIME CHAR(4);             /* CONVERTED PACKED TIME         */ 03141000
*   DECLARE                                                        0729 03142000
*     PARMDATE CHAR(4);             /* CONVERTED PACKED DATE         */ 03143000
*   DECLARE                                                        0730 03144000
*     1 CONVALUE BIT(64) BDY(WORD), /* 64 BIT TOD CLOCK VALUE TO BE     03145000
*                                      CONVERTED             @ZA10129*/ 03146000
*       3 HO32BITS FIXED(32) UNSIGNED,/* H. O. TOD CLOCK     @ZA10129*/ 03147000
*       3 LO32BITS FIXED(32) UNSIGNED;/* L. O. TOD CLOCK     @ZA10129*/ 03148000
*/********************************************************************/ 03149000
*/*                                                                  */ 03150000
*/* SAVE THE RETURN REGISTER ACROSS THE ROUTINE.                     */ 03151000
*/*                                                                  */ 03152000
*/********************************************************************/ 03153000
*                                                                  0731 03154000
*   RETSAVE=REG14;                                                 0731 03155000
*                                                                  0731 03156000
         ST    REG14,RETSAVE                                       0731 03157000
*/********************************************************************/ 03158000
*/*                                                                  */ 03159000
*/* SAVE THE LOW ORDER PART OF THE TOD CLOCK FOR LATER USE.  PUT THE */ 03160000
*/* TOD CLOCK VALUE TO BE CONVERTED INTO REGISTERS 14 AND 15.  NOTE  */ 03161000
*/* THAT BIT 19 OF REGISTER 15 (BIT 51 OF THE TOD CLOCK) REPRESENTS  */ 03162000
*/* 1 MICROSECOND.                                                   */ 03163000
*/*                                                                  */ 03164000
*/********************************************************************/ 03165000
*                                                                  0732 03166000
*   REG1=LO32BITS;                  /*                       @ZA10129*/ 03167000
         L     @07,@PC00006+8                                      0732 03168000
         L     REG1,LO32BITS(,@07)                                 0732 03169000
*   LM(REG14,REG15,CONVALUE);       /*                       @ZA10129*/ 03170000
         LM    REG14,REG15,CONVALUE(@07)                           0733 03171000
*/********************************************************************/ 03172000
*/*                                                                  */ 03173000
*/* THE NEXT 2 INSTRUCTIONS EFFECTIVELY SHIFT BIT 51 OF THE TOD CLOCK*/ 03174000
*/* VALUE TO BIT 63 OF THE DIVIDEND, AND THEN DIVIDE THE RESULTANT   */ 03175000
*/* MICROSECOND VALUE BY 86,400,000,000, THE NUMBER OF MICROSECONDS  */ 03176000
*/* IN 1 DAY.  THIS DIVISION PROVIDES DAYS ELAPSED SINCE THE EPOCH   */ 03177000
*/* IN REGISTER 15, AND THE NUMBER OF 8192 (2**13) MICROSECOND UNITS */ 03178000
*/* ELAPSED SINCE THE LAST MIDNIGHT IN REGISTER 14.                  */ 03179000
*/*                                                                  */ 03180000
*/* NOTE THAT 86,400,000,000 IS EQUAL TO (86,400)*(1,000,000), OR    */ 03181000
*/* THE NUMBER OF SECONDS IN A DAY TIMES THE NUMBER OF MICROSECONDS  */ 03182000
*/* IN A SECOND.                                                     */ 03183000
*/*              86,400 = (2**7)*(5**2)*(3**3)                       */ 03184000
*/*           1,000,000 = (2**6)*(5**6)                              */ 03185000
*/*      86,400,000,000 = (2**13)*(5**8)*(3**3)                      */ 03186000
*/*                                                                  */ 03187000
*/* SINCE A SHIFT OF 12 BITS TO THE RIGHT IS THE EQUIVALENT OF       */ 03188000
*/* 1/(2**12), THE SHIFT AND DIVISION INSTRUCTIONS ARE THE SAME AS   */ 03189000
*/* 1/((2**25)*(5**8)*(3**3)).  THE SHIFT DOES 1/(2**25) AND THE     */ 03190000
*/* DIVISION DOES 1/((5**8)*(3**3)).                                 */ 03191000
*/*                                                                  */ 03192000
*/********************************************************************/ 03193000
*                                                                  0734 03194000
*   SRDL(REG14,25);                 /* SHIFT 12 RIGHT AND DIVIDE BY     03195000
*                                      2**13                 @ZA10129*/ 03196000
         SRDL  REG14,25                                            0734 03197000
*   GENERATE CODE REFS(DAYSFACT) SETS(REG14,REG15)                 0735 03198000
*       (D     REG14,DAYSFACT);     /* DIVIDE BY (5**8)*(3**3)       */ 03199000
         D     REG14,DAYSFACT                                           03200000
*/********************************************************************/ 03201000
*/*                                                                  */ 03202000
*/* ADD 1 DAY.  (NOTE: ADDING 1 MAKES UP FOR THE FACT THAT DATE IS   */ 03203000
*/* ONE-ORIGINED.)                                                   */ 03204000
*/*                                                                  */ 03205000
*/********************************************************************/ 03206000
*                                                                  0736 03207000
*   REG15=REG15+1;                                                 0736 03208000
*                                                                  0736 03209000
         AL    REG15,@CF00096                                      0736 03210000
*/********************************************************************/ 03211000
*/*                                                                  */ 03212000
*/* SEE IF A LOCAL VALUE IS BEING CALCULATED, AND IF SO, SAVE THE    */ 03213000
*/* NUMBER OF DAYS THAT WILL HAVE ELAPSED AT LOCAL MIDNIGHT IN THE   */ 03214000
*/* TOD CLOCK WORKAREA.                                              */ 03215000
*/*                                                                  */ 03216000
*/********************************************************************/ 03217000
*                                                                  0737 03218000
*   IF TCGLELD='1'B THEN                                           0737 03219000
         TM    TCGLELD(TCWAPTR),B'00000100'                        0737 03220000
         BNO   @RF00737                                            0737 03221000
*     TCELDAYS=REG15;                                              0738 03222000
         ST    REG15,TCELDAYS(,TCWAPTR)                            0738 03223000
*   REG15=4*REG15;                  /* MULTIPLY DAYS ELAPSED BY 4    */ 03224000
@RF00737 SLL   REG15,2                                             0739 03225000
*/********************************************************************/ 03226000
*/*                                                                  */ 03227000
*/* SAVE REGISTER 14 FOR LATER USE.  THEN CLEAR IT FOR DIVISION.     */ 03228000
*/* (NOTE:  REGISTER 14 CONTAINS THE NUMBER OF 2**13 MICROSECOND     */ 03229000
*/* UNITS ELAPSED SINCE THE LAST MIDNIGHT.)                          */ 03230000
*/*                                                                  */ 03231000
*/********************************************************************/ 03232000
*                                                                  0740 03233000
*   REG0=REG14;                                                    0740 03234000
         LR    REG0,REG14                                          0740 03235000
*   REG14=0;                                                       0741 03236000
*                                                                  0741 03237000
         SLR   REG14,REG14                                         0741 03238000
*/********************************************************************/ 03239000
*/*                                                                  */ 03240000
*/* DIVIDE BY THE NUMBER OF DAYS IN A "NORMAL" 4 YEAR PERIOD GIVING  */ 03241000
*/* YEAR OF THE CENTURY IN REGISTER 15.  DIVIDE THE REMAINDER BY 4   */ 03242000
*/* GIVING DAYS ELAPSED SINCE THE BEGINNING OF THE YEAR  (EXCEPT     */ 03243000
*/* 1900) IN REGISTER 14.                                            */ 03244000
*/*                                                                  */ 03245000
*/********************************************************************/ 03246000
*                                                                  0742 03247000
*   GENERATE CODE REFS(DAYS4YRS) SETS(REG14,REG15)                 0742 03248000
*       (D     REG14,DAYS4YRS);                                    0742 03249000
         D     REG14,DAYS4YRS                                           03250000
*   SRL(REG14,2);                                                  0743 03251000
*                                                                  0743 03252000
         SRL   REG14,2                                             0743 03253000
*/********************************************************************/ 03254000
*/*                                                                  */ 03255000
*/* TEST FOR A YEAR NOT 1900.                                        */ 03256000
*/*                                                                  */ 03257000
*/********************************************************************/ 03258000
*                                                                  0744 03259000
*   IF REG15^=0 THEN                                               0744 03260000
*                                                                  0744 03261000
         LTR   REG15,REG15                                         0744 03262000
         BZ    @RF00744                                            0744 03263000
*/********************************************************************/ 03264000
*/*                                                                  */ 03265000
*/* IF THE YEAR IS NOT 1900, ADD 1 DAY TO MAKE IT ONE-ORIGIN.  THEN  */ 03266000
*/* SHIFT YEARS FOR CORRECT ALIGNMENT WITH DAYS.                     */ 03267000
*/*                                                                  */ 03268000
*/********************************************************************/ 03269000
*                                                                  0745 03270000
*     DO;                                                          0745 03271000
*       REG14=REG14+1;                                             0746 03272000
         AL    REG14,@CF00096                                      0746 03273000
*       REG15=REG15*K1000;                                         0747 03274000
         LR    @07,REG15                                           0747 03275000
         MH    @07,K1000                                           0747 03276000
         LR    REG15,@07                                           0747 03277000
*     END;                                                         0748 03278000
*                                                                  0748 03279000
*/********************************************************************/ 03280000
*/*                                                                  */ 03281000
*/* ADD DAYS TO YEARS.                                               */ 03282000
*/*                                                                  */ 03283000
*/********************************************************************/ 03284000
*                                                                  0749 03285000
*   REG15=REG15+REG14;                                             0749 03286000
*                                                                  0749 03287000
@RF00744 ALR   REG15,REG14                                         0749 03288000
*/********************************************************************/ 03289000
*/*                                                                  */ 03290000
*/* CONVERT TO PACKED DECIMAL AND PLACE IN THE PARAMETER AREA.       */ 03291000
*/* THEN, MAKE THE SIGN A HEX F SO THAT THE VALUE CAN BE UNPACKED.   */ 03292000
*/*                                                                  */ 03293000
*/********************************************************************/ 03294000
*                                                                  0750 03295000
*   CVD(REG15,PARMDATE);                                           0750 03296000
         L     @07,@PC00006+4                                      0750 03297000
         CVD   REG15,@TS00001                                      0750 03298000
         MVC   PARMDATE(4,@07),@TS00001+4                          0750 03299000
*   PARMDATE(4)=PARMDATE(4) '0F'X;  /*                       @OY02031*/ 03300000
         OI    PARMDATE+3(@07),X'0F'                               0751 03301000
*/********************************************************************/ 03302000
*/*                                                                  */ 03303000
*/* CONCATENATE THE LOW ORDER 13 BITS OF THE TOD CLOCK TO THE        */ 03304000
*/* REMAINDER FROM THE PREVIOUS DIVISION AND SHIFT THE CONCATENATED  */ 03305000
*/* VALUE SO THAT TOD CLOCK BIT 51 (1 MICROSECOND) IS IN THE UNITS   */ 03306000
*/* POSITION OF THE DIVIDEND, BIT 63. THEN DIVIDE BY 1 MILLION TO    */ 03307000
*/* GIVE THE NUMBER OF SECONDS ELAPSED SINCE THE LAST MIDNIGHT IN    */ 03308000
*/* REGISTER 1.                                                      */ 03309000
*/*                                                                  */ 03310000
*/********************************************************************/ 03311000
*                                                                  0752 03312000
*   SLL(REG1,7);                    /* THROW AWAY TOD CLOCK BITS   0752 03313000
*                                      33-39 THAT WERE USED IN THE 0752 03314000
*                                      INITIAL CALCULATIONS  @ZA10129*/ 03315000
         SLL   REG1,7                                              0752 03316000
*   SRDL(REG0,19);                  /* TOD CLOCK BIT 51 TO DIVIDEND     03317000
*                                      BIT 63                @ZA10129*/ 03318000
         SRDL  REG0,19                                             0753 03319000
*   GENERATE CODE REFS(MICSSEC) SETS(REG0,REG1)(D     REG0,MICSSEC);    03320000
*                                                                  0754 03321000
         D     REG0,MICSSEC                                             03322000
*/********************************************************************/ 03323000
*/*                                                                  */ 03324000
*/* DIVIDE SECONDS ELAPSED SINCE THE LAST MIDNIGHT BY 60 GIVING SEC- */ 03325000
*/* ONDS ELAPSED SINCE THE LAST MINUTE IN REGISTER 0, AND MINUTES    */ 03326000
*/* ELAPSED SINCE THE LAST MIDNIGHT IN REGISTER 1.                   */ 03327000
*/*                                                                  */ 03328000
*/********************************************************************/ 03329000
*                                                                  0755 03330000
*   REG0=0;                                                        0755 03331000
         SLR   REG0,REG0                                           0755 03332000
*   GENERATE CODE REFS(REG0,SECSMIN)(D     REG0,SECSMIN);          0756 03333000
*                                                                  0756 03334000
         D     REG0,SECSMIN                                             03335000
*/********************************************************************/ 03336000
*/*                                                                  */ 03337000
*/* SAVE THE SECONDS AND CLEAR REGISTER 0 FOR DIVISION.              */ 03338000
*/*                                                                  */ 03339000
*/********************************************************************/ 03340000
*                                                                  0757 03341000
*   REG14=REG0;                                                    0757 03342000
         LR    REG14,REG0                                          0757 03343000
*   REG0=0;                                                        0758 03344000
*                                                                  0758 03345000
         SLR   REG0,REG0                                           0758 03346000
*/********************************************************************/ 03347000
*/*                                                                  */ 03348000
*/* DIVIDE MINUTES ELAPSED SINCE MIDNIGHT BY 60 GIVING MINUTES ELAP- */ 03349000
*/* SED SINCE THE LAST HOUR IN REGISTER 0, AND HOURS ELAPSED SINCE   */ 03350000
*/* THE LAST MIDNIGHT IN REGISTER 1.                                 */ 03351000
*/*                                                                  */ 03352000
*/********************************************************************/ 03353000
*                                                                  0759 03354000
*   GENERATE CODE REFS(REG0,MINSHOUR)(D     REG0,MINSHOUR);        0759 03355000
*                                                                  0759 03356000
         D     REG0,MINSHOUR                                            03357000
*/********************************************************************/ 03358000
*/*                                                                  */ 03359000
*/* SHIFT HOURS AND MINUTES FOR CORRECT ALIGNMENT AND ADD TO SECONDS.*/ 03360000
*/* CONVERT TO PACKED DECIMAL AND PLACE IN THE PARAMETER AREA.       */ 03361000
*/*                                                                  */ 03362000
*/********************************************************************/ 03363000
*                                                                  0760 03364000
*   REG14=REG14+(REG0*K100)+(REG1*K10000);                         0760 03365000
         LR    @07,REG0                                            0760 03366000
         MH    @07,K100                                            0760 03367000
         ALR   @07,REG14                                           0760 03368000
         LR    @06,REG1                                            0760 03369000
         MH    @06,K10000                                          0760 03370000
         ALR   @07,@06                                             0760 03371000
         LR    REG14,@07                                           0760 03372000
*   CVD(REG14,PARMTIME);                                           0761 03373000
*                                                                  0761 03374000
         L     @07,@PC00006                                        0761 03375000
         CVD   REG14,@TS00001                                      0761 03376000
         MVC   PARMTIME(4,@07),@TS00001+4                          0761 03377000
*/********************************************************************/ 03378000
*/*                                                                  */ 03379000
*/* RETRIEVE THE CALLER'S RETURN ADDRESS.                            */ 03380000
*/*                                                                  */ 03381000
*/********************************************************************/ 03382000
*                                                                  0762 03383000
*   REG14=RETSAVE;                                                 0762 03384000
*                                                                  0762 03385000
         L     REG14,RETSAVE                                       0762 03386000
*/********************************************************************/ 03387000
*/*                                                                  */ 03388000
*/* RETURN TO THE CALLER                                             */ 03389000
*/*                                                                  */ 03390000
*/********************************************************************/ 03391000
*                                                                  0763 03392000
*   END CUTODATE;                                                  0763 03393000
@EL00006 DS    0H                                                  0763 03394000
@EF00006 DS    0H                                                  0763 03395000
@ER00006 BR    @14                                                 0763 03396000
*   END IEAVRCOM;                                                  0764 03397000
@EL00005 DS    0H                                                  0764 03398000
@EF00005 DS    0H                                                  0764 03399000
@ER00005 LM    @14,@12,12(@13)                                     0764 03400000
         BR    @14                                                 0764 03401000
*   RESPECIFY                                                      0765 03402000
*     CVTMAP BASED(CVTPTR);                                        0765 03403000
*   RESPECIFY                                                      0766 03404000
*    (LCVTPTR) UNRESTRICTED;                                       0766 03405000
*                                                                  0767 03406000
*/********************************************************************/ 03407000
*/*                                                                  */ 03408000
*/*            IEAVRSYN--TOD CLOCK SYNCHRONIZING ROUTINE             */ 03409000
*/*                                                                  */ 03410000
*/* THIS ROUTINE RECEIVES CONTROL FROM IEAVRINT AND IEAVRSSC WHEN IT */ 03411000
*/* HAS BEEN DETERMINED THAT THE TOD CLOCKS NEED TO BE SYNCHRONIZED. */ 03412000
*/* IT IS PASSED A LIST OF CLOCKS, THE FIRST ONE BEING THE ONE TO    */ 03413000
*/* WHICH THE OTHERS WILL BE SYNCHRONIZED.                           */ 03414000
*/*                                                                  */ 03415000
*/* WHEN THE CLOCKS HAVE APPARENTLY BEEN SYNCHRONIZED, OR IF THEY    */ 03416000
*/* CANNOT BE SYNCHRONIZED AFTER 5 ATTEMPTS, CONTROL IS RETURNED TO  */ 03417000
*/* THE CALLER.                                                      */ 03418000
*/*                                                                  */ 03419000
*/********************************************************************/ 03420000
*                                                                  0767 03421000
*IEAVRSYN:                                                         0767 03422000
*   PROCEDURE OPTIONS(NOSAVEAREA);                                 0767 03423000
*                                                                  0767 03424000
IEAVRSYN STM   @14,@12,12(@13)                                     0767 03425000
*/********************************************************************/ 03426000
*/*                                                                  */ 03427000
*/* INITIALIZE THE SECURITY SWITCH RELEASED INDICATOR IN THE TOD     */ 03428000
*/* CLOCK WORKAREA SO THE LOOP WILL BE EXECUTED AT LEAST ONCE.       */ 03429000
*/*                                                                  */ 03430000
*/********************************************************************/ 03431000
*                                                                  0768 03432000
*   TCGNSET='1'B;                                                  0768 03433000
         OI    TCGNSET(TCWAPTR),B'00001000'                        0768 03434000
*   DO WHILE TCGNSET='1'B;                                         0769 03435000
*                                                                  0769 03436000
         B     @DE00769                                            0769 03437000
@DL00769 DS    0H                                                  0770 03438000
*/********************************************************************/ 03439000
*/*                                                                  */ 03440000
*/* SEARCH THE TCWA ENTRIES UNTIL A CLOCK REQUIRING SYNCHRONIZATION  */ 03441000
*/* IS FOUND. START WITH THE SECOND ENTRY SINCE THE FIRST REPRESENTS */ 03442000
*/* THE CLOCK BEING SYNCHRONIZED TO (HEREAFTER CALLED THE MASTER),   */ 03443000
*/* AND SO CANNOT REQUIRE SYNCHRONIZING.                             */ 03444000
*/*                                                                  */ 03445000
*/********************************************************************/ 03446000
*                                                                  0770 03447000
*     REG11=PTREBEG+LENGTH(TCENTRY);/* GET POINTER TO 2ND TCWA ENTRY */ 03448000
         LA    REG11,16                                            0770 03449000
         AL    REG11,PTREBEG                                       0770 03450000
*     RESPECIFY                                                    0771 03451000
*      (REG2) RESTRICTED;                                          0771 03452000
*     DO REG2=TCCPUCNT-1 TO 1 BY-1 WHILE TCWAPCCA->PCCASYNC='0'B;  0772 03453000
         LH    REG2,TCCPUCNT(,TCWAPTR)                             0772 03454000
         BCTR  REG2,0                                              0772 03455000
         B     @DE00772                                            0772 03456000
@DL00772 L     @07,TCWAPCCA(,REG11)                                0772 03457000
         TM    PCCASYNC(@07),B'01000000'                           0772 03458000
         BNZ   @DC00772                                            0772 03459000
*       REG11=REG11+LENGTH(TCENTRY);/* POINT TO NEXT TCWA ENTRY      */ 03460000
         AL    REG11,@CF00045                                      0773 03461000
*     END;                                                         0774 03462000
         BCTR  REG2,0                                              0774 03463000
@DE00772 LTR   REG2,REG2                                           0774 03464000
         BP    @DL00772                                            0774 03465000
@DC00772 DS    0H                                                  0775 03466000
*     REG2=REG11;                   /* SAVE POINTER TO SELECTED ENTRY*/ 03467000
         LR    REG2,REG11                                          0775 03468000
*/********************************************************************/ 03469000
*/*                                                                  */ 03470000
*/* SET UP WTOR PARAMETER LIST FOR MESSAGE IEA889A.                  */ 03471000
*/*                                                                  */ 03472000
*/********************************************************************/ 03473000
*                                                                  0776 03474000
*     WPLPTR=ADDR(MSGDEPSS);        /* ADDRESSABILITY FOR PARMLIST   */ 03475000
         LA    WPLPTR,MSGDEPSS                                     0776 03476000
*     WPLRPTRA=ADDR(TCWATXT);       /* REPLY ADDRESS TO PARMLIST     */ 03477000
         LA    @07,TCWATXT(,TCWAPTR)                               0777 03478000
         STCM  @07,7,WPLRPTRA(WPLPTR)                              0777 03479000
*     WPLRECB=ADDR(TODECB);         /* ECB ADDRESS TO PARMLIST       */ 03480000
         LA    @07,TODECB                                          0778 03481000
         ST    @07,WPLRECB(,WPLPTR)                                0778 03482000
*/********************************************************************/ 03483000
*/*                                                                  */ 03484000
*/* TELL THE OPERATOR TO DEPRESS THE TOD CLOCK SECURITY SWITCH SO    */ 03485000
*/* THAT THE CLOCKS CAN BE SET.  GIVE HIM 30 SECONDS TO DO SO.  IF HE*/ 03486000
*/* DOES NOT COMPLY WITHIN THAT PERIOD, REPEAT THE MESSAGE AND BEGIN */ 03487000
*/* A NEW TIME PERIOD. REPEAT THIS PROCESS UNTIL HE COMPLIES.        */ 03488000
*/*                                                                  */ 03489000
*/********************************************************************/ 03490000
*                                                                  0779 03491000
*     DO WHILE TCGNSET='1'B;                                       0779 03492000
*                                                                  0779 03493000
         B     @DE00779                                            0779 03494000
@DL00779 DS    0H                                                  0780 03495000
*/********************************************************************/ 03496000
*/*                                                                  */ 03497000
*/* MOVE THE HIGH ORDER 32 BITS OF THE CURRENT VALUE OF THE "MASTER" */ 03498000
*/* CLOCK TO THE AREA FROM WHICH A CLOCK TO BE SYNCHRONIZED WILL     */ 03499000
*/* RETRIEVE A TEMPORARY CLOCK SETTING VALUE, AND SET THE LOW ORDER  */ 03500000
*/* 32 BITS TO ZERO.                                                 */ 03501000
*/*                                                                  */ 03502000
*/* (NOTE:  THIS VALUE IS THE VALUE WITH WHICH SOME CLOCK REQUIRING  */ 03503000
*/* SYNCHRONIZATION WILL BE INITIALLY SET SO THAT IT CAN BE DETER-   */ 03504000
*/* MINED IF THE OPERATOR HAS DEPRESSED THE SECURITY SWITCH.  THIS   */ 03505000
*/* APPROXIMATE VALUE MUST BE USED IN ORDER TO PREVENT A POTENTIAL   */ 03506000
*/* LOOP IN THE TIME SVC (SVC 11), WHILE THE LOW ORDER MUST BE ZERO  */ 03507000
*/* TO ACOMMODATE THE SYSTEM STATUS RECORDER OF THE MODEL 158.       */ 03508000
*/*                                                                  */ 03509000
*/********************************************************************/ 03510000
*                                                                  0780 03511000
*       TCWACVL=PTREBEG->TCWACLKL;                                 0780 03512000
         L     @07,PTREBEG                                         0780 03513000
         MVC   TCWACVL(4,TCWAPTR),TCWACLKL(@07)                    0780 03514000
*       TCWACVR=0;                                                 0781 03515000
*                                                                  0781 03516000
         SLR   @07,@07                                             0781 03517000
         ST    @07,TCWACVR(,TCWAPTR)                               0781 03518000
*/********************************************************************/ 03519000
*/*                                                                  */ 03520000
*/* CLEAR THE ECB, AND TELL THE OPERATOR TO DEPRESS THE TOD CLOCK    */ 03521000
*/* SECURITY SWITCH.                                                 */ 03522000
*/*                                                                  */ 03523000
*/********************************************************************/ 03524000
*                                                                  0782 03525000
*       TODECB=0;                   /* CLEAR ECB                     */ 03526000
         ST    @07,TODECB                                          0782 03527000
*       WPLPTR=ADDR(MSGDEPSS);      /* GET PARMLIST ADDR     @YM06228*/ 03528000
         LA    WPLPTR,MSGDEPSS                                     0783 03529000
*       GENERATE CODE(WTOR  MF=(E,(1)));                           0784 03530000
*                                                                  0784 03531000
         WTOR  MF=(E,(1))                                               03532000
*/********************************************************************/ 03533000
*/*                                                                  */ 03534000
*/* WAIT FOR THE OPERATOR TO REPLY BEFORE TRYING TO SET THE CLOCK.   */ 03535000
*/* (NOTE:  WTOR/WAIT IS USED HERE RATHER THAN WTO, EVEN THOUGH NO   */ 03536000
*/* PARTICULAR REPLY IS REQUIRED.  THE PURPOSE IS TO ALLOW THE       */ 03537000
*/* SYSTEM TO DISPATCH THE COMMUNICATIONS TASK IN ORDER TO GET THE   */ 03538000
*/* MESSAGE TO THE OPERATOR BEFORE THE 30 SECOND LOOP ATTEMPTING TO  */ 03539000
*/* SET THE CLOCK BEGINS.  IF WTO WAS USED, THE COMMUNICATIONS TASK  */ 03540000
*/* MIGHT NOT BE DISPATCHED, AND THE LOOP WOULD BE EXECUTED WITHOUT  */ 03541000
*/* TELLING THE OPERATOR WHAT TO DO.)                                */ 03542000
*/*                                                                  */ 03543000
*/********************************************************************/ 03544000
*                                                                  0785 03545000
*       REG1=ADDR(TODECB);          /* GET ECB ADDRESS               */ 03546000
         LA    REG1,TODECB                                         0785 03547000
*       GENERATE CODE(WAIT  1,ECB=(1));                            0786 03548000
*                                                                  0786 03549000
         WAIT  1,ECB=(1)                                                03550000
*/********************************************************************/ 03551000
*/*                                                                  */ 03552000
*/* SET UP THE INTERFACE TO IPC SO THAT THE CLOCK REQUIRING SYN-     */ 03553000
*/* CHRONIZATION WILL ATTEMPT TO SET ITS CLOCK AND SET THE SECURITY  */ 03554000
*/* SWITCH RELEASED INDICATOR.  (NOTE: REGISTER 2 CONTAINS A POINTER */ 03555000
*/* TO THE TCWA ENTRY FOR A CPU WHOSE CLOCK REQUIRES SYNCHRONIZING.) */ 03556000
*/*                                                                  */ 03557000
*/********************************************************************/ 03558000
*                                                                  0787 03559000
*       REG1=REG2->TCWAPCCA;        /* GET PCCA ADDRESS              */ 03560000
         L     REG1,TCWAPCCA(,REG2)                                0787 03561000
*       REG12=ADDR(DELTA7);         /* GET ROUTINE ADDRESS           */ 03562000
         LA    REG12,DELTA7                                        0788 03563000
*       REG11=ADDR(TCWA);           /* GET PARAMETER ADDRESS         */ 03564000
         LR    REG11,TCWAPTR                                       0789 03565000
*       GENERATE CODE REFS(CVTIPCRI,CVT)                           0790 03566000
*           (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));             0790 03567000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        03568000
*     END;                                                         0791 03569000
@DE00779 TM    TCGNSET(TCWAPTR),B'00001000'                        0791 03570000
         BO    @DL00779                                            0791 03571000
*     RESPECIFY                                                    0792 03572000
*      (REG2) UNRESTRICTED;                                        0792 03573000
*                                                                  0792 03574000
*/********************************************************************/ 03575000
*/*                                                                  */ 03576000
*/* PERFORM THE SYNCHRONIZING PROCEDURE UNTIL ONE OR MORE OF THE     */ 03577000
*/* FOLLOWING EVENTS OCCUR:                                          */ 03578000
*/*   (1) THE PROCEDURE IS COMPLETED IN LESS THAT 2**20 MICROSECONDS */ 03579000
*/*   (2) THE PROCEDURE IS PERFORMED 5 TIMES                         */ 03580000
*/*   (3) THE OPERATOR PREMATURELY RELEASES THE TOD CLOCK SECURITY   */ 03581000
*/*       SWITCH                                                     */ 03582000
*/* THE PROCEDURE CONSISTS OF THE FOLLOWING:                         */ 03583000
*/*  (1) OBTAINING A VALUE WITH WHICH ALL CLOCKS REQUIRING SYNCHRON- */ 03584000
*/*      IZATION ARE TO BE SET                                       */ 03585000
*/*  (2) SIGNALING  ALL CPU'S WITH SUCH CLOCKS TO SET THE TOD SYNC   */ 03586000
*/*      BIT TO ONE, AND TO SET THEIR CLOCKS TO THE GIVEN VALUE.     */ 03587000
*/*      NOTE:  SETTING THE TOD SYNC BIT CAUSES THE CLOCK TO ENTER   */ 03588000
*/*      THE STOPPED STATE WHEN IT IS SET.  SUBSEQUENTLY, WHEN BIT 31*/ 03589000
*/*      OF A RUNNING CLOCK CHANGES, THE STOPPED CLOCKS WILL START.  */ 03590000
*/*  (3) AGAIN SIGNALING ALL CPU'S WITH CLOCKS REQUIRING SYNCHRONI-  */ 03591000
*/*      ZATION TO SET THEIR CLOCKS TO THE GIVEN VALUE.  NOTE: THIS  */ 03592000
*/*      STEP IS REQUIRED WHERE MORE THAN 2 CLOCKS EXIST IN ORDER TO */ 03593000
*/*      GET ALL CLOCKS REQUIRING SYNCHRONIZATION STOPPED.  OTHER-   */ 03594000
*/*      WISE, A CLOCK WHICH HAD NOT YET BEEN STOPPED COULD CHANGE   */ 03595000
*/*      IN BIT 31 AND CAUSE THE ALREADY STOPPED CLOCKS TO START.    */ 03596000
*/*  (4) TESTING TO MAKE SURE THAT THE PROCEDURE HAS BEEN PERFORMED  */ 03597000
*/*      IN LESS THAN 2*20 MICROSECONDS.                             */ 03598000
*/*                                                                  */ 03599000
*/********************************************************************/ 03600000
*                                                                  0793 03601000
*     PTREBEG->TCWACLKL=0;          /* INITIALIZE THE MASTER'S CLOCK    03602000
*                                      VALUE IN HIS TCWA ENTRY       */ 03603000
         L     @02,PTREBEG                                         0793 03604000
         SLR   @07,@07                                             0793 03605000
         ST    @07,TCWACLKL(,@02)                                  0793 03606000
*/********************************************************************/ 03607000
*/*                                                                  */ 03608000
*/* PERFORM THE LOOP 5 TIMES AS LONG AS IT HAS NOT BEEN COMPLETED IN */ 03609000
*/* IN LESS THAN 2**20 MICROSECONDS.  (NOTE: THE FIRST TIME THRU THIS*/ 03610000
*/* LOOP, TCWAA2 WILL NOT EQUAL TCWACLKL BECAUSE THE LATTER WILL BE  */ 03611000
*/* 0, AND THE FORMER WILL CONTAIN THE LAST CPU TIMER VALUE STORED   */ 03612000
*/* ABOVE.)                                                          */ 03613000
*/*                                                                  */ 03614000
*/********************************************************************/ 03615000
*                                                                  0794 03616000
*     DO LOOPCTR=5 TO 1 BY-1 WHILE PTREBEG->TCWACLKL^=TCWAA2L;     0794 03617000
*                                                                  0794 03618000
         LA    @02,5                                               0794 03619000
         ST    @02,LOOPCTR                                         0794 03620000
@DL00794 L     @02,PTREBEG                                         0794 03621000
         CLC   TCWACLKL(4,@02),TCWAA2L(TCWAPTR)                    0794 03622000
         BE    @DC00794                                            0794 03623000
*/********************************************************************/ 03624000
*/*                                                                  */ 03625000
*/* SET UP THE INTERFACE TO IPC FOR OBTAINING THE CLOCK-SETTING      */ 03626000
*/* VALUE, AND THE BEGINNING VALUE.                                  */ 03627000
*/*                                                                  */ 03628000
*/********************************************************************/ 03629000
*                                                                  0795 03630000
*       REG1=PTREBEG->TCWAPCCA;     /* GET PCCA ADDRESS              */ 03631000
         L     REG1,TCWAPCCA(,@02)                                 0795 03632000
*       REG12=ADDR(DELTA4);         /* GET ROUTINE ADDRESS           */ 03633000
         LA    REG12,DELTA4                                        0796 03634000
*       REG11=ADDR(TCWA);           /* GET PARAMETER ADDRESS         */ 03635000
         LR    REG11,TCWAPTR                                       0797 03636000
*       GENERATE CODE REFS(CVTIPCRI,CVT)                           0798 03637000
*           (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));             0798 03638000
*                                                                  0798 03639000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        03640000
*/********************************************************************/ 03641000
*/*                                                                  */ 03642000
*/* CALCULATE THE VALUE BY SETTING THE LOW ORDER 32 BITS TO 0, AND   */ 03643000
*/* HIGH ORDER 32 BITS TO THE CURRENT VALUE OF THE HIGH ORDER 32     */ 03644000
*/* BITS PLUS 2**20 MICROSECONDS.  (NOTE: THE CLOCK SETTING VALUE    */ 03645000
*/* (TCWACVAL) WILL BE APPROXIMATELY 2**20 MICROSECONDS GREATER      */ 03646000
*/* THAN THE BEGIN VALUE(TCWAA2), BUT CAN NEVER BE MORE THAN 2**20   */ 03647000
*/* MICROSECONDS GREATER.)                                           */ 03648000
*/*                                                                  */ 03649000
*/********************************************************************/ 03650000
*                                                                  0799 03651000
*       TCWACVR=0;                                                 0799 03652000
         SLR   @02,@02                                             0799 03653000
         ST    @02,TCWACVR(,TCWAPTR)                               0799 03654000
*       TCWACVL=TCWAA2L+1;                                         0800 03655000
*                                                                  0800 03656000
         LA    @02,1                                               0800 03657000
         AL    @02,TCWAA2L(,TCWAPTR)                               0800 03658000
         ST    @02,TCWACVL(,TCWAPTR)                               0800 03659000
*/********************************************************************/ 03660000
*/*                                                                  */ 03661000
*/* LOOP THRU TWICE, FOR THE REASONS EXPLAINED ABOVE.                */ 03662000
*/*                                                                  */ 03663000
*/********************************************************************/ 03664000
*                                                                  0801 03665000
*       RESPECIFY                                                  0801 03666000
*        (REG2) RESTRICTED;                                        0801 03667000
*       DO SETLOOP=2 TO 1 BY-1;                                    0802 03668000
         LA    @07,2                                               0802 03669000
         ST    @07,SETLOOP                                         0802 03670000
@DL00802 DS    0H                                                  0803 03671000
*         REG2=PTREBEG+LENGTH(TCENTRY);/* GET PTR TO 2ND TCWA ENTRY  */ 03672000
         LA    REG2,16                                             0803 03673000
         AL    REG2,PTREBEG                                        0803 03674000
*/********************************************************************/ 03675000
*/*                                                                  */ 03676000
*/* BEGINNING WITH THE SECOND ONE, PROCESS ALL TCWA ENTRIES.         */ 03677000
*/* NOTE: THE FIRST ENTRY REPRESENTS THE CLOCK BEING SYNCHRONIZED TO.*/ 03678000
*/*                                                                  */ 03679000
*/********************************************************************/ 03680000
*                                                                  0804 03681000
*         DO I5=TCCPUCNT-1 TO 1 BY-1;                              0804 03682000
*                                                                  0804 03683000
         LH    @07,TCCPUCNT(,TCWAPTR)                              0804 03684000
         BCTR  @07,0                                               0804 03685000
         ST    @07,I5                                              0804 03686000
         B     @DE00804                                            0804 03687000
@DL00804 DS    0H                                                  0805 03688000
*/********************************************************************/ 03689000
*/*                                                                  */ 03690000
*/* TEST TO SEE IF THE CLOCK REPRESENTED BY THIS ENTRY REQUIRES SYN- */ 03691000
*/* CHRONIZING.                                                      */ 03692000
*/*                                                                  */ 03693000
*/********************************************************************/ 03694000
*                                                                  0805 03695000
*           IF REG2->TCWAPCCA->PCCASYNC='1'B THEN                  0805 03696000
*                                                                  0805 03697000
         L     @07,TCWAPCCA(,REG2)                                 0805 03698000
         TM    PCCASYNC(@07),B'01000000'                           0805 03699000
         BNO   @RF00805                                            0805 03700000
*/********************************************************************/ 03701000
*/*                                                                  */ 03702000
*/* IF IT DOES, SET UP THE IPC INTERFACE TO HAVE THE CLOCK SET.      */ 03703000
*/*                                                                  */ 03704000
*/********************************************************************/ 03705000
*                                                                  0806 03706000
*             DO;                                                  0806 03707000
*               REG1=REG2->TCWAPCCA;/* GET PCCA ADDRESS              */ 03708000
         LR    REG1,@07                                            0807 03709000
*               REG12=ADDR(DELTA2); /* GET ROUTINE ADDRESS           */ 03710000
         LA    REG12,DELTA2                                        0808 03711000
*               REG11=ADDR(TCWA);   /* GET PARAMETER ADDRESS         */ 03712000
         LR    REG11,TCWAPTR                                       0809 03713000
*               GENERATE CODE REFS(CVTIPCRI,CVT)                   0810 03714000
*                   (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));     0810 03715000
*                                                                  0810 03716000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        03717000
*/********************************************************************/ 03718000
*/*                                                                  */ 03719000
*/* SEE IF THE CLOCK WAS SUCCESSFULLY SET. (IE. THE OPERATOR HAS NOT */ 03720000
*/* PREMATURELY RELEASED THE TOD CLOCK SECURITY SWITCH.)             */ 03721000
*/*                                                                  */ 03722000
*/********************************************************************/ 03723000
*                                                                  0811 03724000
*               IF TCGNSET='1'B THEN                               0811 03725000
*                                                                  0811 03726000
         TM    TCGNSET(TCWAPTR),B'00001000'                        0811 03727000
         BNO   @RF00811                                            0811 03728000
*/********************************************************************/ 03729000
*/*                                                                  */ 03730000
*/* IF THE SWITCH WAS RELEASED, SET THE LOOP COUNTS OF ALL 3 LOOPS SO*/ 03731000
*/* THAT AN EXIT FROM THEM WILL OCCUR.                               */ 03732000
*/*                                                                  */ 03733000
*/********************************************************************/ 03734000
*                                                                  0812 03735000
*                 DO;                                              0812 03736000
*                   I5=1;                                          0813 03737000
         LA    @07,1                                               0813 03738000
         ST    @07,I5                                              0813 03739000
*                   SETLOOP=1;                                     0814 03740000
         ST    @07,SETLOOP                                         0814 03741000
*                   LOOPCTR=1;                                     0815 03742000
         ST    @07,LOOPCTR                                         0815 03743000
*                 END;                                             0816 03744000
*             END;                                                 0817 03745000
@RF00811 DS    0H                                                  0818 03746000
*           REG2=REG2+LENGTH(TCENTRY);/* POINT TO NEXT TCWA ENTRY    */ 03747000
@RF00805 AL    REG2,@CF00045                                       0818 03748000
*         END;                                                     0819 03749000
         SLR   @07,@07                                             0819 03750000
         BCTR  @07,0                                               0819 03751000
         AL    @07,I5                                              0819 03752000
         ST    @07,I5                                              0819 03753000
@DE00804 LTR   @07,@07                                             0819 03754000
         BP    @DL00804                                            0819 03755000
*       END;                                                       0820 03756000
         SLR   @07,@07                                             0820 03757000
         BCTR  @07,0                                               0820 03758000
         AL    @07,SETLOOP                                         0820 03759000
         ST    @07,SETLOOP                                         0820 03760000
         LTR   @07,@07                                             0820 03761000
         BP    @DL00802                                            0820 03762000
*       RESPECIFY                                                  0821 03763000
*        (REG2) UNRESTRICTED;                                      0821 03764000
*                                                                  0821 03765000
*/********************************************************************/ 03766000
*/*                                                                  */ 03767000
*/* SET UP THE INTERFACE TO IPC SO THAT THE CLOCK BEING SYNCHRONIZED */ 03768000
*/* TO WILL STORE AN ENDING VALUE.  (NOTE: THE ENDING VALUE IS STORED*/ 03769000
*/* IN THE MASTER'S TCWA ENTRY.  IF ITS HIGH ORDER 32 BITS ARE EQUAL */ 03770000
*/* TO THE HIGH ORDER 32 BITS OF THE BEGIN VALUE(TCWAA2), THE LOOP   */ 03771000
*/* HAS BEEN COMPLETED WITHIN 2**20 MICROSECONDS.)                   */ 03772000
*/*                                                                  */ 03773000
*/********************************************************************/ 03774000
*                                                                  0822 03775000
*       REG1=PTREBEG->TCWAPCCA;     /* GET PCCA ADDRESS              */ 03776000
         L     @02,PTREBEG                                         0822 03777000
         L     REG1,TCWAPCCA(,@02)                                 0822 03778000
*       REG12=ADDR(DELTA3);         /* GET ROUTINE ADDRESS           */ 03779000
         LA    REG12,DELTA3                                        0823 03780000
*       REG11=PTREBEG;              /* GET PARAMETER ADDRESS         */ 03781000
         LR    REG11,@02                                           0824 03782000
*       GENERATE CODE REFS(CVTIPCRI,CVT)                           0825 03783000
*           (RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11));             0825 03784000
         RISGNL SERIAL,CPU=(1),EP=(12),PARM=(11)                        03785000
*     END;                                                         0826 03786000
         SLR   @02,@02                                             0826 03787000
         BCTR  @02,0                                               0826 03788000
         AL    @02,LOOPCTR                                         0826 03789000
         ST    @02,LOOPCTR                                         0826 03790000
         LTR   @02,@02                                             0826 03791000
         BP    @DL00794                                            0826 03792000
@DC00794 DS    0H                                                  0827 03793000
*   END;                                                           0827 03794000
*                                                                  0827 03795000
@DE00769 TM    TCGNSET(TCWAPTR),B'00001000'                        0827 03796000
         BO    @DL00769                                            0827 03797000
*/********************************************************************/ 03798000
*/*                                                                  */ 03799000
*/* RETURN TO THE CALLER.                                            */ 03800000
*/*                                                                  */ 03801000
*/********************************************************************/ 03802000
*                                                                  0828 03803000
*   END IEAVRSYN;                                                  0828 03804000
@EL00007 DS    0H                                                  0828 03805000
@EF00007 DS    0H                                                  0828 03806000
@ER00007 LM    @14,@12,12(@13)                                     0828 03807000
         BR    @14                                                 0828 03808000
*                                                                  0829 03809000
*/********************************************************************/ 03810000
*/*                                                                  */ 03811000
*/*             IEAVRNOT--VARY CPU OFFLINE NOTIFICATION              */ 03812000
*/*                                                                  */ 03813000
*/* THIS ROUTINE IS ENTERED FROM IEEVCPU WHENVER A CPU IS BEING      */ 03814000
*/* VARY'ED OFFLINE.  IT CHECKS TO MAKE SURE THAT THE REMOVAL OF THE */ 03815000
*/* SUBJECT CPU WILL NOT CAUSE THE LAST TOD CLOCK, CLOCK COMPARATOR, */ 03816000
*/* OR CPU TIMER TO BE REMOVED FROM THE CONFIGURATION. IF REMOVAL OF */ 03817000
*/* THE CPU WOULD CREATE SUCH A SITUATION, THE RETURN CODE IS SET TO */ 03818000
*/* 4 TO INDICATE THAT THE VARY PROCESS SHOULD BE CANCELLED.  IF NOT,*/ 03819000
*/* THE CPU CAN BE TAKEN OFFLINE.                                    */ 03820000
*/*                                                                  */ 03821000
*/* IF THE CPU BEING TAKEN OFFLINE IS TIMING A REAL TIME TQE, IT     */ 03822000
*/* MUST BE ENSURED THE THE TQE WILL CONTINUE TO BE TIMED ON SOME    */ 03823000
*/* CPU REMAINING ONLINE.                                            */ 03824000
*/*                                                                  */ 03825000
*/********************************************************************/ 03826000
*                                                                  0829 03827000
*IEAVRNOT:                                                         0829 03828000
*   ENTRY(OLDPCCA);                                                0829 03829000
         B     @EC00829                                            0829 03830000
@EP00829 MVC   @PC00001+4(4),0(@01)                                0829 03831000
@EC00829 DS    0H                                                  0830 03832000
*   RESPECIFY                                                      0830 03833000
*     CVTMAP BASED(CVTPTR);                                        0830 03834000
*   R13SAV=REG13;                   /* SAVE THE SAVEAREA ADDRESS     */ 03835000
         ST    REG13,R13SAV                                        0831 03836000
*/********************************************************************/ 03837000
*/*                                                                  */ 03838000
*/* SET UP THE INTERFACE TO FIX THE PAGE CONTAINING THE EXECUTING    */ 03839000
*/* CODE AND ISSUE THE PAGE FIX SVC.                                 */ 03840000
*/*                                                                  */ 03841000
*/********************************************************************/ 03842000
*                                                                  0832 03843000
*   REG1=ADDR(BEGNOT);              /* GET PTR TO START OF CODE      */ 03844000
         LA    REG1,BEGNOT                                         0832 03845000
*   REG15=ADDR(ENDTOD);             /* GET PTR TO END OF MODULE      */ 03846000
         LA    REG15,ENDTOD                                        0833 03847000
*   REG0=ADDR(TODECB);              /* GET ECB ADDRESS               */ 03848000
         LA    REG0,TODECB                                         0834 03849000
*   TODECB=0;                       /* CLEAR ECB                     */ 03850000
         SLR   @02,@02                                             0835 03851000
         ST    @02,TODECB                                          0835 03852000
*   GENERATE CODE(PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N);           0836 03853000
*                                                                  0836 03854000
         PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N                           03855000
*/********************************************************************/ 03856000
*/*                                                                  */ 03857000
*/* WAIT FOR PAGE FIX.                                               */ 03858000
*/*                                                                  */ 03859000
*/********************************************************************/ 03860000
*                                                                  0837 03861000
*   REG1=ADDR(TODECB);              /* GET ECB ADDRESS               */ 03862000
         LA    REG1,TODECB                                         0837 03863000
*   GENERATE CODE(WAIT  1,ECB=(1));                                0838 03864000
*                                                                  0838 03865000
         WAIT  1,ECB=(1)                                                03866000
*/********************************************************************/ 03867000
*/*                                                                  */ 03868000
*/* GET DISPATCHER LOCK TO PROTECT REAL TQE AND FOR INVOKING SETCC   */ 03869000
*/*                                                                  */ 03870000
*/********************************************************************/ 03871000
*                                                                  0839 03872000
*   GENERATE CODE REFS(PSALITA);                                   0839 03873000
*                                                                  0839 03874000
*                            /*                                         03875000
         SETLOCK OBTAIN,TYPE=DISP,MODE=UNCOND,                        **03876000
               RELATED=('REQUIRED BY SETCC',(IEAVRTOD,(NOLABEL))) */    03877000
*/********************************************************************/ 03878000
*/*                                                                  */ 03879000
*/* GET ADDRESSABILITY TO CSD, AND TO PCCA OF CPU GOING OFFLINE.     */ 03880000
*/*                                                                  */ 03881000
*/********************************************************************/ 03882000
*                                                                  0840 03883000
*BEGNOT:                                                           0840 03884000
*   REG12=CVTCSD;                   /* ADDRESSABILITY TO CSD         */ 03885000
BEGNOT   L     @02,CVTPTR                                          0840 03886000
         L     REG12,CVTCSD(,@02)                                  0840 03887000
*   RESPECIFY                                                      0841 03888000
*     CSD BASED(REG12);                                            0841 03889000
*   REG11=ADDR(OLDPCCA);            /* ADDRESSABILITY TO PCCA        */ 03890000
         L     REG11,@PC00001+4                                    0842 03891000
*   RESPECIFY                                                      0843 03892000
*     PCCA BASED(REG11);                                           0843 03893000
*   RETCODE=0;                      /* INITIALIZE RETURN CODE        */ 03894000
         SLR   RETCODE,RETCODE                                     0844 03895000
*/********************************************************************/ 03896000
*/*                                                                  */ 03897000
*/* SEE IF THE TOD CLOCK IN THE CPU GOING OFFLINE IS PERMANENTLY     */ 03898000
*/* DAMAGED.                                                         */ 03899000
*/*                                                                  */ 03900000
*/********************************************************************/ 03901000
*                                                                  0845 03902000
*   IF PCCANFTD='0'B THEN                                          0845 03903000
*                                                                  0845 03904000
         TM    PCCANFTD(REG11),B'01000000'                         0845 03905000
         BNZ   @RF00845                                            0845 03906000
*/********************************************************************/ 03907000
*/*                                                                  */ 03908000
*/* IF NOT, SEE IF IT IS THE LAST GOOD TOD CLOCK IN THE CONFIGURATION*/ 03909000
*/*                                                                  */ 03910000
*/********************************************************************/ 03911000
*                                                                  0846 03912000
*     DO;                                                          0846 03913000
*LASTTODC:                                                         0847 03914000
*       IF CSDGDTOD=1 THEN          /* IF LAST GOOD TOD CLOCK,       */ 03915000
LASTTODC CLC   CSDGDTOD(4,REG12),@CF00096                          0847 03916000
         BNE   @RF00847                                            0847 03917000
*         RETCODE=4;                /* INDICATE TO CANCEL THE VARY   */ 03918000
         LA    RETCODE,4                                           0848 03919000
*       ELSE                                                       0849 03920000
*         DO;                       /* IF NOT LAST GOOD TOD CLOCK,   */ 03921000
         B     @RC00847                                            0849 03922000
@RF00847 DS    0H                                                  0850 03923000
*           CURRENT=CSDGDTOD;       /* GET CURRENT COUNT             */ 03924000
         L     CURRENT,CSDGDTOD(,REG12)                            0850 03925000
*           NEW=CURRENT-1;                                         0851 03926000
         LR    NEW,CURRENT                                         0851 03927000
         BCTR  NEW,0                                               0851 03928000
*           CS(CURRENT,NEW,CSDGDTOD);/* DECREMENT BY 1               */ 03929000
         CS    CURRENT,@01,CSDGDTOD(REG12)                         0852 03930000
*           BC(7,LASTTODC);                                        0853 03931000
         BC    7,LASTTODC                                          0853 03932000
*           PCCATODE='FF'X;         /* INDICATE ERROR COUNT CHANGED  */ 03933000
         MVI   PCCATODE(REG11),X'FF'                               0854 03934000
*         END;                                                     0855 03935000
*     END;                                                         0856 03936000
*                                                                  0856 03937000
@RC00847 DS    0H                                                  0857 03938000
*/********************************************************************/ 03939000
*/*                                                                  */ 03940000
*/* TEST FOR THE VARY PROCESS NOT YET HAVING BEEN CANCELLED.         */ 03941000
*/*                                                                  */ 03942000
*/********************************************************************/ 03943000
*                                                                  0857 03944000
*   IF RETCODE=0 THEN                                              0857 03945000
*                                                                  0857 03946000
@RF00845 LTR   RETCODE,RETCODE                                     0857 03947000
         BNZ   @RF00857                                            0857 03948000
*/********************************************************************/ 03949000
*/*                                                                  */ 03950000
*/* IF NOT YET CANCELLED, CHECK THE CLOCK COMPARATOR FOR PERMANENT   */ 03951000
*/* DAMAGE.                                                          */ 03952000
*/*                                                                  */ 03953000
*/********************************************************************/ 03954000
*                                                                  0858 03955000
*     DO;                                                          0858 03956000
*       IF PCCANFCC='0'B THEN                                      0859 03957000
*                                                                  0859 03958000
         TM    PCCANFCC(REG11),B'01000000'                         0859 03959000
         BNZ   @RF00859                                            0859 03960000
*/********************************************************************/ 03961000
*/*                                                                  */ 03962000
*/* IF O.K., SEE IF IT IS THE LAST GOOD CLOCK COMPARATOR IN THE      */ 03963000
*/* CONFIGURATION.                                                   */ 03964000
*/*                                                                  */ 03965000
*/********************************************************************/ 03966000
*                                                                  0860 03967000
*         DO;                                                      0860 03968000
*LASTCC:                                                           0861 03969000
*           IF CSDGDCC=1 THEN       /* IF LAST GOOD CLOCK COMPARATOR */ 03970000
LASTCC   CLC   CSDGDCC(4,REG12),@CF00096                           0861 03971000
         BNE   @RF00861                                            0861 03972000
*             RETCODE=4;            /* INDICATE TO CANCEL THE VARY   */ 03973000
         LA    RETCODE,4                                           0862 03974000
*           ELSE                                                   0863 03975000
*             DO;                   /* IF NOT LAST GOOD CLOCK      0863 03976000
*                                      COMPARATOR,                   */ 03977000
         B     @RC00861                                            0863 03978000
@RF00861 DS    0H                                                  0864 03979000
*               CURRENT=CSDGDCC;    /* GET CURRENT COUNT             */ 03980000
         L     CURRENT,CSDGDCC(,REG12)                             0864 03981000
*               NEW=CURRENT-1;                                     0865 03982000
         LR    NEW,CURRENT                                         0865 03983000
         BCTR  NEW,0                                               0865 03984000
*               CS(CURRENT,NEW,CSDGDCC);/* DECREMENT BY 1            */ 03985000
         CS    CURRENT,@01,CSDGDCC(REG12)                          0866 03986000
*               BC(7,LASTCC);                                      0867 03987000
         BC    7,LASTCC                                            0867 03988000
*               PCCACCE='FF'X;      /* INDICATE ERROR COUNT CHANGED  */ 03989000
         MVI   PCCACCE(REG11),X'FF'                                0868 03990000
*             END;                                                 0869 03991000
*         END;                                                     0870 03992000
*                                                                  0870 03993000
@RC00861 DS    0H                                                  0871 03994000
*/********************************************************************/ 03995000
*/*                                                                  */ 03996000
*/* TEST FOR VARY PROCESS NOT YET HAVING BEEN CANCELLED.             */ 03997000
*/*                                                                  */ 03998000
*/********************************************************************/ 03999000
*                                                                  0871 04000000
*       IF RETCODE=0 THEN                                          0871 04001000
*                                                                  0871 04002000
@RF00859 LTR   RETCODE,RETCODE                                     0871 04003000
         BNZ   @RF00871                                            0871 04004000
*/********************************************************************/ 04005000
*/*                                                                  */ 04006000
*/* IF NOT YET CANCELLED, CHECK CPU TIMER FOR PERMANENT DAMAGE.      */ 04007000
*/*                                                                  */ 04008000
*/********************************************************************/ 04009000
*                                                                  0872 04010000
*         DO;                                                      0872 04011000
*           IF PCCANFIN='0'B THEN                                  0873 04012000
*                                                                  0873 04013000
         TM    PCCANFIN(REG11),B'01000000'                         0873 04014000
         BNZ   @RF00873                                            0873 04015000
*/********************************************************************/ 04016000
*/*                                                                  */ 04017000
*/* IF O.K., SEE IF IT IS THE LAST CPU TIMER IN THE CONFIGURATION.   */ 04018000
*/*                                                                  */ 04019000
*/********************************************************************/ 04020000
*                                                                  0874 04021000
*             DO;                                                  0874 04022000
*LASTCPUT:                                                         0875 04023000
*               IF CSDGDINT=1 THEN  /* IF LAST GOOD CPU TIMER,       */ 04024000
LASTCPUT CLC   CSDGDINT(4,REG12),@CF00096                          0875 04025000
         BNE   @RF00875                                            0875 04026000
*                 RETCODE=4;        /* INDICATE TO CANCEL THE VARY   */ 04027000
         LA    RETCODE,4                                           0876 04028000
*               ELSE                                               0877 04029000
*                 DO;               /* IF NOT LAST GOOD CPU TIMER,   */ 04030000
         B     @RC00875                                            0877 04031000
@RF00875 DS    0H                                                  0878 04032000
*                   CURRENT=CSDGDINT;/* GET CURRENT COUNT            */ 04033000
         L     CURRENT,CSDGDINT(,REG12)                            0878 04034000
*                   NEW=CURRENT-1;                                 0879 04035000
         LR    NEW,CURRENT                                         0879 04036000
         BCTR  NEW,0                                               0879 04037000
*                   CS(CURRENT,NEW,CSDGDINT);/* DECREMENT BY 1       */ 04038000
         CS    CURRENT,@01,CSDGDINT(REG12)                         0880 04039000
*                   BC(7,LASTCPUT);                                0881 04040000
         BC    7,LASTCPUT                                          0881 04041000
*                   PCCAINTE='FF'X; /* INDICATE ERROR COUNT CHANGED  */ 04042000
         MVI   PCCAINTE(REG11),X'FF'                               0882 04043000
*                 END;                                             0883 04044000
*             END;                                                 0884 04045000
@RC00875 DS    0H                                                  0885 04046000
*         END;                                                     0885 04047000
@RF00873 DS    0H                                                  0886 04048000
*     END;                                                         0886 04049000
*                                                                  0886 04050000
@RF00871 DS    0H                                                  0887 04051000
*/********************************************************************/ 04052000
*/*                                                                  */ 04053000
*/* SAVE THE RETURN CODE ACROSS POTENTIAL DESTRUCTIVE INTERFACES.    */ 04054000
*/* IF THE CPU IS GOING OFFLINE AND IT IS TIMING A REAL TIME TQE,    */ 04055000
*/* ENSURE THAT THE TQE WILL BE TIMED ON SOME OTHER CPU REMAINING    */ 04056000
*/* ONLINE.                                                          */ 04057000
*/*                                                                  */ 04058000
*/********************************************************************/ 04059000
*                                                                  0887 04060000
*   REG3=RETCODE;                   /*                       @YM04766*/ 04061000
@RF00857 LR    REG3,RETCODE                                        0887 04062000
*   IF RETCODE=0&PCCATQEP^=0 THEN                                  0888 04063000
         SLR   @02,@02                                             0888 04064000
         CR    RETCODE,@02                                         0888 04065000
         BNE   @RF00888                                            0888 04066000
         L     @07,PCCATQEP(,REG11)                                0888 04067000
         CR    @07,@02                                             0888 04068000
         BE    @RF00888                                            0888 04069000
*     DO;                                                          0889 04070000
*       PCCATQEP->TQECOMP='0'B;     /* INDICATE TQE NOT BEING TIMED  */ 04071000
         NI    TQECOMP(@07),B'01111111'                            0890 04072000
*       PCCATQEP=0;                 /* INDICATE THIS CPU NOT TIMING A   04073000
*                                      TQE                           */ 04074000
         ST    @02,PCCATQEP(,REG11)                                0891 04075000
*       REG15=TPCCKQ;               /* GET ADRESS OF SETCC           */ 04076000
         L     @02,CVTPTR                                          0892 04077000
         L     @02,TPCPTR(,@02)                                    0892 04078000
         L     REG15,TPCCKQ(,@02)                                  0892 04079000
*       GENERATE CODE REFS(REG15) SETS(REG2)(BALR  REG2,REG15);    0893 04080000
         BALR  REG2,REG15                                               04081000
*     END;                                                         0894 04082000
*                                                                  0894 04083000
*/********************************************************************/ 04084000
*/*                                                                  */ 04085000
*/* RELEASE DISPATCHER LOCK.                                         */ 04086000
*/*                                                                  */ 04087000
*/********************************************************************/ 04088000
*                                                                  0895 04089000
*   GENERATE CODE REFS(PSALITA);                                   0895 04090000
*                                                                  0895 04091000
@RF00888 DS    0H                                                  0895 04092000
*                              /*                                       04093000
         SETLOCK RELEASE,TYPE=DISP,                                   **04094000
               RELATED=('REQUIRED BY SETCC',(IEAVRTOD,(NOLABEL)))  */   04095000
*/********************************************************************/ 04096000
*/*                                                                  */ 04097000
*/* RELEASE FIXED PAGE.                                              */ 04098000
*/*                                                                  */ 04099000
*/********************************************************************/ 04100000
*                                                                  0896 04101000
*   REG1=ADDR(BEGNOT);              /* GET PTR TO BEGINNING OF CODE  */ 04102000
         LA    REG1,BEGNOT                                         0896 04103000
*   REG15=ADDR(ENDTOD);             /* GET PTR TO END OF MODULE      */ 04104000
         LA    REG15,ENDTOD                                        0897 04105000
*   GENERATE CODE(PGFREE R,A=(1),EA=(15));                         0898 04106000
*                                                                  0898 04107000
         PGFREE R,A=(1),EA=(15)                                         04108000
*/********************************************************************/ 04109000
*/*                                                                  */ 04110000
*/* RESTORE THE SAVEAREA ADDRESS, AND RETURN TO IEEVCPU.             */ 04111000
*/*                                                                  */ 04112000
*/********************************************************************/ 04113000
*                                                                  0899 04114000
*   REG13=R13SAV;                                                  0899 04115000
         L     REG13,R13SAV                                        0899 04116000
*   RETURN CODE(REG3);              /*                       @YM04766*/ 04117000
         L     @13,4(,@13)                                         0900 04118000
         L     @00,@SIZDATD                                        0900 04119000
         LR    @01,@08                                             0900 04120000
         FREEMAIN R,LV=(0),A=(1)                                        04121000
         LR    @15,@03                                             0900 04122000
         L     @14,12(,@13)                                        0900 04123000
         LM    @00,@12,20(@13)                                     0900 04124000
         BR    @14                                                 0900 04125000
*                                                                  0901 04126000
*/********************************************************************/ 04127000
*/*                                                                  */ 04128000
*/*             IEAVRCAN--VARY CPU OFFLINE CANCELLATION              */ 04129000
*/*                                                                  */ 04130000
*/* THIS ROUTINE IS ENTERED FROM IEEVCPU WHENEVER A VARY CPU OFFLINE */ 04131000
*/* COMMAND HAS BEEN INTERNALLY CANCELLED, AND ROUTINE IEAVRNOT HAS  */ 04132000
*/* PREVIOUSLY RECEIVED CONTROL.  IT RESETS THE COUNTS IN THE CSD    */ 04133000
*/* AND THE ERROR COUNTS IN THE PCCA FOR ANY TIMING COMPONENTS WHICH */ 04134000
*/* HAD THESE FIELDS CHANGED BY ROUTINE IEAVRNOT.                    */ 04135000
*/*                                                                  */ 04136000
*/********************************************************************/ 04137000
*                                                                  0901 04138000
*IEAVRCAN:                                                         0901 04139000
*   ENTRY(OLDPCCA);                                                0901 04140000
@EP00901 MVC   @PC00001+4(4),0(@01)                                0901 04141000
*   REG12=CVTCSD;                   /* ADDRESSABILITY TO CSD         */ 04142000
         L     @02,CVTPTR                                          0902 04143000
         L     REG12,CVTCSD(,@02)                                  0902 04144000
*   REG11=ADDR(OLDPCCA);            /* ADDRESSABILITY TO PCCA        */ 04145000
         L     REG11,@PC00001+4                                    0903 04146000
*/********************************************************************/ 04147000
*/*                                                                  */ 04148000
*/* SEE IF THE TOD CLOCK ERROR COUNT WAS CHANGED BY IEAVRNOT.        */ 04149000
*/*                                                                  */ 04150000
*/********************************************************************/ 04151000
*                                                                  0904 04152000
*   IF PCCATODE='FF'X THEN                                         0904 04153000
         CLI   PCCATODE(REG11),255                                 0904 04154000
         BNE   @RF00904                                            0904 04155000
*     DO;                           /* IF SO,                        */ 04156000
*       PCCATODE='00'X;             /* RESET ERROR COUNT             */ 04157000
         MVI   PCCATODE(REG11),X'00'                               0906 04158000
*CURRTODC:                                                         0907 04159000
*       CURRENT=CSDGDTOD;           /* GET COUNT OF GOOD TOD CLOCKS  */ 04160000
CURRTODC L     CURRENT,CSDGDTOD(,REG12)                            0907 04161000
*       NEW=CURRENT+1;                                             0908 04162000
         LA    NEW,1                                               0908 04163000
         ALR   NEW,CURRENT                                         0908 04164000
*       CS(CURRENT,NEW,CSDGDTOD);   /* INCREMENT COUNT BY 1          */ 04165000
         CS    CURRENT,@01,CSDGDTOD(REG12)                         0909 04166000
*       BC(7,CURRTODC);                                            0910 04167000
         BC    7,CURRTODC                                          0910 04168000
*     END;                                                         0911 04169000
*                                                                  0911 04170000
*/********************************************************************/ 04171000
*/*                                                                  */ 04172000
*/* SEE IF THE CLOCK COMPARATOR ERROR COUNT WAS CHANGED BY IEAVRNOT. */ 04173000
*/*                                                                  */ 04174000
*/********************************************************************/ 04175000
*                                                                  0912 04176000
*   IF PCCACCE='FF'X THEN                                          0912 04177000
@RF00904 CLI   PCCACCE(REG11),255                                  0912 04178000
         BNE   @RF00912                                            0912 04179000
*     DO;                           /* IF SO,                        */ 04180000
*       PCCACCE='00'X;              /* RESET ERROR COUNT             */ 04181000
         MVI   PCCACCE(REG11),X'00'                                0914 04182000
*CURRCC:                                                           0915 04183000
*       CURRENT=CSDGDCC;            /* GET COUNT OF GOOD CLOCK     0915 04184000
*                                      COMPARATORS                   */ 04185000
CURRCC   L     CURRENT,CSDGDCC(,REG12)                             0915 04186000
*       NEW=CURRENT+1;                                             0916 04187000
         LA    NEW,1                                               0916 04188000
         ALR   NEW,CURRENT                                         0916 04189000
*       CS(CURRENT,NEW,CSDGDCC);    /* INCREMENT COUNT BY 1          */ 04190000
         CS    CURRENT,@01,CSDGDCC(REG12)                          0917 04191000
*       BC(7,CURRCC);                                              0918 04192000
         BC    7,CURRCC                                            0918 04193000
*     END;                                                         0919 04194000
*                                                                  0919 04195000
*/********************************************************************/ 04196000
*/*                                                                  */ 04197000
*/* SEE IF THE CPU TIMER ERROR COUNT WAS CHANGED BY IEAVRNOT.        */ 04198000
*/*                                                                  */ 04199000
*/********************************************************************/ 04200000
*                                                                  0920 04201000
*   IF PCCAINTE='FF'X THEN                                         0920 04202000
@RF00912 CLI   PCCAINTE(REG11),255                                 0920 04203000
         BNE   @RF00920                                            0920 04204000
*     DO;                                                          0921 04205000
*       PCCAINTE='00'X;             /* RESET ERROR COUNT             */ 04206000
         MVI   PCCAINTE(REG11),X'00'                               0922 04207000
*CURRCPUT:                                                         0923 04208000
*       CURRENT=CSDGDINT;           /* GET COUNT OF GOOD CPU TIMERS  */ 04209000
CURRCPUT L     CURRENT,CSDGDINT(,REG12)                            0923 04210000
*       NEW=CURRENT+1;                                             0924 04211000
         LA    NEW,1                                               0924 04212000
         ALR   NEW,CURRENT                                         0924 04213000
*       CS(CURRENT,NEW,CSDGDINT);   /* INCREMENT COUNT BY 1          */ 04214000
         CS    CURRENT,@01,CSDGDINT(REG12)                         0925 04215000
*       BC(7,CURRCPUT);                                            0926 04216000
         BC    7,CURRCPUT                                          0926 04217000
*     END;                                                         0927 04218000
*   RETURN;                                                        0928 04219000
@EL00001 L     @13,4(,@13)                                         0928 04220000
@EF00001 L     @00,@SIZDATD                                        0928 04221000
         LR    @01,@08                                             0928 04222000
         FREEMAIN R,LV=(0),A=(1)                                        04223000
@ER00001 LM    @14,@12,12(@13)                                     0928 04224000
         BR    @14                                                 0928 04225000
*   RESPECIFY                                                      0929 04226000
*     CSD BASED(CVTCSD);                                           0929 04227000
*                                                                  0930 04228000
*/********************************************************************/ 04229000
*/*                                                                  */ 04230000
*/*          IEAVRCLA--TIMING COMPONENT HARDWARE RECOVERY            */ 04231000
*/*                                                                  */ 04232000
*/* THIS ROUTINE IS SCHEDULED INTO THE MASTER MEMORY BY AN SRB THAT  */ 04233000
*/* IS SCHEDULED BY ROUTINE IEAVRCLS, AND IS SUBSEQUENTLY GIVEN      */ 04234000
*/* CONTROL BY THE DISPATCHER.                                       */ 04235000
*/*                                                                  */ 04236000
*/* AFTER SAVING THE RETURN ADDRESS, IT BEGINS A SCAN OF THE PCCA    */ 04237000
*/* VECTOR TABLE SEARCHING FOR NON-ZERO ENTRIES.  WHEN ONE IS FOUND, */ 04238000
*/* IT DETERMINES IF ANY OF THE 3 TIMING COMPONENTS FOR THE CPU REP- */ 04239000
*/* RESENTED BY THAT PCCA REQUIRES RECOVERY, AND IF SO, TAKES APPRO- */ 04240000
*/* PRIATE RECOVERY MEASURES.                                        */ 04241000
*/*                                                                  */ 04242000
*/* WHEN ALL PCCAVT ENTRIES HAVE BEEN SCANNED, IT GOES TO THE SET    */ 04243000
*/* CLOCK COMPARATOR ROUTINE TO ENSURE THAT THE TOP REAL TIME TQE IS */ 04244000
*/* BEING TIMED.  THEN, USING COMPARE AND SWAP ON THE PARAMETER      */ 04245000
*/* FIELD IN THE SRB THAT WAS USED TO SCHEDULE IT, A TEST IS MADE TO */ 04246000
*/* DETERMINE IF FURTHER ERRORS HAVE OCCURRED WHILE IT WAS PROCESS-  */ 04247000
*/* ING.  IF THEY HAVE, IEAVRCLA BEGINS A NEW SCAN OF THE PCCAVT.    */ 04248000
*/* IF NO NEW ERRORS OCCURRED, IT RETURNS TO THE DISPATCHER.         */ 04249000
*/*                                                                  */ 04250000
*/********************************************************************/ 04251000
*                                                                  0930 04252000
*IEAVRCLA:                                                         0930 04253000
*   RESPECIFY                                                      0930 04254000
*    (PCCAPTR,                                                     0930 04255000
*     SLOTCNT,                                                     0930 04256000
*     REG2,                                                        0930 04257000
*     REG9) RESTRICTED;                                            0930 04258000
IEAVRCLA DS    0H                                                  0931 04259000
*   GENERATE CODE REFS(REG8,REG10) SETS(REG9);                     0931 04260000
*                                                                  0931 04261000
         DROP  REG9                 DROP FIRST CODEREG                  04262000
         BALR  REG9,0               ESTABLISH ADDRESSABILITY            04263000
         USING *,REG9                                                   04264000
         DROP  REG8                 DROP DATAREG                        04265000
         DROP  REG10                DROP SECOND CODEREG                 04266000
*/********************************************************************/ 04267000
*/*                                                                  */ 04268000
*/* SET UP THE INTERFACE TO GET THE PAGE CONTAINING IEAVRCLA FIXED.  */ 04269000
*/*                                                                  */ 04270000
*/********************************************************************/ 04271000
*                                                                  0932 04272000
*CLABSLBL:                                                         0932 04273000
*   REG1=ADDR(SETTC);               /* GET POINTER TO START OF     0932 04274000
*                                      IEAVRCLA                      */ 04275000
CLABSLBL LA    REG1,SETTC                                          0932 04276000
*   REG15=ADDR(ENDTOD);             /* GET POINTER TO END OF IEAVRTOD*/ 04277000
         LA    REG15,ENDTOD                                        0933 04278000
*   REG0=ADDR(CLAECB);              /* GET ADDRESS OF ECB            */ 04279000
         LA    REG0,CLAECB                                         0934 04280000
*   CLAECB=0;                       /* INITIALIZE ECB                */ 04281000
         SLR   @07,@07                                             0935 04282000
         ST    @07,CLAECB                                          0935 04283000
*/********************************************************************/ 04284000
*/*                                                                  */ 04285000
*/* ISSUE THE PAGE FIX SVC.                                          */ 04286000
*/*                                                                  */ 04287000
*/********************************************************************/ 04288000
*                                                                  0936 04289000
*   GENERATE CODE(PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N);           0936 04290000
*                                                                  0936 04291000
         PGFIX R,A=(1),EA=(15),ECB=(0),LONG=N                           04292000
*/********************************************************************/ 04293000
*/*                                                                  */ 04294000
*/* WAIT ON PAGE FIX.                                                */ 04295000
*/*                                                                  */ 04296000
*/********************************************************************/ 04297000
*                                                                  0937 04298000
*   GENERATE CODE SETS(CLAECB)(WAIT  1,ECB=CLAECB);                0937 04299000
*                                                                  0937 04300000
         WAIT  1,ECB=CLAECB                                             04301000
*/********************************************************************/ 04302000
*/*                                                                  */ 04303000
*/* ESTABLISH ERROR EXIT FOR UNLOCKED CODE.                          */ 04304000
*/*                                                                  */ 04305000
*/********************************************************************/ 04306000
*                                                                  0938 04307000
*   REG2=ADDR(CLAESTAE);            /* GET EXIT ADDRESS              */ 04308000
         LA    REG2,CLAESTAE                                       0938 04309000
*   GEN CODE REFS(REG2,REG3)(ESTAE (2),CT,PURGE=NONE,RECORD=YES);  0939 04310000
*                                                                  0939 04311000
         ESTAE (2),CT,PURGE=NONE,RECORD=YES                             04312000
*/********************************************************************/ 04313000
*/*                                                                  */ 04314000
*/* RE-BASE THE CVT AND CSD FOR MORE EFFICIENT CODE GENERATION.      */ 04315000
*/*                                                                  */ 04316000
*/********************************************************************/ 04317000
*                                                                  0940 04318000
*   CLACVTP=CVTPTR;                 /* GET POINTER TO CVT            */ 04319000
         L     CLACVTP,CVTPTR                                      0940 04320000
*   RESPECIFY                                                      0941 04321000
*     CVTMAP BASED(CLACVTP);                                       0941 04322000
*   REG3=CVTCSD;                    /* GET POINTER TO CSD            */ 04323000
         L     REG3,CVTCSD(,CLACVTP)                               0942 04324000
*   RESPECIFY                                                      0943 04325000
*     CSD BASED(REG3);                                             0943 04326000
*                                                                  0943 04327000
*/********************************************************************/ 04328000
*/*                                                                  */ 04329000
*/* INDICATE THAT THE SEARCH CYCLE HAS BEGUN                         */ 04330000
*/*                                                                  */ 04331000
*/********************************************************************/ 04332000
*                                                                  0944 04333000
*SETTC:                                                            0944 04334000
*   SRBPTR=ADDR(TPCRSRB);           /* GET ADDRESSABILITY TO RECOVERY   04335000
*                                      SRB                           */ 04336000
SETTC    L     SRBPTR,TPCPTR(,CLACVTP)                             0944 04337000
         LA    SRBPTR,TPCRSRB(,SRBPTR)                             0944 04338000
*   CLATC='0'B;                                                    0945 04339000
         NI    CLATC(SRBPTR),B'11111110'                           0945 04340000
*   CLABND='0'B;                    /* INITIALIZE ABEND-REAL-TQE'S 0946 04341000
*                                      INDICATOR                     */ 04342000
         NI    CLABND,B'01111111'                                  0946 04343000
*/********************************************************************/ 04344000
*/*                                                                  */ 04345000
*/* SEARCH THE TIMER STATUS BYTES OF ALL PHYSICAL CCA'S LOOKING FOR  */ 04346000
*/* THOSE WHICH INDICATE THAT AN ERROR HAS OCCURRED FOR WHICH RECOV- */ 04347000
*/* ERY HAS NOT YET BEEN ATTEMPTED.                                  */ 04348000
*/*                                                                  */ 04349000
*/********************************************************************/ 04350000
*                                                                  0947 04351000
*   REG2=CVTPCCAT;                  /* GET PTR TO PCCAVT             */ 04352000
         L     REG2,CVTPCCAT(,CLACVTP)                             0947 04353000
*   RESPECIFY                                                      0948 04354000
*     PTRPCCA BASED(REG2);                                         0948 04355000
*   DO SLOTCNT=DIM(PCCAT00P) TO 1 BY-1;                            0949 04356000
*                                                                  0949 04357000
         LA    SLOTCNT,16                                          0949 04358000
@DL00949 DS    0H                                                  0950 04359000
*/********************************************************************/ 04360000
*/*                                                                  */ 04361000
*/* SEE IF THE NEXT PCCA VECTOR TABLE SLOT IS NON-ZERO, INDICATING   */ 04362000
*/* THAT THE PCCA EXISTS.                                            */ 04363000
*/*                                                                  */ 04364000
*/********************************************************************/ 04365000
*                                                                  0950 04366000
*     IF PTRPCCA^=0 THEN                                           0950 04367000
         L     @07,PTRPCCA(,REG2)                                  0950 04368000
         LTR   @07,@07                                             0950 04369000
         BZ    @RF00950                                            0950 04370000
*       DO;                                                        0951 04371000
*                                                                  0951 04372000
*/********************************************************************/ 04373000
*/*                                                                  */ 04374000
*/* IF SO, ESTABLISH ADDRESSABILTIY TO THE PCCA.                     */ 04375000
*/*                                                                  */ 04376000
*/********************************************************************/ 04377000
*                                                                  0952 04378000
*         PCCAPTR=PTRPCCA;                                         0952 04379000
         LR    PCCAPTR,@07                                         0952 04380000
*         RESPECIFY                                                0953 04381000
*           PCCA BASED(PCCAPTR);                                   0953 04382000
*                                                                  0953 04383000
*/********************************************************************/ 04384000
*/*                                                                  */ 04385000
*/* CHECK THE TOD CLOCK STATUS BITS TO DETERMINE IF THE CLOCK RE-    */ 04386000
*/* QUIRES RECOVERY.                                                 */ 04387000
*/*                                                                  */ 04388000
*/********************************************************************/ 04389000
*                                                                  0954 04390000
*         IF PCCANUTD='1'B&PCCANFTD='0'B THEN                      0954 04391000
*                                                                  0954 04392000
         TM    PCCANUTD(PCCAPTR),B'10000000'                       0954 04393000
         BNO   @RF00954                                            0954 04394000
         TM    PCCANFTD(PCCAPTR),B'01000000'                       0954 04395000
         BNZ   @RF00954                                            0954 04396000
*/********************************************************************/ 04397000
*/*                                                                  */ 04398000
*/* IF SO, INDICATE THAT NO FURTHER ATTEMPTS SHOULD BE MADE TO FIX   */ 04399000
*/* THE CLOCK AND SUBTRACT ONE FROM THE NUMBER OF GOOD TOD CLOCKS    */ 04400000
*/* IN THE CONFIGURATION.                                            */ 04401000
*/*                                                                  */ 04402000
*/********************************************************************/ 04403000
*                                                                  0955 04404000
*           DO;                                                    0955 04405000
*             PCCANFTD='1'B;        /* INDICATE NOT TO FIX THE CLOCK */ 04406000
         OI    PCCANFTD(PCCAPTR),B'01000000'                       0956 04407000
*             CURRENT=CSDGDTOD;                                    0957 04408000
         L     CURRENT,CSDGDTOD(,REG3)                             0957 04409000
*SUB2:                                                             0958 04410000
*             NEW=CURRENT-1;                                       0958 04411000
SUB2     LR    NEW,CURRENT                                         0958 04412000
         BCTR  NEW,0                                               0958 04413000
*             CS(CURRENT,NEW,CSDGDTOD);                            0959 04414000
         CS    CURRENT,@01,CSDGDTOD(REG3)                          0959 04415000
*             BC(7,SUB2);                                          0960 04416000
*                                                                  0960 04417000
         BC    7,SUB2                                              0960 04418000
*/********************************************************************/ 04419000
*/*                                                                  */ 04420000
*/* SET REGISTER 13 TO THE ADDRESS OF                                */ 04421000
*/* THE SAVEAREA, REGISTER 1 TO THE ADDRESS OF THE OF A FULL WORD    */ 04422000
*/* CONTAINING A POINTER TO THE PCCA, AND REGISTER 0 TO 1 TO INDI-   */ 04423000
*/* CATE THAT THE CALLER IS ASYNCHRONOUS RECOVERY.  THEN CALL THE    */ 04424000
*/* ROUTINE TO RESET OR RE-SYNCHRONIZE THE CLOCK.                    */ 04425000
*/*                                                                  */ 04426000
*/********************************************************************/ 04427000
*                                                                  0961 04428000
*             REG13=ADDR(CLASAV);                                  0961 04429000
         LA    REG13,CLASAV                                        0961 04430000
*             REG1=REG2;                                           0962 04431000
         LR    REG1,REG2                                           0962 04432000
*             REG0=1;                                              0963 04433000
         LA    REG0,1                                              0963 04434000
*             CALL IEAVRSSC;                                       0964 04435000
*                                                                  0964 04436000
         L     @15,@CA02733                                        0964 04437000
         BALR  @14,@15                                             0964 04438000
*/********************************************************************/ 04439000
*/*                                                                  */ 04440000
*/* SEE IF THE CLOCK WAS SUCCESSFULLY RECOVERED.                     */ 04441000
*/*                                                                  */ 04442000
*/********************************************************************/ 04443000
*                                                                  0965 04444000
*             IF RETCODE=0 THEN                                    0965 04445000
*                                                                  0965 04446000
         LTR   RETCODE,RETCODE                                     0965 04447000
         BNZ   @RF00965                                            0965 04448000
*/********************************************************************/ 04449000
*/*                                                                  */ 04450000
*/* IF IT WAS RECOVERED, INDICATE THAT IT IS NOW ALL RIGHT TO ATTEMPT*/ 04451000
*/* TO RECOVER THE CLOCK IF IT FAILS AGAIN, AND ADD ONE TO THE COUNT */ 04452000
*/* OF ERRORS FOR THIS CLOCK.                                        */ 04453000
*/*                                                                  */ 04454000
*/********************************************************************/ 04455000
*                                                                  0966 04456000
*               DO;                                                0966 04457000
*                 PCCANFTD='0'B;    /* INDICATE CLOCK CAN BE FIXED   */ 04458000
         NI    PCCANFTD(PCCAPTR),B'10111111'                       0967 04459000
*                 PCCATODE=PCCATODE+ERRINCR;/* BUMP TOD CLOCK ERR  0968 04460000
*                                      COUNT                         */ 04461000
         LA    @07,32                                              0968 04462000
         SLR   @06,@06                                             0968 04463000
         IC    @06,PCCATODE(,PCCAPTR)                              0968 04464000
         ALR   @07,@06                                             0968 04465000
         STC   @07,PCCATODE(,PCCAPTR)                              0968 04466000
*               END;                                               0969 04467000
*             CLACVTP=CVTPTR;       /* RESTORE BASE FOR CVT          */ 04468000
@RF00965 L     CLACVTP,CVTPTR                                      0970 04469000
*/********************************************************************/ 04470000
*/*                                                                  */ 04471000
*/* SEE IF THE NO-FIX BIT IS ON. (NOTE: THIS BIT MAY HAVE BEEN TURNED*/ 04472000
*/* BACK ON AS A RESULT OF BUMPING THE ERROR COUNT.)                 */ 04473000
*/*                                                                  */ 04474000
*/********************************************************************/ 04475000
*                                                                  0971 04476000
*             IF PCCANFTD='1'B THEN                                0971 04477000
*                                                                  0971 04478000
         TM    PCCANFTD(PCCAPTR),B'01000000'                       0971 04479000
         BNO   @RF00971                                            0971 04480000
*/********************************************************************/ 04481000
*/*                                                                  */ 04482000
*/* IF IT IS ON, SEE IF THE COMPARABLE BIT FOR THE CLOCK COMPARATOR  */ 04483000
*/* IS OFF.  (NOTE: IF THE CLOCK WAS NOT RECOVERED, OR THIS WAS THE  */ 04484000
*/* NTH FAILURE ON IT, THE CLOCK IS CONSIDERED TO BE PERMANENTLY     */ 04485000
*/* DAMAGED, AND WILL NOT BE USED ANY MORE.  IF IT CANNOT BE USED,   */ 04486000
*/* THE CLOCK COMPARATOR MUST ALSO NOT BE USED, AND THE COUNT OF     */ 04487000
*/* GOOD CLOCK COMPARATORS IN THE CONFIGURATION MUST BE DECREMENTED, */ 04488000
*/* UNLESS THE CLOCK COMPARATOR WAS ALREADY PERMANENTLY DAMAGED.)    */ 04489000
*/*                                                                  */ 04490000
*/********************************************************************/ 04491000
*                                                                  0972 04492000
*               DO;                                                0972 04493000
*                 IF PCCANFCC='0'B THEN                            0973 04494000
*                                                                  0973 04495000
         TM    PCCANFCC(PCCAPTR),B'01000000'                       0973 04496000
         BNZ   @RF00973                                            0973 04497000
*/********************************************************************/ 04498000
*/*                                                                  */ 04499000
*/* INDICATE THAT THE CLOCK COMPARATOR SHOULD NOT BE RECOVERED, AND  */ 04500000
*/* DECREMENT THE COUNT OF GOOD CLOCK COMPARATORS.                   */ 04501000
*/*                                                                  */ 04502000
*/********************************************************************/ 04503000
*                                                                  0974 04504000
*                   DO;                                            0974 04505000
*                     PCCANFCC='1'B;                               0975 04506000
         OI    PCCANFCC(PCCAPTR),B'01000000'                       0975 04507000
*                     CURRENT=CSDGDCC;                             0976 04508000
         L     CURRENT,CSDGDCC(,REG3)                              0976 04509000
*SUB3:                                                             0977 04510000
*                     NEW=CURRENT-1;                               0977 04511000
SUB3     LR    NEW,CURRENT                                         0977 04512000
         BCTR  NEW,0                                               0977 04513000
*                     CS(CURRENT,NEW,CSDGDCC);                     0978 04514000
         CS    CURRENT,@01,CSDGDCC(REG3)                           0978 04515000
*                     BC(7,SUB3);                                  0979 04516000
         BC    7,SUB3                                              0979 04517000
*                   END;                                           0980 04518000
*                                                                  0980 04519000
*/********************************************************************/ 04520000
*/*                                                                  */ 04521000
*/* ASSEMBLE THE ERROR MSG, AND TELL THE OPERATOR THAT THE TOD CLOCK */ 04522000
*/* IS PERMANENTLY DAMAGED.                                          */ 04523000
*/*                                                                  */ 04524000
*/********************************************************************/ 04525000
*                                                                  0981 04526000
*                 CVD(PCCACPUA,CVRTAREA);                          0981 04527000
@RF00973 LH    @07,PCCACPUA(,PCCAPTR)                              0981 04528000
         CVD   @07,CVRTAREA                                        0981 04529000
*                 WPLPTR=ADDR(EMSGTOD);                            0982 04530000
         LA    WPLPTR,EMSGTOD                                      0982 04531000
*                 UNPK(CPUADDRE,CVRTAREA(7:8));                    0983 04532000
         UNPK  CPUADDRE(2,WPLPTR),CVRTAREA+6(2)                    0983 04533000
*                 CPUADDRE(2)=CPUADDRE(2) 'F0'X;                   0984 04534000
         OI    CPUADDRE+1(WPLPTR),X'F0'                            0984 04535000
*                 GENERATE CODE(WTO MF=(E,(1)));                   0985 04536000
         WTO MF=(E,(1))                                                 04537000
*               END;                                               0986 04538000
*             ELSE                                                 0987 04539000
*                                                                  0987 04540000
*/********************************************************************/ 04541000
*/*                                                                  */ 04542000
*/* IF THE NO-FIX BIT IS STILL OFF, INDICATING THAT THE CLOCK IS     */ 04543000
*/* USEABLE, TURN OFF THE NO-USE BIT TO INDICATE SO, AND INCREMENT   */ 04544000
*/* THE COUNT OF GOOD TOD CLOCKS.                                    */ 04545000
*/*                                                                  */ 04546000
*/********************************************************************/ 04547000
*                                                                  0987 04548000
*               DO;                                                0987 04549000
         B     @RC00971                                            0987 04550000
@RF00971 DS    0H                                                  0988 04551000
*                 PCCANUTD='0'B;                                   0988 04552000
         NI    PCCANUTD(PCCAPTR),B'01111111'                       0988 04553000
*                 CURRENT=CSDGDTOD;                                0989 04554000
         L     CURRENT,CSDGDTOD(,REG3)                             0989 04555000
*ADD6:                                                             0990 04556000
*                 NEW=CURRENT+1;                                   0990 04557000
ADD6     LA    NEW,1                                               0990 04558000
         ALR   NEW,CURRENT                                         0990 04559000
*                 CS(CURRENT,NEW,CSDGDTOD);                        0991 04560000
         CS    CURRENT,@01,CSDGDTOD(REG3)                          0991 04561000
*                 BC(7,ADD6);                                      0992 04562000
*                                                                  0992 04563000
         BC    7,ADD6                                              0992 04564000
*/********************************************************************/ 04565000
*/*                                                                  */ 04566000
*/* NOW SEE IF THE CLOCK COMPARATOR WAS PERMANENTLY DAMAGED.         */ 04567000
*/*                                                                  */ 04568000
*/********************************************************************/ 04569000
*                                                                  0993 04570000
*                 IF PCCANFCC='0'B THEN                            0993 04571000
*                                                                  0993 04572000
         TM    PCCANFCC(PCCAPTR),B'01000000'                       0993 04573000
         BNZ   @RF00993                                            0993 04574000
*/********************************************************************/ 04575000
*/*                                                                  */ 04576000
*/* IF NOT, INDICATE THAT THE CLOCK COMPARATOR IS NOW USEABLE, AND   */ 04577000
*/* THAT TASKS WITH REAL TQE'S NO LONGER NEED BE ABEND'ED SINCE THERE*/ 04578000
*/* IS NOW A GOOD TOD CLOCK AND CLOCK COMPARATOR IN THE SYSTEM.      */ 04579000
*/* (NOTE: THE CLOCK COMPARATOR WAS MARKED UNUSEABLE BY IEAVRCLS     */ 04580000
*/* WHEN THE TOD CLOCK FAILED.)                                      */ 04581000
*/*                                                                  */ 04582000
*/********************************************************************/ 04583000
*                                                                  0994 04584000
*                   DO;                                            0994 04585000
*                     PCCANUCC='0'B;                               0995 04586000
         NI    PCCANUCC(PCCAPTR),B'01111111'                       0995 04587000
*                     CLABND='1'B;                                 0996 04588000
         OI    CLABND,B'10000000'                                  0996 04589000
*                   END;                                           0997 04590000
*               END;                                               0998 04591000
@RF00993 DS    0H                                                  0999 04592000
*           END;                                                   0999 04593000
@RC00971 DS    0H                                                  1000 04594000
*                                                                  1000 04595000
*/********************************************************************/ 04596000
*/*                                                                  */ 04597000
*/* NOW CHECK THE CLOCK COMPARATOR STATUS BITS TO DETERMINE IF THE   */ 04598000
*/* CLOCK COMPARATOR REQUIRES RECOVERY.                              */ 04599000
*/*                                                                  */ 04600000
*/********************************************************************/ 04601000
*                                                                  1000 04602000
*         IF PCCANUCC='1'B&PCCANFCC='0'B THEN                      1000 04603000
*                                                                  1000 04604000
@RF00954 TM    PCCANUCC(PCCAPTR),B'10000000'                       1000 04605000
         BNO   @RF01000                                            1000 04606000
         TM    PCCANFCC(PCCAPTR),B'01000000'                       1000 04607000
         BNZ   @RF01000                                            1000 04608000
*/********************************************************************/ 04609000
*/*                                                                  */ 04610000
*/* IF S0, TEST TO SEE IF SYNCHRONOUS RECOVERY HAS BEEN UNABLE TO    */ 04611000
*/* RECOVER THE ERROR.                                               */ 04612000
*/*                                                                  */ 04613000
*/********************************************************************/ 04614000
*                                                                  1001 04615000
*           DO;                                                    1001 04616000
*             IF PCCAMCC='1'B THEN                                 1002 04617000
*                                                                  1002 04618000
         TM    PCCAMCC(PCCAPTR),B'00010000'                        1002 04619000
         BNO   @RF01002                                            1002 04620000
*/********************************************************************/ 04621000
*/*                                                                  */ 04622000
*/* IF SO, INDICATE THAT NO FURTHER ATTEMPTS SHOULD BE MADE TO RE-   */ 04623000
*/* COVER THE CLOCK COMPARATOR.  THEN, DECREMENT THE COUNT OF GOOD   */ 04624000
*/* CLOCK COMPARATORS BY 1.                                          */ 04625000
*/*                                                                  */ 04626000
*/********************************************************************/ 04627000
*                                                                  1003 04628000
*               DO;                                                1003 04629000
*                 PCCANFCC='1'B;                                   1004 04630000
         OI    PCCANFCC(PCCAPTR),B'01000000'                       1004 04631000
*                 CURRENT=CSDGDCC;                                 1005 04632000
         L     CURRENT,CSDGDCC(,REG3)                              1005 04633000
*SUB4:                                                             1006 04634000
*                 NEW=CURRENT-1;                                   1006 04635000
SUB4     LR    NEW,CURRENT                                         1006 04636000
         BCTR  NEW,0                                               1006 04637000
*                 CS(CURRENT,NEW,CSDGDCC);                         1007 04638000
         CS    CURRENT,@01,CSDGDCC(REG3)                           1007 04639000
*                 BC(7,SUB4);                                      1008 04640000
         BC    7,SUB4                                              1008 04641000
*                 PCCAMCC='0'B;                                    1009 04642000
*                                                                  1009 04643000
         NI    PCCAMCC(PCCAPTR),B'11101111'                        1009 04644000
*/********************************************************************/ 04645000
*/*                                                                  */ 04646000
*/* ASSEMBLE THE ERROR MSG, AND TELL THE OPERATOR THAT THE CLOCK     */ 04647000
*/* COMPARATOR IS PERMANENTLY DAMAGED.                               */ 04648000
*/*                                                                  */ 04649000
*/********************************************************************/ 04650000
*                                                                  1010 04651000
*                 CVD(PCCACPUA,CVRTAREA);                          1010 04652000
         LH    @07,PCCACPUA(,PCCAPTR)                              1010 04653000
         CVD   @07,CVRTAREA                                        1010 04654000
*                 WPLPTR=ADDR(EMSGCC);                             1011 04655000
         LA    WPLPTR,EMSGCC                                       1011 04656000
*                 UNPK(CPUADDRE,CVRTAREA(7:8));                    1012 04657000
         UNPK  CPUADDRE(2,WPLPTR),CVRTAREA+6(2)                    1012 04658000
*                 CPUADDRE(2)=CPUADDRE(2) 'F0'X;                   1013 04659000
         OI    CPUADDRE+1(WPLPTR),X'F0'                            1013 04660000
*                 GENERATE CODE(WTO   MF=(E,(1)));                 1014 04661000
         WTO   MF=(E,(1))                                               04662000
*               END;                                               1015 04663000
*           END;                                                   1016 04664000
@RF01002 DS    0H                                                  1017 04665000
*                                                                  1017 04666000
*/********************************************************************/ 04667000
*/*                                                                  */ 04668000
*/* NOW CHECK THE CPU TIMER STATUS BITS TO DETERMINE IF THE CPU TIMER*/ 04669000
*/* REQUIRES RECOVERY.                                               */ 04670000
*/*                                                                  */ 04671000
*/********************************************************************/ 04672000
*                                                                  1017 04673000
*         IF PCCANUIN='1'B&PCCANFIN='0'B THEN                      1017 04674000
*                                                                  1017 04675000
@RF01000 TM    PCCANUIN(PCCAPTR),B'10000000'                       1017 04676000
         BNO   @RF01017                                            1017 04677000
         TM    PCCANFIN(PCCAPTR),B'01000000'                       1017 04678000
         BNZ   @RF01017                                            1017 04679000
*/********************************************************************/ 04680000
*/*                                                                  */ 04681000
*/* IF SO, TEST TO SEE IF SYNCHRONOUS RECOVERY HAS BEEN UNABLE TO    */ 04682000
*/* RECOVER THE ERROR.                                               */ 04683000
*/*                                                                  */ 04684000
*/********************************************************************/ 04685000
*                                                                  1018 04686000
*           DO;                                                    1018 04687000
*             IF PCCAMINT='1'B THEN                                1019 04688000
*                                                                  1019 04689000
         TM    PCCAMINT(PCCAPTR),B'00001000'                       1019 04690000
         BNO   @RF01019                                            1019 04691000
*/********************************************************************/ 04692000
*/*                                                                  */ 04693000
*/* IF SO, INDICATE THAT NO ATTEMPTS SHOULD BE MADE TO RECOVER THE   */ 04694000
*/* CPU TIMER.  THEN, DECREMENT THE COUNT OF GOOD CPU TIMERS BY 1.   */ 04695000
*/*                                                                  */ 04696000
*/********************************************************************/ 04697000
*                                                                  1020 04698000
*               DO;                                                1020 04699000
*                 PCCANFIN='1'B;                                   1021 04700000
         OI    PCCANFIN(PCCAPTR),B'01000000'                       1021 04701000
*                 CURRENT=CSDGDINT;                                1022 04702000
         L     CURRENT,CSDGDINT(,REG3)                             1022 04703000
*SUB5:                                                             1023 04704000
*                 NEW=CURRENT-1;                                   1023 04705000
SUB5     LR    NEW,CURRENT                                         1023 04706000
         BCTR  NEW,0                                               1023 04707000
*                 CS(CURRENT,NEW,CSDGDINT);                        1024 04708000
         CS    CURRENT,@01,CSDGDINT(REG3)                          1024 04709000
*                 BC(7,SUB5);                                      1025 04710000
         BC    7,SUB5                                              1025 04711000
*                 PCCAMINT='0'B;                                   1026 04712000
*                                                                  1026 04713000
         NI    PCCAMINT(PCCAPTR),B'11110111'                       1026 04714000
*/********************************************************************/ 04715000
*/*                                                                  */ 04716000
*/* ASSEMBLE THE ERROR MSG, AND TELL THE OPERATOR THAT THE CPU TIMER */ 04717000
*/* IS PERMANENTLY DAMAGED.                                          */ 04718000
*/*                                                                  */ 04719000
*/********************************************************************/ 04720000
*                                                                  1027 04721000
*                 CVD(PCCACPUA,CVRTAREA);                          1027 04722000
         LH    @07,PCCACPUA(,PCCAPTR)                              1027 04723000
         CVD   @07,CVRTAREA                                        1027 04724000
*                 WPLPTR=ADDR(EMSGCPUT);                           1028 04725000
         LA    WPLPTR,EMSGCPUT                                     1028 04726000
*                 UNPK(CPUADDRE,CVRTAREA(7:8));                    1029 04727000
         UNPK  CPUADDRE(2,WPLPTR),CVRTAREA+6(2)                    1029 04728000
*                 CPUADDRE(2)=CPUADDRE(2) 'F0'X;                   1030 04729000
         OI    CPUADDRE+1(WPLPTR),X'F0'                            1030 04730000
*                 GENERATE CODE(WTO   MF=(E,(1)));                 1031 04731000
         WTO   MF=(E,(1))                                               04732000
*               END;                                               1032 04733000
*           END;                                                   1033 04734000
@RF01019 DS    0H                                                  1034 04735000
*       END;                                                       1034 04736000
@RF01017 DS    0H                                                  1035 04737000
*     REG2=REG2+LENGTH(PCCAT00P);   /* POINT TO NEXT PCCAVT ENTRY    */ 04738000
@RF00950 AH    REG2,@CH00108                                       1035 04739000
*   END;                                                           1036 04740000
         BCTR  SLOTCNT,0                                           1036 04741000
         LTR   SLOTCNT,SLOTCNT                                     1036 04742000
         BP    @DL00949                                            1036 04743000
*                                                                  1037 04744000
*/********************************************************************/ 04745000
*/*                                                                  */ 04746000
*/* THIS IS THE RETRY POINT FROM THE ESTAE EXIT.  REGISTER 15 MUST   */ 04747000
*/* BE USED FOR TEMPORARY ADDRESSABILITY IN THE MAINLINE BECAUSE IT  */ 04748000
*/* WILL CONTAIN THE RETRY ENTRY POINT ADDRESS IN A RETRY SITUATION. */ 04749000
*/*                                                                  */ 04750000
*/********************************************************************/ 04751000
*                                                                  1037 04752000
*   GENERATE CODE REFS(CLABASE) SETS(REG9,REG15) DEFS(CLARETRY);   1037 04753000
*                                                                  1037 04754000
*                                                              /*       04755000
         BALR  REG15,0            ESTABLISH ADDRESSABILITY FOR          04756000
         USING *,REG15               RETRY POINT COMPATABILITY          04757000
CLARETRY L     REG9,CLABASE       GET ORIGINAL BASE ADDRESS             04758000
         USING CLABSLBL,REG9                                            04759000
         DROP  REG15                                                    04760000
*/********************************************************************/ 04761000
*/*                                                                  */ 04762000
*/* MAKE SURE THAT THE REAL TIME TQE QUEUE IS KEPT ALIVE.            */ 04763000
*/*                                                                  */ 04764000
*/********************************************************************/ 04765000
*                                                                  1038 04766000
*   GENERATE CODE REFS(PSALITA);                                   1038 04767000
*                             /*                                        04768000
         SETLOCK  OBTAIN,TYPE=DISP,MODE=UNCOND,                       **04769000
               RELATED=('REAL TQE QUEUE',(IEAVRTOD,(NOLABEL))) */       04770000
*   CLACVTP=CVTPTR;                 /* RESTORE POINTER TO CVT        */ 04771000
         L     CLACVTP,CVTPTR                                      1039 04772000
*   REG12=TPCCKQ;                   /* GET ADDRESS OF REAL TIME Q  1040 04773000
*                                      CHECKING ROUTINE              */ 04774000
         L     @07,TPCPTR(,CLACVTP)                                1040 04775000
         L     REG12,TPCCKQ(,@07)                                  1040 04776000
*   IF CLABND='1'B THEN             /* IF THERE IS NOW A GOOD CLOCK     04777000
*                                      COMP/TODC,                    */ 04778000
         TM    CLABND,B'10000000'                                  1041 04779000
         BNO   @RF01041                                            1041 04780000
*     TPCABND='0'B;                 /* INDICATE IT TO SET CC         */ 04781000
         NI    TPCABND(@07),B'01111111'                            1042 04782000
*/********************************************************************/ 04783000
*/*                                                                  */ 04784000
*/* GO TO SET CC ROUTINE.  THEN RESTORE POINTER TO CVT AND RELEASE   */ 04785000
*/* THE DISPATCHER LOCK.                                             */ 04786000
*/*                                                                  */ 04787000
*/********************************************************************/ 04788000
*                                                                  1043 04789000
*   GENERATE CODE REFS(REG12) SETS(REG2)(BALR  REG2,REG12);        1043 04790000
@RF01041 DS    0H                                                  1043 04791000
         BALR  REG2,REG12                                               04792000
*   CLACVTP=CVTPTR;                 /* RESTORE POINTER TO CVT        */ 04793000
         L     CLACVTP,CVTPTR                                      1044 04794000
*   GENERATE CODE REFS(PSALITA);                                   1045 04795000
*                                                                  1045 04796000
*                              /*                                       04797000
         SETLOCK RELEASE,TYPE=DISP,                                   **04798000
               RELATED=('REAL TQE QUEUE',(IEAVRTOD,(NOLABEL)))  */      04799000
*/********************************************************************/ 04800000
*/*                                                                  */ 04801000
*/* SEE IF IEAVRCLA HAS BEEN SCHEDULED AGAIN TO RECOVER AN ERROR     */ 04802000
*/* THAT OCCURRED WHILE IT WAS IN THE PROCESS OF RECOVERING OTHER    */ 04803000
*/* ERRORS.  IT IT HAS, ANOTHER PASS WILL BE MADE THROUGH THE PCCA'S */ 04804000
*/* TO FIND THE NEW ERROR.  (NOTE:  THE COMBINATION OF THE TEST CYCLE*/ 04805000
*/* BIT(CLATC) = 0 AND THE IN-USE BIT(CLASIU) = 1 INDICATES THAT NO  */ 04806000
*/* FURTHER ERROR HAS OCCURRED.)  IF IT HAS NOT BEEN SCHEDULED AGAIN,*/ 04807000
*/* SET BOTH BITS TO 0.                                              */ 04808000
*/*                                                                  */ 04809000
*/********************************************************************/ 04810000
*                                                                  1046 04811000
*   CURRENT=2;                                                     1046 04812000
         LA    CURRENT,2                                           1046 04813000
*   NEW=0;                                                         1047 04814000
         SLR   NEW,NEW                                             1047 04815000
*   REG12=ADDR(TPCRSRB);            /* GET ADDRESSABILITY TO SRB     */ 04816000
         L     REG12,TPCPTR(,CLACVTP)                              1048 04817000
         LA    REG12,TPCRSRB(,REG12)                               1048 04818000
*   CS(CURRENT,NEW,REG12->SRBPARM);                                1049 04819000
         CS    CURRENT,@01,SRBPARM(REG12)                          1049 04820000
*   BC(7,SETTC);                    /* BRANCH IF MORE TO RECOVER     */ 04821000
         BC    7,SETTC                                             1050 04822000
*/********************************************************************/ 04823000
*/*                                                                  */ 04824000
*/* FREE THE PAGE CONTAINING IEAVRCLA.                               */ 04825000
*/*                                                                  */ 04826000
*/********************************************************************/ 04827000
*                                                                  1051 04828000
*   RESPECIFY                                                      1051 04829000
*     CVTMAP BASED(CVTPTR);         /*                       @ZA20300*/ 04830000
*   REG1=ADDR(SETTC);               /* GET POINTER TO START OF     1052 04831000
*                                      IEAVRCLA                      */ 04832000
         LA    REG1,SETTC                                          1052 04833000
*   REG15=ADDR(ENDTOD);             /* GET POINTER TO END OF IEAVRTOD*/ 04834000
         LA    REG15,ENDTOD                                        1053 04835000
*   GENERATE CODE(PGFREE R,A=(1),EA=(15));                         1054 04836000
*                                                                  1054 04837000
         PGFREE R,A=(1),EA=(15)                                         04838000
*/********************************************************************/ 04839000
*/*                                                                  */ 04840000
*/* GET RETURN ADDRESS.                                      @ZA20300*/ 04841000
*/*                                                                  */ 04842000
*/********************************************************************/ 04843000
*                                                                  1055 04844000
*   REG14=ADDR(CVTEXIT);            /*                       @ZA20300*/ 04845000
         L     REG14,CVTPTR                                        1055 04846000
         LA    REG14,CVTEXIT(,REG14)                               1055 04847000
*/********************************************************************/ 04848000
*/*                                                                  */ 04849000
*/* DELETE ERROR EXIT FOR UNLOCKED CODE, AND RETURN TO DISPATCHER.   */ 04850000
*/*                                                                  */ 04851000
*/********************************************************************/ 04852000
*                                                                  1056 04853000
*   GEN CODE(ESTAE 0);                                             1056 04854000
         ESTAE 0                                                        04855000
*   GENERATE CODE REFS(REG14)(BR    REG14);                        1057 04856000
         BR    REG14                                                    04857000
*   RESPECIFY                                                      1058 04858000
*    (PCCAPTR) UNRESTRICTED;                                       1058 04859000
*   RESPECIFY                                                      1059 04860000
*     CSD BASED(CVTCSD);                                           1059 04861000
*                                                                  1060 04862000
*/********************************************************************/ 04863000
*/*                                                                  */ 04864000
*/*         CLAESTAE--IEAVRCLA RECOVERY FOR UNLOCKED CODE            */ 04865000
*/*                                                                  */ 04866000
*/********************************************************************/ 04867000
*                                                                  1060 04868000
*CLAESTAE:                                                         1060 04869000
*   PROC OPTIONS(NOSAVE,NOSAVEAREA,NOENTREG,RETREG);               1060 04870000
*                                                                  1060 04871000
         B     @PB00008                                            1060 04872000
CLAESTAE DS    0H                                                  1061 04873000
*/********************************************************************/ 04874000
*/*                                                                  */ 04875000
*/* ESTABLISH TEMPORARY ADDRESSABILITY.                              */ 04876000
*/*                                                                  */ 04877000
*/********************************************************************/ 04878000
*                                                                  1061 04879000
*   GEN CODE SETS(REG12) REFS(REG9);                               1061 04880000
*                                                                  1061 04881000
         BALR  REG12,0                                                  04882000
         USING *,REG12                                                  04883000
         DROP  REG9                                                     04884000
*/********************************************************************/ 04885000
*/*                                                                  */ 04886000
*/* SEE IF SDWA WAS SUPPLIED                                         */ 04887000
*/*                                                                  */ 04888000
*/********************************************************************/ 04889000
*                                                                  1062 04890000
*   IF REG0=12 THEN                                                1062 04891000
*                                                                  1062 04892000
         CH    REG0,@CH00854                                       1062 04893000
         BNE   @RF01062                                            1062 04894000
*/********************************************************************/ 04895000
*/*                                                                  */ 04896000
*/* IF THE SDWA WAS NOT SUPPLIED, PUT RETRY ADDRESS IN REG 0 AND A   */ 04897000
*/* RETURN CODE OF 4 INTO REG 15 TO INDICATE RETRY.                  */ 04898000
*/*                                                                  */ 04899000
*/********************************************************************/ 04900000
*                                                                  1063 04901000
*     DO;                                                          1063 04902000
*       REG0=CLARTYA;               /* PUT RETRY ADDRESS INTO REG 0  */ 04903000
         L     REG0,CLARTYA                                        1064 04904000
*       RETCODE=4;                                                 1065 04905000
         LA    RETCODE,4                                           1065 04906000
*     END;                                                         1066 04907000
*                                                                  1066 04908000
*/********************************************************************/ 04909000
*/*                                                                  */ 04910000
*/* IF SUPPLIED, ISSUE SETRP TO ESTABLISH INTERFACE BACK TO RTM.     */ 04911000
*/*                                                                  */ 04912000
*/********************************************************************/ 04913000
*                                                                  1067 04914000
*   ELSE                                                           1067 04915000
*     DO;                           /* SETRP RECPARM(CLAID)RC(4)RETAD   04916000
*                                      DR(RETRYCLA)FRESDWA(YES)DUMP(N   04917000
*                                      O)                            */ 04918000
         B     @RC01062                                            1067 04919000
@RF01062 DS    0H                                                  1068 04920000
*       RESPECIFY                                                  1068 04921000
*        (GPR00P,                                                  1068 04922000
*         GPR01P,                                                  1068 04923000
*         GPR14P,                                                  1068 04924000
*         GPR15P) RSTD;                                            1068 04925000
*       GPR01P->SDWARCDE=4;         /* STORE RC INTO SDWA            */ 04926000
         MVI   SDWARCDE(GPR01P),X'04'                              1069 04927000
*       GPR01P->SDWARTYA=ADDR(RETRYCLA);/* SAVE RETRY ADDRESS        */ 04928000
         MVC   SDWARTYA(4,GPR01P),CLARTYA                          1070 04929000
*       GPR01P->SDWAFREE='1'B;      /* SET FLAG TO FREE SDWA         */ 04930000
         OI    SDWAFREE(GPR01P),B'00000100'                        1071 04931000
*       GPR15P=ADDR(CLAID);         /* ACCESS RECORD PARAMETER LIST  */ 04932000
         LA    GPR15P,CLAID                                        1072 04933000
*       GPR01P->SDWARECP=GPR15P->I256C(1:24);/* COPY RECORD        1073 04934000
*                                      PARAMETERS                    */ 04935000
         MVC   SDWARECP(24,GPR01P),I256C(GPR15P)                   1073 04936000
*       GPR01P->SDWAREQ='0'B;       /* TURN OFF DUMP INDICATOR       */ 04937000
         NI    SDWAREQ(GPR01P),B'01111111'                         1074 04938000
*       RESPECIFY                                                  1075 04939000
*        (GPR00P,                                                  1075 04940000
*         GPR01P,                                                  1075 04941000
*         GPR14P,                                                  1075 04942000
*         GPR15P) UNRSTD;                                          1075 04943000
*     END;                                                         1076 04944000
*                                                                  1076 04945000
*/********************************************************************/ 04946000
*/*                                                                  */ 04947000
*/* RETURN TO RTM.                                                   */ 04948000
*/*                                                                  */ 04949000
*/********************************************************************/ 04950000
*                                                                  1077 04951000
*   END CLAESTAE;                                                  1077 04952000
@EL00008 DS    0H                                                  1077 04953000
@EF00008 DS    0H                                                  1077 04954000
@ER00008 BR    @14                                                 1077 04955000
@PB00008 DS    0H                                                  1078 04956000
*                                                                  1078 04957000
*/********************************************************************/ 04958000
*/*                                                                  */ 04959000
*/* THIS ROUTINE GAINS CONTROL WHEN IT IS NECESSARY TO GET THE CUR-  */ 04960000
*/* RENT VALUE OF THE TOD CLOCK ON A GIVEN CPU, AND ALSO TO DETERMINE*/ 04961000
*/* ITS STATE.  IN AN MP CONFIGURATION, IT RECEIVES CONTROL VIA THE  */ 04962000
*/* IPC, WHILE ON A STRICTLY UP SYSTEM IT RECEIVES CONTROL VIA BALR. */ 04963000
*/*                                                                  */ 04964000
*/********************************************************************/ 04965000
*                                                                  1078 04966000
*DELTA1:                                                           1078 04967000
*   GENERATE CODE REFS(REG1,REG14,REG15,TCENTRY,TCWAPCCA) SETS(TCWACLKE 04968000
*       ,REG12,TCWALFLG,PCCASYNC);                                 1078 04969000
DELTA1   DS    0H                                                  1078 04970000
         USING *,REG15                  ADDRESSABILITY FOR ROUTINE      04971000
         USING TCENTRY,REG1             ADDRESSABILITY FOR TCWA         04972000
         STCK  TCWACLKE                 STORE CLOCK INTO TCWA ENTRY     04973000
         BALR  REG12,0                  GET CC FROM STCK INSTRUCTION    04974000
         STCM  REG12,8,TCWALFLG         STORE IT INTO TCWA ENTRY        04975000
         L     REG12,TCWAPCCA           GET PCCA ADDRESS                04976000
         NI    PCCASYNC(REG12),X'BF'    TURN OFF SYNC FALG IN PCCA      04977000
         BR    REG14                    RETURN                          04978000
         DROP  REG15                                                    04979000
         DROP  REG1                                                     04980000
*                                                                  1079 04981000
*/********************************************************************/ 04982000
*/*                                                                  */ 04983000
*/* THIS ROUTINE GAINS CONTROL WHEN IT IS NECESSARY TO SET THE TOD   */ 04984000
*/* CLOCK ON A GIVEN CPU.  IN AN MP CONFIGURATION, IT RECEIVES CON-  */ 04985000
*/* TROL VIA THE IPC, WHILE ON A STRICTLY UP SYSTEM IT RECEIVES CON- */ 04986000
*/* TROL VIA BALR.                                                   */ 04987000
*/*                                                                  */ 04988000
*/********************************************************************/ 04989000
*                                                                  1079 04990000
*   GENERATE CODE REFS(REG15)(USING *,REG15);                      1079 04991000
         USING *,REG15                                                  04992000
*DELTA2:                                                           1080 04993000
*   GENERATE CODE REFS(REG1) SETS(TCWACTL)                         1080 04994000
*       (STCTL 0,0,TCWACTL(REG1)  STORE CONTROL REG 0);            1080 04995000
DELTA2   STCTL 0,0,TCWACTL(REG1)  STORE CONTROL REG 0                   04996000
*   REG1->TCSTOP='1'B;              /* TURN ON BIT TO STOP TOD CLOCK */ 04997000
         OI    TCSTOP(REG1),B'00100000'                            1081 04998000
*   GENERATE CODE REFS(REG1,TCWACTL)                               1082 04999000
*       (LCTL  0,0,TCWACTL(REG1)  LOAD CONTROL REG 0);             1082 05000000
         LCTL  0,0,TCWACTL(REG1)  LOAD CONTROL REG 0                    05001000
*   REG1->TCGNSET='0'B;             /* INDICATE SECURITY SW DEPRESSED*/ 05002000
         NI    TCGNSET(REG1),B'11110111'                           1083 05003000
*   GENERATE CODE REFS(TCWACVAL)(SCK   TCWACVAL(REG1)  SET THE CLOCK);  05004000
         SCK   TCWACVAL(REG1)  SET THE CLOCK                            05005000
*   GENERATE CODE REFS(REG14)(BCR   8,REG14     RETURN IF SWITCH DOWN); 05006000
         BCR   8,REG14     RETURN IF SWITCH DOWN                        05007000
*   REG1->TCGNSET='1'B;             /* ELSE, INDICATE SW RELEASED    */ 05008000
         OI    TCGNSET(REG1),B'00001000'                           1086 05009000
*   GENERATE CODE REFS(REG14)(BR    REG14         RETURN);         1087 05010000
         BR    REG14         RETURN                                     05011000
*   GENERATE CODE REFS(REG15)(DROP  REG15);                        1088 05012000
         DROP  REG15                                                    05013000
*                                                                  1089 05014000
*/********************************************************************/ 05015000
*/*                                                                  */ 05016000
*/* THIS ROUTINE GAINS CONTROL WHEN IT IS NECESSARY TO GET THE CUR-  */ 05017000
*/* VALUE OF THE TOD CLOCK ON A GIVEN CPU.  IN AN MP CONFIGURATION,  */ 05018000
*/* IT RECEIVES CONTROL VIA THE IPC, WHILE IN A STRICTLY UP SYSTEM   */ 05019000
*/* IT RECEIVES CONTROL VIA BALR.                                    */ 05020000
*/*                                                                  */ 05021000
*/********************************************************************/ 05022000
*                                                                  1089 05023000
*DELTA3:                                                           1089 05024000
*   GENERATE CODE REFS(REG1,REG14,REG15);                          1089 05025000
DELTA3   DS    0H                                                  1089 05026000
         USING *,REG15                 ADDRESSABILITY FOR ROUTINE       05027000
         STCK  0(REG1)                 STORE CURRENT CLOCK VALUE        05028000
         BR    REG14                   RETURN                           05029000
         DROP  REG15                                                    05030000
*                                                                  1090 05031000
*/********************************************************************/ 05032000
*/*                                                                  */ 05033000
*/* THIS ROUTINE STORES THE TOD CLOCK ON A GIVEN CPU UNTIL BIT 31    */ 05034000
*/* CHANGES IN ORDER TO ALLOW A MAXIMUM INTERVAL (UNTIL BIT 31       */ 05035000
*/* CHANGES AGAIN) FOR SYNCHRONIZING THE TOD CLOCKS.  IT ALWAYS      */ 05036000
*/* RECEIVES CONTROL VIA THE IPC.                                    */ 05037000
*/*                                                                  */ 05038000
*/********************************************************************/ 05039000
*                                                                  1090 05040000
*DELTA4:                                                           1090 05041000
*   GENERATE CODE REFS(REG15,REG1,TCWA);                           1090 05042000
*                                                                  1090 05043000
DELTA4   DS    0H                                                  1090 05044000
         USING *,REG15                ADDRESSABILITY FOR ROUTINE        05045000
         USING TCWA,REG1              ADDRESSABILITY FOR TCWA           05046000
*/********************************************************************/ 05047000
*/*                                                                  */ 05048000
*/* INITIALIZE STARTING AND ENDING CLOCK VALUES.                     */ 05049000
*/*                                                                  */ 05050000
*/********************************************************************/ 05051000
*                                                                  1091 05052000
*   RESPECIFY                                                      1091 05053000
*     TCWA BASED(REG1);                                            1091 05054000
*   GENERATE CODE SETS(TCWAA1,TCWAA2);                             1092 05055000
*                                                                  1092 05056000
*                                                         /*@ZA28478*/  05057000
         STCK  TCWAA1                                        @ZA28478   05058000
         STCK  TCWAA2                                        @ZA28478   05059000
*/********************************************************************/ 05060000
*/*                                                                  */ 05061000
*/* STORE THE TOD CLOCK SUCCESSIVELY INTO TCWAA2 UNTIL BIT           */ 05062000
*/* 31 CHANGES.  (NOTE: THIS CHANGE IS INDICATED BY THE HIGH ORDER   */ 05063000
*/* 32 BITS OF THE 2 VALUES NOT BEING EQUAL.)                        */ 05064000
*/*                                                                  */ 05065000
*/********************************************************************/ 05066000
*                                                                  1093 05067000
*   DO WHILE TCWAA2L=TCWAA1L;                                      1093 05068000
         B     @DE01093                                            1093 05069000
@DL01093 DS    0H                                                  1094 05070000
*     GENERATE CODE SETS(TCWAA2)(STCK  TCWAA2);/*            @ZA28478*/ 05071000
         STCK  TCWAA2                                                   05072000
*   END;                                                           1095 05073000
@DE01093 CLC   TCWAA2L(4,REG1),TCWAA1L(REG1)                       1095 05074000
         BE    @DL01093                                            1095 05075000
*   GENERATE CODE REFS(REG14)(BR    REG14                 RETURN); 1096 05076000
         BR    REG14                 RETURN                             05077000
*                                                                  1097 05078000
*/********************************************************************/ 05079000
*/*                                                                  */ 05080000
*/* THIS ROUTINE GAINS CONTROL WHEN IT IS NECESSARY FOR THE OPERATOR */ 05081000
*/* TO PLACE THE TOD CLOCK SECURITY SWITCH IN THE ENABLED POSITION   */ 05082000
*/* SO THAT THE CLOCKS CAN BE SYNCHRONIZED.  IT ATTEMPTS FOR 30      */ 05083000
*/* SECONDS TO SET THE CLOCK, THE PURPOSE BEING TO DETERMINE IF THE  */ 05084000
*/* SECURITY SWITCH HAS BEEN ENABLED.  IT EXITS EITHER AFTER 30      */ 05085000
*/* SECONDS HAS ELAPSED, OR WHEN THE SWITCH HAS BEEN ENABLED.        */ 05086000
*/*                                                                  */ 05087000
*/********************************************************************/ 05088000
*                                                                  1097 05089000
*DELTA7:                                                           1097 05090000
*   RESPECIFY                                                      1097 05091000
*     TCWA BASED(REG1);                                            1097 05092000
DELTA7   DS    0H                                                  1098 05093000
*   GENERATE CODE REFS(REG1,REG15,TCWA) SETS(TCWAA1,TCWAA2);       1098 05094000
*                                                                  1098 05095000
         USING *,REG15                                                  05096000
         USING TCWA,REG1                                                05097000
         STPT  TCWAA1               STORE INTERVAL START VALUE          05098000
         STPT  TCWAA2               STORE INTERVAL END VALUE            05099000
*/********************************************************************/ 05100000
*/*                                                                  */ 05101000
*/* LOOP ON A SET CLOCK INSTRUCTION UNTIL THE OPERATOR DEPRESSES THE */ 05102000
*/* SECURITY SWITCH, OR THE ALLOTTED INTERVAL EXPIRES.               */ 05103000
*/*                                                                  */ 05104000
*/********************************************************************/ 05105000
*                                                                  1099 05106000
*   DO WHILE TCWAA1L-TCWAA2L<30&TCGNSET='1'B;                      1099 05107000
         B     @DE01099                                            1099 05108000
@DL01099 DS    0H                                                  1100 05109000
*     GENERATE CODE REFS(TCWACVAL)(SCK   TCWACVAL);                1100 05110000
*                                                                  1100 05111000
         SCK   TCWACVAL                                                 05112000
*/********************************************************************/ 05113000
*/*                                                                  */ 05114000
*/* TEST FOR SUCCESSFUL EXECUTION OF THE SET CLOCK INSTRUCTION.      */ 05115000
*/*                                                                  */ 05116000
*/********************************************************************/ 05117000
*                                                                  1101 05118000
*     BC(7,STCPUT);                                                1101 05119000
*                                                                  1101 05120000
         BC    7,STCPUT                                            1101 05121000
*/********************************************************************/ 05122000
*/*                                                                  */ 05123000
*/* IF SUCCESSFUL, INDICATE IN TCWA THAT SWITCH IS ENABLED.  THEN,   */ 05124000
*/* WHETHER ENABLED OR NOT, STORE AN INTERVAL ENDING VALUE.          */ 05125000
*/*                                                                  */ 05126000
*/********************************************************************/ 05127000
*                                                                  1102 05128000
*     TCGNSET='0'B;                                                1102 05129000
         NI    TCGNSET(REG1),B'11110111'                           1102 05130000
*STCPUT:                                                           1103 05131000
*     GENERATE CODE SETS(TCWAA2)(STPT  TCWAA2);                    1103 05132000
STCPUT   STPT  TCWAA2                                                   05133000
*   END;                                                           1104 05134000
*                                                                  1104 05135000
@DE01099 L     @07,TCWAA1L(,REG1)                                  1104 05136000
         SL    @07,TCWAA2L(,REG1)                                  1104 05137000
         CL    @07,@CF00189                                        1104 05138000
         BNL   @DC01099                                            1104 05139000
         TM    TCGNSET(REG1),B'00001000'                           1104 05140000
         BO    @DL01099                                            1104 05141000
@DC01099 DS    0H                                                  1105 05142000
*/********************************************************************/ 05143000
*/*                                                                  */ 05144000
*/* RETURN TO EXTERNAL SECOND LEVEL INTERRUPT HANDLER.               */ 05145000
*/*                                                                  */ 05146000
*/********************************************************************/ 05147000
*                                                                  1105 05148000
*   GENERATE CODE REFS(REG9,REG10,REG14);                          1105 05149000
         BR    REG14                                                    05150000
         USING @PSTART,REG9                                             05151000
         USING @PSTART+4095,REG10                                       05152000
*   DECLARE                         /* GENERAL PURPOSE REGISTERS     */ 05153000
*     GPR00P PTR(31) REG(0),                                       1106 05154000
*     GPR01P PTR(31) REG(1),                                       1106 05155000
*     GPR14P PTR(31) REG(14),                                      1106 05156000
*     GPR15P PTR(31) REG(15);                                      1106 05157000
*   DECLARE                         /* COMMON VARIABLES              */ 05158000
*     I256C CHAR(256) BASED,                                       1107 05159000
*     I031F FIXED(31) BASED,                                       1107 05160000
*     I031P PTR(31) BASED,                                         1107 05161000
*     I015F FIXED(15) BASED,                                       1107 05162000
*     I015P PTR(15) BASED,                                         1107 05163000
*     I008P PTR(8) BASED,                                          1107 05164000
*     I001C CHAR(1) BASED;                                         1107 05165000
*   END IEAVRTOD                                                   1108 05166000
*                                                                  1108 05167000
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */ 05168000
*/*%INCLUDE SYSLIB  (IEAVVTPC)                                       */ 05169000
*/*%INCLUDE SYSLIB  (IEZWPL  )                                       */ 05170000
*/*%INCLUDE SYSLIB  (IEEBASEA)                                       */ 05171000
*/*%INCLUDE SYSLIB  (IHALCCAT)                                       */ 05172000
*/*%INCLUDE SYSLIB  (IHALCCA )                                       */ 05173000
*/*%INCLUDE SYSLIB  (IHAASVT )                                       */ 05174000
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */ 05175000
*/*%INCLUDE SYSLIB  (IHASRB  )                                       */ 05176000
*/*%INCLUDE SYSLIB  (IHAASCB )                                       */ 05177000
*/*%INCLUDE SYSLIB  (IHACSD  )                                       */ 05178000
*/*%INCLUDE SYSLIB  (IHAPCCA )                                       */ 05179000
*/*%INCLUDE SYSLIB  (IHAPCCAT)                                       */ 05180000
*/*%INCLUDE SYSLIB  (IHATQE  )                                       */ 05181000
*/*%INCLUDE SYSLIB  (CVT     )                                       */ 05182000
*/*%INCLUDE SYSLIB  (IHASDWA )                                       */ 05183000
*                                                                  1108 05184000
*       ;                                                          1108 05185000
         B     @EL00001                                            1108 05186000
@DATA    DS    0H                                                       05187000
@CH00119 DC    H'2'                                                     05188000
@CH00108 DC    H'4'                                                     05189000
@CH00038 DC    H'8'                                                     05190000
@CH00854 DC    H'12'                                                    05191000
@CH00503 DC    H'20'                                                    05192000
@CH02651 DC    H'48'                                                    05193000
@CH02606 DC    H'112'                                                   05194000
@CH02877 DC    H'-60'                                                   05195000
@DATD    DSECT                                                          05196000
         DS    0F                                                       05197000
@SA00001 DS    18F                                                      05198000
@PC00001 DS    2F                                                       05199000
@PC00006 DS    3F                                                       05200000
IEAVRTOD CSECT                                                          05201000
         DS    0F                                                       05202000
@CF00096 DC    F'1'                                                     05203000
@CH00096 EQU   @CF00096+2                                               05204000
@CF00045 DC    F'16'                                                    05205000
@CH00045 EQU   @CF00045+2                                               05206000
@CF00189 DC    F'30'                                                    05207000
@DATD    DSECT                                                          05208000
         DS    0D                                                       05209000
PTREBEG  DS    A                                                        05210000
PTREEND  DS    A                                                        05211000
RCSAVE   DS    F                                                        05212000
I5       DS    F                                                        05213000
LOOPCTR  DS    F                                                        05214000
SETLOOP  DS    F                                                        05215000
TODECB   DS    F                                                        05216000
TCWALN   DS    H                                                        05217000
CALLERID DS    H                                                        05218000
CPUSLEFT DS    H                                                        05219000
         DS    0D                                                       05220000
@TS00001 DS    CL8                                                      05221000
R1314SAV DS    CL8                                                      05222000
         ORG   R1314SAV                                                 05223000
R13SAV   DS    AL4                                                      05224000
RETSAVE  DS    AL4                                                      05225000
         ORG   R1314SAV+8                                               05226000
SSCRPARM DS    CL12                                                     05227000
         ORG   SSCRPARM                                                 05228000
SSCBASE  DS    CL12                                                     05229000
         ORG   SSCBASE                                                  05230000
SSCREG8  DS    AL4                                                      05231000
SSCREG9  DS    AL4                                                      05232000
SSCREG10 DS    AL4                                                      05233000
         ORG   SSCRPARM+12                                              05234000
IEAVRTOD CSECT                                                          05235000
         DS    0F                                                       05236000
@SIZDATD DC    AL1(0)                                                   05237000
         DC    AL3(@ENDDATD-@DATD)                                      05238000
@CA02733 DC    A(IEAVRSSC)                                              05239000
         DS    0D                                                       05240000
CLAECB   DS    F                                                        05241000
CLABASE  DC    AL4(CLABSLBL)                                            05242000
CLARTYA  DC    AL4(CLARETRY)                                            05243000
SSCRTYA  DC    AL4(SSCRETRY)                                            05244000
MICSMIN  DC    F'60000000'                                              05245000
MINSHOUR DC    F'60'                                                    05246000
DAYS4YRS DC    F'1461'                                                  05247000
DAYSFACT DC    F'10546875'                                              05248000
MICSSEC  DC    F'1000000'                                               05249000
K100     DC    H'100'                                                   05250000
K1000    DC    H'1000'                                                  05251000
K10000   DC    H'10000'                                                 05252000
MINSDAY  DC    H'1440'                                                  05253000
@CB02728 DC    X'0000000000000000'                                      05254000
CLAFLGS  DC    X'00'                                                    05255000
         ORG   CLAFLGS                                                  05256000
CLABND   DS    BL1                                                      05257000
@NM00001 EQU   CLAFLGS+0                                                05258000
         ORG   CLAFLGS+1                                                05259000
CLAID    DS    CL24                                                     05260000
         ORG   CLAID                                                    05261000
CLAMODNM DC    CL8'IEAVRTOD'                                            05262000
CLACSECT DC    CL8'IEAVRTOD'                                            05263000
CLAFRRID DC    CL8'CLAESTAE'                                            05264000
         ORG   CLAID+24                                                 05265000
IDVRSSC  DC    X'03808000'                                              05266000
SSCID    DS    CL24                                                     05267000
         ORG   SSCID                                                    05268000
SSCMODNM DC    CL8'IEAVRTOD'                                            05269000
SSCCSECT DC    CL8'IEAVRTOD'                                            05270000
SSCFRRID DC    CL8'SSCESTAE'                                            05271000
         ORG   SSCID+24                                                 05272000
         DS    CL7                                                      05273000
CVRTAREA DS    CL8                                                      05274000
TMSTINIT DC    X'80000000'                                              05275000
CAPU     DC    CL2'U '                                                  05276000
SMALLU   DC    X'A440'                                                  05277000
CHARSET  DC    CL4'SET '                                                05278000
DTEPATRN DC    X'F0212020204B202020'                               @L01 05279000
CLKPATRN DC    X'2120204B20204B2020'                                    05280000
PATCHTOD DC    CL250' '                                                 05281000
         DS    CL2                                                      05282000
CLASAV   DS    18F                                                      05283000
P1900    DC    P'1900000'          Adjustment for 4-digit year     @L01 05283500
@DATD    DSECT                                                          05284000
*                           /*                                          05285000
*                                                                       05286000
ESTAEL   ESTAE ,MF=L                                                    05287000
*                                                            */         05288000
IEAVRTOD CSECT                                                          05289000
*                                                /*                     05290000
*                                                                       05291000
* c117500                                                    @YM04380   05292000
MSGXXX1  WTO  '   IEA888A UTC   DATE=YYYY.DDD,CLOCK=HH.MM.SS',     @L01+05293000
               ROUTCDE=(1),DESC=(3),MCSFLAG=(NOTIME),MF=L               05294000
*                                                                       05295000
* c117800-117900                                             @YM04380   05296000
* C117900                                                    @YM08124   05297000
MSGXXX2  WTOR  'IEA888A LOCAL DATE=YYYY.DDD,CLOCK=HH.MM.SS  REPLY U, OR+05298000
                UTC/LOCAL TIME',,40,ROUTCDE=(1),DESC=(2),          @L01+05299000
               MCSFLAG=(NOTIME),MF=L                                    05300000
*                                                                       05301000
MSGYYY   WTOR  'IEA886A TOD CLOCK(S) MUST BE SET',,40,                **05302000
               ROUTCDE=(1),DESC=(2),MCSFLAG=(NOTIME),MF=L               05303000
*                                                                       05304000
* c118500                                                    @YM04380   05305000
MSGZZZ1  WTO   '   IEA887A CPU XX LOCAL DATE=YYYY.DDD,CLOCK=HH.MM.SS', +05306000
               ROUTCDE=(1),DESC=(3),MCSFLAG=(NOTIME),MF=L          @L01 05307000
*                                                                       05308000
MSGZZZ2  WTOR  'IEA887A TOD CLOCKS MUST BE SET, OR SELECT ADDRESS',,  **05309000
               40,ROUTCDE=(1),DESC=(2),MCSFLAG=(NOTIME),MF=L            05310000
*                                                                       05311000
MSGDEPSS WTOR  'IEA889A DEPRESS TOD CLOCK SECURITY SWITCH',,40,       **05312000
               ROUTCDE=(1),DESC=(2),MCSFLAG=(NOTIME),MF=L               05313000
*                                                                       05314000
EMSGTOD  WTO   'IEA898E CPU XX HAS PERMANENTLY DAMAGED TOD CLOCK',    **05315000
               ROUTCDE=(2,10),DESC=(11),MCSFLAG=(NOTIME),MF=L           05316000
*                                                             @G64UP2K  05317000
EMSGCC   WTO   'IEA898E CPU XX HAS PERMANENTLY DAMAGED CLOCK COMPARATORR05318000
               ',ROUTCDE=(2,10),DESC=(11),MCSFLAG=(NOTIME),MF=L         05319000
*                                                             @G64UP2K  05320000
EMSGCPUT WTO   'IEA898E CPU XX HAS PERMANENTLY DAMAGED CPU TIMER',    XX05321000
               ROUTCDE=(2,10),DESC=(11),MCSFLAG=(NOTIME),MF=L           05322000
*                                                             @G64UP2K  05323000
ENDTOD   EQU   *                      END OF IEAVRTOD                   05324000
*                                                                  */   05325000
@DATD    DSECT                                                          05326000
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA                05327000
@ENDDATD EQU   *                                                        05328000
IEAVRTOD CSECT                                                          05329000
@00      EQU   00                      EQUATES FOR REGISTERS 0-15       05330000
@01      EQU   01                                                       05331000
@02      EQU   02                                                       05332000
@03      EQU   03                                                       05333000
@04      EQU   04                                                       05334000
@05      EQU   05                                                       05335000
@06      EQU   06                                                       05336000
@07      EQU   07                                                       05337000
@08      EQU   08                                                       05338000
@09      EQU   09                                                       05339000
@10      EQU   10                                                       05340000
@11      EQU   11                                                       05341000
@12      EQU   12                                                       05342000
@13      EQU   13                                                       05343000
@14      EQU   14                                                       05344000
@15      EQU   15                                                       05345000
REG0     EQU   @00                                                      05346000
TMICSECL EQU   @00                                                      05347000
CURRENT  EQU   @00                                                      05348000
MASKHLDR EQU   @00                                                      05349000
REG1     EQU   @01                                                      05350000
TMICSECR EQU   @01                                                      05351000
NEW      EQU   @01                                                      05352000
SDWAPTR  EQU   @01                                                      05353000
TQEPTR   EQU   @01                                                      05354000
WPLPTR   EQU   @01                                                      05355000
SRBPTR   EQU   @01                                                      05356000
REG2     EQU   @02                                                      05357000
LCVTPTR  EQU   @02                                                      05358000
TCWAPTR  EQU   @03                                                      05359000
REG3     EQU   @03                                                      05360000
REG8     EQU   @08                                                      05361000
SLOTCNT  EQU   @08                                                      05362000
REG9     EQU   @09                                                      05363000
REG10    EQU   @10                                                      05364000
PCCAPTR  EQU   @10                                                      05365000
REG11    EQU   @11                                                      05366000
REG12    EQU   @12                                                      05367000
REG13    EQU   @13                                                      05368000
REG14    EQU   @14                                                      05369000
LCCAPTR  EQU   @14                                                      05370000
REG15    EQU   @15                                                      05371000
RETCODE  EQU   @15                                                      05372000
CLACVTP  EQU   @15                                                      05373000
GPR00P   EQU   @00                                                      05374000
GPR01P   EQU   @01                                                      05375000
GPR14P   EQU   @14                                                      05376000
GPR15P   EQU   @15                                                      05377000
CVTPTR   EQU   16                                                       05378000
RETRYCLA EQU   0                                                        05379000
RETRYSSC EQU   0                                                        05380000
ENQTQE   EQU   0                                                        05381000
DEQTQE   EQU   0                                                        05382000
PTRPCCA  EQU   0                                                        05383000
PTRLCCA  EQU   0                                                        05384000
TPC      EQU   0                                                        05385000
IEATPC   EQU   TPC                                                      05386000
TPCFLGS1 EQU   IEATPC+4                                                 05387000
TPCABND  EQU   TPCFLGS1                                                 05388000
TPCSYNC  EQU   TPCFLGS1                                                 05389000
TPCTZORG EQU   IEATPC+8                                                 05390000
TPCHDCCQ EQU   IEATPC+12                                                05391000
TPCDMTQE EQU   IEATPC+16                                                05392000
DFD      EQU   TPCDMTQE+4                                               05393000
TPCMNTQE EQU   IEATPC+40                                                05394000
MNIGHT   EQU   TPCMNTQE+16                                              05395000
MNIGHTLH EQU   MNIGHT                                                   05396000
TPCMFTQE EQU   IEATPC+64                                                05397000
IEATSELM EQU   IEATPC+88                                                05398000
TPCLMTQE EQU   IEATPC+112                                               05399000
TPCWORK  EQU   IEATPC+264                                               05400000
TPCTCWA  EQU   IEATPC+356                                               05401000
TPCRSRB  EQU   IEATPC+360                                               05402000
TPCCKQ   EQU   IEATPC+404                                               05403000
TPCCLA   EQU   IEATPC+408                                               05404000
WPLRF    EQU   0                                                        05405000
WPLRPTR  EQU   WPLRF                                                    05406000
WPLRPTRA EQU   WPLRPTR+1                                                05407000
WPLRECB  EQU   WPLRF+4                                                  05408000
WPL      EQU   0                                                        05409000
WPLMCSF  EQU   WPL+2                                                    05410000
WPLMCSF1 EQU   WPLMCSF                                                  05411000
WPLMCSF2 EQU   WPLMCSF+1                                                05412000
WPLTXT   EQU   WPL+4                                                    05413000
WPLFLGS  EQU   0                                                        05414000
WPLDESC  EQU   WPLFLGS                                                  05415000
WPLDESC1 EQU   WPLDESC                                                  05416000
WPLDESC2 EQU   WPLDESC+1                                                05417000
WPLROUT  EQU   WPLFLGS+2                                                05418000
WPLROUT1 EQU   WPLROUT                                                  05419000
WPLROUT2 EQU   WPLROUT+1                                                05420000
WPLMSGTY EQU   WPLFLGS+4                                                05421000
WPLMSGT1 EQU   WPLMSGTY                                                 05422000
WPLMSGT2 EQU   WPLMSGTY+1                                               05423000
WPLLS01  EQU   0                                                        05424000
WPLLTF   EQU   WPLLS01                                                  05425000
WPLLTF1  EQU   WPLLTF                                                   05426000
WPLML    EQU   0                                                        05427000
WPLMLLTF EQU   WPLML+2                                                  05428000
WPLMLLT1 EQU   WPLMLLTF                                                 05429000
BASE     EQU   0                                                        05430000
@NM00039 EQU   BASE+12                                                  05431000
@NM00040 EQU   @NM00039+1                                               05432000
BAIPLCC  EQU   @NM00040+2                                               05433000
BALGSTAT EQU   BASE+24                                                  05434000
BALOG    EQU   BALGSTAT+4                                               05435000
BASFL    EQU   BASE+36                                                  05436000
BARSW    EQU   BASFL                                                    05437000
BAIN     EQU   BARSW                                                    05438000
MSTODWTO EQU   BARSW                                                    05439000
BAINTSET EQU   BARSW                                                    05440000
MSLOGST  EQU   BASE+46                                                  05441000
BASPBYTE EQU   BASE+47                                                  05442000
BAMONITR EQU   BASE+56                                                  05443000
@NM00059 EQU   BASE+104                                                 05444000
MSLGCLOZ EQU   BASE+108                                                 05445000
MSCLOSSW EQU   MSLGCLOZ                                                 05446000
MSLGWLOG EQU   BASE+112                                                 05447000
MSWLOGSW EQU   MSLGWLOG                                                 05448000
MSLGWTR  EQU   BASE+116                                                 05449000
MSWTRSW  EQU   MSLGWTR                                                  05450000
MSLGSTRT EQU   BASE+120                                                 05451000
MSSTRTSW EQU   MSLGSTRT                                                 05452000
MS1BASEX EQU   BASE+136                                                 05453000
BASES01  EQU   0                                                        05454000
MSSSB    EQU   BASES01+1                                                05455000
MSBTN    EQU   MSSSB                                                    05456000
MSECBFL  EQU   BASES01+3                                                05457000
BAMTRTN  EQU   BASES01+8                                                05458000
BAMTSPSZ EQU   BASES01+12                                               05459000
BASTSPSZ EQU   BASES01+16                                               05460000
BAMTCDR  EQU   BASES01+20                                               05461000
BAMTDTSZ EQU   BASES01+28                                               05462000
BAMTCNTL EQU   BASES01+32                                               05463000
BAMTRECF EQU   BASES01+33                                               05464000
BAMTITFL EQU   BASES01+34                                               05465000
BAMTDECB EQU   BASES01+36                                               05466000
BAMTDSW  EQU   BAMTDECB                                                 05467000
BAMTINIT EQU   BASES01+40                                               05468000
LCCAVT   EQU   0                                                        05469000
LCCAT00P EQU   LCCAVT                                                   05470000
LCCA     EQU   0                                                        05471000
LCCAIHRC EQU   LCCA+520                                                 05472000
LCCAIHR1 EQU   LCCAIHRC                                                 05473000
LCCAIHR2 EQU   LCCAIHRC+1                                               05474000
LCCAIHR3 EQU   LCCAIHRC+2                                               05475000
LCCAIHR4 EQU   LCCAIHRC+3                                               05476000
LCCASPIN EQU   LCCA+524                                                 05477000
LCCASPN1 EQU   LCCASPIN                                                 05478000
LCCASPN2 EQU   LCCASPIN+1                                               05479000
LCCASPN3 EQU   LCCASPIN+2                                               05480000
LCCASPN4 EQU   LCCASPIN+3                                               05481000
LCCADSF1 EQU   LCCA+540                                                 05482000
LCCATIMR EQU   LCCADSF1                                                 05483000
LCCADSF2 EQU   LCCA+541                                                 05484000
LCCADTOD EQU   LCCA+600                                                 05485000
LCCAWTIM EQU   LCCA+616                                                 05486000
LCCACRFL EQU   LCCA+692                                                 05487000
LCCACREX EQU   LCCA+693                                                 05488000
LCCALKFG EQU   LCCA+694                                                 05489000
LCCASRBF EQU   LCCA+720                                                 05490000
ASVT     EQU   0                                                        05491000
ASVTMAXU EQU   ASVT+516                                                 05492000
ASVTFRST EQU   ASVT+524                                                 05493000
ASVTENTY EQU   ASVT+528                                                 05494000
ASVTAVAL EQU   ASVTENTY                                                 05495000
PSA      EQU   0                                                        05496000
FLCRNPSW EQU   PSA                                                      05497000
FLCROPSW EQU   PSA+8                                                    05498000
FLCICCW2 EQU   PSA+16                                                   05499000
PSAEEPSW EQU   PSA+132                                                  05500000
PSAESPSW EQU   PSA+136                                                  05501000
FLCSVILC EQU   PSAESPSW+1                                               05502000
PSAEPPSW EQU   PSA+140                                                  05503000
FLCPIILC EQU   PSAEPPSW+1                                               05504000
FLCPICOD EQU   PSAEPPSW+2                                               05505000
PSAPICOD EQU   FLCPICOD+1                                               05506000
FLCTEA   EQU   PSAEPPSW+4                                               05507000
FLCPER   EQU   PSA+152                                                  05508000
FLCMCLA  EQU   PSA+168                                                  05509000
FLCCHNID EQU   FLCMCLA                                                  05510000
FLCCHTM  EQU   FLCCHNID                                                 05511000
FLCIOEL  EQU   FLCMCLA+4                                                05512000
FLCIOA   EQU   FLCMCLA+16                                               05513000
PSAMEDC  EQU   FLCMCLA+76                                               05514000
FLCFSA   EQU   FLCMCLA+80                                               05515000
PSALCCAV EQU   PSA+528                                                  05516000
PSASUPER EQU   PSA+552                                                  05517000
PSASUP1  EQU   PSASUPER                                                 05518000
PSASUP2  EQU   PSASUPER+1                                               05519000
PSASUP3  EQU   PSASUPER+2                                               05520000
PSASUP4  EQU   PSASUPER+3                                               05521000
PSACLHT  EQU   PSA+640                                                  05522000
PSALKSA  EQU   PSA+696                                                  05523000
PSAHLHI  EQU   PSA+760                                                  05524000
PSALITA  EQU   PSA+764                                                  05525000
PSADSSFL EQU   PSA+836                                                  05526000
PSADSSF1 EQU   PSADSSFL                                                 05527000
PSADSSF3 EQU   PSADSSFL+2                                               05528000
PSADSSF4 EQU   PSADSSFL+3                                               05529000
PSARSVT  EQU   PSA+896                                                  05530000
SRBSECT  EQU   0                                                        05531000
SRB      EQU   SRBSECT                                                  05532000
SRBFLC   EQU   SRB+12                                                   05533000
SRBPARM  EQU   SRB+28                                                   05534000
ASCB     EQU   0                                                        05535000
ASCBSUPC EQU   ASCB+16                                                  05536000
ASCBRSM  EQU   ASCB+52                                                  05537000
ASCBRSMF EQU   ASCBRSM                                                  05538000
ASCBEJST EQU   ASCB+64                                                  05539000
ASCBFW1  EQU   ASCB+100                                                 05540000
ASCBRCTF EQU   ASCBFW1+2                                                05541000
ASCBFLG1 EQU   ASCBFW1+3                                                05542000
ASCBDSP1 EQU   ASCB+114                                                 05543000
ASCBFLG2 EQU   ASCB+115                                                 05544000
ASCBLKGP EQU   ASCB+128                                                 05545000
ASCBSRQ  EQU   ASCB+180                                                 05546000
ASCBSRQ1 EQU   ASCBSRQ                                                  05547000
ASCBSRQ2 EQU   ASCBSRQ+1                                                05548000
ASCBSRQ3 EQU   ASCBSRQ+2                                                05549000
ASCBSRQ4 EQU   ASCBSRQ+3                                                05550000
ASCBSRBM EQU   ASCB+195                                                 05551000
CSD      EQU   0                                                        05552000
CSDCPUAL EQU   CSD+8                                                    05553000
CSDCPUOL EQU   CSD+10                                                   05554000
CSDSCWRD EQU   CSD+12                                                   05555000
CSDSCFL1 EQU   CSDSCWRD                                                 05556000
CSDSCFL2 EQU   CSDSCWRD+1                                               05557000
CSDSCFL3 EQU   CSDSCWRD+2                                               05558000
CSDSCFL4 EQU   CSDSCWRD+3                                               05559000
CSDFLAGS EQU   CSD+23                                                   05560000
CSDMP    EQU   CSDFLAGS                                                 05561000
CSDGDCC  EQU   CSD+108                                                  05562000
CSDGDINT EQU   CSD+112                                                  05563000
CSDGDTOD EQU   CSD+116                                                  05564000
PCCA     EQU   0                                                        05565000
PCCACPUA EQU   PCCA+16                                                  05566000
PCCATQEP EQU   PCCA+20                                                  05567000
PCCAPSAV EQU   PCCA+24                                                  05568000
PCCATMST EQU   PCCA+128                                                 05569000
PCCATMFL EQU   PCCATMST                                                 05570000
PCCASYNC EQU   PCCATMFL                                                 05571000
PCCAVKIL EQU   PCCATMFL                                                 05572000
PCCAMCC  EQU   PCCATMFL                                                 05573000
PCCAMINT EQU   PCCATMFL                                                 05574000
PCCATODE EQU   PCCATMST+1                                               05575000
PCCANUTD EQU   PCCATODE                                                 05576000
PCCANFTD EQU   PCCATODE                                                 05577000
PCCACCE  EQU   PCCATMST+2                                               05578000
PCCANUCC EQU   PCCACCE                                                  05579000
PCCANFCC EQU   PCCACCE                                                  05580000
PCCAINTE EQU   PCCATMST+3                                               05581000
PCCANUIN EQU   PCCAINTE                                                 05582000
PCCANFIN EQU   PCCAINTE                                                 05583000
PCCAEMSB EQU   PCCA+136                                                 05584000
PCCAEMSI EQU   PCCAEMSB                                                 05585000
PCCARISP EQU   PCCAEMSI                                                 05586000
PCCAEMS2 EQU   PCCAEMSI+1                                               05587000
PCCAEMS3 EQU   PCCAEMSI+2                                               05588000
PCCARMSB EQU   PCCAEMSI+3                                               05589000
PCCACHAN EQU   PCCA+224                                                 05590000
PCCAWERP EQU   PCCA+280                                                 05591000
PCCACHPF EQU   PCCAWERP+4                                               05592000
PCCACHBL EQU   PCCAWERP+5                                               05593000
PCCACHVA EQU   PCCAWERP+6                                               05594000
PCCACHTS EQU   PCCAWERP+7                                               05595000
PCCACHS1 EQU   PCCA+288                                                 05596000
PCCACHS2 EQU   PCCA+289                                                 05597000
PCCACHRB EQU   PCCA+290                                                 05598000
PCCACHF1 EQU   PCCA+308                                                 05599000
PCCACHF2 EQU   PCCA+309                                                 05600000
PCCACHF3 EQU   PCCA+310                                                 05601000
PCCACHF4 EQU   PCCA+311                                                 05602000
PCCAATTR EQU   PCCA+376                                                 05603000
PCCAVT   EQU   0                                                        05604000
PCCAT00P EQU   PCCAVT                                                   05605000
TQE      EQU   0                                                        05606000
TQEFLGS  EQU   TQE+14                                                   05607000
TQEFLGS2 EQU   TQE+15                                                   05608000
TQECOMP  EQU   TQEFLGS2                                                 05609000
TQEVAL   EQU   TQE+16                                                   05610000
TQEVALLH EQU   TQEVAL                                                   05611000
TQEVALRH EQU   TQEVAL+4                                                 05612000
TQEEXIT  EQU   TQE+28                                                   05613000
TQESRB   EQU   TQE+44                                                   05614000
TQEFLGS3 EQU   TQE+88                                                   05615000
TQESTCK  EQU   TQE+120                                                  05616000
CVTMAP   EQU   0                                                        05617000
CVT      EQU   CVTMAP                                                   05618000
CVTDATE  EQU   CVTMAP+56                                                05619000
CVTDAR   EQU   CVTMAP+72                                                05620000
CVTFLGS1 EQU   CVTDAR                                                   05621000
CVTEXIT  EQU   CVTMAP+80                                                05622000
CVTTPC   EQU   CVTMAP+88                                                05623000
CVTQTE00 EQU   CVTMAP+104                                               05624000
CVTQTD00 EQU   CVTMAP+108                                               05625000
CVTDCB   EQU   CVTMAP+116                                               05626000
CVTIOQET EQU   CVTMAP+120                                               05627000
CVTIERLC EQU   CVTMAP+144                                               05628000
CVTMSER  EQU   CVTMAP+148                                               05629000
CVTHEAD  EQU   CVTMAP+160                                               05630000
CVTSV76C EQU   CVTHEAD                                                  05631000
CVTOPTA  EQU   CVTMAP+182                                               05632000
CVTOPTB  EQU   CVTMAP+183                                               05633000
CVTGTF   EQU   CVTMAP+236                                               05634000
CVTGTFST EQU   CVTGTF                                                   05635000
CVTGTFS  EQU   CVTGTFST                                                 05636000
CVTSTATE EQU   CVTGTFST                                                 05637000
CVTTMODE EQU   CVTGTFST                                                 05638000
CVTFORM  EQU   CVTGTFST                                                 05639000
CVTAQAVT EQU   CVTMAP+240                                               05640000
CVTTCMFG EQU   CVTAQAVT                                                 05641000
CVTVOLM2 EQU   CVTMAP+244                                               05642000
CVTTATA  EQU   CVTVOLM2                                                 05643000
CVTTSKS  EQU   CVTTATA                                                  05644000
CVTVOLF2 EQU   CVTTSKS                                                  05645000
CVTTAT   EQU   CVTTATA+1                                                05646000
CVTATER  EQU   CVTMAP+248                                               05647000
CVTEXT1  EQU   CVTMAP+252                                               05648000
CVTPURG  EQU   CVTMAP+260                                               05649000
CVTQMSG  EQU   CVTMAP+268                                               05650000
CVTDMSR  EQU   CVTMAP+272                                               05651000
CVTRSV37 EQU   CVTDMSR                                                  05652000
CVTDMSRF EQU   CVTRSV37                                                 05653000
CVTTZ    EQU   CVTMAP+304                                               05654000
CVTERPV  EQU   CVTMAP+316                                               05655000
CVTINTLA EQU   CVTMAP+320                                               05656000
CVTAPF   EQU   CVTMAP+324                                               05657000
CVTEXT2  EQU   CVTMAP+328                                               05658000
CVTHJES  EQU   CVTMAP+332                                               05659000
CVTPGSIA EQU   CVTMAP+348                                               05660000
CVTA1F1  EQU   CVTMAP+356                                               05661000
CVTSYSK  EQU   CVTMAP+357                                               05662000
CVTVOLM1 EQU   CVTMAP+380                                               05663000
CVTVOLF1 EQU   CVTVOLM1                                                 05664000
CVTXTNT1 EQU   0                                                        05665000
CVTXTNT2 EQU   0                                                        05666000
CVTDSSV  EQU   CVTXTNT2                                                 05667000
CVTFLGBT EQU   CVTXTNT2+5                                               05668000
CVTQID   EQU   CVTXTNT2+24                                              05669000
CVTRV400 EQU   CVTXTNT2+52                                              05670000
CVTRV409 EQU   CVTXTNT2+53                                              05671000
CVTATCVT EQU   CVTXTNT2+64                                              05672000
CVTRV429 EQU   CVTXTNT2+84                                              05673000
CVTRV438 EQU   CVTXTNT2+85                                              05674000
CVTRV457 EQU   CVTXTNT2+112                                             05675000
CVTRV466 EQU   CVTXTNT2+113                                             05676000
CVTFIX   EQU   0                                                        05677000
CVTRELNO EQU   CVTFIX+252                                               05678000
SDWA     EQU   0                                                        05679000
SDWAPARM EQU   SDWA                                                     05680000
SDWAFIOB EQU   SDWA+4                                                   05681000
SDWAABCC EQU   SDWAFIOB                                                 05682000
SDWACMPF EQU   SDWAABCC                                                 05683000
SDWAREQ  EQU   SDWACMPF                                                 05684000
SDWACTL1 EQU   SDWA+8                                                   05685000
SDWACMKA EQU   SDWACTL1                                                 05686000
SDWAMWPA EQU   SDWACTL1+1                                               05687000
SDWAPMKA EQU   SDWACTL1+4                                               05688000
SDWACTL2 EQU   SDWA+16                                                  05689000
SDWACMKP EQU   SDWACTL2                                                 05690000
SDWAMWPP EQU   SDWACTL2+1                                               05691000
SDWAPMKP EQU   SDWACTL2+4                                               05692000
SDWAGRSV EQU   SDWA+24                                                  05693000
SDWANAME EQU   SDWA+88                                                  05694000
SDWAEC1  EQU   SDWA+104                                                 05695000
SDWAEMK1 EQU   SDWAEC1                                                  05696000
SDWAMWP1 EQU   SDWAEC1+1                                                05697000
SDWAINT1 EQU   SDWAEC1+2                                                05698000
SDWANXT1 EQU   SDWAEC1+4                                                05699000
SDWAAEC1 EQU   SDWA+112                                                 05700000
SDWAILC1 EQU   SDWAAEC1+1                                               05701000
SDWAINC1 EQU   SDWAAEC1+2                                               05702000
SDWAICD1 EQU   SDWAINC1+1                                               05703000
SDWAEC2  EQU   SDWA+120                                                 05704000
SDWAEMK2 EQU   SDWAEC2                                                  05705000
SDWAMWP2 EQU   SDWAEC2+1                                                05706000
SDWAINT2 EQU   SDWAEC2+2                                                05707000
SDWANXT2 EQU   SDWAEC2+4                                                05708000
SDWAAEC2 EQU   SDWA+128                                                 05709000
SDWAILC2 EQU   SDWAAEC2+1                                               05710000
SDWAINC2 EQU   SDWAAEC2+2                                               05711000
SDWAICD2 EQU   SDWAINC2+1                                               05712000
SDWASRSV EQU   SDWA+136                                                 05713000
SDWAIDNT EQU   SDWA+200                                                 05714000
SDWAMCH  EQU   SDWA+204                                                 05715000
SDWASTCK EQU   SDWAMCH                                                  05716000
SDWAMCHI EQU   SDWAMCH+8                                                05717000
SDWAMCHS EQU   SDWAMCHI                                                 05718000
SDWAMCHD EQU   SDWAMCHI+1                                               05719000
SDWARSR1 EQU   SDWAMCH+12                                               05720000
SDWARSR2 EQU   SDWAMCH+13                                               05721000
SDWAFLGS EQU   SDWA+232                                                 05722000
SDWAERRA EQU   SDWAFLGS                                                 05723000
SDWAERRB EQU   SDWAFLGS+1                                               05724000
SDWAERRC EQU   SDWAFLGS+2                                               05725000
SDWAERRD EQU   SDWAFLGS+3                                               05726000
SDWAIOFS EQU   SDWA+238                                                 05727000
SDWARTYA EQU   SDWA+240                                                 05728000
SDWACPUA EQU   SDWA+248                                                 05729000
SDWAPARQ EQU   SDWA+252                                                 05730000
SDWARCDE EQU   SDWAPARQ                                                 05731000
SDWAACF2 EQU   SDWAPARQ+1                                               05732000
SDWAFREE EQU   SDWAACF2                                                 05733000
SDWAACF3 EQU   SDWAPARQ+2                                               05734000
SDWAACF4 EQU   SDWAPARQ+3                                               05735000
SDWALKWA EQU   SDWA+256                                                 05736000
SDWALKWS EQU   SDWALKWA                                                 05737000
SDWARECP EQU   SDWA+292                                                 05738000
SDWASNPA EQU   SDWA+320                                                 05739000
SDWADUMP EQU   SDWASNPA                                                 05740000
SDWADPFS EQU   SDWADUMP+1                                               05741000
SDWADDAT EQU   SDWASNPA+4                                               05742000
SDWASDAT EQU   SDWADDAT                                                 05743000
SDWASDA0 EQU   SDWASDAT                                                 05744000
SDWASDA1 EQU   SDWASDAT+1                                               05745000
SDWAPDAT EQU   SDWADDAT+2                                               05746000
SDWADPSA EQU   SDWA+328                                                 05747000
SDWADPSL EQU   SDWADPSA                                                 05748000
SDWARA   EQU   SDWA+400                                                 05749000
SDWADPVA EQU   SDWARA+2                                                 05750000
TCWA     EQU   0                                                        05751000
TCWACVAL EQU   TCWA                                                     05752000
TCWACVL  EQU   TCWACVAL                                                 05753000
TCWACVR  EQU   TCWACVAL+4                                               05754000
TCWAWRK  EQU   TCWA+8                                                   05755000
TCWAWRK1 EQU   TCWAWRK                                                  05756000
TCWAA1   EQU   TCWAWRK1                                                 05757000
TCWAA1L  EQU   TCWAA1                                                   05758000
TCWAA1R  EQU   TCWAA1+4                                                 05759000
TCWAWRK2 EQU   TCWAWRK+8                                                05760000
TCWAA2   EQU   TCWAWRK2                                                 05761000
TCWAA2L  EQU   TCWAA2                                                   05762000
TCWAA2R  EQU   TCWAA2+4                                                 05763000
TCWAPGT  EQU   TCWA+24                                                  05764000
TCWAPGD  EQU   TCWA+28                                                  05765000
TCWAPLT  EQU   TCWA+32                                                  05766000
TCWAPLD  EQU   TCWA+36                                                  05767000
TCWAPTP  EQU   TCWA+40                                                  05768000
TCWAPDP  EQU   TCWA+44                                                  05769000
TCWACVP  EQU   TCWA+48                                                  05770000
TCWAGFLG EQU   TCWA+52                                                  05771000
TCGXXX   EQU   TCWAGFLG                                                 05772000
TCGYYY   EQU   TCWAGFLG                                                 05773000
TCGZZZ   EQU   TCWAGFLG                                                 05774000
TCGNSET  EQU   TCWAGFLG                                                 05775000
TCGLELD  EQU   TCWAGFLG                                                 05776000
TCGCVTD  EQU   TCWAGFLG                                                 05777000
TCGTQE   EQU   TCWAGFLG                                                 05778000
TCCPUCNT EQU   TCWA+54                                                  05779000
TCWARPLY EQU   TCWA+56                                                  05780000
TCWARLEN EQU   TCWARPLY                                                 05781000
TCWASET  EQU   TCWARPLY+4                                               05782000
TCWATXT  EQU   TCWARPLY+8                                               05783000
TCWACTL  EQU   TCWA+104                                                 05784000
TCSTOP   EQU   TCWACTL                                                  05785000
TCSYNCK  EQU   TCWACTL+2                                                05786000
TCELDAYS EQU   TCWA+108                                                 05787000
TCENTRY  EQU   0                                                        05788000
TCWACLKE EQU   TCENTRY                                                  05789000
TCWACLKL EQU   TCWACLKE                                                 05790000
TCWACLKR EQU   TCWACLKE+4                                               05791000
TCWAPCCA EQU   TCENTRY+8                                                05792000
TCWAIADD EQU   TCENTRY+12                                               05793000
TCWALFLG EQU   TCENTRY+15                                               05794000
TCLCC    EQU   TCWALFLG                                                 05795000
I256C    EQU   0                                                        05796000
I001C    EQU   0                                                        05797000
I008P    EQU   0                                                        05798000
I015F    EQU   0                                                        05799000
I015P    EQU   0                                                        05800000
I031F    EQU   0                                                        05801000
I031P    EQU   0                                                        05802000
MB808    EQU   0                                                        05803000
MB809    EQU   0                                                        05804000
NEWPCCA  EQU   0                                                        05805000
PARMTIME EQU   0                                                        05806000
PARMDATE EQU   0                                                        05807000
CONVALUE EQU   0                                                        05808000
LO32BITS EQU   CONVALUE+4                                               05809000
OLDPCCA  EQU   0                                                        05810000
SECSMIN  EQU   MINSHOUR                                                 05811000
ASCBPTR  EQU   ASVTENTY                                                 05812000
TPCPTR   EQU   CVTTPC                                                   05813000
CVTS01   EQU   CVTPGSIA                                                 05814000
CVTLPDIA EQU   CVTS01+12                                                05815000
CVTDIRST EQU   CVTLPDIA                                                 05816000
CVTSLIDA EQU   CVTS01+24                                                05817000
CVTCTLFG EQU   CVTS01+50                                                05818000
CVTASVT  EQU   CVTS01+208                                               05819000
CVTCSD   EQU   CVTS01+312                                               05820000
CVTIPCRI EQU   CVTS01+408                                               05821000
CVTPCCAT EQU   CVTS01+416                                               05822000
CVTLCCAT EQU   CVTS01+420                                               05823000
CVTRV210 EQU   CVTS01+424                                               05824000
CVTRV219 EQU   CVTS01+425                                               05825000
CVTRV228 EQU   CVTS01+426                                               05826000
CVTRV237 EQU   CVTS01+427                                               05827000
CVTMFRTR EQU   CVTS01+452                                               05828000
CVTRV262 EQU   CVTS01+468                                               05829000
CVTRV271 EQU   CVTS01+469                                               05830000
CVTRV280 EQU   CVTS01+470                                               05831000
CVTRV289 EQU   CVTS01+471                                               05832000
CVTGSDA  EQU   CVTS01+600                                               05833000
ASVTPTR  EQU   CVTASVT                                                  05834000
CLASIUTC EQU   SRBPARM                                                  05835000
CLATC    EQU   CLASIUTC+3                                               05836000
PSARSVTE EQU   PSARSVT                                                  05837000
TEXTZZZ  EQU   WPLTXT                                                   05838000
CPUADDRZ EQU   TEXTZZZ+15                                               05839000
DATEZ    EQU   TEXTZZZ+28                                               05840000
CLOCKZ   EQU   TEXTZZZ+43                                          @L01 05841000
TEXTXXX1 EQU   WPLTXT                                                   05842000
DATEX1   EQU   TEXTXXX1+21                                              05843000
CLOCKX1  EQU   TEXTXXX1+36                                         @L01 05844000
TEXTXXX2 EQU   WPLTXT                                                   05845000
DATEX2   EQU   TEXTXXX2+18                                              05846000
CLOCKX2  EQU   TEXTXXX2+33                                         @L01 05847000
TEXTERR  EQU   WPLTXT                                                   05848000
CPUADDRE EQU   TEXTERR+12                                               05849000
FLC      EQU   PSA                                                      05850000
TQEECB   EQU   TQEEXIT                                                  05851000
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS    05852000
@NM00181 EQU   TEXTERR                                                  05853000
@NM00180 EQU   TEXTXXX2+25                                              05854000
@NM00179 EQU   TEXTXXX2                                                 05855000
@NM00178 EQU   TEXTXXX1+28                                              05856000
@NM00177 EQU   TEXTXXX1                                                 05857000
@NM00176 EQU   TEXTZZZ+35                                               05858000
@NM00175 EQU   TEXTZZZ+17                                               05859000
@NM00174 EQU   TEXTZZZ                                                  05860000
PSARSAV  EQU   PSARSVTE+60                                              05861000
PSARSTK  EQU   PSARSVTE+56                                              05862000
PSAESAV3 EQU   PSARSVTE+52                                              05863000
PSAESTK3 EQU   PSARSVTE+48                                              05864000
PSAESAV2 EQU   PSARSVTE+44                                              05865000
PSAESTK2 EQU   PSARSVTE+40                                              05866000
PSAESAV1 EQU   PSARSVTE+36                                              05867000
PSAESTK1 EQU   PSARSVTE+32                                              05868000
PSAPSAV  EQU   PSARSVTE+28                                              05869000
PSAPSTK  EQU   PSARSVTE+24                                              05870000
PSAMSAV  EQU   PSARSVTE+20                                              05871000
PSAMSTK  EQU   PSARSVTE+16                                              05872000
PSASSAV  EQU   PSARSVTE+12                                              05873000
PSASSTK  EQU   PSARSVTE+8                                               05874000
PSANSTK  EQU   PSARSVTE+4                                               05875000
PSACSTK  EQU   PSARSVTE                                                 05876000
CLASIU   EQU   CLASIUTC+3                                               05877000
@NM00002 EQU   CLASIUTC                                                 05878000
CVTRV628 EQU   CVTS01+728                                               05879000
CVTRV627 EQU   CVTS01+724                                               05880000
CVTRV626 EQU   CVTS01+720                                               05881000
CVTRV625 EQU   CVTS01+716                                               05882000
CVTRV624 EQU   CVTS01+712                                               05883000
CVTRV623 EQU   CVTS01+708                                               05884000
CVTCST   EQU   CVTS01+704                                               05885000
CVTVDCCR EQU   CVTS01+700                                               05886000
CVTIHASU EQU   CVTS01+696                                               05887000
CVTSUSP  EQU   CVTS01+692                                               05888000
CVTT6SVC EQU   CVTS01+688                                               05889000
CVTCDAL  EQU   CVTS01+684                                               05890000
CVTTCTL  EQU   CVTS01+680                                               05891000
CVTRSUME EQU   CVTS01+676                                               05892000
CVTJTERM EQU   CVTS01+672                                               05893000
CVTASMRM EQU   CVTS01+668                                               05894000
CVTTCASP EQU   CVTS01+664                                               05895000
CVT0PT03 EQU   CVTS01+660                                               05896000
CVT0PT0E EQU   CVTS01+656                                               05897000
CVTRV609 EQU   CVTS01+652                                               05898000
CVTCGK   EQU   CVTS01+648                                               05899000
CVTRAC   EQU   CVTS01+644                                               05900000
CVTHSM   EQU   CVTS01+640                                               05901000
CVTRV605 EQU   CVTS01+636                                               05902000
CVTRV604 EQU   CVTS01+632                                               05903000
CVTEFF02 EQU   CVTS01+628                                               05904000
CVTCBBR  EQU   CVTS01+624                                               05905000
CVTSSCR  EQU   CVTS01+620                                               05906000
CVTEVENT EQU   CVTS01+616                                               05907000
CVTCRCA  EQU   CVTS01+612                                               05908000
CVTTPIO  EQU   CVTS01+608                                               05909000
CVTADV   EQU   CVTS01+604                                               05910000
CVTGSDAB EQU   CVTGSDA                                                  05911000
CVTQV3   EQU   CVTS01+596                                               05912000
CVTQV2   EQU   CVTS01+592                                               05913000
CVTQV1   EQU   CVTS01+588                                               05914000
CVTRPT   EQU   CVTS01+584                                               05915000
CVTSSRB  EQU   CVTS01+580                                               05916000
CVTCSDRL EQU   CVTS01+576                                               05917000
CVTEXP1  EQU   CVTS01+572                                               05918000
CVTRMPMT EQU   CVTS01+568                                               05919000
CVTRMPTT EQU   CVTS01+564                                               05920000
CVTVPSA  EQU   CVTS01+560                                               05921000
CVTVSTOP EQU   CVTS01+556                                               05922000
CVTGTFR8 EQU   CVTS01+552                                               05923000
CVTQUIT  EQU   CVTS01+548                                               05924000
CVTVACR  EQU   CVTS01+544                                               05925000
CVTWTCB  EQU   CVTS01+540                                               05926000
CVTSTPRS EQU   CVTS01+536                                               05927000
CVT0PT02 EQU   CVTS01+532                                               05928000
CVTDARCM EQU   CVTS01+528                                               05929000
CVTIRECM EQU   CVTS01+524                                               05930000
CVTJRECM EQU   CVTS01+520                                               05931000
CVTVEMS0 EQU   CVTS01+516                                               05932000
CVTSPFRR EQU   CVTS01+512                                               05933000
CVTRLSTG EQU   CVTS01+508                                               05934000
CVT0TC0A EQU   CVTS01+504                                               05935000
CVTGMBR  EQU   CVTS01+500                                               05936000
CVTLFRM  EQU   CVTS01+496                                               05937000
CVTRMBR  EQU   CVTS01+492                                               05938000
CVTVIOP  EQU   CVTS01+488                                               05939000
CVTRV307 EQU   CVTS01+486                                               05940000
CVTRV306 EQU   CVTS01+484                                               05941000
CVTRV305 EQU   CVTS01+482                                               05942000
CVTRV304 EQU   CVTS01+480                                               05943000
CVTRV303 EQU   CVTS01+478                                               05944000
CVTRV302 EQU   CVTS01+476                                               05945000
CVTTRCA  EQU   CVTS01+472                                               05946000
CVTRV297 EQU   CVTRV289                                                 05947000
CVTRV296 EQU   CVTRV289                                                 05948000
CVTRV295 EQU   CVTRV289                                                 05949000
CVTRV294 EQU   CVTRV289                                                 05950000
CVTRV293 EQU   CVTRV289                                                 05951000
CVTRV292 EQU   CVTRV289                                                 05952000
CVTRV291 EQU   CVTRV289                                                 05953000
CVTRV290 EQU   CVTRV289                                                 05954000
CVTRV288 EQU   CVTRV280                                                 05955000
CVTRV287 EQU   CVTRV280                                                 05956000
CVTRV286 EQU   CVTRV280                                                 05957000
CVTRV285 EQU   CVTRV280                                                 05958000
CVTRV284 EQU   CVTRV280                                                 05959000
CVTRV283 EQU   CVTRV280                                                 05960000
CVTRV282 EQU   CVTRV280                                                 05961000
CVTRV281 EQU   CVTRV280                                                 05962000
CVTRV279 EQU   CVTRV271                                                 05963000
CVTRV278 EQU   CVTRV271                                                 05964000
CVTRV277 EQU   CVTRV271                                                 05965000
CVTRV276 EQU   CVTRV271                                                 05966000
CVTRV275 EQU   CVTRV271                                                 05967000
CVTRV274 EQU   CVTRV271                                                 05968000
CVTRV273 EQU   CVTRV271                                                 05969000
CVTRV272 EQU   CVTRV271                                                 05970000
CVTRV270 EQU   CVTRV262                                                 05971000
CVTRV269 EQU   CVTRV262                                                 05972000
CVTRV268 EQU   CVTRV262                                                 05973000
CVTRV267 EQU   CVTRV262                                                 05974000
CVTRV266 EQU   CVTRV262                                                 05975000
CVTRV265 EQU   CVTRV262                                                 05976000
CVTRV264 EQU   CVTRV262                                                 05977000
CVTRV263 EQU   CVTRV262                                                 05978000
CVTVFP   EQU   CVTS01+464                                               05979000
CVTVSI   EQU   CVTS01+460                                               05980000
CVTVPSIB EQU   CVTS01+456                                               05981000
CVTMFACT EQU   CVTMFRTR                                                 05982000
CVTMFCTL EQU   CVTS01+448                                               05983000
CVTPVBP  EQU   CVTS01+444                                               05984000
CVTPWI   EQU   CVTS01+440                                               05985000
CVTRV254 EQU   CVTS01+438                                               05986000
CVTRV253 EQU   CVTS01+436                                               05987000
CVTRV252 EQU   CVTS01+434                                               05988000
CVTRV251 EQU   CVTS01+433                                               05989000
CVTRV250 EQU   CVTS01+432                                               05990000
CVTRV249 EQU   CVTS01+431                                               05991000
CVTRV248 EQU   CVTS01+430                                               05992000
CVTRV247 EQU   CVTS01+429                                               05993000
CVTRV246 EQU   CVTS01+428                                               05994000
CVTRV245 EQU   CVTRV237                                                 05995000
CVTRV244 EQU   CVTRV237                                                 05996000
CVTRV243 EQU   CVTRV237                                                 05997000
CVTRV242 EQU   CVTRV237                                                 05998000
CVTRV241 EQU   CVTRV237                                                 05999000
CVTRV240 EQU   CVTRV237                                                 06000000
CVTRV239 EQU   CVTRV237                                                 06001000
CVTRV238 EQU   CVTRV237                                                 06002000
CVTRV236 EQU   CVTRV228                                                 06003000
CVTRV235 EQU   CVTRV228                                                 06004000
CVTRV234 EQU   CVTRV228                                                 06005000
CVTRV233 EQU   CVTRV228                                                 06006000
CVTRV232 EQU   CVTRV228                                                 06007000
CVTRV231 EQU   CVTRV228                                                 06008000
CVTRV230 EQU   CVTRV228                                                 06009000
CVTRV229 EQU   CVTRV228                                                 06010000
CVTRV227 EQU   CVTRV219                                                 06011000
CVTRV226 EQU   CVTRV219                                                 06012000
CVTRV225 EQU   CVTRV219                                                 06013000
CVTRV224 EQU   CVTRV219                                                 06014000
CVTRV223 EQU   CVTRV219                                                 06015000
CVTRV222 EQU   CVTRV219                                                 06016000
CVTRV221 EQU   CVTRV219                                                 06017000
CVTRV220 EQU   CVTRV219                                                 06018000
CVTRV218 EQU   CVTRV210                                                 06019000
CVTRV217 EQU   CVTRV210                                                 06020000
CVTRV216 EQU   CVTRV210                                                 06021000
CVTRV215 EQU   CVTRV210                                                 06022000
CVTRV214 EQU   CVTRV210                                                 06023000
CVTRV213 EQU   CVTRV210                                                 06024000
CVTRV212 EQU   CVTRV210                                                 06025000
CVTRV211 EQU   CVTRV210                                                 06026000
CVTIPCRP EQU   CVTS01+412                                               06027000
CVTIPCDS EQU   CVTS01+404                                               06028000
CVTAIDVT EQU   CVTS01+400                                               06029000
CVTSSAP  EQU   CVTS01+396                                               06030000
CVTEHCIR EQU   CVTS01+392                                               06031000
CVTEHDEF EQU   CVTS01+388                                               06032000
CVTDAIR  EQU   CVTS01+384                                               06033000
CVTPERFM EQU   CVTS01+380                                               06034000
CVT044R2 EQU   CVTS01+376                                               06035000
CVTFETCH EQU   CVTS01+372                                               06036000
CVTRSTWD EQU   CVTS01+368                                               06037000
CVTSPOST EQU   CVTS01+364                                               06038000
CVTIOBP  EQU   CVTS01+360                                               06039000
CVTASMVT EQU   CVTS01+356                                               06040000
CVTRECRQ EQU   CVTS01+352                                               06041000
CVTWSAC  EQU   CVTS01+348                                               06042000
CVTWSAG  EQU   CVTS01+344                                               06043000
CVTWSAL  EQU   CVTS01+340                                               06044000
CVTSPSA  EQU   CVTS01+336                                               06045000
CVTGLMN  EQU   CVTS01+332                                               06046000
CVTVEAC0 EQU   CVTS01+328                                               06047000
CVT062R1 EQU   CVTS01+324                                               06048000
CVTRPOST EQU   CVTS01+320                                               06049000
CVTDQIQE EQU   CVTS01+316                                               06050000
CVTLKRMA EQU   CVTS01+308                                               06051000
CVTRSPIE EQU   CVTS01+304                                               06052000
CVTRENQ  EQU   CVTS01+300                                               06053000
CVTLQCB  EQU   CVTS01+296                                               06054000
CVTFQCB  EQU   CVTS01+292                                               06055000
CVTQCS01 EQU   CVTS01+288                                               06056000
CVTAPFT  EQU   CVTS01+284                                               06057000
CVTPARRL EQU   CVTS01+280                                               06058000
CVTVWAIT EQU   CVTS01+276                                               06059000
CVTGSPL  EQU   CVTS01+272                                               06060000
CVTLSMQ  EQU   CVTS01+268                                               06061000
CVTGSMQ  EQU   CVTS01+264                                               06062000
CVTEXPRO EQU   CVTS01+260                                               06063000
CVTOPCTP EQU   CVTS01+256                                               06064000
CVTSIC   EQU   CVTS01+252                                               06065000
CVTTPIOS EQU   CVTS01+248                                               06066000
CVTRTMS  EQU   CVTS01+244                                               06067000
CVTSDBF  EQU   CVTS01+240                                               06068000
CVTSCBP  EQU   CVTS01+236                                               06069000
CVTSDMP  EQU   CVTS01+232                                               06070000
CVTSV60  EQU   CVTS01+228                                               06071000
CVTRTMCT EQU   CVTS01+224                                               06072000
CVTASCBL EQU   CVTS01+220                                               06073000
CVTASCBH EQU   CVTS01+216                                               06074000
CVTGDA   EQU   CVTS01+212                                               06075000
CVTVVMDI EQU   CVTS01+204                                               06076000
CVTAQTOP EQU   CVTS01+200                                               06077000
CVTIOSCS EQU   CVTS01+196                                               06078000
CVTSDRM  EQU   CVTS01+192                                               06079000
CVTOPTE  EQU   CVTS01+188                                               06080000
CVTSTXU  EQU   CVTS01+184                                               06081000
CVTQUIS  EQU   CVTS01+180                                               06082000
CVTPARS  EQU   CVTS01+176                                               06083000
CVTS1EE  EQU   CVTS01+172                                               06084000
CVTFRAS  EQU   CVTS01+168                                               06085000
CVTQSAS  EQU   CVTS01+164                                               06086000
CVTCRAS  EQU   CVTS01+160                                               06087000
CVTCRMN  EQU   CVTS01+156                                               06088000
CVTDELCP EQU   CVTS01+152                                               06089000
CVTFRECL EQU   CVTS01+148                                               06090000
CVTGETCL EQU   CVTS01+144                                               06091000
CVTBLDCP EQU   CVTS01+140                                               06092000
CVTAUTHL EQU   CVTS01+136                                               06093000
CVTSCAN  EQU   CVTS01+132                                               06094000
CVTRV144 EQU   CVTS01+130                                               06095000
CVTMAXMP EQU   CVTS01+128                                               06096000
CVTSTCK  EQU   CVTS01+124                                               06097000
CVTRV139 EQU   CVTS01+123                                               06098000
CVTDSSAC EQU   CVTS01+122                                               06099000
CVTRV513 EQU   CVTS01+121                                               06100000
CVTIOSPL EQU   CVTS01+120                                               06101000
CVTPTGT  EQU   CVTS01+116                                               06102000
CVTCSPIE EQU   CVTS01+112                                               06103000
CVTSMFEX EQU   CVTS01+108                                               06104000
CVTOLT0A EQU   CVTS01+104                                               06105000
CVTSRBRT EQU   CVTS01+100                                               06106000
CVTPUTL  EQU   CVTS01+96                                                06107000
CVTASCRL EQU   CVTS01+92                                                06108000
CVTASCRF EQU   CVTS01+88                                                06109000
CVTRV326 EQU   CVTS01+84                                                06110000
CVTRV325 EQU   CVTS01+80                                                06111000
CVTRV324 EQU   CVTS01+76                                                06112000
CVT0VL01 EQU   CVTS01+72                                                06113000
CVTSHRVM EQU   CVTS01+68                                                06114000
CVTRV332 EQU   CVTS01+64                                                06115000
CVTTAS   EQU   CVTS01+60                                                06116000
CVTRSCN  EQU   CVTS01+56                                                06117000
CVTTRAC2 EQU   CVTS01+54                                                06118000
CVTTRACE EQU   CVTS01+52                                                06119000
CVTAPG   EQU   CVTS01+51                                                06120000
CVTSDTRC EQU   CVTCTLFG                                                 06121000
CVTGTRCE EQU   CVTCTLFG                                                 06122000
CVTNOMP  EQU   CVTCTLFG                                                 06123000
CVTRSV79 EQU   CVTCTLFG                                                 06124000
CVTDSTAT EQU   CVTCTLFG                                                 06125000
CVTRSV78 EQU   CVTCTLFG                                                 06126000
CVTRV333 EQU   CVTCTLFG                                                 06127000
CVTRV323 EQU   CVTCTLFG                                                 06128000
CVTSPVLK EQU   CVTS01+49                                                06129000
CVTEXSNL EQU   CVTS01+48                                                06130000
CVTEXSNR EQU   CVTS01+44                                                06131000
CVTEXSLF EQU   CVTS01+40                                                06132000
CVTVLDWT EQU   CVTS01+36                                                06133000
CVTRV328 EQU   CVTS01+32                                                06134000
CVTRV322 EQU   CVTS01+28                                                06135000
CVTSLID  EQU   CVTSLIDA+1                                               06136000
CVTSYLK  EQU   CVTSLIDA                                                 06137000
CVTRV321 EQU   CVTS01+20                                                06138000
CVTRV320 EQU   CVTS01+16                                                06139000
CVTLPDIR EQU   CVTLPDIA+1                                               06140000
CVTRSV69 EQU   CVTDIRST                                                 06141000
CVTRSV68 EQU   CVTDIRST                                                 06142000
CVTRSV67 EQU   CVTDIRST                                                 06143000
CVTRSV66 EQU   CVTDIRST                                                 06144000
CVTRSV65 EQU   CVTDIRST                                                 06145000
CVTRSV64 EQU   CVTDIRST                                                 06146000
CVTRSV63 EQU   CVTDIRST                                                 06147000
CVTDICOM EQU   CVTDIRST                                                 06148000
CVTPVTP  EQU   CVTS01+8                                                 06149000
CVTLPDSR EQU   CVTS01+4                                                 06150000
CVTGETL  EQU   CVTS01                                                   06151000
HO32BITS EQU   CONVALUE                                                 06152000
@NM00173 EQU   TCWALFLG                                                 06153000
@NM00172 EQU   TCWALFLG                                                 06154000
@NM00171 EQU   TCENTRY+14                                               06155000
@NM00170 EQU   TCWACTL+2                                                06156000
@NM00169 EQU   TCWACTL                                                  06157000
@NM00168 EQU   TCWACTL                                                  06158000
@NM00167 EQU   TCWARPLY+2                                               06159000
@NM00166 EQU   TCWA+53                                                  06160000
@NM00165 EQU   TCWAGFLG                                                 06161000
@NM00164 EQU   SDWA+512                                                 06162000
SDWAVRA  EQU   SDWARA+4                                                 06163000
SDWAURAL EQU   SDWARA+3                                                 06164000
@NM00163 EQU   SDWADPVA                                                 06165000
SDWAEBC  EQU   SDWADPVA                                                 06166000
SDWAHEX  EQU   SDWADPVA                                                 06167000
SDWAVRAL EQU   SDWARA                                                   06168000
SDWAERTM EQU   SDWA+396                                                 06169000
SDWACOMP EQU   SDWA+392                                                 06170000
SDWARCPL EQU   SDWA+364                                                 06171000
@NM00162 EQU   SDWADPSA+32                                              06172000
SDWATO4  EQU   SDWADPSL+28                                              06173000
SDWAFRM4 EQU   SDWADPSL+24                                              06174000
SDWATO3  EQU   SDWADPSL+20                                              06175000
SDWAFRM3 EQU   SDWADPSL+16                                              06176000
SDWATO2  EQU   SDWADPSL+12                                              06177000
SDWAFRM2 EQU   SDWADPSL+8                                               06178000
SDWATO1  EQU   SDWADPSL+4                                               06179000
SDWAFRM1 EQU   SDWADPSL                                                 06180000
@NM00161 EQU   SDWADDAT+3                                               06181000
@NM00160 EQU   SDWAPDAT                                                 06182000
SDWAUSPL EQU   SDWAPDAT                                                 06183000
SDWADPSW EQU   SDWAPDAT                                                 06184000
SDWATJPA EQU   SDWAPDAT                                                 06185000
SDWATLPA EQU   SDWAPDAT                                                 06186000
SDWADREG EQU   SDWAPDAT                                                 06187000
SDWADSAH EQU   SDWAPDAT                                                 06188000
SDWADSAS EQU   SDWAPDAT                                                 06189000
@NM00159 EQU   SDWASDA1                                                 06190000
SDWAERR  EQU   SDWASDA1                                                 06191000
SDWAIO   EQU   SDWASDA1                                                 06192000
SDWADM   EQU   SDWASDA0                                                 06193000
SDWAQQS  EQU   SDWASDA0                                                 06194000
SDWACBS  EQU   SDWASDA0                                                 06195000
SDWAGTF  EQU   SDWASDA0                                                 06196000
SDWASWA  EQU   SDWASDA0                                                 06197000
SDWALSQA EQU   SDWASDA0                                                 06198000
SDWASQA  EQU   SDWASDA0                                                 06199000
SDWANUC  EQU   SDWASDA0                                                 06200000
@NM00158 EQU   SDWADUMP+2                                               06201000
@NM00157 EQU   SDWADPFS                                                 06202000
SDWASLST EQU   SDWADPFS                                                 06203000
@NM00156 EQU   SDWADPFS                                                 06204000
SDWAENSN EQU   SDWADPFS                                                 06205000
SDWADLST EQU   SDWADPFS                                                 06206000
SDWADPT  EQU   SDWADPFS                                                 06207000
SDWADPID EQU   SDWADUMP                                                 06208000
SDWADPLA EQU   SDWA+316                                                 06209000
SDWAREXN EQU   SDWARECP+16                                              06210000
SDWACSCT EQU   SDWARECP+8                                               06211000
SDWAMODN EQU   SDWARECP                                                 06212000
SDWASEQ# EQU   SDWA+290                                                 06213000
SDWAASID EQU   SDWA+288                                                 06214000
SDWATALW EQU   SDWALKWS+28                                              06215000
SDWATDLW EQU   SDWALKWS+24                                              06216000
SDWATNLW EQU   SDWALKWS+20                                              06217000
SDWAAPLW EQU   SDWALKWS+16                                              06218000
SDWAIPLW EQU   SDWALKWS+12                                              06219000
SDWAILLW EQU   SDWALKWS+8                                               06220000
SDWAIULW EQU   SDWALKWS+4                                               06221000
SDWAICLW EQU   SDWALKWS                                                 06222000
SDWAFLLK EQU   SDWAACF4                                                 06223000
SDWACMS  EQU   SDWAACF4                                                 06224000
SDWAOPTM EQU   SDWAACF4                                                 06225000
SDWATADB EQU   SDWAACF4                                                 06226000
SDWATDNB EQU   SDWAACF4                                                 06227000
SDWATNCB EQU   SDWAACF4                                                 06228000
SDWAILCH EQU   SDWAACF4                                                 06229000
SDWAIUCB EQU   SDWAACF4                                                 06230000
SDWAICAT EQU   SDWAACF3                                                 06231000
SDWAIPRG EQU   SDWAACF3                                                 06232000
SDWASALL EQU   SDWAACF3                                                 06233000
SDWAASMP EQU   SDWAACF3                                                 06234000
SDWADISP EQU   SDWAACF3                                                 06235000
@NM00155 EQU   SDWAACF3                                                 06236000
@NM00154 EQU   SDWAACF2                                                 06237000
SDWAUPRG EQU   SDWAACF2                                                 06238000
@NM00153 EQU   SDWAACF2                                                 06239000
SDWASPIN EQU   SDWAACF2                                                 06240000
@NM00152 EQU   SDWAACF2                                                 06241000
SDWARCRD EQU   SDWAACF2                                                 06242000
SDWALCPU EQU   SDWACPUA+2                                               06243000
@NM00151 EQU   SDWACPUA                                                 06244000
SDWARECA EQU   SDWA+244                                                 06245000
SDWACPUI EQU   SDWA+239                                                 06246000
@NM00150 EQU   SDWAIOFS                                                 06247000
SDWANIOP EQU   SDWAIOFS                                                 06248000
SDWANOIO EQU   SDWAIOFS                                                 06249000
SDWAIOHT EQU   SDWAIOFS                                                 06250000
SDWAIOQR EQU   SDWAIOFS                                                 06251000
SDWAFMID EQU   SDWA+236                                                 06252000
SDWAERFL EQU   SDWAERRD                                                 06253000
SDWAMCIV EQU   SDWAERRD                                                 06254000
SDWARPIV EQU   SDWAERRD                                                 06255000
SDWAMABD EQU   SDWAERRD                                                 06256000
SDWACTS  EQU   SDWAERRD                                                 06257000
SDWASTAE EQU   SDWAERRD                                                 06258000
SDWANRBE EQU   SDWAERRD                                                 06259000
SDWACLUP EQU   SDWAERRD                                                 06260000
@NM00149 EQU   SDWAERRC                                                 06261000
SDWAEAS  EQU   SDWAERRC                                                 06262000
SDWAPERC EQU   SDWAERRC                                                 06263000
SDWAIRB  EQU   SDWAERRC                                                 06264000
SDWASTAI EQU   SDWAERRC                                                 06265000
SDWASTAF EQU   SDWAERRC                                                 06266000
SDWASRBM EQU   SDWAERRB                                                 06267000
SDWALDIS EQU   SDWAERRB                                                 06268000
SDWAENRB EQU   SDWAERRB                                                 06269000
SDWATYP1 EQU   SDWAERRB                                                 06270000
@NM00148 EQU   SDWAERRB                                                 06271000
SDWAPGIO EQU   SDWAERRA                                                 06272000
SDWATEXC EQU   SDWAERRA                                                 06273000
SDWASVCE EQU   SDWAERRA                                                 06274000
SDWAABTM EQU   SDWAERRA                                                 06275000
SDWASVCD EQU   SDWAERRA                                                 06276000
SDWARKEY EQU   SDWAERRA                                                 06277000
SDWAPCHK EQU   SDWAERRA                                                 06278000
SDWAMCHK EQU   SDWAERRA                                                 06279000
SDWATIME EQU   SDWAMCH+20                                               06280000
SDWARFSA EQU   SDWAMCH+16                                               06281000
@NM00147 EQU   SDWAMCH+14                                               06282000
SDWAVEQR EQU   SDWARSR2                                                 06283000
SDWAPGFX EQU   SDWARSR2                                                 06284000
SDWAFLSQ EQU   SDWARSR2                                                 06285000
SDWAFSQA EQU   SDWARSR2                                                 06286000
SDWANUCL EQU   SDWARSR2                                                 06287000
SDWASPER EQU   SDWARSR2                                                 06288000
SDWAINTC EQU   SDWARSR2                                                 06289000
SDWAOFLN EQU   SDWARSR2                                                 06290000
SDWACHNG EQU   SDWARSR1                                                 06291000
SDWAMSER EQU   SDWARSR1                                                 06292000
@NM00146 EQU   SDWARSR1                                                 06293000
SDWACPID EQU   SDWAMCH+10                                               06294000
SDWATERR EQU   SDWAMCHD                                                 06295000
SDWAFPRX EQU   SDWAMCHD                                                 06296000
SDWAINSF EQU   SDWAMCHD                                                 06297000
SDWAACR  EQU   SDWAMCHD                                                 06298000
SDWASCK  EQU   SDWAMCHD                                                 06299000
SDWAPSWU EQU   SDWAMCHD                                                 06300000
SDWAREGU EQU   SDWAMCHD                                                 06301000
SDWASKYF EQU   SDWAMCHD                                                 06302000
@NM00145 EQU   SDWAMCHS                                                 06303000
SDWARSRF EQU   SDWAMCHS                                                 06304000
SDWARSRC EQU   SDWAMCHS                                                 06305000
SDWAINVP EQU   SDWAMCHS                                                 06306000
SDWATSVL EQU   SDWAMCHS                                                 06307000
SDWARCDF EQU   SDWAMCHS                                                 06308000
SDWASRVL EQU   SDWAMCHS                                                 06309000
SDWASCKE EQU   SDWASTCK+4                                               06310000
SDWASCKB EQU   SDWASTCK                                                 06311000
SDWALNTH EQU   SDWAIDNT+1                                               06312000
SDWASPID EQU   SDWAIDNT                                                 06313000
SDWASR15 EQU   SDWASRSV+60                                              06314000
SDWASR14 EQU   SDWASRSV+56                                              06315000
SDWASR13 EQU   SDWASRSV+52                                              06316000
SDWASR12 EQU   SDWASRSV+48                                              06317000
SDWASR11 EQU   SDWASRSV+44                                              06318000
SDWASR10 EQU   SDWASRSV+40                                              06319000
SDWASR09 EQU   SDWASRSV+36                                              06320000
SDWASR08 EQU   SDWASRSV+32                                              06321000
SDWASR07 EQU   SDWASRSV+28                                              06322000
SDWASR06 EQU   SDWASRSV+24                                              06323000
SDWASR05 EQU   SDWASRSV+20                                              06324000
SDWASR04 EQU   SDWASRSV+16                                              06325000
SDWASR03 EQU   SDWASRSV+12                                              06326000
SDWASR02 EQU   SDWASRSV+8                                               06327000
SDWASR01 EQU   SDWASRSV+4                                               06328000
SDWASR00 EQU   SDWASRSV                                                 06329000
SDWATRN2 EQU   SDWAAEC2+4                                               06330000
SDWAIPC2 EQU   SDWAICD2                                                 06331000
SDWAIMC2 EQU   SDWAICD2                                                 06332000
SDWAIPR2 EQU   SDWAICD2                                                 06333000
@NM00144 EQU   SDWAINC2                                                 06334000
@NM00143 EQU   SDWAILC2                                                 06335000
SDWAIL2  EQU   SDWAILC2                                                 06336000
@NM00142 EQU   SDWAILC2                                                 06337000
@NM00141 EQU   SDWAAEC2                                                 06338000
SDWAADD2 EQU   SDWANXT2+1                                               06339000
@NM00140 EQU   SDWANXT2                                                 06340000
@NM00139 EQU   SDWAEC2+3                                                06341000
SDWASGN2 EQU   SDWAINT2                                                 06342000
SDWAEXP2 EQU   SDWAINT2                                                 06343000
SDWADEC2 EQU   SDWAINT2                                                 06344000
SDWAFPO2 EQU   SDWAINT2                                                 06345000
SDWACC2  EQU   SDWAINT2                                                 06346000
@NM00138 EQU   SDWAINT2                                                 06347000
SDWAPGM2 EQU   SDWAMWP2                                                 06348000
SDWAWAT2 EQU   SDWAMWP2                                                 06349000
SDWAMCK2 EQU   SDWAMWP2                                                 06350000
SDWAECT2 EQU   SDWAMWP2                                                 06351000
SDWAKEY2 EQU   SDWAMWP2                                                 06352000
SDWAEXT2 EQU   SDWAEMK2                                                 06353000
SDWAIO2  EQU   SDWAEMK2                                                 06354000
SDWATRM2 EQU   SDWAEMK2                                                 06355000
@NM00137 EQU   SDWAEMK2                                                 06356000
SDWAPER2 EQU   SDWAEMK2                                                 06357000
@NM00136 EQU   SDWAEMK2                                                 06358000
SDWATRAN EQU   SDWAAEC1+4                                               06359000
SDWAIPC1 EQU   SDWAICD1                                                 06360000
SDWAIMC1 EQU   SDWAICD1                                                 06361000
SDWAIPR1 EQU   SDWAICD1                                                 06362000
@NM00135 EQU   SDWAINC1                                                 06363000
@NM00134 EQU   SDWAILC1                                                 06364000
SDWAIL1  EQU   SDWAILC1                                                 06365000
@NM00133 EQU   SDWAILC1                                                 06366000
@NM00132 EQU   SDWAAEC1                                                 06367000
SDWAADD1 EQU   SDWANXT1+1                                               06368000
@NM00131 EQU   SDWANXT1                                                 06369000
@NM00130 EQU   SDWAEC1+3                                                06370000
SDWASGN1 EQU   SDWAINT1                                                 06371000
SDWAEXP1 EQU   SDWAINT1                                                 06372000
SDWADEC1 EQU   SDWAINT1                                                 06373000
SDWAFPO1 EQU   SDWAINT1                                                 06374000
SDWACC1  EQU   SDWAINT1                                                 06375000
@NM00129 EQU   SDWAINT1                                                 06376000
SDWAPGM1 EQU   SDWAMWP1                                                 06377000
SDWAWAT1 EQU   SDWAMWP1                                                 06378000
SDWAMCK1 EQU   SDWAMWP1                                                 06379000
SDWAECT1 EQU   SDWAMWP1                                                 06380000
SDWAKEY1 EQU   SDWAMWP1                                                 06381000
SDWAEXT1 EQU   SDWAEMK1                                                 06382000
SDWAIO1  EQU   SDWAEMK1                                                 06383000
SDWATRM1 EQU   SDWAEMK1                                                 06384000
@NM00128 EQU   SDWAEMK1                                                 06385000
SDWAPER1 EQU   SDWAEMK1                                                 06386000
@NM00127 EQU   SDWAEMK1                                                 06387000
SDWAIOBR EQU   SDWA+100                                                 06388000
SDWAEPA  EQU   SDWA+96                                                  06389000
@NM00126 EQU   SDWANAME+4                                               06390000
SDWARBAD EQU   SDWANAME                                                 06391000
SDWAGR15 EQU   SDWAGRSV+60                                              06392000
SDWAGR14 EQU   SDWAGRSV+56                                              06393000
SDWAGR13 EQU   SDWAGRSV+52                                              06394000
SDWAGR12 EQU   SDWAGRSV+48                                              06395000
SDWAGR11 EQU   SDWAGRSV+44                                              06396000
SDWAGR10 EQU   SDWAGRSV+40                                              06397000
SDWAGR09 EQU   SDWAGRSV+36                                              06398000
SDWAGR08 EQU   SDWAGRSV+32                                              06399000
SDWAGR07 EQU   SDWAGRSV+28                                              06400000
SDWAGR06 EQU   SDWAGRSV+24                                              06401000
SDWAGR05 EQU   SDWAGRSV+20                                              06402000
SDWAGR04 EQU   SDWAGRSV+16                                              06403000
SDWAGR03 EQU   SDWAGRSV+12                                              06404000
SDWAGR02 EQU   SDWAGRSV+8                                               06405000
SDWAGR01 EQU   SDWAGRSV+4                                               06406000
SDWAGR00 EQU   SDWAGRSV                                                 06407000
SDWANXTP EQU   SDWACTL2+5                                               06408000
SDWASGP  EQU   SDWAPMKP                                                 06409000
SDWAEUP  EQU   SDWAPMKP                                                 06410000
SDWADOP  EQU   SDWAPMKP                                                 06411000
SDWAFPP  EQU   SDWAPMKP                                                 06412000
SDWACCP  EQU   SDWAPMKP                                                 06413000
SDWAILP  EQU   SDWAPMKP                                                 06414000
SDWAINTP EQU   SDWACTL2+2                                               06415000
SDWASPVP EQU   SDWAMWPP                                                 06416000
SDWAWATP EQU   SDWAMWPP                                                 06417000
SDWAMCKP EQU   SDWAMWPP                                                 06418000
@NM00125 EQU   SDWAMWPP                                                 06419000
SDWAKEYP EQU   SDWAMWPP                                                 06420000
SDWAEXTP EQU   SDWACMKP                                                 06421000
SDWAIOP  EQU   SDWACMKP                                                 06422000
SDWANXTA EQU   SDWACTL1+5                                               06423000
SDWASGA  EQU   SDWAPMKA                                                 06424000
SDWAEUA  EQU   SDWAPMKA                                                 06425000
SDWADOA  EQU   SDWAPMKA                                                 06426000
SDWAFPA  EQU   SDWAPMKA                                                 06427000
SDWACCA  EQU   SDWAPMKA                                                 06428000
SDWAILA  EQU   SDWAPMKA                                                 06429000
SDWAINTA EQU   SDWACTL1+2                                               06430000
SDWASPVA EQU   SDWAMWPA                                                 06431000
SDWAWATA EQU   SDWAMWPA                                                 06432000
SDWAMCKA EQU   SDWAMWPA                                                 06433000
@NM00124 EQU   SDWAMWPA                                                 06434000
SDWAKEYA EQU   SDWAMWPA                                                 06435000
SDWAEXTA EQU   SDWACMKA                                                 06436000
SDWAIOA  EQU   SDWACMKA                                                 06437000
SDWACMPC EQU   SDWAABCC+1                                               06438000
@NM00123 EQU   SDWACMPF                                                 06439000
SDWASTCC EQU   SDWACMPF                                                 06440000
@NM00122 EQU   SDWACMPF                                                 06441000
SDWASTEP EQU   SDWACMPF                                                 06442000
CVTLEVL  EQU   CVTRELNO+2                                               06443000
CVTNUMB  EQU   CVTRELNO                                                 06444000
CVTMDL   EQU   CVTFIX+250                                               06445000
@NM00121 EQU   CVTFIX+248                                               06446000
@NM00120 EQU   CVTFIX                                                   06447000
CVTRV482 EQU   CVTXTNT2+128                                             06448000
CVTRV481 EQU   CVTXTNT2+124                                             06449000
CVTRV480 EQU   CVTXTNT2+120                                             06450000
CVTRV479 EQU   CVTXTNT2+118                                             06451000
CVTRV478 EQU   CVTXTNT2+117                                             06452000
CVTRV477 EQU   CVTXTNT2+116                                             06453000
CVTRV476 EQU   CVTXTNT2+115                                             06454000
CVTRV475 EQU   CVTXTNT2+114                                             06455000
CVTRV474 EQU   CVTRV466                                                 06456000
CVTRV473 EQU   CVTRV466                                                 06457000
CVTRV472 EQU   CVTRV466                                                 06458000
CVTRV471 EQU   CVTRV466                                                 06459000
CVTRV470 EQU   CVTRV466                                                 06460000
CVTRV469 EQU   CVTRV466                                                 06461000
CVTRV468 EQU   CVTRV466                                                 06462000
CVTRV467 EQU   CVTRV466                                                 06463000
CVTRV465 EQU   CVTRV457                                                 06464000
CVTRV464 EQU   CVTRV457                                                 06465000
CVTRV463 EQU   CVTRV457                                                 06466000
CVTRV462 EQU   CVTRV457                                                 06467000
CVTRV461 EQU   CVTRV457                                                 06468000
CVTRV460 EQU   CVTRV457                                                 06469000
CVTRV459 EQU   CVTRV457                                                 06470000
CVTRV458 EQU   CVTRV457                                                 06471000
CVTRV456 EQU   CVTXTNT2+108                                             06472000
CVTRV455 EQU   CVTXTNT2+104                                             06473000
CVTRV454 EQU   CVTXTNT2+100                                             06474000
CVTRV453 EQU   CVTXTNT2+96                                              06475000
CVTRV452 EQU   CVTXTNT2+94                                              06476000
CVTRV451 EQU   CVTXTNT2+92                                              06477000
CVTRV450 EQU   CVTXTNT2+90                                              06478000
CVTRV449 EQU   CVTXTNT2+88                                              06479000
CVTRV448 EQU   CVTXTNT2+87                                              06480000
CVTRV447 EQU   CVTXTNT2+86                                              06481000
CVTRV446 EQU   CVTRV438                                                 06482000
CVTRV445 EQU   CVTRV438                                                 06483000
CVTRV444 EQU   CVTRV438                                                 06484000
CVTRV443 EQU   CVTRV438                                                 06485000
CVTRV442 EQU   CVTRV438                                                 06486000
CVTRV441 EQU   CVTRV438                                                 06487000
CVTRV440 EQU   CVTRV438                                                 06488000
CVTRV439 EQU   CVTRV438                                                 06489000
CVTRV437 EQU   CVTRV429                                                 06490000
CVTRV436 EQU   CVTRV429                                                 06491000
CVTRV435 EQU   CVTRV429                                                 06492000
CVTRV434 EQU   CVTRV429                                                 06493000
CVTRV433 EQU   CVTRV429                                                 06494000
CVTRV432 EQU   CVTRV429                                                 06495000
CVTRV431 EQU   CVTRV429                                                 06496000
CVTRV430 EQU   CVTRV429                                                 06497000
CVTRV428 EQU   CVTXTNT2+80                                              06498000
CVTRV427 EQU   CVTXTNT2+76                                              06499000
CVTRV426 EQU   CVTXTNT2+72                                              06500000
CVTRV425 EQU   CVTXTNT2+68                                              06501000
CVTATACT EQU   CVTATCVT                                                 06502000
CVTRV423 EQU   CVTXTNT2+62                                              06503000
CVTRV422 EQU   CVTXTNT2+60                                              06504000
CVTRV421 EQU   CVTXTNT2+58                                              06505000
CVTRV420 EQU   CVTXTNT2+56                                              06506000
CVTRV419 EQU   CVTXTNT2+55                                              06507000
CVTRV418 EQU   CVTXTNT2+54                                              06508000
CVTRV417 EQU   CVTRV409                                                 06509000
CVTRV416 EQU   CVTRV409                                                 06510000
CVTRV415 EQU   CVTRV409                                                 06511000
CVTRV414 EQU   CVTRV409                                                 06512000
CVTRV413 EQU   CVTRV409                                                 06513000
CVTRV412 EQU   CVTRV409                                                 06514000
CVTRV411 EQU   CVTRV409                                                 06515000
CVTRV410 EQU   CVTRV409                                                 06516000
CVTRV408 EQU   CVTRV400                                                 06517000
CVTRV407 EQU   CVTRV400                                                 06518000
CVTRV406 EQU   CVTRV400                                                 06519000
CVTRV405 EQU   CVTRV400                                                 06520000
CVTRV404 EQU   CVTRV400                                                 06521000
CVTRV403 EQU   CVTRV400                                                 06522000
CVTRV402 EQU   CVTRV400                                                 06523000
CVTRV401 EQU   CVTRV400                                                 06524000
CVTICB   EQU   CVTXTNT2+48                                              06525000
CVTSKTA  EQU   CVTXTNT2+44                                              06526000
CVTCCVT  EQU   CVTXTNT2+40                                              06527000
CVTRSV98 EQU   CVTXTNT2+36                                              06528000
CVTRSV97 EQU   CVTXTNT2+34                                              06529000
CVTRSV96 EQU   CVTXTNT2+32                                              06530000
CVTOLTEP EQU   CVTXTNT2+28                                              06531000
CVTQIDA  EQU   CVTQID+1                                                 06532000
CVTRSV95 EQU   CVTQID                                                   06533000
CVTRSV94 EQU   CVTXTNT2+20                                              06534000
CVTRSV93 EQU   CVTXTNT2+16                                              06535000
CVTRSV92 EQU   CVTXTNT2+12                                              06536000
CVTDEBVR EQU   CVTXTNT2+8                                               06537000
CVTRSV91 EQU   CVTXTNT2+6                                               06538000
CVTRSV9H EQU   CVTFLGBT                                                 06539000
CVTRSV9G EQU   CVTFLGBT                                                 06540000
CVTRSV9F EQU   CVTFLGBT                                                 06541000
CVTRSV9E EQU   CVTFLGBT                                                 06542000
CVTRSV9D EQU   CVTFLGBT                                                 06543000
CVTBAH   EQU   CVTFLGBT                                                 06544000
CVTVME   EQU   CVTFLGBT                                                 06545000
CVTNPE   EQU   CVTFLGBT                                                 06546000
CVTNUCLS EQU   CVTXTNT2+4                                               06547000
CVTDSSVA EQU   CVTDSSV+1                                                06548000
CVTRSV89 EQU   CVTDSSV                                                  06549000
CVTRSV88 EQU   CVTXTNT1+8                                               06550000
CVTRSV87 EQU   CVTXTNT1+4                                               06551000
CVTFACHN EQU   CVTXTNT1                                                 06552000
CVTRV488 EQU   CVTMAP+412                                               06553000
CVTRV487 EQU   CVTMAP+408                                               06554000
CVTRV486 EQU   CVTMAP+404                                               06555000
CVTRV485 EQU   CVTMAP+400                                               06556000
CVTACTAP EQU   CVTMAP+396                                               06557000
CVTAUTH  EQU   CVTMAP+392                                               06558000
CVTRV490 EQU   CVTMAP+388                                               06559000
CVTSU    EQU   CVTMAP+384                                               06560000
CVTVOLT1 EQU   CVTVOLM1+1                                               06561000
CVTVOLI1 EQU   CVTVOLF1                                                 06562000
CVTSTOA  EQU   CVTMAP+376                                               06563000
CVTRSV58 EQU   CVTMAP+374                                               06564000
CVTRSV57 EQU   CVTMAP+372                                               06565000
CVTDDCE  EQU   CVTMAP+368                                               06566000
CVTPNWFR EQU   CVTMAP+364                                               06567000
CVTSMF   EQU   CVTMAP+360                                               06568000
CVTSULK  EQU   CVTMAP+358                                               06569000
CVTSLKO  EQU   CVTSYSK                                                  06570000
CVTSLKP  EQU   CVTSYSK                                                  06571000
CVTSLKQ  EQU   CVTSYSK                                                  06572000
CVTSLKR  EQU   CVTSYSK                                                  06573000
CVTRSV56 EQU   CVTSYSK                                                  06574000
CVTRSV55 EQU   CVTSYSK                                                  06575000
CVTRSV54 EQU   CVTSYSK                                                  06576000
CVTRSV53 EQU   CVTSYSK                                                  06577000
CVTRSV52 EQU   CVTA1F1                                                  06578000
CVTRSV51 EQU   CVTA1F1                                                  06579000
CVTRSV50 EQU   CVTA1F1                                                  06580000
CVTRSV49 EQU   CVTA1F1                                                  06581000
CVTRSV48 EQU   CVTA1F1                                                  06582000
CVTRSV47 EQU   CVTA1F1                                                  06583000
CVTSRSW  EQU   CVTA1F1                                                  06584000
CVTPFSW  EQU   CVTA1F1                                                  06585000
CVTPCVT  EQU   CVTMAP+352                                               06586000
CVTRSV46 EQU   CVTMAP+344                                               06587000
CVTRSV45 EQU   CVTMAP+340                                               06588000
CVTRSV44 EQU   CVTMAP+338                                               06589000
CVTRSV43 EQU   CVTMAP+336                                               06590000
CVTHJESA EQU   CVTHJES+1                                                06591000
CVTRSV42 EQU   CVTHJES                                                  06592000
CVTEXT2A EQU   CVTEXT2+1                                                06593000
CVTRSV41 EQU   CVTEXT2                                                  06594000
CVTAPFA  EQU   CVTAPF+1                                                 06595000
CVTRSV40 EQU   CVTAPF                                                   06596000
CVTRV518 EQU   CVTINTLA                                                 06597000
CVTRV517 EQU   CVTERPV                                                  06598000
CVTEORM  EQU   CVTMAP+312                                               06599000
CVTMCHPR EQU   CVTMAP+308                                               06600000
CVTJEPS  EQU   CVTMAP+300                                               06601000
CVTJESCT EQU   CVTMAP+296                                               06602000
CVTMODE  EQU   CVTMAP+292                                               06603000
CVTPTRV  EQU   CVTMAP+288                                               06604000
CVTREAL  EQU   CVTMAP+284                                               06605000
CVTRSV39 EQU   CVTMAP+280                                               06606000
CVTRSV38 EQU   CVTMAP+276                                               06607000
CVTDMSRA EQU   CVTDMSR+1                                                06608000
CVTRV634 EQU   CVTDMSRF                                                 06609000
CVTRV633 EQU   CVTDMSRF                                                 06610000
CVTRV632 EQU   CVTDMSRF                                                 06611000
CVTRV631 EQU   CVTDMSRF                                                 06612000
CVTRV630 EQU   CVTDMSRF                                                 06613000
CVTRV629 EQU   CVTDMSRF                                                 06614000
CVTUDUMP EQU   CVTDMSRF                                                 06615000
CVTSDUMP EQU   CVTDMSRF                                                 06616000
CVTQMSGA EQU   CVTQMSG+1                                                06617000
CVTRSV36 EQU   CVTQMSG                                                  06618000
CVTAMFF  EQU   CVTMAP+264                                               06619000
CVTPURGA EQU   CVTPURG+1                                                06620000
CVTRSV35 EQU   CVTPURG                                                  06621000
CVTCBSP  EQU   CVTMAP+256                                               06622000
CVTATERA EQU   CVTATER+1                                                06623000
CVTSYST  EQU   CVTATER                                                  06624000
CVTVOLT2 EQU   CVTTAT                                                   06625000
CVTVOLI2 EQU   CVTVOLF2                                                 06626000
CVTAQAVB EQU   CVTAQAVT+1                                               06627000
CVTRSV34 EQU   CVTTCMFG                                                 06628000
CVTRSV33 EQU   CVTTCMFG                                                 06629000
CVTRSV32 EQU   CVTTCMFG                                                 06630000
CVTRSV31 EQU   CVTTCMFG                                                 06631000
CVTRSV30 EQU   CVTTCMFG                                                 06632000
CVTRSV29 EQU   CVTTCMFG                                                 06633000
CVTLDEV  EQU   CVTTCMFG                                                 06634000
CVTTCRDY EQU   CVTTCMFG                                                 06635000
CVTGTFA  EQU   CVTGTF+1                                                 06636000
CVTRSV27 EQU   CVTGTFST                                                 06637000
CVTRNIO  EQU   CVTGTFST                                                 06638000
CVTUSR   EQU   CVTGTFST                                                 06639000
CVTRV318 EQU   CVTFORM                                                  06640000
CVTRV317 EQU   CVTTMODE                                                 06641000
CVTRV316 EQU   CVTSTATE                                                 06642000
CVTRV315 EQU   CVTGTFS                                                  06643000
CVTGTFAV EQU   CVTGTFS                                                  06644000
CVT0SCR1 EQU   CVTMAP+232                                               06645000
CVTRV515 EQU   CVTMAP+228                                               06646000
CVTRMS   EQU   CVTMAP+224                                               06647000
CVTPATCH EQU   CVTMAP+220                                               06648000
CVTTSCE  EQU   CVTMAP+216                                               06649000
CVTLNKSC EQU   CVTMAP+214                                               06650000
CVTQABST EQU   CVTMAP+212                                               06651000
CVTMDLDS EQU   CVTMAP+208                                               06652000
CVTUSER  EQU   CVTMAP+204                                               06653000
CVTABEND EQU   CVTMAP+200                                               06654000
CVTSMCA  EQU   CVTMAP+196                                               06655000
CVTRSV18 EQU   CVTMAP+192                                               06656000
CVTQLPAQ EQU   CVTMAP+188                                               06657000
CVTQCDSR EQU   CVTMAP+184                                               06658000
CVTVS1B  EQU   CVTOPTB                                                  06659000
CVTVS1A  EQU   CVTOPTB                                                  06660000
CVTFP    EQU   CVTOPTB                                                  06661000
CVTAPTHR EQU   CVTOPTB                                                  06662000
CVTNLOG  EQU   CVTOPTB                                                  06663000
CVTTOD   EQU   CVTOPTB                                                  06664000
CVTCTIMS EQU   CVTOPTB                                                  06665000
CVTPROT  EQU   CVTOPTB                                                  06666000
CVTXPFP  EQU   CVTOPTA                                                  06667000
CVTASCII EQU   CVTOPTA                                                  06668000
CVTRSV13 EQU   CVTOPTA                                                  06669000
CVTRSV12 EQU   CVTOPTA                                                  06670000
CVTNIP   EQU   CVTOPTA                                                  06671000
CVTDDR   EQU   CVTOPTA                                                  06672000
CVTAPR   EQU   CVTOPTA                                                  06673000
CVTCCH   EQU   CVTOPTA                                                  06674000
CVTSNCTR EQU   CVTMAP+180                                               06675000
CVTQMWR  EQU   CVTMAP+176                                               06676000
CVTQOCR  EQU   CVTMAP+172                                               06677000
CVT1EF00 EQU   CVTMAP+168                                               06678000
CVTMZ00  EQU   CVTMAP+164                                               06679000
CVTSV76Q EQU   CVTSV76C                                                 06680000
CVTRSV11 EQU   CVTMAP+156                                               06681000
CVT0PT01 EQU   CVTMAP+152                                               06682000
CVTRV516 EQU   CVTIERLC                                                 06683000
CVTILCH  EQU   CVTMAP+140                                               06684000
CVT0DS   EQU   CVTMAP+136                                               06685000
CVTFBOSV EQU   CVTMAP+132                                               06686000
CVTNUCB  EQU   CVTMAP+128                                               06687000
CVTIXAVL EQU   CVTMAP+124                                               06688000
CVTSV76M EQU   CVTIOQET                                                 06689000
CVTDCBA  EQU   CVTMAP+117                                               06690000
CVTMVS2  EQU   CVTDCB                                                   06691000
CVT6DAT  EQU   CVTDCB                                                   06692000
CVT4MPS  EQU   CVTDCB                                                   06693000
CVTRSV09 EQU   CVTDCB                                                   06694000
CVT4MS1  EQU   CVTDCB                                                   06695000
CVT2SPS  EQU   CVTDCB                                                   06696000
CVT1SSS  EQU   CVTDCB                                                   06697000
CVTRSV08 EQU   CVTDCB                                                   06698000
CVTSTB   EQU   CVTMAP+112                                               06699000
CVTCUCB  EQU   CVTMAP+100                                               06700000
CVTSJQ   EQU   CVTMAP+96                                                06701000
CVTPBLDL EQU   CVTMAP+92                                                06702000
CVTSVDCB EQU   CVTMAP+84                                                06703000
CVTBRET  EQU   CVTMAP+82                                                06704000
CVT0FN00 EQU   CVTMAP+76                                                06705000
CVTDARA  EQU   CVTDAR+1                                                 06706000
CVTRSV07 EQU   CVTFLGS1                                                 06707000
CVTRSV06 EQU   CVTFLGS1                                                 06708000
CVTRSV05 EQU   CVTFLGS1                                                 06709000
CVTRSV04 EQU   CVTFLGS1                                                 06710000
CVTRSV03 EQU   CVTFLGS1                                                 06711000
CVTRSV02 EQU   CVTFLGS1                                                 06712000
CVTRSV01 EQU   CVTFLGS1                                                 06713000
CVTDMPLK EQU   CVTFLGS1                                                 06714000
CVTXITP  EQU   CVTMAP+68                                                06715000
CVTZDTAB EQU   CVTMAP+64                                                06716000
CVTMSLT  EQU   CVTMAP+60                                                06717000
CVTBTERM EQU   CVTMAP+52                                                06718000
CVTSYSAD EQU   CVTMAP+48                                                06719000
CVTXTLER EQU   CVTMAP+44                                                06720000
CVTILK2  EQU   CVTMAP+40                                                06721000
CVTILK1  EQU   CVTMAP+36                                                06722000
CVTPRLTV EQU   CVTMAP+32                                                06723000
CVTPCNVT EQU   CVTMAP+28                                                06724000
CVT0VL00 EQU   CVTMAP+24                                                06725000
CVTXAPG  EQU   CVTMAP+20                                                06726000
CVTBUF   EQU   CVTMAP+16                                                06727000
CVTJOB   EQU   CVTMAP+12                                                06728000
CVTLINK  EQU   CVTMAP+8                                                 06729000
CVT0EF00 EQU   CVTMAP+4                                                 06730000
CVTTCBP  EQU   CVTMAP                                                   06731000
TQESTCKR EQU   TQESTCK+4                                                06732000
TQESTCKL EQU   TQESTCK                                                  06733000
TQERSAVE EQU   TQE+116                                                  06734000
@NM00119 EQU   TQE+89                                                   06735000
@NM00118 EQU   TQEFLGS3                                                 06736000
TQEDIE   EQU   TQEFLGS3                                                 06737000
TQEDREGS EQU   TQESRB                                                   06738000
TQELHPSW EQU   TQE+40                                                   06739000
TQEASCB  EQU   TQE+36                                                   06740000
TQETCB   EQU   TQE+32                                                   06741000
TQESADDR EQU   TQE+24                                                   06742000
TQEMIDN  EQU   TQEFLGS2                                                 06743000
TQEMF1   EQU   TQEFLGS2                                                 06744000
TQEOPT   EQU   TQEFLGS2                                                 06745000
TQELM    EQU   TQEFLGS2                                                 06746000
TQEDUM   EQU   TQEFLGS2                                                 06747000
TQECRH   EQU   TQEFLGS2                                                 06748000
TQEUSER  EQU   TQEFLGS2                                                 06749000
TQETYPE  EQU   TQEFLGS                                                  06750000
TQEXITSP EQU   TQEFLGS                                                  06751000
TQEINCOM EQU   TQEFLGS                                                  06752000
TQEWLIM  EQU   TQEFLGS                                                  06753000
@NM00117 EQU   TQEFLGS                                                  06754000
TQETOD   EQU   TQEFLGS                                                  06755000
TQEOFF   EQU   TQEFLGS                                                  06756000
TQEAID   EQU   TQE+12                                                   06757000
TQEBLNK  EQU   TQE+8                                                    06758000
TQEFLNK  EQU   TQE+4                                                    06759000
TQETQE   EQU   TQE                                                      06760000
@NM00116 EQU   PCCA+384                                                 06761000
PCCARV36 EQU   PCCA+380                                                 06762000
PCCARV35 EQU   PCCA+378                                                 06763000
PCCARV01 EQU   PCCA+377                                                 06764000
PCCAR105 EQU   PCCAATTR                                                 06765000
PCCAR104 EQU   PCCAATTR                                                 06766000
PCCAR103 EQU   PCCAATTR                                                 06767000
PCCAR102 EQU   PCCAATTR                                                 06768000
PCCAR101 EQU   PCCAATTR                                                 06769000
PCCAR100 EQU   PCCAATTR                                                 06770000
PCCAIO   EQU   PCCAATTR                                                 06771000
PCCACPUM EQU   PCCAATTR                                                 06772000
PCCARV63 EQU   PCCA+372                                                 06773000
PCCARV62 EQU   PCCA+368                                                 06774000
PCCARV61 EQU   PCCA+364                                                 06775000
PCCARV60 EQU   PCCA+360                                                 06776000
PCCARV59 EQU   PCCA+356                                                 06777000
PCCARV58 EQU   PCCA+352                                                 06778000
PCCARV57 EQU   PCCA+348                                                 06779000
PCCARV56 EQU   PCCA+344                                                 06780000
PCCARV55 EQU   PCCA+340                                                 06781000
PCCARV54 EQU   PCCA+336                                                 06782000
PCCALOGA EQU   PCCA+332                                                 06783000
PCCACHID EQU   PCCA+324                                                 06784000
PCCACHSV EQU   PCCA+312                                                 06785000
PCCARV79 EQU   PCCACHF4                                                 06786000
PCCARV78 EQU   PCCACHF4                                                 06787000
PCCARV77 EQU   PCCACHF4                                                 06788000
PCCARV76 EQU   PCCACHF4                                                 06789000
PCCARV75 EQU   PCCACHF4                                                 06790000
PCCARV74 EQU   PCCACHF4                                                 06791000
PCCARV73 EQU   PCCACHF4                                                 06792000
PCCARV72 EQU   PCCACHF4                                                 06793000
PCCARV71 EQU   PCCACHF3                                                 06794000
PCCARV70 EQU   PCCACHF3                                                 06795000
PCCARV69 EQU   PCCACHF3                                                 06796000
PCCARV68 EQU   PCCACHF3                                                 06797000
PCCARV67 EQU   PCCACHF3                                                 06798000
PCCARV66 EQU   PCCACHF3                                                 06799000
PCCASLCK EQU   PCCACHF3                                                 06800000
PCCAISRB EQU   PCCACHF3                                                 06801000
PCCACF28 EQU   PCCACHF2                                                 06802000
PCCACF27 EQU   PCCACHF2                                                 06803000
PCCACF26 EQU   PCCACHF2                                                 06804000
PCCACF25 EQU   PCCACHF2                                                 06805000
PCCACF24 EQU   PCCACHF2                                                 06806000
PCCACF23 EQU   PCCACHF2                                                 06807000
PCCACF22 EQU   PCCACHF2                                                 06808000
PCCACF21 EQU   PCCACHF2                                                 06809000
PCCACF18 EQU   PCCACHF1                                                 06810000
PCCACF17 EQU   PCCACHF1                                                 06811000
PCCACF16 EQU   PCCACHF1                                                 06812000
PCCACF15 EQU   PCCACHF1                                                 06813000
PCCACF14 EQU   PCCACHF1                                                 06814000
PCCACF13 EQU   PCCACHF1                                                 06815000
PCCACF12 EQU   PCCACHF1                                                 06816000
PCCACF11 EQU   PCCACHF1                                                 06817000
PCCARV05 EQU   PCCA+307                                                 06818000
PCCACHPB EQU   PCCA+306                                                 06819000
PCCALGP2 EQU   PCCA+305                                                 06820000
PCCALGP1 EQU   PCCA+304                                                 06821000
PCCACELL EQU   PCCA+302                                                 06822000
PCCALOGL EQU   PCCA+300                                                 06823000
PCCACHW2 EQU   PCCA+296                                                 06824000
PCCACHW1 EQU   PCCA+292                                                 06825000
PCCAIOSI EQU   PCCA+291                                                 06826000
PCCACNRB EQU   PCCACHRB                                                 06827000
PCCACCVB EQU   PCCACHRB                                                 06828000
PCCACSNB EQU   PCCACHRB                                                 06829000
PCCARV52 EQU   PCCACHRB                                                 06830000
PCCACHIB EQU   PCCACHRB                                                 06831000
PCCACTIB EQU   PCCACHRB                                                 06832000
PCCACINB EQU   PCCACHRB                                                 06833000
PCCACSIB EQU   PCCACHRB                                                 06834000
PCCARV51 EQU   PCCACHS2                                                 06835000
PCCARV50 EQU   PCCACHS2                                                 06836000
PCCACCRA EQU   PCCACHS2                                                 06837000
PCCACURC EQU   PCCACHS2                                                 06838000
PCCACNLG EQU   PCCACHS2                                                 06839000
PCCACMOD EQU   PCCACHS2                                                 06840000
PCCACALT EQU   PCCACHS2                                                 06841000
PCCACIOR EQU   PCCACHS2                                                 06842000
PCCARV47 EQU   PCCACHS1                                                 06843000
PCCACUCB EQU   PCCACHS1                                                 06844000
PCCACIBC EQU   PCCACHS1                                                 06845000
PCCACAND EQU   PCCACHS1                                                 06846000
PCCACNLS EQU   PCCACHS1                                                 06847000
PCCACFRR EQU   PCCACHS1                                                 06848000
PCCACNRE EQU   PCCACHS1                                                 06849000
PCCACCMP EQU   PCCACHS1                                                 06850000
PCCACSEQ EQU   PCCACHTS                                                 06851000
PCCACDIN EQU   PCCACHTS                                                 06852000
PCCARV44 EQU   PCCACHTS                                                 06853000
PCCARV43 EQU   PCCACHTS                                                 06854000
PCCACTEC EQU   PCCACHTS                                                 06855000
PCCACDAV EQU   PCCACHVA                                                 06856000
PCCACCHV EQU   PCCACHVA                                                 06857000
PCCACCMD EQU   PCCACHVA                                                 06858000
PCCACUNS EQU   PCCACHVA                                                 06859000
PCCACSQV EQU   PCCACHVA                                                 06860000
PCCARV42 EQU   PCCACHVA                                                 06861000
PCCARV41 EQU   PCCACHVA                                                 06862000
PCCACITF EQU   PCCACHVA                                                 06863000
PCCARV40 EQU   PCCACHBL                                                 06864000
PCCARV39 EQU   PCCACHBL                                                 06865000
PCCARV38 EQU   PCCACHBL                                                 06866000
PCCACCUE EQU   PCCACHBL                                                 06867000
PCCACSTG EQU   PCCACHBL                                                 06868000
PCCACSCU EQU   PCCACHBL                                                 06869000
PCCACCHA EQU   PCCACHBL                                                 06870000
PCCACCPU EQU   PCCACHBL                                                 06871000
PCCACNOR EQU   PCCACHPF                                                 06872000
PCCACCNT EQU   PCCACHPF                                                 06873000
PCCACSNS EQU   PCCACHPF                                                 06874000
PCCARV37 EQU   PCCACHPF                                                 06875000
PCCACHIO EQU   PCCACHPF                                                 06876000
PCCACTIO EQU   PCCACHPF                                                 06877000
PCCACINT EQU   PCCACHPF                                                 06878000
PCCACSIO EQU   PCCACHPF                                                 06879000
PCCACHUB EQU   PCCAWERP                                                 06880000
PCCAR106 EQU   PCCA+228                                                 06881000
PCCACCHI EQU   PCCA+226                                                 06882000
PCCASRBL EQU   PCCA+225                                                 06883000
PCCAR112 EQU   PCCACHAN                                                 06884000
PCCAR111 EQU   PCCACHAN                                                 06885000
PCCAR110 EQU   PCCACHAN                                                 06886000
PCCAR109 EQU   PCCACHAN                                                 06887000
PCCAR108 EQU   PCCACHAN                                                 06888000
PCCAR107 EQU   PCCACHAN                                                 06889000
PCCAEXDM EQU   PCCACHAN                                                 06890000
PCCAIRST EQU   PCCACHAN                                                 06891000
PCCASRB  EQU   PCCA+180                                                 06892000
PCCACCHM EQU   PCCA+176                                                 06893000
PCCAELBA EQU   PCCA+172                                                 06894000
PCCAELAD EQU   PCCA+168                                                 06895000
PCCALRBR EQU   PCCA+164                                                 06896000
PCCALRBV EQU   PCCA+160                                                 06897000
PCCAPWAR EQU   PCCA+156                                                 06898000
PCCAPWAV EQU   PCCA+152                                                 06899000
PCCAEMSA EQU   PCCAEMSB+12                                              06900000
PCCAEMSE EQU   PCCAEMSB+8                                               06901000
PCCAEMSP EQU   PCCAEMSB+4                                               06902000
PCCARMS  EQU   PCCARMSB                                                 06903000
PCCARV34 EQU   PCCARMSB                                                 06904000
PCCARV33 EQU   PCCARMSB                                                 06905000
PCCARV32 EQU   PCCARMSB                                                 06906000
PCCARV31 EQU   PCCARMSB                                                 06907000
PCCARV30 EQU   PCCARMSB                                                 06908000
PCCARV29 EQU   PCCARMSB                                                 06909000
PCCARV28 EQU   PCCARMSB                                                 06910000
PCCARV27 EQU   PCCAEMS3                                                 06911000
PCCARV26 EQU   PCCAEMS3                                                 06912000
PCCARV25 EQU   PCCAEMS3                                                 06913000
PCCARV24 EQU   PCCAEMS3                                                 06914000
PCCARV23 EQU   PCCAEMS3                                                 06915000
PCCARV22 EQU   PCCAEMS3                                                 06916000
PCCARV21 EQU   PCCAEMS3                                                 06917000
PCCARV20 EQU   PCCAEMS3                                                 06918000
PCCARV19 EQU   PCCAEMS2                                                 06919000
PCCARV18 EQU   PCCAEMS2                                                 06920000
PCCARV17 EQU   PCCAEMS2                                                 06921000
PCCARV16 EQU   PCCAEMS2                                                 06922000
PCCARV15 EQU   PCCAEMS2                                                 06923000
PCCARV14 EQU   PCCAEMS2                                                 06924000
PCCARV13 EQU   PCCAEMS2                                                 06925000
PCCASERP EQU   PCCAEMS2                                                 06926000
PCCARV11 EQU   PCCARISP                                                 06927000
PCCARV10 EQU   PCCARISP                                                 06928000
PCCARV09 EQU   PCCARISP                                                 06929000
PCCARV08 EQU   PCCARISP                                                 06930000
PCCARV07 EQU   PCCARISP                                                 06931000
PCCARV06 EQU   PCCARISP                                                 06932000
PCCASERL EQU   PCCARISP                                                 06933000
PCCAPARL EQU   PCCARISP                                                 06934000
PCCARPB  EQU   PCCA+132                                                 06935000
PCCACTIN EQU   PCCAINTE                                                 06936000
PCCACTCC EQU   PCCACCE                                                  06937000
PCCACTTD EQU   PCCATODE                                                 06938000
PCCARV04 EQU   PCCATMFL                                                 06939000
PCCARV03 EQU   PCCATMFL                                                 06940000
PCCARV02 EQU   PCCATMFL                                                 06941000
PCCAINIT EQU   PCCATMFL                                                 06942000
PCCARV9E EQU   PCCA+124                                                 06943000
PCCARV9D EQU   PCCA+120                                                 06944000
PCCARV9C EQU   PCCA+116                                                 06945000
PCCARV9B EQU   PCCA+112                                                 06946000
PCCARV9A EQU   PCCA+108                                                 06947000
PCCARV99 EQU   PCCA+104                                                 06948000
PCCARV98 EQU   PCCA+100                                                 06949000
PCCARV97 EQU   PCCA+96                                                  06950000
PCCARV96 EQU   PCCA+92                                                  06951000
PCCARV95 EQU   PCCA+88                                                  06952000
PCCARV94 EQU   PCCA+84                                                  06953000
PCCARV93 EQU   PCCA+80                                                  06954000
PCCARV92 EQU   PCCA+76                                                  06955000
PCCARV91 EQU   PCCA+72                                                  06956000
PCCARV90 EQU   PCCA+68                                                  06957000
PCCARV89 EQU   PCCA+64                                                  06958000
PCCARV88 EQU   PCCA+60                                                  06959000
PCCARV87 EQU   PCCA+56                                                  06960000
PCCARV86 EQU   PCCA+52                                                  06961000
PCCARV85 EQU   PCCA+48                                                  06962000
PCCARV84 EQU   PCCA+44                                                  06963000
PCCARV83 EQU   PCCA+40                                                  06964000
PCCARV82 EQU   PCCA+36                                                  06965000
PCCARV81 EQU   PCCA+32                                                  06966000
PCCAPSAR EQU   PCCA+28                                                  06967000
PCCACAFM EQU   PCCA+18                                                  06968000
PCCACPID EQU   PCCA+4                                                   06969000
PCCAPCCA EQU   PCCA                                                     06970000
@NM00115 EQU   CSD+160                                                  06971000
CSDMASK  EQU   CSD+128                                                  06972000
CSDUCNT  EQU   CSD+124                                                  06973000
CSDTCNT  EQU   CSD+120                                                  06974000
CSDDDRCT EQU   CSD+106                                                  06975000
CSDRV044 EQU   CSD+104                                                  06976000
CSDMAFF  EQU   CSD+24                                                   06977000
CSDRV038 EQU   CSDFLAGS                                                 06978000
CSDRV037 EQU   CSDFLAGS                                                 06979000
CSDRV036 EQU   CSDFLAGS                                                 06980000
CSDRV035 EQU   CSDFLAGS                                                 06981000
CSDRV034 EQU   CSDFLAGS                                                 06982000
CSDRV033 EQU   CSDFLAGS                                                 06983000
CSDRV032 EQU   CSDFLAGS                                                 06984000
CSDACR   EQU   CSD+22                                                   06985000
CSDMF1CP EQU   CSD+20                                                   06986000
CSDRV043 EQU   CSD+16                                                   06987000
CSDRV030 EQU   CSDSCFL4                                                 06988000
CSDRV029 EQU   CSDSCFL4                                                 06989000
CSDRV028 EQU   CSDSCFL4                                                 06990000
CSDRV027 EQU   CSDSCFL4                                                 06991000
CSDRV026 EQU   CSDSCFL4                                                 06992000
CSDRV025 EQU   CSDSCFL4                                                 06993000
CSDRV024 EQU   CSDSCFL4                                                 06994000
CSDRV023 EQU   CSDSCFL4                                                 06995000
CSDRV022 EQU   CSDSCFL3                                                 06996000
CSDRV021 EQU   CSDSCFL3                                                 06997000
CSDRV020 EQU   CSDSCFL3                                                 06998000
CSDRV019 EQU   CSDSCFL3                                                 06999000
CSDRV018 EQU   CSDSCFL3                                                 07000000
CSDRV017 EQU   CSDSCFL3                                                 07001000
CSDRV016 EQU   CSDSCFL3                                                 07002000
CSDRV015 EQU   CSDSCFL3                                                 07003000
CSDRV014 EQU   CSDSCFL2                                                 07004000
CSDRV013 EQU   CSDSCFL2                                                 07005000
CSDRV012 EQU   CSDSCFL2                                                 07006000
CSDRV011 EQU   CSDSCFL2                                                 07007000
CSDRV010 EQU   CSDSCFL2                                                 07008000
CSDRV009 EQU   CSDSCFL2                                                 07009000
CSDRV008 EQU   CSDSCFL2                                                 07010000
CSDRV007 EQU   CSDSCFL2                                                 07011000
CSDRV006 EQU   CSDSCFL1                                                 07012000
CSDRV005 EQU   CSDSCFL1                                                 07013000
CSDRV004 EQU   CSDSCFL1                                                 07014000
CSDRV003 EQU   CSDSCFL1                                                 07015000
CSDRV002 EQU   CSDSCFL1                                                 07016000
CSDRV001 EQU   CSDSCFL1                                                 07017000
CSDSYSND EQU   CSDSCFL1                                                 07018000
CSDRV042 EQU   CSDSCFL1                                                 07019000
CSDSAFF  EQU   CSDCPUAL                                                 07020000
CSDCHAD  EQU   CSD+6                                                    07021000
CSDCPUJS EQU   CSD+4                                                    07022000
CSDCSD   EQU   CSD                                                      07023000
ASCBEND  EQU   ASCB+208                                                 07024000
ASCBSRBT EQU   ASCB+200                                                 07025000
ASCBSWTL EQU   ASCB+196                                                 07026000
ASCBPER  EQU   ASCBSRBM                                                 07027000
@NM00114 EQU   ASCBSRBM                                                 07028000
ASCBSMCT EQU   ASCB+194                                                 07029000
ASCBRS12 EQU   ASCB+192                                                 07030000
ASCBPCTT EQU   ASCB+188                                                 07031000
ASCBVGTT EQU   ASCB+184                                                 07032000
ASCBRV43 EQU   ASCBSRQ4                                                 07033000
ASCBRV42 EQU   ASCBSRQ4                                                 07034000
ASCBRV41 EQU   ASCBSRQ4                                                 07035000
ASCBRV40 EQU   ASCBSRQ4                                                 07036000
ASCBRV39 EQU   ASCBSRQ4                                                 07037000
ASCBRV38 EQU   ASCBSRQ4                                                 07038000
ASCBRV37 EQU   ASCBSRQ4                                                 07039000
ASCBRV36 EQU   ASCBSRQ4                                                 07040000
ASCBRV35 EQU   ASCBSRQ3                                                 07041000
ASCBRV34 EQU   ASCBSRQ3                                                 07042000
ASCBRV33 EQU   ASCBSRQ3                                                 07043000
ASCBRV32 EQU   ASCBSRQ3                                                 07044000
ASCBRV31 EQU   ASCBSRQ3                                                 07045000
ASCBRV30 EQU   ASCBSRQ3                                                 07046000
ASCBRV29 EQU   ASCBSRQ3                                                 07047000
ASCBRV28 EQU   ASCBSRQ3                                                 07048000
ASCBRV27 EQU   ASCBSRQ2                                                 07049000
ASCBRV26 EQU   ASCBSRQ2                                                 07050000
ASCBRV25 EQU   ASCBSRQ2                                                 07051000
ASCBRV24 EQU   ASCBSRQ2                                                 07052000
ASCBRV23 EQU   ASCBSRQ2                                                 07053000
ASCBRV22 EQU   ASCBSRQ2                                                 07054000
ASCBRV21 EQU   ASCBSRQ2                                                 07055000
ASCBRV20 EQU   ASCBSRQ2                                                 07056000
ASCBRV19 EQU   ASCBSRQ1                                                 07057000
ASCBRV18 EQU   ASCBSRQ1                                                 07058000
ASCBRV17 EQU   ASCBSRQ1                                                 07059000
ASCBRV16 EQU   ASCBSRQ1                                                 07060000
ASCBRV15 EQU   ASCBSRQ1                                                 07061000
ASCBRV14 EQU   ASCBSRQ1                                                 07062000
ASCBRV13 EQU   ASCBSRQ1                                                 07063000
ASCBSTA  EQU   ASCBSRQ1                                                 07064000
ASCBJBNS EQU   ASCB+176                                                 07065000
ASCBJBNI EQU   ASCB+172                                                 07066000
ASCBMCC  EQU   ASCB+168                                                 07067000
ASCBRTWA EQU   ASCB+164                                                 07068000
ASCBIQEA EQU   ASCB+160                                                 07069000
ASCBXMPQ EQU   ASCB+156                                                 07070000
ASCBRS01 EQU   ASCB+154                                                 07071000
ASCBFMCT EQU   ASCB+152                                                 07072000
ASCBOUXB EQU   ASCB+148                                                 07073000
ASCBOUCB EQU   ASCB+144                                                 07074000
ASCBMECB EQU   ASCB+140                                                 07075000
ASCBQECB EQU   ASCB+136                                                 07076000
ASCBLSQH EQU   ASCBLKGP+4                                               07077000
ASCBLOCK EQU   ASCBLKGP                                                 07078000
ASCBTCBS EQU   ASCB+124                                                 07079000
ASCBNVSC EQU   ASCB+122                                                 07080000
ASCBVSC  EQU   ASCB+120                                                 07081000
ASCBSRBS EQU   ASCB+118                                                 07082000
ASCBSSRB EQU   ASCB+116                                                 07083000
ASCBRV06 EQU   ASCBFLG2                                                 07084000
ASCBRV05 EQU   ASCBFLG2                                                 07085000
ASCBRV04 EQU   ASCBFLG2                                                 07086000
ASCBSNQS EQU   ASCBFLG2                                                 07087000
ASCBS2S  EQU   ASCBFLG2                                                 07088000
ASCBCEXT EQU   ASCBFLG2                                                 07089000
ASCBPXMT EQU   ASCBFLG2                                                 07090000
ASCBXMPT EQU   ASCBFLG2                                                 07091000
ASCBRF07 EQU   ASCBDSP1                                                 07092000
ASCBRF06 EQU   ASCBDSP1                                                 07093000
ASCBRF05 EQU   ASCBDSP1                                                 07094000
ASCBRF04 EQU   ASCBDSP1                                                 07095000
ASCBRF03 EQU   ASCBDSP1                                                 07096000
ASCBRF02 EQU   ASCBDSP1                                                 07097000
ASCBFAIL EQU   ASCBDSP1                                                 07098000
ASCBNOQ  EQU   ASCBDSP1                                                 07099000
ASCBSWCT EQU   ASCB+112                                                 07100000
ASCBASXB EQU   ASCB+108                                                 07101000
ASCBTMCH EQU   ASCB+104                                                 07102000
ASCBNSWP EQU   ASCBFLG1                                                 07103000
ASCBTYP1 EQU   ASCBFLG1                                                 07104000
ASCBSTND EQU   ASCBFLG1                                                 07105000
ASCBABNT EQU   ASCBFLG1                                                 07106000
ASCBTERM EQU   ASCBFLG1                                                 07107000
ASCBS3S  EQU   ASCBFLG1                                                 07108000
ASCBCMSH EQU   ASCBFLG1                                                 07109000
ASCBTOFF EQU   ASCBFLG1                                                 07110000
ASCBRF01 EQU   ASCBRCTF                                                 07111000
ASCBTMLW EQU   ASCBRCTF                                                 07112000
ASCBOUT  EQU   ASCBRCTF                                                 07113000
ASCBWAIT EQU   ASCBRCTF                                                 07114000
ASCBRV08 EQU   ASCBRCTF                                                 07115000
ASCBFQU  EQU   ASCBRCTF                                                 07116000
ASCBFRS  EQU   ASCBRCTF                                                 07117000
ASCBTMNO EQU   ASCBRCTF                                                 07118000
ASCBAFFN EQU   ASCBFW1                                                  07119000
ASCBDUMP EQU   ASCB+96                                                  07120000
ASCBRV44 EQU   ASCB+92                                                  07121000
ASCBUBET EQU   ASCB+88                                                  07122000
ASCBECB  EQU   ASCB+84                                                  07123000
ASCBJSTL EQU   ASCB+80                                                  07124000
ASCBEWST EQU   ASCB+72                                                  07125000
ASCBTSB  EQU   ASCB+60                                                  07126000
ASCBCSCB EQU   ASCB+56                                                  07127000
ASCBRSMA EQU   ASCBRSM+1                                                07128000
ASCBRV54 EQU   ASCBRSMF                                                 07129000
ASCBRV53 EQU   ASCBRSMF                                                 07130000
ASCBRV52 EQU   ASCBRSMF                                                 07131000
ASCBRV51 EQU   ASCBRSMF                                                 07132000
ASCBVEQR EQU   ASCBRSMF                                                 07133000
ASCBN2LP EQU   ASCBRSMF                                                 07134000
ASCB1LPU EQU   ASCBRSMF                                                 07135000
ASCB2LPU EQU   ASCBRSMF                                                 07136000
ASCBLDA  EQU   ASCB+48                                                  07137000
ASCBSTOR EQU   ASCB+44                                                  07138000
ASCBDP   EQU   ASCB+43                                                  07139000
ASCBRV07 EQU   ASCB+42                                                  07140000
ASCBIOSM EQU   ASCB+40                                                  07141000
ASCBSEQN EQU   ASCB+38                                                  07142000
ASCBASID EQU   ASCB+36                                                  07143000
ASCBCPUS EQU   ASCB+32                                                  07144000
ASCBSPL  EQU   ASCB+28                                                  07145000
ASCBIOSP EQU   ASCB+24                                                  07146000
ASCBSYNC EQU   ASCBSUPC+4                                               07147000
ASCBSVRB EQU   ASCBSUPC                                                 07148000
ASCBCMSF EQU   ASCB+12                                                  07149000
ASCBBWDP EQU   ASCB+8                                                   07150000
ASCBFWDP EQU   ASCB+4                                                   07151000
ASCBASCB EQU   ASCB                                                     07152000
ASCBEGIN EQU   ASCB                                                     07153000
@NM00113 EQU   SRB+40                                                   07154000
@NM00112 EQU   SRB+38                                                   07155000
SRBPRIOR EQU   SRB+37                                                   07156000
SRBPKF   EQU   SRB+36                                                   07157000
SRBSAVE  EQU   SRB+32                                                   07158000
SRBRMTR  EQU   SRB+24                                                   07159000
SRBEP    EQU   SRB+20                                                   07160000
SRBPTCB  EQU   SRBFLC+4                                                 07161000
SRBPASID EQU   SRBFLC+2                                                 07162000
SRBCPAFF EQU   SRBFLC                                                   07163000
SRBASCB  EQU   SRB+8                                                    07164000
SRBFLNK  EQU   SRB+4                                                    07165000
SRBID    EQU   SRB                                                      07166000
@NM00111 EQU   PSA+3668                                                 07167000
PSASTAK  EQU   PSA+3072                                                 07168000
@NM00110 EQU   PSA+1048                                                 07169000
PSAUSEND EQU   PSA+1048                                                 07170000
PSARV100 EQU   PSA+1046                                                 07171000
PSACSID  EQU   PSA+1044                                                 07172000
PSACDAL  EQU   PSA+1040                                                 07173000
PSAWTCOD EQU   PSA+1036                                                 07174000
PSAATCVT EQU   PSA+1032                                                 07175000
PSAPCPSW EQU   PSA+1024                                                 07176000
PSAPIR2  EQU   PSA+1020                                                 07177000
PSARV059 EQU   PSA+1018                                                 07178000
PSASVC13 EQU   PSA+1016                                                 07179000
PSALSFCC EQU   PSA+1012                                                 07180000
PSASFACC EQU   PSA+1008                                                 07181000
PSASTOP  EQU   PSA+992                                                  07182000
PSASTART EQU   PSA+976                                                  07183000
PSARSPSW EQU   PSA+968                                                  07184000
PSASRPSW EQU   PSA+960                                                  07185000
PSARV045 EQU   PSA+892                                                  07186000
PSARV044 EQU   PSA+888                                                  07187000
PSARV043 EQU   PSA+884                                                  07188000
PSARV042 EQU   PSA+880                                                  07189000
PSARV041 EQU   PSA+876                                                  07190000
PSARV040 EQU   PSA+872                                                  07191000
PSARV025 EQU   PSA+868                                                  07192000
PSADSSED EQU   PSA+868                                                  07193000
PSADSSPR EQU   PSA+864                                                  07194000
PSADSSFW EQU   PSA+860                                                  07195000
PSADSS14 EQU   PSA+856                                                  07196000
PSADSSPP EQU   PSA+848                                                  07197000
PSADSSRP EQU   PSA+840                                                  07198000
PSADSS05 EQU   PSADSSF4                                                 07199000
PSADSS10 EQU   PSADSSF4                                                 07200000
PSADSSVE EQU   PSADSSF4                                                 07201000
PSADSSDE EQU   PSADSSF4                                                 07202000
PSADSSC0 EQU   PSADSSF4                                                 07203000
PSADSSIE EQU   PSADSSF4                                                 07204000
PSADSS12 EQU   PSADSSF4                                                 07205000
PSADSSRC EQU   PSADSSF4                                                 07206000
PSARV057 EQU   PSADSSF3                                                 07207000
PSARV056 EQU   PSADSSF3                                                 07208000
PSARV055 EQU   PSADSSF3                                                 07209000
PSADSSMC EQU   PSADSSF3                                                 07210000
PSADSSRW EQU   PSADSSF3                                                 07211000
PSADSSNM EQU   PSADSSF3                                                 07212000
PSADSSES EQU   PSADSSF3                                                 07213000
PSADSSGP EQU   PSADSSF3                                                 07214000
PSADSSF2 EQU   PSADSSFL+1                                               07215000
PSADSSPI EQU   PSADSSF1                                                 07216000
PSADSSOI EQU   PSADSSF1                                                 07217000
PSADSSSP EQU   PSADSSF1                                                 07218000
PSADSSTP EQU   PSADSSF1                                                 07219000
PSADSSDW EQU   PSADSSF1                                                 07220000
PSADSSDD EQU   PSADSSF1                                                 07221000
PSADSSDM EQU   PSADSSF1                                                 07222000
PSADSSMV EQU   PSADSSF1                                                 07223000
PSADSSTS EQU   PSA+816                                                  07224000
PSADSSWK EQU   PSA+812                                                  07225000
PSADSSR3 EQU   PSA+808                                                  07226000
PSADSSR2 EQU   PSA+804                                                  07227000
PSADSSRS EQU   PSA+800                                                  07228000
PSASTOR  EQU   PSA+796                                                  07229000
PSACPUSA EQU   PSA+794                                                  07230000
PSAVSTAP EQU   PSA+792                                                  07231000
PSAWKVAP EQU   PSA+788                                                  07232000
PSAWKRAP EQU   PSA+784                                                  07233000
PSAMCHIC EQU   PSA+783                                                  07234000
PSAACTCD EQU   PSA+782                                                  07235000
PSASYMSK EQU   PSA+781                                                  07236000
PSAMCHFL EQU   PSA+780                                                  07237000
PSACR0   EQU   PSA+776                                                  07238000
PSAPSWSV EQU   PSA+768                                                  07239000
PSACLHS  EQU   PSAHLHI                                                  07240000
PSALKR15 EQU   PSALKSA+60                                               07241000
PSALKR14 EQU   PSALKSA+56                                               07242000
PSALKR13 EQU   PSALKSA+52                                               07243000
PSALKR12 EQU   PSALKSA+48                                               07244000
PSALKR11 EQU   PSALKSA+44                                               07245000
PSALKR10 EQU   PSALKSA+40                                               07246000
PSALKR9  EQU   PSALKSA+36                                               07247000
PSALKR8  EQU   PSALKSA+32                                               07248000
PSALKR7  EQU   PSALKSA+28                                               07249000
PSALKR6  EQU   PSALKSA+24                                               07250000
PSALKR5  EQU   PSALKSA+20                                               07251000
PSALKR4  EQU   PSALKSA+16                                               07252000
PSALKR3  EQU   PSALKSA+12                                               07253000
PSALKR2  EQU   PSALKSA+8                                                07254000
PSALKR1  EQU   PSALKSA+4                                                07255000
PSALKR0  EQU   PSALKSA                                                  07256000
PSARV023 EQU   PSACLHT+52                                               07257000
PSALOCAL EQU   PSACLHT+48                                               07258000
PSACMSL  EQU   PSACLHT+44                                               07259000
PSAOPTL  EQU   PSACLHT+40                                               07260000
PSATPACL EQU   PSACLHT+36                                               07261000
PSATPDNL EQU   PSACLHT+32                                               07262000
PSATPNCL EQU   PSACLHT+28                                               07263000
PSAIOSLL EQU   PSACLHT+24                                               07264000
PSAIOSUL EQU   PSACLHT+20                                               07265000
PSAIOSCL EQU   PSACLHT+16                                               07266000
PSAIOSSL EQU   PSACLHT+12                                               07267000
PSASALCL EQU   PSACLHT+8                                                07268000
PSAASML  EQU   PSACLHT+4                                                07269000
PSADISPL EQU   PSACLHT                                                  07270000
PSASRSA  EQU   PSA+636                                                  07271000
PSARV050 EQU   PSA+635                                                  07272000
PSASNSM2 EQU   PSA+634                                                  07273000
PSADSSGO EQU   PSA+633                                                  07274000
PSARECUR EQU   PSA+632                                                  07275000
PSAHLHIS EQU   PSA+628                                                  07276000
PSAIPCSA EQU   PSA+624                                                  07277000
@NM00109 EQU   PSA+621                                                  07278000
PSAIPCDM EQU   PSA+620                                                  07279000
PSAIPCD  EQU   PSA+616                                                  07280000
@NM00108 EQU   PSA+613                                                  07281000
PSAIPCRM EQU   PSA+612                                                  07282000
PSAIPCR  EQU   PSA+608                                                  07283000
PSAMCHEX EQU   PSA+600                                                  07284000
PSAMPSW  EQU   PSA+592                                                  07285000
PSAEXPS2 EQU   PSA+584                                                  07286000
PSAEXPS1 EQU   PSA+576                                                  07287000
PSAPIREG EQU   PSA+572                                                  07288000
PSARSREG EQU   PSA+568                                                  07289000
PSAGPREG EQU   PSA+556                                                  07290000
PSARV022 EQU   PSASUP4                                                  07291000
PSARV021 EQU   PSASUP4                                                  07292000
PSARV020 EQU   PSASUP4                                                  07293000
PSARV019 EQU   PSASUP4                                                  07294000
PSARV018 EQU   PSASUP4                                                  07295000
PSARV017 EQU   PSASUP4                                                  07296000
PSARV016 EQU   PSASUP4                                                  07297000
PSALDWT  EQU   PSASUP4                                                  07298000
PSASLIP  EQU   PSASUP3                                                  07299000
PSAULCMS EQU   PSASUP3                                                  07300000
PSARV012 EQU   PSASUP3                                                  07301000
PSAESTA  EQU   PSASUP3                                                  07302000
PSASPR   EQU   PSASUP3                                                  07303000
PSAPSREG EQU   PSASUP3                                                  07304000
PSAPI2   EQU   PSASUP3                                                  07305000
PSAIOSUP EQU   PSASUP3                                                  07306000
PSALCR   EQU   PSASUP2                                                  07307000
PSARTM   EQU   PSASUP2                                                  07308000
PSAACR   EQU   PSASUP2                                                  07309000
PSAIPCE2 EQU   PSASUP2                                                  07310000
PSAIPCES EQU   PSASUP2                                                  07311000
PSAIPCEC EQU   PSASUP2                                                  07312000
PSAGTF   EQU   PSASUP2                                                  07313000
PSAIPCRI EQU   PSASUP2                                                  07314000
PSATYPE6 EQU   PSASUP1                                                  07315000
PSATCTL  EQU   PSASUP1                                                  07316000
PSADISP  EQU   PSASUP1                                                  07317000
PSALOCK  EQU   PSASUP1                                                  07318000
PSAPI    EQU   PSASUP1                                                  07319000
PSAEXT   EQU   PSASUP1                                                  07320000
PSASVC   EQU   PSASUP1                                                  07321000
PSAIO    EQU   PSASUP1                                                  07322000
PSAAOLD  EQU   PSA+548                                                  07323000
PSAANEW  EQU   PSA+544                                                  07324000
PSATOLD  EQU   PSA+540                                                  07325000
PSATNEW  EQU   PSA+536                                                  07326000
PSALCCAR EQU   PSA+532                                                  07327000
PSAPCCAR EQU   PSA+524                                                  07328000
PSAPCCAV EQU   PSA+520                                                  07329000
PSACPULA EQU   PSA+518                                                  07330000
PSACPUPA EQU   PSA+516                                                  07331000
PSAPSA   EQU   PSA+512                                                  07332000
FLCHDEND EQU   PSA+512                                                  07333000
FLCCRSAV EQU   FLCMCLA+280                                              07334000
FLCGRSAV EQU   FLCMCLA+216                                              07335000
FLCFPSAV EQU   FLCMCLA+184                                              07336000
FLCFLA   EQU   FLCMCLA+88                                               07337000
FLCRGNCD EQU   FLCMCLA+84                                               07338000
FLCFSAA  EQU   FLCFSA+1                                                 07339000
@NM00107 EQU   FLCFSA                                                   07340000
@NM00106 EQU   FLCMCLA+77                                               07341000
PSAMDISC EQU   PSAMEDC                                                  07342000
PSAMINTR EQU   PSAMEDC                                                  07343000
PSAMINST EQU   PSAMEDC                                                  07344000
PSAMCCF  EQU   PSAMEDC                                                  07345000
PSAMCNOP EQU   PSAMEDC                                                  07346000
PSAMEXSR EQU   PSAMEDC                                                  07347000
PSAMCOPR EQU   PSAMEDC                                                  07348000
@NM00105 EQU   PSAMEDC                                                  07349000
@NM00104 EQU   FLCMCLA+72                                               07350000
FLCMCIC  EQU   FLCMCLA+64                                               07351000
@NM00103 EQU   FLCMCLA+20                                               07352000
FLCIOAA  EQU   FLCIOA+1                                                 07353000
@NM00102 EQU   FLCIOA                                                   07354000
@NM00101 EQU   FLCMCLA+15                                               07355000
@NM00100 EQU   FLCMCLA+14                                               07356000
@NM00099 EQU   FLCMCLA+12                                               07357000
FLCLCL   EQU   FLCMCLA+8                                                07358000
FLCIOELA EQU   FLCIOEL+1                                                07359000
@NM00098 EQU   FLCIOEL                                                  07360000
FLCCHIL  EQU   FLCCHNID+2                                               07361000
FLCCHMOD EQU   FLCCHTM                                                  07362000
FLCCHTYP EQU   FLCCHTM                                                  07363000
@NM00097 EQU   PSA+160                                                  07364000
FLCMTRCD EQU   PSA+157                                                  07365000
@NM00096 EQU   PSA+156                                                  07366000
FLCPERA  EQU   FLCPER+1                                                 07367000
@NM00095 EQU   FLCPER                                                   07368000
@NM00094 EQU   PSA+151                                                  07369000
FLCPERCD EQU   PSA+150                                                  07370000
FLCMCNUM EQU   PSA+149                                                  07371000
@NM00093 EQU   PSA+148                                                  07372000
FLCTEAA  EQU   FLCTEA+1                                                 07373000
@NM00092 EQU   FLCTEA                                                   07374000
PSAPIPC  EQU   PSAPICOD                                                 07375000
PSAPIMC  EQU   PSAPICOD                                                 07376000
PSAPIPER EQU   PSAPICOD                                                 07377000
PSARV049 EQU   FLCPICOD                                                 07378000
FLCPILCB EQU   FLCPIILC                                                 07379000
@NM00091 EQU   FLCPIILC                                                 07380000
@NM00090 EQU   PSAEPPSW                                                 07381000
FLCSVCN  EQU   PSAESPSW+2                                               07382000
FLCSILCB EQU   FLCSVILC                                                 07383000
@NM00089 EQU   FLCSVILC                                                 07384000
@NM00088 EQU   PSAESPSW                                                 07385000
FLCEICOD EQU   PSAEEPSW+2                                               07386000
PSASPAD  EQU   PSAEEPSW                                                 07387000
@NM00087 EQU   PSA+128                                                  07388000
FLCINPSW EQU   PSA+120                                                  07389000
FLCMNPSW EQU   PSA+112                                                  07390000
FLCPNPSW EQU   PSA+104                                                  07391000
FLCSNPSW EQU   PSA+96                                                   07392000
FLCENPSW EQU   PSA+88                                                   07393000
FLCTRACE EQU   PSA+84                                                   07394000
FLCTIMER EQU   PSA+80                                                   07395000
FLCCVT2  EQU   PSA+76                                                   07396000
FLCCAW   EQU   PSA+72                                                   07397000
FLCCSW   EQU   PSA+64                                                   07398000
FLCIOPSW EQU   PSA+56                                                   07399000
FLCMOPSW EQU   PSA+48                                                   07400000
FLCPOPSW EQU   PSA+40                                                   07401000
FLCSOPSW EQU   PSA+32                                                   07402000
FLCEOPSW EQU   PSA+24                                                   07403000
@NM00086 EQU   FLCICCW2+4                                               07404000
FLCCVT   EQU   FLCICCW2                                                 07405000
FLCICCW1 EQU   FLCROPSW                                                 07406000
FLCIPPSW EQU   FLCRNPSW                                                 07407000
ASVTEND  EQU   ASVT+528                                                 07408000
ASVTAVAI EQU   ASVTFRST                                                 07409000
ASVTRS00 EQU   ASVT+520                                                 07410000
ASVTASVT EQU   ASVT+512                                                 07411000
ASVTBEGN EQU   ASVT+512                                                 07412000
@NM00085 EQU   ASVT                                                     07413000
LCCAPERA EQU   LCCA+964                                                 07414000
LCCAPERC EQU   LCCA+962                                                 07415000
LCCAR167 EQU   LCCA+960                                                 07416000
LCCASGPR EQU   LCCA+896                                                 07417000
LCCADRT2 EQU   LCCA+888                                                 07418000
LCCADRT1 EQU   LCCA+880                                                 07419000
LCCAR103 EQU   LCCA+876                                                 07420000
LCCAESS2 EQU   LCCA+872                                                 07421000
LCCASPLJ EQU   LCCA+868                                                 07422000
LCCASMQJ EQU   LCCA+864                                                 07423000
LCCAIRT  EQU   LCCA+736                                                 07424000
LCCAASID EQU   LCCA+732                                                 07425000
LCCARV89 EQU   LCCA+728                                                 07426000
LCCAPGTA EQU   LCCASRBF+2                                               07427000
LCCASAFN EQU   LCCASRBF                                                 07428000
LCCAECSA EQU   LCCA+716                                                 07429000
LCCAICR0 EQU   LCCA+712                                                 07430000
LCCALWTM EQU   LCCA+704                                                 07431000
LCCASLIP EQU   LCCA+700                                                 07432000
LCCAPINV EQU   LCCA+696                                                 07433000
LCCARV88 EQU   LCCA+695                                                 07434000
LCCARV87 EQU   LCCALKFG                                                 07435000
LCCARV86 EQU   LCCALKFG                                                 07436000
LCCARV85 EQU   LCCALKFG                                                 07437000
LCCARV84 EQU   LCCALKFG                                                 07438000
LCCALKRD EQU   LCCALKFG                                                 07439000
LCCALKAQ EQU   LCCALKFG                                                 07440000
LCCALKSA EQU   LCCALKFG                                                 07441000
LCCALKDP EQU   LCCALKFG                                                 07442000
LCCACRST EQU   LCCACREX                                                 07443000
LCCACRDP EQU   LCCACREX                                                 07444000
LCCACRLM EQU   LCCACREX                                                 07445000
LCCACRIN EQU   LCCACREX                                                 07446000
LCCACRRT EQU   LCCACREX                                                 07447000
LCCACRLE EQU   LCCACREX                                                 07448000
LCCACRRM EQU   LCCACREX                                                 07449000
LCCACREF EQU   LCCACREX                                                 07450000
LCCAVARY EQU   LCCACRFL                                                 07451000
LCCARV73 EQU   LCCACRFL                                                 07452000
LCCARV72 EQU   LCCACRFL                                                 07453000
LCCARV71 EQU   LCCACRFL                                                 07454000
LCCARV70 EQU   LCCACRFL                                                 07455000
LCCARV69 EQU   LCCACRFL                                                 07456000
LCCACLMS EQU   LCCACRFL                                                 07457000
LCCACRTM EQU   LCCACRFL                                                 07458000
LCCALCR0 EQU   LCCA+688                                                 07459000
LCCACRLC EQU   LCCA+684                                                 07460000
LCCARCPU EQU   LCCA+680                                                 07461000
LCCADCPU EQU   LCCA+676                                                 07462000
LCCASRBJ EQU   LCCA+672                                                 07463000
LCCADSSR EQU   LCCA+668                                                 07464000
LCCADSSC EQU   LCCA+660                                                 07465000
LCCADSS3 EQU   LCCA+648                                                 07466000
LCCADSS2 EQU   LCCA+636                                                 07467000
LCCADSS1 EQU   LCCA+624                                                 07468000
LCCAITOD EQU   LCCA+608                                                 07469000
LCCASTOD EQU   LCCA+592                                                 07470000
LCCAR171 EQU   LCCA+588                                                 07471000
LCCAR170 EQU   LCCA+584                                                 07472000
LCCAR169 EQU   LCCA+580                                                 07473000
LCCAR168 EQU   LCCA+576                                                 07474000
LCCARPR5 EQU   LCCA+572                                                 07475000
LCCARPR4 EQU   LCCA+568                                                 07476000
LCCARPR3 EQU   LCCA+564                                                 07477000
LCCARPR2 EQU   LCCA+560                                                 07478000
LCCADSR5 EQU   LCCA+556                                                 07479000
LCCADSR4 EQU   LCCA+552                                                 07480000
LCCADSR3 EQU   LCCA+548                                                 07481000
LCCADSR2 EQU   LCCA+544                                                 07482000
LCCARV68 EQU   LCCA+543                                                 07483000
LCCAPSMK EQU   LCCA+542                                                 07484000
LCCARV67 EQU   LCCADSF2                                                 07485000
LCCARV66 EQU   LCCADSF2                                                 07486000
LCCARV65 EQU   LCCADSF2                                                 07487000
LCCARV64 EQU   LCCADSF2                                                 07488000
LCCADSRW EQU   LCCADSF2                                                 07489000
LCCADSPL EQU   LCCADSF2                                                 07490000
LCCAGSRB EQU   LCCADSF2                                                 07491000
LCCASRBM EQU   LCCADSF2                                                 07492000
LCCARV61 EQU   LCCADSF1                                                 07493000
LCCARV60 EQU   LCCADSF1                                                 07494000
LCCARV59 EQU   LCCADSF1                                                 07495000
LCCARV58 EQU   LCCADSF1                                                 07496000
LCCADSS  EQU   LCCADSF1                                                 07497000
LCCAVCPU EQU   LCCADSF1                                                 07498000
LCCAACR  EQU   LCCADSF1                                                 07499000
LCCACPUS EQU   LCCA+536                                                 07500000
LCCAASCP EQU   LCCA+532                                                 07501000
LCCAESSA EQU   LCCA+528                                                 07502000
LCCARV55 EQU   LCCASPN4                                                 07503000
LCCARV54 EQU   LCCASPN4                                                 07504000
LCCARV53 EQU   LCCASPN4                                                 07505000
LCCARV52 EQU   LCCASPN4                                                 07506000
LCCARV51 EQU   LCCASPN4                                                 07507000
LCCARV50 EQU   LCCASPN4                                                 07508000
LCCARV49 EQU   LCCASPN4                                                 07509000
LCCARV48 EQU   LCCASPN4                                                 07510000
LCCARV47 EQU   LCCASPN3                                                 07511000
LCCARV46 EQU   LCCASPN3                                                 07512000
LCCARV45 EQU   LCCASPN3                                                 07513000
LCCARV44 EQU   LCCASPN3                                                 07514000
LCCARV43 EQU   LCCASPN3                                                 07515000
LCCARV42 EQU   LCCASPN3                                                 07516000
LCCARV41 EQU   LCCASPN3                                                 07517000
LCCARV40 EQU   LCCASPN3                                                 07518000
LCCARV39 EQU   LCCASPN2                                                 07519000
LCCARV38 EQU   LCCASPN2                                                 07520000
LCCARV37 EQU   LCCASPN2                                                 07521000
LCCARV36 EQU   LCCASPN2                                                 07522000
LCCARV35 EQU   LCCASPN2                                                 07523000
LCCARV34 EQU   LCCASPN2                                                 07524000
LCCARV33 EQU   LCCASPN2                                                 07525000
LCCARV32 EQU   LCCASPN2                                                 07526000
LCCAEXSN EQU   LCCASPN1                                                 07527000
LCCARV30 EQU   LCCASPN1                                                 07528000
LCCAMFIO EQU   LCCASPN1                                                 07529000
LCCARSTR EQU   LCCASPN1                                                 07530000
LCCATSPN EQU   LCCASPN1                                                 07531000
LCCALOCK EQU   LCCASPN1                                                 07532000
LCCASIGP EQU   LCCASPN1                                                 07533000
LCCAPTLB EQU   LCCASPN1                                                 07534000
LCCARV27 EQU   LCCAIHR4                                                 07535000
LCCARV26 EQU   LCCAIHR4                                                 07536000
LCCARV25 EQU   LCCAIHR4                                                 07537000
LCCARV24 EQU   LCCAIHR4                                                 07538000
LCCARV23 EQU   LCCAIHR4                                                 07539000
LCCARV22 EQU   LCCAIHR4                                                 07540000
LCCARV21 EQU   LCCAIHR4                                                 07541000
LCCARV20 EQU   LCCAIHR4                                                 07542000
LCCARV19 EQU   LCCAIHR3                                                 07543000
LCCARV18 EQU   LCCAIHR3                                                 07544000
LCCARV17 EQU   LCCAIHR3                                                 07545000
LCCARV16 EQU   LCCAIHR3                                                 07546000
LCCARV15 EQU   LCCAIHR3                                                 07547000
LCCARV14 EQU   LCCAIHR3                                                 07548000
LCCARV13 EQU   LCCAIHR3                                                 07549000
LCCARV12 EQU   LCCAIHR3                                                 07550000
LCCARV11 EQU   LCCAIHR2                                                 07551000
LCCARV10 EQU   LCCAIHR2                                                 07552000
LCCARV09 EQU   LCCAIHR2                                                 07553000
LCCARV08 EQU   LCCAIHR2                                                 07554000
LCCARV07 EQU   LCCAIHR2                                                 07555000
LCCARV06 EQU   LCCAIHR2                                                 07556000
LCCARV05 EQU   LCCAIHR2                                                 07557000
LCCARV04 EQU   LCCAIHR2                                                 07558000
LCCARV03 EQU   LCCAIHR1                                                 07559000
LCCARV02 EQU   LCCAIHR1                                                 07560000
LCCARV01 EQU   LCCAIHR1                                                 07561000
LCCAPPIE EQU   LCCAIHR1                                                 07562000
LCCAPSG1 EQU   LCCAIHR1                                                 07563000
LCCAPDAT EQU   LCCAIHR1                                                 07564000
LCCAXRC2 EQU   LCCAIHR1                                                 07565000
LCCAXRC1 EQU   LCCAIHR1                                                 07566000
LCCAIOPS EQU   LCCA+512                                                 07567000
LCCAGPGR EQU   LCCA+448                                                 07568000
LCCAR133 EQU   LCCA+444                                                 07569000
LCCAR132 EQU   LCCA+440                                                 07570000
LCCARIR7 EQU   LCCA+436                                                 07571000
LCCARIR6 EQU   LCCA+432                                                 07572000
LCCARIR5 EQU   LCCA+428                                                 07573000
LCCARIR4 EQU   LCCA+424                                                 07574000
LCCARIR3 EQU   LCCA+420                                                 07575000
LCCARIR2 EQU   LCCA+416                                                 07576000
LCCARSGR EQU   LCCA+352                                                 07577000
LCCAXGR3 EQU   LCCA+288                                                 07578000
LCCAXGR2 EQU   LCCA+224                                                 07579000
LCCAXGR1 EQU   LCCA+160                                                 07580000
LCCACR0  EQU   LCCA+156                                                 07581000
LCCAMCR1 EQU   LCCA+152                                                 07582000
LCCAPVAD EQU   LCCA+148                                                 07583000
LCCAPINT EQU   LCCA+144                                                 07584000
LCCAPPSW EQU   LCCA+136                                                 07585000
LCCAPGR2 EQU   LCCA+72                                                  07586000
LCCAPGR1 EQU   LCCA+8                                                   07587000
LCCARV77 EQU   LCCA+6                                                   07588000
LCCACPUA EQU   LCCA+4                                                   07589000
LCCALCCA EQU   LCCA                                                     07590000
BASPWD0G EQU   BASES01+72                                               07591000
BASPWD0F EQU   BASES01+68                                               07592000
BASPWD0E EQU   BASES01+64                                               07593000
BASPWD0D EQU   BASES01+60                                               07594000
BASPWD0C EQU   BASES01+56                                               07595000
BASPWD0B EQU   BASES01+52                                               07596000
BASPWD0A EQU   BASES01+48                                               07597000
BAMTVTCB EQU   BASES01+44                                               07598000
BAMTINLN EQU   BAMTINIT+1                                               07599000
BAMTINSP EQU   BAMTINIT                                                 07600000
@NM00084 EQU   BAMTDSW                                                  07601000
BAMTDP   EQU   BAMTDSW                                                  07602000
BAMTDW   EQU   BAMTDSW                                                  07603000
BASPBT04 EQU   BASES01+35                                               07604000
@NM00083 EQU   BAMTITFL                                                 07605000
BAMTFRRR EQU   BAMTITFL                                                 07606000
BAMTSFRR EQU   BAMTITFL                                                 07607000
BAMTITAB EQU   BAMTITFL                                                 07608000
@NM00082 EQU   BAMTRECF                                                 07609000
BAMTRUNK EQU   BAMTRECF                                                 07610000
BAMTR816 EQU   BAMTRECF                                                 07611000
BAMTR809 EQU   BAMTRECF                                                 07612000
BAMTR808 EQU   BAMTRECF                                                 07613000
@NM00081 EQU   BAMTCNTL                                                 07614000
BAMTRTRY EQU   BAMTCNTL                                                 07615000
BAMT816  EQU   BAMTCNTL                                                 07616000
BAMT809  EQU   BAMTCNTL                                                 07617000
BAMT808  EQU   BAMTCNTL                                                 07618000
BAMTDTLN EQU   BAMTDTSZ+1                                               07619000
BAMTDTSP EQU   BAMTDTSZ                                                 07620000
BAMTDTAB EQU   BASES01+24                                               07621000
BASTSZ   EQU   BASTSPSZ+1                                               07622000
BASTSP   EQU   BASTSPSZ                                                 07623000
BAMTLEN  EQU   BAMTSPSZ+1                                               07624000
BAMTSP   EQU   BAMTSPSZ                                                 07625000
BAMTTBL  EQU   BASES01+4                                                07626000
MSSUM    EQU   MSECBFL                                                  07627000
@NM00080 EQU   MSECBFL                                                  07628000
@NM00079 EQU   MSECBFL                                                  07629000
@NM00078 EQU   MSECBFL                                                  07630000
@NM00077 EQU   MSECBFL                                                  07631000
MSWTL    EQU   MSECBFL                                                  07632000
@NM00076 EQU   MSECBFL                                                  07633000
@NM00075 EQU   MSECBFL                                                  07634000
BASPBT03 EQU   BASES01+2                                                07635000
@NM00074 EQU   MSBTN                                                    07636000
MSTN     EQU   MSBTN                                                    07637000
@NM00073 EQU   MSBTN                                                    07638000
@NM00072 EQU   MSBTN                                                    07639000
@NM00071 EQU   MSBTN                                                    07640000
@NM00070 EQU   MSBTN                                                    07641000
@NM00069 EQU   MSBTN                                                    07642000
@NM00068 EQU   MSBTN                                                    07643000
BASPBT02 EQU   BASES01                                                  07644000
@NM00067 EQU   MS1BASEX+56                                              07645000
@NM00066 EQU   MS1BASEX+40                                              07646000
@NM00065 EQU   MS1BASEX+20                                              07647000
@NM00064 EQU   MS1BASEX                                                 07648000
BASPWD08 EQU   BASE+132                                                 07649000
BASPWD07 EQU   BASE+128                                                 07650000
MSLGJSCB EQU   BASE+124                                                 07651000
@NM00063 EQU   MSSTRTSW                                                 07652000
MSSTRTP  EQU   MSSTRTSW                                                 07653000
MSSTRTW  EQU   MSSTRTSW                                                 07654000
MSSTRTRB EQU   MSLGWTR+1                                                07655000
@NM00062 EQU   MSWTRSW                                                  07656000
MSWTRP   EQU   MSWTRSW                                                  07657000
MSWTRW   EQU   MSWTRSW                                                  07658000
@NM00061 EQU   MSWLOGSW                                                 07659000
MSWLOGP  EQU   MSWLOGSW                                                 07660000
MSWLOGW  EQU   MSWLOGSW                                                 07661000
@NM00060 EQU   MSCLOSSW                                                 07662000
MSCLOSP  EQU   MSCLOSSW                                                 07663000
MSCLOSW  EQU   MSCLOSSW                                                 07664000
MSLOGLMT EQU   @NM00059+1                                               07665000
MSLOGCLS EQU   @NM00059                                                 07666000
BAASCB   EQU   BASE+100                                                 07667000
MSSMFPRM EQU   BASE+96                                                  07668000
MSCOMMND EQU   BASE+92                                                  07669000
BASPHW03 EQU   BASE+90                                                  07670000
BASTCNT  EQU   BASE+88                                                  07671000
BASESCT  EQU   BASE+86                                                  07672000
BAJNCNT  EQU   BASE+84                                                  07673000
BASUBECB EQU   BASE+80                                                  07674000
BALOGCOM EQU   BASE+76                                                  07675000
BASPWD06 EQU   BASE+72                                                  07676000
BASPWD05 EQU   BASE+68                                                  07677000
BASPWD04 EQU   BASE+64                                                  07678000
BASPWD03 EQU   BASE+60                                                  07679000
BABCMAX  EQU   BASE+58                                                  07680000
BAMONTR2 EQU   BASE+57                                                  07681000
@NM00058 EQU   BAMONITR                                                 07682000
@NM00057 EQU   BAMONITR                                                 07683000
@NM00056 EQU   BAMONITR                                                 07684000
@NM00055 EQU   BAMONITR                                                 07685000
@NM00054 EQU   BAMONITR                                                 07686000
@NM00053 EQU   BAMONITR                                                 07687000
BASPACE  EQU   BAMONITR                                                 07688000
BADSN    EQU   BAMONITR                                                 07689000
BASPWD02 EQU   BASE+52                                                  07690000
BALOGECB EQU   BASE+48                                                  07691000
@NM00052 EQU   BASPBYTE                                                 07692000
BAMASCH  EQU   BASPBYTE                                                 07693000
BATRACE  EQU   BASPBYTE                                                 07694000
BAMSSTAR EQU   BASPBYTE                                                 07695000
@NM00051 EQU   MSLOGST                                                  07696000
@NM00050 EQU   MSLOGST                                                  07697000
MSLOGSTA EQU   MSLOGST                                                  07698000
MSLOGIPL EQU   MSLOGST                                                  07699000
MSLOGDAR EQU   MSLOGST                                                  07700000
MSLOGCOM EQU   MSLOGST                                                  07701000
MSLOGTHD EQU   MSLOGST                                                  07702000
MSLOGENQ EQU   MSLOGST                                                  07703000
BASPHW02 EQU   BASE+44                                                  07704000
BASPHW01 EQU   BASE+42                                                  07705000
BAPKES   EQU   BASE+40                                                  07706000
BAICTR   EQU   BASE+38                                                  07707000
BASPBT01 EQU   BASE+37                                                  07708000
@NM00049 EQU   BARSW                                                    07709000
BASPWD01 EQU   BASE+32                                                  07710000
@NM00048 EQU   BALOG                                                    07711000
@NM00047 EQU   BALOG                                                    07712000
@NM00046 EQU   BALOG                                                    07713000
@NM00045 EQU   BALOG                                                    07714000
@NM00044 EQU   BALOG                                                    07715000
@NM00043 EQU   BALOG                                                    07716000
@NM00042 EQU   BALOG                                                    07717000
BALOGINT EQU   BALOG                                                    07718000
MSLOGSVC EQU   BALGSTAT                                                 07719000
BAPRC    EQU   BASE+20                                                  07720000
BAQ      EQU   BASE+16                                                  07721000
@NM00041 EQU   @NM00040                                                 07722000
BAIPL    EQU   @NM00039                                                 07723000
BALAD    EQU   BASE+8                                                   07724000
BACBID   EQU   BASE+4                                                   07725000
BACHN    EQU   BASE                                                     07726000
WPLMLTXT EQU   WPLML+4                                                  07727000
WPLMLLT2 EQU   WPLMLLTF+1                                               07728000
WPLRSV24 EQU   WPLMLLT1                                                 07729000
WPLRSV23 EQU   WPLMLLT1                                                 07730000
WPLRSV22 EQU   WPLMLLT1                                                 07731000
WPLRSV21 EQU   WPLMLLT1                                                 07732000
WPLMLLTD EQU   WPLMLLT1                                                 07733000
WPLMLLTC EQU   WPLMLLT1                                                 07734000
WPLMLLTB EQU   WPLMLLT1                                                 07735000
WPLMLLTA EQU   WPLMLLT1                                                 07736000
WPLMLLEN EQU   WPLML+1                                                  07737000
WPLML0   EQU   WPLML                                                    07738000
WPLLINES EQU   WPLLS01+3                                                07739000
WPLAREA  EQU   WPLLS01+2                                                07740000
WPLLTF2  EQU   WPLLTF+1                                                 07741000
WPLRSV20 EQU   WPLLTF1                                                  07742000
WPLRSV19 EQU   WPLLTF1                                                  07743000
WPLRSV18 EQU   WPLLTF1                                                  07744000
WPLRSV17 EQU   WPLLTF1                                                  07745000
WPLLTFD  EQU   WPLLTF1                                                  07746000
WPLLTFC  EQU   WPLLTF1                                                  07747000
WPLLTFB  EQU   WPLLTF1                                                  07748000
WPLLTFA  EQU   WPLLTF1                                                  07749000
WPLQID   EQU   WPLFLGS+6                                                07750000
WPLRSV32 EQU   WPLMSGT2                                                 07751000
WPLRSV31 EQU   WPLMSGT2                                                 07752000
WPLRSV30 EQU   WPLMSGT2                                                 07753000
WPLRSV29 EQU   WPLMSGT2                                                 07754000
WPLRSV28 EQU   WPLMSGT2                                                 07755000
WPLRSV27 EQU   WPLMSGT2                                                 07756000
WPLRSV26 EQU   WPLMSGT2                                                 07757000
WPLRSV25 EQU   WPLMSGT2                                                 07758000
WPLRSV16 EQU   WPLMSGT1                                                 07759000
WPLRSV15 EQU   WPLMSGT1                                                 07760000
WPLMSGTF EQU   WPLMSGT1                                                 07761000
WPLRSV14 EQU   WPLMSGT1                                                 07762000
WPLMSGTD EQU   WPLMSGT1                                                 07763000
WPLMSGTC EQU   WPLMSGT1                                                 07764000
WPLMSGTB EQU   WPLMSGT1                                                 07765000
WPLMSGTA EQU   WPLMSGT1                                                 07766000
WPLRSV13 EQU   WPLROUT2                                                 07767000
WPLROUTO EQU   WPLROUT2                                                 07768000
WPLROUTN EQU   WPLROUT2                                                 07769000
WPLROUTM EQU   WPLROUT2                                                 07770000
WPLROUTL EQU   WPLROUT2                                                 07771000
WPLROUTK EQU   WPLROUT2                                                 07772000
WPLROUTJ EQU   WPLROUT2                                                 07773000
WPLROUTI EQU   WPLROUT2                                                 07774000
WPLROUTH EQU   WPLROUT1                                                 07775000
WPLROUTG EQU   WPLROUT1                                                 07776000
WPLROUTF EQU   WPLROUT1                                                 07777000
WPLROUTE EQU   WPLROUT1                                                 07778000
WPLROUTD EQU   WPLROUT1                                                 07779000
WPLROUTC EQU   WPLROUT1                                                 07780000
WPLROUTB EQU   WPLROUT1                                                 07781000
WPLROUTA EQU   WPLROUT1                                                 07782000
WPLRSV12 EQU   WPLDESC2                                                 07783000
WPLRSV11 EQU   WPLDESC2                                                 07784000
WPLRSV10 EQU   WPLDESC2                                                 07785000
WPLRSV09 EQU   WPLDESC2                                                 07786000
WPLRSV08 EQU   WPLDESC2                                                 07787000
WPLDESCK EQU   WPLDESC2                                                 07788000
WPLDESCJ EQU   WPLDESC2                                                 07789000
WPLDESCI EQU   WPLDESC2                                                 07790000
WPLDESCH EQU   WPLDESC1                                                 07791000
WPLDESCG EQU   WPLDESC1                                                 07792000
WPLDESCF EQU   WPLDESC1                                                 07793000
WPLDESCE EQU   WPLDESC1                                                 07794000
WPLDESCD EQU   WPLDESC1                                                 07795000
WPLDESCC EQU   WPLDESC1                                                 07796000
WPLDESCB EQU   WPLDESC1                                                 07797000
WPLDESCA EQU   WPLDESC1                                                 07798000
WPLTXTL  EQU   WPLTXT+125                                               07799000
@NM00038 EQU   WPLTXT                                                   07800000
WPLRSV05 EQU   WPLMCSF2                                                 07801000
WPLRSV04 EQU   WPLMCSF2                                                 07802000
WPLMCSFN EQU   WPLMCSF2                                                 07803000
WPLRSV03 EQU   WPLMCSF2                                                 07804000
WPLNOWTP EQU   WPLMCSF2                                                 07805000
WPLMCSFK EQU   WPLMCSF2                                                 07806000
WPLMCSFJ EQU   WPLMCSF2                                                 07807000
WPLMCSFI EQU   WPLMCSF2                                                 07808000
WPLMCSFH EQU   WPLMCSF1                                                 07809000
WPLMCSFG EQU   WPLMCSF1                                                 07810000
WPLMCSFF EQU   WPLMCSF1                                                 07811000
WPLMCSFE EQU   WPLMCSF1                                                 07812000
WPLMCSFD EQU   WPLMCSF1                                                 07813000
WPLMCSFC EQU   WPLMCSF1                                                 07814000
WPLMCSFB EQU   WPLMCSF1                                                 07815000
WPLMCSFA EQU   WPLMCSF1                                                 07816000
WPLLGH   EQU   WPL                                                      07817000
WPLRLN   EQU   WPLRPTR                                                  07818000
TPCOCL   EQU   IEATPC+416                                               07819000
TPCSDIE  EQU   IEATPC+412                                               07820000
TPCCRSAV EQU   IEATPC+352                                               07821000
TPCFRRP  EQU   IEATPC+348                                               07822000
TPCVPTR  EQU   IEATPC+344                                               07823000
TPCMISC  EQU   IEATPC+272                                               07824000
TPCWORKR EQU   TPCWORK+4                                                07825000
TPCWORKL EQU   TPCWORK                                                  07826000
TPCSAVE2 EQU   IEATPC+200                                               07827000
TPCSAVE1 EQU   IEATPC+136                                               07828000
@NM00037 EQU   TPCLMTQE+16                                              07829000
@NM00036 EQU   TPCLMTQE+15                                              07830000
@NM00035 EQU   TPCLMTQE+14                                              07831000
@NM00034 EQU   TPCLMTQE+12                                              07832000
@NM00033 EQU   TPCLMTQE+8                                               07833000
@NM00032 EQU   TPCLMTQE+4                                               07834000
@NM00031 EQU   TPCLMTQE                                                 07835000
@NM00030 EQU   IEATSELM+16                                              07836000
@NM00029 EQU   IEATSELM+15                                              07837000
@NM00028 EQU   IEATSELM+14                                              07838000
@NM00027 EQU   IEATSELM+12                                              07839000
@NM00026 EQU   IEATSELM+8                                               07840000
@NM00025 EQU   IEATSELM+4                                               07841000
@NM00024 EQU   IEATSELM                                                 07842000
@NM00023 EQU   TPCMFTQE+16                                              07843000
@NM00022 EQU   TPCMFTQE+15                                              07844000
@NM00021 EQU   TPCMFTQE+14                                              07845000
@NM00020 EQU   TPCMFTQE+12                                              07846000
@NM00019 EQU   TPCMFTQE+8                                               07847000
@NM00018 EQU   TPCMFTQE+4                                               07848000
@NM00017 EQU   TPCMFTQE                                                 07849000
MNIGHTRH EQU   MNIGHT+4                                                 07850000
@NM00016 EQU   TPCMNTQE+15                                              07851000
@NM00015 EQU   TPCMNTQE+14                                              07852000
@NM00014 EQU   TPCMNTQE+12                                              07853000
@NM00013 EQU   TPCMNTQE+8                                               07854000
@NM00012 EQU   TPCMNTQE+4                                               07855000
@NM00011 EQU   TPCMNTQE                                                 07856000
@NM00010 EQU   TPCDMTQE+16                                              07857000
@NM00009 EQU   TPCDMTQE+15                                              07858000
@NM00008 EQU   TPCDMTQE+14                                              07859000
@NM00007 EQU   TPCDMTQE+12                                              07860000
@NM00006 EQU   TPCDMTQE+8                                               07861000
@NM00005 EQU   TPCDMTQE                                                 07862000
TPCCC    EQU   IEATPC+7                                                 07863000
@NM00004 EQU   IEATPC+5                                                 07864000
@NM00003 EQU   TPCFLGS1                                                 07865000
TPCTPCA  EQU   IEATPC                                                   07866000
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS      07867000
@RC00199 EQU   @RC00188                                                 07868000
@RF00304 EQU   @RC00301                                                 07869000
@RC00360 EQU   @RC00358                                                 07870000
@RF00366 EQU   @RC00358                                                 07871000
@RC00517 EQU   @RC00515                                                 07872000
@RC00496 EQU   @RC00494                                                 07873000
@PB00006 EQU   @EL00005                                                 07874000
@PB00007 EQU   @EC00829                                                 07875000
@RF00920 EQU   @EL00001                                                 07876000
@RC01062 EQU   @EL00008                                                 07877000
@PB00005 EQU   @PB00007                                                 07878000
@PB00004 EQU   @PB00005                                                 07879000
@ENDDATA EQU   *                                                        07880000
         END   IEAVRTOD,(C'PLS1826',0801,78285)                         07881000
??
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(TMVS816 TMVS817)
         .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
 APPLY
       SELECT(TMVS816 TMVS817)
       CHECK
       BYPASS(ID)
       .
/*
//*
//APPLY   EXEC SMPAPP,WORK='SYSALLDA',COND=(0,NE)
//SYSUT1   DD  UNIT=&WORK,SPACE=(TRK,(200,100))
//SYSUT2   DD  UNIT=&WORK,SPACE=(TRK,(200,100))
//SYSUT3   DD  UNIT=&WORK,SPACE=(TRK,(200,100))
//SMPCNTL  DD  *
 APPLY
       SELECT(TMVS816 TMVS817)
       DIS(WRITE)
       .
/*
//
//TTSO801  JOB (SYSGEN),'J03 M13: TTSO801',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2009/10/20 @kl TTSO801 eliminate msgikt012d after p tso
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TTSO801)     /* REWORK(20091020) */             .
++ VER (Z038)
   FMID(ETV0108)
 /*
  PROBLEM DESCRIPTION(S):
    TTSO801 -
      Eliminate msgIKT012D after P TSO.

  COMPONENT:  5752-SC1T9-ETV0108

  APARS FIXED: TTSO801

  SPECIAL CONDITIONS:
    ACTION:  TSO must be restarted after this user modification
      is installed.

  COMMENTS:
    LAST CHANGE:  2009/10/20

    REWORK HISTORY:
      2009/10/20: Allow msgIKT012D to be issued in the case of
                  HALT NET,CANCEL (previous logic resulted in
                  abend0C4 in IKTCAS41 with TCAS hung if HALT
                  NET,CANCEL issued).

    THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

    MODULES
      IKTCAS41

    NOTE:  This was fixed in ACF/VTAM V3 for MVS/370 HVT3204 by
      OY24140 (UY41051) and in ACF/VTAM V3 for MVS/XA HVT3205
      by OY24473 (UY41052)

    LISTEND
 */.
++ ZAP      (IKTCAS41) DISTLIB(AOST3   ).
 NAME IKTCAS41
 IDRDATA TTSO801
 EXPAND IKTCAS41(28)
VER 000004 10                        DC    AL1(16)             VERIFY LENGTH
VER 000005 C9D2E3C3C1E2F4F1          DC    C'IKTCAS41  78.045' VERIFY IDENTIFIER
VER 00000D 4040F7F84BF0F4F5
VER 000015 00
VER 000016 90EC,D00C        @PROLOG  STM   @14,@12,12(@13)
VER 00001A 05C0                      BALR  @12,0
VER 0001E6 9207,A009                 MVI   WERC2(@10),X'07'    SET FUNCTION
VER 0004D0 0000000000000000          DC    (PATCHLEN)X'00'     VERIFY PATCH AREA
VER 0004D8 0000000000000000
VER 0004E0 0000000000000000
VER 0004E8 00000000
REP 0001E6 45E0,C4B4                 BAL   @14,PATCH1          TO PATCH AREA
REP 0004D0 9504,4000        PATCH1   CLI   WECODE1T(@04),X'04' TEST HALT COMMAND
REP 0004D4 4770,C4CA                 BNE   PATCH1A             NOT HALT
REP 0004D8 9512,4001                 CLI   WECODE1F(@04),X'12' TEST FOR CANCEL
REP 0004DC 4770,C4CA                 BNE   PATCH1A             NOT HALT CANCEL
REP 0004E0 9207,A009                 MVI   WERC2(@10),X'07'    TERM WITH WTOR
REP 0004E4 07FE                      BR    @14                 RETURN TO MAIN
REP 0004E6 9206,A009        PATCH1A  MVI   WERC2(@10),X'06'    TERM WITHOUT WTOR
REP 0004EA 07FE                      BR    @14                 RETURN TO MAIN
??
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TTSO801)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TTSO801)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TTSO801)
        DIS(WRITE)
        .
/*
//
//VS49603  JOB (SYSGEN),'J04 M14: VS49603',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//*
//* Desc: Install usermod VS49603 to fix excessive
//* disabled spin loop conditions
//*
//* Source: Doug Wegscheid, Apr 2020
//* Origin: Gerhard Postpischil and Juergen Winkelmann
//*
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(VS49603) .
++VER(Z038) FMID(FBB1221)
PRE(UZ56445)
  /*
    Symptom: IEE331A EXCESSIVE DISABLED SPIN LOOP DETECTED
    Fix: Apply IBM ZAP VS49603, which fixes excessive disabled
    spin loop conditions when MVS is running under VM
  */ .
++ ZAP (IEEVEXSN) .
NAME IEANUC01 IEEVEXSN
VER 011A 47E0 BNO
REP 011A 4700 NOP
IDRDATA VS49603
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(VS49603)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(VS49603)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(VS49603)
        DIS(WRITE)
        .
/*
//
//WM00017  JOB (SYSGEN),'J05 M15: WM00017',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD WM00017 (source: SHARE?) adds 2 JES2 console      *
//* commands: $U and $DP                                              *
//*********************************************************************
//*
//RECV25   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD (WM00017) /* $DP COMMAND AND $U COMMAND */  .
++VER (Z038) FMID(EJE1103) PRE(UZ31176 UZ33158 UZ35334
       UZ37263 UZ54837 UZ57911 UZ63374 UZ65742 UZ71437
       UZ79531)
   /* SOURCE: JES2 MODS (SHARE?)
      TWO NEW COMMANDS HAVE BEEN ADDED TO JES2 TO ENHANCE OUTPUT
      PROCESSING.  THE TWO COMMANDS ARE:- $DP & $U.  THE FORMAT
      OF THE $DP COMMAND IS AS FOLLOWS:-
         $DP   ----  WILL DISPLAY ALL PRINTED OUTPUT.  IT WILL
                     GIVE BOTH THE NUMBER OF LINES AND THE
                     OUTPUT CLASS PLUS THE USUAL JES2 SPOOL
                     UTILIZATION MESSAGE.
         $DPX  ----  WHERE X IS THE SYSOUT CLASS TO BE DISPLAYED.
                     EG. $DPA WILL DISPLAY ALL OUTPUT FOR SYSOUT
                     CLASS=A.  IF THERE IS NO SYSOUT=A THEN THE
                     JES2 SPOOL UTILIZATION MESSAGE IS DISPLAYED.
                     THE $DPX FORMAT DOES NOT SUPPORT MULTIPLE
                     SYSOUT CLASSES.
      THE FORMAT OF THE $U COMMAND IS AS FOLLOWS:-
         $U JOBID,O= FROM CLASS,C= TO CLASS WHERE JOBID CAN BE
                     JOB/TSU/STC NUMBER OR RANGE OF NUMBERS EG
                     J10, S23-25, T51 OR JOBNAME IN QUOTES EG
                     'TSTJOB'.  FROM CLASS CAN BE ONE OR MORE
                     CLASSES EG A, ABV, ABCDEF OR * TO SIGNIFY
                     ALL CLASSES.  TO CLASS IS A SINGLE CLASS
                     SPECIFICATION EG C.
         EXAMPLES OF THE $U COMMAND:-
            $US1,C=P,O=L   WILL RESET 'L' CLASS OUTPUT FOR STARTED
                     TASK 1 TO CLASS 'P'.
            $UJ10,O=V,C=P  WILL RESET 'V' CLASS OUTPUT FOR JOB 10
                     TO CLASS = 'P'.
            $U'TESTJOB',O=2A,C=5  WILL RESET '2' CLASS & 'A' CLASS
                     OUTPUT FOR TESTJOB TO CLASS = '5'.
            $UJ1-999,C=2,O=*  WILL RESET OUTPUT FOR ALL JOBS TO
                     CLASS = '2'.
      POINTS TO NOTE:-
         THE OPERANDS 'O' & 'C' MAY BE IN ANY ORDER.
         OUTPUT CURRENTLY BEING PRINTED CANNOT BE RESET.
         RESETTING OUTPUT TO THE 'Z' QUEUE RE-QUEUES OUTPUT TO A
         'Z' QUEUE BUT DOES NOT AUTOMATICALLY DELETE.
         ('Z' CLASS BEING SYSOUT CLASS NOT PRINTED)
      THE RESPONSES TO THE $U COMMAND WILL BE:-
         $HASP000 SYSOUT CLASS/ES CHANGED
                       OR
         $HASP000 NO OUTPUT FOUND                        */ .
++SRCUPD (HASPCOMM) DISTLIB (HASPSRC).
./       CHANGE NAME=HASPCOMM
*        TO BE INSERTED BETWEEN $DO AND $DQ ENTRIES  ***********   BNSW K1689600
         $COMTAB DP,BNSWCMD        $DP DISPLAY OUTPUT QUEUE(S) *   BNSW K1689700
*                                                    ***********   BNSW K1689800
COMTBLU  $COMTAB U7D,BNSWCMD,REJECT=COMR+COMJ  $U'JOBNAME'     *   BNSW K1720600
         $COMTAB UJ,BNSWCMD,REJECT=COMR+COMJ   $UJ   * CHANGE  *   BNSW K1720602
         $COMTAB US,BNSWCMD,REJECT=COMR+COMJ   $US   * SYSOUT  *   BNSW K1720604
         $COMTAB UT,BNSWCMD,REJECT=COMR+COMJ   $UT   * CLASS.  *   BNSW K1720606
****************************************************************   BNSW K1720608
******** ENTRY FOR 'U' COMMANDS    *****************************   BNSW K1733250
         DC    C'U',AL3(COMTBLU)   $U - CHANGE OUTPUT CLASS    *   BNSW K1733252
****************************************************************   BNSW K1733254
 TITLE '   HASP COMMAND PROCESSOR BNSWCMD - $DP  *** BNSW ***'     BNSW K2577550
         PUSH  USING                                               BNSW K2577551
BNSWCMD  $COMGRUP DP,U7D,UJ,US,UT,DELAY=NO  ADDED COMMANDS         BNSW K2577552
CDP      EQU   *                   DISPLAY OUTPUT QUEUES (FOR A    BNSW K2577553
*                                  PARTICULAR CLASS IF REQUESTED)  BNSW K2577554
         SPACE 2                                                   BNSW K2577555
****************************************************************** BNSW K2577556
*                                                                * BNSW K2577557
*        $DP   OR   $DPX    X=CLASS TO BE DISPLAYED, DEFAULT ALL * BNSW K2577558
*                                                                * BNSW K2577559
****************************************************************** BNSW K2577560
         SPACE 2                                                   BNSW K2577561
         USING JOEDSECT,R1                                         BNSW K2577562
         SPACE 1                                                   BNSW K2577563
         L     R1,0(,WD)           A(1ST OPERAND) I.E. 'P'         BNSW K2577564
         SR    WB,WB               CLASS INDEX - 0=ALL             BNSW K2577565
         CLI   1(R1),C' '          IF NO CLASS SPECIFIED           BNSW K2577566
         BE    CDPALLC             THEN DISPLAY ALL CLASSES        BNSW K2577567
*                                  ELSE CHECK VALIDITY OF CLASS:   BNSW K2577568
         LA    R15,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577569
CDPFNCLS LA    WB,1(,WB)           1=A , 2=B , ...                 BNSW K2577570
         IC    WC,CDPCLSES-1(WB)   CLASS FROM LIST                 BNSW K2577571
         CLM   WC,1,1(R1)          IF THIS IS THE CLASS            BNSW K2577572
         BE    CDPHAVCL            THEN GO FIND JOE'S              BNSW K2577573
         BCT   R15,CDPFNCLS        ELSE CHECK AGAINST NEXT IN LIST BNSW K2577574
         SPACE 1                                                   BNSW K2577575
         $CFINVO OPERAND=(R1)      MSG 'INVALID OPERAND' + GET OUT BNSW K2577576
         SPACE 1                                                   BNSW K2577577
CDPCLSES DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' CLASS LIST  BNSW K2577578
         SPACE 1                                                   BNSW K2577579
CDPHAVCL DS    0H                  RUN THRU CLASS QUEUE IN JOT     BNSW K2577580
         BAL   WC,CDPPRC           PROCESS SPECIFIED CLASS         BNSW K2577581
         B     CDPEND              AND GET OUT WITH FINAL MSG.     BNSW K2577582
         SPACE 2                                                   BNSW K2577583
CDPALLC  LA    WB,1(,WB)           1ST/NEXT CLASS TO PROCESS       BNSW K2577584
         BAL   WC,CDPPRC           PROCESS A CLASS                 BNSW K2577585
         LA    WC,L'CDPCLSES       NUMBER OF CLASES                BNSW K2577586
         CR    WB,WC               IF NOT ON LAST ONE              BNSW K2577587
         BNE   CDPALLC             THEN DO NEXT CLASS              BNSW K2577588
         SPACE 1                                                   BNSW K2577589
CDPEND   EQU   *                   ALL DONE - SETUP FINAL MSG AND  BNSW K2577590
*                                  GET OUT. CODE HERE IS EXACTLY   BNSW K2577591
*                                  THE SAME AS AT 'CDQEND'         BNSW K2577592
         L     R15,$SSVT           A(SSVT)                         BNSW K2577593
         L     R1,$SVTGALC-SSVT(,R15) GROUPS ALLOCATED             BNSW K2577594
         M     R0,=F'100'          CALCULATE                       BNSW K2577595
         D     R0,$SVTGTOT-SSVT(,R15) PERCENTAGE                   BNSW K2577596
         $CFCVE VALUE=(R1)         MAKE PRINTABLE                  BNSW K2577597
         MVC   COMMAND(3),COMDWORK+2 INTO MSG                      BNSW K2577598
         MVC   COMMAND+3(26),=C' PERCENT SPOOL UTILIZATION'        BNSW K2577599
         $CRET L=29                GET OUT AND ISSUE MSG           BNSW K2577600
         SPACE 3                                                   BNSW K2577601
CDPPRC   DS    0H                  PROCESS CLASS INDICATED BY WB   BNSW K2577602
         ST    WC,COMWREGS+12      KEEP RETURN ADDR                BNSW K2577603
         IC    R1,CDPCLSES-1(WB)   GET CHAR FOR CLASS              BNSW K2577604
         STC   R1,COMWREGS+4       AND KEEP IT                     BNSW K2577605
         LA    R1,0(WB,WB)         2,4,6,...                       BNSW K2577606
         LA    R1,(JOTCLSQ-JOTDSECT)-(JOENEXT-JOEDSECT+2)(R1) DISP BNSW K2577607
         AL    R1,$JOTABLE         ACTUAL ADDR TO START            BNSW K2577608
         ST    R1,COMWREGS         KEEP STARTING POINT FOR THIS Q  BNSW K2577609
         SPACE 1                                                   BNSW K2577610
CDPNXTJO LH    R1,JOENEXT          1ST/NEXT JOE OFFSET/4           BNSW K2577611
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577612
         BZ    CDPPRCR             GET OUT IF NO MORE JOE'S        BNSW K2577613
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577614
         AL    R1,$JOTABLE         ADD ADDR OF JOT                 BNSW K2577615
         ST    R1,COMWREGS+8       KEEP ADDR OF CURRENT JOE        BNSW K2577616
         LH    R14,JOEJQE          JQE OFFSET/4                    BNSW K2577617
         N     R14,=X'0000FFFF'    ENSURE +                        BNSW K2577618
         SLL   R14,2               *4 FOR ACTUAL OFFSET            BNSW K2577619
         AL    R14,$JOBQPTR        A(JQE)                          BNSW K2577620
         SPACE 1                                                   BNSW K2577621
*        BUILD MSG AND ISSUE $CWTO                                 BNSW K2577622
         SPACE 1                                                   BNSW K2577623
         BAL   WC,CDPIJOB          IDENTIFY JOB                    BNSW K2577624
         BAL   WC,CDPICLAS         IDENTIFY OUTPUT CLASS           BNSW K2577625
         BAL   WC,CDPILNS          NUMBER OF LINES                 BNSW K2577626
         SPACE 1                                                   BNSW K2577627
         $CWTO L=CDPOLEN           ISSUE MSG                       BNSW K2577628
         SPACE 1                                                   BNSW K2577629
*        MAKE SURE THAT THE JOE THAT WE WERE WORKING WITH IS STILL BNSW K2577630
*        ON THE QUEUE.                                             BNSW K2577631
         L     R1,COMWREGS         GET START POINT FOR THE QUEUE   BNSW K2577632
CDPNXTJX LH    R1,JOENEXT          1ST/NEXT JOE                    BNSW K2577633
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577634
         BZ    CDPINCM             NOT FOUND - MSG 'LIST INCOMPLETE' SW K2577635
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577636
         AL    R1,$JOTABLE         A(JOE)                          BNSW K2577637
         C     R1,COMWREGS+8       IF THIS IS THE ONE WE WERE WORKING W K2577638
         BNE   CDPNXTJX            WITH THEN CONTINUE AS NORMAL ELSE SW K2577639
*                                  TRY NEXT JOE ON THE QUEUE.      BNSW K2577640
         B     CDPNXTJO            JOE FOUND - CONTINUE            BNSW K2577641
         SPACE 2                                                   BNSW K2577642
CDPINCM  MVC   COMMAND(L'CDPCLIC),CDPCLIC 'LIST INCOMPLETE'        BNSW K2577643
         MVC   COMMAND+L'CDPCLIC(1),COMWREGS+4 CLASS               BNSW K2577644
         $CWTO L=L'CDPCLIC+1       ISSUE MSG                       BNSW K2577645
         SPACE 1                                                   BNSW K2577646
CDPPRCR  L     WC,COMWREGS+12      RESTORE RETURN ADDR             BNSW K2577647
         BR    WC                                                  BNSW K2577648
         SPACE 3                                                   BNSW K2577649
CDPIJOB  DS    0H                  PUT JOB IDENTITY IN MSG         BNSW K2577650
         MVC   CDPOJNAM,JQEJNAME-JQE(R14) MOVE IN JOBNAME          BNSW K2577651
         LH    R0,JQEJOBNO-JQE(R14) JOB NUMBER                     BNSW K2577652
         MVC   CDPOJID,CDPCJID     ASSUME 'JOB'                    BNSW K2577653
         CH    R0,=H'10000'        IS IT JOB                       BNSW K2577654
         BL    CDPCONJN            IF SO GO CONVERT NUMBER         BNSW K2577655
         MVC   CDPOJID,CDPCTID     TRY FOR 'TSU'                   BNSW K2577656
         SH    R0,=H'20000'        SUBTRACT TSUS BASE              BNSW K2577657
         BNM   CDPCONJN            GO CONVERT IF IT IS TSU         BNSW K2577658
         MVC   CDPOJID,CDPCSID     MUST BE STC                     BNSW K2577659
         AH    R0,=H'10000'        MAKE STC NUMBER +VE             BNSW K2577660
CDPCONJN CVD   R0,COMDWORK                                         BNSW K2577661
         MVC   CDPOJNUM,CDPCJPAT   EDIT PATTERN                    BNSW K2577662
         ED    CDPOJNUM,COMDWORK+(8-L'CDPOJNUM/2)                  BNSW K2577663
         MVC   CDPOJNID,CDPCJNID   '#'                             BNSW K2577664
         BR    WC                                                  BNSW K2577665
         SPACE 2                                                   BNSW K2577666
CDPICLAS DS    0H                  PUT OUTPUT CLASS INTO MSG       BNSW K2577667
         MVC   CDPOCLAS,COMWREGS+4 A,B,C,...                       BNSW K2577668
         MVC   CDPOCLID,CDPCCLID   'CLASS'                         BNSW K2577669
         BR    WC                                                  BNSW K2577670
         SPACE 2                                                   BNSW K2577671
CDPILNS  DS    0H                  NUMBER OF LINES AND INDICATION  BNSW K2577672
         MVC   CDPOPRIN,CDPCBLNK   IF THE JOB IS CURRENTLY PRINTING NSW K2577673
         L     R0,JOERECCT         NUMBER OF LINES                 BNSW K2577674
         TM    JOEFLAG,$JOEPRT     IF JOB IS NOT PRINTING          BNSW K2577675
         BNO   CDPNOTPR            THEN DON'T LOOK FOR CHKPT JOE   BNSW K2577676
         SPACE 1                                                   BNSW K2577677
         MVC   CDPOPRIN,CDPCPRIN   'PRINTING' TO MSG               BNSW K2577678
*        JOB PRINTING, SUBTRACT NUMBER OF LINES PRINTED FROM TOTAL. NSW K2577679
         TM    JOEFLAG,$JOECKV     IF CHKPT NOT VALID              BNSW K2577680
         BNO   CDPNOTPR            THEN DON'T LOOK AT IT           BNSW K2577681
         LH    R1,JOECKPT          OFFSET/4 FOR CHKPT JOE          BNSW K2577682
         N     R1,=X'0000FFFF'     ENSURE +                        BNSW K2577683
         SLA   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577684
         BZ    CDPNOTPR            SKIP IT IF NOT AVAILABLE        BNSW K2577685
         AL    R1,$JOTABLE         A(CHKPT JOE)                    BNSW K2577686
         S     R0,JOETLNC          TOTAL-(NO. PRINTED)             BNSW K2577687
         L     R1,COMWREGS+8       RESTORE A(WORK JOE)             BNSW K2577688
         SPACE 1                                                   BNSW K2577689
CDPNOTPR CVD   R0,COMDWORK         NUMBER OF LINES                 BNSW K2577690
         MVC   CDPONLIN,CDPCLPAT   EDIT PATTERN                    BNSW K2577691
         ED    CDPONLIN,COMDWORK+(8-L'CDPONLIN/2)                  BNSW K2577692
         MVC   CDPOLINS,CDPCLINS   'LINES'                         BNSW K2577693
         BR    WC                                                  BNSW K2577694
         SPACE 3                                                   BNSW K2577695
*        FIELD 'COMWREGS' USED AS FOLLOWS:                         BNSW K2577696
*                                                                  BNSW K2577697
*              COMWREGS+0(4)  OFFSET OF START OF CURRENT WORK JOE Q NSW K2577698
*              COMWREGS+4(1)  CLASS CURRENTLY BEING PROCESSED      BNSW K2577699
*              COMWREGS+8(4)  ADDR OF CURRENT WORK JOE             BNSW K2577700
*              COMWREGS+12(4) RETURN ADDR FROM 'CDPPRC'            BNSW K2577701
*                                                                  BNSW K2577702
         SPACE 1                                                   BNSW K2577703
CDPCJID  DC    C' JOB '            IDENTIFY 'JOB' IN MSG           BNSW K2577704
CDPCTID  DC    C' TSU '            IDENTIFY 'TSU' IN MSG           BNSW K2577705
CDPCSID  DC    C' STC '            IDENTIFY 'STC' IN MSG           BNSW K2577706
CDPCJNID DC    C'  #'              IDENTIFY JOB NUMBER             BNSW K2577707
CDPCJPAT DC    X'602020202021'     JOB NUMBER PATTERN              BNSW K2577708
CDPCCLID DC    C'  CLASS '         IDENTIFY CLASS IN MSG           BNSW K2577709
CDPCLINS DC    C'     LINES'                                       BNSW K2577710
CDPCLPAT DC    X'4020202020202021' NUMBER OF LINES PATTERN         BNSW K2577711
CDPCPRIN DC    C'  PRINTING'       IF JOB IS PRINTING              BNSW K2577712
CDPCBLNK DC    CL(L'CDPCPRIN)' '   IF JOB NOT PRINTING             BNSW K2577713
CDPCLIC  DC    C'LIST INCOMPLETE FOR CLASS '                       BNSW K2577714
         SPACE 2                                                   BNSW K2577715
*        EQUATES FOR OUTPUT LINE. THESE TOGETHER WITH THE ABOVE    BNSW K2577716
*        CONSTANTS DETERMINE THE FORMAT OF THE MESSAGE.            BNSW K2577717
         SPACE 1                                                   BNSW K2577718
CDPOJID  EQU   COMMAND,L'CDPCJID               'JOB','TSU','STC'   BNSW K2577719
CDPOJNAM EQU   CDPOJID+L'CDPOJID,8             JOBNAME             BNSW K2577720
CDPOJNID EQU   CDPOJNAM+L'CDPOJNAM,L'CDPCJNID  '#'                 BNSW K2577721
CDPOJNUM EQU   CDPOJNID+L'CDPOJNID-1,L'CDPCJPAT JOB NUMBER         BNSW K2577722
CDPOCLID EQU   CDPOJNUM+L'CDPOJNUM,L'CDPCCLID  'CLASS'             BNSW K2577723
CDPOCLAS EQU   CDPOCLID+L'CDPOCLID,1           CLASS BYTE          BNSW K2577724
CDPOLINS EQU   CDPOCLAS+L'CDPOCLAS,L'CDPCLINS  'LINES'             BNSW K2577725
CDPONLIN EQU   CDPOLINS+L'CDPOLINS-1,L'CDPCLPAT NUMBER OF LINES    BNSW K2577726
CDPOPRIN EQU   CDPONLIN+L'CDPONLIN,L'CDPCPRIN  'PRINTING'          BNSW K2577727
CDPOLEN  EQU   CDPOPRIN+L'CDPOPRIN-COMMAND     LENGTH OF MSG       BNSW K2577728
 TITLE '   HASP COMMAND PROCESSOR BNSWCMD - $U   *** BNSW ***'     BNSW K2577729
         EJECT                                                     BNSW K2577730
****************************************************************** BNSW K2577731
*                                                                * BNSW K2577732
*        ROUTINES FOR $U COMMAND:                                * BNSW K2577733
*                                                                * BNSW K2577734
*        EG'S  $U'JOBNAME',O=V,C=Z     (FROM CLASS V TO CLASS Z) * BNSW K2577735
*              $UJ101,O=*,C=A          (ALL CLASSES TO CLASS A)  * BNSW K2577736
*              $UT15-16,O=AB5,C=V      (CLASSES A,B,5 TO CLASS V)* BNSW K2577737
*                                                                * BNSW K2577738
****************************************************************** BNSW K2577739
         SPACE 2                                                   BNSW K2577740
CU7D     DS    0H                  $U'JOBNAME'                     BNSW K2577741
         L     R1,0(,WD)           A(OPERAND) IE 1ST "'"           BNSW K2577742
         L     WC,4(,WD)           END OF OPERAND +2               BNSW K2577743
         BCTR  WC,0                BACK TO END                     BNSW K2577744
         BCTR  WC,0                OF OPERAND                      BNSW K2577745
         CLI   0(WC),C''''         ENDING ' IS OPTIONAL            BNSW K2577746
         BNE   CUNOSUB             IF NOT ' LAST BYTE PART OF NAME BNSW K2577747
         BCTR  WC,0                END OF NAME                     BNSW K2577748
CUNOSUB  SR    WC,R1               LENGTH OF NAME                  BNSW K2577749
         BNP   CUJINVO             NO NAME    - GET OUT            BNSW K2577750
         BCTR  WC,0                LENGTH FOR EX                   BNSW K2577751
         LA    R0,7                MAX LEN                         BNSW K2577752
         CR    WC,R0               TRUNCATE                        BNSW K2577753
         BNH   CUOKL               IF                              BNSW K2577754
         LR    WC,R0               TOO LONG                        BNSW K2577755
CUOKL    MVC   COMJNAME,=CL8' '    INIT JOBNAME                    BNSW K2577756
         EX    WC,CUMVC                                            BNSW K2577757
         SPACE 1                                                   BNSW K2577758
*        LOOK FOR JOB IN JOB QUEUE.                                BNSW K2577759
         USING JQEDSECT,R1                                         BNSW K2577760
         $CFJSCAN PROCESS=CUPROC,NEXT=CUNEXTJ                      BNSW K2577761
         MVC   COMMAND(8),COMJNAME JOB NOT FOUND -                 BNSW K2577762
         MVC   COMMAND+8(14),=C' JOB NOT FOUND' SETUP MSG          BNSW K2577763
         $CRET L=22                AND GET OUT                     BNSW K2577764
         SPACE 1                                                   BNSW K2577765
CUMVC    MVC   COMJNAME(1),1(R1)   TO MOVE JOBNAME FROM COMMAND    BNSW K2577766
         SPACE 1                                                   BNSW K2577767
CUPROC   CLC   COMJNAME,JQEJNAME   IS THIS THE REQD JOB            BNSW K2577768
         BNE   CUNEXTJ             GET NEXT IF NO MATCH            BNSW K2577769
         LH    WC,JQEJOBNO         GET JOB NUMBER                  BNSW K2577770
         STH   WC,CUJWJLO          LO JOB NO                       BNSW K2577771
         STH   WC,CUJWJHI          SAME AS HI JOB NO               BNSW K2577772
         B     CUJHAVJ             PROCESS OTHER OPERANDS          BNSW K2577773
         SPACE 2                                                   BNSW K2577774
         USING JOEDSECT,R1                                         BNSW K2577775
CUS      DS    0H                  $US                             BNSW K2577776
         LH    WA,=H'10000'        STCS LO END                     BNSW K2577777
         B     CUJA                COMMON                          BNSW K2577778
         SPACE 2                                                   BNSW K2577779
CUT      DS    0H                  $UT                             BNSW K2577780
         LH    WA,=H'20000'        TSUS LO END                     BNSW K2577781
         B     CUJA                COMMON                          BNSW K2577782
         SPACE 2                                                   BNSW K2577783
CUJ      DS    0H                  $UJ                             BNSW K2577784
         SR    WA,WA               JOBS LO END                     BNSW K2577785
         SPACE 1                                                   BNSW K2577786
CUJA     $CFCVB POINTER=(WD),NOK=CUJINVO GET JOB NUBER RANGE       BNSW K2577787
         LTR   R0,R0               IF 'HI' JOB NUMBER NOT +VE      BNSW K2577788
         BNP   CUJINVO             THEN GET OUT WITH ERROR MSG     BNSW K2577789
         AR    R0,WA               HI JOB NUMBER                   BNSW K2577790
         STH   R0,CUJWJHI          KEEP HI JOB NUMBER              BNSW K2577791
         AR    R1,WA               LO JOB NUMBER                   BNSW K2577792
         STH   R1,CUJWJLO          KEEP LO JOB NUMBER              BNSW K2577793
CUJHAVJ  MVI   CUJWFLG,0           INIT FLAG                       BNSW K2577794
         SPACE  1                                                  BNSW K2577795
         CLR   WD,WF               IF NO MORE OPERANDS             BNSW K2577796
         BNL   CUJMISS             THEN ISSUE MISSING OP MSG       BNSW K2577797
CUJLOOP  BXH   WD,WE,CUJEND        LOOK AT NEXT OP                 BNSW K2577798
         L     R1,0(,WD)           A(OPERAND)                      BNSW K2577799
         CLI   1(R1),C'='          C=  OR  O=                      BNSW K2577800
         BNE   CUJINVO             GET OUT IF '=' NOT THERE        BNSW K2577801
         $CFSEL (C,CUJCLAS),(O,CUJOUT),OPERAND=(R1) B DEP ON C OR O NSW K2577802
         SPACE 1                                                   BNSW K2577803
CUJINVO  L     R1,0(,WD)           INVALID OPERAND POINTER         BNSW K2577804
         $CFINVO OPERAND=(R1)      GET OUT WITH MSG                BNSW K2577805
         SPACE 1                                                   BNSW K2577806
CUJMISS  $CRET MSG='OPERANDS MISSING FOR $U' GET OUT WITH MSG      BNSW K2577807
         SPACE 2                                                   BNSW K2577808
*        O=CLASSES  OR  O=* FOR ALL CLASSES  EG O=ABV              BNSW K2577809
CUJOUT   OI    CUJWFLG,CUJEOUT     O=  OPERAND FOUND               BNSW K2577810
         XC    CUJWOCLS,CUJWOCLS   CLEAR REQUIRED CLASES           BNSW K2577811
         L     R15,4(,WD)          NEXT OR NULL OPERAND            BNSW K2577812
         BCTR  R15,0               BACK TO ','                     BNSW K2577813
         LA    R1,2(,R1)           1ST OUT CLASS                   BNSW K2577814
         CLI   0(R1),C'*'          IF NOT 'ALL CLASSES'            BNSW K2577815
         BNE   CUJSCLS             THEN GO SCAN LIST               BNSW K2577816
         MVC   CUJWOCLS,CDPCLSES   ELSE MOVE IN LIST OF ALL CLASES BNSW K2577817
         B     CUJLOOP             AND GO GET NEXT OP              BNSW K2577818
         SPACE  1                                                  BNSW K2577819
CUJSCLS  CR    R1,R15              IF AT END OF THIS OPERAND       BNSW K2577820
         BNL   CUJLOOP             THEN GO GET NEXT                BNSW K2577821
         LA    R14,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577822
CUJFCLSO IC    R0,CDPCLSES-1(R14)  PICK UP CLASS (WORKING BACWARDS) NSW K2577823
         CLM   R0,1,0(R1)          IF CLASS MATCHES                BNSW K2577824
         BE    CUJHCLSO            GO PUT IT IN LIST               BNSW K2577825
         BCT   R14,CUJFCLSO        ELSE TRY NEXT VALID CLASS       BNSW K2577826
         B     CUJINVO             CLASS NOT VALID - ISSUE MSG     BNSW K2577827
         SPACE 1                                                   BNSW K2577828
CUJHCLSO STC   R0,CUJWOCLS-1(R14)  CLASS REQUIRED INTO LIST        BNSW K2577829
         LA    R1,1(,R1)           NEXT IN COMMAND                 BNSW K2577830
         B     CUJSCLS             CHECK FOR MORE                  BNSW K2577831
         SPACE 2                                                   BNSW K2577832
CUJCLAS  OI    CUJWFLG,CUJECLAS    'TO' CLASS OPERAND              BNSW K2577833
         LA    R14,L'CDPCLSES      NUMBER OF VALID CLASSES         BNSW K2577834
CUJFCLSN IC    R0,CDPCLSES-1(R14)  GET A VALID CLASS               BNSW K2577835
         CLM   R0,1,2(R1)          IF CLASS IN LIST                BNSW K2577836
         BE    CUJHCLSN            GO STORE IT                     BNSW K2577837
         BCT   R14,CUJFCLSN        ELSE TRY NEXT                   BNSW K2577838
         B     CUJINVO             INVALID 'TO' CLASS              BNSW K2577839
         SPACE 1                                                   BNSW K2577840
CUJHCLSN STC   R0,CUJWNCLS         STORE NEW CLASS                 BNSW K2577841
         BCTR  R14,0               CLASS NO A=0,B=1,...            BNSW K2577842
         STH   R14,CUJWNCLN        USE LATER TO FIND CLASS Q IN JOT NSW K2577843
         B     CUJLOOP             GET NEXT OPERAND                BNSW K2577844
         SPACE 2                                                   BNSW K2577845
CUJEND   DS    0H                  ALL OPERANDS SCANNED            BNSW K2577846
         TM    CUJWFLG,CUJECLAS+CUJEOUT C=  AND  O=  BOTH REQD     BNSW K2577847
         BNO   CUJMISS             IF NOT BOTH THERE ISSUE MSG     BNSW K2577848
         LH    R14,CUJWNCLN        NEW CLASS NO. 0,1,2,...         BNSW K2577849
         SR    R0,R0               REMOVE ANY REQUEST TO CHANGE    BNSW K2577850
         STC   R0,CUJWOCLS(R14)    CLASS TO WHAT IT WAS (EG C=A,O=A) SW K2577851
         SPACE 2                                                   BNSW K2577852
CUJGETQ  $QSUSE ,                  ENQUEUE                         BNSW K2577853
         SPACE 1                                                   BNSW K2577854
*        FIND ANY OUTPUT WHICH MATCHES THAT SPECIFIED IN THE       BNSW K2577855
*        COMMAND.                                                  BNSW K2577856
*        SCAN JOT CLASS QUEUES FOR THE REQUIRED CLASSES AND CHECK  BNSW K2577857
*        IF THE JOES BELONG TO A REQUESTED JOB.                    BNSW K2577858
         SPACE 1                                                   BNSW K2577859
         SR    WA,WA               1ST CLASS NUMBER                BNSW K2577860
CUJSCNJ  SR    R1,R1                                               BNSW K2577861
         IC    R1,CUJWOCLS(WA)     GET CLASS OR 0 IF THAT CLASS    BNSW K2577862
         LTR   R1,R1               IS NOT REQUIRED.                BNSW K2577863
         BZ    CUJENDCL            GET NEXT CLASS IF NOT REQUIRED. BNSW K2577864
         STC   R1,CUJWCURC         KEEP CURRENT CLASS              BNSW K2577865
         LA    R1,(JOTCLSQ-JOTDSECT)-(JOENEXT-JOEDSECT)(WA,WA) DSP BNSW K2577866
         AL    R1,$JOTABLE         ADDR OF JOT CLASS QUEUE HEAD    BNSW K2577867
         LH    WB,JOENEXT          1ST JOE OFFSET/4                BNSW K2577868
CUJNXTJO LR    WC,R1               KEEP A(PREV JOE) (OR CLS Q HEAD) NSW K2577869
CUJNXTJX LR    R1,WB               NEXT/1ST JOE                    BNSW K2577870
         N     R1,=X'0000FFFF'     ENSURE +VE                      BNSW K2577871
         BZ    CUJENDCL            GET OUT IF NO MORE              BNSW K2577872
         SLL   R1,2                *4 FOR ACTUAL OFFSET            BNSW K2577873
         AL    R1,$JOTABLE         A(JOE)                          BNSW K2577874
         LH    WB,JOENEXT          OFFSET FOR NEXT JOE FOR THIS CLS NSW K2577875
         TM    JOEFLAG,$JOEPRT     IF PRINTING                     BNSW K2577876
         BO    CUJNXTJO            THEN DONT TOUCH IT              BNSW K2577877
         SPACE 1                                                   BNSW K2577878
*        WE HAVE FOUND OUTPUT OF A MATCHING CLASS, NOW CHECK JOB   BNSW K2577879
         LH    R14,JOEJQE          JQE OFFSET/4                    BNSW K2577880
         N     R14,=X'0000FFFF'    ENSURE +                        BNSW K2577881
         SLL   R14,2               ACTUAL OFFSET                   BNSW K2577882
         AL    R14,$JOBQPTR        A(JQE)                          BNSW K2577883
         LH    R15,JQEJOBNO-JQEDSECT(R14) GET JOB NUMBER AND       BNSW K2577884
         CH    R15,CUJWJLO         CHECK                           BNSW K2577885
         BL    CUJNXTJO            RANGE                           BNSW K2577886
         CH    R15,CUJWJHI         OF                              BNSW K2577887
         BH    CUJNXTJO            JOBNUMBERS                      BNSW K2577888
         SPACE 2                                                   BNSW K2577889
*        DO ACTUAL SWAP.                                           BNSW K2577890
         OI    CUJWFLG,CUJEDONE    INDICATE JOT UPDATED            BNSW K2577891
         SPACE 1                                                   BNSW K2577892
*        WC HAS ADDR OF PREV JOE OR CLASS QUEUE HEADER             BNSW K2577893
*        WB HAS NEXT JOE OFFSET                                    BNSW K2577894
         STH   WB,JOENEXT-JOEDSECT(WC) TAKE JOE OFF 'FROM' QUEUE   BNSW K2577895
         LH    R14,CUJWNCLN        NEW ('TO') CLASS NO   0,1,2,... BNSW K2577896
         LA    R14,JOTCLSQ-JOTDSECT(R14,R14) DISP OF CLASS Q       BNSW K2577897
         AL    R14,$JOTABLE        A(CLASS Q HEADER)               BNSW K2577898
         MVC   JOENEXT,0(R14)      HOOK REST OF Q ONTO THIS JOE    BNSW K2577899
         MVC   JOECURCL,CUJWNCLS   NEW CLASS TO JOE                BNSW K2577900
         SL    R1,$JOTABLE         JOE OFFSET                      BNSW K2577901
         SRL   R1,2                /4                              BNSW K2577902
         STH   R1,0(R14)           JOE ONTO 'TO' Q                 BNSW K2577903
         SLL   R1,2                *4 OFFSET AGAIN                 BNSW K2577904
         $#CKPT JOE=(R1),TYPE=D    REQUEST CHKPT OF CURRENT JOE    BNSW K2577905
         $#CKPT JOE=(WC),TYPE=A    REQ CHKPT OF PREV JOE OR Q HEADER SW K2577906
         B     CUJNXTJX            GET NEXT - PREVIOUS REMAINS SAME NSW K2577907
         SPACE 2                                                   BNSW K2577908
CUJENDCL LA    WA,1(,WA)           NEXT CLASS NUMBER               BNSW K2577909
         LA    R1,L'CUJWOCLS       NUMBER OF CLASSES               BNSW K2577910
         CR    WA,R1               IF NOT AT END                   BNSW K2577911
         BNE   CUJSCNJ             DO NEXT CLASS                   BNSW K2577912
         SPACE 1                                                   BNSW K2577913
*        AT END OF SCAN CHECK IF JOT HAS BEEN UPDATED              BNSW K2577914
         TM    CUJWFLG,CUJEDONE    IF JOT NOT UPDATED              BNSW K2577915
         BNO   CUJNOCH             ISSUE MSG + GET OUT             BNSW K2577916
         SPACE 1                                                   BNSW K2577917
         LH    R1,CUJWNCLN         'TO' CLASS NO. 0,1,2...         BNSW K2577918
         LA    R1,JOTCLSQ-JOTDSECT(R1,R1) DISP OF Q HEADER         BNSW K2577919
         $#CKPT JOE=(R1),TYPE=D    REQ CHPKT OF 'TO' Q HEADER      BNSW K2577920
         $POST $HASPECF,(JOT,CKPW) POST JOT AND REQUEST CHKPT WRITE NSW K2577921
         SPACE 1                                                   BNSW K2577922
         $CRET MSG='SYSOUT CLASS/ES CHANGED' ISSUE MSG + RETURN    BNSW K2577923
         SPACE 1                                                   BNSW K2577924
CUJNOCH  $CRET MSG='NO OUTPUT FOUND' RETURN WITH MSG               BNSW K2577925
         SPACE 2                                                   BNSW K2577926
****************************************************************** BNSW K2577927
*                                                                  BNSW K2577928
*        END OF 'COMMAND' USED AS WORK AREA                        BNSW K2577929
*                                                                  BNSW K2577930
CUJWNCLN EQU   COMMAND+80,2               NEW ('TO') CLASS NUMBER  BNSW K2577931
CUJWJLO  EQU   CUJWNCLN+L'CUJWNCLN,2      LO JOB NO.               BNSW K2577932
CUJWJHI  EQU   CUJWJLO+L'CUJWJLO,2        HI JOB NO.               BNSW K2577933
CUJWCURC EQU   CUJWJHI+L'CUJWJHI,1        CURRENT CLASS            BNSW K2577934
CUJWNCLS EQU   CUJWCURC+L'CUJWCURC,1      NEW ('TO') CLASS         BNSW K2577935
CUJWOCLS EQU   CUJWNCLS+L'CUJWNCLS,L'CDPCLSES OLD ('FROM') CLASSES BNSW K2577936
CUJWFLG  EQU   CUJWOCLS+L'CUJWOCLS,1      FLAG BYTE:               BNSW K2577937
CUJECLAS EQU   X'80'                      C= FOUND                 BNSW K2577938
CUJEOUT  EQU   X'40'                      O= FOUND                 BNSW K2577939
CUJEDONE EQU   X'20'                      JOT UPDATED              BNSW K2577940
*                                                                  BNSW K2577941
****************************************************************** BNSW K2577942
         SPACE 2                                                   BNSW K2577943
         POP   USING               BACK TO WHAT IT WAS             BNSW K2577944
         SPACE 2                                                   BNSW K2577945
         LTORG                                                     BNSW K2577946
****************************************************************** BNSW K2577947
./       ENDUP
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(WM00017)
          .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//HMASMP.SYSUT1 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//HMASMP.SYSUT2 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//HMASMP.SYSUT3 DD UNIT=&WORK,SPACE=(1700,(1000,200))
//SMPCNTL  DD  *
  APPLY
        SELECT(WM00017)
        DIS(WRITE)
        .
/*
//
//ZP60001  JOB (SYSGEN),'J06 M16: ZP60001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  WTO EXIT TO AUTOMATICALLY START TSO AFTER VTAM INITIALIZATION.
//*
//GENER01  EXEC PGM=IEBGENER
//SYSPRINT DD  DUMMY
//SYSUT1   DD  *
++USERMOD(ZP60001)       /* IEECVXIT WTO EXIT TO START TSO */  .
++VER(Z038) FMID(EBB1102)
 /*
   PROBLEM DESCRIPTION:
     TSO IS DIFFICULT TO START AUTOMATICALLY AFTER AN IPL.
       IF STARTED FROM A COMMNDXX MEMBER OF PARMLIB THEN TSO
       TRIES TO INITIALIZE BEFORE VTAM IS READY.  THE OPERATOR
       MUST THEN REPLY TO RETRY AFTER VTAM IS INITIALIZED.  IF
       TSO IS CONVERTED TO A TWO-STEP PROCEDURE WHERE THE FIRST
       STEP RUNS A PROGRAM TO WAIT FOR VTAM INITIALIZATION THEN
       IKTCAS00 LOSES THE "SYSTEM TASK" STATUS (WHICH REQUIRES
       THAT A STARTED TASK HAVE ONLY ONE STEP) WHICH SHOULD BE
       ASSIGNED AS PER THE PROGRAM PROPERTIES TABLE ENTRY.
       THE "PROBLEM PROGRAM ATTRIBUTES ASSIGNED" MESSAGE IS THEN
       ISSUED BY THE SYSTEM.

       THIS USERMOD CONTAINS AN IEECVXIT WTO EXIT WHICH WILL
       ISSUE THE "S TSO" OPERATOR COMMAND WHENEVER MESSAGE
       "IST02OI  VTAM INITIALIZATION COMPLETE" IS ISSUED.
       ONCE IMPLEMENTED, THIS ACTION WILL OCCUR EVERY TIME
       VTAM INITIALIZES, NOT NECCESSARILY JUST AFTER AN IPL.
       TSO CAN THEN REMAIN A SINGLE-STEP TASK AS DISTRIBUTED
       AND BE AUTOMATICALLY AVAILABLE AFTER VTAM IS READY.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS EXIT TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 1.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEECVXIT
 */.
++MOD(IEECVXIT) DISTLIB(AOSC5).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//IFOX00  EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT)'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
IEECVXIT TITLE ' SINGLE LINE WTO/WTOR EXIT FOR MVS 3.8 '
***********************************************************************
*                                                                     *
*   THIS EXIT RECEIVES CONTROL FROM IEAVVWTO BEFORE THE WQE (AND      *
*   ORE IF IT IS A WTOR) ARE BUILT.  THE EXIT CAN CHANGE THE          *
*   ROUTING AND DESCRIPTOR CODES OF THE WTO (FIELDS USERRC AND        *
*   USERDC RESPECTIVELY).  (A WTOR WILL NOT YET HAVE A REPLY ID       *
*   ASSIGNED.)                                                        *
*                                                                     *
*   THIS EXIT WILL LOOK FOR MESSAGE                                   *
*             IST020I  VTAM INITIALIZATION COMPLETE                   *
*   (ONLY THE MESSAGE ID WILL BE CHECKED) AND WILL THEN ISSUE THE     *
*             S TSO                                                   *
*   OPERATOR COMMAND.                                                 *
*                                                                     *
*   THE INTENTION IS TO ALLOW FOR AN AUTOMATED IPL.  VTAM CAN BE      *
*   STARTED FROM THE COMMNDXX MEMBER OF PARMLIB, AND TSO WILL NOW     *
*   BE AUTOMATICALLY STARTED WHENEVER VTAM INITIALIZES.  THE TSO      *
*   STARTED TASK WILL THEREFORE NOT HAVE TO BE CHANGED TO INCLUDE     *
*   A FIRST STEP TO WAIT FOR VTAM TO COME UP, AND THE "PROBLEM        *
*   PROGRAM ATTRIBUTES ASSIGNED" MESSAGE WILL NOT BE ISSUED.          *
*                                                                     *
*   WRITTEN BY GREG PRICE          22 SEPTEMBER 2001                  *
*                                                                     *
***********************************************************************
         EJECT
IEECVXIT CSECT
         USING IEECVXIT,R15
         B     $START
         DROP  R15                 IEECVXIT
         DC    AL1(17),CL17'IEECVXIT &SYSDATE'
         USING IEECVXIT,R11
$START   STM   R14,R12,12(R13)     SAVE REGISTERS
         LR    R11,R15             SET BASE REGISTER
         L     R2,0(,R1)           POINT TO PARAMETER
         USING USERPARM,R2
         CLC   =C'IST020I ',USERTEXT
         BNE   RETURN              NOT THE MESSAGE TO BE ACTED UPON
         LA    R1,STSO             POINT TO COMMAND BUFFER
         SLR   R0,R0               CLEAR CONSOLE ID FOR MASTER
         SVC   34                  ISSUE OPERATOR COMMAND
         DROP  R2                  USERPARM
RETURN   LM    R14,R12,12(R13)     RESTORE REGISTERS
         BR    R14                 RETURN - R15 IS NOT CHECKED
         SPACE
STSO     DC    H'10',H'0',CL6'S TSO '
         LTORG
         DS    0D                  END OF CSECT
         TITLE ' EXIT PARAMETER STRUCTURE AND EQUATES '
USERPARM DSECT
USERTEXT DS    CL128
USERROUT DS    CL4
USERRC   EQU   USERROUT,2
USERDESC DS    CL4
USERDC   EQU   USERDESC,2
*        ORG   USERPARM+136
         SPACE 2
R0       EQU   0
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
         SPACE 2
         END   IEECVXIT
/*
//*
//GENER2  EXEC PGM=IEBGENER
//SYSPRINT DD  DUMMY
//SYSUT1   DD  *
  IDENTIFY IEECVXIT('ZP60001')
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60001)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60001)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60001)
        DIS(WRITE)
        .
/*
//
//ZP60002  JOB (SYSGEN),'J07 M17: ZP60002',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO STOP TSO TEST 'LIST .... I' STOPPING AT BAD OPCODE.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60002)         /* TSO TEST (LIST SUBCOMMAND) */  .
++VER(Z038) FMID(EBB1102) PRE(UY29953)
 /*
   PROBLEM DESCRIPTION:
     IN TSO TEST, A LIST OF INSTRUCTIONS STOPS AT A BAD OPCODE.
       WHEN LISTING INSTRUCTIONS IN TSO TEST, THE LIST STOPS
       WHENEVER AN INVALID OPCODE IS ENCOUNTERED.  THIS CAN
       REDUCE THE EASE-OF-USE OF TSO TEST DURING DEBUGGING.

       THIS USERMOD CHANGES LIST INSTRUCTION PROCESSING TO
       DISPLAY THE HEXADECIMAL OF EACH HALFWORD ENCOUNTERED
       WITH AN INVALID OPCODE AS A "DC" INSTRUCTION.  THIS
       CAN SIMPLIFY DEBUGGING PASSAGES OF CODE CONTAINING
       SMALL INLINE MACRO EXPANSIONS.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 2.
     THIS IS A COMPLETE REWORK FOR MVS 3.8J OF A MODIFICATION
     SHIPPED IN CBT FILE 300 AND DISCUSSED IN AN ARTICLE BY
     ALAN FIELD IN THE AUGUST 1990 ISSUE OF THE NASPA TECHNICAL
     SUPPORT MAGAZINE.  THIS USERMOD ALSO ADDS SOME EXTRA
     CHARACTERS NOT SUPPORTED BY THE 3277 BUT WHICH ARE AVAILABLE
     ON LATER TERMINALS TO THE DISPLAYABLE CHARACTER TRANSLATE
     TABLE WHICH IS USED WHEN LISTING CHARACTERS.

     ORIGINALLY FOR UZ39425 (OCT 2001).
     THIS REWORK FOR UY29953 (NOV 2002).

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IKJEGLSA
 */.
++ZAP(IKJEGLSA) DISTLIB(ACMDLIB).
 NAME IKJEGLSA
 IDRDATA ZP60002
VER 0A44 12FF                  LTR   R15,R15
VER 0A46 4770,CB96             BNZ   LSTI80
VER 0A4A 43F2,0000             IC    R15,CONLNTH(R2)
VER 0A4E 06F0                  BCTR  R15,NULL
VER 0ABA 5930,A038    LSTI70   C     R3,A2
VER 0EBC 0000,0004    FFOUR    DC    F'4'
VER 0F20 E9E9,E9E9,E9E9,E9E9   PATCH AREA
VER 0F28 E9E9,E9E9,E9E9,E9E9   PATCH AREA
VER 0F30 E9E9,E9E9,E9E9,E9E9   PATCH AREA
VER 0F38 E9E9                  PATCH AREA
VER 0FDA 4B4B7A7B              DC    C'..:#'
VER 1002 4B4BA2A3              DC    C'..ST' (LOWER CASE)
VER 1022 4BC1C2C3              DC    C'.ABC'
VER 1032 4BD1D2D3              DC    C'.JKL'
VER 1042 4B4BE2E3              DC    C'..ST'
REP 0A46 47F0,CF20             B     TO THE FOLLOWING CODE
REP 0F20 41B0,0001             LA    R11,1
REP 0F24 41BB,CFFF             LA    R11,4095(R11,R12)
REP 0F28 59F0,CEBC             C     R15,FFOUR
REP 0F2C 4780,B07A             BE    XX
REP 0F30 12FF                  LTR   R15,R15
REP 0F32 4770,CB96             BNZ   LSTI80
REP 0F36 47F0,CA4A             B     BACK TO WHERE WE CAME FROM
REP 0FDA 4B797A7B              DC    C'.`:#'
REP 1002 4BA1A2A3              DC    C'.~ST' (LOWER CASE)
REP 1022 C0C1C2C3              DC    C'{ABC'
REP 1032 D0D1D2D3              DC    C'}JKL'
REP 1042 E04BE2E3              DC    C'\.ST'
REP 1062 F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6   HEXTAB DC  C'0123...DEF'
REP 1072 C4C340404040E77D  DC  DC    C'DC    X'''
REP 107A D207,6004,B072    XX  MVC   4(8,R6),DC
REP 1080 58F0,A04C             L     R15,VPDEPT
REP 1084 F342,600C,F000        UNPK  12(5,R6),0(3,R15)
REP 108A DC03,600C,CF72        TR    12(4,R6),HEXTAB-240
REP 1090 927D,6010             MVI   16(R6),C''''
REP 1094 41F0,F002             LA    R15,2(,R15)
REP 1098 4130,3002             LA    R3,2(,R3)
REP 109C 4160,6011             LA    R6,17(,R6)
REP 10A0 47F0,CABA             B     LSTI70
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60002)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60002)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *

  APPLY
        SELECT(ZP60002)
        DIS(WRITE)
        .
/*
//
//ZP60003  JOB (SYSGEN),'J08 M18: ZP60003',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO ALLOW IFOX00 ACCEPT BLANK RECORDS AS VALID INPUT.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60003)       /* XF ASSEMBLER */  .
++VER(Z038) FMID(EAS1102) PRE(UZ32460)
 /*
   PROBLEM DESCRIPTION:
     THE XF ASSEMBLER CANNOT ACCEPT BLANK INPUT SOURCE RECORDS.
       MUCH ASSEMBLER CODE WRITTEN FOR THE HIGH-LEVEL ASSEMBLER
       WHICH WOULD OTHERWISE BE FULLY PROCESSABLE BY PUBLICLY
       AVAILABLE ASSEMBLERS CANNOT BE PROCESSED BECAUSE OF
       CHANGES TO RULES FOR ALLOWABLE INPUT.  ONE SUCH RULE IS
       THE REQUIREMENT FOR THE "SPACE" ASSEMBLER INSTRUCTION
       WHENEVER A BLANK LINE IS TO BE PRODUCED IN THE OUTPUT
       LISTING, WHEREAS THE HIGH-LEVEL ASSEMBLER (ASMA90) CAN
       ALSO ACCEPT BLANK INPUT RECORDS.

       THIS USERMOD UPDATES THE XF ASSEMBLER (IFOX00) TO
       ALLOW RECORDS WITH BLANKS IN THE FIRST 72 COLUMNS AS
       VALID INPUT.  NEW LOGIC ADDS THE INTERNAL TEXT FOR THE
       "SPACE" INSTRUCTION IN COLUMN 10 BEFORE THE INPUT RECORD
       IS PARSED WHENEVER A RECORD IS FOUND TO START WITH 72
       BLANKS.

   SPECIAL CONDITIONS:
     NONE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 3.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IF0X0F
 */.
++ZAP(IFOX0F) DISTLIB(AOS03).
 NAME IFOX0F00
 EXPAND IFOX0F00(35)
 IDRDATA ZP60003
VER 0076 12B1                  LTR   R11,R1
VER 0078 47F0,C02A             B     RETURN
VER 0100 12BB                  LTR   R11,R11
VER 0102 47F0,C02A             B     RETURN
REP 0078 47F0,C34E             B     NEWCODE
REP 0102 47F0,C34E             B     NEWCODE
REP 0351 1C190A0C0E   SPACE    DC    C'SPACE'
REP 0356 952F,B000    NEWCODE  CLI   0(R11),C' '
REP 035A 4770,C366             BNE   GOBACK
REP 035E D546,B001,B000        CLC   1(71,R11),0(R11)
REP 0364 4770,C366             BNE   GOBACK
REP 0368 D204,B009,C349        MVC   9(5,R11),SPACE
REP 036E 12BB         GOBACK   LTR   R11,R11
REP 0370 47F0,C02A             B     RETURN
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60003)
          .
/*
//*
//APPLY   EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60003)
        CHECK
        .
/*
//*
//APPLYCK EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'

//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60003)
        DIS(WRITE)
        .
/*
//
//ZP60004  JOB (SYSGEN),'J09 M19: ZP60004',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  ZAP TO SHOW ACTION CONSOLE MESSAGES IN HIGH INTENSITY.
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(ZP60004)       /* 3277 CONSOLE I/O */  .
++VER(Z038) FMID(EBB1102) PRE(UZ35462)
 /*
   PROBLEM DESCRIPTION:
     3270 HIGH INTENSITY IS NOT USED FOR IMMEDIATE ACTION MESSAGES.
       ALL CONSOLE MESSAGES DISPLAYED USING THE SHIPPED 3277
       CONSOLE SUPPORT ARE SHOWN IN LOW INTENSITY, WHEREAS THE
       EXPECTED BEHAVIOUR FOR 3270 OS CONSOLES IS THAT 3270
       DUAL INTENSITY IS EXPLOITED SO THAT IMMEDIATE MESSAGES
       INCLUDING WTORS ARE DISPLAYED IN HIGH INTENSITY UNTIL
       PROCESSED BY DOM.

       THIS USERMOD UPDATES THE 3277 CONSOLE WRITE MODULE TO
       UPDATE THE 3270 FIELD ATTRIBUTE BYTE FOR EACH IN-LINE
       MESSAGE LINE ACCORDING TO THE ACTION MESSAGE STATUS OF
       THE MESSAGE SHOWN ON THAT LINE BEFORE THE SCREEN IS
       WRITTEN.

   SPECIAL CONDITIONS:
     ACTION:
       A "CLPA" MUST BE PERFORMED AT IPL TIME FOR THIS SYSMOD TO
       BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NO. 4.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEECVETV
 */.
++ZAP(IEECVETV) DISTLIB(AOSC5).
 NAME IEECVETV
 IDRDATA ZP60004
VER 0000 05B0                  BALR  RBBASE,N0
VER 0078 9110,A121    FULLWRT  TM    DCMIOCM1,DCMWRMSG
VER 007C 47E0,B0A6             BNO   PARTWRT
VER 0524 5C5C                  PATCH AREA
REP 0078 47F0,B522    FULLWRT  B     PATCH-AREA
REP 0524 5850,A030             L     R5KEEP,DCMASCRN
REP 0528 4B50,B3D8             SH    R5KEEP,H6
REP 052C 92E4,5004    LINELOOP MVI   N4(R5KEEP),LO
REP 0530 915C,5009             TM    N9(R5KEEP),ASTER
REP 0534 47E0,B53A             BNO   NEXTLINE   NOT * OR @
REP 0538 92E8,5004             MVI   N4(R5KEEP),HI
REP 053C 4A50,A104    NEXTLINE AH    R5KEEP,DCMCORLN
REP 0540 5950,A04C             C     R5KEEP,DCMPFKLN
REP 0544 4740,B52A             BL    LINELOOP
REP 0548 9110,A121    FULLWRT  TM    DCMIOCM1,DCMWRMSG
REP 054C 47F0,B07A             B     AFTER-BRANCH-HERE
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60004)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60004)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60004)
        DIS(WRITE)
        COMPRESS(ALL)
        .
/*
//
