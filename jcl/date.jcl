//DATE$    JOB (SYS),'INSTALL DATE',CLASS=S,MSGCLASS=A
//*---------------------------------------------------------------*
//*       INSTALL  OF  'DATE'  TSO CMD                            *
//*---------------------------------------------------------------*
//ASM1    EXEC ASMFCL,PARM.ASM='LIST,RENT,OBJ,NODECK',
//             PARM.LKED='LIST,RENT,XREF'
//ASM.SYSIN DD *
         TITLE 'D A T E   ---   D A T E  /  T I M E   C O M M A N D'
***********************************************************************
*                                                                     *
*  THIS PROGRAM IS A TSO COMMAND PROCESSOR THAT WILL LIST ON THE      *
*  TSO USER'S SCREEN THE CURRENT DATE AND TIME.                       *
*                                                                     *
*  WRITTEN AUGUST, 2003 BY JAY MOSELEY                                *
*                                                                     *
***********************************************************************
*
***********************************************************************
*  INITIALIZE.                                                        *
***********************************************************************
DATE     CSECT
         STM   R14,R12,12(R13)         SAVE REGISTERS
         LR    R12,R15                 LOAD BASE REGISTER
         USING DATE,R12
         LR    R2,R1                   SAVE CPPL PTR
         LA    R0,LOCALGM              GET SIZE OF DSECT
         GETMAIN R,LV=(R0)
         LR    R11,R1                  LOAD DSECT BASE REGISTER
         USING LOCALSTG,R11
         ST    R13,SAVEAREA+4          ESTABLISH SAVE
         ST    R11,8(R13)                 AREA CHAINING
         LR    R13,R11                 PTR TO MY SAVE AREA INTO R13
         EJECT
***********************************************************************
*  RETRIEVE SYSTEM DATE AND TIME, FORMAT IN MESSAGE BUFFER, WRITE     *
*  BUFFER TO TERMINAL.                                                *
***********************************************************************
*
         TIME  DEC
*
         STM   R0,R1,SYSTWRK           SAVE TIME/DATE
         AP    SYSTWRK+4(4),CENTJUST   +19 TO CENTURY BYTE
         UNPK  SYSTIME,SYSTWRK         UNPACK TIME/DATE FIELDS
         OI    SYSTIME+15,X'F0'        CLEAR SIGN
         MVC   JDATEJ,SYSYEAR          JULIAN DATE TO PARM LIST
         LA    R1,JTOGPARM             ADDR OF PARM LIST
         ST    R1,PLIST                STORE FOR CALL
         LA    R1,PLIST                ADDR OF PARAMETER LIST
         L     R15,=V(Y2KJTOG)         ADDR OF ROUTINE
         BALR  R14,R15                 CALL TO CONVERT
         MVC   FDATEG,JDATEG           GREGORIAN TO PARM LIST
         MVI   FMAJOR,C'6'             FORMAT TYPE TO PARM LIST
         MVI   FMINOR,C'1'             JUST HAS TO BE VALID
         LA    R1,DFMTPARM             ADDR OF PARM LIST
         ST    R1,PLIST                STORE FOR CALL
         LA    R1,PLIST                ADDR OF PARAMETER LIST
         L     R15,=V(Y2KDFMT)         ADDR OF ROUTINE
         BALR  R14,R15                 CALL TO CONVERT
         MVC   DMSG(L'DLIT),DLIT       MOVE DATE IDENT TO OUTPUT
         PACK  DWORK,FSIZE             CONVERT SIZE OF OUTPUT
         CVB   R2,DWORK                 FIELD TO BINARY
         LR    R1,R2                   COPY IT FOR MOVE
         BCTR  R1,0                    DECREMENT FOR MOVE
         EX    R1,MOVEDATE             MOVE DATE TO OUTPUT
MOVEDATE MVC   DMDATE(*-*),FTEXT       EXECUTED MOVE
         LA    R1,DMDATE+1             ADDR FOR DATE OUTPUT
         AR    R1,R2                   INCREMENT PAST DATE
         MVI   0(R1),C'('              OPEN PAREN
         MVC   1(4,R1),SYSYEAR         JULIAN YEAR
         MVI   5(R1),C'/'              SLASH
         MVC   6(3,R1),SYSDAY          JULIAN DAY
         MVI   9(R1),C')'              CLOSE PAREN
         MVC   TMSG(L'TLIT),TLIT       MOVE TIME IDENT TO OUTPUT
         LA    R1,TMTIME               ADDR FOR TIME OUTPUT
         MVC   0(2,R1),SYSHOUR         HOUR
         MVI   2(R1),C'.'              PERIOD
         MVC   3(2,R1),SYSMIN          MINUTE
         MVI   5(R1),C'.'              PERIOD
         MVC   6(2,R1),SYSSEC          SECOND
*
         LA R7,DMLEN                   LENGTH OF MESSAGE BUFFER
         TPUT DMSG,(R7)                WRITE MESSAGE TO TERMINAL
         LA R7,TMLEN                   LENGTH OF MESSAGE BUFFER
         TPUT TMSG,(R7)                WRITE MESSAGE TO TERMINAL
         EJECT
***********************************************************************
*  RETURN TO THE SYSTEM AFTER FREEING WORK AREA                       *
***********************************************************************
         LR    R1,R13                  PTR TO GOTTEN AREA INTO R1
         L     R13,SAVEAREA+4          RESTORE R13
         LA    R0,LOCALGM              GET SIZE OF DSECT
         FREEMAIN R,LV=(R0),A=(R1)
         LM    R14,R12,12(R13)         RESTORE R14-R12
         SR    R15,R15                 ZERO R15 FOR RETURN CODE
         BR    R14                     RETURN TO INVOKER
         EJECT
***********************************************************************
*  CONSTANTS.                                                         *
***********************************************************************
DLIT     DC    C'DATE = '              IDENTIFY DATE IN OUTPUT
TLIT     DC    C'TIME = '              IDENTIFY TIME IN OUTPUT
CENTJUST DC    PL4'1900000'            ADJUST CENTURY CONSTANT
         EJECT
***********************************************************************
*  DSECT FOR LOCAL STORAGE.                                           *
***********************************************************************
LOCALSTG DSECT
SAVEAREA DS    18F                     MY SAVE AREA
SYSTIME  DS    CL16                    TIME/DATE UNPACKED
SYSTWRK  DS    D                       TIME/DATE FROM SYSTEM
JTOGPARM DS    0H                      PARM LIST TO Y2KJTOG
JDATEJ   DS    CL7                       JULIAN DATE IN
JRC      DS    CL1                       RETURN CODE OUT
JDATEG   DS    CL8                       GREGORIAN DATE OUT
DFMTPARM DS    0H                      PARM LIST TO Y2KDFMT
FDATEG   DS    CL8                       GREGORIAN DATE IN
FMAJOR   DS    CL1                       MAJOR FORMAT CODE IN
FMINOR   DS    CL1                       MINOR FORMAT CODE IN
FRC      DS    CL1                       RETURN CODE OUT
FSIZE    DS    CL2                       SIZE OF TEXT OUT
FTEXT    DS    CL29                      TEXT OUT
PLIST    DS    A                       PARMLIST FOR SUB CALLS
DWORK    DS    D                       TO CONVERT SIZE FIELD
*
DMSG     DS    0C                      MESSAGE LINE FOR DATE
         DS    CL(L'DLIT)                IDENTIFY
DMDATE   DS    CL41                      FORMATTED DATE HERE
DMLEN    EQU   *-DMSG                  LENGTH OF DATE LINE
TMSG     DS    0C                      MESSAGE LINE FOR TIME
         DS    CL(L'TLIT)                IDENTIFY
TMTIME   DS    CL41                      FORMATTED TIME HERE
TMLEN    EQU   *-TMSG                  LENGTH OF TIME MESSAGE
LOCALGM  EQU   *-LOCALSTG              LENGTH OF LOCAL STORAGE
         EJECT
***********************************************************************
*  EQUATES FOR GENERAL REGISTERS AND LOCAL STORAGE OFFSETS            *
***********************************************************************
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
SYSHOUR  EQU   SYSTIME+1               UNPACKED HOUR FROM TIME/R0
SYSMIN   EQU   SYSTIME+3               UNPACKED MINUTE FROM TIME/R0
SYSSEC   EQU   SYSTIME+5               UNPACKED SECOND FROM TIME/R0
SYSYEAR  EQU   SYSTIME+9               UNPACKED YEAR FROM TIME/R1
SYSDAY   EQU   SYSTIME+13              UNPACKED DAY FROM TIME/R1
         END
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR
//LKED.SYSLIB DD DSN=SYSC.LINKLIB,DISP=SHR
//LKED.SYSIN  DD *
 NAME DATE(R)
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP
//SYSIN    DD  *
./ ADD NAME=DATE
)F DATE FUNCTION -                                                      
  DISPLAY THE SYSTEM DATE AND TIME AT THE TERMINAL IN THE FORMAT:       

        DATE = TUESDAY, AUGUST 5, 1975 (1975/217)
        TIME = 18.58.21
)X SYNTAX -                                                             
         DATE                                                           
)O OPERANDS -                                                           
  NO OPERANDS ON DATE COMMAND.                                          
./ ENDUP
//
