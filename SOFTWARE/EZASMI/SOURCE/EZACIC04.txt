EZACIC04 TITLE 'TCP/IP for MVS 3.8 EBCDIC to ASCII Translation'

EZACIC04 CSECT
***********************************************************************
* This routine provides an interface which is compatible with the     *
* the z/OS Communications Server routine to perform an EBCDIC to      *
* ASCII translation. Translation is accomplished by invoking EZASOH03 *
* which then executes the TCPIP instruction to perform the translation*
* on the Hercules host.                                               *
*                                                                     *
* Invocation:                                                         *
*                                                                     *
*  CALL   EZACIC04,(buffer,length),VL                                 *
*          buffer - Address of the EBCDIC string to translate to ASCII*
*          length - Length of the string to be translated             *
*                                                                     *
* Change history:                                                     *
*                                                                     *
* 02/28/2017 - Initial release                 Shelby Beach           *
*                                                                     *
***********************************************************************

***********************************************************************
*                 Register usage                                      *
***********************************************************************
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8                       A(Buffer)
R9       EQU   9                       A(L'Buffer)
R10      EQU   10                      A(EZACIC04 parameter list)
R11      EQU   11                      Intermediate return code value
R12      EQU   12                      Module base
R13      EQU   13                      A(Dynamic storage area)
R14      EQU   14
R15      EQU   15

***********************************************************************
*                 Entry Processing                                    *
***********************************************************************
         STM   R14,R12,12(R13)         Save caller's regs.
         LR    R12,R15                 R12 = Base addr
         USING EZACIC04,R12

         LR    R10,R1                  R10 = A(EZACIC04 parm list)

*  Allocate dynamic storage area
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         GETMAIN R,LV=(0)              Get DSA.

         ST    R13,4(,R1)              Chain save
         ST    R1,8(,R13)              areas.
         LR    R13,R1                  R13 = A(DSA)
         USING DSA,R13

***********************************************************************
*                 Request Translation                                 *
***********************************************************************
         LM    R8,R9,0(R10)            R8 = A(Buffer); R9 = A(L'Buffer)

*  Perform required error checks
         LA    R11,8                   R11 = RC (Too many parms)
         LTR   R8,R8                   Invalid parm count ?
         BM    LEAVE                   Yes.
         LTR   R9,R9                   Invalid parm count ?
         BNM   LEAVE                   Yes.
         LA    R11,12                  R11 = RC (Zero buffer length)
         ICM   R0,B'1111',0(R9)        R0 = L'Buffer
         BZ    LEAVE                   Invalid buffer length parm.
         LA    R11,16                  R11 = RC (Zero buffer address)
         LTR   R8,R8                   Zero buffer address ?
         BZ    LEAVE                   Yes.

*  Build and execute translation request
         XR    R11,R11                 R11 = RC (No errors)
         LA    R0,=C'EBCD'             We are requesting EBCDIC->ASCII.
         ST    R0,TRANPRMS             Set translation type.
         ST    R9,TRANPRMS+4           Set A(L'Buffer)
         ST    R8,TRANPRMS+8           Set A(Buffer)
         LA    R1,TRANPRMS             R1 = A(Translation parm list)
         L     R15,=V(EZASOH03)        R15 = A(EZASOH03 TCP/IP routine)
         BALR  R14,R15                 Perform translation.

***********************************************************************
*                 Exit Processing                                     *
***********************************************************************
LEAVE    DS    0H
         LR    R1,R13                  R1 = A(DSA)
         LA    R0,DSALEN               R0 = L'Dynamic storage area
         L     R13,4(,R13)             Pt to caller's save area.
         FREEMAIN R,A=(1),LV=(0)       Release dynamic storage area.

         ST    R11,16(,R13)            Set to pass RC to caller in R15.
         LM    R14,R12,12(R13)         Restore caller's regs.
         BR    R14                     Return.

***********************************************************************
*                 Dynamic Storage Area                                *
***********************************************************************
DSA      DSECT
SAVEAREA DS    18F

TRANPRMS DS    3A                      EZASOH03 translation parm list

         DS    0D
DSALEN   EQU   *-DSA
         END
