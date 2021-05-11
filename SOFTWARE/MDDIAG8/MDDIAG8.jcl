//MDDIAG8  JOB (TSO),
//             'Install SYS2 MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASMLKD EXEC ASMFCL,MAC='SYS1.AMODGEN',MAC1='SYS1.APVTMACS',
//             PARM.ASM='OBJECT,NODECK,TERM,XREF(SHORT)',
//             PARM.LKED='LIST,MAP,NCAL,AC=1'
//ASM.SYSLIB DD DISP=SHR,DSN=SYS1.MACLIB
//ASM.SYSIN DD *
         PRINT NOGEN
         TITLE 'MDDIAG8 - ISSUE VM CP COMMAND FROM MVS3.8J'
***********************************************************************
*                                                                     *
* MDDIAG8 - Mark Dickinson, 2015                                      *
* Release level : MVS3.8J (OS/VS2) ... turnkey3 under hercules        *
*                                                                     *
* FUNCTION                                                            *
* Use the DIAGNOSE 0008 function to issue a command to the CP, which  *
* in the case of MVS3.8J under hercules is to issue a command to      *
* hercules itself (ie: tape devinits etc).                            *
*                                                                     *
* - Command to be issued is passed as a program parm, max 128 bytes   *
* - The caller must have access to resource FACILITY DIAG8, if that   *
*   resource is not defined or there is no security product access is *
*   permitted (the security auth checks can be omitted from program   *
*   by toggling the &USERAKF flag in the code if you really must)     *
* - command is passed to the CP to execute via DIAG8 and the response *
*   from the CP is wto'ed to the console                              *
*                                                                     *
* REQUIREMENTS                                                        *
* This program must be assempled with AC=1 and reside in an APF       *
* authorised library, as it must switch to supervisor mode to issue   *
* the diagnose instruction.                                           *
* Also of course the MVS system needs to be running as a guest under  *
* a control program such as hercules.                                 *
*                                                                     *
* References: GC20-1807-7 VM370 System Programmers Guide Rel 6.4-81   *
*                                                                     *
* Enhancements you may want ToDo                                      *
* (1) Use a getmained area as a reply buffer to allow a larger        *
*     response buffer area. I don't need that at the moment.          *
* (2) The manual says interrupts should be disabled during the diag   *
*     call, I don't; doesn't seem to be an issue, yet.                *
* (3) The manual says there should always be a check to make sure the *
*     O/S is running as a guest under a CP, I don't as I will always  *
*     be running under hercules. Plus mvs3.8j running under hercules  *
*     does not set the indicator flag in the cpuid version field to   *
*     indicate it is running under a CP anyway so I cannot test that  *
*     THe PCCA entry for CPU 0 serial 000611 model 3033 is,           *
*     PCCAFD0006113033, if under a CP byte five should be x'FF',      *
*     it's not.                                                       *
*                                                                     *
***********************************************************************
         LCLB  &USERAKF
&USERAKF SETB  1         1=USE SECURITY(FOR RAKF), 0=NO SECURITY CHECKS
*
MDDIAG8  CSECT
         STM   R14,R12,12(13)
         BALR  R12,R0
         USING *,R12
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
         SPACE 3
***********************************************************************
*                                                                     *
* TEST THAT A PARM WAS PROVIDED                                       *
*                                                                     *
***********************************************************************
         LTR   R1,R1          TEST FOR PARM BEING PROVIDED
         BZ    ERRPARM        NO PARM PROVIDED
         L     R2,0(,R1)      ADDRESS PARM AREA, PARM LEN HALFWORD
         SR    R3,R3          CLEAR R3
         LH    R3,0(,R2)      GET PARM LENGTH
         C     R3,=F'128'     WE ALLOW MAX LEN 128 BYTES
         BL    TESTLEN0       IF < 128 THEN MAYBE OK
         L     R3,=F'128'     ELSE SET TO 128
TESTLEN0 C     R3,=F'0'
         BE    ERRPARM
         ST    R3,COMMANDL    SAVE PARM LENGTH
         LA    R2,2(,R2)      ADDRESS PARM DATA BYTES
         EX    R3,EXCPYPRM    SAVE PARM DATA STRING, LEN IN R3
         EJECT
         AIF    (&USERAKF EQ 0).NORAKF1
***********************************************************************
*                                                                     *
* CHECK RAKF AUTHORISATION TO FACILITY DIAG8                          *
* - if access to resource is authorised, proceed                      *
* - if there is no security rule for the resource, proceed            *
* - if there is no security product installed, proceed                *
* - if there is a resource rule and access is denied, to not proceed  *
*                                                                     *
***********************************************************************
         MVC   AUTHCHK(LRACHECK),RACHECKL            INIT RACHECK MACRO
RACSVC   RACHECK CLASS=RACLASS,ENTITY=RAOBJECT,MF=(E,AUTHCHK)
         SR    R3,R3               SET DEFAULT RC
         C     R15,=F'0'           RC < OR = 0?   0 = PERMITTED
         BE    DIAG8GO
         C     R15,=F'8'           EXPLICITLY NOT AUTHORIZED?
         BE    ERRRAKF
         C     R15,=F'4'           4 = RESOURCE NOT PROTECTED
         BNE   CHKERR              (RAKF RETURNS 0 NOT 4)
         WTO   'MDDIAG8:WARNING-NO SECURITY RULES ON FACILITY DIAG8'
         B      DIAG8GO
CHKERR   DS    0H
         WTO   'MDDIAG8:INVALID RETURN CODE FROM RACHECK, ALLOWING'
         EJECT
.NORAKF1 ANOP
***********************************************************************
*                                                                     *
* Diag8 as usable in MVS3.8J is documented in IBM manual              *
* GC20-1807-7 VM370 System Programmers Guide Rel 6.4-81               *
* which is available at bitsavers.org                                 *
*                                                                     *
* SWITCH TO SUPERVISOR MODE AND ISSUE THE COMMAND                     *
*   Rx   - real address of command                                    *
*   Rx+1 - real address of reponse buffer                             *
*   Ry   - length of command                                          *
*   Ry+1 - max length of response we accept                           *
* on response                                                         *
*   Rx+1 - either 0 if OK, or the CP error code                       *
*   Ry   - response will be in response buffer                        *
*   Ry+1 - actual length of response, or is response was too long     *
*          contains number of response bytes that would not fit       *
* SWITCH BACK TO PROBLEM MODE WHEN DONE                               *
*                                                                     *
* Note: we set the flags to X'40' to request the response be returned *
*       to this program (by default output would be written to the CP *
*       terminal, which is the hercules console).                     *
*                                                                     *
***********************************************************************
DIAG8GO  CNOP  0,4
* MAX WTO LEN IS 115, TRUNCATED AFTER THAT IN MVS38J
* SO TRUNCATE PARM TO FIT INTO BUFFER IF WE MUST
         L     R3,COMMANDL              RETRIEVE LEN OF COMMAND
         C     R3,=F'117'               SEE IF MAX FOR WTO
         BL    OKTOLOG                  IF < THEN OK
         LA    R3,117                   ELSE ONLY LOG 117 BYTES
OKTOLOG  EX    R3,EXLOGPRM
DIAGLOG  WTO   'MDDIAG8:                                               X
                                                                       X
                             '          END OF WTO LINE
         MODESET KEY=ZERO,MODE=SUP
         LRA   R2,COMMAND               LRA OF STORAGE VADDR
         L     R4,COMMANDL              COMMAND LEN
         ST    R4,WORKREG               LAZY WAY OF SETTING BYTE1 FLAG
         MVI   WORKREG,X'40'            FLAGS X'40', WE WANT A RESPONSE
         L     R4,WORKREG
         LRA   R3,RESPONS               LRA OF RESPONSE VADDR
         LA    R5,RESPONSL              RESPONSE BUFFER LENGTH (MAX4K)
         CNOP  0,8                      DOUBLEWORD ALIGN
         DC    X'83',X'24',XL2'0008'    DIAGNOSE CODE 8
         MODESET KEY=NZERO,MODE=PROB
*
* CHECK THE CP RC WAS 0 AND THERE IS DATA IN THE RESPONSE BUFFER
* IF NON-ZERO OR NO DATA, JUST EXIT
         LTR   R4,R4                    RETURN CODE 0 (OK) ?
         BNZ   EXIT04                   NO, WE ARE DONE
         LTR   R5,R5                    ANY RESPONSE DATA ?
         BZ    EXIT                     NO, WE ARE DONE
*
***********************************************************************
*                                                                     *
* PARSE THE DATA IN THE RESPONSE BUFFER, WRITING IT ONE LINE AT A     *
* TIME TO THE CONSOLE AS AN AUDIT TRAIL.                              *
*                                                                     *
***********************************************************************
         LA    R3,RESPONS               ADDRESS RESPONSE BUFFER
         AR    R3,R5                    ADD LENGTH RETURNED
         MVI   0(R3),X'15'              ENSURE TERMINATION CHAR EXISTS
*
         LA    R3,RESPONS               PARSE THE RESPONSE AREA
         SLR   R4,R4                    KEEP BYTE COUNT
         LA    R5,WTORESP+16            OFFSET IN OUTPUT BUFFER
NEXTCHAR CLI   0(R3),X'15'              END OF RESPONE ?
         BE    EXIT                     ALL RESPONSE DATA SHOWN
         CLI   0(R3),X'25'              END OF LINE ?
         BE    WTORESP
         MVC   0(1,R5),0(R3)            MOVE CHAR TO OUTPUT
         C     R4,=F'69'                CHECK COUNTER
         BNL   WTORESP                  IF MAX FLUSH OUTPUT BUFFER
         A     R3,=F'1'                 INC PTR
         A     R4,=F'1'                 INC COUNTER
         A     R5,=F'1'                 INC PTR
         B     NEXTCHAR                 GO GET NEXT CHARACTER
* ALLOW 70 REPONSE BYTES PER WTO
WTORESP  WTO   'MDDIAG8:                                               X
                                      '
         SLR   R4,R4                    RESET BYTE COUNT
         LA    R5,WTORESP+16            RESET OFFSET IN OUTPUT BUFFER
         A     R3,=F'1'                 INC PTR PAST X'15'
         B     NEXTCHAR                 GO GET NEXT RESPONSE CHARACTER
         EJECT
***********************************************************************
*                                                                     *
*                            ALL DONE - EXIT                          *
*                                                                     *
***********************************************************************
EXIT     CNOP  0,4
         L     R13,SAVEAREA+4     RESTORE POINTER TO CALLER'S SAVE AREA
         LM    R14,R12,12(R13)    RESTORE REGISTERS
         SLR   R15,R15            EXIT CODE 0
         BR    R14                RETURN TO SYSTEM
*
* ANY ERROR MESSAGES WE REQUIRE
ERRPARM  WTO   'MDDIAG8:INVALID OR NO PARM PROVIDED'
         B     EXIT04
         AIF    (&USERAKF EQ 0).NORAKF2
ERRRAKF  WTO   'MDDIAG8:YOU ARE NOT AUTHORISED FOR THIS RESOURCE'
         B     EXIT04
.NORAKF2 ANOP
ERRCP    WTO   'MDDIAG8:ERROR RESPONSE FROM CP, CHECK CP CONSOLE LOG'
EXIT04   L     R13,SAVEAREA+4     RESTORE POINTER TO CALLER'S SAVE AREA
         LM    R14,R12,12(R13)    RESTORE REGISTERS
         LA    R15,4              EXIT CODE 4
         BR    R14                RETURN TO SYSTEM
         SPACE 5
***********************************************************************
*                                                                     *
*        D A T A   A R E A   B I T S                                  *
*                                                                     *
***********************************************************************
SAVEAREA DC    18F'0'                    MAIN PROGRAM SAVE AREA
EXCPYPRM MVC   COMMAND(0),0(R2)          EX CMD TO SAVE PARM TO COMMAND
EXLOGPRM MVC   DIAGLOG+16(0),COMMAND     EX CMD TO LOG COMMAND
         AIF    (&USERAKF EQ 0).NORAKF3
*
* VARIABLES USED FOR SECURITY AUTH CHECKING
RACLASS  DC    AL1(L'RACLASSN)     CLASS NAME FOR RACCHECK
RACLASSN DC    C'FACILITY'         CLASS NAME FOR RACCHECK
RACHECKL RACHECK MF=L
LRACHECK EQU   *-RACHECKL          LENGTH OF RACHECK MACRO
AUTHCHK  RACHECK MF=L
* NOT SURE HOW LONG A FACILITY NAME IS, 20 BYTES GIVES ENOUGH PADDING
RAOBJECT DC    CL20'DIAG8 '        OBJECT WITHIN CLASS TO CHECK
.NORAKF3 ANOP
*
* VARIABLES USED FOR DIAG8 SECTION
WORKREG  DS    F                   WORK AREA
         DS    0D
COMMAND  DC    CL128' '            MAX CP CMDLEN IS 128
COMMANDL DS    F                   ACTUAL LENGTH OF CMD FROM PARM
RESPONS  DC    CL250' '
         DC    CL250' '
         DC    CL250' '
         DC    CL250' '
RESPONSL EQU   *-RESPONS
         DC    X'15'             PARANOID, TERMINATE RESPONSE AREA
         EJECT
* STANDARD REGISTER EQUATES HERE
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
         END   MDDIAG8
//ASM.SYSTERM DD SYSOUT=*
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB(MDDIAG8),DISP=SHR
//TESTCODE EXEC PGM=MDDIAG8,COND=(0,NE),
//  PARM='sh echo "MDDIAG8 is installed'
