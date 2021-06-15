//STARTREK JOB (SYS),'INSTALL STARTREK',CLASS=A,MSGCLASS=A,COND=(1,LT),
//        USER=IBMUSER,PASSWORD=SYS1
//* From CBT Tape 173
// EXEC ASMFCL,MAC='SYS1.AMODGEN',MAC1='SYS1.MACLIB',
//             PARM.ASM='LIST,XREF,OBJECT,NODECK',
//             PARM.LKED='XREF,LET,LIST,NCAL'
//ASM.SYSIN DD *
STREK    TITLE '** TSO FULL-SCREEN STAR TREK **'
***********************************************************************
*    NAME - STREK                                                     *
*                                                                     *
*    AUTHOR - TED BESTANI                                             *
*                                                                     *
*    PURPOSE - THIS PROGRAM IS DESIGNED TO PLAY STAR TREK USING THE   *
*              HARDWARE CHARACTERISTICS OF 3270 TERMINALS.            *
*                                                                     *
*    ATTRIBUTES - NON-REUSABLE                                        *
*                                                                     *
*    MACROS USED - TGET, TPUT                                         *
*                                                                     *
***********************************************************************
         EJECT
         PRINT ON,NOGEN,NODATA
STREK    CSECT
         STM   R14,R12,12(R13)         SAVE CALLERS REGISTERS
         BALR  R9,0                    WHERE ARE WE?
         USING *,R9                    TEMPORARY BASE REGISTER
         ST    R13,SAVEAREA+4          SAVE BACKWARD SA PTR
         LA    R8,SAVEAREA             GET SA ADDR
         ST    R8,8(R13)               SAVE FORWARD SA PTR
         LR    R13,R8                  COPY SA ADDR
         LA    R12,4095(R13)           BASE2 = BASE1 + 4095 +
         LA    R12,1(R12)                                     1
         LA    R11,4095(R12)           BASE3 = BASE2 + 4095 +
         LA    R11,1(R11)                                     1
         LA    R10,4095(R11)           BASE4 = BASE3 + 4095 +
         LA    R10,1(R10)                                     1
         USING SAVEAREA,R13,R12,R11,R10    PERMANENT BASE REGISTERS
         DROP  R9                      DROP TEMPORARY BASE REGISTER
         SPACE 1
         BAL   R9,DRIVER               CALL MAIN DRIVER ROUTINE
         SPACE 1
         L     R13,SAVEAREA+4          GET BACKWARD SA PTR
         LM    R14,R12,12(R13)         RESTORE REGISTERS
         LA    R15,0                   SET RC=0
         BR    R14                     GO HOME......
         SPACE 1
SAVEAREA DS    18F    PROGRAM MAIN SAVE AREA
         SPACE 1
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
         EJECT
***********************************************************************
*     THIS IS THE HIGHEST LEVEL WORK ROUTINE IN THE PROGRAM.  IT      *
*   DIRECTS THE FLOW OF CONTROL TO THE WORK ROUTINES.                 *
*     THE PROGRAM IS WRITTEN USING BASIC STRUCTURED PROGRAMMING. FLOW *
*   BETWEEN ROUTINES IS ACCOMPLISHED VIA REGISTER 9, WHICH IS ALWAYS  *
*   SAVED UPON ENTRY AND RESTORED BEFORE EXIT.                        *
***********************************************************************
         SPACE 2
DRIVER   DS    0H
         ST    R9,SAVE10
         B     BSAVE10
SAVE10   DC    F'-1'
         DC    CL8'DRIVER  '
BSAVE10  EQU   *
         SPACE 1
         BAL   R9,HEADER               INIT MEMORY AND TPUT HEADER
         BAL   R9,INTERACT             INTERACT WITH USER
         SPACE 1
EXIT10   EQU   *
         L     R9,SAVE10
         BR    R9
         EJECT
***********************************************************************
*  THIS ROUTINE INITIALIZES MEMORY AND TPUT'S HEADERS                 *
***********************************************************************
         SPACE 1
HEADER   DS    0H
         ST    R9,SAVE20
         B     BSAVE20
SAVE20   DC    F'-1'
         DC    CL8'HEADER  '
BSAVE20  EQU   *
         SPACE 1
         L     R1,0(R1)                GET ADDR OF PARM
         LH    R2,0(R1)                GET LENGTH
         CLI   2(R1),C'2'              BYPASS SCREEN2?
         BE    BYPSC2                  YES, BYPASS
         CLI   2(R1),C'3'              GO DIRECTLY TO COMPUTER
         BE    BYPSC3                  YES
         L     R2,=A(SCREEN2)          GET SCREEN2 ADDRESS
         TPUT  (R2),1936,FULLSCR       WRITE HEADER SCREEN
         TGET  SCRETURN,1              GET RETURN
         TPUT  CLSCREEN,12,FULLSCR     CLEAR THE SCREEN
BYPSC2   EQU   *
         L     R2,=A(SCREEN3)          GET SCREEN3 ADDRESS
         TPUT  (R2),1936,FULLSCR       WRITE HEADER SCREEN
         TGET  SCRETURN,1              GET RETURN
         TPUT  CLSCREEN,12,FULLSCR     CLEAR THE SCREEN
BYPSC3   EQU   *
         GTTERM PRMSZE=PRMSZE,ALTSZE=ALTSZE,ATTRIB=ATTRIB
         BAL   R9,RANDOM10             GET FROM 0 - 9
         LR    R2,R1                   COPY REGISTER
         MH    R2,=H'7'                MULTIPLY BY NODE LENGTH
         LA    R3,GAMETBL              GET START ADDRESS
         AR    R3,R2                   ADD TO BASE
         MVC   GAMESTAT,0(R3)          MOVE THE GAME STATISTICS
         XC    STARCHRT+000(256),STARCHRT+000  ** CLEAR
         XC    STARCHRT+256(256),STARCHRT+256  ** CLEAR
         XC    STARCHRT+512(256),STARCHRT+512  ** CLEAR
         XC    STARCHRT+768(256),STARCHRT+768  ** CLEAR
         TIME  MIC,TIMEAREA            GET BASE TIME
         MVC   TIMEAREA(2),TIMEAREA+5  ALIGN
         NI    TIMEAREA,X'03'          AT MOST, 4K
         LH    R2,TIMEAREA             LOAD INTO WORK REGISTER
         ZAP   INITKLNG,GAMEKLNG       ** INIT COUNTERS
         ZAP   INITBASE,GAMEBASE       **
         ZAP   INITSTRS,GAMESTRS       **
         XR    R4,R4                   INIT RANDOM FACTOR
         B     INITLOP2
INITLOP1 EQU   *
         LA    R3,STARCHRT             RE-INIT STAR CHART
         SRL   R2,2                    JUMBLE DISP PTR
         TIME  MIC,TIMEAREA            GET TIME FOR RANDOM FACTOR
         MVC   TIMEAREA(2),TIMEAREA+5  ALIGN IT
         NI    TIMEAREA,X'0F'          CLEAR HIGH ORDER NIBBLE
         NI    TIMEAREA+1,X'F0'              LOW ORDER NIBBLE
         LH    R4,TIMEAREA
         SRL   R4,4                    SHIFT DOWN 0 < X < 256
         AR    R2,R4                   ADD RANDOM FACTOR
         BAL   R9,RANDOM10             GET INCREMENT FOR RANDOM
         AR    R4,R1                   BUMP RANDOM FACTOR
INITLOP2 EQU   *
         CP    INITSTRS,=PL1'0'        ALL THE STARS USED UP?
         BE    TRYKLING
         LA    R3,STARCHRT             RESET START CHART PTR
         LA    R2,129(R2)              BUMP DISPLACEMENT COUNTER
         CH    R2,=H'1024'             EXCEEDED TABLE?
         BH    INITLOP1                YES, CLEAR
         AR    R3,R2                   ADD DISPLACEMENT
         CLI   0(R3),X'00'             HAS THIS POINT BEEN USED YET?
         BNE   TRYKLING
         CP    INITSTRS,=PL1'0'        ALL THE STARS USED UP?
         BE    TRYKLING
         C     R3,=A(STARCHRT+1024)
         BNL   INITLOP1
         MVI   0(R3),X'03'             MARK AS A STAR
         SP    INITSTRS,=PL1'1'        DECREMENT STAR COUNTER
TRYKLING EQU   *
         CP    INITKLNG,=PL1'0'        ALL THE KLINGONS USED UP?
         BE    TRYBASES
         LA    R3,STARCHRT             RESET START CHART PTR
         LA    R2,129(R2)              BUMP DISPLACEMENT COUNTER
         CH    R2,=H'1024'             EXCEEDED TABLE?
         BH    INITLOP1                YES, CLEAR
         AR    R3,R2                   ADD DISPLACEMENT
         CLI   0(R3),X'00'             HAS THIS POINT BEEN USED YET?
         BNE   TRYBASES
         CP    INITKLNG,=PL1'0'        ALL THE KLINGONS USED UP?
         BE    TRYBASES
         C     R3,=A(STARCHRT+1024)
         BNL   INITLOP1
         MVI   0(R3),X'01'             MARK AS A KLINGON
         SP    INITKLNG,=PL1'1'        DECREMENT KLINGON COUNTER
TRYBASES EQU   *
         CP    INITBASE,=PL1'0'        ALL THE BASES USED UP?
         BE    INITLOP3
         LA    R3,STARCHRT             RESET STAR CHART PTR
         LA    R2,129(R2)              BUMP DISPLACEMENT COUNTER
         CH    R2,=H'1024'             EXCEEDED TABLE?
         BH    INITLOP1                YES, CLEAR
         AR    R3,R2                   ADD DISPLACEMENT
         CLI   0(R3),X'00'             HAS THIS POINT BEEN USED YET?
         BNE   INITLOP2
         CP    INITBASE,=PL1'0'        ALL THE BASES USED UP?
         BE    INITLOP3
         C     R3,=A(STARCHRT+1024)
         BNL   INITLOP1
         MVI   0(R3),X'02'             MARK AS A BASE
         SP    INITBASE,=PL1'1'        DECREMENT BASE COUNTER
         B     INITLOP2
INITLOP3 EQU   *
         CP    INITKLNG,=PL1'0'        NO KLINGONS ALSO?
         BNE   INITLOP2
         CP    INITSTRS,=PL1'0'        NO STARS EITHER
         BNE   INITLOP2
INITKIRK EQU   *
         TIME  MIC,TIMEAREA            GET BASE TIME
         MVC   TIMEAREA(2),TIMEAREA+5  ALIGN
         NI    TIMEAREA,X'03'          CLEAR BITS DOWN TO X'02'
         LA    R2,STARCHRT             GET ADDR OF STAR CHART
         AH    R2,TIMEAREA             ADD DISP
         CLI   0(R2),X'00'             IS IT EMPTY
         BNE   INITKIRK
         MVI   0(R2),X'04'             PUT CAPT. KIRK IN IT
*--> NOW LET'S JUMBLE IT UP A BIT.
         LA    R2,STARCHRT
         LA    R3,STARCHRT+768
         LA    R4,128
JUMBLE1  EQU   *
         XC    0(1,R2),0(R3)           *
         XC    0(1,R3),0(R2)           * FLIP THE TWO BYTES
         XC    0(1,R2),0(R3)           *
         LA    R2,2(R2)                BUMP 1ST PTR
         LA    R3,2(R3)                     2ND PTR
         BCT   R4,JUMBLE1              ITERATE
         LA    R2,STARCHRT+512
         LA    R3,STARCHRT+256
         LA    R4,128
JUMBLE2  EQU   *
         XC    0(1,R2),0(R3)           *
         XC    0(1,R3),0(R2)           * FLIP THE TWO BYTES
         XC    0(1,R2),0(R3)           *
         LA    R2,2(R2)                BUMP 1ST PTR
         LA    R3,2(R3)                     2ND PTR
         BCT   R4,JUMBLE2              ITERATE
         SPACE 2
EXIT20   EQU   *
         L     R9,SAVE20
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE RETURNS A NUMBER FROM 0 - 9 IN REGISTER 1.         *
***********************************************************************
         SPACE 2
RANDOM10 DS    0H
         ST    R9,SAVE30
         B     BSAVE30
SAVE30   DC    F'-1'
         DC    CL8'RANDOM10'
BSAVE30  EQU   *
         SPACE 1
         TIME  MIC,TIMEAREA            GET TOD CLOCK
         NI    TIMEAREA+6,X'F0'        CLEAR RIGHT MIC NIBBLE
         XR    R1,R1                   CLEAR WORK REGISTER
         IC    R1,TIMEAREA+6           LOAD LEFT NIBBLE
         SRL   R1,4                    MOVE IT OVER
         CH    R1,=H'10'               IF IT'S GREATER THAN 10....
         BL    LT10
         SRL   R1,1                        DIVIDE BY TWO
LT10     EQU   *
         SPACE 1
EXIT30   EQU   *
         L     R9,SAVE30
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE RETURNS THE SQUARE ROOT OF A NUMBER.               *
***********************************************************************
         SPACE 2
SQRTRTN  DS    0H
         ST    R9,SAVE40
         B     BSAVE40
SAVE40   DC    F'-1'
         DC    CL8'SQRTRTN '
BSAVE40  EQU   *
         SPACE 1
         CH    R1,=H'1'                SQRT OF 1 = 1
         BNH   EXIT40
         STM   R2,R6,SQRTSV5R          SAVE USERS REGISTERS
         XR    R2,R2                   INIT LOW REG TO 0
         LR    R3,R1                        HIGH REG TO X
SQRTLOOP EQU   *
         LR    R6,R3                   IF DISTANCE BETWEEN
         SR    R6,R2                       LOW AND HIGH
         CH    R6,=H'1'                    EQUAL TO 1....
         BNE   SQRTBYP1
         LR    R4,R3                       PUT HIGH IN R4
         B     SQRTEND                     GOTO END
SQRTBYP1 EQU   *
         LR    R4,R3                   GET HIGH
         AR    R4,R2                   GET LOW
         SRL   R4,1                    AVERAGE - TRUNCATED
         LR    R5,R4                   SAVE IT
         STH   R5,SQRTHALF             AGAIN - IN MEMORY
         MH    R5,SQRTHALF             GET SQUARE
         CR    R5,R1                   COMPARE
         BL    SQRTLOW                 LOW
         BE    SQRTEND                 =
         BH    SQRTHIGH                HIGH
SQRTLOW  EQU   *
         LR    R2,R4                   RE-INIT LOW PTR
         B     SQRTLOOP
SQRTHIGH EQU   *
         LR    R3,R4                   RE-INIT HIGH PTR
         B     SQRTLOOP
SQRTEND  EQU   *
         LR    R1,R4                   PUT ANSWER INTO R1
         LM    R2,R6,SQRTSV5R          RESTORE USERS REGISTERS
         SPACE 1
EXIT40   EQU   *
         L     R9,SAVE40
         BR    R9
         SPACE 3
SQRTSV5R DS    5F
SQRTHALF DS    H
         EJECT
***********************************************************************
*     THIS ROUTINE CONTROLS THE INTERACTION WITH THE USER             *
***********************************************************************
         SPACE 2
INTERACT DS    0H
         ST    R9,SAVE50
         B     BSAVE50
SAVE50   DC    F'-1'
         DC    CL8'INTERACT'
BSAVE50  EQU   *
         SPACE 1
INTER1PT EQU   *
         BAL   R9,DISPLAYS             BUILD AND INVOKE PRIMARY SCREEN
         BAL   R9,CALLCMND             INVOKE REQUESTED COMMAND
         BAL   R9,REACTION             THE KLINGONS FIRE BACK
         B     INTER1PT                INFERNAL LOOP
         SPACE 1
EXIT50   EQU   *
         L     R9,SAVE50
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE REBUILDS THE MAIN DISPLAY SCREEN                   *
***********************************************************************
         SPACE 2
DISPLAYS DS    0H
         ST    R9,SAVE60
         B     BSAVE60
SAVE60   DC    F'-1'
         DC    CL8'DISPLAYS'
BSAVE60  EQU   *
         SPACE 1
*--> RESET SCREEN
         MVI   SC1LN17+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN18+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN19+5,C'.'          RESET COMMAND DOTS
         MVC   SC1LN20+2(4),=CL4'....' RESET COMMAND DOTS
*--> FORMAT LOGON RANGE SENSORS
         LA    R1,STARCHRT             INIT PTR TO STAR CHART
         LA    R2,SC1LN02+3                        SCREEN
         LA    R3,4                    4 LINES PER SECTOR
DSPLOOP1 EQU   *
         LA    R5,4                    QUADRANTS PER LINE
DSPLOOP2 EQU   *
         LA    R4,64                   64 POSITIONS PER QUADRANT
         ZAP   ACCUM1,=PL1'0'
         ZAP   ACCUM2,=PL1'0'
         ZAP   ACCUM3,=PL1'0'
DSPLOOP3 EQU   *
         CLI   0(R1),X'00'             EMPTY?
         BE    DSPEMPTY
         CLI   0(R1),X'01'             KLINGON?
         BNE   DSPTYPE2
         AP    ACCUM1,=PL1'1'          COUNT KLINGONS
         B     DSPEMPTY
DSPTYPE2 EQU   *
         CLI   0(R1),X'02'             BASE
         BNE   DSPTYPE3
         CP    ACCUM2,=PL1'0'          CAN'T HAVE TWO BASE IN 1 QUAD
         BE    DSPTYP2A
         MVI   0(R1),X'00'             SORRY - CLEAR IT OUT
         SP    GAMEBASE,=PL1'1'        SUBTRACT FROM TOTAL PER GAME
         B     DSPTYPE3
DSPTYP2A EQU   *
         AP    ACCUM2,=PL1'1'          LET USER HAVE AT LEAST 1
         B     DSPEMPTY
DSPTYPE3 EQU   *
         CLI   0(R1),X'03'             STAR?
         BNE   DSPTYPE4
         AP    ACCUM3,=PL1'1'          COUNT STARS
         B     DSPEMPTY
DSPTYPE4 EQU   *
         ST    R1,KIRKPTR              STORE ADDR OF ENTERPRISE
DSPEMPTY EQU   *
         LA    R1,1(R1)                BUMP STAR CHART PTR
         BCT   R4,DSPLOOP3             LOOP THRU MINOR INDEX
         UNPK  0(1,R2),ACCUM1          PUT KLINGONS IN LONG RANGE
         OI    0(R2),X'F0'
         UNPK  1(1,R2),ACCUM2              BASES
         OI    1(R2),X'F0'
         UNPK  2(1,R2),ACCUM3              STARS
         OI    2(R2),X'F0'
         LA    R2,6(R2)                BUMP SCREEN PTR
         BCT   R5,DSPLOOP2             ITERATE THRU SECOND LEVEL INDEX
         LA    R2,136(R2)              MOVE SCREEN PTR TO NEXT LINE
         BCT   R3,DSPLOOP1             ITERATE THRU MAJOR INDEX
*--> FORMAT QUADRANT INDICATOR
         L     R2,KIRKPTR              GET RELATIVE ADDR OF KIRK
         LA    R3,STARCHRT             GET BASE ADDR
         SR    R2,R3
         SRL   R2,6                    DIVIDE BY 64
         ST    R2,KIRKPTR              SAVE RELATIVE QUADRANT
         MH    R2,=H'3'                MULTIPLY BY QUADCVT1 LENGTH
         LA    R3,QUADCVT1             GET ADDR OF CONVERSION TABLE
         AR    R3,R2                   GET DISP INTO TABLE
         MVC   SC1LN13+67(3),0(R3)     PUT CURRENT QUADRANT IN SCREEN
*--> FORMAT SHORT RANGE SENSORS AND POST COMBAT STATUS
         L     R3,KIRKPTR              GET KIRK'S POSITION
         SLL   R3,6                    MULTIPLY BY 64
         A     R3,=A(STARCHRT)         GET ADDR OF QUADRANT
         LR    R4,R3
         CLI   STATUS,C'4'             IS THE ENTERPRISE DOCKED?
         BE    DSPSTBYP                YES, BYPASS
         XC    TRTABLE,TRTABLE         CLEAR SEARCH TABLE
         MVI   TRTABLE+X'01',X'01'     POST KLINGON FOR SEARCH
         TRT   0(64,R3),TRTABLE        SEARCH QUADRANT FOR KLINGON
         BC    7,DSPR1                 FOUND, GO POST COND RED
         CP    GAMENRGY,=PL3'1000'     LOW ON ENERGY
         BNH   DSPY1                          GO POST COND YELLOW
         CP    GAMEYRS,=PL1'5'         LOW ON YEARS
         BNH   DSPY1                          GO POST COND YELLOW
         MVI   STATUS,C'1'             POST COND GREEN
         MVC   CONDATTR+8(3),=XL3'0042F4'  GREEN ON SCREEN
         B     DSPSTBYP
DSPY1    EQU   *
         MVI   STATUS,C'2'             POST COND YELLOW
         MVC   CONDATTR+8(3),=XL3'0042F6'  YELLOW ON SCREEN
         B     DSPSTBYP
DSPR1    EQU   *
         MVI   STATUS,C'3'             POST COND RED
         MVC   CONDATTR+8(3),=XL3'F142F2'  BLINK RED ON SCREEN
*--> COMPUTE SCREEN EFFICIENCY PERCENTAGE
         ZAP   WORKDBLW,GAMENRGY       GET REMAINING ENERGY
         MP    WORKDBLW,=PL2'100'      * 100
         DP    WORKDBLW,=PL3'5000'     DIVIDE BY STARTING ENERGY
         ZAP   DEFLECT,WORKDBLW+3(2)   SAVE THE PERCENTAGE
         MVC   SCMSG3,=CL44'YOU ARE UNDER ATTACK'
DSPSTBYP EQU   *
         LR    R2,R4                   RESTORE R2
         LA    R3,SC1LN02+53           GET ADDR OF SHORT RANGE SENSORS
         LA    R4,8                    8 LINES PER QUADRANT
DSPLOOP4 EQU   *
         LA    R5,8                    8 POSITIONS PER LINE
DSPLOOP5 EQU   *
         CLI   0(R2),X'00'             IS IT EMPTY?
         BNE   TRYK1
         MVI   0(R3),C'.'              MARK AS EMPTY
         B     DSPLOOP6
TRYK1    EQU   *
         CLI   0(R2),X'01'             IS IT A KLINGON?
         BNE   TRYB1
         MVI   0(R3),C'K'              MARK AS KLINGON
         B     DSPLOOP6
TRYB1    EQU   *
         CLI   0(R2),X'02'             IS IT A STAR BASE?
         BNE   TRYS1
         MVI   0(R3),C'B'              MARK AS STAR BASE
         B     DSPLOOP6
TRYS1    EQU   *
         CLI   0(R2),X'03'             IS IT A STAR?
         BNE   TRYE1
         MVI   0(R3),C'*'              MARK AS STAR
         B     DSPLOOP6
TRYE1    EQU   *
         CLI   0(R2),X'04'             IS IT THE ENTERPRISE?
         BNE   DSPLOOP6
         MVI   0(R3),C'E'              MARK AS ENTERPRISE
DSPLOOP6 EQU   *
         LA    R2,1(R2)                BUMP STARCHRT PTR
         LA    R3,3(R3)                     SCREEN PTR
         BCT   R5,DSPLOOP5             ITERATE THRU LOW INDEX
         LA    R3,56(R3)               GO TO NEXT LINE
         BCT   R4,DSPLOOP4             ITERATE THRU HIGH INDEX
*--> FORMAT OPERATING STATISTICS
         UNPK  SC1LN17+76(4),GAMENRGY   DISPLAY ENERGY RESERVES
         OI    SC1LN17+79,X'F0'
         UNPK  SC1LN18+78(2),GAMETORP           PHOTON TORPEDOES
         OI    SC1LN18+79,X'F0'
         UNPK  SC1LN19+78(2),GAMEYRS            REMAINING YEARS
         OI    SC1LN19+79,X'F0'
         UNPK  SC1LN20+78(2),GAMEKLNG           KLINGONS
         OI    SC1LN20+79,X'F0'
         UNPK  SC1LN21+77(2),GAMEBASE           BASES
         OI    SC1LN21+78,X'F0'
         UNPK  SC1LN22+76(3),DEFLECT            DEFLECTION PERCENTAGE
         OI    SC1LN22+78,X'F0'
         MVC   SC1LN23+73(6),GAMEDATE           STAR DATE
*--> FORMAT BATTLE CONDITION
         CLI   STATUS,C'1'
         BNE   MAYBEY
         MVC   SC1LN24+73(6),=CL6'GREEN '
         B     STATUS1
MAYBEY   EQU   *
         CLI   STATUS,C'2'
         BNE   MAYBER
         MVC   SC1LN24+73(6),=CL6'YELLOW'
         B     STATUS1
MAYBER   EQU   *
         CLI   STATUS,C'3'
         BNE   MAYBED
         MVC   SC1LN24+73(6),=CL6'RED   '
         B     STATUS1
MAYBED   EQU   *
         CLI   STATUS,C'4'
         BNE   STATUS1
         MVC   SC1LN24+73(6),=CL6'DOCKED'
STATUS1  EQU   *
*--> PUT MESSAGES IN SCREEN
         MVC   SC1LN22(44),SCMSG1
         MVC   SC1LN23(44),SCMSG2
         MVC   SC1LN24(44),SCMSG3
*--> PUT OUT THE SCREEN AND THEN RETURN IT
SCPUT1   EQU   *
         TPUT  SCREEN1,1944,FULLSCR    PAINT THE SCREEN
         TM    ATTRIB+3,X'01'          DOES THIS TUBE SUPPORT COLOR?
         BNO   NOCOLOR                 BRANCH AROUND COLORS
         TPUT  SC1COLOR,LSC1CLR,NOEDIT WRITE SPECIALTY COLORS
NOCOLOR  EQU   *
         TGET  SCRETURN,7              RETURN IT
         LA    R1,0(R1)                CLEAR POSSIBLE GARBAGE
         LTR   R1,R1                   IS IT ZERO?
         BZ    SCPUT1                  YES, GO PAINT AGAIN
         CH    R1,=H'7'                DID USER DELETE ANYTHING?
         BNE   SCPUT1                  NOT PERMITTED
         CLI   SCRETURN+00,C'0'           USER REQUEST DIRECTORY
         BE    SCHELP1
         MVC   SC1LN17+5(1),SCRETURN+0      MOVE COMMAND
         MVC   SC1LN18+5(1),SCRETURN+1      MOVE COMMAND
         MVC   SC1LN19+5(1),SCRETURN+2      MOVE COMMAND
         MVC   SC1LN20+2(4),SCRETURN+3      MOVE COMMAND
         MVI   SCMSG1,C' '             CLEAR MSG LINE 1
         MVC   SCMSG1+1(43),SCMSG1
         MVC   SCMSG2,SCMSG1                 MSG LINE 2
         MVC   SCMSG3,SCMSG2                 MSG LINE 3
         B     EXIT60
SCHELP1  EQU   *
         L     R2,=A(SCREEN4)          GET ADDRESS OF SCREEN4
         TPUT  (R2),1936,FULLSCR       PAINT THE SCREEN
         TGET  SCRETURN,1              RETURN IT
         B     SCPUT1
         SPACE 2
EXIT60   EQU   *
         TPUT  CLSCREEN,12,FULLSCR
         L     R9,SAVE60
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE ROUTES CONTROL TO THE COMMAND ROUTINES             *
***********************************************************************
         SPACE 2
CALLCMND DS    0H
         ST    R9,SAVE70
         B     BSAVE70
SAVE70   DC    F'-1'
         DC    CL8'CALLCMND'
BSAVE70  EQU   *
         SPACE 1
         MVI   TRTABLE,C'0'
         MVC   TRTABLE+1(L'TRTABLE-1),TRTABLE
         MVC   TRTABLE+C'1'(9),=CL9'123456789'
         TR    SC1LN17+5(1),TRTABLE    *
         TR    SC1LN18+5(1),TRTABLE    *  RESET NON-NUMERICS
         TR    SC1LN19+5(1),TRTABLE    *    TO NUMERICS
         TR    SC1LN20+2(4),TRTABLE    *
         CLI   SC1LN17+5,C'9'          END OF STAR TREK
         BE    GOHOME
         CLI   SC1LN17+5,C'1'          COMMAND 1
         BNE   CCMND2
         BAL   R9,CMD1                 * WARP DRIVE INTER-QUADRANT
         B     CCMNDNON
CCMND2   EQU   *
         CLI   SC1LN17+5,C'2'          COMMAND 2
         BNE   CCMND3
         BAL   R9,CMD2                 * WARP DRIVE INTRA-QUADRANT
         B     CCMNDNON
CCMND3   EQU   *
         CLI   SC1LN17+5,C'3'          COMMAND 3
         BNE   CCMND4
         BAL   R9,CMD3                 * PHOTON TORPEDOES
         B     CCMNDNON
CCMND4   EQU   *
         CLI   SC1LN17+5,C'4'          COMMAND 4
         BNE   CCMND5
         BAL   R9,CMD4                 * PHASER CONTROL
         B     CCMNDNON
CCMND5   EQU   *
CCMNDNON EQU   *
         B     EXIT70
GOHOME   EQU   *
         TPUT  CLSCREEN,12,FULLSCR
         L     R13,SAVEAREA+4
         LM    R14,R12,12(R13)
         LA    R15,0
         BR    R14
         SPACE 1
EXIT70   EQU   *
         L     R9,SAVE70
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE HANDLES TRAVEL BETWEEN QUADRANTS                   *
***********************************************************************
         SPACE 2
CMD1     DS    0H
         ST    R9,SAVE80
         B     BSAVE80
SAVE80   DC    F'-1'
         DC    CL8'CMD1    '
BSAVE80  EQU   *
         SPACE 1
         CLI   SC1LN18+5,C'1'          *
         BL    CMD1BADC                * X MUST BE 1 - 4
         CLI   SC1LN18+5,C'4'          *
         BH    CMD1BADC
         CLI   SC1LN19+5,C'1'          *
         BL    CMD1BADC                * Y MUST BE 1 - 4
         CLI   SC1LN19+5,C'4'          *
         BNH   CMD1GOOD
CMD1BADC EQU   *
         MVC   SCMSG1,=CL44'THE COORDINATES YOU SUPPLIED ARE NOT'
         MVC   SCMSG2,=CL44'IN THE ROMULA ANDROS IV SECTOR'
         B     EXIT80
CMD1GOOD EQU   *
         PACK  WORKDBLW,SC1LN13+67(1)  GET X AXIS OF KIRK
         CVB   R2,WORKDBLW
         PACK  WORKDBLW,SC1LN18+05(1)  GET X AXIS OF DESTINATION
         CVB   R3,WORKDBLW
         SR    R2,R3                   GET DIFFERENCE
         BNL   CMD1POS1
         MH    R2,=H'-1'               GET ABSOLUTE VALUE
CMD1POS1 EQU   *
         STH   R2,WRKHALF1             STORE
         MH    R2,WRKHALF1             GET SQUARE
         STH   R2,WRKHALF1             SAVE IT
         PACK  WORKDBLW,SC1LN13+69(1)  GET Y AXIS OF KIRK
         CVB   R2,WORKDBLW
         PACK  WORKDBLW,SC1LN19+05(1)  GET Y AXIS OF DESTINATION
         CVB   R3,WORKDBLW
         SR    R2,R3                   GET DIFFERENCE
         BNL   CMD1POS2
         MH    R2,=H'-1'               GET ABSOLUTE VALUE
CMD1POS2 EQU   *
         STH   R2,WRKHALF2             STORE
         MH    R2,WRKHALF2             GET SQUARE
         AH    R2,WRKHALF1             ADD SQUARES
         LR    R1,R2                   MOVE FOR SQRT RTN
         BAL   R9,SQRTRTN              GET SQUARE ROOT
         CVD   R1,WORKDBLW             CONVERT RADIX
         SP    GAMEYRS,WORKDBLW        SUBTRACT FROM YEARS LEFT
         BNH   KIRKDEAD                < 1 FEDERATION IS CONQUERED
         PACK  WRKFULL1,GAMEDATE(4)    PACK DATE
         AP    WRKFULL1,WORKDBLW       SUBTRACT YEARS
         UNPK  GAMEDATE(4),WRKFULL1    REINSTATE DATE
         OI    GAMEDATE+3,X'F0'        CLEAR SIGN BIT
         MH    R1,=H'50'               ENERGY = 50 UNITS PER YEAR
         CVD   R1,WORKDBLW             CONVERT RADIX
         SP    GAMENRGY,WORKDBLW       SUBTRACT FROM ENERGY TOTAL
         BNH   KIRKDEAD                < 0 FEDERATION IS CONQUERED
         L     R2,KIRKPTR              GET QUADRANT OF KIRK
         SLL   R2,6                    MULTIPLY BY 64
         A     R2,=A(STARCHRT)         ADD BASE ADDRESS
         LA    R3,64                   INIT BCT REGISTER
CMD1KRK1 EQU   *
         CLI   0(R2),X'04'             IS THIS KIRK
         BE    CMD1KRK2                YES, GO CLEAR HIM FROM QUAD
         LA    R2,1(R2)                NO, BUMP BY 1
         BCT   R3,CMD1KRK1             BCT
         B     CMD1KRK3                GO AROUND CLEAR
CMD1KRK2 EQU   *
         MVI   0(R2),X'00'             CLEAR KIRK
CMD1KRK3 EQU   *
         MVC   QUADFIND+0(1),SC1LN18+5 BUILD NEW QUADRANT ADDRESS
         MVI   QUADFIND+1,C','
         MVC   QUADFIND+2(1),SC1LN19+5
         LA    R2,QUADCVT1             GET ADDRESS OF CONVERT TABLE
         XR    R3,R3                   CLEAR FIND REGISTER
CMD1KRK4 EQU   *
         CH    R3,=H'16'               END OF TABLE?
         BE    CMD1KRK5                YES, LEAVE
         CLC   QUADFIND,0(R2)          THIS ENTRY IN TABLE?
         BE    CMD1KRK6                YES, THIS IS IT
         LA    R2,3(R2)                NO, BUMP POINTER
         LA    R3,1(R3)
         B     CMD1KRK4
CMD1KRK5 EQU   *
         ISK   0,0                     IMPOSSIBLE - BOMB
CMD1KRK6 EQU   *
         ST    R3,KIRKPTR              STORE NEW KIRK PTR
         SLL   R3,6                    MULTIPLY BY 64
         A     R3,=A(STARCHRT)         ADD BASE ADDRESS
         BAL   R9,RANDOM10             GET RANDOM FACTOR
         SLL   R1,1                    MULTIPLY BY 2
         AR    R3,R1                   ADD RANDOM FACTOR
         LA    R4,64                   SET UP FOR BCT
CMD1KRK7 EQU   *
         CLI   0(R3),X'00'             IS THIS SLOT EMPTY
         BE    CMD1KRK8                YES, PUT KIRK IN IT
         LA    R3,1(R3)                BUMP PTR
         BCT   R4,CMD1KRK7             ITERATE
         ISK   0,0                     IMPOSSIBLE, BOMB
CMD1KRK8 EQU   *
         MVI   0(R3),X'04'             PUT KIRK IN SLOT
         MVI   SC1LN17+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN18+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN19+5,C'.'          RESET COMMAND DOTS
         MVC   SC1LN20+2(4),=CL4'....' RESET COMMAND DOTS
         SPACE 1
EXIT80   EQU   *
         L     R9,SAVE80
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE HANDLES TRAVEL INSIDE QUADRANTS                    *
***********************************************************************
         SPACE 2
CMD2     DS    0H
         ST    R9,SAVE90
         B     BSAVE90
SAVE90   DC    F'-1'
         DC    CL8'CMD2    '
BSAVE90  EQU   *
         SPACE 1
         CLI   SC1LN18+5,C'1'          *
         BL    CMD2BADC                * X MUST BE 1 - 8
         CLI   SC1LN18+5,C'8'          *
         BH    CMD2BADC
         CLI   SC1LN19+5,C'1'          *
         BL    CMD2BADC                * Y MUST BE 1 - 8
         CLI   SC1LN19+5,C'8'          *
         BNH   CMD2GOOD
CMD2BADC EQU   *
         MVC   SCMSG1,=CL44'THE COORDINATES YOU SUPPLIED ARE NOT'
         MVC   SCMSG2,=CL44'IN THE CURRENT QUADRANT'
         B     EXIT90
CMD2GOOD EQU   *
         MVC   QUADFIND+0(1),SC1LN18+5 FORMAT POSITION ADDRESS
         MVC   QUADFIND+2(1),SC1LN19+5
         MVI   QUADFIND+1,C','
         LA    R2,QUADCVT2
         XR    R3,R3                   CLEAR FIND REGISTER
CMD2FULL EQU   *
         CLC   QUADFIND,0(R2)          THIS ENTRY IN TABLE?
         BE    CMD2FND                 YES, THIS IS IT
         LA    R2,3(R2)                NO, BUMP POINTER
         LA    R3,1(R3)
         B     CMD2FULL
CMD2FND  EQU   *
         L     R2,KIRKPTR              GET ABSOLUTE ADDRESS
         SLL   R2,6
         A     R2,=A(STARCHRT)
         LR    R5,R2
         AR    R2,R3
         CLI   0(R2),X'00'
         BE    CMD2NULL
         MVC   SCMSG1,=CL44'THE POINT THAT YOU WANT TO MOVE TO'
         MVC   SCMSG2,=CL44'IS ALREADY OCCUPIED'
         B     EXIT90
CMD2NULL EQU   *
         LA    R4,64
CMD2FIND EQU   *
         CLI   0(R5),X'04'             IS THIS KIRK?
         BE    CMD2FND2                YES, END OF SEARCH
         LA    R5,1(R5)
         BCT   R4,CMD2FIND
         ISK   0,0
CMD2FND2 EQU   *
*--> COMPUTE ENERGY COST OF MOVE
         LR    R6,R5                   SAVE PTR
         S     R6,=A(STARCHRT)         SUBTRACT START ADDRESS
         L     R7,KIRKPTR              GET QUAD PTR
         SLL   R7,6                    * 64
         SR    R6,R7                   SUBTRACT FROM RESIDUAL
         MH    R6,=H'3'                * NODE LENGTH
         LA    R7,QUADCVT2             TABLE BEGIN AREA
         AR    R6,R7                   ADD OFFSET
         PACK  WORKDBLW,QUADFIND+0(1)
         CVB   R8,WORKDBLW
         PACK  WORKDBLW,0(1,R6)
         CVB   R7,WORKDBLW
         SR    R8,R7
         BNL   CMD2POS1
         MH    R8,=H'-1'
CMD2POS1 EQU   *
         STH   R8,WRKHALF1             STORE
         MH    R8,WRKHALF1             GET SQUARE
         STH   R8,WRKHALF1             SAVE IT
         PACK  WORKDBLW,QUADFIND+2(1)
         CVB   R8,WORKDBLW
         PACK  WORKDBLW,2(1,R6)
         CVB   R7,WORKDBLW
         SR    R8,R7                   GET DIFFERENCE
         BNL   CMD2POS2
         MH    R8,=H'-1'               GET ABSOLUTE VALUE
CMD2POS2 EQU   *
         STH   R8,WRKHALF2             STORE
         MH    R8,WRKHALF2             GET SQUARE
         AH    R8,WRKHALF1             ADD SQUARES
         LR    R1,R8                   MOVE FOR SQRT RTN
         BAL   R9,SQRTRTN              GET SQUARE ROOT
         MH    R1,=H'10'
         CVD   R1,WORKDBLW
         SP    GAMENRGY,WORKDBLW
         BNH   KIRKDEAD
         MVI   0(R5),X'00'             CLEAR OLD POSITION
         MVI   0(R2),X'04'             MARK NEW POSITION
         MVI   SC1LN17+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN18+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN19+5,C'.'          RESET COMMAND DOTS
         MVC   SC1LN20+2(4),=CL4'....' RESET COMMAND DOTS
         SPACE 1
EXIT90   EQU   *
         L     R9,SAVE90
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE HANDLES PHOTON TORPEDOES                           *
***********************************************************************
         SPACE 2
CMD3     DS    0H
         ST    R9,SAVE100
         B     BSAVE100
SAVE100  DC    F'-1'
         DC    CL8'CMD3    '
BSAVE100 EQU   *
         SPACE 1
         CLI   SC1LN18+5,C'1'          *
         BL    CMD3BADC                * X MUST BE 1 - 8
         CLI   SC1LN18+5,C'8'          *
         BH    CMD3BADC
         CLI   SC1LN19+5,C'1'          *
         BL    CMD3BADC                * Y MUST BE 1 - 8
         CLI   SC1LN19+5,C'8'          *
         BNH   CMD3GOOD
CMD3BADC EQU   *
         MVC   SCMSG1,=CL44'THE COORDINATES YOU SUPPLIED ARE NOT'
         MVC   SCMSG2,=CL44'IN THE CURRENT QUADRANT'
         B     EXIT100
CMD3GOOD EQU   *
         MVC   QUADFIND+0(1),SC1LN18+5 FORMAT POSITION ADDRESS
         MVC   QUADFIND+2(1),SC1LN19+5
         MVI   QUADFIND+1,C','
         LA    R2,QUADCVT2
         XR    R3,R3                   CLEAR FIND REGISTER
CMD3FULL EQU   *
         CLC   QUADFIND,0(R2)          THIS ENTRY IN TABLE?
         BE    CMD3FND                 YES, THIS IS IT
         LA    R2,3(R2)                NO, BUMP POINTER
         LA    R3,1(R3)
         B     CMD3FULL
CMD3FND  EQU   *
         L     R2,KIRKPTR              GET ABSOLUTE ADDRESS
         SLL   R2,6
         A     R2,=A(STARCHRT)
         LR    R5,R2
         AR    R2,R3
         SP    GAMENRGY,=PL2'10'       COST IS 10 UNITS - ENERGY
         BNH   KIRKDEAD
         SP    GAMETORP,=PL1'1'                1 TORPEDO
         CLI   0(R2),X'00'             IS SLOT EMPTY
         BNE   CMD3TRYS
CMD3MISS EQU   *
         MVC   SCMSG1,=CL44'YOU MISSED, EITHER BY SUPPLYING THE WRONG'
         MVC   SCMSG2,=CL44'COORDINATES OR BY BEING UNLUCKY'
         B     CMD3CLR
CMD3TRYS EQU   *
         CLI   0(R2),X'03'             IS IT A STAR
         BNE   CMD3TRYB
         MVI   0(R2),X'00'             CLEAR STAR
         MVC   SCMSG1,=CL44'YOU JUST WALLOPED A STAR'
         SP    GAMESTRS,=PL1'1'        DECREMENT STAR COUNT
         B     CMD3CLR
CMD3TRYB EQU   *
         CLI   0(R2),X'02'             IS IT A BASE
         BNE   CMD3TRYK
         MVI   0(R2),X'00'             CLEAR BASE
         MVC   SCMSG1,=CL44'CONGRATULATIONS, IDIOT, YOU JUST'
         MVC   SCMSG2,=CL44'DESTROYED ONE OF YOUR OWN BASES'
         SP    GAMEBASE,=PL1'1'
         BNH   KIRKDEAD
         BNL   CMD3BLOW
         ZAP   GAMEBASE,=PL1'0'
         B     CMD3BLOW
CMD3TRYK EQU   *
         CLI   0(R2),X'01'             IS IT A KLINGON
         BNE   CMD3CLR
         BAL   R9,RANDOM10             GET RANDOM NUMBER
         CH    R1,=H'7'
         BE    CMD3MISS
         CH    R1,=H'8'
         BE    CMD3MISS
         CH    R1,=H'9'
         BE    CMD3MISS
         MVI   0(R2),X'00'             CLEAR IT
         SP    GAMEKLNG,=PL1'1'        DECREMENT KLINGON COUNT
         BE    KIRKWIN
         MVC   SCMSG1,=CL44'YOU DESTROYED THE KLINGON'
         B     CMD3CLR
CMD3BLOW EQU   *
         L     R2,=A(SCREEN5)          GET ADDR OF SCREEN5
         TPUT  (R2),1936,FULLSCR
CMD3CLR  EQU   *
         MVI   SC1LN17+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN18+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN19+5,C'.'          RESET COMMAND DOTS
         MVC   SC1LN20+2(4),=CL4'....' RESET COMMAND DOTS
         SPACE 2
EXIT100  EQU   *
         L     R9,SAVE100
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE HANDLES PHASERS                                    *
***********************************************************************
         SPACE 2
CMD4     DS    0H
         ST    R9,SAVE110
         B     BSAVE110
SAVE110  DC    F'-1'
         DC    CL8'CMD4    '
BSAVE110 EQU   *
         SPACE 1
         CLI   SC1LN18+5,C'1'          *
         BL    CMD4BADC                * X MUST BE 1 - 8
         CLI   SC1LN18+5,C'8'          *
         BH    CMD4BADC
         CLI   SC1LN19+5,C'1'          *
         BL    CMD4BADC                * Y MUST BE 1 - 8
         CLI   SC1LN19+5,C'8'          *
         BNH   CMD4GOOD
CMD4BADC EQU   *
         MVC   SCMSG1,=CL44'THE COORDINATES YOU SUPPLIED ARE NOT'
         MVC   SCMSG2,=CL44'IN THE CURRENT QUADRANT'
         B     EXIT110
CMD4GOOD EQU   *
         MVC   QUADFIND+0(1),SC1LN18+5 FORMAT POSITION ADDRESS
         MVC   QUADFIND+2(1),SC1LN19+5
         MVI   QUADFIND+1,C','
         LA    R2,QUADCVT2
         XR    R3,R3                   CLEAR FIND REGISTER
CMD4FULL EQU   *
         CLC   QUADFIND,0(R2)          THIS ENTRY IN TABLE?
         BE    CMD4FND                 YES, THIS IS IT
         LA    R2,3(R2)                NO, BUMP POINTER
         LA    R3,1(R3)
         B     CMD4FULL
CMD4FND  EQU   *
         L     R2,KIRKPTR              GET ABSOLUTE ADDRESS
         SLL   R2,6
         A     R2,=A(STARCHRT)
         LR    R5,R2
         AR    R2,R3
         CLI   0(R2),X'00'             IS SLOT EMPTY
         BNE   CMD4TRYS
CMD4MISS EQU   *
         MVC   SCMSG1,=CL44'YOU MISSED, EITHER BY SUPPLYING THE WRONG'
         MVC   SCMSG2,=CL44'COORDINATES OR BY BEING UNLUCKY'
         B     CMD4CLR
CMD4TRYS EQU   *
         CLI   0(R2),X'03'             IS IT A STAR
         BNE   CMD4TRYB
         MVC   SCMSG1,=CL44'YOU CANNOT DESTROY A STAR WITH PHASERS'
         B     CMD4CLR
CMD4TRYB EQU   *
         CLI   0(R2),X'02'             IS IT A BASE
         BNE   CMD4TRYK
         MVC   SCMSG1,=CL44'YOU CANNOT DESTROY A BASE WITH PHASERS'
         B     CMD4CLR
CMD4TRYK EQU   *
         CLI   0(R2),X'01'             IS IT A KLINGON
         BNE   CMD4MISS
         CLC   SC1LN20+2(4),=CL4'0499' MINIMUM ENERGY
         BL    CMD4LOW
         CLC   SC1LN20+2(4),=CL4'1000' MAXIMUM ENERGY
         BH    CMD4BLOW
         BAL   R9,RANDOM10
         CH    R1,=H'6'                60% CHANCE
         BNH   CMD4BLOW
CMD4LOW  EQU   *
         MVC   SCMSG1,=CL44'INSUFFICIENT ENERGY TO PHASERS'
         B     CMD4CLR
CMD4BLOW EQU   *
         MVI   0(R2),X'00'             CLEAR KLINGON FROM MAP
         L     R2,=A(SCREEN5)          GET ADDR OF SCREEN5
         TPUT  (R2),1936,FULLSCR
         MVC   SCMSG1,=CL44'YOU DESTROYED THE KLINGON'
         SP    GAMEKLNG,=PL1'1'
         BE    KIRKWIN
CMD4CLR  EQU   *
         PACK  WORKDBLW,SC1LN20+2(4)  SUBTRACT RATED ENERGY
         SP    GAMENRGY,WORKDBLW
         BNH   KIRKDEAD
         MVI   SC1LN17+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN18+5,C'.'          RESET COMMAND DOTS
         MVI   SC1LN19+5,C'.'          RESET COMMAND DOTS
         MVC   SC1LN20+2(4),=CL4'....' RESET COMMAND DOTS
         SPACE 2
EXIT110  EQU   *
         L     R9,SAVE110
         BR    R9
         EJECT
***********************************************************************
*     THIS ROUTINE GIVES ANY KLINGONS PRESENT IN THE CURRENT QUAD THE *
*   OPPORTUNITY TO FIRE BACK AT THE ENTERPRISE.                       *
***********************************************************************
         SPACE 2
REACTION DS    0H
         ST    R9,SAVE120
         B     BSAVE120
SAVE120  DC    F'-1'
         DC    CL8'REACTION'
BSAVE120 EQU   *
         SPACE 1
         L     R2,KIRKPTR              GET KIRK'S QUAD
         SLL   R2,6                    * 64
         A     R2,=A(STARCHRT)         ADD BASE ADDR
         LA    R3,64                   SET TO 64
         LR    R5,R2                   SAVE BASE
RCTFND1  EQU   *
         CLI   0(R2),X'02'             IS IT A BASE?
         BE    RCTFND2                 YES, GO LOCATE
         LA    R2,1(R2)                BUMP PTR
         BCT   R3,RCTFND1              ITERATE
         B     RCTCOUNT                NO BASE, GO RETALIATE
RCTFND2  EQU   *
         SR    R2,R5                   GET OFFSET
         MH    R2,=H'3'                * NODE LENGTH
         A     R2,=A(QUADCVT2)         ADD BASE ADDR
         PACK  ACCUM1,0(1,R2)          X AXIS
         PACK  ACCUM2,2(1,R2)          Y AXIS
         L     R2,KIRKPTR              GET (X, Y) OF BASE
         SLL   R2,6                    PROCESS SAME AS ABOVE
         A     R2,=A(STARCHRT)
         LA    R3,64
         LR    R5,R2
RCTFND3  EQU   *
         CLI   0(R2),X'04'
         BE    RCTFND4
         LA    R2,1(R2)
         BCT   R3,RCTFND3
         B     RCTCOUNT
RCTFND4  EQU   *
         SR    R2,R5
         MH    R2,=H'3'
         A     R2,=A(QUADCVT2)
         MVC   QUADFIND,0(R2)
         PACK  ACCUM3,QUADFIND+0(1)
         PACK  ACCUM4,QUADFIND+2(1)
         CP    ACCUM1,ACCUM3           ARE X COOR'S SAME
         BE    RCTFND5
         CP    ACCUM2,ACCUM4               Y
         BNE   RCTCOUNT
         SP    ACCUM1,ACCUM3
         OI    ACCUM1,X'0F'
         CLI   ACCUM1,X'1F'            IS DIFFERENCE 1
         BNE   RCTCOUNT                GO, GO RETALIATE
         B     RCTDOCK                 YES, GO POST DOCKED STATUS
RCTFND5  EQU   *
         SP    ACCUM2,ACCUM4
         OI    ACCUM2,X'0F'
         CLI   ACCUM2,X'1F'            IS DIFFERENCE 1
         BNE   RCTCOUNT                GO, GO RETALIATE
RCTDOCK  EQU   *
         MVI   STATUS,C'4'
         MVC   CONDATTR+8(3),=XL3'0042F4'  GREEN ON SCREEN
         ZAP   GAMENRGY,=PL3'5000'     REPLENISH ENERGY
         ZAP   GAMETORP,=PL2'10'                 TORPEDOES
         ZAP   DEFLECT,=PL2'100'                 DEFLECTION %
         B     EXIT120
RCTCOUNT EQU   *
         L     R2,KIRKPTR              ESTABLISH
         SLL   R2,6
         A     R2,=A(STARCHRT)
         LA    R3,64
         XR    R4,R4
RCTCNT1  EQU   *
         CLI   0(R2),X'01'             COUNT KLINGONS
         BNE   RCTCNT2
         LA    R4,1(R4)
RCTCNT2  EQU   *
         LA    R2,1(R2)
         BCT   R3,RCTCNT1
         MH    R4,=H'500'              RETALIATION ENERGY = 500
         LA    R4,0(R4)                PER KLINGON MINUS
         CVD   R4,WORKDBLW             SCREEN EFFICIENCY
         ZAP   WRKFULL1,=PL2'100'
         SP    WRKFULL1,DEFLECT
         MP    WORKDBLW,WRKFULL1
         DP    WORKDBLW,=PL2'100'
         SP    GAMENRGY,WORKDBLW(6)
         BNH   KIRKDEAD
         MVI   STATUS,C'1'
         MVC   CONDATTR+8(3),=XL3'0042F4'  GREEN ON SCREEN
         SPACE 1
EXIT120  EQU   *
         L     R9,SAVE120
         BR    R9
         EJECT
KIRKDEAD DS    0H
         TPUT  CLSCREEN,12,FULLSCR
         TPUT  DEADMSG1,L'DEADMSG1
         TPUT  DEADMSG2,L'DEADMSG2
         TPUT  DEADMSG3,L'DEADMSG3
         L     R13,SAVEAREA+4
         LM    R14,R12,12(R13)
         LA    R15,0
         BR    R14
         SPACE 3
         SPACE 3
KIRKWIN  DS    0H
         TPUT  CLSCREEN,12,FULLSCR
         TPUT  WINMSG1,L'WINMSG1
         TPUT  WINMSG2,L'WINMSG2
         TPUT  WINMSG3,L'WINMSG3
         L     R13,SAVEAREA+4
         LM    R14,R12,12(R13)
         LA    R15,0
         BR    R14
         EJECT
***********************************************************************
*            W O R K I N G   S T O R A G E                            *
***********************************************************************
         SPACE 3
DEADMSG1 DC    CL33'THE ENTERPRISE HAS BEEN DEFEATED'
DEADMSG2 DC    CL33'THE FEDERATION WILL BE CONQUERED'
DEADMSG3 DC    CL33'MAYBE IN YOUR NEXT LIFE.........'
WINMSG1  DC    CL33'THE KLINGONS HAVE BEEN DEFEATED '
WINMSG2  DC    CL33'THE FEDERATION IS VICTORIOUS    '
WINMSG3  DC    CL34'MAYBE THE KLINGONS WILL TRY AGAIN'
CLSCREEN DC    XL12'C11140403C4040001140C113'
PRMSZE   DS    CL2           PRIMARY SCREEN SIZE - HEX ROW/COLS
ALTSZE   DS    CL2           ALTERNATE
ATTRIB   DS    F             SCREEN ATTRIBUTES - COLOR, ETC.
TIMEAREA DS    D             RETURN AREA FOR TIME MACRO
SCRETURN DS    CL16                          TGET FULLSCR
STARCHRT DS    CL1024        STAR CHART - 16 X 64 DOT MATRIX
INITKLNG DS    PL2           INIT AREA FOR KLINGONS
INITBASE DS    PL1           INIT AREA FOR STAR BASES
INITSTRS DS    PL2           INIT AREA FOR STARS
ACCUM1   DS    PL1
ACCUM2   DS    PL1
ACCUM3   DS    PL1
ACCUM4   DS    PL1
TRTABLE  DS    CL256         WORK AREA FOR TRT'S
WORKDBLW DS    D             WORK AREA FOR CVD & CVB
WRKHALF1 DS    H             WORK AREA
WRKHALF2 DS    H             WORK AREA
WRKFULL1 DS    F             WORK AREA
QUADFIND DS    CL3           WORK AREA FOR QUAD CONVERSIONS
SCMSG1   DC    CL44' '
SCMSG2   DC    CL44' '
SCMSG3   DC    CL44' '
KIRKPTR  DC    F'0'
         SPACE 3
         LTORG
         EJECT
***********************************************************************
*  THIS IS THE GAME TABLE - IT CONTAINS 10 GAMES                      *
***********************************************************************
GAMETBL  EQU   *
         DC    PL2'08',PL2'50',PL1'2',PL2'20'
         DC    PL2'09',PL2'51',PL1'2',PL2'20'
         DC    PL2'10',PL2'52',PL1'2',PL2'21'
         DC    PL2'11',PL2'53',PL1'2',PL2'21'
         DC    PL2'12',PL2'54',PL1'2',PL2'22'
         DC    PL2'13',PL2'55',PL1'3',PL2'22'
         DC    PL2'14',PL2'56',PL1'3',PL2'23'
         DC    PL2'15',PL2'57',PL1'3',PL2'23'
         DC    PL2'16',PL2'58',PL1'4',PL2'24'
         DC    PL2'17',PL2'59',PL1'4',PL2'24'
         SPACE 1
GAMESTAT DS    0CL7
GAMEKLNG DS    PL2           NUMBER OF KLINGONS FOR THIS GAME
GAMESTRS DS    PL2                     STARS
GAMEBASE DS    PL1                     BASES
GAMEYRS  DS    PL2                     YEARS
         SPACE 1
GAMENRGY DC    PL4'5000'     AMOUNT OF ENERGY TO START
GAMEDATE DC    CL6'5537.1'   STAR DATE OF THE GAME
GAMETORP DC    PL2'10'       PHOTON TORPEDOES
STATUS   DC    CL1'1'        1=GREEN, 2=YELLOW, 3=RED, 4=DOCKED
DEFLECT  DC    PL5'100'      DEFLECTION ENERGY PERCENTAGE
         SPACE 3
***********************************************************************
*  CONVERSION TABLES FROM INTERNAL QUAD NUMBERS TO WORLD COORDINATES  *
***********************************************************************
         SPACE 1
QUADCVT1 DC    CL24'1,42,43,44,41,32,33,34,3'
         DC    CL24'1,22,23,24,21,12,13,14,1'
         SPACE 1
QUADCVT2 DC    CL24'1,82,83,84,85,86,87,88,8'
         DC    CL24'1,72,73,74,75,76,77,78,7'
         DC    CL24'1,62,63,64,65,66,67,68,6'
         DC    CL24'1,52,53,54,55,56,57,58,5'
         DC    CL24'1,42,43,44,45,46,47,48,4'
         DC    CL24'1,32,33,34,35,36,37,38,3'
         DC    CL24'1,22,23,24,25,26,27,28,2'
         DC    CL24'1,12,13,14,15,16,17,18,1'
         EJECT
***********************************************************************
*  EXTENDED COLOR FIELDS FOR MAIN BATTLE COMPUTER                     *
***********************************************************************
         SPACE 1
SC1COLOR EQU   *
         DC    XL1'F1'                 WRITE COMMAND
         DC    XL1'C2'                 WCC - RELEASE KEYBOARD
*--> SHORT RANGE SENSORS
         DC    XL16'11C2C32903C0F8410042F611C25B1DE8'
         DC    XL16'11C3D32903C0F8410042F611C36B1DE8'
         DC    XL16'11C4E32903C0F8410042F611C47B1DE8'
         DC    XL16'11C5F32903C0F8410042F611C64B1DE8'
         DC    XL16'11C7C32903C0F8410042F611C75B1DE8'
         DC    XL16'11C8D32903C0F8410042F611C86B1DE8'
         DC    XL16'11C9E32903C0F8410042F611C97B1DE8'
         DC    XL16'114AF32903C0F8410042F6114B4B1DE8'
*--> COMMAND ENTRY CHARACTERS
         DC    XL11'11D4C42C03C0C941F142F2'
         DC    XL11'11D5D42C03C0C941F142F2'
         DC    XL11'11D6E42C03C0C941F142F2'
         DC    XL11'11D7F12C03C0C941F142F2'
*--> UNDERLINE THE QUADRANT INDICATOR
         DC    XL16'1150C12903C0F841F442F31150C71DE8'
*--> LONG RANGE SENSORS
         DC    XL16'11C1D22903C0F8410042F511C1D61DE8'
         DC    XL16'11C1D82903C0F8410042F511C15C1DE8'
         DC    XL16'11C15E2903C0F8410042F511C1E21DE8'
         DC    XL16'11C1E42903C0F8410042F511C1E81DE8'
         DC    XL16'11C3F22903C0F8410042F511C3F61DE8'
         DC    XL16'11C3F82903C0F8410042F511C37C1DE8'
         DC    XL16'11C37E2903C0F8410042F511C4C21DE8'
         DC    XL16'11C4C42903C0F8410042F511C4C81DE8'
         DC    XL16'11C6D22903C0F8410042F511C6D61DE8'
         DC    XL16'11C6D82903C0F8410042F511C65C1DE8'
         DC    XL16'11C65E2903C0F8410042F511C6E21DE8'
         DC    XL16'11C6E42903C0F8410042F511C6E81DE8'
         DC    XL16'11C8F22903C0F8410042F511C8F61DE8'
         DC    XL16'11C8F82903C0F8410042F511C87C1DE8'
         DC    XL16'11C87E2903C0F8410042F511C9C21DE8'
         DC    XL16'11C9C42903C0F8410042F511C9C81DE8'
*--> REVERSE VIDEO THE USS ENTERPRISE
         DC    XL16'11405F2903C0F841F242F11140611DE8'
         DC    XL16'1140E62903C0F841F242F11140E81DE8'
         DC    XL16'11C16F2903C0F841F242F111C1F11DE8'
         DC    XL16'11C1F62903C0F841F242F111C1F81DE8'
         DC    XL16'11C27F2903C0F841F242F111C3C11DE8'
         DC    XL16'11C3C62903C0F841F242F111C3C81DE8'
         DC    XL16'11C55F2903C0F841F242F111C5611DE8'
         DC    XL16'11C5E62903C0F841F242F111C5E81DE8'
         DC    XL16'11C66F2903C0F841F242F111C6F11DE8'
         DC    XL16'11C6F62903C0F841F242F111C6F81DE8'
         DC    XL16'11C77F2903C0F841F242F111C8C11DE8'
         DC    XL16'11C8C62903C0F841F242F111C8C81DE8'
         DC    XL16'11C94F2903C0F841F242F111C9D11DE8'
         DC    XL16'11C9D62903C0F841F242F111C9D81DE8'
         DC    XL16'114A5F2903C0F841F242F1114A611DE8'
         DC    XL16'114AE62903C0F841F242F1114AE81DE8'
         DC    XL16'114B6F2903C0F841F242F1114BF11DE8'
         DC    XL16'114BF62903C0F841F242F1114BF81DE8'
         DC    XL16'114C7F2903C0F841F242F1114DC11DE8'
         DC    XL16'114DC62903C0F841F242F1114DC81DE8'
         DC    XL16'114E4F2903C0F841F242F1114ED11DE8'
         DC    XL16'114ED62903C0F841F242F1114ED81DE8'
         DC    XL16'114F5F2903C0F841F242F1114F611DE8'
         DC    XL16'114F662903C0F841F242F1114F681DE8'
         DC    XL16'11506F2903C0F841F242F11150F11DE8'
         DC    XL16'1150F62903C0F841F242F11150F81DE8'
*--> STATISTICS
         DC    XL16'11D54A2903C0F8410042F411D54F1DE8'
         DC    XL16'11D65C2903C0F8410042F411D65F1DE8'
         DC    XL16'11D76C2903C0F8410042F411D76F1DE8'
         DC    XL16'11D87C2903C0F8410042F411D87F1DE8'
         DC    XL16'115A4C2903C0F8410042F4115A4F1DE8'
         DC    XL16'115B5B2903C0F8410042F4115B5F1DE8'
         DC    XL16'115CE82903C0F8410042F4115C6F1DE8'
CONDATTR DC    XL16'115DF82903C0F8410042F4115D7F1DE8'
*--> INSERT CURSOR AT COMMAND ENTRY FIELD
         DC    XL4'11D4C513'           SET BUFFER ADDR/COMMAND ENTRY
ESC1CLR  EQU   *
LSC1CLR  EQU   ESC1CLR-SC1COLOR
         EJECT
***********************************************************************
*  MAIN BATTLE COMPUTER SCREEN LAYOUT                                 *
***********************************************************************
SCREEN1  DC    XL13'401140403C4040001140401DE8'
SC1LN01  DC    CL039'=========================      U      U'
         DC    CL40'          ============================  '
SC1LN02  DC    CL40' = XXX = XXX = XXX = XXX =      S      S'
         DC    CL40'     Y-8  =  .  .  .  .  .  .  .  .  =  '
SC1LN03  DC    CL40' =========================      S      S'
         DC    CL40'       7  =  .  .  .  .  .  .  .  .  =  '
SC1LN04  DC    CL40' = XXX = XXX = XXX = XXX =              '
         DC    CL40'       6  =  .  .  .  .  .  .  .  .  =  '
SC1LN05  DC    CL40' =========================      E      E'
         DC    CL40'       5  =  .  .  .  .  .  .  .  .  =  '
SC1LN06  DC    CL40' = XXX = XXX = XXX = XXX =      N      N'
         DC    CL40'       4  =  .  .  .  .  .  .  .  .  =  '
SC1LN07  DC    CL40' =========================      T      T'
         DC    CL40'       3  =  .  .  .  .  .  .  .  .  =  '
SC1LN08  DC    CL40' = XXX = XXX = XXX = XXX =      E      E'
         DC    CL40'       2  =  .  .  .  .  .  .  .  .  =  '
SC1LN09  DC    CL40' =========================      R      R'
         DC    CL40'       1  =  .  .  .  .  .  .  .  .  =  '
SC1LN10  DC    CL40' SECTOR - ROMULA ANDROS IV      P      P'
         DC    CL40'          ============================  '
SC1LN11  DC    CL40' LONG RANGE SENSORS             R      R'
         DC    CL40'         X - 1  2  3  4  5  6  7  8     '
SC1LN12  DC    CL40' POSITION 1 - KLINGONS          I      I'
         DC    CL40'              SHORT RANGE SENSORS       '
SC1LN13  DC    CL40' POSITION 2 - STAR BASES        S      S'
         DC    CL40'               QUADRANT = (X,Y)         '
SC1LN14  DC    CL40' POSITION 3 - STARS             E      E'
         DC    CL40'                                        '
SC1LN15  DC    CL40'                                        '
         DC    CL40'               OPERATING STATISTICS     '
SC1LN16  DC    CL40'************* COMMUNICATIONS ***********'
         DC    CL41'******    ==============================*'
SC1LN17  DC    CL41'     .  <---ENTER COMMAND (0 FOR DIRECTOR'
         DC    CL41'Y)   *    ENERGY..................(0000)*'
SC1LN18  DC    CL41'     .  <---ENTER X AXIS COORDINATE      '
         DC    CL41'     *    PHOTON TORPEDOES..........(00)*'
SC1LN19  DC    CL41'     .  <---ENTER Y AXIS COORDINATE      '
         DC    CL41'     *    YEARS REMAINING...........(00)*'
SC1LN20  DC    CL41'  ....  <---ENTER ENERGY QUANTITY        '
         DC    CL40'     *    KLINGONS..................(00)'
SC1LN21  DC    CL40'****************************************'
         DC    CL40'******    STAR BASES................(00)'
SC1LN22  DC    CL40'                                        '
         DC    CL40'          DEFLECTION ENERGY (%)....(000)'
SC1LN23  DC    CL40'                                        '
         DC    CL40'          STAR DATE.............(0000.0)'
SC1LN24  DC    CL40'                                        '
         DC    CL40'          COMBAT STATUS.........(YELLOW)'
         DC    XL4'11D4C513'
         ORG   SC1LN17+3
         DC    XL2'1DC9'
         ORG   SC1LN17+6
         DC    XL2'1DF8'
         ORG   SC1LN18+3
         DC    XL2'1DC9'
         ORG   SC1LN18+6
         DC    XL2'1DF8'
         ORG   SC1LN19+3
         DC    XL2'1DC9'
         ORG   SC1LN19+6
         DC    XL2'1DF8'
         ORG   SC1LN20+0
         DC    XL2'1DC9'
         ORG   SC1LN20+6
         DC    XL2'1DF8'
         ORG
         EJECT
***********************************************************************
*  MAIN BATTLE COMPUTER SCREEN LAYOUT                                 *
***********************************************************************
SCREEN2  DC    XL13'441140403C4040001140401DE8'
SC2LN01  DC    CL039'   SSSSSSSSSSS      TTTTTTTTTTTTTTTT   '
         DC    CL40'   AAAAAAAAAAAA      RRRRRRRRRRRRRR     '
SC2LN02  DC    CL40'   SS         SS     TTTTTTTTTTTTTTTT   '
         DC    CL40'  AAAAAAAAAAAAAA     RR           RR    '
SC2LN03  DC    CL40'  SS           SS           TT          '
         DC    CL40' AA            AA    RR            RR   '
SC2LN04  DC    CL40'  SS           SS           TT          '
         DC    CL40' AA            AA    RR            RR   '
SC2LN05  DC    CL40'   SS                       TT          '
         DC    CL40' AA            AA    RR           RR    '
SC2LN06  DC    CL40'    SSSSSSSSSSS             TT          '
         DC    CL40' AAAAAAAAAAAAAAAA    RRRRRRRRRRRRRR     '
SC2LN07  DC    CL40'              SS            TT          '
         DC    CL40' AAAAAAAAAAAAAAAA    RR       RR        '
SC2LN08  DC    CL40'  SS           SS           TT          '
         DC    CL40' AA            AA    RR        RR       '
SC2LN09  DC    CL40'  SS           SS           TT          '
         DC    CL40' AA            AA    RR         RR      '
SC2LN10  DC    CL40'   SS         SS            TT          '
         DC    CL40' AA            AA    RR          RR     '
SC2LN11  DC    CL40'    SSSSSSSSSSS             TT          '
         DC    CL40' AA            AA    RR           RR    '
SC2LN12  DC    CL40'                                        '
         DC    CL40'                                        '
SC2LN13  DC    CL40'                                        '
         DC    CL40'                                        '
SC2LN14  DC    CL40'TTTTTTTTTTTTTTTTT    RRRRRRRRRRRRRR     '
         DC    CL40' EEEEEEEEEEEEEEEE    KK            KK   '
SC2LN15  DC    CL40'TTTTTTTTTTTTTTTTT    RR           RR    '
         DC    CL40' EEEEEEEEEEEEEEEE    KK          KK     '
SC2LN16  DC    CL40'       TT            RR            RR   '
         DC    CL40' EE                  KK        KK       '
SC2LN17  DC    CL40'       TT            RR            RR   '
         DC    CL40' EE                  KK      KK         '
SC2LN18  DC    CL40'       TT            RR           RR    '
         DC    CL40' EE                  KK    KK           '
SC2LN19  DC    CL40'       TT            RRRRRRRRRRRRRR     '
         DC    CL40' EEEEEEEEEE          KK  KKK            '
SC2LN20  DC    CL40'       TT            RR      RR         '
         DC    CL40' EE                  KKKK  KK           '
SC2LN21  DC    CL40'       TT            RR       RR        '
         DC    CL40' EE                  KK      KK         '
SC2LN22  DC    CL40'       TT            RR        RR       '
         DC    CL40' EE                  KK        KK       '
SC2LN23  DC    CL40'       TT            RR         RR      '
         DC    CL40' EEEEEEEEEEEEEEEE    KK          KK     '
SC2LN24  DC    CL40'       TT            RR          RR     '
         DC    CL40' EEEEEEEEEEEEEEEE    KK            KK   '
         DC    XL4'11404013'
         EJECT
***********************************************************************
*  MAIN BATTLE COMPUTER SCREEN LAYOUT                                 *
***********************************************************************
SCREEN3  DC    XL13'401140403C4040001140401DE8'
SC3LN01  DC    CL039'                     **** WELCOME TO ST'
         DC    CL40'AR TREK ****                            '
SC3LN02  DC    CL40'                                        '
         DC    CL40'                                        '
SC3LN03  DC    CL40'    HI!  WELCOME TO STAR TREK!  THIS GAM'
         DC    CL40'E IS BASED ON THE STAR TREK TV SERIES,  '
SC3LN04  DC    CL40'AND IS INTENDED FOR THOSE OF US THAT JUS'
         DC    CL40'T NEVER WERE ABLE TO GET ENOUGH.        '
SC3LN05  DC    CL40'                                        '
         DC    CL40'                                        '
SC3LN06  DC    CL40'    THE CONTEXT OF THE GAME IS A SPACE B'
         DC    CL40'ATTLE BETWEEN THE FEDERATION STARSHIP   '
SC3LN07  DC    CL40'ENTERPRISE AND SEVERAL KLINGON VESSELS. '
         DC    CL40' THE ENTERPRISE HAS BEEN PATROLLING THE '
SC3LN08  DC    CL40'ROMULA ANDROS IV SECTOR AT THE OUTBREAK '
         DC    CL40'OF INTERSTELLAR WAR.  THE MISSION IS NOW'
SC3LN09  DC    CL40'TO DEFEAT THE KLINGON VESSELS IN A SPECI'
         DC    CL40'FIC NUMBER OF "PARALLAX YEARS".         '
SC3LN10  DC    CL40'    FOLLOWING THIS PAGE, YOU WILL BE INT'
         DC    CL40'RODUCED TO THE MAIN SCREEN OF THE SHIPS '
SC3LN11  DC    CL40'BATTLE COMPUTER.  BY SKILLFULLY USING TI'
         DC    CL40'ME, PHOTON TORPEDOES, AND PHASERS, YOU  '
SC3LN12  DC    CL40'CAN DEFEAT THE KLINGONS AND SAVE THE FED'
         DC    CL40'ERATION.                                '
SC3LN13  DC    CL40'    THE SCREEN OF THE BATTLE COMPUTER IS'
         DC    CL40' MADE UP OF FOUR DISTINCT SECTIONS.  THE'
SC3LN14  DC    CL40'UPPER LEFT SECTION CONTAINS THE LONG RAN'
         DC    CL40'GE SENSORS.  THIS IS A DISPLAY OF THE   '
SC3LN15  DC    CL40'STATUS OF THE SECTOR, BROKEN INTO QUADRA'
         DC    CL40'NTS, AND REFERRED TO BY THEIR (X,Y) AXIS'
SC3LN16  DC    CL40'COORDINATES.  THE UPPER RIGHT SECTION CO'
         DC    CL40'NTAINS THE SHORT RANGE SENSORS.  THIS IS'
SC3LN17  DC    CL40'A DISPLAY OF THE STATUS OF THE QUADRANT '
         DC    CL40'YOU ARE IN.  STARS ARE REPRESENTED BY AN'
SC3LN18  DC    CL40'"*", THE ENTERPRISE BY "E", AND KLINGONS'
         DC    CL40' BY "K".  THIS LOWER RIGHT SECTION IS A '
SC3LN19  DC    CL40'DISPLAY OF THE CURRENT OPERATING STATIST'
         DC    CL40'ICS OF THE ENTERPRISE.  THE LOWER LEFT  '
SC3LN20  DC    CL40'SECTION IS THE AREA YOU USE TO COMMUNICA'
         DC    CL40'TE WITH THE BATTLE COMPUTER.            '
SC3LN21  DC    CL40'    IF YOU RUN OUT OF SUPPLIES OR ENERGY'
         DC    CL40' YOU CAN GET REFUELED BY GOING TO A STAR'
SC3LN22  DC    CL40'BASE.  NOTE, HOWEVER, THAT EVERY TIME YO'
         DC    CL40'U USE THE WARP DRIVE, IT COSTS YOU AT   '
SC3LN23  DC    CL40'LEAST A YEAR.  TO BYPASS THESE INTRODUCT'
         DC    CL40'ION SCREENS, CALL STREK WITH OPTION 3.  '
SC3LN24  DC    CL40'                         G O O D    L U '
         DC    CL40'C K    !!!!                             '
         DC    XL4'11404013'
         EJECT
***********************************************************************
*  MAIN BATTLE COMPUTER SCREEN LAYOUT                                 *
***********************************************************************
SCREEN4  DC    XL13'401140403C4040001140401DE8'
SC4LN01  DC    CL039'                   **** STAR TREK COMMA'
         DC    CL40'ND DIRECTORY ****                       '
SC4LN02  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN03  DC    CL40' 9<---- END THE GAME (THIS IMPLIES THAT '
         DC    CL40'YOU ARE SURRENDERING)                   '
SC4LN04  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN05  DC    CL40' 1<---- WARP DRIVE, INTERQUADRANT TRAVEL'
         DC    CL40' (BETWEEN QUADRANTS)                    '
SC4LN06  DC    CL40'        OPERAND 1 - ENTER THE X AXIS OF '
         DC    CL40'THE DESTINATION QUADRANT                '
SC4LN07  DC    CL40'        OPERAND 2 - ENTER THE Y AXIS OF '
         DC    CL40'THE DESTINATION QUADRANT                '
SC4LN08  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN09  DC    CL40' 2<---- IMPULSE POWER, INTRAQUADRANT TRA'
         DC    CL40'VEL (WITHIN THE CURRENT QUADRANT)       '
SC4LN10  DC    CL40'        OPERAND 1 - ENTER THE X AXIS OF '
         DC    CL40'THE DESTINATION POSITION                '
SC4LN11  DC    CL40'        OPERAND 2 - ENTER THE Y AXIS OF '
         DC    CL40'THE DESTINATION POSITION                '
SC4LN12  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN13  DC    CL40' 3<---- FIRE PHOTON TORPEDO             '
         DC    CL40'                                        '
SC4LN14  DC    CL40'        OPERAND 1 - ENTER THE X AXIS COO'
         DC    CL40'RDINATE OF THE TARGET                   '
SC4LN15  DC    CL40'        OPERAND 2 - ENTER THE Y AXIS COO'
         DC    CL40'RDINATE OF THE TARGET                   '
SC4LN16  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN17  DC    CL40' 4<---- FIRE PHASERS                    '
         DC    CL40'                                        '
SC4LN18  DC    CL40'        OPERAND 1 - ENTER THE X AXIS COO'
         DC    CL40'RDINATE OF THE TARGET                   '
SC4LN19  DC    CL40'        OPERAND 2 - ENTER THE Y AXIS COO'
         DC    CL40'RDINATE OF THE TARGET                   '
SC4LN20  DC    CL40'        OPERAND 3 - ENTER THE AMOUNT OF '
         DC    CL40'ENERGY FOR PHASER BURST                 '
SC4LN21  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN22  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN23  DC    CL40'                                        '
         DC    CL40'                                        '
SC4LN24  DC    CL40'                                        '
         DC    CL40'                                        '
         DC    XL4'11404013'
         EJECT
***********************************************************************
*  MAIN BATTLE COMPUTER SCREEN LAYOUT                                 *
***********************************************************************
SCREEN5  DC    XL13'441140403C4040001140401DE8'
SC5LN01  DC    CL039'          *****    *                   '
         DC    CL40'    .                  .                '
SC5LN02  DC    CL40' .        ***..*** *               *    '
         DC    CL40'            ***                         '
SC5LN03  DC    CL40'          *** ...**           .   *|*   '
         DC    CL40'          #*#*#     . .                 '
SC5LN04  DC    CL40' .  .      *.** . **.     .        |    '
         DC    CL40'.        #*#*# .                        '
SC5LN05  DC    CL40'              * **** *             |    '
         DC    CL40'          .      . .                    '
SC5LN06  DC    CL40'                .****** .     .    |    '
         DC    CL40'    .    .        .                     '
SC5LN07  DC    CL40'     .     .              .        |    '
         DC    CL40'        .    .                          '
SC5LN08  DC    CL40'                  .       .             '
         DC    CL40' .         % .       .                  '
SC5LN09  DC    CL40'                             .    @@@   '
         DC    CL40'         .%%%                           '
SC5LN10  DC    CL40'                                 @@@@@  '
         DC    CL40'        .                               '
SC5LN11  DC    CL40'  <---------------------------- @@@@@@@ '
         DC    CL40'----------------------------->          '
SC5LN12  DC    CL40'                  .              @@@@@  '
         DC    CL40'                                        '
SC5LN13  DC    CL40'              .      .      .     @@@   '
         DC    CL40'      .      .             .            '
SC5LN14  DC    CL40'              .            .           .'
         DC    CL40'   .    .            .                  '
SC5LN15  DC    CL40'      .   .                        |    '
         DC    CL40'                                        '
SC5LN16  DC    CL40'           .        . *   .   *    |    '
         DC    CL40'          **        .      .            '
SC5LN17  DC    CL40'                      * .   *      |    '
         DC    CL40'  .       ***                           '
SC5LN18  DC    CL40'                     *    *        |   .'
         DC    CL40'            **       .         .        '
SC5LN19  DC    CL40'      **          ***  . *         |    '
         DC    CL40'               *          .             '
SC5LN20  DC    CL40'       **  .     ****              |    '
         DC    CL40'       .        *     .                 '
SC5LN21  DC    CL40'                ****              *|*   '
         DC    CL40'          .      *                      '
SC5LN22  DC    CL40'                **                 *    '
         DC    CL40'                  *    .     .          '
SC5LN23  DC    CL40'         .                              '
         DC    CL40'                                        '
SC5LN24  DC    CL40'                                        '
         DC    CL40'                                        '
         DC    XL4'11404013'
         END   STREK
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR
//LKED.SYSIN DD *
 NAME    STARTREK(R)
//