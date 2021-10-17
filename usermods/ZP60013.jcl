//ZP60013  JOB (SYSGEN),'J08 M27: ZP60013',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*  MAINTAIN AN SVC EVENT COUNT FOR EACH SVC NUMBER.
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
++USERMOD(ZP60013)                     /* COUNT SVC EVENTS */  .
++VER(Z038) FMID(FBB1221)
 /*
   PROBLEM DESCRIPTION:
     THE NUMBER OF SVCS ISSUED IS NOT ACCURATELY KNOWN.
       THE SYSTEM DOES NOT PROVIDE A METHOD FOR TRACKING THE
       FREQUENCY OF SUPERVISOR CALLS (SVC INSTRUCTIONS) WITHOUT
       A GTF TRACE.

       THIS USERMOD CHANGES THE SVC FIRST LEVEL INTERRUPT HANDLER
       TO MAINTAIN A FULLWORD COUNTER FOR EACH OF THE 256 SVC
       NUMBERS (0 TO 255) IN A CONTIGUOUS TABLE.  THIS TABLE IS
       AVAILABLE TO MONITORS FOR TRACKING SVC ACTIVITY.  ACCESS
       TO THE TABLE IS VIA THE SECOND WORD OF THE SVC NEW PSW
       FLCSNPSW WHICH POINTS TO THE SVC FLIH.  AFTER THE SYSTEM
       IS REIPLED WITH THIS SYSMOD APPLIED, THE TWELVE BYTES
       BEFORE THE SVC FLIH ENTRY POINT CONTAIN THE EIGHT-BYTE
       LITERAL 'SVCCTTBL' AND A FOUR-BYTE POINTER TO THE TABLE.
       THE PRESENCE OF THE LITERAL SHOULD BE VERIFIED BEFORE
       AN ATTEMPT IS MADE TO ACCESS THE TABLE.

       ADDITIONALLY THE SVC FLIH WILL ABEND (S0F8) ISSUERS OF
       THE SVC INSTRUCTION WHICH ARE IN CROSS-MEMORY MODE.

   SPECIAL CONDITIONS:
     ACTION:
       AN IPL MUST BE PERFORMED FOR THIS SYSMOD TO BECOME ACTIVE.

   COMMENTS:
     PRYCROFT SIX P/L PUBLIC DOMAIN USERMOD FOR MVS 3.8 NUMBER 13.

     THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:
     MODULES:
       IEAVESVC
 */.
++MOD(IEAVESVC) DISTLIB(AOSC5).
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSALLDA,
//             SPACE=(CYL,3),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  DUMMY
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSALLDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS1.APVTMACS,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
         TITLE 'IEAVESVC- SVC FIRST LEVEL INTERRUPT HANDLER'
* /* START OF SPECIFICATIONS ****
*
*01*  MODULE-NAME = IEAVESVC
*
*02*     CSECT-NAME = IEAVESVC
*
*01*  DESCRIPTIVE-NAME = SVC FIRST LEVEL INTERRUPT HANDLER
*
*01*  COPYRIGHT = NONE
*
*01*  STATUS = VERSION 1
*
*01*  FUNCTION = SEE ENTRY POINT PURPOSES BELOW.
*
*02*     OPERATION = THE SVC FLIH WILL RECEIVE CONTROL VIA THE SVC NEW
*        PSW AFTER AN SVC INTERRUPT. ITS MAIN FUNCTION IS TO DETERMINE
*        IF THE ISSUER IS AUTHORIZED TO ISSUE THE SVC AND IF SO SET UP
*        THE PROPER ENVIRONMENT ON BEHALF OF THE SVC ROUTINE AND THEN
*        BRANCH TO THE ROUTINE.
*
*01*   NOTES = SEE ENTRY POINT DESCRIPTIONS.
*
*02*        DEPENDENCIES =
*
*           GENERAL SVC FLIH DEPENDENCIES:
*           1. NO SVCS MAY BE ISSUED BY ANY BRANCH ENTRY.
*           2. SUPERVISOR STATE,KEY 0 MUST BE MAINTAINED ACROSS
*           INTERFACES.
*           3. DISABLEMENT MUST BE PRESERVED BY ALL INTERFACES(EXCEPT
*           LOCK MANAGER WHERE IT APPLIES).
*           4. THE SVCTABLE CAN NOT CHANGE DYNAMICALLY EXCEPT THE TYPE.
*           IF THE TYPE IS CHANGED THE SVC ROUTINE MUST BE CAPABLE OF
*           RUNNING AS EITHER TYPE.
*
*           EXIT AND END OF TASK:
*           1. BOTH MUST RETURN THE SVRB TO THE SUPERVISOR SVRB
*           POOL.
*
*           SVC ROUTINES:
*           1. SVC ROUTINES AFTER COMPLETING THEIR FUNCTIONS MUST GO
*           TO THE EXIT PROLOGUE(IEAVEXPR) VIA A BRANCH 14(OR PICK UP
*           THE ADDRESS FROM CVTEXPRO).
*           THE SOLE EXCEPTION TO THIS APPLIES TO TYPE 6 SVC   @Z40FPYK
*           ROUTINES WHICH MUST RETURN TO THE SVC FLIH.  THIS  @Z40FPYK
*           MAY BE DONE BY A BR14 OR BY ISSUEING A T6EXIT      @Z40FPYK
*           MACRO.  UNDER NO CONDITIONS MAY A TYPE 6 SVC       @Z40FPYK
*           RETURN TO EITHER EXIT OR EXIT PROLOGUE.            @Z40FPYK
*           2. IF LOCKS WERE OBTAINED ON BEHALF OF AN SVC ROUTINE, A
*           SETFRR MUST BE ISSUED UPON ENTRY TO THE SVC ROUTINE TO
*           ESTABLISH RECOVERY. THIS MEANS THAT ALL TYPE 1 SVCS MUST DO
*           THIS SINCE THEY ARE ALWAYS ENTERED WITH THE LOCAL LOCK.
*           A TYPE 6 SVC ROUTINE MUST NOT EXIT TO THE SVC FLIH @Z40FPYK
*           WHILE HOLDING ANY LOCKS.  LOCKS WILL NOT BE        @Z40FPYK
*           RELEASED.                                          @Z40FPYK
*
*           GETMAIN(CONDITIONAL):
*           1. IF THE TCBABGM BIT IS ON, ALTHOUGH THE REQUEST WAS FOR
*           LSQS, GETMAIN WILL SATISFY THE REQUEST FROM SQS IF LSQS HAS
*           BEEN EXHAUSTED. IF NO SQS IS AVAILABLE, A NORMAL CODE
*           IS SENT BACK FROM GETMAIN.
*
*           RTM(ABTERM ENTRY):
*           1. THE LOCAL LOCK IS NOT NEEDED FOR THIS ENTRY SINCE THE
*           ENTRY IS ENTERED DISABLED AND THE CURRENT TCB IS THE ONE TO
*           BE TERMINATED.
*           2. ABTERM IS EXPECTED TO, IN ALL CASES SET THE TCBABGM BIT
*           WHEN A TASK IS SET UP FOR ABNORMAL TERMINATION. IN THIS WAY
*           ABEND MAY SAFELY ISSUE SVCS AND BE ASSURED THAT SVRBS WILL
*           BE SUPPLIED TO SATISFY THE REQUEST.
*
*           RTM(SVCERR ENTRY):
*           1. RTM WILL INITIATE SVC 13 PROCESSING, IF THE FRR STACK
*           IS EMPTY, EVENTHOUGH THE ISSUER OF THE SVC 13 WAS LOCKED IN
*           SOME WAY. RTM IN THIS CASE WILL FREE ALL OF THE LOCKS HELD
*           AND PROCEED WITH SVC 13 PROCESSING AND USE THE COMPLETION
*           CODE IN REGISTER 1 OF THE INTERRUPT SAVE AREA(LCCASGPR).
*           2. RTM MUST MOVE OR BE FINISHED WITH THE GENERAL REGISTER
*           SAVE AREA LCCASGPR BEFORE ENABLEMENT IS ALLOWED.
*           3. IF AN SVC 13 WAS NOT THE SVC THAT WAS ISSUED, THEN RTM
*           WILL USE THE COMPLETION CODE PASSED FROM THE SVC FLIH IN
*           REGISTER 1.
*
*           SYSGEN:
*           1. THE FOLLOWING VALIDITY CHECKING MUST BE DONE BY SYSGEN:
*              A. TYPES 3 AND 4 CAN NOT INDICATE GLOBAL SPIN LOCKS IN
*              THE SVC TABLE.
*              B. TYPE 1 SVCS MUST INDICATE THE LOCAL LOCK IN THE
*              SVC TABLE.
*              C. AN SVC MAY NOT INDICATE THE CMS LOCK IN THE SVC
*              TABLE WITHOUT ALSO INDICATING THE LOCAL LOCK.
*              D.  TYPE 6 SVC ROUTINES CAN NOT HAVE ANY LOCKS  @Z40FPYK
*              SPECIFIED IN THE SVC TABLE.                     @Z40FPYK
*           2. THE SVC TABLE WILL BE A FULL 256 ENTRY TABLE. THE TABLE
*           WILL CONTAIN AN ENTRY POINT ADDRESS FOR EVERY SVC NUMBER.
*           IF THE SVC NUMBER IS AN INVALID NUMBER, THE SVC MUST BE
*           DESIGNATED AS A TYPE 2 SVC WITH NO LOCKS REQUIRED UPON
*           ENTRY AND AN ENTRY POINT ADDRESS TO IGCERROR.
*           3. THE SVC TABLE MUST REFLECT LOCKING REQUIREMENTS RATHER
*           THAN THE ENABLED-DISABLED FLAG.
*           4. AN ENTRY STATEMENT MUST BE SUPPLIED TO THE BEGINNING
*           OF THE SVC TABLE CALLED SVCTABLE.
*           5. THE FORMAT OF THE SVC TABLE MUST BE AS FOLLOWS:
*           TWO FULL WORD ENTRIES.
*           WORD         BITS
*           1            0-31 ENTRY POINT ADDRESS
*           2            0-3 SVC TYPE: 0=TYPE 1
*                                      2=TYPE 6                @Z40FPYK
*                                      8=TYPE 2                @Z40FPYK
*                                      C=TYPE 3/4              @Z40FPYK
*           2            4  FUNCTION CODE(APF AUTHORIZED)
*                           1=AUTHORIZED,0=UNAUTHORIZED
*           2            5  SVC IS PART OF ESR
*           2            6  SVC IS NON-PREEMPTIVE              @Z40FPYK
*           2            7-15 RESERVED                         @Z40FPYK
*           2            16  LOCAL LOCK TO BE ACQUIRED
*           2            17  CMS LOCK TO BE ACQUIRED
*           2            18  SRM LOCK TO BE ACQUIRED
*           2            19  SALLOC LOCK TO BE ACQUIRED
*           2            20  DISP LOCK TO BE ACQUIRED
*           2            21-31 RESERVED FOR LOCKS
*
*    EXTENDED SVC ROUTER:
*        1.    ESR TABLE FORMAT:
*          A . AN EXSVC TABLE (ESRTABLE) EXTERNAL TO ESR RESIDES
*              IN THE NUCLEUS, CONTAINING A TWO WORD ENTRY FOR
*              EVERY EXTENDED SVC DEFINED TO THE SYSTEM.
*          B . THE ESRTABLE IS DIVIDED INTO FIVE SECTIONS:
*              1. MODID CONTAINING THE C-SECT NAME ESRTABLE IN
*                 EBCDIC.
*              2. HEADER FIELD CONTAINS 4 HALFWORDS OF INFORMATION
*                 USED BY NIP.
*              3. TYPE IV SECTION CONTINING ENTRIES FOR ALL
*                 EXSVCS INVOKED BY SVC 109.
*              4. TYPE I SECTION CONTAINING ENTRIES FOR ALL
*                 EXSVCS INVOKED BY SVC 116.
*              5. TYPE II SECTION CONTAINING ENTRIES FOR ALL
*                 EXSVCS INVOKED BY SVC 122.
*03*        CHARACTER-CODE-DEPENDENCIES = NONE
*
*02*     RESTRICTIONS = NONE
*
*02*     REGISTER-CONVENTIONS = REFERENCE EQUATE SECTION IN MODULE.
*
*02*     PATCH-LABEL = NONE(NUCLEUS RESIDENT)
*
*01*  MODULE-TYPE = PROCEDURE
*
*02*     PROCESSOR = ASSEMBLER
*
*02*     MODULE-SIZE =  REFERENCE ESD LENGTH AT BEGINNING OF MODULE.
*
*02*     ATTRIBUTES = NUCLEUS, ZERO PROTECT KEY, DISABLED, REFRESHABLE,
*        ADDRSPC=FIXED,SUPERVISOR MODE.
*
*01*  ENTRY-POINT = IEAQSC00
*
*02*     PURPOSE = THIS IS THE MAIN ENTRY INTO THE SVC INTERRUPT
*        HANDLER. IT WILL DECIDE WHETHER THE ISSUER CAN ISSUE SVCS, IF
*        THE ISSUER IS AUTHORIZED, WHAT TYPE OF SVC IS ASKED FOR AND
*        SET UP THE PROPER INPUT ENVIRONMENT FOR THE ROUTINE.
*
*02*     LINKAGE = SEE INPUT SECTION BELOW.
*
*02*     INPUT =
*        INPUT ENVIRONMENT:
*        STATE- DISABLED,SUPERVISOR
*        KEY-0
*        LOCKS REQUIRED-NONE
*        INPUT DATA:
*        SVC OLD PSW-LOCATION DECIMAL 32.
*        SVC INTERRUPT CODE-LOCATION DECIMAL 138.
*        ILC-LOCATION DECIMAL 136-137.
*        SEE CONTROL-BLOCKS SECTION BELOW FOR CONTROL BLOCK INPUTS.
*        INPUT REGISTERS:
*        NONE
*
*02*     OUTPUT = NONE UNIQUE TO THIS ENTRY
*
*01*  ENTRY POINT NAME:   IGXERROR
*        MODE OF ENTRY:  BRANCH ENTRY
*        SOURCE OF ENTRY POINT: VIA ADDRESS IN ESR TABLE
*        MACRO TO INVOKE FUNCTION: NONE
*02*     FUNCTION PROVIDED:
*            THIS ENTRY IS TAKEN VIA A BRANCH INSTRUCTION FROM
*            THE SVC INTERRUPT HANDLER MAINLINE. IT WAS TAKEN
*            AS A RESULT OF OBTAINING THE ADDRESS FROM THE ESR
*            TABLE AS THE ENTRY POINT TO THE EXSVC ROUTINE. IT
*            PROVIDES THE COMPLETION CODE PLUS AN ENTRY TO SVC
*            13.
*02*     INPUT ENVIRONMENT:
*            STATE: ENABLED, SUPERVISOR STATE
*            KEY: 0
*            LOCKS REQUIRED: NONE
*        INPUT DATA:
*            ALL AS NORMALLY RECEIVED BY AN SVC ROUTINE.
*        INPUT REGISTERS:
*            ALL AS NORMALLY RECEIVED BY AN SVC ROUTINE.
*        ENVIRONMENT CHANGES: NONE
*        FUNCTIONS CALLED:
*            SVC 13
*02*     EXIT POINTS:
*            TO: SVC 13
*
*                OUTPUT DATA - AS ON ENTRY
*                OUTPUT REGISTERS -
*                   1   -   COMPLETION CODE
*                OUTPUT ENVIRONMENT -
*                   ENABLED, SUPERVISOR STATE
*                   KEY 0
*                   UNLOCKED
*                RETURN CODES - NONE
*
*02*     ABEND CODES:
*             X'16D'   -   INVALID OR UNSUPPORTED EXSVC ISSUED
*01*  ENTRY-POINT = IGCERROR
*
*02*     PURPOSE = THIS ENTRY IS TAKEN VIA A BRANCH INSTRUCTION FROM
*        THE SVC INTERRUPT HANDLER MAINLINE. IT WAS TAKEN AS A RESULT
*        OF OBTAINING THE ADDRESS FROM THE SVC TABLE AS THE ENTRY POINT
*        TO THE SVC ROUTINE. IT PROVIDES THE COMPLETION CODE PLUS AN
*        ENTRY TO SVC 13.
*
*02*     LINKAGE = SEE INPUT SECTION BELOW.
*
*02*     INPUT =
*        INPUT ENVIRONMENT:
*        STATE-ENABLED,SUPERVISOR
*        KEY-0
*        LOCKS REQUIRED-NONE
*        INPUT DATA:
*        ALL AS NORMALLY RECEIVED BY SVC ROUTINE.
*        INPUT REGISTERS:
*        ALL AS NORMALLY RECEIVED BY SVC ROUTINE.
*
*02*     OUTPUT = NONE UNIQUE TO THIS ENTRY.
*                                                              @Z40FPYK
*01*  ENTRY-POINT = IEAVET6E                                   @Z40FPYK
*                                                              @Z40FPYK
*02*     PURPOSE = THIS ENTRY IS TAKEN VIA ISSUING A T6EXIT    @Z40FPYK
*        MACRO FROM A TYPE 6 SVC ROUTINE.  THE ROUTINE MAY     @Z40FPYK
*        REUTRN SELECTING ONE OF THREE OPTIONS--RETURN TO THE  @Z40FPYK
*        CALLER, EXIT TO THE DISPATCHER FUNCTION, OR SCHEDULE  @Z40FPYK
*        AN SRB.                                               @Z40FPYK
*                                                              @Z40FPYK
*02*     LINKAGE = SEE INPUT SECTION BELOW.                    @Z40FPYK
*                                                              @Z40FPYK
*02*     INPUT =                                               @Z40FPYK
*        INPUT ENVIRONMENT:                                    @Z40FPYK
*        STATE-DISABLED,SUPERVISOR                             @Z40FPYK
*        KEY-0                                                 @Z40FPYK
*        LOCKS REQUIRED-NONE MUST BE HELD                      @Z40FPYK
*        INPUT DATA:                                           @Z40FPYK
*        EXIT OPTION SELECTED BY THE TYPE 6 ROUTINE.           @Z40FPYK
*        INPUT REGISTERS:                                      @Z40FPYK
*        EXIT OPTION IS INDICATED AS FOLLOWS:                  @Z40FPYK
*             REGISTER 1 HAS SRB ADDRESS IF RETURN=SRB         @Z40FPYK
*             REGISTER 2 HAS AN EIGHT IF RETURN=SRB            @Z40FPYK
*             REGISTER 2 HAS A FOUR IF RETURN=DISPATCH         @Z40FPYK
*             REGISTER 2 HAS A ZERO IF RETURN=CALLER           @Z40FPYK
*                                                              @Z40FPYK
*02*     OUTPUT = NONE UNIQUE TO THIS ENTRY.
*
*01*  EXIT-NORMAL = EXIT PROLOGUE(IEAVEEXP) IF RETURN=CALLER   @Z40FPYK
*
*02*     CONDITIONS = IF THE TYPE 6 SVC ROUTINE DOES A BR14 OR @Z40FPYK
*                     A T6EXIT RETURN=CALLER, THE SVC FLIH     @Z40FPYK
*                     WILL EXIT TO EXIT PROLOGUE IN ORDER TO   @Z40FPYK
*                     RE-DISPATCH THE TASK (IF DISPATCHABILITY)@Z40FPYK
*                     TESTS ARE MET.                           @Z40FPYK
*
*02*     OUTPUT =                                              @Z40FPYK
*                   REGISTERS 0,1, AND 15 ARE RETAINED FOR     @Z40FPYK
*                   THE CALLER FROM THE SVC                    @Z40FPYK
*        OUTPUT REGISTERS -                                    @Z40FPYK
*                   R0,R1,R2,R3=UNUSED                         @Z40FPYK
*                   R4=TCB ADDRESS                             @Z40FPYK
*                   R5,R6=UNUSED                               @Z40FPYK
*                   R7=ASCB ADDRESS                            @Z40FPYK
*                   R8,R9,R10,R11,R12,R13=UNUSED               @Z40FPYK
*                   R14=EPA OF EXIT PROLOGUE                   @Z40FPYK
*                   R15=UNUSED                                 @Z40FPYK
*                                                              @Z40FPYK
*02*     RETURN-CODES  =  NONE                                 @Z40FPYK
*                                                              @Z40FPYK
*01*  EXIT-NORMAL = DISPATCHER (GLOBAL SRB DISPATCH ROUTINE)   @Z40FPYK
*
*02*     CONDITIONS = IF THE TYPE 6 SVC ROUTINE DOES A         @Z40FPYK
*                     A T6EXIT RETURN=SRB, THE SVC FLIH        @Z40FPYK
*                     WILL GO TO THE GLOBAL SRB DISPATCHER     @Z40FPYK
*                     ROUTINE IN ORDER TO DISPATCH THE SRB     @Z40FPYK
*                     IMMEDIATELY.                             @Z40FPYK
*
*02*     OUTPUT =                                              @Z40FPYK
*                   NO SPECIAL OUTPUT                          @Z40FPYK
*        OUTPUT REGISTERS -                                    @Z40FPYK
*                   R0,R1=UNUSED                               @Z40FPYK
*                   R2=SRB ADDRESS                             @Z40FPYK
*                   R3,R4=UNUSED                               @Z40FPYK
*                   R5=ASCB ADDRESS                            @Z40FPYK
*                   R6=UNUSED                                  @Z40FPYK
*                   R7=LCCA ADDRESS                            @Z40FPYK
*                   R8=UNUSED                                  @Z40FPYK
*                   R9=DISPATCHER BASE ADDRESS                 @Z40FPYK
*                   R10,R11,R12,R13,R14=UNUSED                 @Z40FPYK
*                   R15=DISPATCHER ENTRY POINT                 @Z40FPYK
*                                                              @Z40FPYK
*02*     RETURN-CODES  =  NONE                                 @Z40FPYK
*                                                              @Z40FPYK
*01*   EXIT-ERROR  = RTM                                       @Z40FPYK
*                                                              @Z40FPYK
*02*     CONDITIONS = THE ISSUER OF THE SVC WAS IN SRB MODE    @Z40FPYK
*                     OWNED A LOCK OR MADE AN ILLEGAL T6EXIT   @Z40FPYK
*                     REQUEST THAT A GOLBAL SRB BE SCHEDULED   @Z40FPYK
*                                                              @Z40FPYK
*02*     OUTPUT =
*        OUTPUT DATA-LCCASGPR(CONTAINS SAVED REGS),PLUS HARDWARE INFO.
*        OUTPUT REGISTERS- 1-CONTAINS COMPLETION CODE
*        OUTPUT ENVIRONMENT-DISABLED,KEY 0,SRB OR LOCKED OR BOTH.
*
*02*     RETURN-CODES = NONE
*
*01*  EXIT-NORMAL = SVC ROUTINE
*
*02*     CONDITIONS = AFTER ALL OF THE NECESSARY SET UP ON BEHALF OF
*        THE SVC ROUTINE HAS BEEN MADE , A BRANCH TO THE SVC ROUTINE IS
*        MADE VIA ITS CORRESPONDING ADDRESS IN THE SVC TABLE.
*
*02*     OUTPUT =
*        OUTPUT DATA-SVRB SET UP(FOR NON-TYPE1)
*        OUTPUT REGISTERS-
*              3=CVT ADDRESS
*              4=TCB ADDRESS
*              5=RB ADDRESS(SVRB IF NON-TYPE1)
*              6=ENTRY POINT ADDRESS
*              7=ASCB ADDRESS
*              14=RETURN ADDRESS
*              0,1,13,15 AS AT ENTRY TO SVC FLIH
*        OUTPUT ENVIRONMENT-
*              ENABLED OR DISABLED(DEPENDING ON LOCKS REQUESTED)
*              SUPERVISOR STATE
*              KEY 0
*              LOCKS AS INDICATED IN SVC TABLE.
*
*02*     RETURN-CODES = NONE
*
*01*  EXIT-ERROR = SVC 13
*
*02*     CONDITIONS = TO ABEND THE CURRENT TASK DUE TO A LOCK MANAGER
*        FAILURE(OTHER THAN LOCAL LOCK). OR TO ABEND THE CURRENT TASK
*        DUE TO ISSUANCE OF AN INVALID SVC.
*
*02*     OUTPUT =
*        OUTPUT DATA- REGS IN TCB(OR SVRB IF NON-TYPE1). PSW IN
*        RBOPSW FIELD.
*        OUTPUT REGISTERS-1=COMPLETION CODE
*        OUTPUT ENVIRONMENT-
*              ENABLED OR DISABLED (DEPENDING ON LOCK STATUS)
*              SUPERVISOR STATE
*              KEY 0
*              ANY LOCKS HELD
*
*02*     RETURN-CODES = NONE
*
*01*  EXIT-ERROR = IEAPDS7 (DISPATCHER)
*
*02*     CONDITIONS = MEMORY TERMINATION HAS BEEN INITIATED DUE TO A
*        FAILURE IN THE LOCK MANAGER IN GETTING THE LOCAL LOCK. SINCE
*        THE SVC INTERRUPT HANDLER IS OPERATING AS A PROLOGUE TO THE
*        SVC ROUTINE, THE NORMAL EXIT TO DISPATCHER IS APPROPRIATE.
*
*02*     OUTPUT =
*        OUTPUT DATA-NONE
*        OUTPUT REGISTERS-NONE
*        OUTPUT ENVIRONMENT-
*              ENABLED,KEY 0
*              SUPERVISOR STATE,UNLOCKED
*
*02*     RETURN-CODES = NONE
*
*01*  EXTERNAL-REFERENCES = SEE ROUTINES,DATA AREAS,CONTROL-BLOCKS
*     BELOW.
*
*02*     ROUTINES =
*        SVC 13
*        RTM(SVCERR)
*        RTM(ABTERM)
*        RTM(MEMTERM)
*        TRSVC(TRACE ENTRY)
*        GETCELL
*        GETMAIN(COND AND UNCOND FOR LSQS)
*        BLDCPOOL
*        SETLOCK(OBTAIN/RELEASE)
*        TESTAUTH
*        IEAPDS7(DISPATCHER ENTRY)
*        DSJSTCSR(DISPATCHER JOB STEP TIMING ENTRY POINT)      @Z40FPYK
*        DSSRBRTN(DISPATCHER GLOBAL SRB DISPATCHER ROUTINE)    @Z40FPYK
*        EXPEPAT6(EXIT PROLOGUE TYPE 6 SVC EXIT ENTRY POINT)   @Z40FPYK
*
*02*     DATA-AREAS = ALL INCLUDED IN CONTROL-BLOCKS BELOW.
*
*02*     CONTROL-BLOCKS =
*        PSA R/W
*        LCCA R/W
*        ASCB R/W
*        TCB R/W
*        RB R/W
*        CVT R
*        SVC TABLE R
*        ASXB R
*        ESRTABLE R
*01*  TABLES = NONE
*
*01*  MACROS =
*        ABEND
*        GETMAIN
*        BLDCPOOL
*        TESTAUTH
*        SETLOCK
*        CALLRTM
*        GETCELL
*        HOOK
*
*02*     SERIALIZATION =
*        DISABLE
*        TCB ACTIVE BIT
*        SVRB NOT ON QUEUE
*
*01*  CHANGE-ACTIVITY = Y02715,Y02751,Y02752,Z40FPYK           @Z40FPYK
*                       OZ09430 - VERIFY THAT THE COMPUTED     @ZA29595
*                                 ADDRESS OF THE ESR TABLE     @ZA29595
*                                 ENTRY IS NOT BEYOND THE END  @ZA29595
*                                 OF THE ESR TABLE.            @ZA29595
*                       OZ12726 - VERIFY THAT THE COMPUTED     @ZA29595
*                                 ADDRESS OF THE ESR TABLE     @ZA29595
*                                 ENTRY IS NOT BEFORE THE      @ZA29595
*                                 BEGINNING OF THE ESR TABLE.  @ZA29595
*                       OZ13392 - WHEN EXPANDING THE SVRB POOL,@ZA29595
*                                 CLEAR THE SCB AND ITS PARM   @ZA29595
*                                 AREA IN THE SVRB'S.          @ZA29595
*                       OZ15765 - PURGE THE FRR STACK UPON     @ZA29595
*                                 RETURN FROM A TYPE 6 SVC     @ZA29595
*                       OZ20352 - VERIFY THAT AN SRB RETURNED  @ZA29595
*                                 BY A TYPE 6 SVC DOES NOT     @ZA29595
*                                 HAVE CPU AFFINITY TO A       @ZA29595
*                                 DIFFERENT PROCESSOR.         @ZA29595
*                       OZ29595 - WHEN A SINGLE SVRB IS GOTTEN @ZA29595
*                                 FOR ABEND, MARK IT SO IT     @ZA29595
*                                 WILL BE FREED BY EXIT        @ZA29595
*                                 PROLOGUE, BECAUSE IT MAY BE  @ZA29595
*                                 GOTTEN FROM SQA.             @ZA29595
*
*     MODIFIED BY GREG PRICE FOR USERMOD ZP60013   2005-01-15   ZP60013
*                                                               ZP60013
*        1. ADD CROSS-MEMORY MODE INDICATED IN ISSUER'S PSW     ZP60013
*           AS ANOTHER REASON TO FAIL WITH ABEND S0F8.          ZP60013
*                                                               ZP60013
*        2. ADD A TABLE OF 256 FULLWORDS TO COUNT THE NUMBER    ZP60013
*           OF TIMES EACH (UNSCREENED) SVC IS ISSUED.           ZP60013
*                                                               ZP60013
*           THIS TABLE IS AVAILABLE FOR USE BY MONITORS:        ZP60013
*              FLCSNPSW+4 POINTS TO THE SVC FLIH.               ZP60013
*              IF SVC_FLIH-X'C' = CL8'SVCCTTBL'                 ZP60013
*              THEN SVC_FLIH-4 -> SVC COUNTER TABLE.            ZP60013
*
*01*  MESSAGES = NONE
*
*01*  ABEND-CODES =
*        X'047'-UNAUTHORIZED ISSUER OF SVC.
*        X'0F8'-SRB MODE, LOCKS WERE HELD, DISABLED OR XMEM.    ZP60013
*        X'0F9'-UNABLE TO EXPAND SVRB POOL.
*        X'0FA'-UNABLE TO OBTAIN SVRB STORAGE FOR ABTERM IN PROCESS.
*        X'16D'-INVALID OR UNSUPPORTED EXSVC ISSUED.
*        X'FXX'-UNDEFINED SVC ISSUED.(XXX IS NUMBER)
*
**** END OF SPECIFICATIONS ***/
         EJECT
***********************************************************************
*                                                                     *
*                     IEAVESVC MAINLINE                               *
*                                                                     *
***********************************************************************
IEAVESVC CSECT
*        MODID BR=NO
         DC    CL26'IEAVESVC UZ86400 11/09/78 '
         DC    CL18' ZP60013 20050115 '                         ZP60013
         DC    CL8'SVCCTTBL'       FLAG COUNTER TABLE PRESENCE  ZP60013
         DC    A(SVCCTTBL)         SVC COUNTER TABLE POINTER    ZP60013
         SPACE 2
*/*IEAVESVC: CHART SVC FLIH */
*/* HEADER
*/*
*/*
*/*
*/*                                               SECTION 3.1.8.1
*/* SVC FLIH
*/*                                               PAGE # */
         SPACE 2
*/*IEAQSC00: E IEAQSC00 */
         SPACE 2
         ENTRY IEAQSC00
IEAQSC00 DS    0H
         USING PSA,0
         SPACE 2
*/* P MAKE SVC FLIH STACK CURRENT */
         SPACE 2
         MVC   PSASSAV,PSACSTK         SAVE CURRENT FRR STACK
         MVC   PSACSTK,PSASSTK         MAKE SVC FLIH STACK CURRENT
*/* P SET SVC FLIH BIT PSASVC=1 */
         SPACE 2
         OI    PSASUP1,PSASVC          TURN SVC FLIH FLAG ON
         SPACE 2
*/* P SAVE ONE REG TO GET LCCA ADDR IN PSAGPREG */
         SPACE 2
         ST    R7,PSAGPREG             SAVE ONE REGISTER IN PSA
         SPACE 2
*/* P SAVE ALL REGS IN LCCASGPR */
         SPACE 2
         L     R7,PSALCCAV             GET ADDRESS OF LOGICAL CCA
         USING LCCA,R7                 ESTABLISH BASE TO LCCA
         STM   R0,R15,LCCASGPR         SAVE ALL REGISTERS      @YA02598
         L     R14,PSAGPREG            GET REGISTER SAVED INITIALLY
         ST    R14,LCCASGPR+C28        SAVE REGISTER USED      @YA02598
         SPACE 2
*/* P ESTABLISH ADDRESSABILITY */
         SPACE 2
         BALR  R9,C0                   ESTABLISH
         USING *,R9                    ADDRESSABILITY
         SPACE 2
*/*SRBTST: D (YES,SVCINV,NO,) SRB MODE? */
         SPACE 2
SRBTST   DC    0H'0'                   TEST LABEL
         TM    LCCADSF2,LCCASRBM       IS SRB MODE INDICATED
         BO    SVCINV                  IF SO,INVALID ISSUER
         SPACE 2
*/*LOCKTST: L (NO,,YES,SVCINV) SETLOCK-- ANY LOCKS HELD? */
         SPACE 2
LOCKTST  DC    0H'0'                   TEST LABEL
         SETLOCK TEST,TYPE=ALL,REGS=(R8),BRANCH=(HELD,SVCINV)
*                                      ANY LOCKS HELD?
         SPACE 2
*/* D (YES,SVCPROC,NO,SVCINV) USER ENABLED? */
         SPACE 2
         TM    FLCSOPSW,ENABLE         WAS USER ENABLED?
         BNO   SVCINV                  NO GO CALL RTM          @Z40FPYJ
         SPACE 2
*/* D (YES,SVCPROC,NO,SVCINV) CROSS MEMORY MODE? */             ZP60013
         SPACE 2
         TM    FLCSOPSW+2,X'C0'        PRIMARY ADDRESSING MODE? ZP60013
         BNZ   SVCINV                  NO GO CALL RTM           ZP60013
         SPACE 2
*/* SVCPROC: P GET CVTTRACE FROM CVT INTO REG 11 */
         SPACE 2
SVCPROC  L     R13,FLCCVT              GET CVT ADDRESS
         USING CVT,R13                 ESTABLISH BASE TO CVT
         SPACE 2
*/* D (NO,GTFHOOK,YES,) SHOULD TRACE BE ENTERED? */
         SPACE 2
         CLI   CVTTRACE+C1,CXFA        IS TRACE TO BE ENTERED
         BNE   GTFHOOK                 IF NOT DONT TRACE
         SPACE 2
*/* P GET TRSVC ENTRY POINT TO TRACE IN REG 10 */
         SPACE 2
         L     R10,ATRSVC              GET SVC TRACE ENTRY ADDRESS
         SPACE 2
*/*TRACEEP: L TRACE-- VIA BALR 11,10 */
         SPACE 2
TRACEEP  DC    0H'0'                   TEST LABEL
         BALR  R11,R10                 GO TO TRACE
         DROP  R13
         SPACE 2
*/*GTFHOOK: L HOOK--  ISSUE HOOK EID=IEASVCH TYPE=P */
         SPACE 2
GTFHOOK  DC    0H'0'                   TEST LABEL
         HOOK  EID=IEASVCH,TYPE=P      GTF TRACE SVC INTERRUPT
         SPACE 2
*/* P GET TCB ADDR FROM PSATOLD IN REG 4 */
         SPACE 2
         L     R4,PSATOLD              GET CURRENT TCB ADDRESS
         USING TCB,R4                  ESTABLISH BASE TO TCB
         SPACE 2
*/* P GET RB ADDR FROM TCBRBP IN REG 5 */
         SPACE 2
         L     R5,TCBRBP               GET CURRENT RB ADDRESS
         SPACE 2
*/* P SAVE SVCOPSW IN RBOPSW */
         SPACE 2
         USING RBSECT,R5               ESTABLISH BASE TO RB
         MVC   RBOPSW(L8),FLCSOPSW     SAVE PSW IN RB
         LR    R8,R5                   TRANSFER RB ADDRESS
         S     R8,KRBPRFXL             GET PTR TO RB PREFIX
         DROP  R5
         USING RBPRFXST,R8             ESTABLISH BASE TO RB PREFIX
         SPACE 2
*/* P SAVE SVCILC IN RBINLNTH AND SVCNUM IN RBINTCOD */
         SPACE 2
         IC    R6,FLCSVILC             GET SVC INSTRUCTION LENGTH
         STC   R6,RBINLNTH             STORE IN THE RB
         LH    R6,FLCSVCN              GET THE SVC NUMBER
         STH   R6,RBINTCOD             STORE IN THE RB PREFIX
         DROP  R8
         SPACE 2
*/* D (YES,SVCSCREN,NO,) IS SVC SCREENING ACTIVE? */
         SPACE 2
         TM    TCBFLGS7,TCBSVCS   CHECK FOR SVC SCREENING      @Z40EPVC
         BO    SVCSCREN           ACTIVE -- DO SCREENING       @Z40EPVC
         SPACE 2
*/*SVCTENTY: P GET SVCTABLE ENTRY FROM SVC TABLE */            @Z40FPYK
         SPACE 2
SVCTENTY DS    0H                                              @Z40FPYK
         SPACE 2
*/*          P INCREMENT EVENT COUNT FOR THIS SVC NUMBER */     ZP60013
         LR    R10,R6                  COPY SVC NUMBER          ZP60013
         SLL   R10,C2                  MULTIPLY SVC NUMBER BY 4 ZP60013
         LA    R10,SVCCTTBL(R10)       POINT TO SVC COUNTER     ZP60013
         L     R14,0(,R10)             GET CURRENT COUNT        ZP60013
SVCCTLP  LA    R8,C1                   GET INCREMENT SIZE       ZP60013
         AR    R8,R14                  GET NEW SVC EVENT COUNT  ZP60013
         CS    R14,R8,0(R10)           SAVE THE NEW COUNT       ZP60013
         BNZ   SVCCTLP                 RETRY IF NECESSARY       ZP60013
         SPACE 2
         SLL   R6,C3                   MULTIPLY SVC NUMBER BY 8
         A     R6,SVCTAB               GET ADDRESS OF SVC ENTRY
         USING SVCENTRY,R6             ESTABLISH BASE TO SVC ENTRY
         SPACE 2
*/* D (YES,SVCEXT,NO,) ESR APF OR N/P BIT ? */                 @Z40FPYK
         SPACE 2
         TM    SVCATTR1,SVCAPF+SVCESR+SVCNP CHECK FOR ESR,APF, @Z40FPYK
*                                      OR N/P BIT ON IN ENTRY  @Z40FPYK
         BNZ   SVCEXT                  IF NOT ON CONTINUE PROCESSING
         SPACE 2
*/*SVCOK: P GET ASCB ADDR FROM PSAAOLD */
         SPACE 2
SVCOK    LR    R10,R7                  SAVE LCCA ADDRESS
         L     R7,PSAAOLD              GET CURRENT ASCB ADDRESS
         USING ASCB,R7                 ESTABLISH BASE TO ASCB
         USING LCCA,R10
         SPACE 2
*/*TYPTST: D (YES,,NO,SVCCELL) IS SVC A TYPE 1 ? */            @Z40FPYK
         SPACE 2
TYPTST   DC    0H'0'                   TEST LABEL
         TM    SVCATTR1,CXF0-SVCTP1    IS SVC TYPE 1
         BNZ   SVCCELL                 IF NOT, PROCESS TYPE 2, @Z40FPYJ
*                                      3, 4, OR 6
         SPACE 2                                               @Z40FPYK
         EJECT                                                 @Z40FPYK
************************************************************** @Z40FPYK
*                                                            * @Z40FPYK
*        ROUTINE TO PROCESS TYPE 1 SVC                      *  @Z40FPYJ
*                                                           *  @Z40FPYK
*************************************************************  @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*SVC1: S GETRTN:INTERNAL ROUTINE TO GET LOCK */
         SPACE 2
SVC1     MVC   TCBGRS(L64),LCCASGPR    MOVE THE GENERAL REGS TO THE TCB
*                                                              @YA02598
         L     R13,LLCO                GET ENTRY POINT TO LOCK MANAGER
         BALR  R14,R13                 OBTAIN LOCAL LOCK CONDITIONALLY
         LTR   R13,R13                 CHECK IF LOCK OBTAINED OK
         BNZ   BACKUP                  IF NOT BACK UP INSTRUCTION
         SPACE 2
*/* P SET TYPE1 SWITCH */
         SPACE 2
         OI    ASCBFLG1,ASCBTYP1       TURN ON TYPE 1 SVC FLAG
         LA    R1,TCBGRS               POINT REG1 TO SVC ISSUER REGS
         SPACE 2
*/* P CLEAR SVC FLIH BIT */
         SPACE 2
         NI    PSASUP1,CXFF-PSASVC     CLEAR SVC FLIH BIT
         SPACE 2
*/* P MAKE FRR NORMAL STACK CURRENT */
         SPACE 2
         L     R8,PSANSTK              GET NORMAL STACK PTR
         ST    R8,PSACSTK              MAKE NORMAL CURRENT PTR
         SPACE 2
*/*ENABLE1: P ENABLE PSW FOR I/O & EXT INT */
         SPACE 2
ENABLE1  DC    0H'0'                   TEST LABEL
         STOSM LCCAPSMK,ENABLE         ENABLE THE PSW
         SPACE 2
*/*INSLOCK: D (NO,SVCENTPT,YES,CMSCK) ANY MORE LOCKS/ */
         SPACE 2
INSLOCK  DC    0H'0'                   TEST LABEL
         LH    R8,SVCLOCKS             GET LOCK INDICATORS FROM SVC
         SLL   R8,C16                  TABLE INTO HIGH ORDER 2 BYTES
         ALR   R8,R8                   FORCE LOCAL LOCK BIT OUT
         BC    M10,SVCENTPT            IF 0 LEFT THEN NO MORE LOCKS
         B     CMSCK                   OTHERWISE CHECK FOR MORE LOCKS
         EJECT                                                 @Z40FPYK
         CNOP  0,8                                             @Z40FPYK
*/*SVCCELL: N GET AN SVRB FROM THE POOL OF SVRBS */
         SPACE 2
SVCCELL  DS    0H                      GET SVRB FROM POOL      @Z40FPYJ
         SPACE 2
*/* D (YES,TYPE6SVC,NO,) TYPE 6 SVC ? */                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
         TM    SVCATTR1,SVCTP6         TYPE 6 SVC ?            @Z40FPYK
         BO    TYPE6SVC                YES, PROCESS NO SVRB    @Z40FPYK
*                                      IS NECESSARY            @Z40FPYK
         SPACE 2                                               @Z40FPYK
************************************************************** @Z40FPYK
*                                                            * @Z40FPYK
*        ROUTINE FOR TYPE 2,3, AND 4 SVC                     * @Z40FPYK
*        GET AND INITIALIZE AN SVRB.                         * @Z40FPYK
*                                                            * @Z40FPYK
************************************************************** @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P GET FIRST IN POOL ADDRESS AND SYNC COUNT */
         SPACE 2
         L     R2,ASCBSVRB             ADDRESS OF 1ST AVAIL    @Z40FPYJ
         L     R3,ASCBSYNC             ADDRESS OF SYNC FIELD   @Z40FPYJ
         SPACE 2
*/*CKSVRB: D (YES,,NO,GETCORE) SVRB AVAILABLE ? */             @Z40FPYK
         SPACE 2
CKSVRB   LTR   R2,R2                   ANY AVAILABLE?          @Z40FPYJ
         BZ    GETCORE                 NO, EXPAND POOL         @Z40FPYJ
         SPACE 2
*/* P INCREMENT SYNC COUNT */
         SPACE 2
         LA    R15,1(R3)               BUMP COUNT BY ONE       @Z40FPYJ
         SPACE 2
*/* P GET NEXT MAIN CONTROL BLOCK POINTER */
         SPACE 2
         USING MBCB,R2                                         @Z40FPYJ
         L     R14,MBCBLINK            NEXT BLOCK ON CHAIN     @Z40FPYJ
         SPACE 2
*/* P SWAP ADDRESS AND SYNC FIELDS */
         SPACE 2
         CDS   R2,R14,ASCBSUPC         SWAP ADDR AND SYNC      @Z40FPYJ
         BNE   CKSVRB                  RETRY IF COMPARE FAILS  @Z40FPYJ
         SPACE 2
*/*SVRBINIT: P ZERO OUT THE SVRB CELL */                       @Z40FPYK
         SPACE 2
SVRBINIT XC    MBCBPXLN(RBSCBB-RBPRFXST,R2),MBCBPXLN(R2)       @Z40FPYK
*                                      ZERO THE WHOLE  SVRB AREA
*                                      EXCEPT THE SCB. THE DISPL OF
*                                      8 SKIPS THE ID AND LINK FIELDS
*                                      OF THE MAIN CONTROL BLOCK.
         SPACE 2
*/*SVRBINT2: P PLACE PREVIOUS RB ADDR IN RBLINK */             @ZA29595
         SPACE 2                                               @Z40FPYK
SVRBINT2 EQU   *                                               @ZA29595
         USING RBPRFXST-MBCBPXLN,R2                            @Z40FPYK
         ST    R5,RBLINK               CHAIN SVRB TO PREVIOUS  @Z40FPYK
*                                      RB ON TCB CHAIN         @Z40FPYK
         DROP  R2                                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SET UP TO THE MAIN BODY OF SVRB */                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
         LA    R5,RBPRFLNA+MBCBPXLN(R2) ALIGN TO MAIN SVRB     @Z40FPYK
         USING RBSECT,R5                                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P MOVE TCBGRS TO RBGRSAVC */                               @Z40FPYK
         SPACE 2                                               @Z40FPYK
         MVC   RBGRSAVE(L64),LCCASGPR  SAVE TCBREGS IN SVRB SAVE AREA
*                                                              @YA02598
         SPACE 2
*/* P PUT SVRB SIZE IN RBSIZE */
         SPACE 2
*/* P SET SVRB BITS: RBFDYN RBATTN RBFTSVRB */
         SPACE 2
*                                                              @Z40FPYK
*        THE RB SIZE, TYPE, AND FLAGS ARE SET BY THE           @Z40FPYK
*        FOLLOWING MASK IN THE SVRB:                           @Z40FPYK
*        RBTRSVRB + RBFDYN +RBSIZE                             @Z40FPYK
*                                                              @Z40FPYK
         L     R1,SVRBMASK             GET MASK                @Z40FPYJ
         ST    R1,RBSIZE               SET MASK IN RB          @Z40FPYJ
         SPACE 2
*/* P PUT LEFT HALF SVCNPSW IN LEFT HALF RBOPSW (ENABLED) */
         SPACE 2
         L     R8,ALHSNPSW             PUT LEFT HALF OF SVC NEW PSW
         ST    R8,RBOPSW               INTO LEFT HALF OF RB OLD PSW
         IC    R8,ASCBSRBM             PUT MODEL PSW BYTE 0
         STC   R8,RBOPSW               INTO BYTE 0 OF RB OLD PSW
         SPACE 2
*/*TYP34TST: D (NO,TYP2,YES,) SVC TYPE 3 OR 4? */
         SPACE 2
TYP34TST DC    0H'0'                   TEST LABEL
         TM    SVCATTR1,SVCTP34        IS SVC A TYPE3 OR 4
         BNO   TYP2                    IF NOT,THEN TYPE2
         SPACE 2
*/*TYP34: P SET SVRB BIT */
         SPACE 2
TYP34    DC    0H'0'                   TEST LABEL
         OI    RBSTAB1,RBTRSVRB        IF TYPE 3 OR 4 INDICATE AS SUCH
         SPACE 2
*/*TYP2: P PLACE SVRB ADDR IN TCBRBP */
         SPACE 2
TYP2     DS    0H
         ST    R5,TCBRBP               CHAIN TCB TO SVRB       @Z40FPYK
         SPACE 2
*/* P SUPPRESS ATTENTIONS SET TCBATT */
         SPACE 2
         OI    TCBTSFLG,TCBATT         SUPPRESS ATTENTIONS IN TCB
         SPACE 2
*/* P REG 1= ADDR REGISTER SAVE AREA IN RB */                  @Z40FPYK
         SPACE 2
         LA    R1,RBGRSAVE             POINT REG1 TO SVC ISSUER REGS
         SPACE 2
*/*SVRBTST: P CLEAR SVC FLIH BIT */
         SPACE 2
SVRBSTST DC    0H'0'                   TEST LABEL
         NI    PSASUP1,CXFF-PSASVC     CLEAR SVC FLIH BIT
         SPACE 2
*/* P MAKE FRR NORMAL STACK CURRENT */
         SPACE 2
         L     R8,PSANSTK              GET NORMAL STACK PTR
         ST    R8,PSACSTK              MAKE NORMAL CURRENT PTR
*                                      FROM HERE ON SVC FLIH ACTS AS
*                                      PART OF SVC ROUTINE
         SPACE 2
*/*ENABLE2: P ENABLE PSW FOR I/O & EXT INT */
         SPACE 2
ENABLE2  DC    0H'0'                   TEST LABEL
         STOSM LCCAPSMK,ENABLE         ENABLE THE PSW FOR I/O AND
*                                      EXTERNAL INTERRUPTS
         SPACE 2
*/*INSLOCK2: D (NO,CMSCK,YES,) LOCAL LOCK NEEDED? */
         SPACE 2
INSLOCK2 DC    0H'0'                   TEST LABEL
         LH    R8,SVCLOCKS             PUT LOCK BYTES OF SVC TABLE INTO
         SLL   R8,C16                  HIGH ORDER 2 BYTES OF REGISTER
         ALR   R8,R8                   DOUBLE VALUE,FORCING HIGH ORDER
*                                      BIT OUT CAUSING CONDITION CODE
         BC    M8,SVCENTPT             IF NO CARRY AND 0 IS LEFT, NO
*                                      LOCKS NEEDED
         BC    M4,CMSCK                IF NO CARRY AND NOT 0,
*                                      CHECK NEXT LOCK
         SPACE 2
*/*LOCLOCK: L SETLOCK-- OBTAIN LOCAL UNCONDITIONAL */
         SPACE 2
LOCLOCK  DS    0H
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,RELATED=('OBTAINED ON BE*
               HALF OF SVC ROUTINE','MAY BE GIVEN UP BY SVC ROUTINE OR *
               BY EXIT PROLOGUE',IEAVEEXP(RELLOCAL)) GET LOCAL LOCK
LOCLCK   DC    0H'0'                   TEST LABEL
         SPACE 2
*/*CMSCK: D (NO,SRMCK,YES,) CMS LOCK NEEDED? */
         SPACE 2
CMSCK    ALR   R8,R8                   CHECK NEXT LOCK BIT
         BC    M8,SVCENTPT             IF NOCARRY AND 0,NO MORE LOCKS
         BC    M4,SRMCK                IF NOCARRY AND NOT 0, CHECK NEXT
         SPACE 2
*/*CMSLK: L SETLOCK-- OBTAIN CMS UNCONDITIONAL */
         SPACE 2
CMSLK    SETLOCK OBTAIN,TYPE=CMS,MODE=UNCOND,RELATED=('OBTAINED ON BEHA*
               LF OF SVC ROUTINE','MAY BE GIVEN UP BY SVC ROUTINE OR BY*
               EXIT PROLOGUE',IEAVEEXP(RELCMS)) GET CMS LOCK
CMSLKTST DC    0H'0'                   TEST LABEL
         SPACE 2
*/*SRMCK: D (NO,SALCK,YES,) SRM LOCK NEEDED? */
         SPACE 2
SRMCK    ALR   R8,R8                   CHECK NEXT LOCK BIT
         BC    M8,SVCENTPT             IF NOCARRY AND 0,NO MORE LOCKS
         BC    M4,SALCK                IF NOCARRY AND NOT 0,CHECK NEXT
         SPACE 2
*/*SRMLK: L SETLOCK--  OBTAIN SRM UNCONDITIONAL */
         SPACE 2
SRMLK    SETLOCK OBTAIN,TYPE=SRM,MODE=UNCOND,RELATED=('OBTAINED ON BEHA*
               LF OF SVC ROUTINE','MAY BE GIVEN UP BY SVC ROUTINE OR BY*
               EXIT PROLOGUE',IEAVEEXP(LKDISTST,NOTDIS)) GET SRM LOCK
SRMLKTST DC    0H'0'                   TEST LABEL
         SPACE 2
*/*SALCK: D (NO,DISPPCK,YES,SALLCLK) SALLOC LOCK NEEDED? */
         SPACE 2
SALCK    ALR   R8,R8                   CHECK NEXT LOCK BIT
         BC    M8,SVCENTPT             IF NOCARRY AND 0,NO MORE LOCKS
         BC    M4,DISPPCK              IF NOCARRY AND NOT 0,CHECK NEXT
         SPACE 2
*/*SALLCLK: L SETLOCK-- OBTAIN SALLOC UNCOND */
         SPACE 2
SALLCLK  SETLOCK OBTAIN,TYPE=SALLOC,MODE=UNCOND,RELATED=('OBTAINED ON B*
               EHALF OF SVC ROUTINE','MAY BE GIVEN UP BY SVC ROUTINE OR*
               BY EXIT PROLOGUE',IEAVEEXP(LKDISTST,NOTDIS)) GET SALLOC *
               LOCK
SALCKTST DC    0H'0'                   TEST LABEL
         SPACE 2
*/*DISPPCK: D (NO,SVCENTPT,YES,DISPLK) DISP LOCK NEEDED? */
         SPACE 2
DISPPCK  ALR   R8,R8                   CHECK NEXT LOCK BIT
         BC    M12,SVCENTPT            IF NOCARRY,NO MORE LOCKS
         SPACE 2
*/*DISPLK: L SETLOCK-- OBTAIN DISP UNCONDITIONAL */
         SPACE 2
*                                      GET DISPATCHER LOCK
DISPLK   SETLOCK OBTAIN,TYPE=DISP,MODE=UNCOND,RELATED=('OBTAINED ON BEH*
               ALF OF SVC ROUTINE','MAY BE GIVEN UP BY SVC ROUTINE OR B*
               Y EXIT PROLOGUE',IEAVEEXP(LKDISTST,NOTDIS))
DISPLTST DC    0H'0'                   TEST LABEL
         EJECT                                                 @Z40FPYK
*/*SVCENTPT: P R3=PTR(CVT) R4=PTR(TCB) R5=PTR(RB) R7=PTR(ASCB) */
         SPACE 2
         CNOP  0,8                     OPTIMAL BRANCH ALIGNMENT
SVCENTPT DS    0H
         L     R3,FLCCVT               GET CVT POINTER IN REG3
         USING CVT,R3
         SPACE 2
*/* P R0,R1,R13,R15 AS ON ENTRY */
         SPACE 2
         L     R13,C52(R1)             GET REG 13 AS UPON ENTRY
         L     R15,C60(R1)             GET REG 15 AS UPON ENTRY
         L     R0,C0(R1)               SET REG 0 AS UPON ENTRY
         L     R1,C4(R1)               SET REG 1 AS UPON ENTRY
         L     R6,C0(,R6)              GET SVC ENTRY POINT ADDRESS
         SPACE 2
*/* P R14= PTR(IEAVEXPR) RETURN ADDR =EXIT PROLOGUE */
         SPACE 2
         L     R14,CVTEXPRO            SET UP RETURN ADDRESS
         SPACE 2
*/*SVCEPTST: R BRANCH TO SVC EP */
         SPACE 2
SVCEPTST DC    0H'0'                   TEST LABEL
         BR    R6                      GO TO SVC ROUTINE
         TITLE 'IEAVESVC- SVCEXT: SPECIAL SVC PROCESSING'
         SPACE 2
*/*SVCEXT: D (NO,APFTST,YES,) ESR? */
         SPACE 2
SVCEXT   TM    SVCATTR1,SVCESR         CHECK FOR ESR BIT
         BZ    APFTST                  NOT ON -- CHECK APF BIT
         SPACE 2
*/* P MPLY ESR CODE BY 8 (= CALLER'S REG 15) */
         SPACE 2
         SLL   R15,C3                  MULT ESR CODE BY 8
         SPACE 2
*/* P GET CORRECT ENTRY IN ESR TABLE FOR INVOKED EXSVC */
         SPACE 2
         L     R6,C0(,R6)              GET START THIS SECTION ESRTABLE
         AR    R6,R15                  GET CORRECT OFFSET IN TABLE
         SPACE 2
*/* D (YES,APFTST,NO,) CHECK THAT ENTRY IS IN ESR TABLE */
         SPACE 2
         CL    R6,ESRTBBGN             CHK ENTRY IS NOT BEFORE @ZA12726
*                                      BEGINNING OF TABLE      @ZA12726
         BL    INVESR                  ESR CODE IS INVALID     @ZA12726
         CL    R6,ESRTBEND             CHK ENTRY IS BEFORE END @ZA09430
*                                      OF TABLE                @ZA12726
*                                      (C INST CHNGED TO CL)   @ZA09430
         BL    APFTST                  ENTRY IS OK -- CONTINUE
         SPACE 2
*/* P (,ABTERMR) LOAD REG 1 WITH ABEND CODE = X'16D' */
         SPACE 2
INVESR   EQU   *                                               @ZA12726
         LA    R1,COMPCDE4             INVALID ESRCODE -- ABEND
         B     ABTERMR                 GO TO ABTERM WITH 16D CODE
         SPACE 2
*/*APFTST: D (NO,NPTST,YES,) IS SVC AUTHORIZED? */             @Z40FPYK
         SPACE 2
APFTST   DC    0H'0'                   TEST LABEL
         TM    SVCATTR1,SVCAPF         IS SVC AUTHORIZED
         BZ    NPTST                   IF NOT AUTHORIZED,CONTINUE
*                                      OTHERWISE CHECK AUTHORIZATION
         SPACE 2
*/*TESTAUTH: L TESTAUTH-- FCTN=1 STATE=YES KEY=YES */
         SPACE 2
TESTAUTH DC    0H'0'                   TEST LABEL
         TESTAUTH FCTN=1,STATE=YES,KEY=YES,RBLEVEL=1,BRANCH=YES        X
                                       GO TO CHECK AUTHORIZATION
         SPACE 2
*/*AUTHTST: D (YES,NPTST,NO,) AUTHORIZED? */                   @Z40FPYK
         SPACE 2
AUTHTST  DC    0H'0'                   TEST LABEL
         LTR   R15,R15                 IS RETURN CODE ZERO
         BZ    NPTST                   IF SO CONTINUE PROCESS  @Z40FPYK
         SPACE 2
*/* P SET COMPLETION CODE = X'047' */
         SPACE 2
         LA    R1,COMPCD2              GET COMPLETION CODE
         SPACE 2
*/*ABTERMR: P MOVE THE GENERAL REGISTERS FROM THE LCCA INTO THE TCB */
         SPACE 2
         DROP  R10                                             @Z40FPYK
         USING LCCA,R7                                         @Z40FPYK
ABTERMR  MVC   TCBGRS(L64),LCCASGPR    MOVE THE REGS INTO THE TCB
*                                                              @YA02598
         DROP  R7                                              @Z40FPYK
         USING LCCA,R10                                        @Z40FPYK
         USING ASCB,R7                                         @Z40FPYK
         SPACE 2
*/*ABTERMRT: L CALLRTM-- TYPE= ABTERM */
         SPACE 2
ABTERMRT CALLRTM TYPE=ABTERM,TCB=0,COMPCOD=(1) GO TO RTM
         SPACE 2
*/*EX1: P CLEAR SVC FLIH BIT */
         SPACE 2
EX1      DC    0H'0'                   TEST LABEL
         L     R6,AIEAPDS7             GET DISPATCHER ADDRESS
         NI    PSASUP1,CXFF-PSASVC     CLEAR SVC FLIH BIT
         SPACE 2
*/*FIN1: R EXIT TO DISPATCHER */
         SPACE 2
FIN1     DC    0H'0'                   TEST LABEL
         BR    R6                      EXIT TO DISPATCHER
         SPACE 2                                               @Z40FPYK
*/*NPTST: D (YES,,NO,SVCOK) N/P SVC ? */                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
NPTST    DS    0H                                              @Z40FPYK
         TM    SVCATTR1,SVCNP          NON-PREEMPTIVE SVC ?    @Z40FPYK
         BNO   SVCOK                   NO--CONTINUE            @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P (,SVCOK) SET TASK NON PREEMPTIVE */                      @Z40FPYK
         SPACE 2                                               @Z40FPYK
         OI    TCBFLGS1,TCBNONPR       SET TASK N/P            @Z40FPYK
         B     SVCOK                   NORMAL SVC PROCESSING   @Z40FPYK
         TITLE 'IEAVESVC- GETCORE: GET SVRB POOL'              @Z40FPYJ
         SPACE 2
*/*GETCORE: P MOVE THE GENERAL REGISTERS INTO THE TCB */
         SPACE 2
GETCORE  MVC   TCBGRS(L64),LCCASGPR    MOVE THE REGISTERS INTO THE TCB
*                                                              @YA02598
         SPACE 2
*/* S GETRTN:INTERNAL ROUTINE TO GET LOCK */
         SPACE 2
         BAL   R2,GETRTN               GO INTERNAL SUBRTN TO GETLOCK
         SPACE 2
*/* P SET UP REGS FOR GETMAIN */
         SPACE 2
         LA    R0,MBCBLEN*SVRBNUM      LOAD AMOUNT OF CORE NEEDED
               SPACE 2
*/* D (YES,,NO,POOLGET) ABEND IN PROCESS? */
         SPACE 2
         TM    TCBFLGS3,TCBABGM        IF OUT OF CORE IS ABEND IN
*                                      PROCESS
         BNO   POOLGET                 IF SO DO GETMAIN FOR SINGLE SVRB
         SPACE 2
*/*ABSVRB: P USE SVRBLEN TO INDICATE AMNT OF CORE */
         SPACE 2
ABSVRB   LA    R0,SVRBLEN CHANGE       AMOUNT OF CORE          @ZA29595
         SPACE 2
*/*POOLGET: L GETMAIN-- GET CORE FOR EXPANDING SVRB POOL */
               SPACE 2
POOLGET  DC    0H'0'                   TEST LABEL
         GETMAIN RC,SP=255,LV=(0),BRANCH=YES GO GET CORE
         SPACE 2
*/*POOLGOT:  L SETLOCK-- RELEASE TYPE=LOCAL */
         SPACE 2
POOLGOT  DC    0H'0'                   TEST LABEL
         SETLOCK RELEASE,TYPE=LOCAL,                                   *
               RELATED=('GETMAIN COMPLETE',IEAVESVC(GETRTN)) CLEAR     *
               LOCAL LOCK
         SPACE 2
*/*POOLGTST: D (NO,GOTCORE,YES,) OUT OF CORE? */
         SPACE 2
POOLGTST DC    0H'0'                   TEST LABEL
         LTR   R15,R15                 WAS CORE GOTTEN
         BZ    GOTCORE                 IF SO GO SET UP
               SPACE 2
*/* D (YES,TERMMEM,NO,) ABEND IN PROCESS? */
         SPACE 2
         TM    TCBFLGS3,TCBABGM        IF OUT OF CORE IS ABEND IN
*                                      PROCESS
         BO    TERMMEM                 IF SO GO TO MEMTERM     @Z40FPYJ
         SPACE 2
*/* P (,ABTERMRT) SET COMPLETION CODE= X'0F9' */               @Z40FPYK
         SPACE 2
         SR    R1,R1                   SET UP R15 = 0          @Z40FPYK
         ST    R1,TCBGRS15             COMPLETION CODE IN TCB  @Z40FPYK
         LA    R1,COMPCD3              GET COMPLETION CODE
         B     ABTERMRT                GO TO ABTERM ISSUER
         SPACE 2
         TITLE 'IEAVESVC- SVCINV: INVOKE RTM'
         SPACE 2
*/*SVCINV: P LOAD COMPLETION CODE */
         SPACE 2
SVCINV   TM    PSASUP1,PSATYPE6        TYPE 6 SVC IN CONTROL?
         BO    SVCINSTK                YES
         NI    PSASUP1,CXFF-PSASVC     NO, NO LONGER IN SVC FLIH
SVCINSTK MVC   PSACSTK,PSASSAV         RESTORE FRR STACK
         LA    R1,COMPCD1              GET COMPLETION CODE
         SPACE 2
*/*SVCERR: R EXIT TO RTM */
         SPACE 2
SVCERR   DC    0H'0'                   TEST LABEL
*                                      GO TO RTM FOR ERROR RECOVERY
         CALLRTM TYPE=SVCERR,COMPCOD=(1)
         B     SVCPROC                 RETURN TO MAINLINE      @Z40FPYJ
         TITLE 'IEAVESVC- IGCERROR:SVC ERROR ROUTINE '
***********************************************************************
*                                                                     *
*                     SVC ERROR ROUTINE                               *
*                                                                     *
***********************************************************************
         ENTRY IGCERROR
         SPACE 2
*/*IGCERROR: E IGCERROR */
         SPACE 2
IGCERROR DS    0H
         USING RBSECT,R5               ESTABLISH BASE TO SVRB
         L     R2,RBLINK               GET ISSUING RB ADDRESS
         S     R2,KRBPRFXL             GET PTR TO RB PREFIX
         SPACE 2
*/* P GET SVC NUMBER FROM PREVIOUS RB */
         SPACE 2
         DROP  R5
         USING RBPRFXST,R2             ESTABLISH BASE TO RB PREFIX
         LH    R1,RBINTCOD             GET SVC NUMBER ISSUED
         SPACE 2
*/* P (,ABTST) SET COMPLETION CODE =X'F(SVCNUM)' */
         SPACE 2
         LA    R1,X'F00'(,R1)          PUT F IN FRONT OF SVC NUMBER
         B     ABTST               BRANCH AROUND IGXERROR ENTRY
         ENTRY IGXERROR            ENTRY FOR INVALID EXSVC NUMBER
         SPACE 2
*/*IGXERROR: E IGXERROR */
         SPACE 2
IGXERROR DS    0H
         LA    R1,COMPCDE4         UNSUPPORTED ESRCODE -- ABEND
         SPACE 2
*/*ABTST: R EXIT VIA SVC 13 */
         SPACE 2
ABTST    DC    0H'0'                   TEST LABEL
         ABEND (1),DUMP,,SYSTEM        ABEND ISSUER OF SVC
         TITLE 'IEAVESVC- GETRTN:INTERNAL ROUTINE TO OBTAIN LOCAL LOCK'
         SPACE 2
*/*GETRTN: E GETRTN */
*/* L SETLOCK-- OBTAIN LOCAL COND */
         SPACE 2
GETRTN   SETLOCK OBTAIN,TYPE=LOCAL,MODE=COND,RELATED=('SVC INTERRUPT PR*
               OCESS FOR GETMAIN OR TYPE 1 SVC',IEAVESVC(POOLFAIL,RELLL*
               2,RELLL3,RELLL4),IEAVEEXP(RELLOCAL)) GET LOCAL LOCK
         SPACE 2
*/*LLTST1: D (NO,BACKUP,YES,) GOTTEN? */
         SPACE 2
LLTST1   DC    0H'0'                   TEST LABEL
         LTR   R13,R13                 WAS REQUEST SUCCESSFUL
         SPACE 2
*/* R RETURN */
         SPACE 2
         BCR   M8,R2                   IF SO RETURN
*                                      NOT GOTTEN BACK UP PSW
         SPACE 2
*/*BACKUP: P BACK UP INSTR ADDR OF SVCOPSW BY SVCILC */
         SPACE 2
BACKUP   DS    0H
         USING RBSECT,R5
         L     R8,FLCSOPSW+C4          GET NEXT INSTRUCTION FROM PSW
         SR    R14,R14                 CLEAR WORK REGISTER
         IC    R14,FLCSVILC            GET INSTRUCTION LENGTH
         SR    R8,R14                  BACK UP PSW BY INSTRUCTION LTH
         SPACE 2
*/* P STORE VALUE IN RBOPSW */
         SPACE 2
         ST    R8,RBOPSW+C4            PUT BACKED UP PSW IN RB OLD PSW
         SPACE 2
*/* P CLEAR SVC FLIH BIT */
         SPACE 2
         L     R6,AIEAPDS7             GET DISPATCHER ADDRESS
         NI    PSASUP1,CXFF-PSASVC     CLEAR SVC FLIH BIT
         SPACE 2
*/*BACKXIT: R EXIT TO DISPATCHER */
         SPACE 2
BACKXIT  DC    0H'0'                   TEST LABEL
         BR    R6                      GO TO DISPATCHER
         TITLE 'IEAVESVC- SVC SCREENING ROUTINE '
SVCSCREN DS    0H                 SVC SCREENING PROCESSING     @Z40EPVC
         SPACE 2
*/*SVCSCREN: P GET ADDRESS SSSCREEN TABLE */
         SPACE 2
         L     R11,TCBSVCA2       GET ADDRESS SS SCREEN TABLE  @Z40EPVC
         USING SSTAB,R11
         SPACE 2
*/* D (YES,SVCTENTY,NO,) OK TO ISSUE THIS SVC? */
         SPACE 2
         LA    R14,SSTMASK-SSTAB(R6,R11)
*                                 GET ADDRESS OF SCREEN FLAG   @Z40EPVC
         TM    0(R14),SSTSVCOK
*                                 CHECK IF OK TO ISSUE         @Z40EPVC
         BO    SVCTENTY           OK -- CONTINUE PROCESSING    @Z40EPVC
         SPACE 2
*/* P (,SVCOK) CHANGE TO SS SVC AND PROCESS */
         SPACE 2
         LR    R6,R11             NOT OK -- CHANGE TO SS SVC   @Z40EPVC
         B     SVCOK              PROCESS SS SVC REQUEST       @Z40EPVC
         TITLE 'IEAVESVC- FORMAT POOL OF SVRBS'
         SPACE 2
*/*GOTCORE: N FORMAT POOL OF SVRBS */
         SPACE 2
GOTCORE  DS    0H
         SPACE 2
*/* D (1,SETT01,9,) HOW MANY SVRBS */
         SPACE 2
         TM    TCBFLGS3,TCBABGM        1 SVRB FOR G/M ABEND    @Z40FPYJ
         BO    SETTO1                  YES, PROCESS ONLY ONE   @Z40FPYJ
         SPACE 2
*/* P SET COUNT TO 9 SVRBS IN POOL    */
         SPACE 2
         LA    R15,SVRBNUM             9 SVRBS IN  POOL        @Z40FPYJ
         SPACE 2
*/* P INIT ID AND MAIN CONTROL BLOCK PTR */
         SPACE 2
         L     R0,IDSVRB               PLACE 'SVRB' IN REG     @Z40FPYJ
         LR    R14,R1                  COPY ADDR OF 1ST MBCB   @Z40FPYJ
         USING MBCB,R14
         SPACE 2
*/*CALCNXT: P CALC ADDR OF NEXT MBCB */
         SPACE 2
CALCNXT  LA    R13,MBCBLEN(R14)        CALCULATE THE NEXT      @Z40FPYJ
*                                      MBCB ADDRESS            @Z40FPYJ
         SPACE 2
*/* P SET UP ID IN THIS MBCB   */
         SPACE 2
         ST    R0,MBCBID               PLACE ID IN MBCBID      @ZA29595
         SPACE 2
*/* P SET UP LINK ADDR */
         SPACE 2
         ST    R13,MBCBLINK            PLACE @  IN MBCB        @Z40FPYJ
         SPACE 2
*/*SKIP1: P BUMP DOWN TO ADDRESS SVRB */
         SPACE 2
SKIP1    EQU   *                                               @ZA29595
         LA    R11,RBPRFLNA+MBCBPXLN(R14) ADDRESS THE MAIN     @Z40FPYJ
*                                         SVRB                 @Z40FPYJ
         USING RBSECT,R11                                      @Z40FPYJ
         SPACE 2                                               @Z40FPYJ
*/* P INITIALIZE THE SCB */                                    @Z40FPYJ
         SPACE 2                                               @Z40FPYJ
         XC    RBSCBB(SVRBEND-RBSCBB),RBSCBB  CLEAR SCB AND    @ZA13392
*                                      PARAMETER AREA          @ZA13392
         ST    R11,RBSOWNR             @RB INTO SCB            @Z40FPYJ
         MVI   RBSPKEY,C0              KEY 0 INTO SCB          @Z40FPYJ
         MVI   RBSID,SCBID             ID  INTO SCB            @Z40FPYJ
         SPACE 2
*/* P MAKE NEXT THE CURRENT MBCB */
         SPACE 2
         LR    R14,R13                 COPY NEXT INTO CURRENT  @Z40FPYJ
         SPACE 2
*/* D (NO,CALCNXT,YES,) COUNT TO 0 */
         SPACE 2
         BCT   R15,CALCNXT             LOOP TO PROCESS ALL RBS @Z40FPYJ
         SPACE 2
*/* D (YES,SVRBINT2,NO,) SINGLE SVRB DONE ? */                 @ZA29595
         SPACE 2
         LR    R2,R1                   COPY ADDR OF 1ST BLOCK  @Z40FPYJ
         LTR   R13,R13                 NEXT IS 0, IF SINGLE RB @Z40FPYJ
         BZ    SVRBINT2                GO TO MAINLINE BUT DO   @ZA29595
*                                      NOT CLEAR RBNOCELL      @ZA29595
         SPACE 2
*/* P SAVE ADDR OF LAST MBCB */
         SPACE 2
         S     R14,LENMBCB             BUMP BACK TO LAST BLOCK @Z40FPYJ
         LR    R11,R14                 ADDRESS LAST BLOCK      @Z40FPYJ
         DROP  R14                                             @Z40FPYJ
         USING MBCB,R11                                        @Z40FPYJ
         SPACE 2
*/* P BUMP TO 2ND MBCB SO 1ST CAN BE RETURNED */
         SPACE 2
         LA    R14,MBCBLEN(R1)         BUMP TO 2ND MBCB        @Z40FPYJ
         SPACE 2
*/* P QUEUE NEW POOL TO ASCB HEADER */
         SPACE 2
         LM    R2,R3,ASCBSUPC          GET ADDR AND SYNC WORDS @Z40FPYJ
CDSLOOP  LA    R15,C1(R3)              BUMP SYNC COUNT         @Z40FPYJ
         ST    R2,MBCBLINK             LINK ADDR OR ZERO       @Z40FPYJ
         CDS   R2,R14,ASCBSUPC         SWAP ADDR + SYNC  WORDS @Z40FPYJ
         BNE   CDSLOOP                 RETRY IF COMPARE FAILS  @Z40FPYJ
         SPACE 2                                               @Z40FPYJ
*/* P SET UP R2 WITH INITIALIZED SVRB */                       @Z40FPYJ
         SPACE 2                                               @Z40FPYJ
         LR    R2,R1                   NEW SVRB FROM POOL      @Z40FPYJ
         SPACE 2                                               @Z40FPYJ
*/* P (,SVRBINIT) RETURN TO MAINLINE WITH BLOCK ADDR IN R2 */
         SPACE 2
         B     SVRBINIT                RETURN TO MAINLINE      @Z40FPYJ
         SPACE 2
*/*SETTO1: N SET UP FOR ONLY ONE SVRB */
*/* P SET COUNT TO ONE AND NEXT TO ZERO */
         SPACE 2
SETTO1   LA    R15,1                   SET COUNT TO ONE        @Z40FPYJ
         SLR   R13,R13                 SET NEXT TO ZERO        @Z40FPYJ
         SPACE
*        SET UP SINGLE SVRB TO LOOK AS IF IT HAS AN MBCB, SO   @ZA29595
*        THE SVRB CAN BE INITIALIZED USING MAINLINE CODE. THE, @ZA29595
*        SVRB CAN NOT HAVE AN MBCB, BECAUSE EXIT PROLOGUE AND  @ZA29595
*        EXIT DO NOT FREE MBCB'S.                              @ZA29595
         SPACE
         LA    R14,MBCBPXLN                                    @ZA29595
         SR    R1,R14                                          @ZA29595
         XC    MBCBPXLN(RBSCBB-RBPRFXST,R1),MBCBPXLN(R1) ZERO  @ZA29595
*                                      SVRB BUT NOT SCB        @ZA29595
         OI    RBCDFLGS-RBPRFXST+MBCBPXLN(R1),RBNOCELL         @ZA29595
*                                      INDICATE THAT RB SHOULD @ZA29595
*                                      BE FREED BY EXIT OR     @ZA29595
*                                      EXIT PROLOGUE           @ZA29595
         LR    R14,R1                  COPY ADDR               @ZA29595
         B     SKIP1                   PROCESS ONE SVRB        @Z40FPYJ
         TITLE 'IEAVESVC- TERMMEM: CALL RTM ROUTINE'
         SPACE 2
*/*TERMMEM: P SET UP PARAMETERS TO PASS TO RTM */
         SPACE 2
TERMMEM  DS    0H
         SPACE 2
*/* S GETRTN:INTERNAL ROUTINE TO GET LOCK */
         SPACE 2
         BAL   R2,GETRTN               GO INTERNAL SUBRTN TO GETLOCK
         SPACE 2
         L     R11,ASCBASXB            GET ASXB ADDRESS
         USING ASXB,R11                                        @Z40FPYJ
         L     R11,ASXBSPSA            GET WSAL ADDRESS
         USING WSAL,R11
         L     R13,WSALABTM            PASS MEMTERMS SAVE AREA IN R13
         LA    R1,COMPCD7              GET COMPLETION CODE VALUE
         SPACE 2
*/*MEM2: L CALLRTM-- TYPE=MEMTERM */
         SPACE 2
MEM2     DC    0H'0'                   TEST LABEL
         CALLRTM TYPE=MEMTERM,COMPCOD=(1)  TERMINATE MEMORY
         SPACE 2
*/*RELLL4: L SETLOCK-- RELEASE LOCAL LOCK */
         SPACE 2
RELLL4   DC    0H'0'                   TEST LABEL
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=('GETMAIN COMPLETE',IEAVESV*
               C(GETRTN))              FREE LOCAL LOCK
         L     R6,AIEAPDS7             GET DISPATCHER ADDRESS
         SPACE 2
*/* P CLEAR SVC FLIH BIT */
         SPACE 2
         NI    PSASUP1,CXFF-PSASVC     CLEAR SVC FLIH BIT
         SPACE 2
*/*MEMXIT2: R EXIT TO DISPATCHER */
         SPACE 2
MEMXIT2  DC    0H'0'                   TEST LABEL
         BR    R6                      EXIT TO DISPATCHER
         TITLE 'TYPE 6 SVC PROCESSING'                         @Z40FPYK
************************************************************** @Z40FPYK
*                                                            * @Z40FPYK
*          TYPE 6 SVC INTERFACE ROUTINE                      * @Z40FPYK
*             TYPE 6 ENTRY PROCESSING                        * @Z40FPYK
*                                                            * @Z40FPYK
************************************************************** @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*TYPE6SVC: P PROCESS TYPE 6 SVC ENTRY */                     @Z40FPYK
*/* P MOVE REGISTERS INTO THE TCB */                           @Z40FPYK
         SPACE 2                                               @Z40FPYK
         CNOP  0,8                                             @Z40FPYK
TYPE6SVC DS    0H                                              @Z40FPYK
         MVC   TCBGRS(L64),LCCASGPR    SAVE REGISTERS IN TCB   @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SET PSA INDICATOR FOR RECOVERY */                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
         OI    PSASUP1,PSATYPE6        SET TYPE 6 SVC IN       @Z40FPYK
*                                      CONTROL INDICATOR       @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SET UP REGISTERS FOR SVC INPUT */                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
         LA    R1,TCBGRS               PARAMETER REGISTERS     @Z40FPYK
*                                      ARE OBTAINED FROM TCB   @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*  P R3 = CVT ADDR */                                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R3,FLCCVT               SET UP R3 = CVT ADDR    @Z40FPYK
        SPACE 2                                                @Z40FPYK
*/* P R13 = INPUT R13 */                                       @Z40FPYK
        SPACE 2                                                @Z40FPYK
        L     R13,C52(R1)               SET UP R13             @Z40FPYK
        SPACE 2                                                @Z40FPYK
*/* P R0, R1, R15 AS ON ENTRY TO SVC FLIH */                   @Z40FPYK
        SPACE 2                                                @Z40FPYK
        L     R15,C60(R1)               SET UP R15             @Z40FPYK
        L     R0,C0(R1)                 SET UP R0              @Z40FPYK
        L     R1,C4(R1)                 SET UP R1              @Z40FPYK
        SPACE 2                                                @Z40FPYK
*/* P R6 IS TYPE 6 EPA */                                      @Z40FPYK
        SPACE 2                                                @Z40FPYK
        L     R6,C0(R6)                 R6 HAS TYPE 6 EPA      @Z40FPYK
        SPACE 2                                                @Z40FPYK
*/* S TYPE6:GO TO TYPE 6 SVC */                                @Z40FPYK
        SPACE 2                                                @Z40FPYK
        BALR  R14,R6                   GO TO TYPE 6 SVC...R14  @Z40FPYK
*                                      HAS RETURN ADDRESS IF   @Z40FPYK
*                                      A BR 14 OR T6EXIT       @Z40FPYK
*                                      CALLER RETURN IS USED   @Z40FPYK
         EJECT                                                 @Z40FPYK
************************************************************** @Z40FPYK
*                                                            * @Z40FPYK
*         TYPE 6 EXIT PROCESSING                             * @Z40FPYK
*            RETURN = CALLER                                 * @Z40FPYK
*              BR 14 PROCESSING                              * @Z40FPYK
*                                                            * @Z40FPYK
************************************************************** @Z40FPYK
*/* N IF THE TYPE 6 SVC DOES A BR 14 */                        @Z40FPYK
*/* N OR A T6EXIT RETURN = CALLER */                           @Z40FPYK
*/* N CONTROL WILL BE RECEIVED HERE */                         @Z40FPYK
*/* P RE-ESTABLISH ADDRESSIBILITY */                           @Z40FPYK
         SPACE 2                                               @Z40FPYK
         BALR  R9,C0                   ESTABLISH ADDRESSIBILITY@Z40FPYK
         USING *,R9                                            @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*CALLER: P RESTORE THE TCB POINTER */                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
CALLER   DS    0H                                              @Z40FPYK
         L     R4,PSATOLD              RESTORE TCB POINTER     @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P RESTORE ASCB POINTER */                                  @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R7,PSAAOLD              RESTORE ASCB POINTER    @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SAVE SVC RETURN REGISTERS FOR CALLER */                  @Z40FPYK
         SPACE 2                                               @Z40FPYK
         ST    R0,TCBGRS0              SAVE RETURN REG 0       @Z40FPYK
         ST    R1,TCBGRS1              SAVE RETURN REG 1       @Z40FPYK
         ST    R15,TCBGRS15            SAVE RETURN REG 15      @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P RESET TYPE 6 SVC AND SVC FLIH RECOVERY FLAGS */          @Z40FPYK
         SPACE 2                                               @Z40FPYK
         NI    PSASUP1,CXFF-(PSATYPE6+PSASVC) TURN OFF TYPE 6  @Z40FPYK
*                                             AND SVC FLIH     @Z40FPYK
*                                             SUPER RECOVERY   @Z40FPYK
*                                             INDICATORS       @Z40FPYK
         SPACE 2
*/* P PURGE FRR STACK USED BY TYPE 6 SVC */
         SETFRR P,WRKREGS=(R2,R3)                              @ZA15765
         SPACE 2                                               @Z40FPYK
*/* P RESTORE SUPER STACK TO NORMAL */                         @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R8,PSANSTK              RESTORE FRR STACK       @Z40FPYK
         ST    R8,PSACSTK              TO NORMAL               @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P PICK UP EPA OF COMMON EXIT PROLOGUE ENTRY */             @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R14,EXPEPA              EXIT PROLOGUE EPA       @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* R EXIT TO EXIT PROLOGUE */                                 @Z40FPYK
         SPACE 2                                               @Z40FPYK
         BR    R14                     EXIT TO EXIT PROLOGUE   @Z40FPYK
*                                      EPA                     @Z40FPYK
         EJECT                                                 @Z40FPYK
************************************************************** @Z40FPYK
*                                                            * @Z40FPYK
*            TYPE 6 EXIT ENTRY POINT                         * @Z40FPYK
*              T6EXIT RETURN=SRB                             * @Z40FPYK
*              T6EXIT RETURN=DISP                            * @Z40FPYK
*               IF SRB RETURN, R1 = @SRB                     * @Z40FPYK
*                              R2 = 8 (SRB REQUEST CODE)     * @Z40FPYK
*               IF DISPATCHER RETURN, R2 = 4                 * @Z40FPYK
*               IF CALLER RETURN, R2 = 0                     * @Z40FPYK
*                                                            * @Z40FPYK
************************************************************** @Z40FPYK
         SPACE 2                                               @Z40FPYK
T6EXIT   DS    0H                                              @Z40FPYK
         ENTRY IEAVET6E                                        @Z40FPYK
IEAVET6E DS    0H                                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*T6EXIT: E TYPE6 EXIT EPA */                                 @Z40FPYK
*/* P ESTABLISH ADDRESSIBILITY */                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
         BALR  R10,C0                  ADDRESSIBILITY          @Z40FPYK
         USING *,R10                                           @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (SRB,,CALL,RETCALL,DISP,DISPEXIT) REQUEST TYPE ?  */     @Z40FPYK
         SPACE 2                                               @Z40FPYK
BRTAB    DS    0H                                              @Z40FPYK
         B     C4(R2,R10)              REQUEST TYPE ?          @Z40FPYK
         B     RETCALL                 R2=0 RETURN TO CALLER   @Z40FPYK
         B     DISPEXIT                R2=4 RETURN TO DISPATCH @Z40FPYK
*                                      R2=8 RETURN TO SRB IN R1@Z40FPYK
         EJECT                                                 @Z40FPYK
*        DISPATCH AN SRB
*
*/* P ACQUIRE ASCB ADDRESS */                                  @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R8,PSAAOLD              ACQUIRE ASCB ADDRESS    @Z40FPYK
         USING ASCB,R8                                         @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (YES,SRBOK,NO,) SRB ASCB DEFAULTED ? */                  @Z40FPYK
         SPACE 2                                               @Z40FPYK
         USING SRB,R1                                          @Z40FPYK
         L     R5,SRBASCB              GET SRB ASCB ADDRESS    @Z40FPYK
         LTR   R5,R5                   IF ZERO - CURRENT MEM   @Z40FPYK
         BZ    SRBOK                   AND ALL IS OKAY         @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (YES,,NO,ABNDASCB) SRB SCHED IN THIS MEMORY ? */         @Z40FPYK
         SPACE 2                                               @Z40FPYK
         CR    R8,R5                   SRB TO BE SCHEDULED     @Z40FPYK
*                                      IN THIS MEMORY ?        @Z40FPYK
         BNE   ABNDASCB                NO - INVALID REQUEST    @Z40FPYK
*                                      ABEND SVC               @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*SRBOK: P SAVE SRB ADDRESS */                                @Z40FPYK
         SPACE 2                                               @Z40FPYK
SRBOK    DS    0H                                              @Z40FPYK
         LR    R5,R1                   SAVE INPUT SRB ADDRESS  @Z40FPYK
         DROP  R1                                              @Z40FPYK
         USING SRB,R5                                          @ZA20352
         SPACE 2
*        IF CPU AFFINITY WAS SPECIFIED, GO OFF TO CHECK        @ZA20352
*        WHETHER THE SRB CAN RUN ON THIS CPU.                  @ZA20352
*
*        IT IS NOT NECESSARY TO CHECK WHETHER SRB'S ARE        @ZA20352
*        DISPATCHABLE. THAT IS BECAUSE THIS CODE RUNS UNDER A  @ZA20352
*        TCB AND TCB'S ARE ALWAYS MADE NON-DISPATCHABLE EITHER @ZA20352
*        PRIOR TO OR CONCURRENTLY WITH SRB'S BEING STOPPED.    @ZA20352
         SPACE 2
         LH    R7,SRBCPAFF             SRB'S AFFINITY MASK     @ZA20352
         LTR   R7,R7                   ANY?                    @ZA20352
         BNZ   CHKAFFIN                SPECIFIED, GO CHECK     @ZA20352
*                                      AFFINITY                @ZA20352
AFFINOK  DS    0H                                              @ZA20352
         SPACE 2                                               @Z40FPYK
*/* P GET LCCA ADDRESS */                                      @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R7,PSALCCAV             NEED LCCA ADDRESS FOR   @Z40FPYK
*                                      DISPATCHER JST ROUTINE  @Z40FPYK
         USING LCCA,R7                                         @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P NOW NEED TO SAVE STATUS OF TASK */                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R4,PSATOLD              SET UP TO TASK ADDRESS  @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SAVE FLOATING POINT REGISTERS */                         @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R2,KTCBPRFX             NEED PREFIX TO TCB      @Z40FPYK
         AR    R2,R4                   POINT TO TCB PREFIX     @Z40FPYK
         USING TCBFIX,R2                                       @Z40FPYK
         STD   R0,TCBFRS0              SAVE FP REG 0           @Z40FPYK
         STD   R2,TCBFRS2              SAVE FP REG 2           @Z40FPYK
         STD   R4,TCBFRS4              SAVE FP REG 4           @Z40FPYK
         STD   R6,TCBFRS6              SAVE FP REG 6           @Z40FPYK
         DROP  R2                                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (NO,SKIPTQE,YES,)  TEST FOR TQE */                       @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R2,TCBTME               TQE ADDRESS             @Z40FPYK
         LTR   R2,R2                   TEST IF TQE EXISTS      @Z40FPYK
         BNH   SKIPTQE                 NO, DO NO UPDATE        @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (NO,SKIPTQE,YES,) IS CPU TIMER WORKING ? */              @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R1,PSAPCCAV             GET PCCA ADDRESS        @Z40FPYK
         USING PCCA,R1                                         @Z40FPYK
         TM    PCCAINTE,PCCANUIN       IS THIS CPU'S TIMER OK? @Z40FPYK
         BO    SKIPTQE                 NO, BYPASS SAVING TIMER @Z40FPYK
         DROP  R1                                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SAVE CPU INTERVAL TIMER VALUE IN TQE */                  @Z40FPYK
         SPACE 2                                               @Z40FPYK
         USING TQE,R2                                          @Z40FPYK
         STPT  TQEVAL                  SAVE TIMER VALUE        @Z40FPYK
         DROP  R2                                              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P RESET TIMER TO A HIGH VALUE */                           @Z40FPYK
         SPACE 2                                               @Z40FPYK
         SPT   HITIME                  SET CPU TIMER HIGH      @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*SKIPTQE: P RESET SVC FLIH AND TYPE 6 IN CONTROL */          @Z40FPYK
         SPACE 2                                               @Z40FPYK
SKIPTQE  DS    0H                                              @Z40FPYK
         NI    PSASUP1,CXFF-(PSASVC+PSATYPE6) SVC FLIH +       @Z40FPYK
*                                             TYPE 6 NO        @Z40FPYK
*                                             LONGER IN        @Z40FPYK
*                                             CONTROL          @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P PURGE FRR STACK USED BY TYPE 6 SVC */
         SETFRR P,WRKREGS=(R2,R3)                              @ZA15765
         SPACE 2                                               @Z40FPYK
*/* P INDICATE DISPATCHER IN CONTROL */                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
         OI    PSASUP1,PSADISP         INDICATE DISPATCHER IN  @Z40FPYK
*                                      CONTROL                 @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P  GET ADDRESS OF DISPATCHER SRB ROUTINE */                @Z40FPYK
         SPACE 2                                               @Z40FPYK
*  AT THIS TIME THE DISPATCHER JOB STEP TIMING ROUTINE WILL    @Z40FPYK
*  BE INVOLKED.  THIS FUNCTION WILL CLOBBER REG 9 WHICH IS     @Z40FPYK
*  OUR BASE REGISTER.  REGISTER 9 ON RETURN TO THE SVC FLIH    @Z40FPYK
*  WILL CONTAIN THE DISPATCHER'S BASE REGISTER.  THIS WILL BE  @Z40FPYK
*  USED AS THE BASE WHEN WE GO TO THE SRB ROUTINE              @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* S DISPJST:CALCULATE JOB STEP TIMING */                     @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R14,DISPJST             CALCULATE JOB STEP      @Z40FPYK
         BALR  R14,R14                 TIMING FOR TASK         @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P RESET TCB ACTIVE STATE */                                @Z40FPYK
         SPACE 2                                               @Z40FPYK
         SR    R1,R1                   RESET TCB ACTIVE        @Z40FPYK
         ST    R1,TCBXSCT              CLEAR CPUID AND ACTIVE  @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/*ACTASK: P DECREMENT COUNT OF CPU'S IN MEMORY */             @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R3,ASCBCPUS             ACTIVE CPUS IN MEMORY   @Z40FPYK
ACTASK   DS    0H                                              @Z40FPYK
         LR    R2,R3                   DUPLICATE COUNT FOR CS  @Z40FPYK
         BCTR  R2,0                    DECREMENT BY 1          @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* D (YES,ACTASK,NO,) UPDATE FAIL ? */                        @Z40FPYK
         SPACE 2                                               @Z40FPYK
         CS    R3,R2,ASCBCPUS          UPDATE CPU COUNT        @Z40FPYK
         BNE   ACTASK                  FAIL -- TRY AGAIN       @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P TAKE OUT OF TASK MODE */                                 @Z40FPYK
         SPACE 2                                               @Z40FPYK
         ST    R1,PSATNEW              ZERO PSA TCB NEW        @Z40FPYK
         ST    R1,PSATOLD              ZERO PSA TCB OLD        @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P RESTORE SRB TO R2 FOR DISP ROUTINE */                    @Z40FPYK
         SPACE 2                                               @Z40FPYK
         LR    R2,R5                   SRB FOR DISPATCHER      @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P SET UP ASCB FOR DISPATCHER ROUTINE */                    @Z40FPYK
         SPACE 2                                               @Z40FPYK
         LR    R5,R8                   ASCB MUST BE IN R5      @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P ACQUIRE DISPATCHER LOCK */                               @Z40FPYK
         SPACE 2                                               @Z40FPYK
* FOLLOWING IS CODE THAT WOULD BE GENERATED BY A SETLOCK       @Z40FPYK
*        SETLOCK OBTAIN,TYPE=DISP,MODE=UNCOND,RELATED=(DISP)   @Z40FPYK
         L     R13,AGSLDISP            ADDRESS OF LOCK MGR     @Z40FPYK
         BALR  R14,R13                 GO TO LOCK MGR          @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* E DISP SRB ROUTINE */                                      @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R15,DISPSRB             SRB DISP ROUTINE ADDR   @Z40FPYK
         BR    R15                     GO TO DISPATCHER        @Z40FPYK
         EJECT
*        CPU AFFINITY WAS SPECIFIED FOR THE SRB.               @ZA20352
*        CHECK WHETHER THE AFFINITY IS TO THIS CPU.            @ZA20352
*
CHKAFFIN DS    0H                                              @ZA20352
         L     R1,PSAPCCAV             PCCA ADDRESS            @ZA20352
         USING PCCA,R1                                         @ZA20352
         LH    R1,PCCACAFM             CPU'S ADDR. AS BIT MASK @ZA20352
         DROP  R1                                              @ZA20352
         NR    R1,R7                   CAN SRB RUN ON THIS CPU @ZA20352
         BNZ   AFFINOK                 YES, RETURN TO MAINLINE @ZA20352
*
*        SRB CAN NOT RUN ON THIS CPU. FALL THROUGH TO SCHEDULE @ZA20352
*        THE SRB TO RUN LATER.                                 @ZA20352
*                                                              @ZA20352
         SCHEDULE SRB=(R5),SCOPE=LOCAL                         @ZA20352
*
*        FALL THROUGH TO CLEAN UP AND EXIT TO DISPATCHER       @ZA20352
         EJECT                                                 @Z40FPYK
*        EXIT TO THE DISPATCHER
*
*/*DISPEXIT: P RESET SVC FLIH + TYPE 6 FLAGS */                @Z40FPYK
         SPACE 2                                               @Z40FPYK
DISPEXIT DS    0H                                              @Z40FPYK
         NI    PSASUP1,CXFF-(PSASVC+PSATYPE6) RESET SVC FLIH + @Z40FPYK
*                                             TYPE 6 FLAGS     @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* P PURGE FRR STACK USED BY TYPE 6 SVC */
         SETFRR P,WRKREGS=(R2,R3)                              @ZA15765
         SPACE 2                                               @Z40FPYK
*/* E MAIN DISPATCHER ENTRY */                                 @Z40FPYK
         SPACE 2                                               @Z40FPYK
         L     R6,AIEAPDS7             MAIN DISPATCHER ENTRY   @Z40FPYK
         BR    R6                      GO TO DISPATCHER        @Z40FPYK
         EJECT                                                 @Z40FPYK
*/*ABNDASCB: P INVALID REQUEST = ABEND 0FD          */         @Z40FPYK
         SPACE 2                                               @Z40FPYK
ABNDASCB DS    0H                                              @Z40FPYK
         NI    PSASUP1,CXFF-(PSASVC+PSATYPE6) RESET SVC FLIH + @Z40FPYK
*                                             TYPE 6 FLAGS     @Z40FPYK
         MVC   PSACSTK,PSANSTK         NORMALIZE FRR STACK
         LA    R1,COMPCDD              SET UP ABEND CODE       @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* E ABEND 0FD */                                             @Z40FPYK
         SPACE 2                                               @Z40FPYK
         CALLRTM TYPE=SVCERR,COMPCOD=(1)                       @Z40FPYK
         EJECT                                                 @Z40FPYK
*        PREPARE TO RETURN TO CALLER
*
*/*RETCALL: P (,CALLER) SET UP COMMON CALLER BASE */           @Z40FPYK
         SPACE 2                                               @Z40FPYK
RETCALL  DS    0H                                              @Z40FPYK
         L     R9,CALLBASE             COMMON BASE FOR CALLER  @Z40FPYK
         USING CALLER,R9               AND BR 14 EXITS         @Z40FPYK
         B     CALLER                  GO TO THE COMMON        @Z40FPYK
*                                      CALLER AND BR14 CODE    @Z40FPYK
         SPACE 2                                               @Z40FPYK
*/* FOOTING
*/*
*/* */
*/*IEAVESVC: END SVC FLIH */
         EJECT
***********************************************************************
*                                                                     *
*                        CONSTANTS & STORAGE                          *
*                                                                     *
***********************************************************************
         EXTRN TRSVC
ATRSVC   DC    A(TRSVC)                TRACE ENTRY POINT
LLCO     DC    V(LLCOSVCF)             LOCK MGR -- LOCAL LOCK OBTAIN
AGSLDISP DC    V(GSLSDISP)             ENTRY--DISP LOCK OBTAIN @Z40FPYK
ESRTBBGN DC    V(IGC109)               BEGINNING OF ACTUAL ESR @ZA12726
*                                      TABLE                   @ZA12726
ESRTBEND DC    V(ESREND)               END OF ESRTABLE
         EXTRN IEAPDS7
AIEAPDS7 DC    A(IEAPDS7)              DISPATCHER ENTRY POINT
         EXTRN SVCTABLE
SVCTAB   DC    A(SVCTABLE)             SVC TABLE ENTRY POINT
KRBPRFXL DC    AL4(RBPRFLNA)           LENGTH OF RB PREFIX
FOUR     DC    H'4'                    COMPARE VALUE
CALLBASE DC    A(CALLER)               BASE FOR CALLER EXIT    @Z40FPYK
ALHSNPSW DS    0F                      ENABLED LEFT HALF SVC NPSW
         DC    X'070C0000'
IDSVRB   DC    C'SVRB'                 ID FOR SVRB POOL BLOCK  @Z40FPYJ
LENMBCB  DC    A(MBCBLEN)              LENGTH OF MBCB   BLOCK  @Z40FPYJ
SVRBMASK DC    X'00'                   MASK FOR SETTING SVRB   @Z40FPYJ
         DC    AL1(SVRBDWDS)           LENGTH                  @Z40FPYJ
         DC    AL1(RBFTSVRB)           TYPE                    @Z40FPYJ
         DC    AL1(RBFDYN+RBATTN)      AND FLAGS               @Z40FPYJ
DISPJST  DC    V(DSJSTCSR)             DISPATCHER JOB STEP     @Z40FPYK
*                                      TIMING ROUTINE          @Z40FPYK
DISPSRB  DC    V(DSSRBRTN)             DISPATCHER GLOBAL SRB   @Z40FPYK
*                                      SCHEDULING ROUTINE      @Z40FPYK
EXPEPA   DC    V(EXPEPAT6)             EXIT PROLOGUE TYPE 6    @Z40FPYK
*                                      ENTRY POINT ADDRESS     @Z40FPYK
HITIME   DS    0D                                              @Z40FPYK
         DC    X'7FFFFFFFFFFFFFFF'     HIGH CPU INTERVAL TIMER @Z40FPYK
KTCBPRFX DC    A(-(TCB-TCBFRS))        TCB PREFIX DECREMENT VAL@Z40FPYK
         DS    0D                                               ZP60013
SVCCTTBL DC    256F'0'                 SVC COUNTER TABLE        ZP60013
         TITLE 'IEAVESVC- PCCA DSECT MAPPING'                  @Z40FPYK
         IHAPCCA                                               @Z40FPYK
         TITLE 'IEAVESVC- TQE  DSECT MAPPING'                  @Z40FPYK
         IHATQE                                                @Z40FPYK
         TITLE 'IEAVESVC- LCCA DSECT MAPPING'
         IHALCCA
         TITLE 'IEAVESVC- PSA MAPPING'
         IHAPSA
         TITLE 'IEAVESVC- ASCB DSECT MAPPING'
         IHAASCB
         TITLE 'IEAVESVC- ASXB DSECT MAPPING'
         IHAASXB
         TITLE 'IEAVESVC- TCB DSECT MAPPING'
         IKJTCB
         TITLE 'IEAVESVC- RB DSECT MAPPING'
         IKJRB
         TITLE 'IEAVESVC- SRB DSECT MAPPING'                   @Z40FPYK
         IHASRB                                                @Z40FPYK
         TITLE 'IEAVESVC- SVC DSECT MAPPING'
         IHASVC
         TITLE 'IEAVESVC- MBCB DSECT MAPPING'                  @Z40FPYK
         IHAMBCB                                               @Z40FPYK
         TITLE 'IEAVESVC- CVT DSCET MAPPING'
         CVT   DSECT=YES,PREFIX=NO,LIST=YES
         TITLE 'IEAVESVC- WSAV DSECT MAPPING'
         IHAWSAVT DSECT=YES
         TITLE 'IEAVESVC- FRRS DSECT MAPPING'
         IHAFRRS
         TITLE 'IEAVESVC- SS SCREEN TABLE DSECT'
***********************************************************************
*
* PURPOSE -
*        THE SUBSYSTEM SCREEN TABLE SPECIFIES WHICH SVC'S ARE ALLOWED
*        TO BE ISSUED BY A TASK, AND WHICH ARE BEING SCREENED
*        AGAINST.  THIS AREA IS INITIALIZED BY THE SUB SYSTEM WHICH MAY
*        WISH TO RESTRICT CERTAIN TASKS FROM ISSUING A SUBSET OF SVC'S.
* USAGE -
*        IT IS ACTIVATED FOR A TASK BY SETTING THE TCBSVCS FLAG IN
*        TCBFLGS7 AND SETTING TCBSVCA2 TO THE ADDRESS OF THE SCREEN
*        TABLE.
*        A REQUEST FOR AN SVC BEING SCREENED AGAINST WILL BE CONVERTED
*        TO THE SVC REQUEST SPECIFIED AT THE TOP OF THE SCREEN TABLE.
*        EACH SVC (0-255) IS MAPPED INTO A BYTE IN THE SCREEN TABLE,
*        STARTING AT SSTMASK AND IN ASCENDING ORDER.  WHETHER OR NOT
*        THE SVC MAY BE ISSUED MAY BE DETERMINED BY THE SSTSVCOK FLAG.
*
***********************************************************************
         SPACE 2
SSTAB    DSECT SUB SYSTEM SCREEN TABLE                         @Z40EPVC
SSTSVCN  DS    D                  SUBSYSTEM SVC ENTRY          @Z40EPVC
SSTMASK  DS    CL256              SVC SCREENING MASK           @Z40EPVC
SSTSVCOK EQU   X'80'              SVC MAY BE ISSUED            @Z40EPVC
SSTEND   EQU   *                                               @Z40EPVC
SSTSIZE  EQU   SSTEND-SSTAB       SIZE OF SCREEN TABLE         @Z40EPVC
         SPACE 4
         TITLE 'IEAVESVC- EQUATES'
***********************************************************************
*                                                                     *
*                      REGISTER     EQUATES                           *
*                                                                     *
***********************************************************************
R0       EQU   0                       STORAGE LOCATION REGISTER
*                                      GENERAL REGISTER
R1       EQU   1                       GENERAL REGISTER
R2       EQU   2                       GENERAL REGISTER
*                                      PREVIOUS RB ADDRES
R3       EQU   3                       GENERAL REGISTER        @Z40FPYJ
*                                      CVT ADDRESS
R4       EQU   4                       TCB ADDRESS REGISTER
*                                      REGISTER 4--WORK REG    @Z40FPYK
R5       EQU   5                       RB ADDRESS
*                                      GENERAL REGISTER 5      @Z40FPYK
R6       EQU   6                       SVC NUMBER REGISTER
*                                      SVC TABLE ADDRESS
*                                      DISPATCHER ENTRY ADDRESS
*                                      REGISTER 6--WORK REG    @Z40FPYK
*                                      SVC ENTRY POINT ADDRESS
R7       EQU   7                       LCCA BASE ADDRESS
*                                      GENERAL REGISTER
*                                      ASCB ADDRESS
R8       EQU   8                       RB PREFIX ADDRESS
*                                      GENERAL REGISTER 8      @Z40FPYK
*                                      LOCKS NEEDED REGISTER
*                                      WORK REGISTER
R9       EQU   9                       BASE REGISTER
R10      EQU   10                      LCCA ADDRESS
*                                      TRACE ENTRY POINT ADDRESS
*                                      T6EXIT BASE REGISTER    @Z40FPYK
R11      EQU   11                      GENERAL REGISTER        @Z40FPYJ
*                                      TRACE BR INSTRUCTION ADDRESS
*                                      ASXB ADDRESS REGISTER
*                                      WSAL ADDRESS REG
*                                      SS SCREEN TABLE ADDRESS @Z40EPVC
R12      EQU   12                      UNUSED
R13      EQU   13                      CVT ADDRESS
*                                      GENERAL REGISTER
*                                      RETURN CODE REGISTER
R14      EQU   14                      GENERAL REGISTER        @Z40FPYJ
*                                      WORK REGISTER
*                                      RETURN ADDRESS
R15      EQU   15                      GENERAL REGISTER
*                                      RETURN CODE REGISTER
*                                      ESR CODE = CALLER'S REG 15
         EJECT
***********************************************************************
*                                                                     *
*                      CONSTANT    EQUATES                            *
*                                                                     *
***********************************************************************
C0       EQU   0                       DISPLACEMENT VALUE
C1       EQU   1                       DISPLACEMENT VALUE
C2       EQU   2                       DISPLACEMENT VALUE
C3       EQU   3                       DISPLACEMENT VALUE
C4       EQU   4                       DISPLACEMENT VALUE
C16      EQU   16                      DISPLACEMENT VALUE
C28      EQU   28                      DISPLACEMENT VALUE
C52      EQU   52                      DISPLACEMENT VALUE
C60      EQU   60                      DISPLACEMENT VALUE
C64      EQU   64                      DISPLACEMENT VALUE
CXF0     EQU   X'F0'                   IMMEDIATE VALUE
CXFA     EQU   X'FA'                   IMMEDIATE VALUE
CXFF     EQU   X'FF'                   IMMEDIATE VALUE
SCBID    EQU   X'DB'                   SCB ID VALUE            @Z40FPYJ
L8       EQU   8                       LENGTH VALUE
L64      EQU   64                      LENGTH VALUE
T255     EQU   255                     LENGTH VALUE
EXSARB   EQU   RBGRSAVE+64-RBPRFXST    REMAINDER RB AFTER GR SAVE AREA
M4       EQU   4                       MASK VALUE
M8       EQU   8                       MASK VALUE
M15      EQU   15                      MASK VALUE
ONE      EQU   1                       GENERAL VALUE
TWO      EQU   2                       GENERAL VALUE
COMPCD2  EQU   X'047'                  UNAUTHORIZED COMP CODE
COMPCD1  EQU   X'0F8'                  SRB OR LOCKED ISSUER
COMPCD3  EQU   X'0F9'                  UNABLE TO EXPAND SVRB POOL
COMPCDE4 EQU   X'16D'                  INCORRECT ESR CODE
COMPCD7  EQU   X'0FA'                  NO SVRB CORE FOR ABEND
COMPCDD  EQU   X'0FD'                  TYPE 6 INVALID SRB      @Z40FPYK
AB13     EQU   13                      ABEND SVC NUMBER
M10      EQU   10                      MASK VALUE
M12      EQU   12                      MASK VALUE
SVRBDWDS EQU   (SVRBLEN+7)/8           CALCULATE SVRB IN DWORDS
ENABLE   EQU   X'03'                   MASK FOR STOSM
SVRBNUM  EQU   9                       NUMBER OF SVRBS IN THE  @Z40FPYJ
*                                      POOL,     WITH SOME PADDING
*                                      LEFT BETWEEN THE RBS.
         END
/*
//*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  *
  IDENTIFY IEAVESVC('ZP60013')
//*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*
//STEP4   EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZP60013)
          .
/*
//*
//STEP5CK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZP60013)
        CHECK
        .
/*
//*
//STEP5   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *

  APPLY
        SELECT(ZP60013)
        DIS(WRITE)
        .
/*
//
