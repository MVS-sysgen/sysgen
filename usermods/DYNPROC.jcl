//DYNPROC JOB  (SMPEJOB),
//             'DYNAMIC PROCLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* ***************************************************************** *
//* SYS1.PROCLIB: BACKUP JES2 TO JES20099                             *
//* ***************************************************************** *
//*
//BACKUP01 EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=SYS1.PROCLIB(JES2),DISP=SHR
//SYSUT2   DD  DSN=SYS1.PROCLIB(JES20098),DISP=SHR
//SYSIN    DD  DUMMY
//*
//UPDATE02 EXEC PGM=IEBUPDTE,PARM=NEW
//*
//* ***************************************************************** *
//* SYS1.PROCLIB: CREATE JES2 (JES2 STARTUP PROC) TO ADD DYNPROC      *
//* ***************************************************************** *
//*
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=JES2,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//JES2     PROC M=JES2PM00,
//             N1=SYS1,
//             N2=SYS2,
//             L=LINKLIB,
//             U=3350,
//             P=PARMLIB
//IEFPROC  EXEC PGM=HASJES20,
//             TIME=1440,
//             DPRTY=(15,15)
//STEPLIB  DD  UNIT=&U,DISP=SHR,DSN=&N1..&L
//PROC00   DD  DSN=&N1..PROCLIB,DISP=SHR
//         DD  UNIT=&U,VOL=SER=SYSCPK,DISP=SHR,DSN=SYSC.PROCLIB
//         DD  DSN=&N2..PROCLIB,DISP=SHR
//         DD  DSN=&N1..PROCLIB,DISP=SHR
//********************************************************************
//* ADDED FOR DYNPROC                                                 
//* SEE HTTP://WWW.JAYMOSELEY.COM/HERCULES/USERMODS/INDEX.HTM         
//* FOR MORE INFO                                                     
//********************************************************************
//IEF0PDSI DD  DSN=&N1..PROCLIB,DISP=SHR             <---1ST CONVERTER
//         DD  UNIT=&U,VOL=SER=SYSCPK,DISP=SHR,DSN=SYSC.PROCLIB       
//         DD  DSN=&N2..PROCLIB,DISP=SHR                              
//         DD  DSN=&N1..PROCLIB,DISP=SHR                              
//IEF1PDSI DD  DSN=&N1..PROCLIB,DISP=SHR             <---2ND CONVERTER
//         DD  UNIT=&U,VOL=SER=SYSCPK,DISP=SHR,DSN=SYSC.PROCLIB       
//         DD  DSN=&N2..PROCLIB,DISP=SHR                              
//         DD  DSN=&N1..PROCLIB,DISP=SHR                              
//********************************************************************
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR
//HASPLIST DD  DDNAME=IEFRDER
./ ENDUP
><
//*********************************************************************
//*
//* NAME: SYS1.DYNAMIC.PROCLIB(DYNPROCS)
//*
//* DESC: INSTALL DYNAMIC PROCLIB SUPPORT
//*
//* YOU MAY ELECT TO INSTALL MANUALLY BY FOLLOWING THE DIRECTIONS
//*WHICH ARE INCLUDED IN THE $$$DOC MEMBER OF THIS PDS
//*
//* IF YOU HAVE ANY QUESTIONS PLEASE CALL:
//*
//* BRIAN WESTERMAN        EMAIL:  BRIAN_WESTERMAN@SYZYGYINC.COM
//* SYZYGY INCORPORATED
//* PHONE: (800) 767-2244  FAX:(800) 366-4082
//*********************************************************************
//*
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA,DLM='><'
++ USERMOD (#DYP001)  .
++ VER   (Z038)  FMID (FBB1221)
                 PRE (UZ45794)
               /*

               PRIVATE PROCLIB MODIFICATIONS
               FOR HERCULES TURNKEY#3 SYSTEM
               VERSION 4, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS

               ***** WARNING ***** WARNING ***** WARNING *****

               DO NOT ATTEPMT TO RESEQUENCE THIS MODIFICATION,
               AS IT CONTAINS A MACRO UPDATE TO A SYSGEN MACRO.

               ***** WARNING ***** WARNING ***** WARNING *****

               THIS MOD SUPPLIES THE SYSGEN MACRO UPDATE FOR
               SGIEF441.  THIS MACRO IS UPDATED TO SUPPORT
               DYNAMIC PROCLIB.

               DOCUMENTATION ON THE DYNAMIC PROCLIB MODS
               ARE CONTAINED IN THE MOD CONTAINING THE
               OBJECT DECK FOR IEFVPP, A NEW MODULE.

               BEWARE THAT THE "CHANGE" STATEMENT FOR IEFVH1
               MAY NOT BE EFFECTIVE WITHOUT A UCLIN TO
               DELETE THE CURRENT DEFINITIONS OF MODULE
               IEFVH1 AND LOAD MODULE IEFVH1.

                                                                */  .
++ JCLIN .
//SG15 EXEC LINKS,
// PARM='NCAL,LIST,XREF,LET,RENT',
// NAME=LPALIB
//SYSLIN DD *
 INCLUDE AOSB3(IEFVHA)
 INCLUDE AOSB3(IEFVHC,IEFVHCB)
 INCLUDE AOSB3(IEFVINE,IEFVHM,IEFVHL)
 INCLUDE AOSB3(IEFVINA)
 INCLUDE AOSB3(IEFVIND)
 INCLUDE AOSB3(IEFVINB)
 INCLUDE AOSB3(IEFVINC)
 INCLUDE AOSB3(IEFVHEB)
 INCLUDE AOSB3(IEFNB9CR)
 INCLUDE AOSB3(IEFNB9CT)
 INCLUDE AOSB3(IEFVFA,IEFVFB)
 INCLUDE AOSB3(IEFVGM)
 INCLUDE AOSB3(IEFVHQ)
 INCLUDE AOSB3(IEFVHR)
 INCLUDE AOSB3(IEFVHF)
        CHANGE IEFVHA(IEFVPP0) PRIVATE PROCLIB
 INCLUDE AOSB3(IEFVH1)
 INCLUDE AOSB3(IEZNCODE)
 INCLUDE AOSB3(IEZDCODE)
 INCLUDE AOSB3(IEFVGM90)
         INCLUDE AOSB3(IEFVPP) PRIVATE PROCLIB
 ENTRY IEFVH1
 NAME IEFVH1(R)
/*
++ MACUPD (SGIEF441)  DISTLIB (AGENLIB)  .
./       CHANGE NAME=SGIEF441,LIST=ALL
         PUNCH '        CHANGE IEFVHA(IEFVPP0) PRIVATE PROCLIB' #DYP001 38907099
*        NEW CSECT FOR PRIVATE PROCLIB                          #DYP001 38971099
         PUNCH '         INCLUDE AOSB3(IEFVPP) PRIVATE PROCLIB' #DYP001 38972099
./       ENDUP
++ USERMOD (#DYP002)  .
++ VER   (Z038)  FMID (EBB1102)              /* MVS 3.9 BASE */
                 PRE (
                      #DYP001                /* SYSGEN MACRO UPDATE */
                             )
               /*

               PRIVATE PROCLIB MODIFICATIONS
               VERSION 4, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS

               ***** WARNING ***** WARNING ***** WARNING *****

               THIS OBJECT DECK WAS ASSEMBLED USING THE RESERVED WORD
               "RFULLE" IN THE CONVERTER WORK AREA USED AS THE ANCHOR
               FOR THE PRIVATE PROCLIB WORK AREA.

               ***** WARNING ***** WARNING ***** WARNING *****

  PREFACE
               THESE MODIFICATIONS ARE CONSTRUCTED IN FIVE SEPARATE
               PIECES BECASUE OF FUNCTIONAL OWNERSHIP AND
               MAINTENANCE PACKAGING.  IN ADDITION, THERE IS AN
               OPTIONAL MODIFICATION TO JES2 SUPPLIED IN THE
               TEXT OF THESE COMMENTS.

               THE ORIGINAL SOURCE OF THIS MOD IS MUTUAL LIFE OF
               WATERLOO, ONTARIO, CANADA.  AMDAHL HAS PROVIDED
               SUBSTANTIAL CHANGES IN THE METHODS OF INSTALLATION
               AND INTERFACES, AS WELL AS FUNCTIONAL ENHANCEMENTS.
               THE BASIC LOGIC, HOWEVER, IS UNCHANGED.

               THE IEFVPP MODULE HAS BEEN REWRITTEN TO BE COM-
               PATIBLE WITH ALL VERSIONS OF THE MVS CONVERTER
               THROUGH MVS/SP 2.1.1.  HOWEVER, IT SHOULD BE
               REASSEMBLED WHENEVER A CHANGE IS MADE TO THE
               CONVERTER WORK AREA MACROS (IEFCOMWA AND IEFCVRWA).

  JCLIN
               A JCLIN IS NECESSARY TO INCLUDE THIS
               SUPPORT INTO AN EXISTING SYSTEM.
               HOWEVER, IN DOING SO, THE CHANGE OF THE EXTERNAL
               REFERENCE TO IEFVHA IN IEFVH1 WILL NOT HAVE TAKEN
               PLACE AND THE INCORPORATION OF PRIVATE PROCLIB SUPPORT
               WILL BE INCOMPLETE.  SPECIFICALLY, THE CONVERTER WILL
               FAIL IN THE MOST HORRIBLE WAY IF THE SUPERZAPS TO
               THE VARIOUS CONVERTER MODULES HAVE BEEN PUT ON.
               THE JCLIN IS INCLUDED IN THE MODIFICATION THAT
               UPDATES THE SYSGEN MACRO, SGIEF441.  A TECHNIQUE
               TO EFFECT THE CHANGE IS DESCRIBED LATER.

               THE JCLIN IS AS FOLLOWS:

               JCLIN .

              //SG15 EXEC LINKS,
              //  PARM='NCAL,LIST,XREF,LET,RENT',NAME=LPALIB
              //SYSLIN DD *
                INCLUDE AOSB3(IEFVHA)
                INCLUDE AOSB3(IEFVHC,IEFVHCB)
                INCLUDE AOSB3(IEFVINE,IEFVHM,IEFVHL)
                INCLUDE AOSB3(IEFVINA)
                INCLUDE AOSB3(IEFVIND)
                INCLUDE AOSB3(IEFVINB)
                INCLUDE AOSB3(IEFVINC)
                INCLUDE AOSB3(IEFVHEB)
                INCLUDE AOSB3(IEFNB9CR)
                INCLUDE AOSB3(IEFNB9CT)
                INCLUDE AOSB3(IEFVFA,IEFVFB)
                INCLUDE AOSB3(IEFVGM)
                INCLUDE AOSB3(IEFVHQ)
                INCLUDE AOSB3(IEFVHR)
                INCLUDE AOSB3(IEFVHF)
       ====>    CHANGE IEFVHA(IEFVPP0)
                INCLUDE AOSB3(IEFVH1)
                INCLUDE AOSB3(IEZNCODE)
                INCLUDE AOSB3(IEZDCODE)
                INCLUDE AOSB3(IEFVGM90)
       ====>    INCLUDE AOSB3(IEFVPP0)
                ENTRY IEFVH1
                NAME IEFVH1(R)
              /*

               THIS JCLIN WAS TAKEN FROM THE SYSGEN STAGE 2.  THE
               TWO STATEMENTS MARKED BY "====>" ARE THE ADDED
               STATEMENTS.  CURRENT STAGE 1 OUTPUT SHOULD BE CHECKED
               BEFORE THE JCLIN AND ANY NECESSARY CHANGED BY MADE.

               A UCLIN TO DELETE THE LMOD ENTRY MAY
               BE NECESSARY, AS SMP MAY IGNORE THE "CHANGE"
               STATEMENT FOR IEFVH1 IF IEFVH1 IS ALREADY
               DEFINED TO IT (NOT SO HOT, EH?).
               THE UCLIN MAY NOT BE NECESSARY IN MVS 3.8
               WITH SMP 4.

               UCLIN .
               DEL LMOD (IEFVH1) .
               ENDUCL .

               NEITHER THE JCLIN OR UCLIN IS NECESSARY WHEN
               INSTALLING THESE MODS PRE-GEN.

  SYSGEN MACRO
               THE SYSGEN MACRO, SGIEF441, IS MODIFIED TO CONTAIN
               THE NECESSARY LINK EDIT CONTROL STATEMENTS TO FULLY
               INCORPORATE THE MODIFICATION AND TO INSURE THAT A
               RE-SYSGEN DOES NOT "DOWN LEVEL" THE MODIFICATION.

               THE MACRO MODIFICATIONS CONSIST OF CHANGING
               THE EXTERNAL REFERENCE TO IEFVHA IN IEFVH1 TO
               REFER TO IEFVPP0, WHICH IS THE PRIVATE PROCLIB
               INITIALIZATION ENTRY POINT, AND INCLUDING MODULE
               IEFVPP, WHICH IS A NEW MODULE THAT CONTAINS ALL
               PRIVATE PROCLIB SUPPORT, EXCEPT THE SUPERZAPS THAT
               CAUSE THE VARIOUS PRIVATE PROCLIB ENTRY POINTS TO
               BE ENTERED.

  SUPERZAPS
               SUPERZAPS ARE MADE TO THE CONVERTER/INTERPRETER
               TO PERFORM THE LINKAGE TO THE PRIVATE PROCLIB
               SUPPORT AND PROVIDE THE IEFUJV INTERNAL TEXT EXIT
               FOR A JOB STREAM MANAGER.  SEE THE CO-REQUISITE
               MODIFICATIONS FOR A DESCRIPTION OF THE LOGIC
               ASSOCIATED WITH EACH OF THE MODIFICATIONS.
               THE MODULES ZAPPED ARE IEFVHF, IEFVFA, AND IEFVHE.

  NEW MODULE
               IEFVPP IS THE NEW MODULE ADDED FOR THIS SUPPORT.
               IT IS COMPATIBLE WITH ALL CURRENTLY AVAILABLE
               VERSIONS OF THE CONVERTER/INTERPRETER.
               HOWEVER, IT IS DEPENDENT ON THE EXISTANCE OR
               NON-EXISTANCE OF MVS/SE RELEASE 2.  THE SOURCE
               MODULE MUST BE ASSEMBLED WITH THE PROPER
               LEVEL OF "SYS1.AMODGEN" IN ORDER FOR IT
               TO FUNCTION PROPERLY.  CONDITIONAL ASSEMBLY IS
               USED TO IMPLEMENT THE NECESSARY LOGIC
               CHANGES AND CONTROL BLOCK DEPENDENCIES.
               CONSULT THE ASSEMBLY LISTING OF IEFVPP FOR SPECIFIC
               INFORMATION ON ITS FUNCTIONS AND LOGIC.

               THE SU MACRO FOR MVS/SE RELEASE 2 (IHASU74)
               IS USED TO EFFECT THE CONDITIONAL ASSEMBLY.
               IT IS ASSUMED THAT ALL RELEASES OF MVS/SP
               WILL PROVIDE THIS MACRO WITH THE SU BIT
               TURNED ON.  IF NOT, THEN THE SOURCE OF IEFVPP
               WILL HAVE TO BE UPDATED ACCORDINGLY.

               IT IS RECOMMENDED THAT THE IEFVPP SOURCE MODULE
               BE ASSEMBLED BY THE INSTALLATION AND THAT THE
               OBJECT DECK PROVIDED IN THE MODIFICATION BE
               REPLACED WITH THE ONE CREATED BY THE ASSEMBLY.

  JOB ENTRY SUBSYSTEM SUPPORT
               JES2 IS FULLY SUPPORTED AS THE DYNAMIC PROCLIB
               CODE IS NOT SENSITIVE TO THE LEVEL OF JES THAT
               IS RUNNING.  HOWEVER, IT MAY HAVE SOME PROBLEMS
               IN A JES3 ENVIRONMENT, WHERE PERFORMING THE
               DYNAMIC ALLOCATION OF PROCLIBS IS NOT SUBJECT
               TO JES3 SETUP CONTROL.  NO ATTEMPT HAS BEEN
               MADE BY THE AUTHORS TO RUN IT IN A JES3 ENVIRONMENT.

               THIS CODE FULLY SUPPORTS THE USE OF DYNAMIC
               PROCLIB(S) BY STARTED TASKS AND TSO USERS.

               THE ABILITY TO REQUEUE A JOB FOR RECONVERSION WHEN
               A PROCLIB VOLUME IS NOT AVAILABLE HAS BEEN REMOVED
               FROM THIS VERSION OF IEFVPP.  IT WAS THOUGHT TO BE
               OF MINIMAL VALUE AND FREQUENTLY LEFT JOBS AWAITING
               CONVERSION FOR LONGS PERIODS OF TIME.

  MSS SUPPORT
               A BY-PRODUCT OF THE MSS IS THAT AN MSS VOLUME
               CONTAINING A PROCLIB MAY NOT BE MOUNTED.  SINCE
               IEFVPP PERFORMS ALL LOCATES AND ALLOCATIONS
               REQUESTING NO MOUNTING, SUCH A PROCLIB WILL
               NOT BE FOUND, AND THE JOB WILL FAILED WITH A
               JCL ERROR.  HOWEVER, EVEN IF THE MSS VOLUME
               CONTAING THE PROCLIB IS MOUNTED, IEFVPP WILL
               BE STILL INDICATE A JCL ERROR, AS THERE COULD
               BE SUBSTANTIAL DELAYS IN THE CONVERTER
               IF IT HAS TO WAIT FOR STAGING OR CYLINDER
               FAULTS.  SINCE THE CONVERTER IS A SERIAL
               PROCESS, THIS CAN CAUSE SERIOUS DELAYS
               IN JOB PROCESSING, MOST NOTICABLE IN TSO
               LOGONS (BEING STACKED BEHIND A JOB STUCK
               CYLINDER FAULTING IN THE CONVERTER).

  INSTALLATION
               AN APPROACH TO PUTTING THE PRIVATE PROCLIB SUPPORT
               ON IS:
               1.  APPLY THE SYSGEN MACRO UPDATE AND THE NEW
                   MODULE MODIFICATION.  AT THIS POINT, THE
                   JCLIN WILL HAVE BEEN DONE AND THE NEW MODULE
                   (IEFVPP) WILL HAVE BEEN LINKED INTO THE
                   CONVERTER, BUT DYNAMIC PROCLIB WILL NOT
                   BE FUNCTIONING (THE CONVERTER WILL
                   STILL FUNCTION, HOWEVER).
               2.  CREATE A "DUMMY" SUPERZAP FOR MODULE IEFVH1.
                   APPLY THIS ZAP TO IEFVH1.  THIS WILL STILL NOT
                   CAUSE DYNMAIC PROCLIB TO WORK, AS THE
                   EXTERNAL REFERENCE TO IEFVH1 POINTING TO IEFVHA
                   WILL NOT HAVE BEEN CHANGED.
               3.  RESTORE THE ZAP TO IEFVH1.  THIS WILL CAUSE A
                   RE-LINK OF IEFVH1 AND SMP WILL INSERT THE CHANGE
                   STATEMENT, CAUSING IEFVH1 TO NOW POINT TO
                   IEFVPP0 IN IEFVPP.
               4.  APPLY THE ZAPS TO IEFVHF, IEFVHE, AND IEFVFA.
               5.  PLACE IEFVH1 AND IEFNB903 ON THE MLPA FOR TESTING.
                   CLPA WHEN READY.  IEFVH1 COULD BE PLACED IN THE
                   STEPLIB USED TO RUN JES2, IF ANY.

               SHOULD IT BE NECESSARY TO RESTORE THE CONVERTER TO ITS
               ORIGINAL STATE, THE FOLLOWING COULD BE USED:
               1.  PERFORM AN SMP RESTORE OF ALL FIVE MODIFICATIONS.
                   THIS ASSUMES THAT THE SAVED CDS WILL BE USED
                   TO RESTORE THE ORIGINAL JCLIN FOR IEFVH1.
                   THE SAME TRICK USED TO FORCE AN INCLUDE OF IEFVH1
                   FROM THE DLIB (THIS TIME WITHOUT THE CHANGE
                   STATEMENT WILL HAVE TO BE DONE).
                   THE IEFVPP CSECT WILL REMAIN BEHIND IN THE
                   IEFVH1 LOAD MODULE, BUT THIS WILL CAUSE NO HARM.
               2.  REMOVE THE MLPA OR CLPA, IF NECESSARY.

  PTF LEVEL
               PTF LEVEL FOR THE INDIVIDUAL MODULES IS NOTED
               WITH EACH MODULE.  SOME OF THE CONVERTER
               MODULES WERE STRUCK BY MVS/SE AND MVS/SP, BUT THE CODE
               IS BASICALLY COMPATIBLE WITH ALL SU/PTF COMBINATIONS,
               BUT THE ZAP DISPLACEMENTS AND PATCH AREAS CHANGE.

  CURRENT RESTRICTIONS
               THE LIMIT ON THE NUMBER OF MULTIPLE CONCURRENT
               CONVERTERS IS 16.  ANYONE WHO HAS A PROBLEM WITH
               THIS DESERVES IT.

                                                                */  .
++ MOD   (IEFVPP)  DISTLIB (AOSB3)
                   LMOD (IEFVH1)
                   LEPARM (RENT,REUS,REFR) .
><
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(NEW,PASS),UNIT=SYSDA,
//             SPACE=(CYL,(5,5)),
//             DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=4080)
//SYSIN    DD  *
//*
//STEP2   EXEC PGM=IFOX00,PARM='OBJECT,NODECK,NOTERM,XREF(SHORT),RENT'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,10)
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,10)
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,10)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.SMPMTS,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS1.APVTMACS,DISP=SHR
//SYSGO    DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  *
VPP      TITLE 'IEFVPP - AMDAHL MVS PRIVATE PROCLIB SUPPORT'
***********************************************************************
*                                                                     *
*        IEFVPP                                                       *
*        ******                                                       *
*                                                                     *
*        VERSION 4, RELEASE 1, MODIFICATION 0                         *
*        ******* ** ******* ** ************ *                         *
*                                                                     *
* MODIFIED TO SUPPORT MVS3.8J TURNKEY 3 SYSTEM 2/17/03                *
* THIS VERSION WAS MODIFIED 02/17/03 BY BRIAN WESTERMAN               *
*                                       SYZYGY INCORPORTED            *
*                                       897 OAK PARK BLVD - 500       *
*                                       PISMO BEACH, CA 93449         *
*                             PHONE (800) 767-2244 FAX (800) 366-4082 *
*                                                                     *
* THIS CSECT CONTAINS THE PRIVATE PROCEDURE LIBRARY SUPPORT ROUTINES. *
*                                                                     *
* THIS PROGRAM IS A REWRITE OF THE USER PROCLIB SUPPORT PROGRAM       *
* UPROCRDR, WHICH HAD BEEN IN USE AT THE MUTUAL LIFE OF CANADA ON     *
* OS/MVT SINCE MARCH OF 1972.                                         *
*                                                                     *
* THIS CODE HAS BEEN SUBSTANTIALLY MODIFIED BY THE AMDAHL CORPORATE   *
* COMPUTER CENTER TO SIMPLIFY THE MODIFICATIONS NECESSARY TO          *
* IBM CODE IN ORDER TO IMPLEMENT AND INSTALL THIS CODE.               *
* ADDITIONAL FEATURES ADDED ARE CLEANUP RECOVERY, MULTIPLE            *
* CONVERTER SUPPORT, MVS/SE RELEASE 2 COMPATIBILITY, AND MVS/SP       *
* VERSION 1, RELEASE 3.2 SUPPORT.  IT IS ANTICIPATED THAT ONLY        *
* MINIMAL CHANGES WILL BE NECESSARY FOR MVS/SP VERSION 2 (MVS/XA).    *
*                                                                     *
***********************************************************************
         TITLE '**** PRIVATE PROCLIB LINKAGE REQUIREMENTS ****'
***********************************************************************
*                                                                     *
* ENTRIES:                                                            *
*        IEFVPP0 - (FROM IEFVH1)  - INITIALIZATION PROCESSING         *
*        IEFVPP1 - (FROM IEFVFA)  - //JOBPROC VALIDATE & ALLOCATE     *
*        IEFVPP2 - (FROM IEFVFA)  - JOBPROC CONCATENATE & OPEN        *
*        IEFVPP3 - (FROM IEFVHF)  - CLEANUP AT JOB PROCESSING END     *
*        IEFVPP4 - (FROM IEFVFA)  - COMBINED LINKAGE FROM IEFVFA      *
*                                   TO IEFVPP1 AND IEFVPP2            *
*        IEFVPP5 - (FROM IEFVFA)  - INTERFACE TO IEFUJV FOR THE       *
*                                   INTERNAL TEXT EXIT (TYPE 64)      *
*        IEFVPPM - (FROM IEFVPP)  - LOCAL VERSION OF IEFVGM TO        *
*                                   ELIMINATE DEPENDENCE ON IBM CODE  *
*                                                                     *
* ATTRIBUTES:                                                         *
*        REENTRANT, REFRESHABLE.                                      *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXITS:                                                              *
*        ALWAYS RETURN TO CALLER WITH ALL REGS EXCEPT R15 RESTORED.   *
*        IEFVPP0 BRANCHES TO IEFVHA AND DOES NOT RETURN TO CALLER.    *
*        IEFVPP5 BRANCHES TO IEFUJV WHICH WILL DO THE RETURN.         *
*        IN AN MVS/SE2 ENVIRONMENT, IEFVPP5 RETURNS TO CALLER.        *
*        SEE COMMENTS AT EACH INDIVIDUAL ENTRY POINT FOR MORE         *
*        INFORMATION.                                                 *
*                                                                     *
***********************************************************************
         TITLE '**** PRIVATE PROCLIB INSTALLATION NOTES ****'
***********************************************************************
*                                                                     *
*     1) ALL ENTRY POINTS EXCEPT IEFVPP0 ARE ENTERED VIA A STANDARD   *
*           OS/VS CALL "MACRO".  IEFVPP0 IS ENTERED BY CHANGING       *
*           THE BRANCH TO IEFVHA FROM IEFVH1 VIA THE LINKAGE EDITOR.  *
*           ALL ENTRIES (EXCEPT IEFVPP5) TEST THE "PPINITC"           *
*           SWITCH IN THE PRIVATE PROCLIB WORK AREA BEFORE            *
*           PERFORMING ANY PROCESSING.                                *
*                                                                     *
*           THE USER PROCLIB SUPPORT CODE CAN THEREFORE BE DISABLED   *
*           BY SIMPLY ALTERING IEFVPP0 NOT TO TURN ON "PPINITC".      *
*           IEFVPP0 MUST ALWAYS BE ENTERED AS IT BUILDS THE ENTRY     *
*           POINTS FOR ALL THE OTHER ROUTINES IN THE CONVERTER        *
*           WORK AREA.                                                *
*                                                                     *
*     2) ALL ROUTINES EXPECT R12 TO PERMANENTLY ADDRESS THE CWA.      *
*                                                                     *
*     3) A RESERVED FULL WORD MUST BE AVAILABLE IN THE CWA TO         *
*           ANCHOR THE ADDRESS OF THE PRIVATE PROCLIB WORK AREA.      *
*           THE NAME OF THIS FIELD IS DEFINED BY THE &ANCHOR SET      *
*           SYMBOL IN THIS PROGRAM.                                   *
*                                                                     *
*     4) STANDARD CONVERTER LINKAGE AND SAVING CONVENTIONS ARE        *
*           OBEYED AT ALL PRIVATE PROCLIB ENTRY POINTS.  THE          *
*           SAVE AREA FOR ALL OF THESE ENTRY POINTS IS CONTAINED      *
*           WITHIN THE PRIVATE PROCLIB EXTENSION TO THE CONVERTER     *
*           WORK AREA.                                                *
*                                                                     *
*     5) ANY ERROR NOTED DURING CLEANUP OPERATIONS WILL CAUSE THE     *
*           CONVERTER TO ABORT.                                       *
*                                                                     *
*     6) SEE THE SECTION OF COMMENTS ON CONVERTER MODIFICATION FOR    *
*           PARTICULARS ON THE CHANGES TO THE CONVERTER/INTERPRETER.  *
*                                                                     *
*     7) TO PROPERLY ASSEMBLE THIS MODULE, THE CONVERTER/INTERPRETER  *
*           WORK AREA MACRO, IEFCVRWA, MUST BE AVAILABLE.  IBM        *
*           DISTRIBUTES THIS MACRO IN APVTMACS.                       *
*                                                                     *
*     8) IN ORDER TO SUPPORT MULTIPLE CONVERTER, AN ENQUEUE WITH A    *
*           MAJOR NAME OF SYSJPROC AND MINOR NAME OF IEF*PDS0         *
*           (* = 0-9,A-F).  THIS IS A STEP LEVEL ENQUEUE THAT MUST    *
*           NOT BE OTHERWISE USED BY THE ADDRESS SPACE IN WHICH       *
*           CONVERTER IS RUNNING IN.  THIS INCLUDES THE MASTER        *
*           ADDRESS SPACE AND ANY NON-JES (I.E., STARTING OF A        *
*           SECONDARY SUBSYSTEM) USE OF THE CONVERTER.                *
*                                                                     *
*     9) THE MESSAGE NUMBER PREFIX IS SET BY THE SET SYMBOL           *
*           &MSGPFX.  ITS DEFINITION FOLLOWS THE LOCAL MACRO          *
*           DEFINITIONS.                                              *
*                                                                     *
*    10) THE INTERNAL TEXT ENTRY INTO IEFUJV IS ENABLED OR DISABLED   *
*           BY THE USE OF THE &INTEXTX SET SYMBOL IN THIS PROGRAM.    *
*                                                                     *
*    11) DISABLING OF THIS CODE CAN BE DONE BY ZAPPING THE            *
*           INSTRUCTION AT "PPCANCEL".  THE DEBUGGING CODE CAN BE     *
*           ENABLED BY ZAPPING THE INSTRUCTION AT "PPSETBUG".         *
*                                                                     *
*    12) THIS MODULE MUST BE ASSEMBLED WITH THE PROPER LEVEL          *
*           OF "SYS1.AMODGEN" CORRESPONDING TO THE EXISTANCE          *
*           OR NON-EXISTANCE OF MVS/SE RELEASE 2, MVS/SP              *
*           VERSION 1, RELEASE 3.2, OR LATER.                         *
*           FOR HERCULES, THE SPLEVEL STATMENT MUST BE COMMENTED OUT  *
*                                                                     *
* COPYRIGHT:                                                          *
*        SYZYGY INCORPORATED 2003.                                    *
*        THIS COPYRIGHT IS NOT INTENDED TO LIMIT THE USE OF THIS      *
*        CODE BY THE RECEIVING INSTALLATION, BUT TO PREVENT ITS       *
*        REDISTRIBUTION FOR PROFIT.                                   *
* COPYRIGHT:                                                          *
*        AMDAHL CORPORATION, 1978, 1981, 1982.                        *
*        THIS COPYRIGHT IS NOT INTENDED TO LIMIT THE USE OF THIS      *
*        CODE BY THE RECEIVING INSTALLATION, BUT TO PREVENT ITS       *
*        REDISTRIBUTION FOR PROFIT.                                   *
*                                                                     *
***********************************************************************
         TITLE '**** PRIVATE PROCLIB SUMMARY OF AMENDMENTS ****'
***********************************************************************
*                                                                     *
*        VERSION 4, RELEASE 1, MODIFICATION 0                         *
*                                                                     *
*        THESE CHANGES ARE FOR THE HERCULES TURNKEY 3 IMPLEMENTATION  *
*        THE SPLEVEL STATEMENT IS PERMAENTLY REMOVED                  *
*        THE LINK TO INTERNAL TEXT EXIT IS DUMMYED OUT                *
*        ALL SUPERZAPS HAVE CHANGED AND ARE DOCUMENTED IN THE ZAPS    *
*        REFERENCES TO SMPE SHOULD NOW SAY SMP4 ONLY                  *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*        VERSION 3, RELEASE 1, MODIFICATION 0                         *
*                                                                     *
*        THE PRIVATE PROCLIB WORK AREA IS NO LONGER APPENDED          *
*        TO THE END OF THE CONVERTER WORK AREA.  IT IS SEPARATELY     *
*        GETMAINED AND IS ANCHORED IN A RESERVED WORD IN THE          *
*        CONVERTER WORK AREA.  THE NAME OF THIS FULL WORD CAN BE      *
*        SPECIFIED USING THE &ANCHOR SETC SYMBOL IN THIS PROGRAM.     *
*        AS DISTRIBUTED, THIS DEFAULTS TO "RFULLE", WHICH HAPPENS     *
*        TO BE THE LAST RESERVED FULL WORD IN THE CONVERTER           *
*        WORK AREA.                                                   *
*                                                                     *
*        BECAUSE OF THE ABOVE CHANGE, THE SUPERZAP MODIFICATION       *
*        TO MODULE IEFVH1 IS NO LONGER NECESSARY.  THE THREE          *
*        REMAINING SUPERZAPS ARE STILL NECESSARY AND THE ZAPS         *
*        TO IEFVFA AND IEFVHF ARE DEPENDENT ON THE FIELD CHOSEN       *
*        TO ANCHOR THE PRIVATE PROCLIB WORK AREA IN THE CON-          *
*        VERTER WORK AREA.                                            *
*                                                                     *
*        THE ABILITY TO "REQUEUE" A JOB WHEN THE PROCLIB VOLUME       *
*        OR CATALOG VOLUME HAS BEEN REMOVED.  IN ADDITION, CODE       *
*        HAS BEEN ADDED TO EXPLICITLY DISALLOW PROCLIBS ON MSS        *
*        VOLUMES.  THIS IS BECAUSE THE CONVERTER IS A SERIAL          *
*        PROCESS AND THE DELAY THAT COULD BE INTRODUCED BY MSS        *
*        STAGING COULD BE SUBSTANTIAL.                                *
*                                                                     *
*        THE GENERATION OF CODE FOR THE INTERNAL TEXT EXIT TO         *
*        IEFUJV HAS BEEN MADE OPTIONAL THROUGH THE USE OF THE         *
*        &INTEXTX SETB SYMBOL.  IF THIS IS SET TO ZERO (0), NO        *
*        INTERNAL TEXT EXIT TO IEFUJV WILL BE TAKEN.  IF SET          *
*        TO ONE (1), CODE WILL BE GENERATED TO ENTER IEFUJV           *
*        WITH ENTRY CODE 64 FOR EACH INTERNAL TEXT RECORD.            *
*        THE EXIT LINKAGE WILL DIFFER DEPENDING ON WHETHER OR         *
*        NOT AND MVS/SE RELEASE 2 (OR LATER) SYSTEM IS BEING USED.    *
*        THE DEFAULT AS DISTRIBUTED IS ZERO (0), CAUSING NO           *
*        INTERNAL TEXT TEXT TO BE TAKEN.                              *
*                                                                     *
***********************************************************************
         TITLE '**** PRIVATE PROCLIB USER EXTERNALS ****'
***********************************************************************
*                                                                     *
*        THE USER WISHING TO SUPPLY A PRIVATE PROCLIB NEED ONLY       *
*        CODE ONE OR MORE DD STATEMENTS (FOLLOWING THE NORMAL         *
*        RULES OF CONCATENATION) WITH THE DD NAME OF "JOBPROC".       *
*        THE JOBPROC DD MUST APPEAR BEFORE THE FIRST EXEC STATE-      *
*        MENT OF A JOB.  THE ONLY JCL KEYWORD PARAMETERS SUPPORTED    *
*        ARE DSN, DISP, UNIT, VOL=SER, VOL=REF, AND SYSPROC.  USE     *
*        OF OTHER JCL PARAMETERS MAY CAUSE ERRORS OR MAY BE IGNORED.  *
*        DSN IS MANDATORY.  DISP CAN ONLY BE "SHR".  UNIT AND VOL     *
*        ARE NECESSARY ONLY IF THE DATA SET IS NOT CATALOGUED (VIA    *
*        MASTER AND ALIASES - JOBCAT IS NOT USED).  SYSPROC INDICATES *
*        THE CONCATENATION (SYSPROC=YES) OR NON-CONCATENATION         *
*        (SYSPROC=NO) OF THE SYSTEM PROCLIBS WITH THE USER SUPPLIED   *
*        PROCLIBS.  THE DEFAULT IS SYSPROC=YES.                       *
*                                                                     *
***********************************************************************
         TITLE '**** PRIVATE PROCLIB INSTALLATION EXTERNALS ****'
***********************************************************************
*                                                                     *
*        A SERIES OF SPECIAL DD NAMES ARE USED BY DYNAMIC PROCLIB     *
*        AND ARE RESERVED FOR ITS USE.  SOME OF THESE NAMES ARE       *
*        CREATED THROUGH THE USE OF DYNAMIC ALLOCATION.  OTHERS       *
*        MUST BE SUPPLIED IN THE JCL PROCEDURE THAT IS USED TO        *
*        START THE ADDRESS SPACE (NORMALLY JES) THAT THE CONVERTER    *
*        IS RUNNING IN.  NO SPECIAL DD STATEMENTS ARE NECESSARY       *
*        FOR EITHER THE INTERPRETER OR INITIATOR.  INVOCATION         *
*        OF THE CONVERTER USING THE MASTER ADDRESS SPACE'S TIOT       *
*        (JOBNAME MSTRJCL) FOR SUCH THINGS AS SUBSYSTEM START         *
*        DOES NOT REQUIRE THESE SPECIAL DDNAMES AND SYSPROC IS        *
*        SET TO BE UNAVAILABLE (WITHOUT ERROR MESSAGE).               *
*                                                                     *
*        AS MANY SETS OF SPECIAL DDNAMES ARE NEEDED AS THERE ARE      *
*        CONVERTERS RUNNING.  IF THERE ARE MORE CONVERTER SUBTASKS    *
*        RUNNING THEN THERE ARE SETS OF DDNAMES, THE EXCESS           *
*        CONVERTERS WILL NOT PROCESS DYNAMIC PROCLIB REQUESTS AND     *
*        ANY JOBS PROCESSED BY THEM WILL IN ALL PROBABILITY RECEIVE   *
*        JCL ERRORS.                                                  *
*                                                                     *
*        EACH SPECIAL DD NAME IS OF THE FORM IEF#PDS*.  THE POUND     *
*        SIGN (#) IS COMMON TO ALL DDNAMES IN A GIVEN SET AND IS      *
*        A SINGLE HEX DIGIT (0-9,A-F).  THE SETS MUST BE NUMBERED     *
*        CONSECUTIVELY (I.E., IF FOUR SETS ARE BEING USED, THEY       *
*        MUST BE 0, 1, 2, AND 3).  THE ASTERISK (*) IS USED TO        *
*        UNIQUELY ALLOCATE EACH USER SUPPLIED PROCLIB AND THE         *
*        INSTALLATION SUPPLIED PROCLIB(S).  THE USER SUPPLIED         *
*        PROCLIBS WILL BE ALLOCATED WITH THE DDNAME IEF#PDS0,         *
*        IEF#PDS1, ..., IEF#PDSF (0-9,A-F).  THE INSTALLAION          *
*        SUPPLIED PROCLIB MUST BE IEF#PDSI.  FOR EXAMPLE, THE         *
*        "FIRST" CONVERTER RUNNING WILL NEED AN INSTALLATION          *
*        SUPPLIED PROCLIB NAME OF IEF0PDSI AND WILL GENERATE          *
*        DDNAMES IEF0PDS0, IEF0PDS1, ..., IEF0PDSF.                   *
*                                                                     *
*        THE INSTALLATION OF THE IEF#PDSI DD STATEMENTS IN THE        *
*        JES PROCEDURE DOES NOT ELIMINATE THE NEED FOR THE            *
*        PROC00, PROC01, ETC. DD STATEMENTS IN THE JES2 PROCEDURE     *
*        AND WHATEVER IS THE SPECIFIED NAMES IN JES3.  THESE DATA     *
*        SETS ARE USED WHENEVER A USER DOES NOT CODE A "JOBPROC"      *
*        DD STATEMENT IN HIS OR HER JOB.                              *
*                                                                     *
***********************************************************************
         TITLE '**** CONVERTER/INTERPRETER MODIFICATIONS ****'
***********************************************************************
*                                                                     *
*        CONVERTER/INTERPRETER MODIFICATIONS                          *
*        ********************* *************                          *
*                                                                     *
*        THERE ARE THREE AREAS OF CHANGES TO THE CONVERTER/           *
*        INTERPRETER NECESSARY TO PROPERLY IMPLEMENT PRIVATE PROCLIB  *
*        SUPPORT IN MVS.  THE THREE AREAS ARE SYSGEN MACROS (WHICH    *
*        INCLUDES LINK EDIT CHANGES TO THE CONVERTER), A NEW MODULE   *
*        (IEFVPP), AND SUPERZAPS TO CONVERTER/INTERPRETER MODULES.    *
*        IN ADDITION, IF THESE MODIFICATIONS ARE INSTALLED ONTO       *
*        AN EXISTING SYSTEM, A UCLIN AND JCLIN WILL BE NECESSARY.     *
*                                                                     *
*        THE CHANGES TO SYSGEN ARE ALL CONTAINED IN MACRO SGIEF441.   *
*        THIS MACRO CONTAINS THE LINK EDIT CONTROL STATEMENTS FOR     *
*        THE CONVERTER.  THE CHANGES ARE A CHANGE STATEMENT TO ALTER  *
*        THE EXTERNAL REFERENCE TO IEFVHA IN IEFVH1 TO REFER TO       *
*        IEFVPP0 IN IEFVPP AND AN INCLUDE OF THE NEW MODULE IEFVPP.   *
*                                                                     *
*        THE CHANGE STATEMENT:                                        *
*        PUNCH ' CHANGE IEFVHA(IEFVPP0) '                             *
*        IS INSERTED IMMEDIATELY BEFORE THE PUNCH INCLUDE OF IEFVH1.  *
*                                                                     *
*        THE INCLUDE STATEMENT:                                       *
*        PUNCH ' INCLUDE AOSB3(IEFVPP) '                              *
*        IS INSERTED IMMEDIATELY BEFORE THE PUNCH ENTRY IEFVH1.       *
*                                                                     *
*        VALID STATEMENT NUMBERS FOR MVS 3.8 THROUGH MVS/SP 1.3.2     *
*        ARE 38907099 FOR THE CHANGE AND 38972099 FOR THE INCLUDE.    *
*        THESE SHOULD BE VERIFIED BEFORE ACTUALLY UPDATING THE MACRO. *
*                                                                     *
*        THE NEW MODULE, IEFVPP IS LINK EDITTED AS PART OF IEFVH1,    *
*        THE JCL CONVERTER.  THE LINK EDIT CHANGE STATEMENT CAUSES    *
*        IT TO BE INVOKED TO INITIALIZE THE EXPANDED CONVERTER        *
*        WORK AREA.  ADDRESSES INSERTED INTO THIS WORK AREA ARE USED  *
*        BY THE SUPERZAPS DESCRIBED BELOW TO PASS CONTROL TO IEFVPP   *
*        AT ALL THE RIGHT MOMENTS.                                    *
*                                                                     *
*        THE SUPERZAPS ARE TO MODULES IEFVHF AND IEFVFA IN LOAD       *
*        MODULE IEFVH1 AND TO MODULE IEFVHE IN LOAD MODULE            *
*        IEFNB903.  THE ZAPS TO IEFVHF AND IEFVFA ARE QUITE           *
*        CRUTIAL TO THE PROPER OPERATION OF THE CONVERTER.            *
*        THE ZAP TO IEFVHE (IN THE INTERPRETER) DOES NOT PREVENT      *
*        THE PROPER OPERATION OF IEFVPP AND CAN BE INSTALLED IN       *
*        ADVANCE OF THE REMAINING ZAPS.                               *
*                                                                     *
*        THE ZAP FOR IEFVHE IS TO HAVE THE INTERPRETER BYPASS         *
*        INTERNAL TEXT DD STATEMENTS THAT HAVE THE JOBPROC FLAG,      *
*        JPROCSTR, TURNED ON.  THIS FLAG IS SET BY IEFVPP TO          *
*        INDICATE A JOBPROC DD STATEMENT.  THE PURPOSE OF THIS        *
*        IS TO INSURE THAT THE INTERPRETER STATEMENT NUMBERING        *
*        AGREES WITH THAT DONE BY THE CONVERTER AND THAT IT           *
*        DOESN'T ATTEMPT TO PROCESS A JOBPROC STATEMENT DURING        *
*        INTERPRETATION, AS IT WOULDN'T KNOW WHAT TO DO WITH IT.      *
*                                                                     *
*        THE ZAP TO IEFVFA HAS TWO SEPARATE PURPOSES.  ONE IS TO      *
*        ALTER A SUITABLY UNUSED JCL KEYWORD (HOW'S "SUBALLOC"        *
*        GRAB YOU?) TO BECOME THE "SYSPROC" KEYWORD.  THE OTHER       *
*        PURPOSE IS TO CAUSE ENTRY TO IEFVPP4 AND IEFVPP5 IN IEFVPP   *
*        IMMEDIATELY AFTER A JCL STATEMENT HAS BEEN CONVERTED INTO    *
*        INTERNAL TEXT.  THE EXIT TO IEFVPP5 IS NOT NECESSARY FOR     *
*        PRIVATE PROCLIB SUPPORT, BUT PROVIDES AN INTERNAL TEXT       *
*        EXIT TO IEFUJV.                                              *
*                                                                     *
*        THE ZAP TO IEFVHF CAUSES ENTRY TO IEFVPP3 IN IEFVPP DURING   *
*        CONVERTER TERMINATION, SO THAT ANY ALLOCATED PROCLIBS        *
*        CAN BE CLOSED, DECONCATENATED, AND DEALLOCATED.              *
*                                                                     *
*        SINCE THE ACTUAL TEXT OF THESE ZAPS WILL DEPEND ON THE       *
*        CURRENT MAINTENANCE LEVELS OF THE VARIOUS MODULES, THEY ARE  *
*        NOT ENUMERATED HERE.  THE PTF TO APPLY THESE ZAPS SHOULD     *
*        BE EXAMINED, AS WELL AS THE SOURCE MICROFICHE OR OPTIONAL    *
*        MATERIALS.                                                   *
*                                                                     *
***********************************************************************
         TITLE '**** STATEMENT OF SUPPORT ****'
***********************************************************************
*                                                                     *
*        STATEMENT OF SUPPORT                                         *
*        ********* ** *******                                         *
*                                                                     *
*        THIS PROGRAM IS MAINTAINED BY BRIAN WESTERMAN FOR THE        *
*        HERCULES TURNKEY MVS 3.8J SYSTEM AND IS TESTED TO RUN        *
*        ON THE TURNKEY #3 SYSTEM AS OF FEB. 2003.                    *
*                                                                     *
*        ADDITIONAL INFORMATION CONCERNING THESE MODIFICATIONS        *
*        CAN BE OBTAINED BY CONTACTING:                               *
*                                                                     *
*        BRIAN WESTERMAN                                              *
*        SYZYGY INCORPORATED            SYZYGY INCORPORATED           *
*        897 OAK PARK BLVD - 500        1381 KILDAIRE FARM RD - 326   *
*        PISMO BEACH, CA 93449          CARY, NC 27511-5525           *
*        PHONE:  (800) 767-2244   FAX (800) 366-4082                  *
*        EMAIL  BRIAN_WESTERMAN@SYZYGYINC.COM                         *
***********************************************************************
         TITLE '**** PRIVATE MACRO INSTRUCTIONS ****'
         MACRO
&L       PPEPA &TRACEI,&ERREXIT=NO
         GBLC  &ANCHOR
         LCLC  &P,&O,&C
&P       SETC  '&TRACEI'(2,3)
&C       SETC  '     INDICATE IEF'
&C       SETC  '&C&TRACEI'
&O       SETC  ' IS EXECUTING'
&C       SETC  '&C&O'
&O       SETC  'PPSTAT0,'          SET BASIC OPERAND TEXT
&O       SETC  '&O.&P.CODE&C'      ADD IN COMMENT
&L       DS    0H                  ENTRY POINT ADDRESS
         USING *,R15               TEMPORARY BASE
*
         STM   R14,R12,12(R13)     SAVE CALLERS REGS
         L     R11,PPBASE          SET UP MODULE BASE
         LA    R7,2048             SET UP SECOND
         LA    R7,2048(R7,R11)      BASE REGISTER
         USING IEFVPP,R11,R7       INFORM ASSEMBLER OF BASE REG
         DROP  R15                 DROP TEMPORARY BASE
*
         AIF   ('&TRACEI' EQ 'VPP0').PP0CALL
         L     R6,&ANCHOR          LOAD PP WORK AREA ADDRESS
         TM    PPSTAT0,PPINITC     HAS IEFVPP0 BEEN CALLED ?
         BO    *+10                YES, CONTINUE
         LM    R14,R12,12(R13)     NO, RELOAD REGISTERS
         BR    R14                 AND RETURN IMMEDIATELY
         OI    &O
         AGO   .PPCOMM
.PP0CALL ANOP  ,
         LA    R0,PPWORKLN         LENGTH OF PROCLIB WORK AREA
         GETMAIN R,LV=(0)          GET THE PRIVATE PROCLIB WORK AREA
         LR    R6,R1               MOVE TO ITS PERMANENT HOME
         ST    R6,&ANCHOR          STORE IN CONVERTER WORK AREA
*
         LR    R0,R1               SET FOR MVCL
         LA    R1,PPWORKLN         LENGTH OF MVCL
         SLR   R15,R15             ZERO FROM LENGTH
         MVCL  R0,R14              CLEAR THE WORK AREA
*
         MVI   &O
.PPCOMM  ANOP  ,
*
         L     R15,&P.TRCV         LOAD CONVERTER TRACE ROUTINE ADDRESS
         CNOP  2,4                 ALIGN TRACE PARM LIST
         BALR  R14,R15             ENTER MODULE ID IN TRACE REC.
&P.TRCV  DC    V(TRACE)            TRACE ROUTINE ID (IN IEFVH1)
         DC    CL4'&TRACEI'        MODULE ID FOR TRACE RECORD
*
         XC    PPSAVE(72),PPSAVE   CLEAR SAVE AREA
         ST    R13,PPSAVE+4        STORE BACKWARD POINTER
         LA    R1,PPSAVE           POINT AT SAVE AREA
         ST    R1,8(,R13)          STORE FORWARD POINTER
         LR    R13,R1              SET NEW SAVE AREA ADDRESS
         AIF   ('&ERREXIT' EQ 'YES').PPERR02
*
         MVC   PPESTAEP(PPESTAEL),PPESTAEM MOVE IN MODEL PLIST
         ESTAE PPESTAEX,CT,        ESTABLISH PRIVATE PROCLIB           X
               TERM=YES,PARAM=(12), ERROR RECOVERY                     X
               MF=(E,PPESTAEP)       ENVIRONMMENT
         OI    PPMISC,PPESTAE      INDICATE ESTAE IN EFFECT
.PPERR02 ANOP  ,
         MEND
*
         MACRO
         IEFVPPMS &MNO,&TEXT
         LCLA  &LTH
         LCLC  &STR
         GBLC  &MSGPFX
         AIF   (T'&TEXT EQ 'O').VGMER01
         AIF   (T'&MNO EQ 'O').VGMER02
&LTH     SETA  K'&TEXT-2           GET LENGTH OF MESSAGE TEXT
&STR     SETC  '&TEXT'(2,&LTH)     EXTRACT MESSAGE TEXT
&LTH     SETA  &LTH+8              CALCULATE TOTAL MESSAGE LENGTH
MSG&MNO  DC    AL1(0)              RESERVED
         DC    AL1(&LTH)           LENGTH OF MESSAGE TEXT
         DC    CL8'&MSGPFX&MNO'    EXTERNAL MESSAGE IDENTIFICATION
         DC    C'&STR'
         SPACE 2
         MEXIT ,
.VGMER01 MNOTE 12,'*** MESSAGE TEXT REQUIRED BUT NOT SPECIFIED ***'
         MEXIT ,
.VGMER02 MNOTE 12,'*** MESSAGE NUMBER REQUIRED BUT NOT SPECIFIED ***'
         MEND
*
         MACRO
&NAME    PPDEBUG &WHERE
&NAME    BAL   R14,IEFVPPDB        GO TO DEBUGGING TRACER
         DC    CL4'&WHERE'         INDICATE WHERE WE ARE
         MEND
*
         TITLE '**** SYMBOLIC PARAMETER DEFINITIONS ****'
***********************************************************************
*                                                                     *
*        THE SYMBOLIC PARAMETERS THAT CAN BE USED TO ALTER THE        *
*        CHARACTERISTICS OF THIS MODULE ARE DEFINED HERE.             *
*                                                                     *
***********************************************************************
*
         GBLB  &INTEXTX
         GBLB  &SGIHASU(100)
*
         GBLC  &ANCHOR
         GBLC  &MSGPFX
*
***********************************************************************
*                                                                     *
*        INTERNAL TEXT EXIT                                           *
*                                                                     *
*        THIS PARAMETER SHOULD BE SET TO EITHER 1 (ONE) OR 0 (ZERO)   *
*        TO GENERATE OR NOT GENERATE SUPPORT FOR THE INTERNAL         *
*        TEXT EXIT TO IEFUJV.                                         *
*        THIS FIELD MUST BE A FULL WORD THAT IS UNUSED BY THE         *
*        CONVERTER.                                                   *
*                                                                     *
***********************************************************************
*
&INTEXTX SETB  0                   WE DON'T WANT THE EXIT
*
***********************************************************************
*                                                                     *
*        PRIVATE PROCLIB WORK AREA ANCHOR                             *
*                                                                     *
*        THIS PARAMETER SHOULD BE SET TO THE NAME OF THE FIELD        *
*        IN THE COMMON OR CONVERTER WORK AREA THAT WILL BE USED       *
*        TO CONTAIN THE ADDRESS OF THE PRIVATE PROCLIB WORK AREA.     *
*        THIS FIELD MUST BE A FULL WORD THAT IS UNUSED BY THE         *
*        CONVERTER.                                                   *
*                                                                     *
***********************************************************************
*
&ANCHOR  SETC  'RFULLE'            AT THE END OF THE CONVERTER WORKAREA
*
***********************************************************************
*                                                                     *
*        MESSAGE PREFIX                                               *
*                                                                     *
*        THIS PARAMETER SHOULD BE SET TO THE THREE (3) CHARACTER      *
*        WHICH IS DESIRED ON ALL MESSAGES ISSUED BY THE MODULE.       *
*        THE MESSAGE NUMBERS WILL BE XXX800 THROUGH XXX899, WHERE     *
*        XXX IS THE SELECTED PREFIX.                                  *
*                                                                     *
***********************************************************************
*
&MSGPFX  SETC  'DYP'  DYNAMIC PROCLIB CAN SET TO ANY 3 CHARACTERS
*                                  -         -        -
         IHASU74                   SET THE SE2 INDICATOR
*
*REM BHW SPLEVEL SET=1             INDICATE SP 1 LEVEL CODE
*
         PRINT OFF
*
         TITLE '**** INTERPRETER ENTRANCE LIST ****'
NEL      DSECT
         IEFNEL SUBCOM=C
         TITLE '**** JOB MANAGEMENT WORK AREA ****'
         IEFJMR ,
         TITLE '**** SMF MANAGEMENT CONTROL AREA ****'
         IEESMCA ,
         TITLE '**** REQUEST PARAMETER LIST ****'
         IFGRPL ,
         TITLE '**** DYNAMIC ALLOCATION TEXT UNIT KEYS ****'
         IEFZB4D2  ,
         TITLE '**** DYNAMIC ALLOCATION DATA AREAS ****'
         IEFZB4D0  ,
         TITLE '**** DAIRFAIL SERVICE ROUTINE PARM LIST ****'
         IKJEFFDF  DFDSECT=YES
         TITLE '**** DATA CONTROL BLOCK ****'
         DCBD  DSORG=PO,DEVD=DA
         TITLE '**** SYSTEM DIAGNOSTIC WORK AREA ****'
         IHASDWA DSECT=YES
         TITLE '**** PREFIX STORAGE AREA (LOW CORE) ****'
         IHAPSA
         TITLE '**** ADDRESS SPACE CONTROL BLOCK ****'
         IHAASCB
         TITLE '**** TASK CONTROL BLOCK ****'
         IKJTCB
         TITLE '**** TASK I/O TABLE (TIOT) ****'
TIODSECT DSECT
         IEFTIOT1
         TITLE '**** UNIT CONTROL BLOCK (UCB) ****'
UCBDSECT DSECT
         IEFUCBOB
*
         PRINT ON
*
         TITLE '**** CONVERTER WORK AREA ****'
         IEFCOMWA ,                COMMON WORK AREA
*
         IEFCVRWA ,                CONVERTER WORK AREA
*
         TITLE '**** PRIVATE PROCLIB WORK AREA ****'
***********************************************************************
*                                                                     *
* PPWORK                                                              *
* ******                                                              *
*                                                                     *
* THE FOLLOWING IS THE PRIVATE PROCEDURE LIBRARY SUPPORT WORK AREA.   *
* IT IS POINTED TO BY THE "&ANCHOR" FIELD IN THE CONVERTER WORK       *
* AREA.                                                               *
*                                                                     *
***********************************************************************
         SPACE 2
         USING PPWORK,R6
PPWORK   DSECT                     USER PROCLIB WORK AREA
*
PPSAVE   DC    9D'0'               SAVE AREA FOR ALL ENTRY POINTS
*                                  THAT NEED A REGSITER SAVE AREA
PPIEFVPP DC    V(IEFVPP)           BASE ADDRESS OF PROCLIB MODULE
PPVPP0   DC    V(IEFVPP0)          INITIALIZATION ENTRY POINT
PPVPP1   DC    V(IEFVPP1)          JOBPROC DD ENTRY POINT
PPVPP2   DC    V(IEFVPP2)          JOBPROC OPEN ENTRY POINT
PPVPP3   DC    V(IEFVPP3)          JOBPROC CLOSE ENTRY POINT
PPVPP4   DC    V(IEFVPP4)          COMBINED CALL TO VPP1 & VPP2
PPVPP5   DC    V(IEFVPP5)          IEFUJV INT TEXT INTERFACE
PPVPPM   DC    V(IEFVPPM)          MESSAGE PROCESSOR ENTRY POINT
PPVPPLEN EQU   *-PPVPP0            LENGTH OF ADDRESS VECTOR
*
PPFUTURE DC    20F'0'              RESERVED FOR FUTURE USE
*
PPSTAT0  DC    X'00'               STATUS FLAGS 0 (PER INVOCATION)
PP0CODE  EQU   X'80'                  IEFVPP0 IS EXECUTING
PP1CODE  EQU   X'40'                  IEFVPP1 IS EXECUTING
PP2CODE  EQU   X'20'                  IEFVPP2 IS EXECUTING
PP3CODE  EQU   X'10'                  IEFVPP3 IS EXECUTING
PPECODE  EQU   X'08'                  ERROR RECOVERY IN PROGRESS
PPMCODE  EQU   X'04'                  CONTROL PASSED TO IEFVGM
PPDACODE EQU   X'02'                  CONTROL PASSED TO SVC 99
PPINITC  EQU   X'01'                  INITIALIZATION IS COMPLETE
*
PPSTAT1  DC    X'00'               STATUS FLAGS 1 (PER JOB):
PPJPERR  EQU   X'80'                  ERROR NOTED IN JOBPROC STMT
PPHAVJP  EQU   X'40'                  JOBPROC DD CARD IS PRESENT
PPJPBUSY EQU   X'20'                  POSSIBLE JOBPROC CONCAT.
PPCONCJP EQU   X'10'                  JOBPROC IS A CONCATENATION
PPSYSYES EQU   X'08'                  SYSTEM PROCLIB IS REQUIRED
PPBUFGOT EQU   X'04'                  NEW PROCLIB BUFFER OBTAINED
PPOPENED EQU   X'02'                  USER PROCLIB DCB IS OPEN
PPCONCD  EQU   X'01'                  JOBPROC IS NOW CONCATENATED
*
PPSTAT2  DC    X'00'               STATUS FLAGS 2 (PER STATEMENT):
PPHAVDSN EQU   X'80'                    DSNAME PROVIDED
PPHAVSER EQU   X'40'                    VOLSER PROVIDED
PPHAVREF EQU   X'20'                    VOL=REF=DSNAME SPECIFIED
PPHAVUNT EQU   X'10'                    UNIT=" " SPECIFIED
PPRREQUE EQU   X'08'                    RE-QUEUE FOR CONVERSION
*
PPALLOC1 DS    BL4                 ALLOCATION STATUS FLAGS
*                                     ONE STATUS BIT PER DDNAME
*                                      ON -> DDNAME IS ALLOCATED
*
*        THE FOLLOWING TRACE FLAGS ARE USED FOR ERROR RECOVERY
*
PPTRPP0  DC    X'00'               TRACE FLAGS FOR IEFVPP0
PPTRPP1  DC    X'00'               TRACE FLAGS FOR IEFVPP1
PPTRPP2  DC    X'00'               TRACE FLAGS FOR IEFVPP2
PPTRPP2E EQU   X'80'                  PP2 CALLED FOR CURRENT JOB
PPTRDCON EQU   X'40'                  DYNAMIC CONCAT IN PROGRESS
PPTROPEN EQU   X'20'                  PROCLIB OPEN IN PROGRESS
PPTRS213 EQU   X'10'                  USER DATA SET "NOT FOUND"
PPTRPP3  DC    X'00'               TRACE FLAGS FOR IEFVPP3
PPTRDDCN EQU   X'80'                  DYNAMIC DE-CON IN PROGRESS
PPTRDDAL EQU   X'40'                  DYNAMIC DE-ALOC IN PROGRESS
PPMISC   DC    X'00'               MISCELLANEOUS FLAGS
PPDEBUG  EQU   X'80'                  ALL SORTS OF DEBUGGING CODE
PPESTAE  EQU   X'40'                  ESTAE HAS BEEN ISSUED
PPNOSYSP EQU   X'20'                  SYSPROC UNAVAILABLE
*
PPTIOTAD DC    A(*-*)              ADDRESS OF TIOT
PPJPCNT  DC    Y(*-*)              NO. OF JOBPROC DD STATEMENTS
PPJPMAX  DC    Y(*-*)              MAXIMUM USER PROCLIBS ALLOWED
         DC    2H'0'               RESERVED FOR FUTURE USE
*
PPSDCBA  DC    A(*-*)              -> ORIGINAL IEFPDSI DCB
PPSBUFA  DC    A(*-*)              -> ORIG. PDSI BUF(IF REPLACED)
PPBUFAD  DC    A(*-*)              -> NEW PROCLIB BUF(IF REPLACED)
PPSYSNAM DC    CL8'IEF*PDSI'       NAME OF SYSTEM PROCLIB DD
*
PPDCB    DCB   DDNAME=IEF*PDS0,DSORG=PO,MACRF=(R)
PPUSRDDN EQU   PPDCB+(DCBDDNAM-IHADCB) PRIVATE PROCLIB DDNAME
*
PPDAARGL DC    A(0)                DYN ALLOC ARG LIST
PPDARB   DC    0CL20' ',5F'0'      DYN ALLOC REQUEST BLOCK
PPDATXTL DC    6A(*-*)             DYN ALLOC TEXT POINTERS
*
PPDDNAMK DC    Y(*-*)              DDNAME KEY FOR DYN ALLOC
PPDDNAM# DC    Y(*-*)                    NO. OF LENGTH-PARM ITEMS:
PPDDNAML DC    Y(8)                      DDNAME LENGTH
PPDDNAME DC    CL8'IEF*PDS0'             DDNAME STRING
*
PPDSNAMK DC    Y(*-*)              DSNAME KEY FOR DYN ALLOC
PPDSNAM# DC    Y(1)                      NO. OF LENGTH-PARM ITEMS:
PPDSNAML DC    Y(*-*)                    DSNAME LENGTH
PPDSNAME DC    CL44' '                   USER PROCLIB DSNAME
*
PPVOLSEK DC    Y(*-*)              VOLSER KEY FOR DYN ALLOC
PPVOLSE# DC    Y(1)                      NO. OF LENGTH-PARM ITEMS:
PPVOLSEL DC    Y(*-*)                    VOLSER LENGTH
PPVOLSER DC    CL8' '                    USER PROCLIB VOLSER
*
PPUNITK  DC    Y(*-*)              UNIT KEY FOR DYN ALLOC
PPUNIT#  DC    Y(1)                      NO. OF LENGTH-PARM ITEMS
PPUNITL  DC    Y(*-*)                    UNIT LENGTH
PPUNIT   DC    CL8' '                    USER PROCLIB UNIT
*
PPSDISPK DC    Y(*-*)              STATUS DISPOSITION KEY
PPSDISP# DC    Y(1)                      NO. OF LENGTH-PARM ITEMS
PPSDISPL DC    Y(1)                      DISPOSITION CODE LENGTH
PPSDISP  DC    X'04'                     DISP CODE (NEW DEFAULT)
*
PPVLREFK DC    Y(*-*)              VOLUME REFERENCE TO DSNAME KEY
PPVLREF# DC    Y(1)                      NO. OF LENGTH-PARM ITEMS
PPVLREFL DC    Y(*-*)                    REFERENCE DSNAME LENGTH
PPVOLREF DC    CL44' '                   VOLUME REFERENCE DSNAME
*
PPCONCDK DC    Y(*-*)              DDNAME KEY FOR CONCATENATIONS
PPCONCD# DC    Y(*-*)                 NUMBER OF LENGTH-PARM ITEMS:
PPCONCDD DC    16XL10'00'                DDNAME LTH & TEXT(MAX=16)
*
PPDFPARM DS    5F                        DAIRFAIL PARM LIST
PPDFF02A DS    A                         DAIRFAIL - IKJEFF02 WORD
PPDFRETC DS    F                         DAIRFAIL RETURN CODE
PPDFIDN  DS    H                         DAIRFAIL CALLER IDENT.
*
PPESTAEP ESTAE *-*,TERM=YES,MF=L
PPSTAEPL EQU   *-PPESTAEP          AREA LENGTH
*
PPOCLST  OPEN  (PPDCB,(INPUT)),MF=L
*
PPLOCPRM CAMLST NAME,*-*,,*-*      PARM LIST FOR LOCATE
*
         DS    0D
PPLOCWRK DS    CL268               WORK AREA FOR LOCATE
*
PPWTOWRK DS    CL144               WTO MESSAGE AREA
*
         DS    0D                  ALIGN TO A DOUBLE WORD
PPWRKEND EQU   *                   END OF PRIVATE PROCLIB ADD-ON
PPWORKLN EQU   *-PPWORK            WORK AREA LENGTH
*
         TITLE '**** INTERNAL TEXT HEADER ****'
         IEFTXTFT ,
*
***********************************************************************
*                                                                     *
*        THE FOLLOWING SYMBOL IS USED IN THE DD CARD INTERNAL TEXT    *
*        HEADER TO INDICATE A JOBPROC DD STATEMENT.  ITS VALUE MUST   *
*        BE USED IN THE SUPERZAP TO IEFVHE THAT CHECKS FOR A JOBPROC  *
*        DD STATEMENT AND IGNORES IT.  IT IS DEFINED HERE TO AVOID    *
*        HAVING TO MAKE CHANGES TO THE IEFTXTFT MACRO.  THIS VALUE    *
*        WAS PREVIOUSLY USED TO SET A FLAG IN FIELD "STRDINDC".       *
*        HOWEVER, SUBSEQUENT MAINTENANCE HAS USED THAT FIELD.  IT IS  *
*        NOW USED TO SET A FLAG IN FIELD "STRINDCS".  THE SUPERZAP    *
*        TO IEFVHE MUST BE CHANGED ACCORDINGLY.                       *
*                                                                     *
***********************************************************************
*
JPROCSTR EQU   X'80'               TEXT IS FOR A JOBPROC DD CARD
*
         TITLE '**** JCL KEYWORD CODES ****'
         IEFVKEYS  ,
*
***********************************************************************
*                                                                     *
*        THE FOLLOWING SYMBOL IS USED AS THE INTERNAL TEXT KEY VALUE  *
*        FOR THE "SYSPROC=" KEYWORD.  ITS VALUE MUST BE USED IN       *
*        CHANGING THE JCL KEYWORD TABLE IN IEFVFA.  THIS SYMBOL IS    *
*        DEFINED HERE TO AVOID HAVING TO CHANGE THE IEFVKEYS MACRO.   *
*                                                                     *
***********************************************************************
*
SYSPROCK EQU   X'01'   *     DD      SYSPROC=  JOBPROC DD STATEMENTS
*
         TITLE '**** MODULE PREFIX ****'
IEFVPP   CSECT ,                   CONTROL SECTION NAME
*
         ENTRY IEFVPP0             INITIALIZATION CALL
         ENTRY IEFVPP1             JOBPROC DD CARD PROCESSOR
         ENTRY IEFVPP2             SET USER PROCLIB ENVIRONMENT
         ENTRY IEFVPP3             END OF JOB CLEANUP ROUTINE
         ENTRY IEFVPP4             COMBINED IEFVPP1 AND IEFVPP2 ENTRY
         ENTRY IEFVPP5             IEFUJV INTERNAL TEXT EXIT INTERFACE
         ENTRY IEFVPPM             MESSAGE PROCESSOR
*
R10      EQU   10                  INTERNAL SUB-ROUTINE LINKAGE
R11      EQU   11                  PRIMARY BASE REGISTER
R12      EQU   12                  CONVERTER WORK AREA
R13      EQU   13                  REGISTER SAVE AREA
R14      EQU   14                  SCRATCH AND LINK
R15      EQU   15                  SCRATCH AND LINK
         SPACE 3
         USING COMWA,R12           SET STANDARD BASE
         SPACE 3
         DC    AL1(VPPIDL)         LTH OF IDENTIFICATION PREFIX
VPPID    DC    CL8'IEFVPP'         USER PROCLIB SUPPORT MODULE ID
         DC    CL9'V4.R1.M0 '      CURRENT SYZYGY RELEASE NO.
         DC    CL8'&SYSDATE'       LAST ASSEMBLY DATE
         DC    CL8' &SYSTIME'      TIME OF LAST ASSEMBLY
         DC    C'COPYRIGHT FEBRUARY 25, 2003 BY SYZYGY INC. '
VPPIDL   EQU   *-VPPID             LENGTH OF IDENTIFICATION HEADER
*
         TITLE '**** IEFVPP0  - INITIALIZATION PROCESSING ****'
***********************************************************************
*                                                                     *
* IEFVPP0                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED TO PROCESS ANY INITIALIZATION FUNCTIONS.     *
* IF THIS ROUTINE IS NOT INVOKED, THE CONVERTER WILL FAIL IN THE MOST *
* TERRIBLE WAY, SINCE ADDRESSES THAT IT WILL LATER ATTEMPT TO BRANCH  *
* TO WILL NOT HAVE BEEN PLACED IN THE CONVERTER WORK AREA RESERVED    *
* WORD AND THE PRIVATE PROCLIB WORK AREA.                             *
*                                                                     *
*    1  GETMAIN THE PRIVATE PROCLIB WORK AREA AND ANCHOR IT IN        *
*       THE CONVERTER WORK AREA.                                      *
*    2  INITIALIZE ENTRY POINTS IN THE PRIVATE PROCLIB WORK AREA.     *
*    3  INITIALIZE SWITCHES, COUNTERS, AND TRACE FLAGS.               *
*    4  INITIALIZE DYNAMIC ALLOCATION PARAMETER BLOCK.                *
*    5  DETERMINE PROCLIB DDNAME BY ENQUEUE.                          *
*    6  PERFORM CLEANUP FROM PREVIOUS CONVERSION (IF NECESSARY).      *
*    7  BRANCH TO IEFVHA.                                             *
*                                                                     *
* INPUT:                                                              *
*        CONVERTER WORK AREA (REGISTER 12).                           *
*                                                                     *
* OUTPUT:                                                             *
*        INITIALIZE PRIVATE PROCLIB ANCHOR IN THE CWA AND THE         *
*        PRIVATE PROCLIB WORK AREA ITSELF.                            *
*                                                                     *
* ENTERNALS:                                                          *
*        NONE                                                         *
*                                                                     *
* EXIT:                                                               *
*        BRANCH TO IEFVHA.                                            *
*                                                                     *
* SVC:                                                                *
*        ESTAE    (SVC 60)                                            *
*        ENQ      (SVC 56)                                            *
*        DYNALLOC (SVC 99)                                            *
*        WTO      (SVC 35)                                            *
*                                                                     *
***********************************************************************
         EJECT
IEFVPP0  PPEPA VPP0                IEFVPP0 ENTRY POINT ADDRESS
*
***********************************************************************
*                                                                     *
*        THE INSTRUCTION BELOW CAN BE CHANGED OR ZAPPED TO ENABLE     *
*        EXTERNAL DEBUGGING TRACE CODE (WTO MESSAGES).                *
*                                                                     *
***********************************************************************
*
PPSETBUG DS    0AL4(PPSETBUG)      PROVIDE XREF REFERENCE
         OI    PPMISC,*-*          PATCH TO TURN ON DEBUGGING CODE
*
         EJECT
         L     R0,PPIEFVHA         SET INITIALIZATION "RETURN" ADDRESS
         L     R1,4(,R13)          LOAD ADDRESS OF CALLING SAVE AREA
         ST    R0,12(,R1)          SET "RETURN" R14
         ST    R0,16(,R1)          SET "RETURN" R15
*
         ST    R11,PPIEFVPP        SET MODULE BASE IN WORK AREA
         MVC   PPVPP0(PPVPPLEN),PP0ADDRS INITIALIZE ENTRY POINTS
*
         MVI   PPSTAT1,0           INITIALIZE STATUS FLAG 1
         MVI   PPSTAT2,0           INITIALIZE STATUS FLAG 2
         MVI   PPTRPP0,0           INITIALIZE
         MVI   PPTRPP1,0            TRACE
         MVI   PPTRPP2,0             FLAGS
         MVI   PPTRPP3,0
*
         MVC   PPDCB(PPDCBPTL),PPDCBPAT  INIT DCB AREA
*
         LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            GET ADDRESSABILITY TO DARB
         ST    R9,PPDAARGL         SET PTR TO DARB IN PARM LIST->
         MVI   PPDAARGL,S99RBPND   SET END OF PARM LIST INDICATOR
         XC    PPDARB(L'PPDARB),PPDARB CLEAR ALLOC REQUEST BLOCK
         MVI   S99RBLN,L'PPDARB    SET LENGTH OF REQUEST BLOCK
         MVI   S99FLG11,S99ONCNV+S99NOCNV+S99NOMNT SET FLAGS 1
*              NO CONVERTIBLE, NO EXISTING ALLOC, NO VOLUME MOUNT
         MVI   S99FLG21,S99NORES   SET NO DATA SET ENQUEUE FLAGS 2
         LA    R0,PPDATXTL         -> DYNAMIC ALLOCATION TEXT
         ST    R0,S99TXTPP         SET TEXT LIST PTR IN DARB
*
         LA    R9,PPDFPARM         -> DAIRFAIL PARM LIST
         USING DFDSECTD,R9         MAKE IT ADDRESSABLE
         LA    R1,PPDARB           -> DYNAMIC ALLOCATE RB
         ST    R1,DFS99RBP         SET IN DFPL
         LA    R1,PPDFRETC         -> RETURN CODE HOLDING AREA
         ST    R1,DFRCP            SET IN DFPL
         LA    R1,PPDFF02A         -> IKJEFF02 ADDRESS AREA
         ST    R1,DFJEFF02         STORE PTR IN DAIRFAIL PLIST
         XC    PPDFF02A,PPDFF02A   SHOW IKJEFF02 ADDRESS UNKNOWN
         LA    R1,PPDFIDN          -> DAIRFAIL IDENTIFICATION NO.
         ST    R1,DFIDP            SET IN DFPL
         MVI   DFIDP,X'80'         FLAG END OF PARM LIST
         XC    DFCPPLP,DFCPPLP     CLEAR CPPL POINTER
         LA    R1,50               INDICATE SVC 99 ERROR
         STH   R1,PPDFIDN           IN DAIRFAIL PARM LIST
         OI    PPDFIDN,DFWTP       INDICATE WTP REQUEST
*
         MVC   PPLOCPRM(PPLOCPL),PPCAMLST MOVE IN LOCATE CAMLST
         LA    R1,PPDSNAME         -> DSNAME HOLD AREA
         ST    R1,PPLOCPRM+4       SET DSNAME PTR IN CAMLST
         LA    R1,PPLOCWRK         -> LOCATE WORK AREA
         ST    R1,PPLOCPRM+12      SET WORK AREA PTR IN CAMLST
*
         MVC   PPLOCWRK(PPENQPL),PPENQLST  MOVE ENQ PARM LIST
         LA    R1,PPSYSNAM         POINT AT MINOR NAME
         ST    R1,PPLOCWRK+8       STORE ADDRESS IN ENQ PARM LIST
         MVC   PPSYSNAM,PPSYSDDN   INITIALIZE MINOR NAME
         LA    R9,PPDDNSUF         POINT AT NAME VARIABLES
         LA    R8,L'PPDDNSUF       NUMBER OF CONVERTERS
*
PPENQLP  MVC   PPSYSNAM+3(1),0(R9) MOVE NAME CHARACTER
         ENQ   MF=(E,PPLOCWRK)     TRY THE ENQ
         LTR   R15,R15             CHECK IF RESOURCE GOTTEN
         BZ    PPNAMOBT            BRANCH IF NAME OBTAINED
*
         LA    R9,1(,R9)           POINT AT NEXT NAME CODE
         BCT   R8,PPENQLP          LOOP THROUGH THE NAMES
*
         WTO   MF=(E,PPENQWTO)     INFORM OPERATOR OF FAILURE
         B     PPRETURN            RETURN WITHOUT ENABLING JOBPROC
*
PPNAMOBT PPDEBUG ENQ0              TRACE END OF ENQUEUE
         MVC   PPUSRDDN+3(1),PPSYSNAM+3 INITIALIZE ALLOCATION DDNAME
         L     R1,PSATOLD-PSA(,R0) LOAD TCB ADDRESS
         L     R1,TCBTIO-TCB(,R1)  LOAD TIOT ADDRESS
         ST    R1,PPTIOTAD         SAVE THE ADDRESS
         USING TIODSECT,R1         SET UP ADDRESSIBILITY
         SLR   R15,R15             ZERO FOR INSERTS
*
PPTIOTLP CLI   TIOELNGH,0          CHECK FOR END OF TIOT
         BE    PPDDNMOK            BRANCH IF NAME NOT FOUND
         CLC   TIOEDDNM,PPUSRDDN   CHECK IF DD LEFT ALLOCATED
         BE    PPCLNUP             GO CLEAN UP IF SO
         IC    R15,TIOELNGH        LOAD THE LENGTH
         AR    R1,R15              INCREMENT ADDRESS
         B     PPTIOTLP            CONTINUE TIOT SCAN
*
PPCLNUP  IC    R15,TIOELNGH        LOAD ENTRY LENGTH
         AR    R1,R15              FIND NEXT ENTRY
         CLI   TIOELNGH,0          CHECK FOR LAST ENTRY
         BE    PPCLNNOC            BRANCH IF AT END
         CLI   TIOEDDNM,C' '       CHECK FOR CONCATENATION
         BNE   PPCLNNOC            BRANCH IF NOT CONCATENATED
         BAL   R10,PPDECONC        ATTEMPT DE-CONCATENATION
         PPDEBUG DCN0              END OF DE-CONCATENATION
*
PPCLNNOC MVC   PPALLOC1(2),=X'FFFF'     FORCE DEALLOCATION
         BAL   R10,PPDEALOC        OF EVERYTHING
         PPDEBUG DAL0              END OF DEALLOCATION
         B     PPDDNMOK            GO LOOK FOR SYSPROC DDNAME
*
PPDDNMOK L     R1,PPTIOTAD         RELOAD TIOT ADDRESS
         SLR   R15,R15             ZERO FOR INSERTS
         SLR   R14,R14             ZERO FOR COUNTING
*
PPTIOSLP CLI   TIOELNGH,0          CHECK FOR END OF TIOT
         BE    PPSYSMIS            BRANCH IF END - SYSPROC MISSING
         CLC   TIOEDDNM,PPSYSNAM   MATCH DD NAMES
         BE    PPSYSCNT            BRANCH IF WE FOUND IT
         IC    R15,TIOELNGH        LOAD THE LENGTH
         AR    R1,R15              INCREMENT ADDRESS
         B     PPTIOSLP            CONTINUE TIOT SCAN
*
PPSYSCNT LA    R14,1(,R14)         BUMP SYSTEM PROCLIB COUNT
         IC    R15,TIOELNGH        LOAD THE LENGTH
         AR    R1,R15              INCREMENT THE ADDRESS
         CLI   TIOEDDNM,C' '       CHECK FOR CONCATENATION
         BE    PPSYSCNT            LOOP IF IT IS
         LA    R0,16               SET MAXIMUM PROCLIBS
         SR    R0,R14              DEDUCT FOT SYSTEM PROCLIBS
         STH   R0,PPJPMAX          SET THE MAX USER PROCLIB
         B     PPSYSDOK            AND FINISH INITIALIZATION
*
PPSYSMIS L     R1,PPTIOTAD         LOAD THE TIOT ADDRESS
         CLC   TIOCNJOB(8),=CL8'MSTRJCL' TEST IF MASTER'S TIOT
         BE    PPSYSMMS            SKIP MESSAGE IF SO
         CLC   TIOCNJOB(6),=C'MSTJCL'   TEST IF MASTER'S TIOT (1.3.2)
         BE    PPSYSMMS            SKIP MESSAGE IF SO
*
         L     R1,PSAAOLD-PSA(,0)  LOAD ASCB ADDRESS
         LH    R1,ASCBASID-ASCB(,R1)    LOAD THE ASID
         CH    R1,=H'1'            CHECK FOR THE MASTER ADDRESS SPACE
         BE    PPSYSMMS            BRANCH IF SO
         WTO   MF=(E,PPSYSWTO)     INDICATE ERROR
*
PPSYSMMS OI    PPMISC,PPNOSYSP     INDICATE SYSPROC UNAVAILABLE
         LA    R0,16               SET MAXIMUM PROCLIBS
         STH   R0,PPJPMAX          STASH THE MAX
*
PPSYSDOK DS    0H
         SR    R0,R0               CLEAR R0
         STH   R0,PPJPCNT          INITIALIZE CONCATENATION COUNT
         STCM  R0,3,PPALLOC1       INITIALIZE ALLOCATION FLAGS
*
         PPDEBUG END0              INDICATE END OF PP0
         DROP  R1                  DONE WITH THE TIOT
*
***********************************************************************
*                                                                     *
*        THE FOLLOWING MVI INSTRUCTION SHOULD HAVE ITS IMMEDIATE      *
*        FIELD MODIFIED FROM PPINITC TO ZERO TO CRIPPLE THE PRIVATE   *
*        PROCLIB SUPPORT.  THIS IS THE ONLY WAY, SHORT OF REMOVING    *
*        ALL CONVERTER MODIFICATIONS (ZAPS AND ALL).                  *
*                                                                     *
***********************************************************************
*
PPCANCEL DS    0AL4(PPCANCEL)      PROVIDE XREF REFERENCE
         MVI   PPSTAT0,PPINITC     INDICATE VPP0 HAS COMPLETED
*
         B     PPRETURN            BACK TO CALLER
*
         DROP  R9,R11,R7
*
PPIEFVHA DC    V(IEFVHA)           EXIT FROM IEFVPP0
PP0ADDRS DS    0F                  ENTRY POINT ADDRESS VECTOR
         DC    A(IEFVPP0)
         DC    A(IEFVPP1)
         DC    A(IEFVPP2)
         DC    A(IEFVPP3)
         DC    A(IEFVPP4)
         DC    A(IEFVPP5)
         DC    A(IEFVPPM)
*
     TITLE '**** IEFVPP4  - COMBINED LINK TO IEFVPP1 AND IEFVPP2 ****'
***********************************************************************
*                                                                     *
* IEFVPP4                                                             *
* *******                                                             *
*                                                                     *
*        THIS ROUTINE IS CALLED BY IEFVFA EACH TIME A JCL STATEMENT   *
*    HAS BEEN COMPLETELY TRANSLATED TO INTERNAL TEXT.  THIS ROUTINE   *
*    WILL BRANCH TO EITHER IEFVPP1 OR IEFVPP2 OR RETURN TO IEFVFA     *
*    IF THE STATEMENT DOES NOT MEET THE CRITERIA FOR IEFVPP1 OR       *
*    IEFVPP2.                                                         *
*                                                                     *
* INPUT:                                                              *
*        SEE IEFVPP1 AND IEFVPP2.                                     *
*                                                                     *
* OUTPUT:                                                             *
*        NONE.                                                        *
*                                                                     *
* SUBROUTINES:                                                        *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        BRANCH TO IEFVPP1 OR IEFVPP2 OR RETURN TO IEFVFA.            *
*                                                                     *
* SVC:                                                                *
*        NONE.                                                        *
*                                                                     *
* NOTES:                                                              *
*        THIS ROUTINE DOES NOT SAVE ANY REGISTERS, SINCE IEFVPP1 OR   *
*        IEFVPP2 (IF BRANCHED TO) WILL DO SO.  IT IS UNNECESSARY      *
*        TO SAVE THEM IF CONTROL IS RETURNED DIRECTLY TO IEFVFA.      *
*                                                                     *
***********************************************************************
*
IEFVPP4  DS    0H
         USING *,R15
         TM    SWZ,DDSW           IS THIS A DD STATEMENT
         BZ    PP4EXEC            BRANCH IF NOT
         L     R15,&ANCHOR        LOAD THE ANCHOR ADDRESS
         L     R15,PPVPP1-PPWORK(,R15) LOAD THE REAL ENTRY POINT
         BR    R15                BRANCH TO IEFVPP1
*
PP4EXEC  TM    SWA,JHS            HAD FIRST EXEC STATEMENT
         BOR   R14                RETURN IF SO
         TM    SWZ,EXECSW         IS THIS AN EXEC STATEMENT
         BZR   R14                BRANCH IF NOT
         TM    PSTMT+4,X'08'      IS THIS A CONVERTED PROC
         BOR   R14                RETURN IF SO
         L     R15,&ANCHOR        LOAD THE ANCHOR ADDRESS
         L     R15,PPVPP2-PPWORK(,R15) LOAD THE REAL ENTRY POINT
         BR    R15                BRANCH TO IEFVPP2
*
         DROP  R15
*
         TITLE '**** IEFVPP1  - //JOBPROC DD PROCESSOR ****'
***********************************************************************
*                                                                     *
* IEFVPP1                                                             *
* *******                                                             *
*                                                                     *
*        THIS ROUTINE IS CALLED BY IEFVFA EACH TIME A DD STATEMENT    *
*    HAS BEEN COMPLETELY TRANSLATED TO INTERNAL TEXT. ON EACH CALL    *
*    IEFVPP1 DOES THE FOLLOWING:                                      *
*                                                                     *
*    1) DETERMINES WHETHER THE DD STATEMENT IS A JOBPROC. IF NOT,     *
*       CONTROL IS PASSED BACK IMMEDIATELY TO IEFVFA.                 *
*                                                                     *
*    2) THE INTERNAL TEXT STRING IS FLAGGED TO INDICATE THAT IT       *
*       REPRESENTS A JOBPROC DD CARD. THIS IS THE METHOD USED TO      *
*       CAUSE THE INTERPRETER TO IGNORE THE DD STATEMENT AT THE       *
*       INTERPRETER "GET" MODULE (IEFVHE) DURING INTERPRETATION.      *
*       THIS IS NECESSARY IN ORDER TO KEEP THE CONVERTER JCL LISTING  *
*       STATEMENT NUMBERS CONSISITENT WITH THE INTERPRETERS LISTING.  *
*                                                                     *
*    3) DETERMINES IF THE FIRST EXEC STATEMENT OF THE STEP HAS        *
*       BEEN RECEIVED. IF IT HAS, AN ERROR MESSAGE INDICATING THAT    *
*       THE JOBPROC DD STATEMENT IS "MISPLACED" IS ISSUED AND         *
*       CONTROL IS PASSED BACK TO IEFVFA.                             *
*                                                                     *
*    4) DETERMINES IF THIS DD STATEMENT IS CONCATENATED TO A PREVIOUS *
*       JOBPROC DD STATEMENT. IF NOT (IE. A DDNAME HAS BEEN CODED), A *
*       MESSAGE IS ISSUED INDICATING THE THIS IS A DUPLICATE JOBPROC  *
*       DD STATEMENT.                                                 *
*                                                                     *
*                                                                     *
*    5) ALL INFORMATION NECESSARY TO ALLOCATE THE JOBPROC DATA SET    *
*       DEFINED ON THE DD STATEMENT IS OBTAINED BY SCANNING THE       *
*       INTERNAL TEXT BUFFER. CURRENTLY ONLY THE FOLLOWING DD CARD    *
*       KEYWORDS ARE PROCESSED:                                       *
*                                                                     *
*           KEYWORD       RESTRICTIONS                                *
*          ---------     --------------                               *
*           DSNAME           NONE                                     *
*           UNIT             FIRST POSITIONAL ONLY (IE. UNIT NAME)    *
*           VOLUME           SER="VOLUME SERIAL NUMBER"               *
*                            REF="DSNAME"                             *
*           SYSPROC          YES -> INSTALLATION PROCLIBS AS DEFINED  *
*                                   BY THE IEFDPDSI DD CARD IN THE    *
*                                   JES PROC SHOULD BE CONCATENATED   *
*                                   FOLLOWING THE USERS' PROCLIBS.    *
*                            NO  -> ONLY THE USERS' PROCLIBS SHOULD   *
*                                   BE SEARCHED.                      *
*                                                                     *
*    6) THE USER PROCLIB IS ALLOCATED TO "JES" VIA THE DYNAMIC        *
*       ALLOCATION FACILITIES OF OS/VS. THE PROCLIB IS ALLOCATED TO   *
*       THE DDNAME "IEF#PDS*" WHERE "*" IS AN INDEX BETWEEN 0 AND     *
*       15 (X'F') WHICH REPRESENTS THE RELATIVE CONCATENATION NO. OF  *
*       THE CURRENT JOBPROC ALLOCATION TO OTHERS (IF THE JOBPROC IS   *
*       IN FACT A CONCATENATION). THE MAXIMUM NO. OF CONCATENATIONS   *
*       ALLOWED IS SET DURING INITIALIZATION (IEFVPP0) AS 16 MINUS    *
*       THE NUMBER OF PROCLIBS PROVIDED IN THE IEF#PDSI DEFINITION.   *
*       IF SYSPROC=NO IS SPECIFIED, THE NUMBR OF USER SUPPLIED        *
*       PROCLIBS IS AUTOMATICALLY SET TO THE MAXIMUM (16).            *
*                                                                     *
*    7) CONTROL IS PASSED BACK TO IEFVFA WHO WILL WRITE THE INTERNAL  *
*       TEXT STRING FOR THE JOBPROC BACK TO "JES".                    *
*                                                                     *
* INPUT:                                                              *
*        IWA(SWY)      - INDICATES IF IEFVFA HAS DETECTED A SYNTAX    *
*                        ERROR IN THE CURRENT STATEMENT               *
*        IWA(TEXTBUFP) - CONTAINS A PTR TO THE INTERNAL TEXT BUFFER   *
*        IEFVKEYS      - JCL KEY EQUATES USED FOR SCANNING THE TEXT   *
*                        BUFFER                                       *
*                                                                     *
* OUTPUT:                                                             *
*        INTERNAL TEXT STRING IS FLAGGED AS REPRESENTING A JOBPROC    *
*        AND USER PROCLIB IS ALLOCATED.                               *
*                                                                     *
* SUBROUTINES:                                                        *
*        CALL TO IEFVPPM WHEN ERROR CONDITION IS DETECTED.            *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER IEFVFA VIA RETURN MACRO.                    *
*                                                                     *
* SVC:                                                                *
*        ESTAE (SVC 60)                                               *
*        DYNAMIC ALLOCATION (SVC 99)                                  *
*                                                                     *
* NOTES:                                                              *
*     1) THE TEXT SCANNING IS SOMEWHAT SIMPLIFIED SINCE ALL REQUIRED  *
*           OPERANDS ARE THE FIRST FOR THE RELATED KEYWORDS.          *
*                                                                     *
*     2) ALL ERROR SITUATIONS CAUSE A CALL TO THE IEFVPPM SUBROUTINE  *
*        WHICH CAUSES THE JOB TO BE FLUSHED ON A JCL ERROR. ERROR     *
*        MESSAGES ARE ACTUALLY WRITTEN TO THE USERS' LOG FILE VIA     *
*        AN ABOMINATION TAKEN FROM IEFVGM.                            *
*                                                                     *
***********************************************************************
         EJECT
IEFVPP1  PPEPA VPP1                IEFVPP1 ENTRY POINT ADDRESS
         EJECT
**************************** ROUTING **********************************
         TM    SWY,X'01'           ANY SYNTAX ERROR IN STATEMENT ?
         BO    PPRETURN            YES, IGNORE THE STATEMENT
         L     R1,TEXTBUFP         -> INTERNAL TEXT BUFFER
         LA    R1,STRDKEY-TEXT(,R1)  -> START KEY
         CLI   1(R1),1             SINGLE POSITIONAL (= DDNAME) ?
         BNE   PPRETURN            NO, WILL GET JCL ERROR
*
         TM    PPSTAT1,PPJPBUSY    BUSY ON JOBPROC CONCAT ?
         BO    PPTCONC             YES, SKIP
         CLC   2(8,R1),PPJOBPRO    IS START OF JOBPROC CONCAT ?
         BNE   PPRETURN            NO, EXIT
         TM    PPSTAT1,PPHAVJP     BEEN HERE BEFORE ?
         BO    PPERM836            YES -> MISPLACED JOBPROC ERROR
*
         OI    PPSTAT1,PPHAVJP+PPJPBUSY+PPSYSYES    BUSY, DEFAULT INIT
         B     PP1COMM             CONTINUE INITIALIZATION
*
PPTCONC  DS    0H
         CLI   2(R1),0             MISSING DDNAME = CONCATENATION
         BE    PPSETCNC            YES, GO SAVE STATUS
         NI    PPSTAT1,PPALL-PPJPBUSY    CLEAR JOBPROC BUSY STATUS
         CLC   2(8,R1),PPJOBPRO    A SECOND "JOBPROC" DD CARD ?
         BE    PPERM836            YES, GIVE "MISPLACED JOBPROC"
         B     PPRETURN            AND EXIT
*
PPSETCNC DS    0H
         OI    PPSTAT1,PPCONCJP    SHOW CONCATENATED USER FILES
*
PP1COMM  DS    0H
         MVI   PPDSNAME,C' '       CLEAR DSNAME AREA
         MVC   PPDSNAME+1(43),PPDSNAME    TO BLANKS
         MVC   PPVOLREF,PPDSNAME   CLEAR VOL=REF=DSNAME
         MVC   PPVOLSER,PPDSNAME   CLEAR VOLSER AREA
         MVC   PPUNIT,PPDSNAME     CLEAR THE UNIT NAME AREA
         MVI   PPSDISP,PPASHR      SET DEFAULT DISP TO "SHR" SO THAT
*                                   DISP PARAMETER MAY BE IGNORED
         PPDEBUG ENT1              TRACE PP1 WITH JOBPROC CARD
*
         L     R2,TEXTBUFP         R2 -> INTERNAL TEXT BUFFER
         OI    STRINDCS-TEXT(R2),JPROCSTR FLAG DD CARD AS JOBPROC
         TM    PPSTAT1,PPJPERR     PREVIOUS ERROR NOTED ?
         BO    PPRETURN            YES, JUST EXIT
         MVI   PPSTAT2,0           CLEAR STATEMENT STATUS FLAGS
*
         EJECT
************************** COLLECT PARAMETERS *************************
         SR    R0,R0               CLEAR NUMBER/COUNT STACK (MAX = 4)
         SR    R14,R14             USED FOR NUMBER/COUNT
         SR    R15,R15             USED FOR LENGTH
*
PPITXT01 DS    0H
         LR    R2,R1               SAVE KEY ADDR
         CLI   0(R2),ENDK          END OF TEXT ?
         BE    PPTSTOPT            BRANCH IF YES..GO TEST OPTIONS
         IC    R14,1(,R1)          LOAD NUMBER
         LA    R1,2(,R1)           -> LENGTH/COUNT/KEY
         LTR   R14,R14             HAVE OPERAND ?
         BZ    PPITXT01            NULL OPERAND, LOOP
*
PPITXT02 DS    0H
         TM    0(R1),PPVLBIT       IS THIS A COUNT ?
         BZ    PPITXT03            NO, SKIP
         SLA   R0,8                PUSH NUMBER/COUNT STACK
         BO    PPERM800            ERROR IF OVERFLOW
         OR    R0,R14              SAVE CURRENT NUMBER/COUNT
         IC    R14,0(,R1)          LOAD NEW COUNT
         LA    R1,1(,R1)           -> LENGTH/COUNT
         N     R14,PPMSKCLR        CLEAR TOP FLAG & TEST ZERO
         BNZ   PPITXT02            HAVE COUNT, GO INSPECT NEXT
         B     PPITXT05            NULL COUNT, GO POP STACK
*
PPITXT03 DS    0H
         IC    R15,0(,R1)          LOAD LENGTH
         LTR   R2,R2               HAVE A KEY TO INSPECT ?
         BZ    PPITXT04            NO, SKIP
         LTR   R3,R15              IS LENGTH NULL ?
         BZ    PPITXT04            IGNORE IF SO
*
         CLI   0(R2),DSNAMEK       DSNAME ?
         BE    PPSETDSN                 GO SAVE IT
         CLI   0(R2),SERMK         VOLUME SERIAL ?
         BE    PPSETSER                 GO SAVE IT
         CLI   0(R2),REFMK         VOLUME REF BY DSNAME ?
         BE    PPSETREF                 GO SAVE IT
         CLI   0(R2),SYSPROCK      SYSPROC OPTION ?
         BE    PPSETSYS                 GO SAVE IT
         CLI   0(R2),UNITK         UNIT SPECIFICATION ?
         BE    PPSETUNT                 GO SAVE IT
*
PPITXT04 DS    0H
         SR    R2,R2               KILL KEY INSPECTION TILL NEXT KEY
         LA    R1,1(R15,R1)        -> LENGTH/COUNT/KEY
         BCT   R14,PPITXT02        LOOP FOR REST OF NUMBER/COUNT
*
PPITXT05 DS    0H
         SRA   R0,8                OPERAND END, POP THE NUMBER/COUNT
         BZ    PPITXT01            STACK EMPTY, GO DO NEXT KEY
         LR    R14,R0              SET LOOP COUNTER
         N     R14,PPMSKCLR        ONLY ONE BYTE VALUE
         B     PPITXT02            GO TEST FOR COUNT/LENGTH NEXT
*
         EJECT
********************** EXTRACT PARAMETER DATA *************************
PPSETSYS DS    0H
         TM    PPSTAT1,PPCONCJP    PRIMARY JOBPROC STATEMENT ?
         BO    PPERM801            NO -> ERROR, SYSPROC NOT VALID
         CLC   PPSYES(4),0(R1)     SYSPROC=YES ?
         BE    PPITXT04                 IS DEFAULT, RETURN TO SCAN
         CLC   PPSNO(3),0(R1)      SYSPROC=NO ?
         BNE   PPERM843            ELSE INVALID OPERAND
*
         NI    PPSTAT1,PPALL-PPSYSYES   SAVE NO SYSTEM PROC WANTED
         LA    R4,16               SET MAXIMUM NUMBER OF PROCLIBS
         STH   R4,PPJPMAX          SET IT FOR LATER USE
         B     PPITXT04            CONTINUE SCAN
*
PPSETDSN DS    0H
         OI    PPSTAT2,PPHAVDSN    SHOW DSNAME GIVEN
         STH   R3,PPDSNAML         SAVE STRING LENGTH
         LA    R4,PPDSNTAB         -> TABLE ENTRY FOR PROCESSING
         B     PPTSTMOV            MERGE
*
PPSETSER DS    0H
         OI    PPSTAT2,PPHAVSER    SHOW VOLSER GIVEN
         STH   R3,PPVOLSEL         SAVE STRING LENGTH
         LA    R4,PPSERTAB         -> TABLE ENTRY FOR PROCESSING
         B     PPTSTMOV            MERGE
*
PPSETUNT DS    0H
         OI    PPSTAT2,PPHAVUNT    SHOW UNIT GIVEN
         STH   R3,PPUNITL          SAVE STRING LENGTH
         LA    R4,PPUNTTAB         -> TABLE ENTRY FOR PROCESSING
         B     PPTSTMOV            MERGE
*
PPSETREF DS    0H
         OI    PPSTAT2,PPHAVREF    SHOW VOLUME REF BY DSNAME
         STH   R3,PPVLREFL         SAVE DSNAME LENGTH FOR ALLOC
         LA    R4,PPREFTAB         -> TABLE ENTRY FOR PROCESSING
*
PPTSTMOV DS    0H
         CH    R3,0(,R4)           TEST STRING LENGTH FOR MAX
         BH    PPERM842            "EXCESSIVE PARAMETER LENGTH"
         BCTR  R3,0                GET MC LENGTH FOR MOVE
         EX    R3,2(,R4)           SET STRING ACCORDING TO MOVE
         B     PPITXT04            CONTINUE SCAN
*
PPDSNTAB DC    Y(44)                       MAX STRING LENGTH
         MVC   PPDSNAME(*-*),1(R1)         SHIFT STRING TO WORK AREA
*
PPSERTAB DC    Y(6)
         MVC   PPVOLSER(*-*),1(R1)
*
PPREFTAB DC    Y(44)
         MVC   PPVOLREF(*-*),1(R1)
*
PPUNTTAB DC    Y(8)
         MVC   PPUNIT(*-*),1(R1)
*
         EJECT
********************** CHECK FOR REQUIRED PARMS ***********************
PPTSTOPT DS    0H
         TM    PPSTAT2,PPHAVDSN    IS DSNAME PROVIDED ?
         BZ    PPERM802            ERROR IF MISSING
*
         TM    PPSTAT2,PPHAVUNT    UNIT PARAMETER SPECIFIED ?
         BZ    PPTSNMSS            BRANCH IF NOT
         CLC   PPUNIT,=CL8'3330V'  WAS MSS SPECIFIED AS UNIT?
         BE    PPERM811            ERROR IF SO
         CLC   PPUNIT,=CL8'SDG00'  CHECK LOWER BOUND OF SDGXX NAME
         BL    PPTSNMSS            OK IF LOWER
         CLC   PPUNIT,=CL8'SDG99'  CHECK UPPER BOUND OF SDGXX NAME
         BNH   PPERM811            ERROR IF NOT HIGHER
*
PPTSNMSS DS    0H
*
*        IF IT IS DESIRED TO PROHIBIT ACCESS TO ANY DATA SETS VIA
*        JOBPROC, THE CHECK CAN BE MADE HERE APPROPRIATE MESSAGE
*        ROUTINE (PPERM803) CAN BE BRANCHED TO.
*
         CLC   PPJPCNT(2),PPJPMAX  ARE WE ALREADY AT THE MAXIMUM ?
         BNL   PPERM804            YES, GIVE UP
         SPACE 2
********************* ALLOCATE TO USER PROCLIB ************************
PPGOALOC LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            GET ADDRESSABILITY TO DARB
         MVI   S99VERB,S99VRBAL    INDICATE "ALLOC DSNAME" REQUEST
         LA    R9,PPDATXTL         -> ALLOCATION TEXT POINTERS
         USING S99TUPL,R9          MAKE TEXT PTRS ADDRESSABLE
         LA    R8,PPDDNAMK         -> DDNAME TEXT UNIT SLOT
         USING S99TUNIT,R8         MAKE TEXT UNITS ADDRESSABLE
         MVC   S99TUKEY,=AL2(DALDDNAM) SET DDNAME KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         MVC   S99TULNG,=H'8'      SET DDNAME LENGTH
         MVC   S99TUPAR(7),PPUSRDDN SET BASIC DDNAME
         LH    R2,PPJPCNT          GET CURRENT CONCATENATION NO.
         LA    R2,1(,R2)           BUMP BY ONE
         LA    R15,PPDDNSUF-1(R2)  POINT TO PROPER DDNAME SUFFIX
         MVC   S99TUPAR+7(1),0(R15) SET DDNAME SUFFIX
         ST    R8,S99TUPTR         SET PTR TO DDNAME TEXT UNIT
*
         LA    R8,PPDSNAMK         -> DSNAME TEXT UNIT SLOT
         LA    R9,L'S99TUPTR(,R9)  -> NEXT TEXT UNIT PTR
         MVC   S99TUKEY,=AL2(DALDSNAM) SET "DSNAME" TEXT KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         ST    R8,S99TUPTR         SET PTR TO "DSNAME" TEXT UNIT
         LA    R8,PPSDISPK         -> STATUS DISPOSITION TEXT
*
         LA    R9,L'S99TUPTR(,R9)  -> NEXT TEXT PTR SLOT
         MVC   S99TUKEY,=AL2(DALSTATS) SET "STATUS" DISP KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         MVC   S99TULNG,=AL2(1)    SET LENGTH OF DISP CODE
         ST    R8,S99TUPTR         SET PTR TO "STATUS" DISP TEXT
*
         TM    PPSTAT2,PPHAVSER    VOLUME SERIAL NO. SPECIFIED ?
         BZ    PPASVREF            BRANCH IF NOT
         LA    R8,PPVOLSEK         -> VOLUME SERIAL TEXT UNIT
         LA    R9,L'S99TUPTR(,R9)  -> NEXT TEXT PTR SLOT
         MVC   S99TUKEY,=AL2(DALVLSER) SET "VOL=SER=" TEXT KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         ST    R8,S99TUPTR         SET PTR TO "VOL=SER=" TEXT
*
PPASVREF TM    PPSTAT2,PPHAVREF    "VOL=REF=DSN" SPECIFIED ?
         BZ    PPASUNIT            BRANCH IF NOT
         LA    R8,PPVLREFK         -> VOLUME REFERENCE TEXT UNIT
         LA    R9,L'S99TUPTR(,R9)  -> NEXT TEXT PTR SLOT
         MVC   S99TUKEY,=AL2(DALVLRDS) SET "VOL=REF=" TEXT KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         ST    R8,S99TUPTR         SET PTR TO "VOL=SER=" TEXT
*
PPASUNIT TM    PPSTAT2,PPHAVUNT    UNIT PARAMETER SPECIFIED ?
         BZ    PPAENDTL            BRANCH IF NOT
         LA    R8,PPUNITK          -> "UNIT" TEXT UNIT
         LA    R9,L'S99TUPTR(,R9)  -> NEXT TEXT PTR SLOT
         MVC   S99TUKEY,=AL2(DALUNIT) SET "UNIT" TEXT KEY
         MVC   S99TUNUM,=AL2(1)    SET NUMBER OF OPERANDS
         ST    R8,S99TUPTR         SET PTR TO "UNIT" TEXT
*
PPAENDTL MVI   S99TUPTR,S99TUPLN   SET END OF TEXT UNITS INDICATOR
         LA    R1,PPDAARGL         R1 -> DYNAMIC ALLOC PARM LIST
         OI    PPSTAT0,PPDACODE    INDICATE WE ARE IN ALLOC. CODE
         DYNALLOC ,                ALLOCATE TO USER PROCLIB
         NI    PPSTAT0,PPALL-PPDACODE INDICATE WE ARE BACK
         LTR   R15,R15             ALLOCATION SUCCESSFUL ?
         BZ    PPADAOK             BRANCH IF YES
         BAL   R10,PPDAERRA        DO ERROR ANALYSIS
         B     PPRETURN            RETURN TO CALLER
*
PPADAOK  STH   R2,PPJPCNT          UPDATE CONCATENATION NO.
         BCTR  R2,0                REDUCE COUNT FOR SHIFT
         ICM   R1,12,=X'8000'      INSERT BIT MASK
         SRL   R1,0(R2)            SHIFT TO CORRECT BIT POSITION
         ICM   R2,12,PPALLOC1      GET CURRENT ALLOCATION MASK
         OR    R2,R1               GET NEW ALLOCATION STATUS
         STCM  R2,12,PPALLOC1      SET NEW ALLOCATION STATUS
         PPDEBUG END1              TRACE END OF PP1
*
         L     R1,PPTIOTAD         LOAD THE TIOT ADDRESS
         USING TIODSECT,R1         SET UP ADDRESSIBILITY
         SLR   R15,R15             ZERO FOR INSERTS
*
PPATIOLP CLI   TIOELNGH,0          CHECK FOR END OF TIOT
         BE    PPERM811            BRANCH IF NAME NOT FOUND (ERROR)
         CLC   TIOEDDNM,PPDDNAME   CHECK FOR THE ALLOCATED DD ENTRY
         BE    PPATIOTF            BRANCH IF FOUND
         IC    R15,TIOELNGH        LOAD THE LENGTH
         AR    R1,R15              INCREMENT ADDRESS
         B     PPATIOLP            CONTINUE TIOT SCAN
*
PPATIOTF ICM   R1,7,TIOEFSRT       LOAD THE UCB ADDRESS
         BZ    PPERM811            IF NONE, THIS IS VERY BAD
         USING UCBDSECT,R1         TELL THE ASSEMBLER ABOUT THE UCB
*
         TM    UCBTBYT3,UCB3DACC   TEST FOR DIRECT ACCESS
         BZ    PPERM811            IF NOT, THIS CANNOT BE ALLOWED
         TM    UCBTBYT2,UCBRVDEV   TEST FOR VIRTUAL DEVICE
         BO    PPERM811            THIS IS ALSO NOT ALLOWED
*
         DROP  R1
*
         PPDEBUG END1              TRACE END OF PP1
*
         B     PPRETURN            RETURN TO CONVERTER
*
         DROP  R8,R9
         EJECT ,
***********************************************************************
*        THE FOLLOWING ARE THE ERROR MESSAGE INTERFACES TO IEFVPPM    *
*        FOR THE JOBPROC DD STATEMENT PROCESSOR (IEFVPP1).            *
***********************************************************************
*
PPERM800 DS    0H
         LA    R2,MSG800I          SYNTAX ERROR IN JOBPROC STMT
         B     PPERRNSL            GO PRODUCE ERROR MESSAGE
*
PPERM801 DS    0H
         LA    R2,MSG801I          ILLEGAL USE OF "SYSPROC" KEY
         B     PPERRNSL            GO PRODUCE ERROR MESSAGE
*
PPERM802 DS    0H
         LA    R2,MSG802I          JOBPROC DSNAME MISSING
         B     PPERRNSL
*
PPERM803 DS    0H
         LA    R2,MSG803I          DATA SET NOT VALID FOR JOBPROC
         B     PPERRNSL
*
PPERM804 DS    0H
         LA    R2,MSG804I          CONCATENATION LIMIT EXCEEDED
         B     PPERRNSL
*
PPERM811 DS    0H
         LA    R2,MSG811I          JOBPROC VOLUME NOT ALLOWED
         B     PPERRNSL
*
PPERM836 DS    0H
         LA    R2,MSG836I          MISPLACED OR DUPLICATE JOBPROC
         B     PPERRNSL
*
PPERM842 DS    0H
         LA    R2,MSG842I          EXCESSIVE PARAMETER LENGTH (SYSPROC)
         B     PPERRNSL
*
PPERM843 DS    0H
         LA    R2,MSG843I          UNIDENTIFIED PARAMETER IN SYSPROC
         B     PPERRNSL
         SPACE 2
PPERRNSL DS    0H
         BAL   R14,IEFVPPM         PRODUCE ERROR MESSAGE
         PPDEBUG ERR1              TRACE ERROR IN PP1
*
         B     PPRETURN            RETURN TO CALLER
*
         DROP  R11,R7
*
         TITLE '**** IEFVPP2 - SET USER PROCLIB ENVIRONMENT ****'
***********************************************************************
*                                                                     *
* IEFVPP2                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED WHEN THE FIRST EXEC STATEMENT IS TO BE       *
* PROCESSED.                                                          *
*                                                                     *
*      1) A CHECK IS MADE TO SEE IF A JOBPROC DD CARD HAS BEEN        *
*         DETECTED BY IEFVPP1. IF NOT, CONTROL IS IMMEDIATELY PASSED  *
*         BACK TO IEFVFA.                                             *
*                                                                     *
*      2) IF THE JOBPROC DEFINES A CONCATENATION OR THE "SYSPROC=YES" *
*         OPTION HAS BEEN CHOSEN, THE OS/VS DYNAMIC ALLOCATION        *
*         ROUTINES ARE CALLED TO CONCATENATE THE DATA SETS ALLOCATED  *
*         BY IEFVPP1.                                                 *
*                                                                     *
*      3) THE USER PROCLIB DCB IS OPENED. IF AN ERROR IS DETECTED     *
*         BY THE OPEN ROUTINES (IE. S213, ETC.), THEN AN ERROR        *
*         MESSAGE IS GIVEN AND CONTROL IS RETURNED TO IEFVFA.         *
*                                                                     *
*      4) IF THE PROCLIB BUFFER OBTAINED BY CONVERTER INITIALIZATION  *
*         IS SMALLER THAN THE USER'S PROCLIB BLKSIZE, A NEW BUFFER IS *
*         OBTAINED AND ITS ADDRESS SET IN THE CONVERTER WORK AREA.    *
*                                                                     *
*      5) ALL ERRORS ENCOUNTERED DURING IEFVPP2 CAUSE AN APPROPRIATE  *
*         ERROR MESSAGE TO BE ISSUED AND THE JOB BEING PROCESSED TO   *
*         FAIL WITH A JCL ERROR.                                      *
*                                                                     *
*      6) CONTROL IS PASSED BACK TO IEFVFA.                           *
*                                                                     *
* INPUT:                                                              *
*        IWA(PDCBP)    -> ORIGINAL PROCLIB DCB                        *
*        IWA(IWAINTS4) -> ORIGINAL PROCLIB BUFFER                     *
*                                                                     *
* OUTPUT:                                                             *
*        REGISTERS RESTORED                                           *
*        IWA(PDCBP)    -> JOBPROC DCB                                 *
*        IWA(IWAINTS4) -> TEMPORARY PROCLIB BUFFER (IF NEEDED)        *
*        IWA(PSTMT) -> TEMPORARY PROCLIB BUFFER (IF NEEDED COPY)      *
*                                                                     *
* EXTERNALS:                                                          *
*        IEFVPPM FOR ERROR CONDITION                                  *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER VIA STANDARD RETURN MACRO                   *
*                                                                     *
* SVC:                                                                *
*        ESTAE (SVC 60)                                               *
*        DYNAMIC ALLOCATION (SVC 99)                                  *
*        OPEN (SVC 19)                                                *
*        GETMAIN (SVC 120)                                            *
*                                                                     *
***********************************************************************
         EJECT
IEFVPP2  PPEPA VPP2                IEFVPP2 ENTRY POINT ADDRESS
         EJECT
************************* STATUS ROUTING ******************************
         TM    PPSTAT1,PPHAVJP     HAVE JOBPROC TO PROCESS ?
         BZ    PPRETURN            NO, RETURN
         PPDEBUG ENT2              TRACE ENTRY INTO PP2
*
         TM    PPSTAT1,PPJPERR     ERROR NOTED IN JOBPROC ?
         BO    PPRETURN            SO JUST RETURN
         LH    R0,PPJPCNT          LOAD DD CARD COUNT
         LTR   R0,R0               TEST IF ANY
         BZ    PPRETURN            FORGET THE WHOLE THING
*
         TM    PPTRPP2,PPTRPP2E    HAVE WE BEEN HERE BEFORE ?
         BO    PPRETURN            YES, MUST HAVE BEEN A FAILURE
*                                   DURING PROCESSING OF FIRST
*                                    EXEC STATEMENT WHICH TURNED
*                                     OUT TO BE AN "EXEC PROC="
         OI    PPTRPP2,PPTRPP2E    INDICATE PROCLIB OPEN HAS BEEN
*                                   TRIED FOR CURRENT JOB IN CASE
*                                    CONVERTER RE-ENTERS IEFVPP2
*                                     FOR THE SAME JOB
         EJECT
*********************** CONCATENATION *********************************
         TM    PPSTAT1,PPCONCJP    REQUIRE CONCATENATION ?
         BO    PPCONCAT            YES, PERFORM CONCATENATION
         TM    PPSTAT1,PPSYSYES    TEST IF SYSTEM PROCLIB(S) WANTED
         BZ    PPOPEN              NO, SKIP
         TM    PPMISC,PPNOSYSP     TEST IF SYSPROC UNAVAILABLE
         BO    PPOPEN              YES, SKIP CONCATENATION
*
PPCONCAT LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            GET DARB ADDRESSABILITY
         MVI   S99VERB,S99VRBCC    INDICATE CONCATENATION REQUEST
         LA    R9,PPDATXTL         -> ALLOCATION TEXT PTR SLOTS
         USING S99TUPL,R9          GET TEXT LIST ADDRESSABILITY
         LA    R8,PPCONCDK         -> CONCATENATION TXT UNIT SLOT
         USING S99TUNIT,R8         GET TEXT UNIT ADDRESSABILITY
         MVC   S99TUKEY,=AL2(DCCDDNAM) SET CONCATENATE DDNAME KEY
         ST    R8,S99TUPTR         SET PTR TO CONCATENATE TEXT
         MVI   S99TUPTR,S99TUPLN   FLAG END OF TEXT UNIT PTR'S
         LA    R1,S99TULNG         -> FIRST DDNAME SLOT
         LA    R15,PPDDNSUF        -> DDNAME SUFFIX TABLE
         LH    R0,PPJPCNT          R0 = COUNT OF DDNAMES
         LR    R14,R0              COPY INTO R14
         USING S99TUFLD,R1         MAKE TEXT PARMS ADDRESSABLE
*
PPCTUBLD MVC   S99TULEN,=H'8'      SET DDNAME LENGTH
         MVC   S99TUPRM(7),PPUSRDDN SET BASIC DDNAME
         MVC   S99TUPRM+7(1),0(R15) MOVE IN DDNAME SUFFIX
         LA    R1,L'S99TULEN+8(,R1) BUMP TO NEXT DDNAME SLOT
         LA    R15,1(,R15)         BUMP TO NEXT SUFFIX ENTRY
         BCT   R0,PPCTUBLD         CONTINUE TO BUILD DDNAME LIST
*
         TM    PPSTAT1,PPSYSYES    "SYS1.PROCLIB" REQUIRED
         BZ    PPCNSYSP            BRANCH IF NOT
         TM    PPMISC,PPNOSYSP     TEST FOR SYSPROC UNAVAILABLE
         BO    PPCNSYSP            SKIP SYSTEM ADD ON IF SO
         MVC   S99TULEN,=H'8'      SET PROCLIB DDNAME LENGTH
         MVC   S99TUPRM(8),PPSYSNAM GET SYSTEM PROCLIB INTO LIST
         DROP  R1                  DROP TEXT UNIT FIELD ADDR.
         LA    R14,1(,R14)         BUMP COUNT BY 1 FOR PROCLIB
*
PPCNSYSP STH   R14,S99TUNUM        SET NO. OF DDNAMES TO CONC.
         LA    R1,PPDAARGL         -> ALLOC REQUEST PARM PTR
         OI    PPSTAT0,PPDACODE    INDICATE WE ARE IN ALLOC. CODE
         DYNALLOC ,                CONCATENATE THE PROCLIB'S
         NI    PPSTAT0,PPALL-PPDACODE INDICATE WE ARE BACK
         LTR   R15,R15             WAS CONCATENATION OK ?
         BZ    PPCSETCI            BRANCH IF YES
         BAL   R10,PPDAERRA        DO ERROR ANALYSIS
         B     PPRETURN            RETURN TO CALLER
*
         DROP  R8,R9
*
PPCSETCI OI    PPSTAT1,PPCONCD     SHOW FILES CONCATENATED
         PPDEBUG CON2              TRACE CONCATENATION IN PP2
*
         EJECT
*************************** OPEN **************************************
PPOPEN   DS    0H
         OI    PPTRPP2,PPTROPEN    SET "OPEN" TRACE EVENT
         MVI   PPOCLST,PPVLBIT     SET LISTEND
         OPEN  (PPDCB),MF=(E,PPOCLST)  OPEN THE FILE
*
PPTOPEN  DS    0H
         NI    PPTRPP2,PPALL-PPTROPEN RESET "OPEN" TRACE EVENT ?
         NI    PPSTAT0,PPALL-PPECODE RESET IN CASE WE HAD ERROR
         TM    PPTRPP2,PPTRS213    DID WE GET "DATA SET NOT FOUND"
         BNO   PPOPENCK            BRANCH IF NOT
         NI    PPTRPP2,PPALL-PPTRS213 RESET "NOT FOUND" FLAG
         LA    R2,MSG807I          SET "NOT FOUND" MESSAGE KEY
         B     PP2ERROR            GIVE "NOT FOUND" MESSAGE
*
PPOPENCK TM    DCBOFLGS-IHADCB+PPDCB,DCBOFOPN DID FILE OPEN OK ?
         BZ    PPERM812            NO, SKIP
         OI    PPSTAT1,PPOPENED    ELSE SAVE STATUS
         PPDEBUG OPN2              TRACE OPEN IN PP2
*
         EJECT
*************************** BUFFER ************************************
         LH    R2,DCBBLKSI-IHADCB+PPDCB  LOAD REQUIRED BUFFER LEN
         L     R3,PDCBP            -> STANDARD PROCLIB DCB
         USING IHADCB,R3
         CH    R2,DCBBLKSI         COMPARE BLOCK SIZES
         BNL   *+10                BRANCH IF NEW NOT SMALLER
         MVC   DCBBLKSI-IHADCB+PPDCB,DCBBLKSI REPLACE SMALLER BLKSIZE
         BNH   PPSETDCB            NO NEED FOR NEW
         GETMAIN RC,LV=(R2)        NOW GETMAIN BUFFER
         LTR   R15,R15             ALL OK ?
         BNZ   PPERM810            NOGO
         ST    R1,PPBUFAD          SAVE PTR TO OUR BUFFER
         OI    PPSTAT1,PPBUFGOT    SHOW BUFFER GOTTEN
         MVC   PPSBUFA(4),IWAINTS4 SAVE ORIG BUFFER PTR
         MVC   IWAINTS4+1(3),PPBUFAD+1   SET NEW
         MVC   PSTMT+1(3),PPBUFAD+1      SET NEW (COPY)
PPSETDCB DS    0H
         MVC   DCBREAD+1-IHADCB+PPDCB(3),DCBREAD+1 SHIFT FIELDS THAT
         MVC   DCBPOINT+1-IHADCB+PPDCB(3),DCBPOINT+1   CONVERTER
         MVC   DCBEODAD+1-IHADCB+PPDCB(3),DCBEODAD+1   LIKES TO
         MVC   DCBSYNAD+1-IHADCB+PPDCB(3),DCBSYNAD+1   PLAY WITH
         MVC   PPSDCBA(4),PDCBP    SAVE ORIG DCB ADDR
         MVC   PDCBP+1(3),PPOCLST+1    AND SET TO NEW
         PPDEBUG END2              TRACE END OF PP2
*
         B     PPRETURN            RETURN
         DROP  R3
         EJECT
********************** ERROR HANDLERS *********************************
PPERM812 DS    0H
         LA    R2,MSG812I          USER PROCLIB OPEN FAILED
         B     PP2ERROR
         SPACE 1
PPERM810 DS    0H
         LA    R2,MSG810I          INSUFFICIENT BUFFER STORAGE
         SPACE 2
PP2ERROR DS    0H
         BAL   R14,IEFVPPM         GIVE USER HIS ERROR
         PPDEBUG ERR2              TRACE ERROR IN PP2
*
         B     PPRETURN            RETURN TO CALLER
*
         DROP  R11,R7
*
         TITLE '**** IEFVPP3 - JOB END CLEANUP ****'
***********************************************************************
*                                                                     *
* IEFVPP3                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED AT THE END OF PROCESSING FOR A JOB.          *
*                                                                     *
*    1  RESTORE ORIGINAL PROCLIB DCB ADDRESS.                         *
*    2  CLOSE THE PRIVATE PROCLIB DCB.                                *
*    3  DE-CONCATENATE THE PRIVATE AND SYSTEM PROCLIB DATASETS.       *
*    4  DE-ALLOCATE THE PRIVATE PROCLIB DATASET.                      *
*    5  FREEMAIN THE PRIVATE PROCLIB WORK AREA.                       *
*    6  RETURN TO CALLER OR ABORT IF ERROR.                           *
*                                                                     *
* INPUT:                                                              *
*        IWA(PDCBP)    -> JOBPROC DCB                                 *
*        IWA(IWAINTS4) -> TEMPORARY PROCLIB BUFFER                    *
*                                                                     *
* OUTPUT:                                                             *
*        REGISTERS RESTORED                                           *
*        IWA(PDCBP)    RESTORED FOR ORIG PROCLIB DCB                  *
*        IWA(IWAINTS4) RESTORED FOR ORIG PROCLIB BUFFER               *
*        IWA(PSTMT) RESTORED FOR ORIG PROCLIB BUFFER - COPY           *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE                                                         *
*                                                                     *
* EXITS:                                                              *
*        NORMAL    - RETURN TO CALLER                                 *
*        ERROR     - ABORT                                            *
*                                                                     *
* SVC:                                                                *
*        ESTAE (SVC 60)                                               *
*        FREEMAIN (SVC 10)                                            *
*        CLOSE (SVC 20)                                               *
*        DYNALLOC (SVC 99)                                            *
*        WTO (SVC 35)                                                 *
*        DEQ (SVC 48)                                                 *
*        ABEND (SVC 13)                                               *
*                                                                     *
***********************************************************************
         EJECT
IEFVPP3  PPEPA VPP3                IEFVPP3 ENTRY POINT ADDRESS
         SPACE 1
         TM    PPSTAT1,PPHAVJP     WAS JOBPROC DD NOTED ?
         BZ    PPDADEQ             NO, GO DEQUEUE THE DDNAME
         PPDEBUG ENT3              TRACE ENTRY TO PP3
*
         NI    PPTRPP2,PPALL-PPTRPP2E RESET "IEFVPP2 ENTERED" FLAG
         EJECT
**************************** CLOSE ************************************
         TM    PPSTAT1,PPOPENED    WAS PRIVATE PROCLIB DCB OPENED ?
         BZ    PPDCONC             NO, SKIP
         TM    PPSTAT1,PPBUFGOT    WAS BUFFER GOTTEN ?
         BZ    PPCLOSE             NO, SKIP
         L     R1,IWAINTS4         -> GOTTEN BUFFER
         LH    R0,DCBBLKSI-IHADCB+PPDCB  LOAD BUFFER LENGTH
         FREEMAIN R,LV=(0),A=(1)   RELEASE IT
         NI    PPSTAT1,PPALL-PPBUFGOT   CLEAR STATUS
         MVC   IWAINTS4+1(3),PPSBUFA+1   RESET ORIG BUFFER ADDR
         MVC   PSTMT+1(3),PPSBUFA+1      RESET ORIG BUFFER ADDR
PPCLOSE  DS    0H
         L     R3,PPSDCBA          -> ORIGINAL PROCLIB DCB ADDRESS
         USING IHADCB,R3
         MVC   DCBEODAD+1(3),DCBEODAD+1-IHADCB+PPDCB PASS ON ANY
         MVC   DCBSYNAD+1(3),DCBSYNAD+1-IHADCB+PPDCB   CHANGES
         MVC   DCBREAD+1(3),DCBREAD+1-IHADCB+PPDCB
         MVC   DCBPOINT+1(3),DCBPOINT+1-IHADCB+PPDCB
         DROP  R3
         MVC   PDCBP+1(3),PPSDCBA+1    RESTORE ORIGINAL DCB ADDR
         MVI   PPOCLST,PPVLBIT     SET VL FOR CLOSE PARMLIST
         CLOSE (PPDCB),MF=(E,PPOCLST)   CLOSE THE FILE
         NI    PPSTAT1,PPALL-PPOPENED   CLEAR OPEN STATUS
         PPDEBUG CLS3              TRACE CLOSE IN PP3
*
         EJECT
************************** DE-CONCATENATION ***************************
PPDCONC  DS    0H
         TM    PPSTAT1,PPCONCD     ANY CONCATENATIONS DONE ?
         BZ    PPDALLOC            NO, SKIP
         BAL   R10,PPDECONC        CALL THE DE-CONCATENATOR
         PPDEBUG DCN3              TRACE DECONCATENATION IN PP3
*
         EJECT
************************** DE-ALLOCATION ******************************
PPDALLOC DS    0H
         NI    PPSTAT1,PPALL-PPCONCD CLEAR CONCATENATION STATUS
         LH    R0,PPJPCNT          GET COUNT OF CONCATENATIONS
         LTR   R0,R0               DO WE HAVE ANYTHING TO FREE ?
         BZ    PPDADEQ             NO, GO RESET STATUS FLAGS
*
         BAL   R10,PPDEALOC        GO DEALLOCATE THE PROCLIB(S)
         PPDEBUG DAL3              END OF DEALLOCATION
*
PPDADEQ  MVC   PPLOCWRK(PPENQPL),PPENQLST  MOVE PARAMETER LIST
         LA    R1,PPSYSNAM         POINT AT MINOR NAME
         ST    R1,PPLOCWRK+8       STORE MINOR NAME ADDRESS
         DEQ   MF=(E,PPLOCWRK)     DEQUEUE THE PROCLIB NAME
*
         TM    PPMISC,PPESTAE      TEST IF ESTAE ESTABLISHED
         BZ    PP3NESTA            BRANCH IF NOT
         ESTAE 0                   CANCEL ERROR RECOVERY
         NI    PPMISC,PPALL-PPESTAE     TURN OFF ESTAE EXISTS FLAG
*
PP3NESTA DS    0H
         XC    &ANCHOR,&ANCHOR     CLEAR ANCHOR WORD
         L     R13,4(,R13)         UNCHAIN SAVE AREAS
*
         LR    R1,R6               LOAD THE WORK AREA ADDRESS
         LA    R0,PPWORKLN         LOAD LENGTH OF WORK AREA
         FREEMAIN R,A=(1),LV=(0)   FREE THE WORK AREA
*
         LM    R14,R12,12(R13)     RELOAD SAVED REGISTERS
         BR    R14                 RETURN TO CALLER
*
      TITLE '**** COMMON DE-CONCATENATION AND DEALLOCATION ****'
***********************************************************************
*                                                                     *
* PPDECONC                                                            *
* ********                                                            *
*                                                                     *
*        THIS ROUTINE ATTEMPTS TO DE-CONCATENATE IEF*PDS0, RESULTING  *
*    IN IEF*PDS0, IEF*PDS1, ..., AND IEF*PDSI.                        *
*                                                                     *
* INPUT:                                                              *
*        R10 -> RETURN ADDRESS OF CALLER (EITHER IEFVPP0 OR IEFVPP3). *
*                                                                     *
* OUTPUT:                                                             *
*        EITHER SUCCESSFUL DE-CONCATENTION OR AN ERROR MESSAGE.       *
*        IN THE CASE OF A DE-CONCATENATION REQUEST FROM IEFVPP0       *
*        RETURN IS MADE IN ALL CASES.  A DE-CONCATENATION ERROR IN    *
*        IEFVPP3 WILL RESULT IN CONVERTER TERMINATION (ABEND).        *
*                                                                     *
* PPDEALOC                                                            *
* ********                                                            *
*                                                                     *
*        THIS ROUTINE ATTEMPTS TO DE-ALLOCATE IEF*PDS0, IEF*PDS1,     *
*    ..., IEF*PDSF.  IN THE CASE OF BEING CALLED BY IEFVPP0,          *
*    DEALLOCATION IS ATTEMPTED ON ALL 16 DDNAMES WITHOUT REGARD TO    *
*    ANY ERRORS ENCOUNTERED.  WHEN CALLED BY IEFVPP3, DEALLOCATION    *
*    WILL ABEND THE CONVERTER IF AN ERROR IS ENCOUNTERED.             *
*                                                                     *
* INPUT:                                                              *
*        R10 -> RETURN ADDRESS OF CALLER (EITHER IEFVPP0 OR IEFVPP3). *
*                                                                     *
* OUTPUT:                                                             *
*        EITHER SUCCESSFUL DE-CONCATENTION OR AN ERROR MESSAGE.       *
*        IN THE CASE OF A DE-CONCATENATION REQUEST FROM IEFVPP0       *
*        RETURN IS MADE IN ALL CASES.  A DE-CONCATENATION ERROR IN    *
*        IEFVPP3 WILL RESULT IN CONVERTER TERMINATION (ABEND).        *
*                                                                     *
* EXTERNALS:                                                          *
*        DYNAMIC ALLOCATION PARAMETER LIST                            *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER OR ABEND.                                   *
*                                                                     *
* SVC:                                                                *
*        DYNALLOC (SVC 99)                                            *
*                                                                     *
***********************************************************************
*
PPDECONC DS    0H
         LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            GET DARB ADDRESSABILITY
         MVI   S99VERB,S99VRBDC    INDICATE DE-CONCAT REQUEST
         LA    R9,PPDATXTL         -> TEXT UNIT PTR SLOTS
         USING S99TUPL,R9          GET TEXT PTR ADDRESSABILITY
         LA    R8,PPCONCDK         -> DE-CONCATENATE TEXT UNIT
         USING S99TUNIT,R8         GET TEXT UNIT ADDRESSABILITY
         MVC   S99TUKEY,=AL2(DDCDDNAM) SET DE-CONCATENATE KEY
         MVC   S99TUNUM,=AL2(1)    SET NO. OF DDNAMES
         MVC   S99TULNG,=H'8'      SET DDNAME LENGTH
         MVC   S99TUPAR(8),PPUSRDDN SET DDNAME FOR DE-CONCAT
         ST    R8,S99TUPTR         SET PTR TO DE-CONC TEXT UNIT
         MVI   S99TUPTR,S99TUPLN   FLAG END OF TEXT UNIT PTR'S
         LA    R1,PPDAARGL         -> DYNAMIC ALLOC PARM PTR
         OI    PPSTAT0,PPDACODE    INDICATE WE ARE IN ALLOC. CODE
         DYNALLOC ,                DE-CONCATENATE PROCLIB'S
         NI    PPSTAT0,PPALL-PPDACODE INDICATE WE ARE BACK
         LTR   R15,R15             WAS DE-CONCATENATION OK ?
         BZR   R10                 RETURN IF YES
         B     PPDAERRA            DO ERROR ANALYSIS
*                                   AND MAYBE RETURN
         DROP  R8,R9
*
PPDEALOC DS    0H
         LA    R2,16               SET MAXIMUM TO DEALLOCATE
         LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            GET DARB ADDRESSABILITY
         MVI   S99VERB,S99VRBUN    INDICATE UNALLOC REQUEST
         LA    R9,PPDATXTL         -> TEXT UNIT PTR SLOTS
         USING S99TUPL,R9          GET TEXT PTR ADDRESSABILITY
         LA    R8,PPDDNAMK         -> DDNAME TEXT UNIT
         USING S99TUNIT,R8         GET TEXT UNIT ADDRESSABILITY
         MVC   S99TUKEY,=AL2(DUNDDNAM) SET DDNAME TEXT KEY
         MVC   S99TUNUM,=AL2(1)    SET NO. OF DDNAMES
         MVC   S99TULNG,=H'8'      SET DDNAME LENGTH
         ST    R8,S99TUPTR         SET PTR TO DDNAME TEXT UNIT
         MVI   S99TUPTR,S99TUPLN   FLAG END OF TEXT UNIT PTR'S
*
PPDADDNM MVC   S99TUPAR(7),PPUSRDDN SET BASIC DDNAME
         LR    R15,R2              R15 = CURRENT CONCATENATION NO.
         BCTR  R15,0               DECREMENT FOR SHIFT
         ICM   R3,15,=X'00008000'  INSERT BIT MASK
         SRL   R3,0(R15)           SHIFT TO CORRECT BIT POSITION
         LA    R4,PPALLOC1+1       -> CURRENT ALLOCATION STATUS 2
         CH    R2,=H'8'            ALLOCATION NO. LE 8
         BH    *+12                BRANCH IF NOT
         LA    R4,PPALLOC1         POINT TO CORRECT STATUS BYTE
         SRL   R3,8                SHIFT MASK TO LOW ORDER BYTE
         EX    R3,PPDATEST         IS DDNAME ALLOCATED ?
         BNO   PPDANEXT            BRANCH IF NOT
*
         LA    R15,PPDDNSUF-1(R2)  POINT TO PROPER SUFFIX
         MVC   S99TUPAR+7(1),0(R15) SET DDNAME SUFFIX
         LA    R1,PPDAARGL         -> DYNAMIC ALLOC PARM PTR
         OI    PPSTAT0,PPDACODE    INDICATE WE ARE IN ALLOC. CODE
         DYNALLOC ,                FREE A PROCLIB DATA SET
         NI    PPSTAT0,PPALL-PPDACODE INDICATE WE ARE BACK
*
         LTR   R15,R15             DID DE-ALLOCATE GO OK ?
         BZ    PPDANEXT            BRANCH IF YES
         TM    PPSTAT0,PP0CODE     ARE WE IN CLEANUP MODE?
         BNO   PPDAERRA            NO, GO BOMB THE MOTHER
*
PPDANEXT BCT   R2,PPDADDNM         DE-ALLOCATE ALL PROCLIB'S
*
         XC    PPJPCNT,PPJPCNT     RESET CONCATENATION COUNTER
         XC    PPSTAT1(2),PPSTAT1  CLEAR REMAINING STATUS FLAGS
         XC    PPALLOC1(2),PPALLOC1 AND ALLOCATION FLAGS
         BR    R10                 RETURN TO CALLER
*
PPDATEST TM    0(R4),*-*           *** EXECUTE ONLY ***
*
         DROP  R8,R9
*
      TITLE '**** COMMON EXIT ROUTINE FOR MOST ENTRIES TO IEFVPP ****'
***********************************************************************
*                                                                     *
* PPRETURN                                                            *
* ********                                                            *
*                                                                     *
*        THIS ROUTINE PERFORMS FINAL CLEANUP PRIOR TO PASSING CONTROL *
*    BACK TO ONE OF THE CALLING CONVERTER MODULES.                    *
*                                                                     *
* INPUT:                                                              *
*        R13 -> SAVE AREA IN PRIVATE PROCLIB EXTENSION TO CONVERTER   *
*        WORK AREA.                                                   *
*                                                                     *
* OUTPUT:                                                             *
*        CURRENT ERROR RECOVERY ENVIRONMENT, IF ANY, IS CANCELLED,    *
*        TRACE FLAGS ARE RESET, AND CONTROL IS PASSED BACK TO         *
*        THE CALLER VIA A REGISTER 14.                                *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE                                                         *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO IEFVPPX CALLER VIA RETURN PROTOCOL.                *
*                                                                     *
* SVC:                                                                *
*        ESTAE (SVC 60)                                               *
*                                                                     *
***********************************************************************
*
PPRETURN DS    0H
         TM    PPMISC,PPESTAE      TEST IF ESTAE ESTABLISHED
         BZ    PPLEAVE             BRANCH IF NOT
         ESTAE 0                   CANCEL ERROR RECOVERY
         NI    PPMISC,PPALL-PPESTAE     TURN OFF ESTAE EXISTS FLAG
*
***********************************************************************
*        CLEAR EXECUTION TRACE INDICATORS                             *
***********************************************************************
PPLEAVE  DS    0H
         NI    PPSTAT0,PPALL-PP1CODE-PP2CODE-PP3CODE-PPECODE
*
         L     R13,4(,R13)         UNCHAIN SAVE AREAS
         LM    R14,R12,12(R13)     RELOAD SAVED REGISTERS
         BR    R14                 RETURN TO CALLER
*
         TITLE '**** DYNAMIC ALLOCATION ERROR ANALYSIS ****'
***********************************************************************
*                                                                     *
* PPDAERRA                                                            *
* ********                                                            *
*                                                                     *
*     THIS ROUTINE ANALYZES ERRORS DETECTED BY THE OS/VS DYNAMIC      *
*  ALLOCATION ROUTINES. COMMON ERRORS WILL CAUSE A SPECIFIC ERROR     *
*  MESSAGE TO BE PRODUCED WHILE ALL OTHERS WILL CAUSE A WTP MESSAGE   *
*  GIVING A LESS PRECISE ERROR DESCRIPTION. IN THIS LATTER CASE       *
*  THE USER WILL ALSO RECEIVE MESSAGE XXX808I INFORMING HIM THAT      *
*  HE SHOULD EXAMINE THE WTP MESSAGE FOR ERROR DIAGNOSIS.             *
*                                                                     *
* INPUT:                                                              *
*        R15 = DYNAMIC ALLOCATION RETURN CODE (NOT = 0).              *
*                                                                     *
* OUTPUT:                                                             *
*        ERROR MESSAGE IS PRODUCED VIA IEFVPPM AND CONTROL IS PASSED  *
*        TO THE CALLER VIA R10.                                       *
*                                                                     *
*        CWA(PPDARB)   - OUR DYNAMIC ALLOCATE PARAMETER BLOCK.        *
*        CWA(PPDFRETC) - RETURN CODE HOLD AREA FOR DAIRFAIL(IKJEFF18) *
*                                                                     *
* EXTERNALS:                                                          *
*        IKJEFF18 (DAIRFAIL SERVICE ROUTINE) - ISSUES WTP MESSAGE.    *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER VIA R10.                                    *
*        ABEND THE CONVERTER BECAUSE OF UNRECOVERABLE ERROR.          *
*                                                                     *
* SVC:                                                                *
*        LINK (SVC 6)                                                 *
*        LOCATE (SVC 26)                                              *
*        WTO (SVC 35)                                                 *
*        ABEND (SVC 13)                                               *
*                                                                     *
***********************************************************************
         EJECT
PPDAERRA LA    R9,PPDARB           -> DYNAMIC ALLOC REQUEST BLOCK
         USING S99RB,R9            MAKE DARB ADDRESSABLE
         ST    R15,PPDFRETC        SET RETURN CODE FOR DAIRFAIL
*
***********************************************************************
*               ANALYZE ALLOCATION TIME ERRORS                        *
***********************************************************************
         CLI   S99VERB,S99VRBAL    IS THIS AN ALLOCATION FAILURE
         BNE   PPDAERRC            NO, TRY CONCATENATION
         CLI   S99ERROR,LOCATE     IS THIS A CATALOG LOCATE ERROR
         BE    PPERM805            GO GIVE ERROR MESSAGE
         CLI   S99ERROR,OBTAIN     IS THIS AN OBTAIN ERROR ?
         BE    PPERM807            GO GIVE ERROR MESSAGE
         CLI   S99ERROR,DALRNOTA   IS THIS A "RESOURCE" ERROR ?
         BNE   PPERM808            NO, GIVE ERROR CODE IN MESSAGE
         CLI   S99ERROR+1,DALDSNEX EXCLUSIVE "DSN" REQUEST ?
         BE    PPERM813            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALDSNNA DATA SET CURRENTLY "IN USE" ?
         BE    PPERM813            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALUNITE UNIT NOT AVAILABLE ?
         BE    PPERM814            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALUNITI INVALID UNIT NAME ?
         BE    PPERM814            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALVOLNM VOLUME NOT MOUNTED ?
         BE    PPERM806            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALVOLNA VOLUME NOT AVAILABLE ?
         BE    PPERM806            YES, GIVE ERROR MESSAGE
         CLI   S99ERROR+1,DALCVOLE CVOL NOT MOUNTED ?
         BE    PPERM815            YES, GIVE ERROR MESSAGE
         B     PPERM808            GIVE GENERAL FAILURE MESSAGE
*
***********************************************************************
*                 ANALYZE CONCATENATION ERRORS                        *
***********************************************************************
PPDAERRC CLI   S99VERB,S99VRBCC    CONCATENATION FAILURE ?
         BE    PPERM809            GIVE ERROR MESSAGE
*
***********************************************************************
*                 ANALYZE DE-CONCATENATION ERRORS                     *
***********************************************************************
PPDAERRD CLI   S99VERB,S99VRBDC    DE-CONCATENATION FAILURE ?
         BNE   PPDAERRR            NO, TRY DE-ALLOCATION
         CLC   S99ERROR,=X'0438'   DDNAME NOT FOUND ?
         BNE   PPERM808            NO, GIVE MESSAGE AND ABEND
         BR    R10                 RETURN -> IGNORE ERROR
*
***********************************************************************
*                  ANALYZE DE-ALLOCATION ERRORS                       *
***********************************************************************
PPDAERRR CLI   S99VERB,S99VRBUN    DE-ALLOCATION FAILURE ?
         BNER  R10                 IMPOSSIBLE ERROR -> RETURN
         CLC   S99ERROR,=X'0438'   DDNAME NOT FOUND ?
         BNE   PPERM808            NO, GIVE MESSAGE AND ABEND
         BR    R10                 RETURN TO CALLER
         EJECT
***********************************************************************
*             DYNAMIC ALLOCATION ERROR MESSAGE ROUTINES               *
***********************************************************************
PPERM805 DS    0H
         LA    R2,MSG805I          CATALOG SEARCH FAILED
         B     PPDAERRM
*
PPERM806 DS    0H
         OI    PPSTAT2,PPRREQUE    INDICATE RE-QUEUE FOR CONVERT
         LA    R2,MSG806I          REQUIRED VOLUME NOT MOUNTED
         MVC   PPWTOWRK(PPVOLNML),PPVOLNMT MOVE IN BASIC WTO MESSAGE
         LA    R15,PPVOLSER        ASSUME USER HARD CODED VOLSER
         TM    PPSTAT2,PPHAVSER+PPHAVUNT VOLSER AND UNIT HARD CODED ?
         BO    PPVNMMSG            BRANCH IF YES, NO NEED FOR LOCATE
*
         LOCATE PPLOCPRM           LOCATE REQUIRED VOLSER FROM CATLG
         LTR   R15,R15             LOCATE SUCCESSFUL ?
         BZ    PPVNLOCT            BRANCH IF IT WAS
*
         NI    PPSTAT2,255-PPRREQUE     DISALLOW REQUEUE IF UNLOCATABLE
         LA    R15,=C'??????'      SHOW LOCATE FAILURE
         TM    PPSTAT2,PPHAVSER    WAS VOLSER CODED?
         BZ    PPVNMMSG            BRANCH IF NOT
         LA    R15,PPVOLSER        SET THE HARD CODED VOLSER
         B     PPVNMMSG            AND ISSUE THE MESSAGE
*
PPVNLOCT LA    R15,PPLOCWRK+6      SET LOCATED VOLUME
         CLC   PPLOCWRK(2),=H'1'   MAKE SURE THERE IS ONLY ONE VOLUME
         BNE   PPVNALOW            BRANCH IF IT ISN'T
         TM    PPLOCWRK+2+2,UCB3DACC    TEST FOR DIRECT ACCESS DEVICE
         BZ    PPVNALOW            BRANCH IF NOT DASD
*
         TM    PPLOCWRK+2+1,UCBRVDEV    TEST FOR MSS DEVICE
         BZ    PPVNMMSG            BRANCH IF NOT MSS
         NI    PPSTAT2,255-PPRREQUE     DISALLOW REQUEUE FOR MSS
*
PPVNALOW LA    R2,MSG811I          VOLUME NOT ALLOW FOR JOBPROC
*
PPVNMMSG MVC   PPWTOWRK+PPVOLNMS(6),0(R15) SET VOLSER IN MESSAGE
         B     PPDAERRM
*
PPERM807 DS    0H
         LA    R2,MSG807I          JOBPROC DATA SET NOT ON VOLUME
         B     PPDAERRM
*
PPERM808 DS    0H
         LA    R1,PPDFPARM         -> DAIRFAIL PARM LIST
         LINK  EP=IKJEFF18         ISSUE WTP FOR FAILURE
         CLI   S99VERB,S99VRBDC    FAILURE DURING DE-CONCAT. ?
         BE    PPERM816            YES, ABEND THE CONVERTER
         CLI   S99VERB,S99VRBUN    FAILURE DURING DE-ALLOCATION ?
         BE    PPERM817            YES, ABEND THE CONVERTER
         LA    R2,MSG808I          PROCLIB ALLOCATION FAILED
         B     PPDAERRM            ISSUE ERROR MESSAGE
*
PPERM809 DS    0H
         LA    R1,PPDFPARM         -> DAIRFAIL PARM LIST
         LINK  EP=IKJEFF18         ISSUE WTP FOR FAILURE
         LA    R2,MSG809I          USER PROCLIB CONCATENATION ERR
         B     PPDAERRM
*
PPERM813 DS    0H
         LA    R2,MSG813I          DATA SET NOT AVALIABLE EXCL'VE
         B     PPDAERRM
*
PPERM814 DS    0H
         LA    R2,MSG814I          UNIT NOT AVAILABLE (INVALID ?)
         B     PPDAERRM
*
PPERM815 DS    0H
         OI    PPSTAT2,PPRREQUE    INDICATE RE-QUEUE FOR CONVERT
         MVC   PPWTOWRK(PPCVLNML),PPCVLNMT MOVE IN WTO MESSAGE
         LA    R2,MSG815I          CVOL NOT MOUNTED
         B     PPDAERRM
*
PPERM816 WTO   MF=(E,PPDECON1)     DECONCATENATION FAILURE
         LA    R2,MSG816I          GIVE USER ERROR MESSAGE TOO
         B     PPDAERRM            GO ISSUE ERROR MESSAGE
*
PPERM817 WTO   MF=(E,PPDEALC1)     DEALLOCATION FAILURE
*
PPDABEND ABEND PPABEND,DUMP,,SYSTEM LET JES CLEANUP CONVERTER
*
PPDAERRM BAL   R14,IEFVPPM         GIVE USER ERROR MESSAGE
         TM    PPSTAT0,PP0CODE     ARE WE IN INITIALIZATION CLEANUP?
         BOR   R10                 YES, GO ON
         TM    PPSTAT2,PPRREQUE    RE-QUEUE FOR CONVERT REQUESTED
         BNOR  R10                 RETURN TO CALLER IF NOT
         WTO   MF=(E,PPWTOWRK)     SEND MESSAGE TO OPERATOR AS WELL
         L     R2,CWAJMRPT         -> JOB MANAGEMENT RECORD
         OI    JMRINDC-JMR(R2),X'04' FLAG JOB FOR JES2 RE-QUEUE
         BR    R10                 RETURN TO CALLER
*
LOCATE   EQU   X'17'               CATALOG LOCATE FAILED
OBTAIN   EQU   X'67'               DSCB OBTAIN FAILED
DALRNOTA EQU   X'02'               RESOURCE NOT AVAILABLE
DALDSNEX EQU   X'0C'                  UNABLE TO GET DATA SET EXCLUSIVE
DALDSNNA EQU   X'10'                  DATA SET IN USE
DALUNITE EQU   X'14'                  UNAVAILABLE UNIT SPECIFIED
DALVOLNM EQU   X'18'                  REQUIRED VOLUME NOT MOUNTED
DALUNITI EQU   X'1C'                  INVALID UNIT SPECIFIED
DALVOLNA EQU   X'20'                  VOLUME NOT AVAILABLE
DALCVOLE EQU   X'3C'                  CVOL NOT MOUNTED
*
         DROP  R11,R7,R9
*
         AIF   (&INTEXTX).UJVYES
 TITLE '**** IEFVPP5 - INTERNAL TEXT EXIT FOR IEFUJV (NONE) ****'
***********************************************************************
*                                                                     *
* IEFVPP5                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED BY IEFVFA AND IMMEDIATELY RETURNS TO IT,     *
* AS NO INTERNAL TEXT EXIT TO IEFUJV IS DESIRED.                      *
*                                                                     *
* INPUT:                                                              *
*        R12 -> CONVERTER WORK AREA                                   *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        IMMEDIATE RETURN TO IEFVFA.                                  *
*                                                                     *
* SVC:                                                                *
*        NONE                                                         *
*                                                                     *
* NOTES:                                                              *
*        THIS ROUTINE IS NOT PART OF THE PRIVATE PROCLIB SUPPORT      *
*        PER SE, BUT IT DOES USE THE CONVERTER WORK AREA INTERFACE.   *
*                                                                     *
***********************************************************************
*
IEFVPP5  DS    0H
         BR    R14
*
         AGO   .UJVDONE
.UJVYES  AIF   (&SGIHASU(74)).UJVSE2
 TITLE '**** IEFVPP5 - INTERNAL TEXT EXIT FOR IEFUJV (NON SE2) ****'
***********************************************************************
*                                                                     *
* IEFVPP5                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED BY IEFVFA AND IN TURN BRANCHES TO IEFUJV     *
* (WHICH RETURNS TO IEFVFA).  THE PURPOSE IS TO GIVE IEFUJV AN        *
* INTERNAL TEXT EXIT (TYPE 64).                                       *
*                                                                     *
* INPUT:                                                              *
*        R12 -> CONVERTER WORK AREA                                   *
*                                                                     *
* OUTPUT:                                                             *
*        REGISTER R5 DESTROYED.                                       *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        IEFUJV WILL RETURN DIRECTLY TO IEFVFA.                       *
*                                                                     *
* SVC:                                                                *
*        NONE                                                         *
*                                                                     *
* NOTES:                                                              *
*        THIS ROUTINE IS NOT PART OF THE PRIVATE PROCLIB SUPPORT      *
*        PER SE, BUT IT DOES USE THE CONVERTER WORK AREA INTERFACE.   *
*        THE PRIMARY PURPOSE OF THIS EXIT WAS THE IMPLEMENTATION      *
*        OF A JOB RESOURCE MANAGEMENT SUBSYSTEM.                      *
*        REGISTER SAVING IS NOT DONE BY THIS ROUTINE, AS IEFUJV WILL  *
*        DO ANY NECESSARY SAVING AND RESTORING.                       *
*                                                                     *
***********************************************************************
*
IEFVPP5  DS    0H
         USING *,R15
         L     R5,WANELPTR           LOAD THE NEL ADDRESS
         TM    NELOPSWT-NEL(R5),NELSMF TEST FOR SMF OPTION
         BNOR  R14                   RETURN IF NO SMF
         L     R5,CWAJMRPT           LOAD JMR ADDRESS
         USING JMR,R5
         TM    JMROPT,JMREXITS       TEST IF USERS EXITS TO BE TAKEN
         BZR   R14                   RETURN IF NOT
*
         LA    R1,JMRJOB             POINT AT JOB LOG
         ST    R1,JMRJOBP            SET IN PARAMETER LIST
         MVC   JMRJCLP,TEXTBUFP      SET POINTER TO INTERNAL TEXT
         LA    R1,JMRJCLCD           SET ADDRESS OF ENTRY CODE BYTE
         ST    R1,JMRJCLCP           STORE IN PARAMETER LIST
         MVI   JMRJCLCD,64           INTERNAL TEXT EXIT TYPE
         LA    R1,JMRPTRS            POINT AT PARAMETER LIST
         L     R15,JMRUJVP           LOAD IEFUJV ADDRESS
         BR    R15                   GO TO IEFUJV
*
         DROP  R15,R5
*
         AGO   .UJVDONE
.UJVSE2  ANOP
    TITLE '**** IEFVPP5 - INTERNAL TEXT EXIT FOR IEFUJV (MVS/SE2) ****'
***********************************************************************
*                                                                     *
* IEFVPP5                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE IS CALLED BY IEFVFA AND IN TURN BRANCHES TO IEFUJV     *
* (WHICH RETURNS TO IEFVFA).  THE PURPOSE IS TO GIVE IEFUJV AN        *
* INTERNAL TEXT EXIT (TYPE 64).  LINKAGE TO IEFUJV IS PERFORMED       *
* USING THE SMFEXIT MACRO (MVS/SE2).                                  *
*                                                                     *
* INPUT:                                                              *
*        R12 -> CONVERTER WORK AREA                                   *
*                                                                     *
* OUTPUT:                                                             *
*        REGISTER R5 DESTROYED.                                       *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER                                             *
*                                                                     *
* SVC:                                                                *
*        NONE                                                         *
*                                                                     *
* NOTES:                                                              *
*        THIS ROUTINE IS NOT PART OF THE PRIVATE PROCLIB SUPPORT      *
*        PER SE, BUT IT DOES USE THE CONVERTER WORK AREA INTERFACE.   *
*        THE PRIMARY PURPOSE OF THIS EXIT WAS THE IMPLEMENTATION      *
*        OF A JOB RESOURCE MANAGEMENT SUBSYSTEM.                      *
*        REGISTER SAVING IS NOT DONE BY THIS ROUTINE, AS IEFUJV WILL  *
*        DO ANY NECESSARY SAVING AND RESTORING.                       *
*                                                                     *
***********************************************************************
*
IEFVPP5  DS    0H
         LR    R5,R14                SAVE THE RETURN ADDRESS
         LR    R14,R15               SHIFT THE BASE REGISTER
         USING IEFVPP5,R14
         L     R15,WANELPTR          LOAD THE NEL ADDRESS
         TM    NELOPSWT-NEL(R15),NELSMF TEST FOR SMF OPTION
         BNOR  R5                    RETURN IF NO SMF
         L     R15,CWAJMRPT          LOAD JMR ADDRESS
         USING JMR,R15
*
         LA    R1,JMRJOB             POINT AT JOB LOG
         ST    R1,JMRJOBP            SET IN PARAMETER LIST
         MVC   JMRJCLP,TEXTBUFP      SET POINTER TO INTERNAL TEXT
         LA    R1,JMRJCLCD           SET ADDRESS OF ENTRY CODE BYTE
         ST    R1,JMRJCLCP           STORE IN PARAMETER LIST
         MVI   JMRJCLCD,64           INTERNAL TEXT EXIT TYPE
         LA    R1,JMRPTRS            POINT AT PARAMETER LIST
*
         SMFEXIT IEFUJV              INVOKE UJV EXIT
*
         BR    R5                    RETURN TO CALLER
*
         DROP  R15,R14
*
.UJVDONE ANOP
         TITLE '**** SYSTEM ERROR RECOVERY EXIT (ESTAE) ****'
***********************************************************************
*                                                                     *
* PPESTAEX                                                            *
* ********                                                            *
*                                                                     *
* THIS ROUTINE SETS THE JOB ERROR FLAG FOR THE CONVERTER AND PRINTS   *
* AN ERROR MESSAGE FOR THE USER.                                      *
*                                                                     *
* INPUT:                                                              *
*        R1 -> SYSTEM DIAGNOSTIC WORK AREA (SDWA).                    *
*                                                                     *
* OUTPUT:                                                             *
*        UPDATED SDWA FOR THE APPROPRIATE ERROR RECOVERY ROUTINE.     *
*        IWA(WAJOBPFX) SET TO INDICATE JCL ERROR.                     *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        NORMAL:  RETURN TO R/TM2 VIA SETRP MACRO.                    *
*        NO SDWA: RETURN TO R/TM2 WITH R15 = 0.                       *
*                                                                     *
* SVC:                                                                *
*        SDUMP (SVC 51) WHEN IN DEBUGGING MODE.                       *
*                                                                     *
* NOTES:                                                              *
*        IT IS HOPED THAT SDWAPARM CONTAINS A PTR TO THE CWA.         *
*                                                                     *
***********************************************************************
         EJECT
********* PREPARE THE SDWA AND CHECK FOR UNRECOVERABLE ABENDS *********
*
         USING PPESTAEX,R15        TEMPORARY BASE
PPESTAEX DS    0H
         C     R0,=F'12'           CHECK FOR NO SDWA
         BE    PPESTAEZ            GIVE UP IF NONE
         LTR   R1,R1               CHECK FOR NO SDWA
         BE    PPESTAEZ            GIVE UP IF NONE
         STM   R14,R12,12(R13)     SAVE R/TM REGISTERS
         LR    R3,R1               SET PTR TO SDWA
         USING SDWA,R3             ESTABLISH SDWA ADDRESSABILITY
*
         L     R12,SDWAPARM        ENSURE CWA ADDRESSABILITY
         CLC   CWAID,=X'11111111'  ENSURE THIS IS IN FACT THE CWA
         BNE   PPESFATL            NO, DO NOT RECOVER FROM ABEND
         L     R6,&ANCHOR          LOAD PRIVATE PROCLIB WORK AREA
         LTR   R6,R6               DO WE HAVE A WORK AREA
         BZ    PPESFATL            THIS IS DEADLY IF WE DON'T
*
         TM    PPMISC,PPDEBUG      TEST IF DEBUGGING IN FORCE
         BZ    PPESBYP             BRANCH IF NO DEBUGGING
         LR    R2,R15              SAVE THE BASE REGISTER
         SDUMP MF=(E,PPESDUMP)     INVOKE A BIG DUMP
         LR    R15,R2              RESTORE THE BASE REGISTER
PPESBYP  DS    0H
*
         MVC   SDWARECP,PPESRECP   MOVE IN RECORDING PARMS
         MVI   SDWADPVA,SDWAHEX    DUMP USER DATA IN HEX
         MVI   SDWAURAL,PPDCB-PPSTAT0 MOVE IN LENGTH OF USER DATA
         MVC   SDWAVRA(PPDCB-PPSTAT0),PPSTAT0 TRACE INFO FOR LOGREC
*
         OI    STRJINDC-TEXT+WAJOBPFX,JTXJOBFL FLUSH THE BAD JOB
         OI    PPSTAT1,PPJPERR     ALSO FLAG ERROR FOR US
*
         SR    R1,R1               CLEAR R1
         ICM   R1,3,SDWACMPC       R1 = ABEND CODE (XXX0)
         SRL   R1,4                DROP INSIGNIFICANT BITS
         CLM   R1,3,=AL2(PPABEND)  IS THIS A SELF INFLICTED ABEND
         BE    PPESFATL            BRANCH IF YES
         TM    PPSTAT0,PPECODE     IS THIS A RECURSIVE ABEND ?
         BNO   PPESENVR            BRANCH IF NOT
*
PPESFATL DS    0H
         SETRP REGS=(14,12),RC=0,RECORD=YES, * CONTINUE ABEND *        X
               WKAREA=(R3)
*
PPESTAEZ DS    0H                  NO SDWA ADDRESS PROVIDED
         SLR   R15,R15             INDICATE NO RETRY
         BR    R14                  AND RETURN
         EJECT
****************** DETERMINE LOCATION OF ERROR ************************
PPESENVR OI    PPSTAT0,PPECODE     INDICATE ERROR EXIT IN CONTROL
         L     R11,PPBASE          GET COMMON BASE REG
         LA    R7,2048             AND SECOND BASE REGISTER
         LA    R7,2048(R7,R11)
         USING IEFVPP,R11,R7       INFORM ASSEMBLER ABOUT IT
         DROP  R15                 DROP TEMPORARY BASE
*
         TM    PPSTAT0,PP0CODE     FAILURE DURING INITIALIZATION
         BNO   PPESPP1             NO, CONTINUE
         MVI   SDWARECP+14,C'0'    INDICATE IEFVPP0 FAILED
         OI    SDWAACF2,SDWARCRD   INDICATE LOGREC RECORD REQ'D
         MVI   PPSTAT0,0           DISABLE USER PROCLIB SUPPORT
         LA    R4,PPRETURN         SET RETRY FOR GRACEFUL EXIT
         B     PPESRTRN            RETURN TO R/TM
*
PPESPP1  TM    PPSTAT0,PP1CODE     FAILURE IN JOBPROC PROCESSOR ?
         BNO   PPESPP2             BRANCH IF NOT
         MVI   SDWARECP+14,C'1'    INDICATE IEFVPP1 FAILED
         OI    SDWAACF2,SDWARCRD   INDICATE LOGREC RECORD REQ'D
         LA    R4,PPRETURN         SET RETRY ADDRESS
         B     PPESRTRN            RETURN TO R/TM
*
PPESPP2  TM    PPSTAT0,PP2CODE     FAILURE IN PROCLIB PREPARE ?
         BNO   PPESPP3             BRANCH IF NOT
         MVI   SDWARECP+14,C'2'    INDICATE IEFVPP2 FAILED
         TM    PPTRPP2,PPTROPEN    IS PROCLIB OPEN IN PROGRESS ?
         BNO   PPESPP2A            BRANCH IF NOT
         SR    R1,R1               CLEAR R1
         ICM   R1,3,SDWACMPC       R1 = ABEND CODE (XXX0)
         SRL   R1,4                DROP INSIGNIFICANT BITS
         CLM   R1,3,=X'0213'       POSSIBLE "DATA SET NOT FOUND"?
         BNE   PPESOERR            BRANCH IF NOT
         CLC   SDWAGR15+2(2),=H'4' S213-04 -> NO DATA SET ?
         BNE   PPESOERR            BRANCH IF NOT
         OI    PPTRPP2,PPTRS213    INDICATE S213-04 ABEND IN PP2
*
PPESOERR LA    R4,PPTOPEN          SET RECOVERY PTR FOR OPEN ERR
         NI    SDWAACF2,PPALL-SDWARCRD NO RECORDING FOR USER ERR
         B     PPESRTRN            RETURN TO R/TM
*
PPESPP2A LA    R4,PPRETURN         GRACEFUL EXIT IF NOT OPEN ERR
         OI    SDWAACF2,SDWARCRD   INDICATE LOGREC RECORD REQ'D
         B     PPESRTRN            RETURN TO R/TM
*
PPESPP3  TM    PPSTAT0,PP3CODE     FAILURE IN CLEANUP PROCESSING
         BNO   PPESFATL            NO -> THINGS ARE IN BAD SHAPE
         MVI   SDWARECP+14,C'3'    INDICATE IEFVPP3 FAILED
         B     PPESFATL            DO NOT RECOVER FROM PP3 ERROR
*
         EJECT
*********************** RETURN TO THE RTM *****************************
PPESRTRN SETRP RC=4,RETADDR=(R4),FRESDWA=YES,WKAREA=(R3),              X
               REGS=(14,12)        RETURN TO R/TM
*
         DROP  R3                  DROP SDWA ADDRESSABILITY
*
PPBASE   DC    A(IEFVPP)           BASE ADDRESS FOR ALL ROUTINES
*              MUST BE WITHIN THE FIRST 4K OF THE MODULE
*
         TITLE '**** ERROR MESSAGE ROUTINE ****'
***********************************************************************
*                                                                     *
* IEFVPPM                                                             *
* *******                                                             *
*                                                                     *
* THIS ROUTINE SETS THE JOB ERROR FLAG FOR THE CONVERTER AND PRINTS   *
* AN ERROR MESSAGE FOR THE USER.                                      *
*                                                                     *
* INPUT:                                                              *
*        R2 =  ADDRESS OF MESSAGE.                                    *
*                                                                     *
* OUTPUT:                                                             *
*        IWA(WAJOBPFX) SET TO INDICATE JCL ERROR IF NOT A             *
*        REQUEUEABLE ERROR.                                           *
*        MESSAGE TO THE JCL MESSAGE DATA SET.                         *
*                                                                     *
* EXTERNALS:                                                          *
*        PUT RPL TO SYSTEM MESSAGE DATA SET.                          *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER OF THIS ROUTINE.                            *
*                                                                     *
* SVC:                                                                *
*        NONE                                                         *
*                                                                     *
***********************************************************************
         EJECT ,
*************************** BUILD MESSAGE *****************************
IEFVPPM  DS    0H
         STM   R14,R12,12(R13)     SAVE CALLERS' REGISTERS
         OI    STRJINDC-TEXT+WAJOBPFX,JTXJOBFL SET JOB ERROR FLAG
         OI    PPSTAT1,PPJPERR     BYPASS ALL FURTHER PROCESSING
         OI    PPSTAT0,PPMCODE     INDICATE WE ARE IN IEFVPPM CODE
*
         L     R1,WAMSGBUF         LOAD ADDRESS OF MESSAGE BUFFER
         MVI   0(R1),C' '          BLANK THE BUFFER OUT
         MVC   1(159,R1),0(R1)
         LR    R15,R1              SAVE IT FOR LATER
         LH    R14,WASTMTNO        LOAD THE STATEMENT NUMBER
         CVD   R14,AOEPWORK        CONVERT IT TO DECIMAL
         OI    AOEPWORK+7,X'0F'    FIX THE SIGN
         UNPK  0(5,R1),AOEPWORK+5(3)
         LR    R14,R1              SET FOR LEADING BLANKER
*
PPMBLANK CLI   0(R14),C'0'         CHECK FOR LEADING ZEROES
         BNE   PPMNZERO            BRANCH IF NOT
         MVI   0(R14),C' '         BLANK IT OUT
         LA    R14,1(,R14)         INCREMENT
         B     PPMBLANK            AND TRY AGAIN
*
PPMNZERO LA    R3,10               LENGTH OF STMT NUMBER FIELD
         AR    R15,R3              POINT PAST STMT NUMBER
         SR    R14,R14             CLEAR FOR INSERT
         IC    R14,1(,R2)          GET LENGTH OF MESSAGE
         AR    R3,R14              CALCULATE TOTAL LENGTH
         BCTR  R14,0               DECREMENT FOR MVC
         EX    R14,PPMMVMSG        MOVE THE MESSAGE TEXT
         MVI   0(R1),C' '          BLANK THE FIRST CHARACTER
*
         EJECT
************* PUT OUT HEADER, MESSAGE, AND RETURN *********************
         LA    R8,WAJOBPFX         LOAD ADDRESS OF JOB PREFIX
         USING TEXT,R8
         TM    STRJINDC,JTXMHEDR   TEST IF JOB HEADER PRESENT
         BO    PPMHDRDN            BRANCH IF SO
         OI    STRJINDC,JTXMHEDR   SET HEADER DONE
         DROP  R8
*
         LA    R15,PPMHEAD1        POINT AT FIRST HEADER
         LA    R14,L'PPMHEAD1      GET LENGTH OF HEADER
         BAL   R9,PPMPUTLN         PUT OUT LINE 1 OF HEADER
         LA    R15,PPMHEAD2        POINT AT SECOND HEADER
         LA    R14,L'PPMHEAD2      GET LENGTH OF HEADER
         BAL   R9,PPMPUTLN         PUT OUT LINE 2 OF HEADER
*
PPMHDRDN L     R15,WAMSGBUF        RELOAD BUFFER ADDRESS
         LA    R14,1(,R3)          SET TOTAL LENGTH
         BAL   R9,PPMPUTLN         GO PUT THE LINE
*
         NI    PPSTAT0,PPALL-PPMCODE INDICATE COMPLETION OF IEFVPPM
         LM    R14,R12,12(R13)     RESTORE CALLERS' REGISTERS
         BR    R14                 RETURN TO CALLER
*
PPMMVMSG MVC   0(0,R15),2(R2)      MOVE MESSAGE TEXT - EXECUTE ONLY
*
         EJECT
******************** PUT MESSAGE SUBROUTINE ***************************
PPMPUTLN DS    0H
         LA    R1,RPLADDR          LOAD THE MESSAGE RPL ADDRESS
         USING IFGRPL,R1
         MVI   RPLREQ,RPLPUT       SET PUT REQUEST
         ST    R15,RPLAREA         SET MESSAGE ADDRESS
         XC    RPLARG,RPLARG       ZERO UNNECESSARY FIELD
         ST    R14,RPLRLEN         SET RECORD LENGTH
         MVI   RPLOPT1,RPLSEQ      SET SEQUENTIAL REQUEST
         XC    RPLBUFL,RPLBUFL     ZERO UNNECESSARY FIELD
         XC    RPLDDDD,RPLDDDD     ZERO UNNECESSARY FIELD
         L     R15,WANELPTR        LOAD NEL ADDRESS
         USING NEL,R15
         MVC   RPLDACB,NELMSGCB    SET MSG ACB ADDRESS
         DROP  R15
*
         ST    R13,IWASACHN        SAVE CURRENT SAVE AREA ADDRESS
         LA    R13,IWAIOSA         SET I/O SAVE AREA
         PUT   RPL=(1)             DO IT
         DROP  R1
         L     R13,IWASACHN        RESTORE SAVE AREA ADDRESS
*
         TM    AOSW1,AOIOERR       WAS IT BAD
         BO    PPMBADNW            BRANCH IF SO
         LTR   R15,R15             ANOTHER TEST FOR BAD
         BZR   R9                  RETURN IF OK
*
PPMBADNW MVC   CWARET,=X'0024'     SET I/O ERROR RETURN CODE
         OI    AOSW4,CWATERM       INDICATE TERMINATION
         L     R15,TERMRTN         LOAD TERMINATION ROUTINE ADDRESS
         BR    R15                 AND GO TO IT
*
         TITLE '**** DEBUGGING TRACER OF LAST RESORT ****'
***********************************************************************
*                                                                     *
* IEFVPPDB                                                            *
* ********                                                            *
*                                                                     *
* THIS ROUTINE WILL PRODUCE A CONSOLE MESSAGE FOR TRACING IF THE      *
* "PPDEBUG" FLAG IS ON.                                               *
*                                                                     *
* INPUT:                                                              *
*        R14 = ADDRESS OF FOUR BYTE TRACE ID.                         *
*                                                                     *
* OUTPUT:                                                             *
*        MESSAGE CCC869I XXXX HAS BEEN ENCOUNTERED                    *
*        XXXX IS THE FOUR BYTE TRACE ID.                              *
*                                                                     *
* EXTERNALS:                                                          *
*        NONE.                                                        *
*                                                                     *
* EXIT:                                                               *
*        RETURN TO CALLER OF THIS ROUTINE TO FOUR AFTER REGISTER 14.  *
*                                                                     *
* SVC:                                                                *
*        WTO (SVC 35)                                                 *
*                                                                     *
***********************************************************************
         SPACE 3
IEFVPPDB DS    0H
         TM    PPMISC,PPDEBUG      SEE IF TRACING TURNED ON
         BNO   4(,R14)             RETURN PAST GOODIE
*
         STM   R14,R12,12(R13)     SAVE THE REGISTERS
         MVC   PPWTOWRK(PPDBWTOL),PPDBWTO  SET THE WTO
         MVC   PPWTOWRK+12(4),0(R14)   SET THE DEBUG CODE
         WTO   MF=(E,PPWTOWRK)     SO THE TERRIBLE WTO
         LM    R14,R12,12(R13)     RESTORE THE REGISTERS
         B     4(,R14)             RETURN TO CALLER
*
         DROP  R11,R7
*
         TITLE '**** CONSTANTS ****'
***************************** CONSTANTS *******************************
PPMSKCLR DC    A(X'7F')            TO CLEAR FLAG BIT IN INT TEXT COUNT
PPSYSMAJ DC    CL8'SYSJPROC'       MAJOR PROCLIB ENQUEUE NAME
PPSYSDDN DC    CL8'IEF*PDSI'       SYSTEM PROCLIB DDNAME (FOR CONCAT)
PPDDNSUF DC    C'0123456789ABCDEF' SUFFIX FOR ALLOCATING JOBPROC'S
PPJOBPRO DC    AL1(7),C'JOBPROC'   INT. TEXT DEF FOR DDNAME
*
************************** EQUATES ************************************
PPALL    EQU   X'FF'               ALL BITS MASK
PPVLBIT  EQU   X'80'               PARMLIST VL
PPABEND  EQU   X'BAD'              INTERNAL ABEND CODE
         SPACE 1
PPASHR   EQU   X'08'               "DISP=SHR" PARM CODE
         SPACE 1
**********************SYSPROC KEYWORD VALUES **************************
PPSYES   DC    AL1(3),C'YES'       INT TEXT FORM FOR SYSPROC=YES
PPSNO    DC    AL1(2),C'NO'        INT TEXT FORM FOR SYSPROC=NO
         SPACE 1
************************* LITERALS ************************************
LITERALS LTORG ,                   FLUSH LITERALS
         SPACE 2
*********************** ESTAE CONSTANTS *******************************
PPESRECP DC    CL8'IEFVH1',CL8'IEFVPP?',CL8'PPESTAEX'
PPESHDR  DC    AL1(32),CL32'PRIVATE PROCLIB ESTAE DUMP'
*
PPESDUMP SDUMP HDRAD=PPESHDR,      SVC DUMP PARAMETER LIST             X
               SDATA=(SQA,PSA,RGN,TRT),                                X
               MF=L
*
         TITLE '**** MODEL CONTROL BLOCKS FOR INITIALIZATION ****'
PPDCBPAT DCB   DDNAME=IEF*PDS0,                                        X
               DSORG=PO,                                               X
               MACRF=(R),                                              X
               BUFNO=0,                                                X
               BUFCB=0,                                                X
               RECFM=FB,                                               X
               LRECL=80
PPDCBPTL EQU   *-PPDCBPAT          PATTERN DCB LENGTH
         SPACE 1
PPESTAEM ESTAE *-*,CT,             ESTABLISH PRIVATE PROCLIB           X
               TERM=YES,            ERROR                              X
               PARAM=*-*,            RECOVERY                          X
               MF=L                   ENVIRONMMENT
PPESTAEL EQU   *-PPESTAEM          LTH OF ESTAE PARM LIST
         SPACE 1
PPCAMLST CAMLST NAME,*-*,,*-*      BUILD LOCATE PARM LIST
*
PPLOCPL  EQU   *-PPCAMLST          PARM LIST LENGTH
*
PPENQLST ENQ   (PPSYSMAJ,*-*,E,8,STEP),RET=HAVE,MF=L
*
PPENQPL  EQU   *-PPENQLST          ENQUEUE LIST LENGTH
*
         TITLE '**** ALL USER AND OPERATOR ERROR MESSAGES ****'
PPMHEAD1 DC    CL17' STMT NO. MESSAGE'  JCL MESSAGES
PPMHEAD2 DC    CL17'-'                   HEADERS
         SPACE 2
      IEFVPPMS 800I,                                                   X
               'SYNTAX ERROR IN JOBPROC DD STATEMENT'
      IEFVPPMS 801I,                                                   X
               'ILLEGAL USE OF "SYSPROC" OPTION'
      IEFVPPMS 802I,                                                   X
               'JOBPROC DATA SET NAME NOT SPECIFIED'
      IEFVPPMS 803I,                                                   X
               'DATA SET NAME NOT SPECIFIABLE FOR JOBPROC'
      IEFVPPMS 804I,                                                   X
               'JOBPROC CONCATENATION LIMIT EXCEEDED'
      IEFVPPMS 805I,                                                   X
               'JOBPROC DATA SET NOT FOUND IN CATALOG'
      IEFVPPMS 806I,                                                   X
               'VOLUME CONTAINING JOBPROC DATA SET NOT MOUNTED'
PPVOLNMT WTO   '&MSGPFX.806I JOBPROC VOLUME XXXXXX NOT MOUNTED',       X
               ROUTCDE=(2,4,6),DESC=6,MF=L
PPVOLNML EQU   *-PPVOLNMT          MESSAGE LENGTH
PPVOLNMS EQU   27                  OFFSET TO VOLUME INSERT
         SPACE 2
      IEFVPPMS 807I,                                                   X
               'JOBPROC DATA SET NOT ON VOLUME'
      IEFVPPMS 808I,                                                   X
               'JOBPROC ALLOCATION FAILED'
      IEFVPPMS 809I,                                                   X
               'JOBPROC CONCATENATION FAILED'
      IEFVPPMS 810I,                                                   X
               'INSUFFICIENT STORAGE AVAILABLE TO ALLOCATE JOBPROC BUFFX
               ER'
      IEFVPPMS 811I,                                                   X
               'VOLUME OR DEVICE TYPE NOT ALLOWED FOR JOBPROC USAGE'
      IEFVPPMS 812I,                                                   X
               'JOBPROC OPEN FAILED'
      IEFVPPMS 813I,                                                   X
               'JOBPROC DATA SET NOT AVAILABLE UNDER REQUESTED DISPOSITX
               ION'
      IEFVPPMS 814I,                                                   X
               'JOBPROC DATA SET HAS INCORRECT UNIT SPECIFICATION'
      IEFVPPMS 815I,                                                   X
               'CATALOG VOLUME REQUIRED FOR JOBPROC PROCESSING IS NOT MX
               OUNTED'
PPCVLNMT WTO   '&MSGPFX.815I JOBPROC CATALOG VOLUME NOT MOUNTED',      X
               ROUTCDE=(2,4,6),DESC=6,MF=L
PPCVLNML EQU   *-PPCVLNMT          MESSAGE LENGTH
         SPACE 2
      IEFVPPMS 816I,                                                   X
               'JOBPROC DE-CONCATENATION FAILED'
PPDECON1 WTO   '&MSGPFX.816I JOBPROC DE-CONCATENATION FAILED',         X
               ROUTCDE=1,DESC=1,MF=L
         SPACE 2
      IEFVPPMS 817I,                                                   X
               'JOBPROC DE-ALLOCATION FAILED'
PPDEALC1 WTO   '&MSGPFX.817I JOBPROC DE-ALLOCATION FAILED',            X
               ROUTCDE=1,DESC=1,MF=L
         SPACE 2
      IEFVPPMS 836I,                                                   X
               'MISPLACED JOBPROC STATEMENT'
      IEFVPPMS 842I,                                                   X
               'EXCESSIVE PARAMETER LENGTH IN THE SYSPROC FIELD'
      IEFVPPMS 843I,                                                   X
               'UNIDENTIFIED POSITIONAL PARAMETER IN THE SYSPROC FIELD'
PPENQWTO WTO   '&MSGPFX.851I TOO MANY CONVERTERS RUNNING',             X
               ROUTCDE=2,DESC=2,MF=L
         SPACE 2
PPSYSWTO WTO   '&MSGPFX.852I SYSPROC LIBRARIES UNAVAILABLE',           X
               ROUTCDE=2,DESC=6,MF=L
         SPACE 2
PPDBWTO  WTO   '&MSGPFX.869I **** HAS BEEN ENCOUNTERED',               X
               ROUTCDE=2,DESC=6,MF=L
PPDBWTOL EQU   *-PPDBWTO
         SPACE 2
*
         END
/*
//STEP3   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DATA,DLM='><'
         IDENTIFY IEFVPP('#DYP002')
++ USERMOD (#DYP003)  .
++ VER   (Z038)  FMID (EBB1102)                /* MVS 3.8 BASE */
                 REQ (#DYP004)                 /* ZAP TO IEFVFA */
                 PRE (UZ51830
                      #DYP001                  /* SYSGEN MACRO UPDTE */
                      #DYP002                  /* NEW MODULE IEFVPP */
                             )
               /*

               PRIVATE PROCLIB MODIFICATIONS
               VERSION 4, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS

               ***** WARNING ***** WARNING ***** WARNING *****

               THIS SUPERZAP IS WRITTEN WITH RESERVED WORD "RFULLE"
               IN THE CONVERTER WORK AREA USED AS THE ANCHOR FOR
               THE PRIVATE PROCLIB WORK AREA.

               ***** WARNING ***** WARNING ***** WARNING *****

               IEFVHF

               IEFVHF IS THE CONVERTER TERMINATION MODULE.  THE ZAP
               CHANGES IEFVHF TO BRANCH TO IEFVPP3 IN IEFVPP TO
               ALLOW PRIVATE PROCLIB CLEANUP TO TAKE PLACE.  THE
               ADDRESS OF IEFVPP3 RESIDES IN THE PRIVATE PROCLIB
               WORK AREA THAT IS POINTED TO BY THE ANCHOR WORD IN
               THE CONVERTER WORK AREA.  THE OFFSET OF THE WORD
               USED TO ANCHOR THE ADDRESS THE OF THE PRIVATE PROCLIB
               WORK AREA IN THIS ZAP MUST CORRESPOND TO THAT USED
               IN THE ASSEMBLY OF IEFVPP.

                                                                */  .
++ ZAP   (IEFVHF)  DISTLIB (AOSB3)  .
         NAME  IEFVHF
*
*
         VER   0000 05B0           BALR R11,0 (BASE=0002)
*
         VER   00DA 5830,C0EC      L    R3,WANELPTR
*
         VER   0260 0000,0000      START OF NEEDED PATCH AREA
         VER   0264 0000,0000      NEEDED PATCH AREA
         VER   0268 0000,0000      NEEDED PATCH AREA
         VER   026C 0000,0000      END OF NEEDED PATCH AREA
*
         REP   0260 5830,C0EC      L    R3,WANELPTR OVERLAID INSTRUCT
         REP   0264 58F0,C3A0      L    R15,RFULLE  (ANCHOR WORD)
         REP   0268 58F0,F058      L    R15,PPVPP3  A(IEFVPP3)
         REP   026C 07FF           BR   R15  RETURN ON R14
*
         REP   00DA 45E0,B25E      BAL  R14,PATCH
*
         IDRDATA #DYP003
++ USERMOD (#DYP004)  .
++ VER   (Z038)  FMID (EBB1102)               /* MVS 3.8 BASE */
                 REQ (#DYP003)                /* ZAP TO IEFVHF */
                 PRE (
                      UZ69627                 /* PTF  */
                      #DYP001                 /* SYSGEN MACRO UPDATE */
                      #DYP002                 /* NEW MODULE IEFVPP */
                             )
               /*

               PRIVATE PROCLIB MODIFICATIONS
               VERSION 4, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS

               ***** WARNING ***** WARNING ***** WARNING *****

               THIS SUPERZAP IS WRITTEN WITH RESERVED WORD "RFULLE"
               IN THE CONVERTER WORK AREA USED AS THE ANCHOR FOR
               THE PRIVATE PROCLIB WORK AREA.

               ***** WARNING ***** WARNING ***** WARNING *****

               IEFVFA

               IEFVFA IS THE CONVERTER SCAN ROUTINE.  THE ZAP IS
               TWO-FOLD.

               FIRST, AN ENTRY IN THE JCL KEYWORD TABLE IS
               CONVERTED TO THE "SYSPROC=" KEYWORD.  "SUBALLOC="
               WAS CHOSEN, SINCE ITS USE IS VIRTUALLY NON-EXISTANT.  TO
               FILL IN THE EXTRA BYTE LEFT OVER WHEN "SYSPROC=" AND ITS
               INTERNAL TEXT CODE (SYMBOL SYSPROCK DEFINED IN IEFVPP)
               IS ZAPPED OVER "SUBALLOC=" AND ITS INTERNAL TEXT
               CODE (SYMBOL SUBALLOK - X'4C' - DEFINED IN MACRO
               IEFVKEYS), "SYSPROC=" IS MADE MUTUALLY EXCLUSIVE
               WITH "DCB=" (SYMBOL DCBK - X'40' - DEFINED IN MACRO
               IEFVKEYS).

               SECOND, IEFVFA IS CHANGED TO BRANCH TO BOTH IEFVPP4
               IN IEFVPP AND IEFVPP5 IN IEFVPP, IMMEDIATELY
               AFTER A JCL STATEMENT HAS BEEN CONVERTED
               INTO INTERNAL TEXT.  IEFVPP4 EFFECTS LINKAGE TO
               IEFVPP1 AND IEFVPP2 IN IEFVPP DEPENDING ON THE
               STATEMENT BEING PROCESSED.  IEFVPP1 PERFORMS
               PRIVATE PROCLIB ALLOCATION.  IEFVPP2 PERFORMS
               PRIVATE PROCLIB CONCATENATION AND OPEN.  IEFVPP5
               CALLS IEFUJV WITH ENTRY CODE 64, GIVING IEFUJV
               AN INTERNAL TEXT EXIT.  THIS CODE IS IN SUPPORT OF
               THE JOB STREAM MANAGER.  IF THIS NEW ENTRY INTO
               IEFUJV IS NOT DESIRED OR NECESSARY, IT MAY BE
               ELIMINATED BY APPROPRIATE CHANGES TO THIS ZAP
               OR ASSEMBLING IEFVPP WITHOUT THE INTERNAL TEXT
               EXIT OPTION SET.

               THE ADDRESSES OF IEFVPP4 AND IEFVPP5 RESIDE IN THE
               PRIVATE PROCLIB WORK AREA THAT IS POINTED TO BY THE
               ANCHOR WORD THE CONVERTER WORK AREA.  THE OFFSET OF THE
               WORD USED TO ANCHOR THE ADDRESS THE OF THE PRIVATE
               PROCLIB WORK AREA IN THIS ZAP MUST CORRESPOND TO
               THAT USED IN THE ASSEMBLY OF IEFVPP.

                                                                */  .
++ ZAP   (IEFVFA)  DISTLIB (AOSB3) .
         NAME  IEFVFA
*
*
         VER   0000 05B0                    BALR RB,0     (BASE=0002)
         VER   003A 58A0,B012               L    RA,VFA02 (BASE=1002)
*
         VER   071C 2000           TXTBFLEN DC   H'8192'
*
         VER   071E 45E0,BB68               BAL  R14,AOTXTLTH
*
         VER   0B6A 5850,C02C      AOTXTLTH L    R5,TEXTBUFP
*
         VER   1DF0 D7E3,C3C8               DC   C'PTCH'
         VER   1E20 0000,0000,0000,0000     START OF NEEDED PATCH AREA
         VER   1E28 0000,0000,0000,0000     NEEDED PATCH AREA
         VER   1E30 0000,0000,0000,0000     NEEDED PATCH AREA
         VER   1E38 0000,0000               END OF NEEDED PATCH AREA
*
         VER   23A0 16E2,E4C2,C1D3,D3D6,C37E,4C  SUBALLOC=
*
         REP   23A0 16E2,E8E2,D7D9,D6C3,7E01,40  SYSPROC=
*
         REP   1E20 58F0,C3A0               L    R15,RFULLE (ANCHOR)
         REP   1E24 58F0,F05C               L    R15,PPVPP4 A(IEFVPP4)
         REP   1E28 05EF                    BALR R14,R15
         REP   1E2A 45E0,BB68               BAL  R14,AOTXTLTH
         REP   1E2E 58F0,C3A0               L    R15,RFULLE (ANCHOR)
         REP   1E32 58F0,F060               L    R15,PPVPP5 A(IEFVPP5)
         REP   1E36 41E0,B720               LA   R14,RETURN ADDRESS
         REP   1E3A 07FF                    BR   R15
*
         REP   071E 47F0,AE1E               B    PATCH AREA
*
         IDRDATA #DYN004
*
++ USERMOD (#DYP005)  .
++ VER   (Z038)  FMID (EBB1102)               /* MVS 3.8 BASE */
                 PRE (UZ58715
                      #DYP001                 /* SYSGEN MACRO UPDATE */
                      #DYP002                 /* NEW MODULE IEFVPP */
                      #DYP003                 /* ZAP TO IEFVHF */
                      #DYP004                 /* ZAP TO IEFVFA */
                             )
               /*

               PRIVATE PROCLIB MODIFICATIONS
               VERSION R, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS

               IEFVHE

               IEFVHE IS THE INTERPRETER GET AND ROUTE ROUTINE.  THE
               ZAP CHANGES IEFVHE TO CHECK FOR A "JOBPROC" DD
               STATEMENT AND BYPASS PROCESSING IT.  THIS IS TO KEEP
               THE JOBPROC STATEMENT(S) FROM BEING PROCESSED BY THE
               INTERPRETER.  IF THIS IS NOT DONE, ANY JOB WITH A
               JOBPROC DD STATEMENT WILL RECEIVE A "MISPLACED DD
               STATEMENT" JCL ERROR.

               THE BYTE USED TO CONTAIN THE FLAG BIT IN THE
               INTERNAL TEXT HAS BEEN CHANGED FROM THE FOURTH
               TO THE THIRD BYTE.  THIS IS TO ACCOMODATE THE
               USE OF THE PREVIOUSLY USED FLAG BY THE CONVERTER
               ITSELF.

               THIS MODIFICATION DOES NOT NEED TO PRE-REQ OR CO-REQ
               ANY OF THE OTHER MODIFICATIONS FOR DYNAMIC PROCLIB,
               AS IT DOES NOT HAVE ANY DEPENDANCIES ON THE
               EXISTANCE OF THE OTHER CODE.  THE PRE-REQUISITES
               LISTED ARE TO INSURE THAT ALL OF THE OTHER PIECES
               ARE PROPERLY INSTALLED.  IF THIS MOD IS
               LEFT OFF, HOWEVER, ANY JOB THAT CONTAINS "JOBPROC"
               DD STATEMENTS WILL RECEIVE A JCL ERROR.

                                                                  */ .
++ ZAP   (IEFVHE)  DISTLIB (AOSB3) .
         NAME  IEFVHE
*
*
         VER   0000 05B0                   BALR R11,0 (BASE=0002)
*
         VER   0060 5860,C0EC      VHE0010 L R6,WANELPTR
*
         VER   014E D700,C16E,C16E         XC   SWY2(1),SWY2
*
         VER   02A0 0000,0000,0000,0000    START OF NEEDED PATCH AREA
         VER   02A8 0000,0000,0000,0000    END OF NEEDED PATCH AREA
*
         REP   02A0 9180,7002              TM   STRINDCS,JPROCSTR
         REP   02A4 4710,B05E              BO   VHE0010
         REP   02A8 D700,C16E,C16E         XC   SWY2(1),SWY2
         REP   02AE 07FF                   BR   R15  RETURN
*
         REP   014E 45F0,B29E,0700         BAL  R15,PATCH; NOPR 0
*
         IDRDATA #DYP005
++ USERMOD (#DYPDMY)  .
++ VER   (Z038)  FMID (EBB1102)                /* MVS 3.8 BASE */
                 PRE (UZ59124
                             )
               /*

               PRIVATE PROCLIB MODIFICATIONS
               VERSION 4, RELEASE 1, MODIFICATION 0

               THIS LOCAL MODIFICATION, ALONG WITH ITS
               COMPANION CO-REQUISITES, TOTALLY INTEGRATES THE
               NECESSARY SYSTEM MODIFICATIONS FOR DYNAMIC PROCLIB
               SUPPORT.

               #DYP001  SGIEF441  SYSGEN MACRO UPDATE AND JCLIN
               #DYP002  IEFVPP    NEW CONVERTER MODULE
               #DYP003  IEFVHF    ZAP FOR LINKAGE TO IEFVPP3
               #DYP004  IEFVFA    ZAP FOR LINKAGE TO IEFVPP4 & IEFVPP5
               #DYP005  IEFVHE    ZAP FOR INTERPRETER JOBPROC BYPASS
               #DYPDMY  IEFVH1    DUMMY ZAP TO FORCE RELINK OF IEFVH1

                                                                */  .
++ ZAP   (IEFVH1)  DISTLIB (AOSB3)  .
         NAME  IEFVH1
*
*
         VER   0300 0000           VERIFY ALL ZEROS IN PATCH AREA
*
         REP   0300 0000           REPLACE WITH SAME
*
         IDRDATA #DYPDMY
><
/*
//SYSUT2   DD  DSN=&&SMPMCS,DISP=(MOD,PASS)
//SYSIN    DD  DUMMY
//*********************************************************************
//*
//* DESC: RECEIVE THE DYNAMIC PROCLIB USERMODS
//*       MAKE SURE YOU CHANGE THE DSN OF THE SMPPTFIN TO MATCH
//*       THE DATASET NAME WHERE THE USERMODS ARE.  (USUUALLY THIS ONE)
//*
//*********************************************************************
//RECEIVE EXEC SMPREC,COND=(0,NE)
//*
//*  REJECT THE USERMODS JUST IN CASE THIS IS A RE-RUN
//*  THEN RECEIVE ALL 6 USERMODS
//*
//SMPPTFIN DD  DSN=&&SMPMCS,DISP=(OLD,DELETE)
//*  REJECT  SELECT(#DYP001,#DYP002,#DYP003,#DYP004,#DYP005)
//SMPCNTL  DD  *
 RESETRC
 .
 RECEIVE SELECT(#DYP001,#DYP002,#DYP003,#DYP004,#DYP005,#DYPDMY)
 .
/*
//APPLY1CK  EXEC SMPAPP,WORK='SYSDA'
//*
//*  APPLY THE SGIEF441 AND IEFVPP UP1ATES
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYP001,#DYP002,UZ45794)
         CHECK
 .
/*
//APPLY1    EXEC SMPAPP,COND=(0,NE,APPLY1CK.HMASMP),WORK='SYSALLDA'
//*
//*  APPLY THE SGIEF441 AND IEFVPP UP1ATES
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYP001,#DYP002,UZ45794)
         DIS(WRITE)
 .
/*
//APPLY2CK EXEC SMPAPP,WORK='SYSDA'
//*
//*  APPLY THE DUMMY UPDATE TO FORCE A LATER RE-LINK OF IEFVH1
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYPDMY)
         CHECK
 .
/*
//APPLY2  EXEC SMPAPP,WORK='SYSDA',COND=(0,NE,APPLY2CK.HMASMP)
//*
//*  APPLY THE DUMMY UPDATE TO FORCE A LATER RE-LINK OF IEFVH1
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYPDMY)
         DIS(WRITE)
 .
/*
//RESTORE EXEC SMPAPP,COND=(0,NE,APPLY2.HMASMP),WORK='SYSDA'
//*
//*  RESTORE THE DUMMY UPDATE, THIS FORCES A RE-LINK OF IEFVH1
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 RESTORE SELECT(#DYPDMY)
         DIS(WRITE)
 .
/*
//APPLY3CK EXEC SMPAPP,WORK='SYSDA'
//*
//*  APPLY THE ZAPS TO IEFVFA, IEFVHE AND IEFVHF
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYP003,#DYP004,#DYP005)
         CHECK
 .
/*
//APPLY3 EXEC SMPAPP,WORK='SYSDA',COND=(0,NE,APPLY3CK.HMASMP)
//*
//*  APPLY THE ZAPS TO IEFVFA, IEFVHE AND IEFVHF
//*
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 APPLY   SELECT(#DYP003,#DYP004,#DYP005)
         DIS(WRITE)
 .
/*
//*********************************************************************
//*
//* NAME: SYS1.DYNAMIC.PROCLIB(ACCEPT)
//*
//* DESC: ACCEPT DYNAMIC PROCLIB SUPPORT
//*
//* YOU MAY ELECT TO INSTALL MANUALLY BY FOLLOWING THE DIRECTIONS
//*WHICH ARE INCLUDED IN THE $$$DOC MEMBER OF THIS PDS
//*
//* IF YOU HAVE ANY QUESTIONS PLEASE CALL:
//*
//* BRIAN WESTERMAN        EMAIL:  BRIAN_WESTERMAN@SYZYGYINC.COM
//* SYZYGY INCORPORATED
//* PHONE: (800) 767-2244  FAX:(800) 366-4082
//*********************************************************************
//*
//DYNACCP  EXEC SMPAPP,WORK='SYSDA',
//             COND=((0,NE,APPLY1.HMASMP),(0,NE,APPLY2.HMASMP),        X
//             (0,NE,APPLY3.HMASMP))
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB
//SMPCNTL  DD  *
 ACCEPT  SELECT(#DYP001,#DYP002,#DYP003,#DYP004,#DYP005,UZ45794)
         USERMODS
         DIS(WRITE)
         COMPRESS(ALL)
 .
/*
//
