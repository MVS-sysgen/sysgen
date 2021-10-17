//JLM0004  JOB (SYSGEN),'J05 M05: JLM0004',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0004 - replace TSO Command Authorization Table *
//* (IKJEFTE8)                                                        *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IKJEFTE8
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* SOURCE -- MVS 3.8 OPTIONAL MATERIALS; SYM1-3                        *
*                                                                     *
* STATUS --                                                           *
*                                                                     *
*    CHANGE LEVEL  040  000                                           *
*                                                                     *
* FUNCTION --                                                         *
*                                                                     *
*    THIS MODULE COMPRISES THE CSECT FOR THE PROGRAM NAMES WHICH ARE  *
*    TO BE ATTACHED BY TMP-CALL AS ELIGIBLE FOR AUTHORIZATION         *
*                                                                     *
* ENTRY POINTS --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* INPUT --                                                            *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* OUTPUT --                                                           *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXTERNAL REFERENCES --                                              *
*                                                                     *
*     NONE                                                            *
*                                                                     *
* EXITS, NORMAL --                                                    *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* EXITS, ERROR --                                                     *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* TABLES/WORK AREA --                                                 *
*                                                                     *
*    NONE                                                             *
*                                                                     *
* ATTRIBUTE --                                                        *
*                                                                     *
*    REFRESHABLE                                                      *
*                                                                     *
* NOTES --                                                            *
*                                                                     *
*    THIS MODULE IS CHARACTER CODE DEPENDENT UPON AN INTERNAL REPRE-  *
*    SENTATION OF THE EXTERNAL CHARACTER SET EQUIVALENT TO THE ONE IN *
*    USE AT ASSEMBLY TIME                                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         EJECT
* /* START OF SPECIFICATIONS ****
*
*1MODULE-NAME = IKJEFTE8
*
*1DESCRIPTIVE-NAME = APF-ELIGIBLE-PROGRAM-NAME TABLE
*
*1DESCRIPTION = CONTAINS THE PROGRAM-NAMES WHICH ARE TO BE ATTACHED
*           BY TMP-CALL AS ELIGIBLE FOR AUTHORIZATION
*
*1MODULE TYPE = TABLE
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*  2FIELD = BLANKS
*    3TYPE = CHARACTER STRING
*
**** END OF SPECIFICATIONS ***/
         EJECT
         SPACE 3
         ENTRY APFPTABL
         SPACE 1
IKJEFTE8 CSECT
         DC    C'IKJEFTE8'             MODULE NAME
         DC    C'&SYSDATE'             RELEASE LEVEL
APFPTABL DS    0D                      ALIGNMENT
         SPACE 2
         DC    C'IEBCOPY '
         DC    C'IKJEFF76'
         DC    C'ISPF    '
         DC    C'ISPLINK '
         DC    C'ISPMAIN '
         DC    C'RECV370 '
         DC    C'XMIT370 '
         DC    C'CDSCB   '
         DC    C'BREXX   '
         DC    C'REXX    '
         DC    C'RX      '
         DC    C'        '             TERMINATOR
         END
./ ENDUP
/*
//*
//SMPASM02 EXEC SMPASML,M=IKJEFTE8,COND=(0,NE)
//*
//RECV03   EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(JLM0004)
  .
++VER(Z038)
  FMID(EBB1102)
  PRE(UZ78841)
  .
++MOD(IKJEFTE8)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0004)
         .
  APPLY
        SELECT(JLM0004)
        DIS(WRITE)
        .
//
