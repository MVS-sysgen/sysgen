//JLM0003  JOB (SYSGEN),'J04 M04: JLM0003',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD JLM0003 - replace TSO Command Authorization Table *
//* (IKJEFTE2)                                                        *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC
//SYSIN    DD  *
./ ADD NAME=IKJEFTE2
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
*    THIS MODULE COMPRISES THE CSECT FOR THE COMMAND NAMES WHICH ARE  *
*    TO BE ATTACHED BY THE TMP AS ELIGIBLE FOR AUTHORIZATION          *
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
*1MODULE-NAME = IKJEFTE2
*
*1DESCRIPTIVE-NAME = APF-ELIGIBLE-COMMAND-NAME TABLE
*
*1DESCRIPTION = CONTAINS THE COMMAND-NAMES WHICH ARE TO BE ATTACHED
*           BY THE TMP AS ELIGIBLE FOR AUTHORIZATION
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
         ENTRY APFCTABL
         SPACE 1
IKJEFTE2 CSECT
         DC    C'IKJEFTE2'             MODULE NAME
         DC    C'&SYSDATE'             RELEASE LEVEL
APFCTABL DS    0D                      ALIGNMENT
         SPACE 2
         DC    C'#       '             CMDSBSYS
         DC    C'CMDSBSYS'             CMDSBSYS
         DC    C'IM      '             IMON/370
         DC    C'LISTD   '             TSO LIST DATASET
         DC    C'LISTDS  '             TSO LIST DATASET
         DC    C'IKJEHDS1'             TSO LIST DATASET
         DC    C'ISPF    '             ISPF
         DC    C'ISPFLINK'             ISPF
         DC    C'ISPFMAIN'             ISPF
         DC    C'PDS     '             PDS UTILITY
         DC    C'PDS86   '             PDS UTILITY
         DC    C'QUEUE   '             QUEUE
         DC    C'QUE     '             QUEUE
         DC    C'Q       '             QUEUE
         DC    C'RFE     '             REVIEW FRONT END
         DC    C'RPF     '             RPF
         DC    C'RPFMAIN '             RPF
         DC    C'SPY     '             SPY CONSOLE MONITOR
         DC    C'BREXX   '             BREXX/370
         DC    C'REXX    '             BREXX/370
         DC    C'RX      '             BREXX/370
         DC    C'CDSCB   '
         DC    C'        '             TERMINATOR
         SPACE 2
               END
./ ENDUP
/*
//*
//SMPASM02 EXEC SMPASML,M=IKJEFTE2,COND=(0,NE)
//*
//RECV03   EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(JLM0003)
  .
++VER(Z038)
  FMID(EBB1102)
  .
++MOD(IKJEFTE2)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 RECEIVE
         SELECT(JLM0003)
         .
  APPLY
        SELECT(JLM0003)
        DIS(WRITE)
        .
//
