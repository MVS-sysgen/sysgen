//NJE38FIN JOB (TSO),
//             'Install NJE38',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ALLOC03  EXEC PGM=IEFBR14
//PARMLIB  DD  DSN=SYS2.PARMLIB,DISP=(,CATLG,),
//             UNIT=3350,VOL=SER=MVS000,
//             SPACE=(CYL,(10,5,50)),
//             DCB=(SYS1.PARMLIB)
//*
//* CREATE THE NJE38 NETSPOOL DATASET
//*
//SPOOL    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SPLVOL   DD UNIT=SYSDA,DISP=SHR,VOL=SER=PUB001
//SYSIN    DD *
  DELETE (PUB001.NJE38.NETSPOOL) CLUSTER PURGE
  /* IF THERE WAS NO DATASET TO DELETE, RESET CC           */
  IF LASTCC = 8 THEN
    DO
        SET LASTCC = 0
        SET MAXCC  = 0
    END

   /* DEFINE NJE38 CLUSTER */

  DEF CL (  NAME( PUB001.NJE38.NETSPOOL )                     -
            RECSZ(4089,4089)                                  -
            CYL(100)                                          -
            NUMBERED                                          -
            CISZ(4096)                                        -
            SHR(4 4)                                          -
            FILE( SPLVOL )                                    -
            VOLUMES( PUB001 ))                                -
  DATA (  NAME( PUB001.NJE38.NETSPOOL.DATA )                  -
            UNIQUE )
/*
//*
//* FORMAT THE NJE38 NETSPOOL DATASET
//*
//FMT      EXEC PGM=NJEFMT
//STEPLIB  DD DISP=SHR,DSN=SYSGEN.NJE38.AUTHLIB
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//NETSPOOL DD DISP=OLD,DSN=PUB001.NJE38.NETSPOOL
//*
//* ADD THE NJE CONFIG FILE TO SYS2.PARMLIB(NJE38C00)
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*
* NJE38 EXAMPLE CONFIGURATION FILE
*
*-- Local parameters
*
LOCAL MVSA     DEFUSER IBMUSER
*
*-- Physical links to remote nodes
*
LINK  MVSB     LINE 602   AUTO YES   BUFF 1012
*
*-- Routes to indirect nodes
*
*ROUTE nodeid   TO linkid
*
*
*-- Authorized users
*
*     Userid   AT Node
*     -------- -- --------
AUTH  HMVS01   AT MVSA
AUTH  HMVS01   AT MVSB
*
//SYSUT2   DD DSN=SYS2.PARMLIB(NJE38C00),DISP=SHR
//*
//* Add the NJE38 proc to SYS2.PROCLIB(NJE38)
//*
//NJEPROC  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=NJE38
//NJE38    PROC M=NJE38C00,
//            D='SYS2.PARMLIB'
//*
//* STARTED TASK PROCEDURE FOR NJE38
//*
//NJEINIT  EXEC PGM=NJEINIT,REGION=4096K
//STEPLIB  DD DSN=SYSGEN.NJE38.AUTHLIB,DISP=SHR
//NETSPOOL DD DSN=PUB001.NJE38.NETSPOOL,DISP=SHR
//CONFIG   DD DSN=&D(&M),DISP=SHR,FREE=CLOSE
//IEFRDER  DD DUMMY
@@
//*
//*********************************************************************
//* Install USERMOD NJE0001 - replace TSO Command Authorization Table *
//* (IKJEFTE2)                                                        *
//*********************************************************************
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//IKJEFTE2 EXEC PGM=IEBUPDTE,PARM=NEW
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
         DC    C'NJE38   '             NJE38 Command
         DC    C'PDS     '             PDS UTILITY
         DC    C'PDS86   '             PDS UTILITY
         DC    C'QUEUE   '             QUEUE
         DC    C'QUE     '             QUEUE
         DC    C'Q       '             QUEUE
         DC    C'RECEIVE '             NJE38 RECEIVE
         DC    C'RFE     '             REVIEW FRONT END
         DC    C'RPF     '             RPF
         DC    C'RPFMAIN '             RPF
         DC    C'SPY     '             SPY CONSOLE MONITOR
         DC    C'TRANSMIT'             NJE38 TRANSMIT
         DC    C'XMIT    '             NJE38 TRANSMIT Alias
         DC    C'        '             TERMINATOR
         SPACE 2
               END
./ ENDUP
/*
//*
//NJSMPASM EXEC SMPASML,M=IKJEFTE2,COND=(0,NE)
//*
//NJERECV  EXEC SMPAPP,COND=(0,NE),WORK=SYSALLDA
//SMPPTFIN DD  *
++USERMOD(NJE0001)
  .
++VER(Z038)
  FMID(EBB1102)
  PRE(JLM0003)
  .
++MOD(IKJEFTE2)
  DISTLIB(AOST4)
  LKLIB(UMODLIB)
  .
/*
//SMPCNTL  DD  *
 REJECT  S (NJE0001) .
 RESETRC .
 RECEIVE
         SELECT(NJE0001)
         .
  APPLY
        SELECT(NJE0001)
        DIS(WRITE)
        .
/*
//*
//* ADD RAKF PROFILE
//*
//SORTP   EXEC PGM=SORT,REGION=512K,PARM='MSG=AP'
//STEPLIB DD   DSN=SYSC.LINKLIB,DISP=SHR
//SYSOUT  DD   SYSOUT=A
//SYSPRINT DD  SYSOUT=A
//SORTLIB DD   DSNAME=SYSC.SORTLIB,DISP=SHR
//SORTOUT DD   DSN=SYS1.SECURE.CNTL(PROFILES),
//             DISP=SHR,DCB=(BLKSIZE=80,RECFM=F)
//SORTWK01 DD  UNIT=2314,SPACE=(CYL,(5,5)),VOL=SER=SORTW1
//SORTWK02 DD  UNIT=2314,SPACE=(CYL,(5,5)),VOL=SER=SORTW2
//SORTWK03 DD  UNIT=2314,SPACE=(CYL,(5,5)),VOL=SER=SORTW3
//SORTWK04 DD  UNIT=2314,SPACE=(CYL,(5,5)),VOL=SER=SORTW5
//SORTWK05 DD  UNIT=2314,SPACE=(CYL,(5,5)),VOL=SER=SORTW6
//SYSIN  DD    *
 SORT FIELDS=(1,80,CH,A)
 RECORD TYPE=F,LENGTH=(80)
 END
/*
//SORTIN DD DSN=SYS1.SECURE.CNTL(PROFILES),DISP=SHR
//       DD *
DATASET PUB001.NJE38.NETSPOOL                               UPDATE
/*
//* Reload profile table
//RAKFPROF EXEC RAKFPROF