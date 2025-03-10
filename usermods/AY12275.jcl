//AY12275  JOB (SYSGEN),'J01 M01: AY12275',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/02/25 @kl
//*
//*        OY12275 - TCTRSZ INCORRECTLY REFLECTS SIZE OF PRIVATE AREA,
//*                  RATHER THAN REGION SIZE REQUESTED.
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++USERMOD(AY12275).
++VER(Z038) FMID(EBB1102) PRE(UZ35042)
 /*
  PROBLEM DESCRIPTION(S):
           OY12275 - TCTRSZ INCORRECTLY REFLECTS SIZE OF PRIVATE AREA,
                     RATHER THAN REGION SIZE REQUESTED.
  COMPONENT: 5752-SC1CH-EBB1102
  APARS FIXED: OY12275
  SPECIAL CONDITIONS:
  COMMENTS:          NONE
 */.
++ZAP(IEAVPRT0) DISTLIB(AOSC5).
NAME IEAVPRT0
VER 032E 58D0,7014     L  RWORK3,PQESIZE    GET REGION SIZE FROM PQE
REP 032E 58D0,45BC     L  RWORK3,VVREGSZ    GET REGION SIZE FROM LDA
IDRDATA AY12275
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(AY12275)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS
//SMPCNTL  DD  *
  APPLY
        SELECT(AY12275)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS
//SMPCNTL  DD  *
  APPLY
        SELECT(AY12275)
        DIS(WRITE)
        .
/*
//
