//TMVS804  JOB (SYSGEN),'J01 M10: TMVS804',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/03/29 @kl TMVS804 Don't autostart primary subsystem
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TMVS804) /* REWORK(20020624) */                 .
++ VER (Z038)
   FMID(EBB1102)
 /*
  PROBLEM DESCRIPTION(S):
    TMVS804 -
      Delete autostart for primary subsystem from master
      scheduler JCL.

  COMPONENT:  5752-SC1B6-EBB1102

  SPECIAL CONDITIONS:
    ACTION:  An IPL is required after installation of this user
      modification.

  COMMENTS:
    LAST CHANGE:  2002/06/24

    THE FOLLOWING MODULES AND/OR MACROS ARE AFFECTED BY THIS USERMOD:

    MODULES
      MSTRJCL

    MACROS
      SGIEE0MS
 */.
++ MACUPD   (SGIEE0MS) DISTLIB(AMODGEN ).
./ CHANGE NAME=SGIEE0MS
         DC    CL80'//*START &SSNAME'                          @TMVS804 04900002
??
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TMVS804)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TMVS804)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TMVS804)
        DIS(WRITE)
        .
/*
//
