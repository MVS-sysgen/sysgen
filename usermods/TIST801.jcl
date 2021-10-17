//TIST801  JOB (SYSGEN),'J08 M08: TIST801',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//* 2002/04/12 @KL TIST801 SKIP READING VTAMOBJ
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN   DD DATA,DLM='??'
++ USERMOD(TIST801) /* REWORK(20020624) */                 .
++ VER(Z038)
   FMID(EVT0108)
 /*
  PROBLEM DESCRIPTION(S):
    TIST801
      Bypass reading VTAMOBJ and always fetch resource
        definition from VTAMLST.

  COMPONENT:  5752-SC123-EVT0108

  SPECIAL CONDITIONS:
    ACTION:  VTAM must be restarted after this user modification
      is installed.

  COMMENTS:
    LAST CHANGE:  2002/06/24

     The following modules are affected by this usermod:
       ISTSDCRC
 */.
++ ZAP      (ISTSDCRC) DISTLIB(AOS26   ).
NAME ISTSDCRC
VER 0282 58F0,C6A0    L     @15,@CV03189     Point to VTAMOBJ read rtn
VER 0286 05EF         BALR  @14,@15          Go to it
REP 0282 41F0,0004    LA    @15,4            Simulate "VTAMOBJ member
REP 0286 0700         NOPR  0                 not found" error
IDRDATA TIST801
??
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(TIST801)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TIST801)
        BYPASS(ID)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(TIST801)
        DIS(WRITE)
        .
/*
//
