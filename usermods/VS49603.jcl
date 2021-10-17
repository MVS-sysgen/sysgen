//VS49603  JOB (SYSGEN),'J04 M14: VS49603',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//*
//* Desc: Install usermod VS49603 to fix excessive
//* disabled spin loop conditions
//*
//* Source: Doug Wegscheid, Apr 2020
//* Origin: Gerhard Postpischil and Juergen Winkelmann
//*
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD(VS49603) .
++VER(Z038) FMID(FBB1221)
PRE(UZ56445)
  /*
    Symptom: IEE331A EXCESSIVE DISABLED SPIN LOOP DETECTED
    Fix: Apply IBM ZAP VS49603, which fixes excessive disabled
    spin loop conditions when MVS is running under VM
  */ .
++ ZAP (IEEVEXSN) .
NAME IEANUC01 IEEVEXSN
VER 011A 47E0 BNO
REP 011A 4700 NOP
IDRDATA VS49603
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(VS49603)
          .
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(VS49603)
        CHECK
        .
/*
//*
//APPLY   EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(VS49603)
        DIS(WRITE)
        .
/*
//
