//SYZM001  JOB (SYSGEN),'J07 M07: SYZM001',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD SYZM001 (source: Brian Westerman) suppress CN(00) *
//* being appended to SEND operator command                           *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN  DD *
++USERMOD (SYZM001) /* SEND COMMAND IMPROVEMENT */.
++VER (Z038) FMID(EBB1102) /*
  THIS USER MOD FIXES THE SEND OPERATOR COMMAND TO NOT APPEND
  CN(00) TO THE MESSAGE WHEN SEND IS ISSUED BY A SYSTEM TASK.
  THIS IMPROVES THE APPEARANCE OF JES2 NOTIFY MESSAGES. */.
++ZAP (IEEVSND6).
 NAME IEEVSND6
 VER 0078 4350,A01D
 VER 058C 0000,0000
 REP 0078 47F0,B586
 REP 058C BF51,A01D,4770,B076
 REP 0594 D205,C16D,C16C,47F0,B098
/*
//*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(SYZM001)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SYZM001)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(SYZM001)
        DIS(WRITE)
        .
/*
//
