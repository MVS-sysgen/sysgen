//ZUM0007  JOB (SYSGEN),'J05 M52: ZUM0007',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD ZUM0007 (source: Michael Koehne) to update TSO to *
//* display dates correctly post Y2k                                  *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN  DD *
++USERMOD (ZUM0007) .
++VER (Z038) FMID(EBB1102)
  PRE(UY17588 UZ27405)
  /*
   Update date offsets for post Y2k
  */ .
++ ZAP (IKJEFLA) .
 NAME IKJEFLPB
 VER 0000 197A6B40    from 19
 REP 0000 207A6B40    to   20
 IDRDATA ZUM0007
++ ZAP (IKJEFT25) .
 NAME IKJEFT25
 VER 06C4 4040F1F9    from '  19'
 REP 06C4 4040F2F0    to   '  20'
 IDRDATA ZUM0007
/*
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZUM0007)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(ZUM0007)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(ZUM0007)
        DIS(WRITE)
        .
/*
//
