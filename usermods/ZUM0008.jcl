//ZUM0008  JOB (SYSGEN),'J06 M53: ZUM0008',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* Install USERMOD ZUM0008 (source: Juergin Winkelmann) to complete  *
//* TSO date display post Y2k during logon and logoff.                *
//*********************************************************************
//*
//RECEIVE EXEC SMPREC,WORK='SYSALLDA'
//SMPPTFIN DD  *
++USERMOD (ZUM0008) .
++VER (Z038) FMID(EBB1102)
  PRE(UY17588 UZ27405 ZUM0007)
  /*
   completion of Michael Koehne's Y2K patch ZUM0007
  */ .
++ ZAP (IKJEFLA) .
 NAME IKJEFLPA
 VER 0318 6B40F1F9   from 19
 REP 0318 6B40F2F0   to   20
 IDRDATA ZUM0008
/*
//SMPCNTL  DD  *
  RECEIVE SELECT(ZUM0008).
/*
//*
//APPLYCK EXEC SMPAPP,WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZUM0008)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD *
  APPLY
        SELECT(ZUM0008)
        DIS(WRITE)
        COMPRESS(ALL)
        .
/*
//
