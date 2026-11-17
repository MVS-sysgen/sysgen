//ZUM0015  JOB (SYSGEN),'ZUM0015',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             REGION=4096K
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*********************************************************************
//* INSTALL USERMOD ZUM0015 - PUT THE CONSOLES IN ROLL DELETE MODE AT *
//* IPL                                                               *
//*                                                                   *
//* FROM JAY MOSELEY'S INSTALLMVS V8 TAPE/USERMODS.HET                *
//*********************************************************************
//*
//RECEIVE  EXEC SMPREC,WORK='SYSALLDA',COND=(0,NE)
//SMPPTFIN DD  DATA,DLM='??'
++USERMOD (ZUM0015) /* SET CONSOLES TO ROLL-DELETE MODE SP 3
  DESC: Install USERMOD ZUM0015 (put consoles in RD mode at IPL)
        */.
++VER (Z038) FMID(EBB1102) PRE(UZ35148,UZ34427).
++ZAP(IEECVETC) DISTLIB(AOSC5).
NAME IGC0007B IEECVETC
VER 00F8 92E8910A        MVI 'Y'  RDEL='Y'
VER 00FC 9240910B        MVI ' '  RDEL='Y '
VER 0106 92E8910C        MVI 'Y'  RDEL='Y'
VER 010A 92E0911B        MVI X'E0' ?
VER 01AC 96E09122        OI  X'E0' ?
VER 0274 58F0918C        L   R15,X'18C'(,R9)
VER 0378 5C5C40C9C5C5C3E5 ** IEECVETC **
*
REP 00F8 92D9              MVI  'R'  RDEL=R
REP 00FC 92C4              MVI  'D'  RDEL=RD
REP 0106 92D5              MVI  'N'  CON=N
REP 010A 92309123          MVI  X'30' ?
*
REP 01AC 47F0C376       *** BRANCH TO PATCH ***
REP 0378 96509018          MVI X'50' ?
REP 037C 9603912C          MVI X'03' ?
REP 0380 9680912D          MVI X'80' ?
REP 0384 96E09122          MVI X'E0' ?
REP 0388 47F0C272          JUST AFTER 9110C36A
++UPDTE (IEECDCM) .
./ CHANGE NAME=IEECDCM
&P       SETC  '01'                 DCMRTMED                   ZUM0015  01800110
&M       SETC  '16'                 DCMSEGDF                   ZUM0015  01800120
&H       SETC  '9'                 DCMSEG INITIAL VALUE        ZUM0015  03500110
&K       SETC  '19'                DCMRNUM INITIAL VALUE       ZUM0015  03500120
&L       SETC  '01'                DCMRTME INITIAL VALUE       ZUM0015  03500130
&M       SETC  '19'                DCMSEGDF INITIAL VALUE      ZUM0015  03500140
&N       SETC  '19'                DCMRNUMD INITIAL VALUE      ZUM0015  03500150
??
//SMPCNTL  DD  *
  RECEIVE
          SELECT(ZUM0015)
          .
/*
//*
//APPLYCK  EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZUM0015)
        CHECK
        .
/*
//*
//APPLY    EXEC SMPAPP,COND=(0,NE),WORK='SYSALLDA'
//SMPCNTL  DD  *
  APPLY
        SELECT(ZUM0015)
        DIS(WRITE)
        .
/*
//
