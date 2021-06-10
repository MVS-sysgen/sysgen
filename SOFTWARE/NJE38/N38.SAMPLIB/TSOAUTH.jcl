//HERC01X JOB  1,
//             'Rebuild IKJEFT02',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1)
//*********************************************************************
//*
//* Desc: Adds TSO command NJE38 to be an authorized command
//*
//* Please refer to Appendix C of the NJE38 documentation before
//*  running this job.
//*
//*
//*********************************************************************
//*
/*MESSAGE  ******************************************
/*MESSAGE  *                                        *
/*MESSAGE  * This change becomes effective only     *
/*MESSAGE  * if you do an IPL with the CLPA option  *
/*MESSAGE  *                                        *
/*MESSAGE  ******************************************
//*
//ASM      EXEC  PGM=IFOX00,REGION=4096K,
// PARM=('XREF(FULL),OBJ,NODECK')
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1700,(5600,500))
//SYSUT2   DD DSN=&&SYSUT2,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSUT3   DD DSN=&&SYSUT3,UNIT=SYSDA,SPACE=(1700,(1300,500))
//SYSPRINT DD SYSOUT=*
//SYSGO    DD DSN=&&LOADSET,DISP=(,PASS),UNIT=SYSDA,
//            SPACE=(800,(320,100))
//SYSIN    DD *
         ENTRY APFCTABL
IKJEFTE2 CSECT
         DC    CL8'IKJEFTE2'
         DC    CL8'&SYSDATE'
APFCTABL DC    CL8'#       '       CMDSBSYS TSO interface
         DC    CL8'CMDSBTSO'       CMDSBSYS TSO interface
         DC    CL8'IEBCOPY '       Copy under TSO
         DC    CL8'IM      '       IMON/370
         DC    CL8'LISTD   '       TSO List Dataset commands
         DC    CL8'LISTDS  '       ditto
         DC    CL8'IKJEHDS1'       ditto
         DC    CL8'NJE38   '       NJE38 Command
         DC    CL8'PDS     '       PDS Utility
         DC    CL8'PDSAA   '       PDS Utility (Test)
         DC    CL8'PDS73   '       PDS Utility (Alias)
         DC    CL8'PDS85   '       PDS Utility (Alias)
         DC    CL8'QUEUE   '       QUEUE COMMAND
         DC    CL8'Q       '       ALIAS
         DC    CL8'QUE     '       ALIAS
         DC    CL8'RECEIVE '       NJE38 RECEIVE
         DC    CL8'RPF     '       SPARE TABLE ENTRIES
         DC    CL8'RPFMAIN '       SPARE TABLE ENTRIES
         DC    CL8'SPFCOPY '       Copy under TSO
         DC    CL8'TRANSMIT'       NJE38 TRANSMIT
         DC    CL8'TSOUSER '
         DC    CL8'XMIT    '       NJE38 TRANSMIT Alias
         DC    CL8'        '       8 BLANKS TABLE TERMINATOR
         END
         ENTRY APFCTABL
/*
//LKED    EXEC PGM=IEWL,PARM='NCAL,LIST,XREF,LET,REUS,RENT',COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSLMOD  DD DSN=SYS1.LPALIB,DISP=SHR
//AOST4    DD DSN=SYS1.AOST4,DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,PASS)
//         DD *
   INCLUDE AOST4(IKJEFT02,IKJEFT03,IKJEFT06)
   INCLUDE AOST4(IKJEFTNS,IKJEFT08)
   INCLUDE AOST4(IKJEFTE8)
   ORDER IKJEFT02(P)
   ORDER IKJEFTNS
   ORDER IKJEFTE2
   ORDER IKJEFT03
   ORDER IKJEFT06
   ORDER IKJEFT08
   ORDER IKJEFTE8
   ENTRY IKJEFT02
   ALIAS IKJEFT03
   ALIAS IKJEFT0B
   NAME IKJEFT02(R)
/*
